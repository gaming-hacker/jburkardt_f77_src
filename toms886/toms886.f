      subroutine cheb ( deg, pt, tcheb )

c*********************************************************************72
c
cc CHEB computes normalized Chebyshev polynomials.
c
c  Discussion:
c
c    This subroutine computes the array TCHEB of normalized Chebyshev 
c    polynomials from degree 0 to DEG:
c      T_0(x)=1, 
c      T_j(x) = sqrt(2) * cos ( j * acos(x) ) 
c    at the point x = PT.
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
c    Original FORTRAN77 version by Marco Caliari, Stefano De Marchi, 
c    Marco Vianello.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Marco Caliari, Stefano de Marchi, Marco Vianello,
c    Algorithm 886:
c    Padua2D: Lagrange Interpolation at Padua Points on Bivariate Domains,
c    ACM Transactions on Mathematical Software,
c    Volume 35, Number 3, October 2008, Article 21, 11 pages.
c
c  Parameters:
c
c    Input, integer DEG, the degree.
c    0 <= DEG.
c
c    Input, double precision PT, the evaluation point.
c
c    Output, double precision TCHEB(0:DEG), the value of the normalized
c    Chebyshev polynomials of degrees 0 through DEG at the point PT.
c
      implicit none

      integer deg

      integer j
      double precision pt
      double precision sqrt2
      parameter ( sqrt2 = 1.4142135623730951D+00 )
      double precision tcheb(0:deg)

      if ( deg .lt. 0 ) then
        return
      end if

      tcheb(0) = 1.0D+00

      if ( deg .lt. 1 ) then
        return
      end if

      tcheb(1) = sqrt2 * pt
 
      if ( deg .lt. 2 ) then
        return
      end if

      tcheb(2) = 2.0D+00 * pt * tcheb(1) - sqrt2 * tcheb(0)
c
c  Chebyshev recurrence.
c
      do j = 3, deg
        tcheb(j) = 2.0D+00 * pt * tcheb(j-1) - tcheb(j-2)
      end do

      return
      end
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
c    This FORTRAN77 version by John Burkardt.
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
c    Input, double precision C(LDC,N).
c    Before entry, the leading M by N part of this array must contain the 
c    matrix C, except when BETA is zero, in which case C need not be set 
c    on entry.
c    On exit, the array C is overwritten by the M by N matrix
c      alpha * op ( A ) * op ( B ) + beta * C.
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
      function franke ( x, y )

c*********************************************************************72
c
cc FRANKE returns the value of the Franke function #1.
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
c    John Burkardt
c
c  Reference:
c
c    Richard Franke,
c    Scattered Data Interpolation: Tests of Some Methods,
c    Mathematics of Computation,
c    Volume 38, Number 157, January 1982, pages 181-200.
c
c  Parameters:
c
c    Input, double precision X, Y, the evalution points.
c
c    Output, double precision FRANKE, the function values.
c
      implicit none

      double precision franke
      double precision x
      double precision y

      franke =
     &    0.75D+00 * exp ( 
     &      - ( ( 9.0D+00 * x - 2.0D+00 )**2            
     &        + ( 9.0D+00 * y - 2.0D+00 )**2 ) / 4.0D+00 )  
     &  + 0.75D+00 * exp ( 
     &      - ( ( 9.0D+00 * x + 1.0D+00 )**2 ) / 49.0D+00   
     &        - ( 9.0D+00 * y + 1.0D+00 )      / 10.0D+00 )      
     &  + 0.5D+00  * exp ( 
     &      - ( ( 9.0D+00 * x - 7.0D+00 )**2            
     &        + ( 9.0D+00 * y - 3.0D+00 )**2 ) / 4.0D+00 )  
     &  - 0.2D+00  * exp ( 
     &        - ( 9.0D+00 * x - 4.0D+00 )**2            
     &        - ( 9.0D+00 * y - 7.0D+00 )**2 )

      return
      end
      subroutine padua2 ( deg, degmax, npd, wpd, fpd, raux1, raux2, 
     &  c0, esterr )

c*********************************************************************72
c
cc PADUA2 computes the Padua interpolation coefficient matrix.
c
c  Discussion:
c
c    This subroutine computes the coefficient matrix C0, in the 
c    orthonormal Chebyshev basis T_j(x)T_{k-j}(y), 0 <= j <= k <= DEG, 
c    T_0(x)=1, T_j(x) = sqrt(2) * cos(j * acos(x)), of the 
c    interpolation polynomial of degree DEG of the function values FPD 
c    at the set of NPD Padua points (PD1,PD2) in the square [-1,1]^2. 
c
c    The interpolant may be evaluated at an arbitrary point by the 
c    function PD2VAL. PD1, PD2 and WPD are the Padua points and weights 
c    computed by PDPTS.
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
c    Original FORTRAN77 version by Marco Caliari, Stefano De Marchi, 
c    Marco Vianello.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Marco Caliari, Stefano de Marchi, Marco Vianello,
c    Algorithm 886:
c    Padua2D: Lagrange Interpolation at Padua Points on Bivariate Domains,
c    ACM Transactions on Mathematical Software,
c    Volume 35, Number 3, October 2008, Article 21, 11 pages.
c
c  Parameters:
c
c    Input, integer DEG, the degree of approximation.
c
c    Input, integer DEGMAX, the maximum degree allowed.
c
c    Input, integer NPD, the number of Padua points.
c
c    Input, double precision WPD(NPD), the weights.
c
c    Input, double precision FPD(NPD), the value at the Padua points
c    of the function to be interpolated.
c
c    Workspace, double precision RAUX1(0:DEGMAX,DEG+2).
c
c    Workspace, double precision RAUX2(0:DEGMAX,DEG+2).
c
c    Output, double precision C0(0:DEG,0:DEG), the coefficient matrix.
c
c    Output, double precision ESTERR, the estimated error.
c
      implicit none

      integer deg
      integer degmax
      integer npd

      double precision angle
      double precision c0(0:degmax+1,0:deg+1)
      double precision esterr
      double precision fpd(npd)
      integer i
      integer j
      integer k
      double precision pi
      parameter ( pi = 3.1415926535897931D+00 )
      double precision pt
      double precision raux1(0:degmax,deg+2)
      double precision raux2(0:degmax,deg+2)
      double precision wpd(npd)
c
c  Build the matrix P_2 and store it in RAUX2.
c
      do i = 0, deg + 1
        angle = dble ( i ) * pi / dble ( deg + 1 )
        pt = - cos ( angle )
        call cheb ( deg, pt, raux2(0:deg,i+1) )
      end do
c
c  Build the matrix G(f) and store it in C0.
c
      k = 0
      do j = 0, deg + 1
        do i = 0, deg
          if ( mod ( i + j, 2 ) .eq. 0 ) then
            k = k + 1
            c0(i,j) = fpd(k) * wpd(k)
          else
            c0(i,j) = 0.0D+00
          end if
        end do
      end do
c
c  Compute the matrix-matrix product G(f)*P_2' and store it in RAUX1.
c
      call dgemm ( 'n', 't', deg + 1, deg + 1, deg + 2, 1.0D+00,
     &  c0, degmax + 2, raux2, degmax + 1, 0.0D+00, raux1, degmax + 1 )
c
c  Build the matrix P_1 and store it in RAUX2.
c
      do i = 0, deg
        angle = dble ( i ) * pi / dble ( deg )
        pt = - cos ( angle )
        call cheb ( deg, pt, raux2(0:deg,i+1) )
      end do
c
c  Compute the matrix-matrix product C(f) = P_1 * ( G(f) * P_2' ) 
c  and store it in C0.
c
      call dgemm ( 'n', 'n', deg + 1, deg + 1, deg + 1, 1.0D+00,
     &  raux2, degmax + 1, raux1, degmax + 1, 0.0D+00, c0, degmax + 2 )

      c0(deg,0) = c0(deg,0) / 2.0D+00
c
c  Estimate the error.
c
      esterr = 0.0D+00
      do j = 0, 2
        do i = 0, deg - j
          esterr = esterr + abs ( c0(i,deg-i-j) )
        end do
      end do
      esterr = 2.0D+00 * esterr

      return
      end
      function pd2val ( deg, degmax, c0, tg1, tg2, ttg1, ttg2 )

c*********************************************************************72
c
cc PD2VAL evaluates the Padua2 interpolant.
c
c  Discussion:
c
c    This function returns the value of the interpolant at (TG1,TG2).
c    C0 is the matrix of the coefficients computed by PADUA2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c  
c  Modified:
c
c    13 February 2014
c
c  Author:
c
c    Original FORTRAN77 version by Marco Caliari, Stefano De Marchi, 
c    Marco Vianello.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Marco Caliari, Stefano de Marchi, Marco Vianello,
c    Algorithm 886:
c    Padua2D: Lagrange Interpolation at Padua Points on Bivariate Domains,
c    ACM Transactions on Mathematical Software,
c    Volume 35, Number 3, October 2008, Article 21, 11 pages.
c
c  Parameters:
c
c    Input, integer DEG, the degree of approximation.
c
c    Input, integer DEGMAX, the maximum degree allowed.         
c
c    Input, double precision C0(0:DEGMAX+1,0:DEG), the coefficient matrix.
c
c    Input, double precision TG1, TG2, the first and second coordinates of
c    the target point.
c
c    Output, double precision TTG1(0:DEG), TTG2(0:DEG), the normalized
c    Chebyshev polynomials at the first and second coordinates of the
c    target point.
c
c    Output, double precision PD2VAL, the value of the interpolant at
c    the target point.
c
      implicit none

      integer deg
      integer degmax

      double precision c0(0:degmax+1,0:deg)
      integer i
      double precision pd2val
      double precision r8vec_dot_product
      double precision tg1
      double precision tg2
      double precision ttg1(0:deg)
      double precision ttg2(0:deg)
c
c  Compute the normalized Chebyshev polynomials at the target point.
c     
      call cheb ( deg, tg1, ttg1 )
      call cheb ( deg, tg2, ttg2 )
c 
c  Evaluate the interpolant
c
      pd2val = 0.0D+00
      do i = deg, 0, -1
        pd2val = pd2val + ttg2(deg-i) 
     &    * r8vec_dot_product ( i + 1, ttg1(0:i), c0(0:i,deg-i) )
      end do

      return
      end
      subroutine pdpts ( deg, pd1, pd2, wpd, npd )

c*********************************************************************72
c
cc PDPTS returns the points and weights for Padua interpolation.
c
c  Discussion:
c
c    This subroutine computes the first family of Padua points and 
c    weights corresponding to degree DEG.
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
c    Original FORTRAN77 version by Marco Caliari, Stefano De Marchi, 
c    Marco Vianello.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Marco Caliari, Stefano de Marchi, Marco Vianello,
c    Algorithm 886:
c    Padua2D: Lagrange Interpolation at Padua Points on Bivariate Domains,
c    ACM Transactions on Mathematical Software,
c    Volume 35, Number 3, October 2008, Article 21, 11 pages.
c
c  Parameters:
c
c    Input, integer DEG, the degree of approximation.
c
c    Output, double precision PD1(NPD), PD2(NPD), the first and second
c    coordinates of the Padua points
c
c    Output, double precision WPD(NPD), the weights.
c
c    Output, integer NPD, the number of Padua points.
c    NPD = ( DEG + 1 ) * ( DEG + 2 ) / 2.
c
      implicit none

      integer deg
      integer itemp0
      integer j
      integer k
      integer npd
      double precision pd1(*)
      double precision pd2(*)
      double precision pi
      parameter ( pi = 3.1415926535897931D+00 )
      double precision rtemp0
      double precision wpd(*)
c
c  Compute the Padua points of the first family at degree DEG.
c
      if ( deg .eq. 0 ) then    
        pd1(1) = -1.0D+00
        pd2(1) = -1.0D+00
        wpd(1) = 2.0D+00
        npd = 1
        return
      end if
   
      npd = 0
      itemp0 = deg * ( deg + 1 )
      rtemp0 = pi / itemp0

      do j = 0, deg + 1
        do k = mod ( j, 2 ), deg, 2

          npd = npd + 1
          pd1(npd) = - cos ( ( deg + 1 ) * k * rtemp0 )
          pd2(npd) = - cos ( deg * j * rtemp0 )
          wpd(npd) = 2.0D+00 / itemp0

          if ( k .eq. 0 .or. k .eq. deg ) then
            wpd(npd) = wpd(npd) / 2.0D+00
          end if

          if ( j .eq. 0 .or. j .eq. deg + 1 ) then
            wpd(npd) = wpd(npd) / 2.0D+00
          end if

        end do
      end do

      return
      end
      function r8_huge ( )

c*********************************************************************72
c
cc R8_HUGE returns a "huge" R8.
c
c  Discussion:
c
c    The value returned by this function is NOT required to be the
c    maximum representable R8.  This value varies from machine to machine,
c    from compiler to compiler, and may cause problems when being printed.
c    We simply want a "very large" but non-infinite number.
c
c    FORTRAN90 provides a built-in routine HUGE ( X ) that
c    can return the maximum representable number of the same datatype
c    as X, if that is what is really desired.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_HUGE, a huge number.
c
      implicit none

      double precision r8_huge

      r8_huge = 1.0D+30

      return
      end
      function r8vec_dot_product ( n, v1, v2 )

c*********************************************************************72
c
cc R8VEC_DOT_PRODUCT finds the dot product of a pair of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    In FORTRAN90, the system routine DOT_PRODUCT should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), V2(N), the vectors.
c
c    Output, double precision R8VEC_DOT_PRODUCT, the dot product.
c
      implicit none

      integer n

      integer i
      double precision r8vec_dot_product
      double precision v1(n)
      double precision v2(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i) * v2(i)
      end do

      r8vec_dot_product = value

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ', 
     &  'May      ', 'June     ', 'July     ', 'August   ', 
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *, 
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) 
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
