      subroutine bandwidth ( m, n, a, b, l, d, u )

c*********************************************************************72
c
cc BANDWIDTH returns the bandwidth of a matrix.
c
c  Discussion:
c
c    If the nonzeros of a matrix only occur in entries that are "close"
c    to the main diagonal, we say the matrix is banded.
c
c    Roughly speaking, the bandwidth B of a matrix is the number of
c    diagonals containing nonzeros.  More precisely, it is the minimum number
c    of contiguous diagonals that contain all the nonzeros.  It is presumed
c    that the main diagonal is nonzero.
c
c    We can also measure U and L, the upper and lower "half-bandwidths" which
c    count the number of contiguous diagonals above or below the main
c    diagonal.
c
c    We may write
c      B = L + D + U
c    where D is presumably 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), the matrix.
c
c    Output, integer B, the total bandwidth.
c
c    Output, integer L, D, U, the lower, diagonal, and upper
c    bandwidths.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer b
      integer d
      integer i
      integer j
      integer l
      integer u

      l = 0
      d = 0
      u = 0

      do i = 1, n

        j = 1

10      continue

        if ( l .lt. i - j ) then
          if ( a(i,j) .ne. 0.0D+00 ) then
            l = i - j
            go to 20
          end if
          j = j + 1
          go to 10
        end if

20      continue

        if ( a(i,i) .ne. 0.0D+00 ) then
          d = 1
        end if

        j = n

30      continue

        if ( u .lt. j - i ) then
          if ( a(i,j) .ne. 0.0D+00 ) then
            u = j - i
            go to 40
          end if
          j = j - 1
          go to 30
        end if

40      continue

      end do

      b = l + d + u

      return
      end
      subroutine cg_gb ( n, ml, mu, a, b, x )

c*********************************************************************72
c
cc CG_GB uses the conjugate gradient method for a general banded (GB) matrix.
c
c  Discussion:
c
c    The linear system has the form A*x=b, where A is a positive-definite
c    symmetric matrix.
c
c    The method is designed to reach the solution to the linear system
c      A * x = b
c    after N computational steps.  However, roundoff may introduce
c    unacceptably large errors for some problems.  In such a case,
c    calling the routine a second time, using the current solution estimate
c    as the new starting guess, should result in improved results.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Frank Beckman,
c    The Solution of Linear Equations by the Conjugate Gradient Method,
c    in Mathematical Methods for Digital Computers,
c    edited by John Ralston, Herbert Wilf,
c    Wiley, 1967,
c    ISBN: 0471706892,
c    LC: QA76.5.R3.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, integer ML, MU, the lower and upper bandwidths.
c
c    Input, double precision A(2*ML+MU+1,N), the band matrix.
c
c    Input, double precision B(N), the right hand side vector.
c
c    Input/output, double precision X(N).
c    On input, an estimate for the solution, which may be 0.
c    On output, the approximate solution vector.
c
      implicit none

      integer ml
      integer mu
      integer n

      double precision a(2*ml+mu+1,n)
      double precision alpha
      double precision ap(n)
      double precision b(n)
      double precision beta
      integer i
      integer it
      double precision p(n)
      double precision pap
      double precision pr
      double precision r(n)
      double precision r8vec_dot_product
      double precision rap
      double precision x(n)
c
c  Initialize
c    AP = A * x,
c    R  = b - A * x,
c    P  = b - A * x.
c
      call mv_gb ( n, n, ml, mu, a, x, ap )

      do i = 1, n
        r(i) = b(i) - ap(i)
      end do

      do i = 1, n
        p(i) = b(i) - ap(i)
      end do
c
c  Do the N steps of the conjugate gradient method.
c
      do it = 1, n
c
c  Compute the matrix*vector product AP = A*P.
c
        call mv_gb ( n, n, ml, mu, a, p, ap )
c
c  Compute the dot products
c    PAP = P*AP,
c    PR  = P*R
c  Set
c    ALPHA = PR / PAP.
c
        pap = r8vec_dot_product ( n, p, ap )
        pr = r8vec_dot_product ( n, p, r )

        if ( pap .eq. 0.0D+00 ) then
          return
        end if

        alpha = pr / pap
c
c  Set
c    X = X + ALPHA * P
c    R = R - ALPHA * AP.
c
        do i = 1, n
          x(i) = x(i) + alpha * p(i)
        end do

        do i = 1, n
          r(i) = r(i) - alpha * ap(i)
        end do
c
c  Compute the vector dot product
c    RAP = R*AP
c  Set
c    BETA = - RAP / PAP.
c
        rap = r8vec_dot_product ( n, r, ap )

        beta = - rap / pap
c
c  Update the perturbation vector
c    P = R + BETA * P.
c
        do i = 1, n
          p(i) = r(i) + beta * p(i)
        end do

      end do

      return
      end
      subroutine cg_ge ( n, a, b, x )

c*********************************************************************72
c
cc CG_GE uses the conjugate gradient method for a general storage (GE) matrix.
c
c  Discussion:
c
c    The linear system has the form A*x=b, where A is a positive-definite
c    symmetric matrix, stored as a full storage matrix.
c
c    The method is designed to reach the solution to the linear system
c      A * x = b
c    after N computational steps.  However, roundoff may introduce
c    unacceptably large errors for some problems.  In such a case,
c    calling the routine a second time, using the current solution estimate
c    as the new starting guess, should result in improved results.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Frank Beckman,
c    The Solution of Linear Equations by the Conjugate Gradient Method,
c    in Mathematical Methods for Digital Computers,
c    edited by John Ralston, Herbert Wilf,
c    Wiley, 1967,
c    ISBN: 0471706892,
c    LC: QA76.5.R3.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(N,N), the matrix.
c
c    Input, double precision B(N), the right hand side vector.
c
c    Input/output, double precision X(N).
c    On input, an estimate for the solution, which may be 0.
c    On output,  the approximate solution vector.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision alpha
      double precision ap(n)
      double precision b(n)
      double precision beta
      integer i
      integer it
      double precision p(n)
      double precision pap
      double precision pr
      double precision r(n)
      double precision r8vec_dot_product
      double precision rap
      double precision x(n)
c
c  Initialize
c    AP = A * x,
c    R  = b - A * x,
c    P  = b - A * x.
c
      call r8mat_mv ( n, n, a, x, ap )

      do i = 1, n
        r(i) = b(i) - ap(i)
      end do

      do i = 1, n
        p(i) = b(i) - ap(i)
      end do
c
c  Do the N steps of the conjugate gradient method.
c
      do it = 1, n
c
c  Compute the matrix*vector product AP = A*P.
c
        call r8mat_mv ( n, n, a, p, ap )
c
c  Compute the dot products
c    PAP = P*AP,
c    PR  = P*R
c  Set
c    ALPHA = PR / PAP.
c
        pap = r8vec_dot_product ( n, p, ap )
        pr = r8vec_dot_product ( n, p, r )

        if ( pap .eq. 0.0D+00 ) then
          return
        end if

        alpha = pr / pap
c
c  Set
c    X = X + ALPHA * P
c    R = R - ALPHA * AP.
c
        do i = 1, n
          x(i) = x(i) + alpha * p(i)
        end do

        do i = 1, n
          r(i) = r(i) - alpha * ap(i)
        end do
c
c  Compute the vector dot product
c    RAP = R*AP
c  Set
c    BETA = - RAP / PAP.
c
        rap = r8vec_dot_product ( n, r, ap )

        beta = - rap / pap
c
c  Update the perturbation vector
c    P = R + BETA * P.
c
        do i = 1, n
          p(i) = r(i) + beta * p(i)
        end do

      end do

      return
      end
      subroutine cg_st ( n, nz_num, row, col, a, b, x )

c*********************************************************************72
c
cc CG_ST uses the conjugate gradient method for a sparse triplet (ST) matrix.
c
c  Discussion:
c
c    The linear system has the form A*x=b, where A is a positive-definite
c    symmetric matrix, stored as a full storage matrix.
c
c    The method is designed to reach the solution to the linear system
c      A * x = b
c    after N computational steps.  However, roundoff may introduce
c    unacceptably large errors for some problems.  In such a case,
c    calling the routine a second time, using the current solution estimate
c    as the new starting guess, should result in improved results.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Frank Beckman,
c    The Solution of Linear Equations by the Conjugate Gradient Method,
c    in Mathematical Methods for Digital Computers,
c    edited by John Ralston, Herbert Wilf,
c    Wiley, 1967,
c    ISBN: 0471706892,
c    LC: QA76.5.R3.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, integer NZ_NUM, the number of nonzeros.
c
c    Input, integer ROW(NZ_NUM), COL(NZ_NUM), the row and column
c    indices of the nonzero entries.
c
c    Input, double precision A(NZ_NUM), the nonzero entries.
c
c    Input, double precision B(N), the right hand side vector.
c
c    Input/output, double precision X(N).
c    On input, an estimate for the solution, which may be 0.
c    On output, the approximate solution vector.
c
      implicit none

      integer n
      integer nz_num

      double precision a(nz_num)
      double precision alpha
      double precision ap(n)
      double precision b(n)
      double precision beta
      integer col(nz_num)
      integer i
      integer it
      double precision p(n)
      double precision pap
      double precision pr
      double precision r(n)
      double precision r8vec_dot_product
      double precision rap
      integer row(nz_num)
      double precision x(n)
c
c  Initialize
c    AP = A * x,
c    R  = b - A * x,
c    P  = b - A * x.
c
      call mv_st ( n, n, nz_num, row, col, a, x, ap )

      do i = 1, n
        r(i) = b(i) - ap(i)
      end do

      do i = 1, n
        p(i) = b(i) - ap(i)
      end do
c
c  Do the N steps of the conjugate gradient method.
c
      do it = 1, n
c
c  Compute the matrix*vector product AP = A*P.
c
        call mv_st ( n, n, nz_num, row, col, a, p, ap )
c
c  Compute the dot products
c    PAP = P*AP,
c    PR  = P*R
c  Set
c    ALPHA = PR / PAP.
c
        pap = r8vec_dot_product ( n, p, ap )
        pr = r8vec_dot_product ( n, p, r )

        if ( pap .eq. 0.0D+00 ) then
          return
        end if

        alpha = pr / pap
c
c  Set
c    X = X + ALPHA * P
c    R = R - ALPHA * AP.
c
        do i = 1, n
          x(i) = x(i) + alpha * p(i)
        end do

        do i = 1, n
          r(i) = r(i) - alpha * ap(i)
        end do
c
c  Compute the vector dot product
c    RAP = R*AP
c  Set
c    BETA = - RAP / PAP.
c
        rap = r8vec_dot_product ( n, r, ap )

        beta = - rap / pap
c
c  Update the perturbation vector
c    P = R + BETA * P.
c
        do i = 1, n
          p(i) = r(i) + beta * p(i)
        end do

      end do

      return
      end
      subroutine daxpy ( n, da, dx, incx, dy, incy )

c*********************************************************************72
c
cc DAXPY computes constant times a vector plus a vector.
c
c  Discussion:
c
c    This routine uses double precision real arithmetic.
c
c    This routine uses unrolled loops for increments equal to one.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2008
c
c  Author:
c
c    Jack Dongarra
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
c    Basic Linear Algebra Subprograms for FORTRAN usage,
c    ACM Transactions on Mathematical Software,
c    Volume 5, Number 3, pages 308-323, 1979.
c
c  Parameters:
c
c    Input, integer N, the number of elements in DX and DY.
c
c    Input, double precision DA, the multiplier of DX.
c
c    Input, double precision DX(*), the first vector.
c
c    Input, integer INCX, the increment between successive entries of DX.
c
c    Input/output, double precision DY(*), the second vector.
c    On output, DY(*) has been replaced by DY(*) + DA * DX(*).
c
c    Input, integer INCY, the increment between successive entries of DY.
c
      implicit none

      double precision da
      double precision dx(*)
      double precision dy(*)
      integer i
      integer incx
      integer incy
      integer ix
      integer iy
      integer m
      integer n

      if ( n .le. 0 ) then
        return
      end if

      if ( da .eq. 0.0d0 ) then
        return
      end if

      if ( incx .ne. 1 .or. incy .ne. 1 ) then

        ix = 1
        iy = 1
        if ( incx .lt. 0 ) then
          ix = (-n+1) * incx + 1
        end if

        if ( incy .lt. 0 ) then
          iy = (-n+1) * incy + 1
        end if

        do i = 1, n
          dy(iy) = dy(iy) + da * dx(ix)
          ix = ix + incx
          iy = iy + incy
        end do

      else

        m = mod ( n, 4 )

        do i = 1, m
          dy(i) = dy(i) + da * dx(i)
        end do

        do i = m + 1, n, 4
          dy(i) = dy(i) + da * dx(i)
          dy(i + 1) = dy(i + 1) + da * dx(i + 1)
          dy(i + 2) = dy(i + 2) + da * dx(i + 2)
          dy(i + 3) = dy(i + 3) + da * dx(i + 3)
        end do

      end if

      return
      end
      function ddot ( n, dx, incx, dy, incy )

c*********************************************************************72
c
cc DDOT forms the dot product of two vectors.
c
c  Discussion:
c
c    This routine uses double precision real arithmetic.
c
c    This routine uses unrolled loops for increments equal to one.
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
c    This version by John Burkardt
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
c    Basic Linear Algebra Subprograms for FORTRAN usage,
c    ACM Transactions on Mathematical Software,
c    Volume 5, Number 3, pages 308-323, 1979.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vectors.
c
c    Input, double precision DX(*), the first vector.
c
c    Input, integer INCX, the increment between successive entries in DX.
c
c    Input, double precision DY(*), the second vector.
c
c    Input, integer INCY, the increment between successive entries in DY.
c
c    Output, double precision DDOT, the sum of the product of the 
c    corresponding entries of DX and DY.
c
      implicit none

      double precision ddot
      double precision dx(*)
      double precision dy(*)
      double precision dtemp
      integer i
      integer incx
      integer incy
      integer ix
      integer iy
      integer m
      integer n

      ddot = 0.0D+00
      dtemp = 0.0D+00

      if ( n .le. 0 ) then
        return
      end if

      if ( incx .eq. 1 .and. incy .eq. 1 ) then

        m = mod ( n, 5 )

        do i = 1, m
          dtemp = dtemp + dx(i) * dy(i)
        end do

        do i = m + 1, n, 5
          dtemp = dtemp 
     &      + dx(i)   * dy(i) 
     &      + dx(i+1) * dy(i+1) 
     &      + dx(i+2) * dy(i+2) 
     &      + dx(i+3) * dy(i+3) 
     &      + dx(i+4) * dy(i+4)
        end do
c
c  Code for unequal increments or equal increments not equal to 1
c
      else

        if ( incx .lt. 0 ) then
          ix = ( - n + 1 ) * incx + 1
        else
          ix = 1
        end if

        if ( incy .lt. 0 ) then
          iy = ( - n + 1 ) * incy + 1
        else
          iy = 1
        end if

        do i = 1, n
          dtemp = dtemp + dx(ix) * dy(iy)
          ix = ix + incx
          iy = iy + incy
        end do

      end if

      ddot = dtemp

      return
      end
      subroutine dgbfa ( abd, lda, n, ml, mu, ipvt, info )

c*********************************************************************72
c
cc DGBFA factors a double precision band matrix by elimination.
c
c     dgbfa is usually called by dgbco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c
c     on entry
c
c        abd     double precision(lda, n)
c                contains the matrix in band storage.  the columns
c                of the matrix are stored in the columns of  abd  and
c                the diagonals of the matrix are stored in rows
c                ml+1 through 2*ml+mu+1 of  abd .
c                see the comments below for details.
c
c        lda     integer
c                the leading dimension of the array  abd .
c                lda must be .ge. 2*ml + mu + 1 .
c
c        n       integer
c                the order of the original matrix.
c
c        ml      integer
c                number of diagonals below the main diagonal.
c                0 .le. ml .lt. n .
c
c        mu      integer
c                number of diagonals above the main diagonal.
c                0 .le. mu .lt. n .
c                more efficient if  ml .le. mu .
c     on return
c
c        abd     an upper triangular matrix in band storage and
c                the multipliers which were used to obtain it.
c                the factorization can be written  a = l*u  where
c                l  is a product of permutation and unit lower
c                triangular matrices and  u  is upper triangular.
c
c        ipvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if  u(k,k) .eq. 0.0 .  this is not an error
c                     condition for this subroutine, but it does
c                     indicate that dgbsl will divide by zero if
c                     called.  use  rcond  in dgbco for a reliable
c                     indication of singularity.
c
c     band storage
c
c           if  a  is a band matrix, the following program segment
c           will set up the input.
c
c                   ml = (band width below the diagonal)
c                   mu = (band width above the diagonal)
c                   m = ml + mu + 1
c                   do 20 j = 1, n
c                      i1 = max0(1, j-mu)
c                      i2 = min0(n, j+ml)
c                      do 10 i = i1, i2
c                         k = i - j + m
c                         abd(k,j) = a(i,j)
c                10    continue
c                20 continue
c
c           this uses rows  ml+1  through  2*ml+mu+1  of  abd .
c           in addition, the first  ml  rows in  abd  are used for
c           elements generated during the triangularization.
c           the total number of rows needed in  abd  is  2*ml+mu+1 .
c           the  ml+mu by ml+mu  upper left triangle and the
c           ml by ml  lower right triangle are not referenced.
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
      integer lda,n,ml,mu,ipvt(1),info
      double precision abd(lda,1)
      double precision t
      integer i,idamax,i0,j,ju,jz,j0,j1,k,kp1,l,lm,m,mm,nm1
c
c
      m = ml + mu + 1
      info = 0
c
c     zero initial fill-in columns
c
      j0 = mu + 2
      j1 = min0(n,m) - 1
      if (j1 .lt. j0) go to 30
      do 20 jz = j0, j1
         i0 = m + 1 - jz
         do 10 i = i0, ml
            abd(i,jz) = 0.0d0
   10    continue
   20 continue
   30 continue
      jz = j1
      ju = 0
c
c     gaussian elimination with partial pivoting
c
      nm1 = n - 1
      if (nm1 .lt. 1) go to 130
      do 120 k = 1, nm1
         kp1 = k + 1
c
c        zero next fill-in column
c
         jz = jz + 1
         if (jz .gt. n) go to 50
         if (ml .lt. 1) go to 50
            do 40 i = 1, ml
               abd(i,jz) = 0.0d0
   40       continue
   50    continue
c
c        find l = pivot index
c
         lm = min0(ml,n-k)
         l = idamax(lm+1,abd(m,k),1) + m - 1
         ipvt(k) = l + k - m
c
c        zero pivot implies this column already triangularized
c
         if (abd(l,k) .eq. 0.0d0) go to 100
c
c           interchange if necessary
c
            if (l .eq. m) go to 60
               t = abd(l,k)
               abd(l,k) = abd(m,k)
               abd(m,k) = t
   60       continue
c
c           compute multipliers
c
            t = -1.0d0/abd(m,k)
            call dscal(lm,t,abd(m+1,k),1)
c
c           row elimination with column indexing
c
            ju = min0(max0(ju,mu+ipvt(k)),n)
            mm = m
            if (ju .lt. kp1) go to 90
            do 80 j = kp1, ju
               l = l - 1
               mm = mm - 1
               t = abd(l,j)
               if (l .eq. mm) go to 70
                  abd(l,j) = abd(mm,j)
                  abd(mm,j) = t
   70          continue
               call daxpy(lm,t,abd(m+1,k),1,abd(mm+1,j),1)
   80       continue
   90       continue
         go to 110
  100    continue
            info = k
  110    continue
  120 continue
  130 continue
      ipvt(n) = n
      if (abd(m,n) .eq. 0.0d0) info = n
      return
      end
      subroutine dgbsl ( abd, lda, n, ml, mu, ipvt, b, job )

c*********************************************************************72
c
cc DGBSL solves a real banded system factored by DGBCO or DGBFA.
c
c     dgbsl solves the double precision band system
c     a * x = b  or  trans(a) * x = b
c     using the factors computed by dgbco or dgbfa.
c
c     on entry
c
c        abd     double precision(lda, n)
c                the output from dgbco or dgbfa.
c
c        lda     integer
c                the leading dimension of the array  abd .
c
c        n       integer
c                the order of the original matrix.
c
c        ml      integer
c                number of diagonals below the main diagonal.
c
c        mu      integer
c                number of diagonals above the main diagonal.
c
c        ipvt    integer(n)
c                the pivot vector from dgbco or dgbfa.
c
c        b       double precision(n)
c                the right hand side vector.
c
c        job     integer
c                = 0         to solve  a*x = b ,
c                = nonzero   to solve  trans(a)*x = b , where
c                            trans(a)  is the transpose.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains a
c        zero on the diagonal.  technically this indicates singularity
c        but it is often caused by improper arguments or improper
c        setting of lda .  it will not occur if the subroutines are
c        called correctly and if dgbco has set rcond .gt. 0.0
c        or dgbfa has set info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dgbco(abd,lda,n,ml,mu,ipvt,rcond,z)
c           if (rcond is too small) go to ...
c           do 10 j = 1, p
c              call dgbsl(abd,lda,n,ml,mu,ipvt,c(1,j),0)
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
      integer lda,n,ml,mu,ipvt(1),job
      double precision abd(lda,1),b(1)
      double precision ddot,t
      integer k,kb,l,la,lb,lm,m,nm1

      m = mu + ml + 1
      nm1 = n - 1
      if (job .ne. 0) go to 50
c
c        job = 0 , solve  a * x = b
c        first solve l*y = b
c
         if (ml .eq. 0) go to 30
         if (nm1 .lt. 1) go to 30
            do 20 k = 1, nm1
               lm = min0(ml,n-k)
               l = ipvt(k)
               t = b(l)
               if (l .eq. k) go to 10
                  b(l) = b(k)
                  b(k) = t
   10          continue
               call daxpy(lm,t,abd(m+1,k),1,b(k+1),1)
   20       continue
   30    continue
c
c        now solve  u*x = y
c
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/abd(m,k)
            lm = min0(k,m) - 1
            la = m - lm
            lb = k - lm
            t = -b(k)
            call daxpy(lm,t,abd(la,k),1,b(lb),1)
   40    continue
      go to 100
   50 continue
c
c        job = nonzero, solve  trans(a) * x = b
c        first solve  trans(u)*y = b
c
         do 60 k = 1, n
            lm = min0(k,m) - 1
            la = m - lm
            lb = k - lm
            t = ddot(lm,abd(la,k),1,b(lb),1)
            b(k) = (b(k) - t)/abd(m,k)
   60    continue
c
c        now solve trans(l)*x = y
c
         if (ml .eq. 0) go to 90
         if (nm1 .lt. 1) go to 90
            do 80 kb = 1, nm1
               k = n - kb
               lm = min0(ml,n-k)
               b(k) = b(k) + ddot(lm,abd(m+1,k),1,b(k+1),1)
               l = ipvt(k)
               if (l .eq. k) go to 70
                  t = b(l)
                  b(l) = b(k)
                  b(k) = t
   70          continue
   80       continue
   90    continue
  100 continue
      return
      end
      subroutine dgefa ( a, lda, n, ipvt, info )

c*********************************************************************72
c
cc DGEFA factors a double precision matrix by gaussian elimination.
c
c     dgefa is usually called by dgeco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the matrix to be factored.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix and the multipliers
c                which were used to obtain it.
c                the factorization can be written  a = l*u  where
c                l  is a product of permutation and unit lower
c                triangular matrices and  u  is upper triangular.
c
c        ipvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if  u(k,k) .eq. 0.0 .  this is not an error
c                     condition for this subroutine, but it does
c                     indicate that dgesl or dgedi will divide by zero
c                     if called.  use  rcond  in dgeco for a reliable
c                     indication of singularity.
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
      integer lda,n,ipvt(1),info
      double precision a(lda,1)
      double precision t
      integer idamax,j,k,kp1,l,nm1
c
c     gaussian elimination with partial pivoting
c
      info = 0
      nm1 = n - 1
      if (nm1 .lt. 1) go to 70
      do 60 k = 1, nm1
         kp1 = k + 1
c
c        find l = pivot index
c
         l = idamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k) = l
c
c        zero pivot implies this column already triangularized
c
         if (a(l,k) .eq. 0.0d0) go to 40
c
c           interchange if necessary
c
            if (l .eq. k) go to 10
               t = a(l,k)
               a(l,k) = a(k,k)
               a(k,k) = t
   10       continue
c
c           compute multipliers
c
            t = -1.0d0/a(k,k)
            call dscal(n-k,t,a(k+1,k),1)
c
c           row elimination with column indexing
c
            do 30 j = kp1, n
               t = a(l,j)
               if (l .eq. k) go to 20
                  a(l,j) = a(k,j)
                  a(k,j) = t
   20          continue
               call daxpy(n-k,t,a(k+1,k),1,a(k+1,j),1)
   30       continue
         go to 50
   40    continue
            info = k
   50    continue
   60 continue
   70 continue
      ipvt(n) = n
      if (a(n,n) .eq. 0.0d0) info = n
      return
      end
      subroutine dgesl ( a, lda, n, ipvt, b, job )

c*********************************************************************72
c
cc DGESL solves a linear system factored by DGEFA.
c
c  DGESL solves the double precision system
c     a * x = b  or  trans(a) * x = b
c     using the factors computed by dgeco or dgefa.
c
c     on entry
c
c        a       double precision(lda, n)
c                the output from dgeco or dgefa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        ipvt    integer(n)
c                the pivot vector from dgeco or dgefa.
c
c        b       double precision(n)
c                the right hand side vector.
c
c        job     integer
c                = 0         to solve  a*x = b ,
c                = nonzero   to solve  trans(a)*x = b  where
c                            trans(a)  is the transpose.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains a
c        zero on the diagonal.  technically this indicates singularity
c        but it is often caused by improper arguments or improper
c        setting of lda .  it will not occur if the subroutines are
c        called correctly and if dgeco has set rcond .gt. 0.0
c        or dgefa has set info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dgeco(a,lda,n,ipvt,rcond,z)
c           if (rcond is too small) go to ...
c           do 10 j = 1, p
c              call dgesl(a,lda,n,ipvt,c(1,j),0)
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
      integer lda,n,ipvt(1),job
      double precision a(lda,1),b(1)
      double precision ddot,t
      integer k,kb,l,nm1
c
      nm1 = n - 1
      if (job .ne. 0) go to 50
c
c        job = 0 , solve  a * x = b
c        first solve  l*y = b
c
         if (nm1 .lt. 1) go to 30
         do 20 k = 1, nm1
            l = ipvt(k)
            t = b(l)
            if (l .eq. k) go to 10
               b(l) = b(k)
               b(k) = t
   10       continue
            call daxpy(n-k,t,a(k+1,k),1,b(k+1),1)
   20    continue
   30    continue
c
c        now solve  u*x = y
c
         do kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/a(k,k)
            t = -b(k)
            call daxpy(k-1,t,a(1,k),1,b(1),1)
         end do
      go to 100
   50 continue
c
c        job = nonzero, solve  trans(a) * x = b
c        first solve  trans(u)*y = b
c
         do k = 1, n
            t = ddot(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k) - t)/a(k,k)
         end do
c
c        now solve trans(l)*x = y
c
         if (nm1 .lt. 1) go to 90
         do 80 kb = 1, nm1
            k = n - kb
            b(k) = b(k) + ddot(n-k,a(k+1,k),1,b(k+1),1)
            l = ipvt(k)
            if (l .eq. k) go to 70
               t = b(l)
               b(l) = b(k)
               b(k) = t
   70       continue
   80    continue
   90    continue
  100 continue
      return
      end
      subroutine dscal ( n, da, dx, incx )

c*********************************************************************72
c
cc DSCAL scales a vector by a constant.
c
c  Discussion:
c
c    This routine uses double precision real arithmetic.
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
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
c    Basic Linear Algebra Subprograms for FORTRAN usage,
c    ACM Transactions on Mathematical Software,
c    Volume 5, Number 3, pages 308-323, 1979.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision SA, the multiplier.
c
c    Input/output, double precision X(*), the vector to be scaled.
c
c    Input, integer INCX, the increment between successive entries of X.
c
      implicit none

      double precision da
      double precision dx(*)
      integer i
      integer incx
      integer m
      integer n
      integer nincx

      if ( n .le. 0 ) then
        return
      end if

      if ( incx .le. 0 ) then
        return
      end if

      if ( incx .eq. 1 ) then

        m = mod ( n, 5 )

        do i = 1, m
          dx(i) = da * dx(i)
        end do

        do i = m + 1, n, 5
          dx(i) =     da * dx(i)
          dx(i+1) = da * dx(i+1)
          dx(i+2) = da * dx(i+2)
          dx(i+3) = da * dx(i+3)
          dx(i+4) = da * dx(i+4)
        end do

      else

        nincx = n * incx
        do i = 1, nincx, incx
          dx(i) = da * dx(i)
        end do
      end if

      return
      end
      function idamax ( n, dx, incx )

c*********************************************************************72
c
cc IDAMAX finds the index of element having maximum absolute value.
c
c  Discussion:
c
c    This routine uses double precision real arithmetic.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 July 2007
c
c  Author:
c
c    Jack Dongarra
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
c    Basic Linear Algebra Subprograms for FORTRAN usage,
c    ACM Transactions on Mathematical Software,
c    Volume 5, Number 3, pages 308-323, 1979.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision X(*), the vector to be examined.
c
c    Input, integer INCX, the increment between successive entries of SX.
c
c    Output, integer IDAMAX, the index of the element of SX of maximum
c    absolute value.
c
      implicit none

      double precision dmax
      double precision dx(*)
      integer i
      integer idamax
      integer incx
      integer ix
      integer n

      idamax = 0
      if ( n .lt. 1 .or. incx .le. 0 ) then
        return
      end if

      idamax = 1
      if ( n .eq. 1 ) then
        return
      end if

      if ( incx .eq. 1 ) then

        dmax = dabs ( dx(1) )
        do i = 2, n
          if( dmax .lt. dabs ( dx(i) ) ) then
            idamax = i
            dmax = dabs ( dx(i) )
          end if
        end do

      else

        ix = 1
        dmax = dabs ( dx(1) )
        ix = ix + incx
        do  i = 2, n
          if ( dmax .lt. dabs ( dx(ix) ) ) then
            idamax = i
            dmax = dabs ( dx(ix) )
          end if
          ix = ix + incx
        end do

      end if

      return
      end
      subroutine mv_gb ( m, n, ml, mu, a, x, b )

c*********************************************************************72
c
cc MV_GB multiplies a banded matrix by an R8VEC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows of the matrix.
c    M must be positive.
c
c    Input, integer N, the number of columns of the matrix.
c    N must be positive.
c
c    Input, integer ML, MU, the lower and upper bandwidths.
c
c    Input, double precision A(2*ML+MU+1,N), the matrix.
c
c    Input, double precision X(N), the vector to be multiplied by A.
c
c    Output, double precision B(M), the product A * x.
c
      implicit none

      integer m
      integer ml
      integer mu
      integer n

      double precision a(2*ml+mu+1,n)
      double precision b(m)
      integer i
      integer j
      integer jhi
      integer jlo
      double precision x(n)

      do i = 1, m
        b(i) = 0.0D+00
      end do

      do i = 1, n
        jlo = max ( 1, i - ml )
        jhi = min ( n, i + mu )
        do j = jlo, jhi
          b(i) = b(i) + a(i-j+ml+mu+1,j) * x(j)
        end do
      end do

      return
      end
      subroutine mv_ge ( m, n, a, x, b )

c*********************************************************************72
c
cc MV_GE multiplies an R8GE matrix by an R8VEC.
c
c  Discussion:
c
c    The R8GE storage format is used for a general M by N matrix.  A storage
c    space is made for each entry.  The two dimensional logical
c    array can be thought of as a vector of M*N entries, starting with
c    the M entries in the column 1, then the M entries in column 2
c    and so on.  Considered as a vector, the entry A(I,J) is then stored
c    in vector location I+(J-1)*M.
c
c    R8GE storage is used by LINPACK and LAPACK.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows of the matrix.
c    M must be positive.
c
c    Input, integer N, the number of columns of the matrix.
c    N must be positive.
c
c    Input, double precision A(M,N), the R8GE matrix.
c
c    Input, double precision X(N), the vector to be multiplied by A.
c
c    Output, double precision B(M), the product A * x.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision b(m)
      integer i
      integer j
      double precision x(n)

      do i = 1, m
        b(i) = 0.0D+00
        do j = 1, n
          b(i) = b(i) + a(i,j) * x(j)
        end do
      end do

      return
      end
      subroutine mv_st ( m, n, nz_num, row, col, a, x, b )

c*********************************************************************72
c
cc MV_ST multiplies a sparse triple matrix times a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer NZ_NUM, the number of nonzero values.
c
c    Input, integer ROW(NZ_NUM), COL(NZ_NUM), the row and
c    column indices.
c
c    Input, double precision A(NZ_NUM), the nonzero values in the matrix.
c
c    Input, double precision X(N), the vector to be multiplied.
c
c    Output, double precision B(M), the product A*X.
c
      implicit none

      integer m
      integer n
      integer nz_num

      double precision a(nz_num)
      double precision b(m)
      integer col(nz_num)
      integer i
      integer k
      integer row(nz_num)
      double precision x(n)

      do i = 1, m
        b(i) = 0.0D+00
      end do

      do k = 1, nz_num
        b(row(k)) = b(row(k)) + a(k) * x(col(k))
      end do

      return
      end
      subroutine nonzeros ( m, n, a, nnz )

c*********************************************************************72
c
cc NONZEROS counts the nonzeros in a matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), the matrix.
c
c    Output, integer NNZ, the number of nonzero entries.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      integer nnz

      nnz = 0
      do j = 1, n
        do i = 1, m
          if ( a(i,j) .ne. 0.0D+00 ) then
            nnz = nnz + 1
          end if
        end do
      end do

      return
      end
      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a pseudorandom R8 scaled to [0,1].
c
c  Discussion:
c
c    This routine implements the recursion
c
c      seed = 16807 * seed mod ( 2^31 - 1 )
c      r8_uniform_01 = seed / ( 2^31 - 1 )
c
c    The integer arithmetic never requires more than 32 bits,
c    including a sign bit.
c
c    If the initial seed is 12345, then the first three computations are
c
c      Input     Output      R8_UNIFORM_01
c      SEED      SEED
c
c         12345   207482415  0.096616
c     207482415  1790989824  0.833995
c    1790989824  2035175616  0.947702
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley Interscience, page 95, 1998.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      double precision r8_uniform_01
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine r8mat_uniform_01 ( m, n, seed, r )

c*********************************************************************72
c
cc R8MAT_UNIFORM_01 returns a unit pseudorandom R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns in the array.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(M,N), the array of pseudorandom values.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      integer k
      integer seed
      double precision r(m,n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do j = 1, n

        do i = 1, m

          k = seed / 127773

          seed = 16807 * ( seed - k * 127773 ) - k * 2836

          if ( seed .lt. 0 ) then
            seed = seed + 2147483647
          end if

          r(i,j) = dble ( seed ) * 4.656612875D-10

        end do
      end do

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
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
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
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do

      return
      end
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

      end do

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
      subroutine wathen_bandwidth ( nx, ny, l, d, u )

c*********************************************************************72
c
cc WATHEN_BANDWIDTH returns the bandwidth of the WATHEN matrix.
c
c  Discussion:
c
c    The bandwidth measures the minimal number of contiguous diagonals,
c    including the central diagonal, which contain all the nonzero elements
c    of a matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Higham,
c    Algorithm 694: A Collection of Test Matrices in MATLAB,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 3, September 1991, pages 289-305.
c
c    Andrew Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size of A.
c
c    Output, integer L, D, U, the lower, diagonal, and upper
c    bandwidths of the matrix,
c
      implicit none

      integer d
      integer l
      integer nx
      integer ny
      integer u

      l = 3 * nx + 4
      d = 1
      u = 3 * nx + 4

      return
      end
      subroutine wathen_gb ( nx, ny, n, seed, a )

c*********************************************************************72
c
cc WATHEN_GB returns the Wathen matrix, using general banded (GB) storage.
c
c  Discussion:
c
c    The Wathen matrix is a finite element matrix which is sparse.
c
c    The entries of the matrix depend in part on a physical quantity
c    related to density.  That density is here assigned random values between
c    0 and 100.
c
c    The matrix order N is determined by the input quantities NX and NY,
c    which would usually be the number of elements in the X and Y directions.
c    The value of N is
c
c      N = 3*NX*NY + 2*NX + 2*NY + 1,
c
c    The matrix is the consistent mass matrix for a regular NX by NY grid
c    of 8 node serendipity elements.
c
c    The local element numbering is
c
c      3--2--1
c      |     |
c      4     8
c      |     |
c      5--6--7
c
c    Here is an illustration for NX = 3, NY = 2:
c
c     23-24-25-26-27-28-29
c      |     |     |     |
c     19    20    21    22
c      |     |     |     |
c     12-13-14-15-16-17-18
c      |     |     |     |
c      8     9    10    11
c      |     |     |     |
c      1--2--3--4--5--6--7
c
c    For this example, the total number of nodes is, as expected,
c
c      N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
c
c    The matrix is symmetric positive definite for any positive values of the
c    density RHO(X,Y).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Higham,
c    Algorithm 694: A Collection of Test Matrices in MATLAB,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 3, September 1991, pages 289-305.
c
c    Andrew Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, Number 4, October 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size
c    of the matrix.
c
c    Input, integer N, the number of rows and columns.
c
c    Input/output, integer SEED, the random number seed.
c
c    Output, double precision A(9*NX+13,N), the matrix.
c
      implicit none

      integer n
      integer nx
      integer ny

      double precision a(9*nx+13,n)
      double precision em(8,8)
      integer i
      integer ii
      integer j
      integer jj
      integer kcol
      integer krow
      integer ml
      integer mu
      integer node(8)
      double precision r8_uniform_01
      double precision rho
      integer seed

      save em

      data em /
     &   6.0, -6.0,  2.0, -8.0,  3.0, -8.0,  2.0, -6.0,
     &  -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, -8.0, 20.0,
     &   2.0, -6.0,  6.0, -6.0,  2.0, -8.0,  3.0, -8.0,
     &  -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, -8.0, 16.0,
     &   3.0, -8.0,  2.0, -6.0,  6.0, -6.0,  2.0, -8.0,     
     &  -8.0, 16.0, -8.0, 20.0, -6.0, 32.0, -6.0, 20.0,
     &   2.0, -8.0,  3.0, -8.0,  2.0, -6.0,  6.0, -6.0,
     &  -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, -6.0, 32.0 /

      ml = 3 * nx + 4
      mu = 3 * nx + 4

      do j = 1, n 
        do i = 1, 9 * nx + 13
          a(i,j) = 0.0D+00
        end do
      end do

      do j = 1, ny
        do i = 1, nx

          node(1) = 3 * j * nx + 2 * j + 2 * i + 1
          node(2) = node(1) - 1
          node(3) = node(1) - 2
          node(4) = ( 3 * j - 1 ) * nx + 2 * j + i - 1
          node(5) = ( 3 * j - 3 ) * nx + 2 * j + 2 * i - 3
          node(6) = node(5) + 1
          node(7) = node(5) + 2
          node(8) = node(4) + 1

          rho = r8_uniform_01 ( seed )

          do krow = 1, 8
            do kcol = 1, 8
              ii = node(krow);
              jj = node(kcol);
              a(ii-jj+ml+mu+1,jj) = a(ii-jj+ml+mu+1,jj)
     &          + rho(i,j) * em(krow,kcol)
            end do
          end do

        end do
      end do

      return
      end
      subroutine wathen_ge ( nx, ny, n, seed, a )

c*********************************************************************72
c
cc WATHEN_GE returns the Wathen matrix as a general storage (GE) matrix.
c
c  Discussion:
c
c    The Wathen matrix is a finite element matrix which is sparse.
c
c    The entries of the matrix depend in part on a physical quantity
c    related to density.  That density is here assigned random values between
c    0 and 100.
c
c    The matrix order N is determined by the input quantities NX and NY,
c    which would usually be the number of elements in the X and Y directions.
c    The value of N is
c
c      N = 3*NX*NY + 2*NX + 2*NY + 1,
c
c    The matrix is the consistent mass matrix for a regular NX by NY grid
c    of 8 node serendipity elements.
c
c    The local element numbering is
c
c      3--2--1
c      |     |
c      4     8
c      |     |
c      5--6--7
c
c    Here is an illustration for NX = 3, NY = 2:
c
c     23-24-25-26-27-28-29
c      |     |     |     |
c     19    20    21    22
c      |     |     |     |
c     12-13-14-15-16-17-18
c      |     |     |     |
c      8     9    10    11
c      |     |     |     |
c      1--2--3--4--5--6--7
c
c    For this example, the total number of nodes is, as expected,
c
c      N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
c
c    The matrix is symmetric positive definite for any positive values of the
c    density RHO(X,Y).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Higham,
c    Algorithm 694: A Collection of Test Matrices in MATLAB,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 3, September 1991, pages 289-305.
c
c    Andrew Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, Number 4, October 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size
c    of the matrix.
c
c    Input, integer N, the number of rows and columns.
c
c    Input/output, integer SEED, the random number seed.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n
      integer nx
      integer ny

      double precision a(n,n)
      double precision em(8,8)
      integer i
      integer j
      integer kcol
      integer krow
      integer node(8)
      double precision r8_uniform_01
      double precision rho
      integer seed

      save em

      data em /
     &   6.0, -6.0,  2.0, -8.0,  3.0, -8.0,  2.0, -6.0,
     &  -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, -8.0, 20.0,
     &   2.0, -6.0,  6.0, -6.0,  2.0, -8.0,  3.0, -8.0,
     &  -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, -8.0, 16.0,
     &   3.0, -8.0,  2.0, -6.0,  6.0, -6.0,  2.0, -8.0,     
     &  -8.0, 16.0, -8.0, 20.0, -6.0, 32.0, -6.0, 20.0,
     &   2.0, -8.0,  3.0, -8.0,  2.0, -6.0,  6.0, -6.0,
     &  -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, -6.0, 32.0 /

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      do j = 1, ny
        do i = 1, nx

          node(1) = 3 * j * nx + 2 * j + 2 * i + 1
          node(2) = node(1) - 1
          node(3) = node(1) - 2
          node(4) = ( 3 * j - 1 ) * nx + 2 * j + i - 1
          node(5) = ( 3 * j - 3 ) * nx + 2 * j + 2 * i - 3
          node(6) = node(5) + 1
          node(7) = node(5) + 2
          node(8) = node(4) + 1

          rho = 100.0 * r8_uniform_01 ( seed )

          do krow = 1, 8
            do kcol = 1, 8
              a(node(krow),node(kcol)) = a(node(krow),node(kcol))       
     &          + rho * em(krow,kcol)
            end do
          end do

        end do
      end do

      return
      end
      subroutine wathen_order ( nx, ny, n )

c*********************************************************************72
c
cc WATHEN_ORDER returns the order of the WATHEN matrix.
c
c  Discussion:
c
c    N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Higham,
c    Algorithm 694: A Collection of Test Matrices in MATLAB,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 3, September 1991, pages 289-305.
c
c    Andrew Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size of A.
c
c    Output, integer N, the order of the matrix,
c    as determined by NX and NY.
c
      implicit none

      integer n
      integer nx
      integer ny

      n = 3 * nx * ny + 2 * nx + 2 * ny + 1

      return
      end
      subroutine wathen_st ( nx, ny, nz_num, seed, row, col, a )

c*********************************************************************72
c
cc WATHEN_ST: Wathen matrix stored in sparse triplet (ST) format.
c
c  Discussion:
c
c    When dealing with sparse matrices in MATLAB, it can be much more efficient
c    to work first with a triple of I, J, and X vectors, and only once
c    they are complete, convert to MATLAB's sparse format.
c
c    The Wathen matrix is a finite element matrix which is sparse.
c
c    The entries of the matrix depend in part on a physical quantity
c    related to density.  That density is here assigned random values between
c    0 and 100.
c
c    The matrix order N is determined by the input quantities NX and NY,
c    which would usually be the number of elements in the X and Y directions.
c
c    The value of N is
c
c      N = 3*NX*NY + 2*NX + 2*NY + 1,
c
c    The matrix is the consistent mass matrix for a regular NX by NY grid
c    of 8 node serendipity elements.
c
c    The local element numbering is
c
c      3--2--1
c      |     |
c      4     8
c      |     |
c      5--6--7
c
c    Here is an illustration for NX = 3, NY = 2:
c
c     23-24-25-26-27-28-29
c      |     |     |     |
c     19    20    21    22
c      |     |     |     |
c     12-13-14-15-16-17-18
c      |     |     |     |
c      8     9    10    11
c      |     |     |     |
c      1--2--3--4--5--6--7
c
c    For this example, the total number of nodes is, as expected,
c
c      N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
c
c    The matrix is symmetric positive definite for any positive values of the
c    density RHO(X,Y).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 July 2014
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Nicholas Higham,
c    Algorithm 694: A Collection of Test Matrices in MATLAB,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 3, September 1991, pages 289-305.
c
c    Andrew Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, Number 4, October 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size of
c    the matrix.
c
c    Input, integer NZ_NUM, the number of values used to
c    describe the matrix.
c
c    Input/output, integer SEED, the random number seed.
c
c    Output, integer ROW(NZ_NUM), COL(NZ_NUM), the row and
c    column indices of the nonzero entries.
c
c    Output, double precision A(NZ_NUM), the nonzero entries of the matrix.
c
      implicit none

      integer nx
      integer ny
      integer nz_num

      double precision a(nz_num)
      integer col(nz_num)
      double precision em(8,8)
      integer i
      integer j
      integer k
      integer kcol
      integer krow
      integer node(8)
      double precision r8_uniform_01
      double precision rho
      integer row(nz_num)
      integer seed

      save em

      data em /
     &   6.0, -6.0,  2.0, -8.0,  3.0, -8.0,  2.0, -6.0,
     &  -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, -8.0, 20.0,
     &   2.0, -6.0,  6.0, -6.0,  2.0, -8.0,  3.0, -8.0,
     &  -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, -8.0, 16.0,
     &   3.0, -8.0,  2.0, -6.0,  6.0, -6.0,  2.0, -8.0,     
     &  -8.0, 16.0, -8.0, 20.0, -6.0, 32.0, -6.0, 20.0,
     &   2.0, -8.0,  3.0, -8.0,  2.0, -6.0,  6.0, -6.0,
     &  -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, -6.0, 32.0 /

      do k = 1, nz_num
        row(k) = 0
        col(k) = 0
        a(k) = 0.0D+00
      end do

      k = 0

      do j = 1, ny
        do i = 1, nx

          node(1) = 3 * j * nx + 2 * j + 2 * i + 1
          node(2) = node(1) - 1
          node(3) = node(1) - 2
          node(4) = ( 3 * j - 1 ) * nx + 2 * j + i - 1
          node(5) = ( 3 * j - 3 ) * nx + 2 * j + 2 * i - 3
          node(6) = node(5) + 1
          node(7) = node(5) + 2
          node(8) = node(4) + 1

          rho = 100.0 * r8_uniform_01 ( seed )

          do krow = 1, 8
            do kcol = 1, 8
              k = k + 1
              row(k) = node(krow)
              col(k) = node(kcol)
              a(k) = rho * em(krow,kcol)
            end do
          end do

        end do
      end do

      return
      end
      subroutine wathen_st_size ( nx, ny, nz_num )

c*********************************************************************72
c
cc WATHEN_ST_SIZE: Size of Wathen matrix stored in sparse triplet format.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Higham,
c    Algorithm 694: A Collection of Test Matrices in MATLAB,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 3, September 1991, pages 289-305.
c
c    Andrew Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, Number 4, October 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size of the matrix.
c
c    Output, integer NZ_NUM, the number of items of data used to describe
c    the matrix.
c
      implicit none

      integer nx
      integer ny
      integer nz_num

      nz_num = nx * ny * 64

      return
      end
