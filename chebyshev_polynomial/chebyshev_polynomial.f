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

      if ( da .eq. 0.0D+00 ) then
        return
      end if

      if ( incx .ne. 1 .or. incy .ne. 1 ) then

        if ( incx .lt. 0 ) then
          ix = (-n+1) * incx + 1
        else
          ix = 1
        end if

        if ( incy .lt. 0 ) then
          iy = (-n+1) * incy + 1
        else
          iy = 1
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
          dy(i)     = dy(i)     + da * dx(i)
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

      if(incx.eq.1.and.incy.eq.1)go to 20
c
c  code for unequal increments or equal increments not equal to 1
c
        if ( incx .lt. 0 ) then
          ix = (-n+1)*incx + 1
        else
          ix = 1
        end if

        if ( incy .lt. 0 ) then
          iy = (-n+1) * incy + 1
        else
          iy = 1
        end if

      do i = 1, n
        dtemp = dtemp + dx(ix) * dy(iy)
        ix = ix + incx
        iy = iy + incy
      end do
      ddot = dtemp
      return
c
c  code for both increments equal to 1
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do i = 1,m
        dtemp = dtemp + dx(i) * dy(i)
      end do
      if( n .lt. 5 ) go to 60
   40 continue
      do i = m+1, n, 5
        dtemp = dtemp + dx(i)     * dy(i) 
     &                + dx(i + 1) * dy(i + 1) 
     &                + dx(i + 2) * dy(i + 2) 
     &                + dx(i + 3) * dy(i + 3) 
     &                + dx(i + 4) * dy(i + 4)
      end do

   60 ddot = dtemp

      return
      end
      function dnrm2 ( n, x, incx )

c*********************************************************************72
c
cc DNRM2 returns the euclidean norm of a vector. 
c
c  Discussion:
c
c    This routine uses double precision real arithmetic.
c
c      DNRM2 ( X ) = sqrt ( X' * X )
c
c  Author:
c
c    Sven Hammarling
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
c    Input, double precision X(*), the vector whose norm is to be computed.
c
c    Input, integer INCX, the increment between successive entries of X.
c
c    Output, double precision DNRM2, the Euclidean norm of X.
c
      implicit none

      double precision absxi
      double precision dnrm2
      integer incx
      integer ix
      integer n
      double precision norm
      double precision one
      double precision scale
      double precision ssq
      double precision x( * )
      double precision zero


      parameter ( one = 1.0d+0, zero = 0.0d+0 )

      if( n.lt.1 .or. incx.lt.1 )then
        norm  = zero
      else if( n.eq.1 )then
        norm  = abs( x( 1 ) )
      else
        scale = zero
        ssq   = one
        do ix = 1, 1 + ( n - 1 ) * incx, incx
          if( x( ix ).ne.zero )then
            absxi = abs( x( ix ) )
            if( scale.lt.absxi )then
              ssq   = one   + ssq * ( scale/absxi )**2
              scale = absxi
            else
              ssq   = ssq   +     ( absxi/scale )**2
            end if
          end if
        end do
        norm  = scale * sqrt( ssq )
      end if

      dnrm2 = norm

      return
      end
      subroutine drot ( n, dx, incx, dy, incy, c, s )

c*********************************************************************72
c
cc DROT applies a plane rotation.
c
c  Discussion:
c
c    This routine uses double precision real arithmetic.
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
c    Input, integer N, the number of entries in the vectors.
c
c    Input/output, double precision X(*), one of the vectors to be rotated.
c
c    Input, integer INCX, the increment between successive entries of X.
c
c    Input/output, double precision Y(*), one of the vectors to be rotated.
c
c    Input, integer INCY, the increment between successive elements of Y.
c
c    Input, double precision C, S, parameters (presumably the cosine and
c    sine of some angle) that define a plane rotation.
c
      implicit none

      double precision c
      double precision dtemp
      double precision dx(*)
      double precision dy(*)
      double precision s
      integer i,incx,incy,ix,iy,n

      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c  code for unequal increments or equal increments not equal to 1
c
      if(incx.lt.0)then
        ix = (-n+1)*incx + 1
      else
        ix = 1
      end if

      if ( incy .lt. 0 ) then
        iy = (-n+1)*incy + 1
      else
        iy = 1
      end if

      do i = 1,n
        dtemp = c*dx(ix) + s*dy(iy)
        dy(iy) = c*dy(iy) - s*dx(ix)
        dx(ix) = dtemp
        ix = ix + incx
        iy = iy + incy
      end do
      return
c
c  code for both increments equal to 1
c
   20 do i = 1,n
        dtemp = c*dx(i) + s*dy(i)
        dy(i) = c*dy(i) - s*dx(i)
        dx(i) = dtemp
      end do
      return
      end
      subroutine drotg ( da, db, c, s )

c*********************************************************************72
c
cc DROTG constructs a Givens plane rotation.
c
c  Discussion:
c
c    This routine uses double precision real arithmetic.
c
c    Given values A and B, this routine computes
c
c    SIGMA = sign ( A ) if abs ( A ) >  abs ( B )
c          = sign ( B ) if abs ( A ) <= abs ( B );
c
c    R     = SIGMA * ( A * A + B * B );
c
c    C = A / R if R is not 0
c      = 1     if R is 0;
c
c    S = B / R if R is not 0,
c        0     if R is 0.
c
c    The computed numbers then satisfy the equation
c
c    (  C  S ) ( A ) = ( R )
c    ( -S  C ) ( B ) = ( 0 )
c
c    The routine also computes
c
c    Z = S     if abs ( A ) > abs ( B ),
c      = 1 / C if abs ( A ) <= abs ( B ) and C is not 0,
c      = 1     if C is 0.
c
c    The single value Z encodes C and S, and hence the rotation:
c
c    If Z = 1, set C = 0 and S = 1;
c    If abs ( Z ) .lt. 1, set C = sqrt ( 1 - Z * Z ) and S = Z;
c    if abs ( Z ) > 1, set C = 1/ Z and S = sqrt ( 1 - C * C );
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
c    Input/output, double precision SA, SB.  On input, SA and SB are the values
c    A and B.  On output, SA is overwritten with R, and SB is
c    overwritten with Z.
c
c    Output, double precision C, S, the cosine and sine of the
c    Givens rotation.
c
      implicit none

      double precision c
      double precision da
      double precision db
      double precision r
      double precision roe
      double precision s
      double precision scale
      double precision z

      if( dabs(da) .gt. dabs(db) ) then
        roe = da
      else
        roe = db
      end if

      scale = dabs(da) + dabs(db)

      if( scale .eq. 0.0d0 ) then
         c = 1.0d0
         s = 0.0d0
         r = 0.0d0
         z = 0.0d0
      else
        r = scale*dsqrt((da/scale)**2 + (db/scale)**2)
        r = dsign(1.0d0,roe)*r
        c = da/r
        s = db/r
        z = 1.0d0
        if( dabs(da) .gt. dabs(db) ) z = s
        if( dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0 ) z = 1.0d0/c
      end if

      da = r
      db = z

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

      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c  code for increment not equal to 1
c
      nincx = n*incx
      do i = 1,nincx,incx
        dx(i) = da*dx(i)
      end do
      return
c
c  code for increment equal to 1
c
c
c  clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do i = 1,m
        dx(i) = da*dx(i)
      end do
      if( n .lt. 5 ) return
   40 continue
      do i = m+1, n, 5
        dx(i)     = da * dx(i)
        dx(i + 1) = da * dx(i + 1)
        dx(i + 2) = da * dx(i + 2)
        dx(i + 3) = da * dx(i + 3)
        dx(i + 4) = da * dx(i + 4)
      end do

      return
      end
      subroutine dsvdc ( x, ldx, n, p, s, e, u, ldu, v, ldv, work, 
     &  job, info )

c*********************************************************************72
c
cc DSVDC computes the singular value decomposition of a matrix.
c
c     dsvdc is a subroutine to reduce a double precision nxp matrix x
c     by orthogonal transformations u and v to diagonal form.  the
c     diagonal elements s(i) are the singular values of x.  the
c     columns of u are the corresponding left singular vectors,
c     and the columns of v the right singular vectors.
c
c     on entry
c
c         x         double precision(ldx,p), where ldx.ge.n.
c                   x contains the matrix whose singular value
c                   decomposition is to be computed.  x is
c                   destroyed by dsvdc.
c
c         ldx       integer.
c                   ldx is the leading dimension of the array x.
c
c         n         integer.
c                   n is the number of rows of the matrix x.
c
c         p         integer.
c                   p is the number of columns of the matrix x.
c
c         ldu       integer.
c                   ldu is the leading dimension of the array u.
c                   (see below).
c
c         ldv       integer.
c                   ldv is the leading dimension of the array v.
c                   (see below).
c
c         work      double precision(n).
c                   work is a scratch array.
c
c         job       integer.
c                   job controls the computation of the singular
c                   vectors.  it has the decimal expansion ab
c                   with the following meaning
c
c                        a.eq.0    do not compute the left singular
c                                  vectors.
c                        a.eq.1    return the n left singular vectors
c                                  in u.
c                        a.ge.2    return the first min(n,p) singular
c                                  vectors in u.
c                        b.eq.0    do not compute the right singular
c                                  vectors.
c                        b.eq.1    return the right singular vectors
c                                  in v.
c
c     on return
c
c         s         double precision(mm), where mm=max(n+1,p).
c                   the first min(n,p) entries of s contain the
c                   singular values of x arranged in descending
c                   order of magnitude.
c
c         e         double precision(mm), where mm=max(n+1,p).
c                   e ordinarily contains zeros.  however see the
c                   discussion of info for exceptions.
c
c         u         double precision(ldu,k), where ldu.ge.n.  if
c                                   joba.eq.1 then k.eq.n, if joba.ge.2
c                                   then k.eq.min(n,p).
c                   u contains the matrix of left singular vectors.
c                   u is not referenced if joba.eq.0.  if n.le.p
c                   or if joba.eq.2, then u may be identified with x
c                   in the subroutine call.
c
c         v         double precision(ldv,p), where ldv.ge.p.
c                   v contains the matrix of right singular vectors.
c                   v is not referenced if job.eq.0.  if p.le.n,
c                   then v may be identified with x in the
c                   subroutine call.
c
c         info      integer.
c                   the singular values (and their corresponding
c                   singular vectors) s(info+1),s(info+2),...,s(m)
c                   are correct (here m=min(n,p)).  thus if
c                   info.eq.0, all the singular values and their
c                   vectors are correct.  in any event, the matrix
c                   b = trans(u)*x*v is the bidiagonal matrix
c                   with the elements of s on its diagonal and the
c                   elements of e on its super-diagonal (trans(u)
c                   is the transpose of u).  thus the singular
c                   values of x and b are the same.
c
c     linpack. this version dated 08/14/78 .
c              correction made to shift 2/84.
c     g.w. stewart, university of maryland, argonne national lab.
c
c     dsvdc uses the following functions and subprograms.
c
c     external drot
c     blas daxpy,ddot,dscal,dswap,dnrm2,drotg
c     fortran dabs,dmax1,max0,min0,mod,dsqrt
c
      integer ldx,n,p,ldu,ldv,job,info
      double precision x(ldx,*),s(*),e(*),u(ldu,*),v(ldv,*),work(*)

      integer i,iter,j,jobu,k,kase,kk,l,ll,lls,lm1,lp1,ls,lu,m,maxit,
     *        mm,mm1,mp1,nct,nctp1,ncu,nrt,nrtp1
      double precision ddot,t,r
      double precision b,c,cs,el,emm1,f,g,dnrm2,scale,shift,sl,sm,sn,
     *                 smm1,t1,test,ztest
      logical wantu,wantv
c
c
c     set the maximum number of iterations.
c
      maxit = 30
c
c     determine what is to be computed.
c
      wantu = .false.
      wantv = .false.
      jobu = mod(job,100)/10
      ncu = n
      if (jobu .gt. 1) ncu = min0(n,p)
      if (jobu .ne. 0) wantu = .true.
      if (mod(job,10) .ne. 0) wantv = .true.
c
c     reduce x to bidiagonal form, storing the diagonal elements
c     in s and the super-diagonal elements in e.
c
      info = 0
      nct = min0(n-1,p)
      nrt = max0(0,min0(p-2,n))
      lu = max0(nct,nrt)
      if (lu .lt. 1) go to 170
      do 160 l = 1, lu
         lp1 = l + 1
         if (l .gt. nct) go to 20
c
c           compute the transformation for the l-th column and
c           place the l-th diagonal in s(l).
c
            s(l) = dnrm2(n-l+1,x(l,l),1)
            if (s(l) .eq. 0.0d0) go to 10
               if (x(l,l) .ne. 0.0d0) s(l) = dsign(s(l),x(l,l))
               call dscal(n-l+1,1.0d0/s(l),x(l,l),1)
               x(l,l) = 1.0d0 + x(l,l)
   10       continue
            s(l) = -s(l)
   20    continue
         if (p .lt. lp1) go to 50
         do 40 j = lp1, p
            if (l .gt. nct) go to 30
            if (s(l) .eq. 0.0d0) go to 30
c
c              apply the transformation.
c
               t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
               call daxpy(n-l+1,t,x(l,l),1,x(l,j),1)
   30       continue
c
c           place the l-th row of x into  e for the
c           subsequent calculation of the row transformation.
c
            e(j) = x(l,j)
   40    continue
   50    continue
         if (.not.wantu .or. l .gt. nct) go to 70
c
c           place the transformation in u for subsequent back
c           multiplication.
c
            do 60 i = l, n
               u(i,l) = x(i,l)
   60       continue
   70    continue
         if (l .gt. nrt) go to 150
c
c           compute the l-th row transformation and place the
c           l-th super-diagonal in e(l).
c
            e(l) = dnrm2(p-l,e(lp1),1)
            if (e(l) .eq. 0.0d0) go to 80
               if (e(lp1) .ne. 0.0d0) e(l) = dsign(e(l),e(lp1))
               call dscal(p-l,1.0d0/e(l),e(lp1),1)
               e(lp1) = 1.0d0 + e(lp1)
   80       continue
            e(l) = -e(l)
            if (lp1 .gt. n .or. e(l) .eq. 0.0d0) go to 120
c
c              apply the transformation.
c
               do 90 i = lp1, n
                  work(i) = 0.0d0
   90          continue
               do 100 j = lp1, p
                  call daxpy(n-l,e(j),x(lp1,j),1,work(lp1),1)
  100          continue
               do 110 j = lp1, p
                  call daxpy(n-l,-e(j)/e(lp1),work(lp1),1,x(lp1,j),1)
  110          continue
  120       continue
            if (.not.wantv) go to 140
c
c              place the transformation in v for subsequent
c              back multiplication.
c
               do 130 i = lp1, p
                  v(i,l) = e(i)
  130          continue
  140       continue
  150    continue
  160 continue
  170 continue
c
c     set up the final bidiagonal matrix or order m.
c
      m = min0(p,n+1)
      nctp1 = nct + 1
      nrtp1 = nrt + 1
      if (nct .lt. p) s(nctp1) = x(nctp1,nctp1)
      if (n .lt. m) s(m) = 0.0d0
      if (nrtp1 .lt. m) e(nrtp1) = x(nrtp1,m)
      e(m) = 0.0d0
c
c     if required, generate u.
c
      if (.not.wantu) go to 300
         if (ncu .lt. nctp1) go to 200
         do 190 j = nctp1, ncu
            do 180 i = 1, n
               u(i,j) = 0.0d0
  180       continue
            u(j,j) = 1.0d0
  190    continue
  200    continue
         if (nct .lt. 1) go to 290
         do 280 ll = 1, nct
            l = nct - ll + 1
            if (s(l) .eq. 0.0d0) go to 250
               lp1 = l + 1
               if (ncu .lt. lp1) go to 220
               do 210 j = lp1, ncu
                  t = -ddot(n-l+1,u(l,l),1,u(l,j),1)/u(l,l)
                  call daxpy(n-l+1,t,u(l,l),1,u(l,j),1)
  210          continue
  220          continue
               call dscal(n-l+1,-1.0d0,u(l,l),1)
               u(l,l) = 1.0d0 + u(l,l)
               lm1 = l - 1
               if (lm1 .lt. 1) go to 240
               do 230 i = 1, lm1
                  u(i,l) = 0.0d0
  230          continue
  240          continue
            go to 270
  250       continue
               do 260 i = 1, n
                  u(i,l) = 0.0d0
  260          continue
               u(l,l) = 1.0d0
  270       continue
  280    continue
  290    continue
  300 continue
c
c     if it is required, generate v.
c
      if (.not.wantv) go to 350
         do 340 ll = 1, p
            l = p - ll + 1
            lp1 = l + 1
            if (l .gt. nrt) go to 320
            if (e(l) .eq. 0.0d0) go to 320
               do 310 j = lp1, p
                  t = -ddot(p-l,v(lp1,l),1,v(lp1,j),1)/v(lp1,l)
                  call daxpy(p-l,t,v(lp1,l),1,v(lp1,j),1)
  310          continue
  320       continue
            do 330 i = 1, p
               v(i,l) = 0.0d0
  330       continue
            v(l,l) = 1.0d0
  340    continue
  350 continue
c
c     main iteration loop for the singular values.
c
      mm = m
      iter = 0
  360 continue
c
c        quit if all the singular values have been found.
c
c     ...exit
         if (m .eq. 0) go to 620
c
c        if too many iterations have been performed, set
c        flag and return.
c
         if (iter .lt. maxit) go to 370
            info = m
c     ......exit
            go to 620
  370    continue
c
c        this section of the program inspects for
c        negligible elements in the s and e arrays.  on
c        completion the variables kase and l are set as follows.
c
c           kase = 1     if s(m) and e(l-1) are negligible and l.lt.m
c           kase = 2     if s(l) is negligible and l.lt.m
c           kase = 3     if e(l-1) is negligible, l.lt.m, and
c                        s(l), ..., s(m) are not negligible (qr step).
c           kase = 4     if e(m-1) is negligible (convergence).
c
         do 390 ll = 1, m
            l = m - ll
c        ...exit
            if (l .eq. 0) go to 400
            test = dabs(s(l)) + dabs(s(l+1))
            ztest = test + dabs(e(l))
            if (ztest .ne. test) go to 380
               e(l) = 0.0d0
c        ......exit
               go to 400
  380       continue
  390    continue
  400    continue
         if (l .ne. m - 1) go to 410
            kase = 4
         go to 480
  410    continue
            lp1 = l + 1
            mp1 = m + 1
            do 430 lls = lp1, mp1
               ls = m - lls + lp1
c           ...exit
               if (ls .eq. l) go to 440
               test = 0.0d0
               if (ls .ne. m) test = test + dabs(e(ls))
               if (ls .ne. l + 1) test = test + dabs(e(ls-1))
               ztest = test + dabs(s(ls))
               if (ztest .ne. test) go to 420
                  s(ls) = 0.0d0
c           ......exit
                  go to 440
  420          continue
  430       continue
  440       continue
            if (ls .ne. l) go to 450
               kase = 3
            go to 470
  450       continue
            if (ls .ne. m) go to 460
               kase = 1
            go to 470
  460       continue
               kase = 2
               l = ls
  470       continue
  480    continue
         l = l + 1
c
c        perform the task indicated by kase.
c
         go to (490,520,540,570), kase
c
c        deflate negligible s(m).
c
  490    continue
            mm1 = m - 1
            f = e(m-1)
            e(m-1) = 0.0d0
            do 510 kk = l, mm1
               k = mm1 - kk + l
               t1 = s(k)
               call drotg(t1,f,cs,sn)
               s(k) = t1
               if (k .eq. l) go to 500
                  f = -sn*e(k-1)
                  e(k-1) = cs*e(k-1)
  500          continue
               if (wantv) call drot(p,v(1,k),1,v(1,m),1,cs,sn)
  510       continue
         go to 610
c
c        split at negligible s(l).
c
  520    continue
            f = e(l-1)
            e(l-1) = 0.0d0
            do 530 k = l, m
               t1 = s(k)
               call drotg(t1,f,cs,sn)
               s(k) = t1
               f = -sn*e(k)
               e(k) = cs*e(k)
               if (wantu) call drot(n,u(1,k),1,u(1,l-1),1,cs,sn)
  530       continue
         go to 610
c
c        perform one qr step.
c
  540    continue
c
c           calculate the shift.
c
            scale = dmax1(dabs(s(m)),dabs(s(m-1)),dabs(e(m-1)),
     *                    dabs(s(l)),dabs(e(l)))
            sm = s(m)/scale
            smm1 = s(m-1)/scale
            emm1 = e(m-1)/scale
            sl = s(l)/scale
            el = e(l)/scale
            b = ((smm1 + sm)*(smm1 - sm) + emm1**2)/2.0d0
            c = (sm*emm1)**2
            shift = 0.0d0
            if (b .eq. 0.0d0 .and. c .eq. 0.0d0) go to 550
               shift = dsqrt(b**2+c)
               if (b .lt. 0.0d0) shift = -shift
               shift = c/(b + shift)
  550       continue
            f = (sl + sm)*(sl - sm) + shift
            g = sl*el
c
c           chase zeros.
c
            mm1 = m - 1
            do 560 k = l, mm1
               call drotg(f,g,cs,sn)
               if (k .ne. l) e(k-1) = f
               f = cs*s(k) + sn*e(k)
               e(k) = cs*e(k) - sn*s(k)
               g = sn*s(k+1)
               s(k+1) = cs*s(k+1)
               if (wantv) call drot(p,v(1,k),1,v(1,k+1),1,cs,sn)
               call drotg(f,g,cs,sn)
               s(k) = f
               f = cs*e(k) + sn*s(k+1)
               s(k+1) = -sn*e(k) + cs*s(k+1)
               g = sn*e(k+1)
               e(k+1) = cs*e(k+1)
               if (wantu .and. k .lt. n)
     *            call drot(n,u(1,k),1,u(1,k+1),1,cs,sn)
  560       continue
            e(m-1) = f
            iter = iter + 1
         go to 610
c
c        convergence.
c
  570    continue
c
c           make the singular value  positive.
c
            if (s(l) .ge. 0.0d0) go to 580
               s(l) = -s(l)
               if (wantv) call dscal(p,-1.0d0,v(1,l),1)
  580       continue
c
c           order the singular value.
c
  590       if (l .eq. mm) go to 600
c           ...exit
               if (s(l) .ge. s(l+1)) go to 600
               t = s(l)
               s(l) = s(l+1)
               s(l+1) = t
               if (wantv .and. l .lt. p)
     *            call dswap(p,v(1,l),1,v(1,l+1),1)
               if (wantu .and. l .lt. n)
     *            call dswap(n,u(1,l),1,u(1,l+1),1)
               l = l + 1
            go to 590
  600       continue
            iter = 0
            m = m - 1
  610    continue
      go to 360
  620 continue
      return
      end
      subroutine dswap ( n, dx, incx, dy, incy )

c*********************************************************************72
c
cc DSWAP interchanges two vectors.
c
c  Discussion:
c
c    This routine uses double precision real arithmetic.
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
c    Input, integer N, the number of entries in the vectors.
c
c    Input/output, double precision X(*), one of the vectors to swap.
c
c    Input, integer INCX, the increment between successive entries of X.
c
c    Input/output, double precision Y(*), one of the vectors to swap.
c
c    Input, integer INCY, the increment between successive elements of Y.
c
      implicit none

      double precision dx(*)
      double precision dy(*),dtemp
      integer i,incx,incy,ix,iy,m,n

      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c  code for unequal increments or equal increments not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0) then
        ix = (-n+1)*incx + 1
      end if
      if(incy.lt.0) then
        iy = (-n+1)*incy + 1
      end if
      do i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
      end do
      return
c
c  code for both increments equal to 1
c
c
c  clean-up loop
c
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
      end do
      if( n .lt. 3 ) return
   40 continue

      do i = m+1, n, 3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
      end do

      return
      end
      function i4_uniform_ab ( a, b, seed )

c*********************************************************************72
c
cc I4_UNIFORM_AB returns a scaled pseudorandom I4 between A and B.
c
c  Discussion:
c
c    An I4 is an integer value.
c
c    The pseudorandom number should be uniformly distributed
c    between A and B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Second Edition,
c    Springer, 1987,
c    ISBN: 0387964673,
c    LC: QA76.9.C65.B73.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, December 1986, pages 362-376.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998,
c    ISBN: 0471134031,
c    LC: T57.62.H37.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, Number 2, 1969, pages 136-143.
c
c  Parameters:
c
c    Input, integer A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, integer I4_UNIFORM_AB, a number between A and B.
c
      implicit none

      integer a
      integer b
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4_uniform_ab
      integer k
      real r
      integer seed
      integer value

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge
      end if

      r = real ( seed ) * 4.656612875E-10
c
c  Scale R to lie between A-0.5 and B+0.5.
c
      r = ( 1.0E+00 - r ) * ( real ( min ( a, b ) ) - 0.5E+00 )
     &  +             r   * ( real ( max ( a, b ) ) + 0.5E+00 )
c
c  Use rounding to convert R to an integer between A and B.
c
      value = nint ( r )

      value = max ( value, min ( a, b ) )
      value = min ( value, max ( a, b ) )

      i4_uniform_ab = value

      return
      end
      subroutine imtqlx ( n, d, e, z )

c*********************************************************************72
c
cc IMTQLX diagonalizes a symmetric tridiagonal matrix.
c
c  Discussion:
c
c    This routine is a slightly modified version of the EISPACK routine to 
c    perform the implicit QL algorithm on a symmetric tridiagonal matrix. 
c
c    The authors thank the authors of EISPACK for permission to use this
c    routine. 
c
c    It has been modified to produce the product Q' * Z, where Z is an input 
c    vector and Q is the orthogonal matrix diagonalizing the input matrix.  
c    The changes consist (essentially) of applying the orthogonal 
c    transformations directly to Z as they are generated.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 August 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Sylvan Elhay, Jaroslav Kautsky,
c    Algorithm 655: IQPACK, FORTRAN Subroutines for the Weights of 
c    Interpolatory Quadrature,
c    ACM Transactions on Mathematical Software,
c    Volume 13, Number 4, December 1987, pages 399-415.
c
c    Roger Martin, James Wilkinson,
c    The Implicit QL Algorithm,
c    Numerische Mathematik,
c    Volume 12, Number 5, December 1968, pages 377-383.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input/output, double precision D(N), the diagonal entries of the matrix.
c    On output, the information in D has been overwritten.
c
c    Input/output, double precision E(N), the subdiagonal entries of the 
c    matrix, in entries E(1) through E(N-1).  On output, the information in
c    E has been overwritten.
c
c    Input/output, double precision Z(N).  On input, a vector.  On output,
c    the value of Q' * Z, where Q is the matrix that diagonalizes the
c    input symmetric tridiagonal matrix.
c
      implicit none

      integer n

      double precision b
      double precision c
      double precision d(n)
      double precision e(n)
      double precision f
      double precision g
      integer i
      integer ii
      integer itn
      parameter ( itn = 30 )
      integer j
      integer k
      integer l
      integer m
      integer mml
      double precision p
      double precision prec
      double precision r
      double precision r8_epsilon
      double precision s
      double precision z(n)

      prec = r8_epsilon ( )

      if ( n .eq. 1 ) then
        return
      end if

      e(n) = 0.0D+00

      do l = 1, n

        j = 0

10      continue

          do m = l, n

            if ( m .eq. n ) then
              go to 20
            end if

            if ( abs ( e(m) ) .le. 
     &        prec * ( abs ( d(m) ) + abs ( d(m+1) ) ) ) then
              go to 20
            end if

          end do

20        continue

          p = d(l)

          if ( m .eq. l ) then
            go to 30
          end if

          if ( itn .le. j ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'IMTQLX - Fatal error!'
            write ( *, '(a)' ) '  Iteration limit exceeded.'
            write ( *, '(a,i8)' ) '  J = ', j
            write ( *, '(a,i8)' ) '  L = ', l
            write ( *, '(a,i8)' ) '  M = ', m
            write ( *, '(a,i8)' ) '  N = ', n
            stop 1
          end if

          j = j + 1
          g = ( d(l+1) - p ) / ( 2.0D+00 * e(l) )
          r =  sqrt ( g * g + 1.0D+00 )
          g = d(m) - p + e(l) / ( g + sign ( r, g ) )
          s = 1.0D+00
          c = 1.0D+00
          p = 0.0D+00
          mml = m - l

          do ii = 1, mml

            i = m - ii
            f = s * e(i)
            b = c * e(i)

            if ( abs ( g ) .le. abs ( f ) ) then
              c = g / f
              r =  sqrt ( c * c + 1.0D+00 )
              e(i+1) = f * r
              s = 1.0D+00 / r
              c = c * s
            else
              s = f / g
              r =  sqrt ( s * s + 1.0D+00 )
              e(i+1) = g * r
              c = 1.0D+00 / r
              s = s * c
            end if

            g = d(i+1) - p
            r = ( d(i) - g ) * s + 2.0D+00 * c * b
            p = s * r
            d(i+1) = g + p
            g = c * r - b
            f = z(i+1)
            z(i+1) = s * z(i) + c * f
            z(i) = c * z(i) - s * f

          end do

          d(l) = d(l) - p
          e(l) = g
          e(m) = 0.0D+00

        go to 10

30      continue

      end do
c
c  Sorting.
c
      do ii = 2, n

        i = ii - 1
        k = i
        p = d(i)

        do j = ii, n
          if ( d(j) .lt. p ) then
            k = j
            p = d(j)
          end if
        end do

        if ( k .ne. i ) then
          d(k) = d(i)
          d(i) = p
          p = z(i)
          z(i) = z(k)
          z(k) = p
        end if

      end do

      return
      end
      function r8_choose ( n, k )

c*********************************************************************72
c
cc R8_CHOOSE computes the binomial coefficient C(N,K) as an R8.
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in R8 arithmetic.
c
c    The formula used is:
c
c      C(N,K) = N! / ( K! * (N-K)! )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 June 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    ML Wolfson, HV Wright,
c    Algorithm 160:
c    Combinatorial of M Things Taken N at a Time,
c    Communications of the ACM,
c    Volume 6, Number 4, April 1963, page 161.
c
c  Parameters:
c
c    Input, integer N, K, are the values of N and K.
c
c    Output, double precision R8_CHOOSE, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer i
      integer k
      integer mn
      integer mx
      integer n
      double precision r8_choose
      double precision value

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        value = 0.0D+00

      else if ( mn .eq. 0 ) then

        value = 1.0D+00

      else

        mx = max ( k, n - k )
        value = dble ( mx + 1 )

        do i = 2, mn
          value = ( value * dble ( mx + i ) ) / dble ( i )
        end do

      end if

      r8_choose = value

      return
      end
      function r8_epsilon ( )

c*********************************************************************72
c
cc R8_EPSILON returns the R8 roundoff unit.
c
c  Discussion:
c
c    The roundoff unit is a number R which is a power of 2 with the
c    property that, to the precision of the computer's arithmetic,
c      1 .lt. 1 + R
c    but
c      1 = ( 1 + R / 2 )
c
c    FORTRAN90 provides the superior library routine
c
c      EPSILON ( X )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_EPSILON, the R8 roundoff unit.
c
      implicit none

      double precision r8_epsilon

      r8_epsilon = 2.220446049250313D-016

      return
      end
      function r8_sign ( x )

c*********************************************************************72
c
cc R8_SIGN returns the sign of an R8.
c
c  Discussion:
c
c    value = -1 if X .lt. 0;
c    value = +1 if X => 0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the number whose sign is desired.
c
c    Output, double precision R8_SIGN, the sign of X.
c
      implicit none

      double precision r8_sign
      double precision x

      if ( x .lt. 0.0D+00 ) then
        r8_sign = -1.0D+00
      else
        r8_sign = +1.0D+00
      end if

      return
      end
      subroutine r8mat_mtv ( m, n, a, x, y )

c*****************************************************************************80
c
cc R8MAT_MTV multiplies a transposed matrix times a vector
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of
c    the matrix.
c
c    Input, double precision A(M,N), the M by N matrix.
c
c    Input, double precision X(M), the vector to be multiplied by A.
c
c    Output, double precision Y(N), the product A'*X.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision x(m)
      double precision y(n)
      double precision y1(n)

      do i = 1, n
        y1(i) = 0.0D+00
        do j = 1, m
          y1(i) = y1(i) + a(j,i) * x(j)
        end do
      end do

      do i = 1, n
        y(i) = y1(i)
      end do

      return
      end
      subroutine r8mat_mv ( m, n, a, x, y )

c*********************************************************************72
c
cc R8MAT_MV multiplies a matrix times a vector.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c    In FORTRAN90, this operation can be more efficiently carried
c    out by the command
c
c      Y(1:M) = MATMUL ( A(1:M,1:N), X(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of the matrix.
c
c    Input, double precision A(M,N), the M by N matrix.
c
c    Input, double precision X(N), the vector to be multiplied by A.
c
c    Output, double precision Y(M), the product A*X.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision x(n)
      double precision y(m)
      double precision y1(m)

      do i = 1, m
        y1(i) = 0.0D+00
        do j = 1, n
          y1(i) = y1(i) + a(i,j) * x(j)
        end do
      end do

      do i = 1, m
        y(i) = y1(i)
      end do

      return
      end
      subroutine r8mat_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_PRINT prints an R8MAT.
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
c    20 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character * ( * ) title

      call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R8MAT_PRINT_SOME prints some of an R8MAT.
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
c    25 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(g14.6)' ) a(i,j)

          end do

          write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

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
      function r8vec_in_ab ( n, x, a, b )

c*********************************************************************72
c
cc R8VEC_IN_AB is TRUE if the entries of an R8VEC are in the range [A,B].
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
c    15 April 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries.
c
c    Input, double precision X(N), the vector.
c
c    Input, double precision A, B, the limits of the range.
c
c    Output, logical R8VEC_IN_01, is TRUE if every entry is
c    between A and B.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      logical r8vec_in_ab
      double precision x(n)

      do i = 1, n
        if ( x(i) .lt.a .or. b .lt. x(i) ) then
          r8vec_in_ab = .false.
          return
        end if
      end do

      r8vec_in_ab = .true.

      return
      end
      subroutine r8vec_linspace ( n, a_first, a_last, a )

c*********************************************************************72
c
cc R8VEC_LINSPACE creates a vector of linearly spaced values.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
c
c    In other words, the interval is divided into N-1 even subintervals,
c    and the endpoints of intervals are used as the points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A_FIRST, A_LAST, the first and last entries.
c
c    Output, double precision A(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_first
      double precision a_last
      integer i

      if ( n .eq. 1 ) then

        a(1) = ( a_first + a_last ) / 2.0D+00

      else

        do i = 1, n
          a(i) = ( dble ( n - i     ) * a_first 
     &           + dble (     i - 1 ) * a_last )
     &           / dble ( n     - 1 )
        end do

      end if

      return
      end
      subroutine r8vec_max ( n, a, amax )

c*********************************************************************72
c
cc R8VEC_MAX returns the maximum value in an R8VEC.
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
c    31 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision AMAX, the value of the largest entry.
c
      implicit none

      integer n

      double precision a(n)
      double precision amax
      integer i

      amax = a(1)
      do i = 2, n
        amax = max ( amax, a(i) )
      end do

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
      character ( len = * ) title

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
      subroutine r8vec_uniform_ab ( n, a, b, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_AB returns a scaled pseudorandom R8VEC.
c
c  Discussion:
c
c    Each dimension ranges from A to B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 January 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Second Edition,
c    Springer, 1987,
c    ISBN: 0387964673,
c    LC: QA76.9.C65.B73.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, December 1986, pages 362-376.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998,
c    ISBN: 0471134031,
c    LC: T57.62.H37.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, Number 2, 1969, pages 136-143.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A, B, the lower and upper limits.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      integer seed
      double precision r(n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge
        end if

        r(i) = a + ( b - a ) * dble ( seed ) * 4.656612875D-10

      end do

      return
      end
      subroutine r8vec2_print ( n, a1, a2, title )

c*********************************************************************72
c
cc R8VEC2_PRINT prints an R8VEC2.
c
c  Discussion:
c
c    An R8VEC2 is a dataset consisting of N pairs of R8s, stored
c    as two separate vectors A1 and A2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 February 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A1(N), A2(N), the vectors to be printed.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a1(i), a2(i)
      end do

      return
      end
      subroutine svd_solve ( m, n, a, b, x )

c*********************************************************************72
c
cc SVD_SOLVE solves a linear system in the least squares sense.
c
c  Discussion:
c
c    The vector X returned by this routine should always minimize the 
c    Euclidean norm of the residual ||A*x-b||.
c
c    If the matrix A does not have full column rank, then there are multiple
c    vectors that attain the minimum residual.  In that case, the vector
c    X returned by this routine is the unique such minimizer that has the 
c    the minimum possible Euclidean norm, that is, ||A*x-b|| and ||x||
c    are both minimized.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2012
c
c  Reference:
c
c    David Kahaner, Cleve Moler, Steven Nash,
c    Numerical Methods and Software,
c    Prentice Hall, 1989,
c    ISBN: 0-13-627258-4,
c    LC: TA345.K34.
c
c  Parameters:
c
c    Input, integer M, the number of rows of A.
c
c    Input, integer N, the number of columns of A.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, double precision B(M), the right hand side.
c
c    Output, double precision X(N), the least squares solution.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision a_copy(m,n)
      double precision b(m)
      double precision e(max(m+1,n))
      integer i
      integer info
      integer j
      integer lda
      integer ldu
      integer ldv
      integer job
      double precision r8_epsilon
      double precision sdiag(max(m+1,n))
      double precision smax
      double precision stol
      double precision sub(n)
      double precision u(m,m)
      double precision ub(m)
      double precision v(n,n)
      double precision work(m)
      double precision x(n)
c
c  Get the SVD.
c
      do j = 1, n
        do i = 1, m
          a_copy(i,j) = a(i,j)
        end do
      end do

      lda = m
      ldu = m
      ldv = n
      job = 11

      call dsvdc ( a_copy, lda, m, n, sdiag, e, u, ldu, v, ldv, work, 
     &  job, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SVD_SOLVE - Failure!'
        write ( *, '(a)' ) '  The SVD could not be calculated.'
        write ( *, '(a)' ) '  LINPACK routine DSVDC returned a nonzero'
        write ( *, '(a,i8)' ) '  value of the error flag, INFO = ', info
        stop 1
      end if

      call r8mat_mtv ( m, m, u, b, ub )

      do i = 1, n
        sub(i) = 0.0D+00
      end do
c
c  For singular problems, there may be tiny but nonzero singular values
c  that should be ignored.  This is a reasonable attempt to avoid such 
c  problems, although in general, the user might wish to control the tolerance.
c
      call r8vec_max ( n, sdiag, smax )

      if ( smax .le. r8_epsilon ( ) ) then
        smax = 1.0D+00
      end if

      stol = r8_epsilon ( ) * smax

      do i = 1, n
        if ( i .le. m ) then
          if ( stol .le. sdiag(i) ) then
            sub(i) = ub(i) / sdiag(i)
          end if
        end if
      end do

      call r8mat_mv ( n, n, v, sub, x )

      return
      end
      function t_double_product_integral ( i, j )

c*********************************************************************72
c
cc T_DOUBLE_PRODUCT_INTEGRAL: integral (-1<=x<=1) T(i,x)*T(j,x)/sqrt(1-x^2) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, the polynomial indices.
c    0 <= I, J.
c
c    Output, double precision T_DOUBLE_PRODUCT_INTEGRAL, the integral.
c
      implicit none

      integer i
      integer j
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision t_double_product_integral
      double precision value

      if ( i .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'T_DOUBLE_PRODUCT_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  0 <= I is required.'
        stop 1
      end if

      if ( j .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'T_DOUBLE_PRODUCT_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  0 <= J is required.'
        stop 1
      end if

      if ( i .ne. j ) then
        value = 0.0D+00
      elseif ( i .eq. 0 ) then
        value = pi
      elseif ( 0 .lt. i ) then
        value = pi / 2.0D+00
      end if

      t_double_product_integral = value

      return
      end
      function t_integral ( e )

c*********************************************************************72
c
cc T_INTEGRAL: integral ( -1 <= x <= +1 ) x^e dx / sqrt ( 1 - x^2 ).
c
c  Discussion:
c
c    Set 
c      x = cos ( theta ), 
c      dx = - sin ( theta ) d theta = - sqrt ( 1 - x^2 ) d theta
c    to transform the integral to
c      integral ( 0 <= theta <= pi ) - ( cos ( theta ) )^e d theta
c    which becomes
c      0 if E is odd,
c      (1/2^e) * choose ( e, e/2 ) * pi if E is even.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer E, the exponent of X.
c    0 <= E.
c
c    Output, double precision T_INTEGRAL, the value of the integral.
c
      implicit none

      integer e
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_choose
      double precision t_integral
      double precision value

      if ( mod ( e, 2 ) .eq. 1 ) then

        value = 0.0D+00

      else

        value = r8_choose ( e, e / 2 ) * pi / 2.0D+00 ** e

      end if

      t_integral = value

      return
      end
      subroutine t_polynomial ( m, n, x, v )

c*********************************************************************72
c
cc T_POLYNOMIAL evaluates Chebyshev polynomials T(n,x).
c
c  Discussion:
c
c    Chebyshev polynomials are useful as a basis for representing the
c    approximation of functions since they are well conditioned, in the sense
c    that in the interval [-1,1] they each have maximum absolute value 1.
c    Hence an error in the value of a coefficient of the approximation, of
c    size epsilon, is exactly reflected in an error of size epsilon between
c    the computed approximation and the theoretical approximation.
c
c    Typical usage is as follows, where we assume for the moment
c    that the interval of approximation is [-1,1].  The value
c    of N is chosen, the highest polynomial to be used in the
c    approximation.  Then the function to be approximated is
c    evaluated at the N+1 points XJ which are the zeroes of the N+1-th
c    Chebyshev polynomial.  Let these values be denoted by F(XJ).
c
c    The coefficients of the approximation are now defined by
c
c      C(I) = 2/(N+1) * sum ( 1 <= J <= N+1 ) F(XJ) T(I,XJ)
c
c    except that C(0) is given a value which is half that assigned
c    to it by the above formula,
c
c    and the representation is
c
c    F(X) approximated by sum ( 0 <= J <= N ) C(J) T(J,X)
c
c    Now note that, again because of the fact that the Chebyshev polynomials
c    have maximum absolute value 1, if the higher order terms of the
c    coefficients C are small, then we have the option of truncating
c    the approximation by dropping these terms, and we will have an
c    exact value for maximum perturbation to the approximation that
c    this will cause.
c
c    It should be noted that typically the error in approximation
c    is dominated by the first neglected basis function (some multiple of
c    T(N+1,X) in the example above).  If this term were the exact error,
c    then we would have found the minimax polynomial, the approximating
c    polynomial of smallest maximum deviation from the original function.
c    The minimax polynomial is hard to compute, and another important
c    feature of the Chebyshev approximation is that it tends to behave
c    like the minimax polynomial while being easy to compute.
c
c    To evaluate a sum like 
c
c      sum ( 0 <= J <= N ) C(J) T(J,X), 
c
c    Clenshaw's recurrence formula is recommended instead of computing the
c    polynomial values, forming the products and summing.
c
c    Assuming that the coefficients C(J) have been computed
c    for J = 0 to N, then the coefficients of the representation of the
c    indefinite integral of the function may be computed by
c
c      B(I) = ( C(I-1) - C(I+1))/2*(I-1) for I=1 to N+1, 
c
c    with
c 
c      C(N+1)=0
c      B(0) arbitrary.  
c
c    Also, the coefficients of the representation of the derivative of the 
c    function may be computed by:
c
c      D(I) = D(I+2)+2*I*C(I) for I=N-1, N-2, ..., 0, 
c
c    with
c
c      D(N+1) = D(N)=0.
c
c    Some of the above may have to adjusted because of the irregularity of C(0).
c
c    The formula is:
c
c      T(N,X) = COS(N*ARCCOS(X))
c
c  Differential equation:
c
c    (1-X*X) Y'' - X Y' + N N Y = 0
c
c  First terms:
c
c    T(0,X) =  1
c    T(1,X) =  1 X
c    T(2,X) =  2 X^2 -   1
c    T(3,X) =  4 X^3 -   3 X
c    T(4,X) =  8 X^4 -   8 X^2 +  1
c    T(5,X) = 16 X^5 -  20 X^3 +  5 X
c    T(6,X) = 32 X^6 -  48 X^4 + 18 X^2 - 1
c    T(7,X) = 64 X^7 - 112 X^5 + 56 X^3 - 7 X
c
c  Inequality:
c
c    abs ( T(N,X) ) <= 1 for -1 <= X <= 1
c
c  Orthogonality:
c
c    For integration over [-1,1] with weight
c
c      W(X) = 1 / sqrt(1-X*X), 
c
c    if we write the inner product of T(I,X) and T(J,X) as
c
c      .lt. T(I,X), T(J,X) > = integral ( -1 <= X <= 1 ) W(X) T(I,X) T(J,X) dX
c
c    then the result is:
c
c      .lt. T(I,X), T(J,X) > = 0    if I .ne. J
c      .lt. T(I,X), T(J,X) > = PI/2 if I .eq. J .ne. 0
c      .lt. T(I,X), T(J,X) > = PI   if I .eq. J .eq. 0
c
c    A discrete orthogonality relation is also satisfied at each of
c    the N zeroes of T(N,X):  sum ( 1 <= K <= N ) T(I,X) * T(J,X)
c                              = 0 if I .ne. J
c                              = N/2 if I .eq. J .ne. 0
c                              = N if I .eq. J .eq. 0
c
c  Recursion:
c
c    T(0,X) = 1,
c    T(1,X) = X,
c    T(N,X) = 2 * X * T(N-1,X) - T(N-2,X)
c
c    T'(N,X) = N * ( -X * T(N,X) + T(N-1,X) ) / ( 1 - X^2 )
c
c  Special values:
c
c    T(N,1) = 1
c    T(N,-1) = (-1)^N
c    T(2N,0) = (-1)^N
c    T(2N+1,0) = 0
c    T(N,X) = (-1)^N * T(N,-X)
c
c  Zeroes:
c
c    M-th zero of T(N,X) is X = cos((2*M-1)*PI/(2*N)), M = 1 to N.
c
c  Extrema:
c
c    M-th extremum of T(N,X) is X = cos(PI*M/N), M = 0 to N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest polynomial to compute.
c
c    Input, double precision X(1:M), the evaluation points.
c
c    Output, double precision V(1:M,0:N), the values of the polynomials.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision v(1:m,0:n)
      double precision x(1:m)

      if ( n .lt. 0 ) then
        return
      end if

      do i = 1, m
        v(i,0) = 1.0D+00
      end do

      if ( n .lt. 1 ) then
        return
      end if

      do i = 1, m
        v(i,1) = x(i)
      end do
   
      do j = 2, n
        do i = 1, m
          v(i,j) = 2.0D+00 * x(i) * v(i,j-1) - v(i,j-2)
        end do
      end do
     
      return
      end
      subroutine t_polynomial_ab ( a, b, m, n, xab, v )

c*********************************************************************72
c
cc T_POLYNOMIAL_AB: evaluates Chebyshev polynomials T(n,x) in [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the domain of definition.
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest polynomial to compute.
c
c    Input, double precision XAB(M), the evaluation points.
c    It must be the case that A <= XAB(*) <= B.
c
c    Output, double precision V(M,N+1), the values.
c
      implicit none

      integer m
      integer n

      double precision a
      double precision b
      integer i
      double precision v(1:m,0:n)
      double precision x(1:m)
      double precision xab(1:m)

      do i = 1, m
        x(i) = ( 2.0 * xab(i) - a - b ) / ( b - a )
      end do

      call t_polynomial ( m, n, x, v )
     
      return
      end
      subroutine t_polynomial_coefficients ( n, c )

c*********************************************************************72
c
cc T_POLYNOMIAL_COEFFICIENTS: coefficients of the Chebyshev polynomial T(n,x).
c
c  First terms:
c
c    N/K     0     1      2      3       4     5      6    7      8    9   10
c
c     0      1
c     1      0     1
c     2     -1     0      2
c     3      0    -3      0      4
c     4      1     0     -8      0       8
c     5      0     5      0    -20       0    16
c     6     -1     0     18      0     -48     0     32
c     7      0    -7      0     56       0  -112      0    64
c
c  Recursion:
c
c    T(0,X) = 1,
c    T(1,X) = X,
c    T(N,X) = 2 * X * T(N-1,X) - T(N-2,X)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c  Parameters:
c
c    Input, integer N, the highest order polynomial to compute.
c    Note that polynomials 0 through N will be computed.
c
c    Output, double precision C(0:N,0:N), the coefficients of the Chebyshev T
c    polynomials.
c
      implicit none

      integer n

      double precision c(0:n,0:n)
      integer i
      integer j

      if ( n .lt. 0 ) then
        return
      end if

      do j = 0, n
        do i = 0, n
          c(i,j) = 0.0D+00
        end do
      end do

      c(0,0) = 1.0D+00

      if ( n .eq. 0 ) then
        return
      end if

      c(1,1) = 1.0D+00
     
      do i = 2, n
        c(i,0) = - c(i-2,0)
        do j = 1, i - 2
          c(i,j) = 2.0D+00 * c(i-1,j-1) - c(i-2,j)
        end do
        c(i,i-1) = 2.0D+00 * c(i-1,i-2)
        c(i,i) = 2.0D+00 * c(i-1,i-1)
      end do
     
      return
      end
      function t_polynomial_value ( n, x )

c*********************************************************************72
c
cc T_POLYNOMIAL_VALUE: returns the single value T(n,x).
c
c  Discussion:
c
c    In cases where calling T_POLYNOMIAL is inconvenient, because it returns
c    a vector of values for multiple arguments X, this simpler interface
c    may be appropriate.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the polynomial.
c
c    Input, double precision X, the argument of the polynomial.
c
c    Output, double precision T_POLYNOMIAL_VALUE, the value of T(n,x).
c
      implicit none

      integer n

      integer m
      double precision t_polynomial_value
      double precision vec(0:n)
      double precision x

      m = 1

      call t_polynomial ( m, n, x, vec )

      t_polynomial_value = vec(n)

      return
      end
      subroutine t_polynomial_values ( n_data, n, x, fx )

c*********************************************************************72
c
cc T_POLYNOMIAL_VALUES returns values of Chebyshev polynomials T(n,x).
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      ChebyshevT[n,x]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N, the order of the function.
c
c    Output, double precision X, the point where the function is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 13 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save n_vec
      save x_vec

      data fx_vec /
     &   0.1000000000000000D+01,
     &   0.8000000000000000D+00,
     &   0.2800000000000000D+00,
     &  -0.3520000000000000D+00,
     &  -0.8432000000000000D+00,
     &  -0.9971200000000000D+00,
     &  -0.7521920000000000D+00,
     &  -0.2063872000000000D+00,
     &   0.4219724800000000D+00,
     &   0.8815431680000000D+00,
     &   0.9884965888000000D+00,
     &   0.7000513740800000D+00,
     &   0.1315856097280000D+00 /
      data n_vec /
     &   0,  1,  2,
     &   3,  4,  5,
     &   6,  7,  8,
     &   9, 10, 11,
     &  12 /
      data x_vec /
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine t_polynomial_zeros ( n, z )

c*********************************************************************72
c
cc T_POLYNOMIAL_ZEROS returns zeroes of the Chebyshev polynomial T(n,x).
c
c  Discussion:
c
c    The I-th zero of T(N,X) is cos((2*I-1)*PI/(2*N)), I = 1 to N
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the polynomial.
c
c    Output, double precision Z(N), the zeroes of T(N,X).
c
      implicit none

      integer n

      double precision angle
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision z(n)

      do i = 1, n
        angle = dble ( 2 * i - 1 ) * pi / dble ( 2 * n )
        z(i) = cos ( angle )
      end do

      return
      end
      subroutine t_project_coefficients ( n, f, c )

c*********************************************************************72
c
cc T_PROJECT_COEFFICIENTS: function projected onto Chebyshev polynomials T(n,x).
c
c  Discussion:
c
c    It is assumed that the interval of definition is -1 <= x <= +1.
c
c    Over this interval, f(x) will be well approximated by
c
c      f(x) approx sum ( 0 <= i <= n ) c(i) * T(i,x)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the highest order polynomial to compute.
c
c    Input, external double precision function F ( X ), evaluates the function.
c
c    Output, double precision C(0:N), the projection coefficients of f(x) onto
c    T(0,x) through T(n,x).
c
      implicit none

      integer n

      double precision c(0:n)
      double precision d(0:n)
      double precision f
      external f
      double precision fac
      integer j
      integer k
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision total
      double precision y

      do k = 0, n
        y = cos ( pi * ( dble ( k ) + 0.5D+00 ) / dble ( n + 1 ) )
        d(k) = f ( y )
      end do

      fac = 2.0D+00 / dble ( n + 1 )

      do j = 0, n
        total = 0.0D+00
        do k = 0, n
          total = total + d(k) * cos ( ( pi * dble ( j ) ) 
     &      * ( ( dble ( k ) + 0.5 ) / dble ( n + 1 ) ) )
        end do
        c(j) = fac * total
      end do

      c(0) = c(0) / 2.0D+00

      return
      end
      subroutine t_project_coefficients_ab ( n, f, a, b, c )

c*********************************************************************72
c
cc T_PROJECT_COEFFICIENTS_AB: function projected onto T(n,x) over [a,b].
c
c  Discussion:
c
c    It is assumed that the interval of definition is a <= x <= b.
c
c    Over this interval, f(x) will be well approximated by
c
c      f(x) approx sum ( 0 <= i <= n ) c(i) * T(i,(2x-a-b)/(b-a))
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the highest order polynomial to compute.
c
c    Input, external double precision function F ( X ), evaluates the function.
c
c    Input, double precision A, B, the interval of definition.
c
c    Output, double precision C(0:N), the projection coefficients of f(x) onto
c    T(0,x) through T(n,x).
c
      implicit none

      integer n

      double precision a
      double precision b
      double precision c(0:n)
      double precision d(0:n)
      double precision f
      external f
      double precision fac
      integer j
      integer k
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision t
      double precision total
      double precision y

      do k = 0, n

        t = cos ( pi * ( dble ( k ) - 0.5 ) / dble ( n + 1 ) )

        y = ( ( 1.0D+00 + t ) * b   
     &      + ( 1.0D+00 - t ) * a ) 
     &      /   2.0D+00

        d(k) = f ( y )

      end do

      fac = 2.0D+00 / dble ( n + 1 )

      do j = 0, n
        total = 0.0D+00
        do k = 0, n
          total = total + d(k) * cos ( ( pi * dble ( j ) ) 
     &      * ( ( dble ( k ) + 0.5 ) / dble ( n + 1 ) ) )
        end do
        c(j) = fac * total
      end do

      c(0) = c(0) / 2.0D+00

      return
      end
      subroutine t_project_coefficients_data ( a, b, m, n, x, d, c )

c*********************************************************************72
c
cc T_PROJECT_COEFFICIENTS_DATA: project data onto Chebyshev polynomials T(n,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the domain of definition.
c
c    Input, integer M, the number of data values.
c
c    Input, integer N, the desired order of the Chebyshev 
c    expansion.
c
c    Input, double precision X(M), the data abscissas.  These need not 
c    be sorted.  It must be the case that A <= X() <= B.
c
c    Input, double precision D(M), the data values.
c
c    Output, double precision C(0:N), the approximate Chebshev coefficients.
c
      implicit none

      integer m
      integer n

      double precision a
      double precision b
      double precision c(0:n)
      double precision d(m)
      logical r8vec_in_ab
      double precision v(m,0:n)
      double precision x(m)

      if ( .not. r8vec_in_ab ( m, x, a, b ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) ' T_PROJECT_COEFFICIENTS_DATA - Fatal error!'
        write ( *, '(a)' ) '  Some X not in [A,B].'
        stop 1
      end if
c
c  Compute the M by N+1 Chebyshev Vandermonde matrix V.
c
      call t_polynomial_ab ( a, b, m, n, x, v )
c
c  Compute the least-squares solution C.
c
      call svd_solve ( m, n + 1, v, d, c )

      return
      end
      subroutine t_project_value ( m, n, x, c, v )

c*********************************************************************72
c
cc T_PROJECT_VALUE evaluates an expansion in Chebyshev polynomials T(n,x).
c
c  Discussion:
c
c    The projection is assumed to be based on the interval [-1,+1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to compute.
c
c    Input, double precision X(M), the evaluation points.
c
c    Input, double precision C(0:N), the expansion coefficients.
c
c    Output, double precision V(M), the value of the Chebyshev function.
c
      implicit none

      integer m
      integer n 

      double precision b0(m)
      double precision b1(m)
      double precision b2(m)
      double precision c(0:n)
      integer i
      integer j
      double precision v(m)
      double precision x(m)

      do i = 1, m
        b1(i) = 0.0D+00
        b0(i) = 0.0D+00
      end do

      do j = n, 0, -1
        do i = 1, m
          b2(i) = b1(i)
          b1(i) = b0(i)
          b0(i) = c(j) + 2.0D+00 * x(i) * b1(i) - b2(i)
        end do
      end do

      do i = 1, m
        v(i) = 0.5D+00 * ( c(0) + b0(i) - b2(i) )
      end do

      return
      end
      subroutine t_project_value_ab ( m, n, x, c, a, b, v )

c*********************************************************************72
c
cc T_PROJECT_VALUE_AB evaluates an expansion in Chebyshev polynomials T(n,x).
c
c  Discussion:
c
c    The projection is assumed to be based on the interval [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to compute.
c
c    Input, double precision X(M), the evaluation points.
c
c    Input, double precision C(0:N), the expansion coefficients.
c
c    Input, double precision A, B, the interval of definition.
c
c    Output, double precision V(M), the value of the Chebyshev function.
c
      implicit none

      integer m
      integer n 

      double precision a
      double precision b
      double precision b0(m)
      double precision b1(m)
      double precision b2(m)
      double precision c(0:n)
      integer i
      integer j
      double precision v(m)
      double precision x(m)

      do i = 1, m
        b1(i) = 0.0D+00
        b0(i) = 0.0D+00
      end do

      do j = n, 0, -1
        do i = 1, m
          b2(i) = b1(i)
          b1(i) = b0(i)
          b0(i) = c(j) + 2.0D+00 / ( b - a ) 
     &      * ( 2.0D+00 * x(i) - a - b ) * b1(i) - b2(i)
        end do
      end do

      do i = 1, m
        v(i) = 0.5D+00 * ( c(0) + b0(i) - b2(i) )
      end do

      return
      end
      subroutine t_quadrature_rule ( n, t, w )

c*********************************************************************72
c
cc T_QUADRATURE_RULE: quadrature rule for T(n,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision T(N), W(N), the points and weights of the rule.
c
      implicit none

      integer n

      double precision bj(n)
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision t(n)
      double precision w(n)

      do i = 1, n
        t(i) = 0.0D+00
      end do

      bj(1) = sqrt ( 0.5D+00 )
      do i = 2, n
        bj(i) = 0.5D+00
      end do

      w(1) = sqrt ( pi )
      do i = 2, n
        w(i) = 0.0D+00
      end do

      call imtqlx ( n, t, bj, w )

      do i = 1, n
        w(i) = w(i)**2
      end do

      return
      end
      function t_triple_product_integral ( i, j, k )

c*********************************************************************72
c
cc T_TRIPLE_PRODUCT_INTEGRAL: int (-1<=x<=1) T(i,x)*T(j,x)*T(k,x)/sqrt(1-x^2) dx
c 
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2016
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    John Mason, David Handscomb,
c    Chebyshev Polynomials,
c    CRC Press, 2002,
c    ISBN: 0-8493-035509,
c    LC: QA404.5.M37.
c
c  Parameters:
c
c    Input, integer I, J, K, the polynomial indices.
c    0 <= I, J.
c
c    Output, double precision T_TRIPLE_PRODUCT_INTEGRAL, the integral.
c
      implicit none

      integer i
      integer j
      integer k
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision t_double_product_integral
      double precision t_triple_product_integral
      double precision value

      if ( i .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'T_TRIPLE_PRODUCT_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  0 <= I is required.'
        stop 1
      end if

      if ( j .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'T_TRIPLE_PRODUCT_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  0 <= J is required.'
        stop 1
      end if

      if ( k .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'T_TRIPLE_PRODUCT_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  0 <= K is required.'
        stop 1
      end if

      value = 0.5D+00 * ( 
     &    t_double_product_integral (       i + j,   k ) 
     &  + t_double_product_integral ( abs ( i - j ), k ) )

      t_triple_product_integral = value

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
      function u_double_product_integral ( i, j )

c*********************************************************************72
c
cc U_DOUBLE_PRODUCT_INTEGRAL: integral (-1<=x<=1) U(i,x)*U(j,x)*sqrt(1-x^2) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, the polynomial indices.
c    0 <= I, J.
c
c    Output, double precision U_DOUBLE_PRODUCT_INTEGRAL, the integral.
c
      implicit none

      integer i
      integer j
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision u_double_product_integral
      double precision value

      if ( i .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'U_DOUBLE_PRODUCT_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  0 <= I is required.'
        stop 1
      end if

      if ( j .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'U_DOUBLE_PRODUCT_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  0 <= J is required.'
        stop 1
      end if

      if ( i .ne. j ) then
        value = 0.0D+00
      else
        value = pi / 2.0D+00
      end if

      u_double_product_integral = value

      return
      end
      function u_integral ( e )

c*********************************************************************72
c
cc U_INTEGRAL: integral ( -1 <= x <= +1 ) x^e sqrt ( 1 - x^2 ) dx.
c
c  Discussion:
c
c     E    U_INTEGRAL
c    --    --------------
c     0         pi /    2   
c     2         pi /    8
c     4         pi /   16
c     6     5 * pi /  128
c     8     7 * pi /  256
c    10    21 * pi / 1024
c    12    33 * pi / 2048
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer E, the exponent of X.
c    0 <= E.
c
c    Output, double precision U_INTEGRAL, the value of the integral.
c
      implicit none

      double precision arg1
      double precision arg2
      integer e
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision u_integral
      double precision value

      if ( mod ( e, 2 ) .eq. 1 ) then

        value = 0.0D+00

      else

        arg1 = 0.5D+00 * dble ( 1 + e )
        arg2 = 2.0D+00 + 0.5D+00 * dble ( e )
        value = 0.5D+00 * sqrt ( pi ) * gamma ( arg1 ) / gamma ( arg2 )

      end if

      u_integral = value

      return
      end
      subroutine u_polynomial ( m, n, x, v )

c*********************************************************************72
c
cc U_POLYNOMIAL evaluates Chebyshev polynomials U(n,x).
c
c  Discussion:
c
c    The formula is:
c
c      If |X| <= 1, then
c
c        U(N,X) = sin ( (N+1) * arccos(X) ) / sqrt ( 1 - X^2 )
c               = sin ( (N+1) * arccos(X) ) / sin ( arccos(X) )
c
c      else
c
c        U(N,X) = sinh ( (N+1) * arccosh(X) ) / sinh ( arccosh(X) )
c
c  Differential equation:
c
c    (1-X*X) Y'' - 3 X Y' + N (N+2) Y = 0
c
c  First terms:
c
c    U(0,X) =   1
c    U(1,X) =   2 X
c    U(2,X) =   4 X^2 -   1
c    U(3,X) =   8 X^3 -   4 X
c    U(4,X) =  16 X^4 -  12 X^2 +  1
c    U(5,X) =  32 X^5 -  32 X^3 +  6 X
c    U(6,X) =  64 X^6 -  80 X^4 + 24 X^2 - 1
c    U(7,X) = 128 X^7 - 192 X^5 + 80 X^3 - 8X
c
c  Orthogonality:
c
c    For integration over [-1,1] with weight
c
c      W(X) = sqrt(1-X*X), 
c
c    we have
c
c      .lt. U(I,X), U(J,X) > = integral ( -1 <= X <= 1 ) W(X) U(I,X) U(J,X) dX 
c
c    then the result is:
c
c      .lt. U(I,X), U(J,X) >  =  0    if I .ne. J
c      .lt. U(I,X), U(J,X) >  =  PI/2 if I .eq. J
c
c  Recursion:
c
c    U(0,X) = 1,
c    U(1,X) = 2 * X,
c    U(N,X) = 2 * X * U(N-1,X) - U(N-2,X)
c
c  Special values:
c
c    U(N,1) = N + 1
c    U(2N,0) = (-1)^N
c    U(2N+1,0) = 0
c    U(N,X) = (-1)^N * U(N,-X)
c
c  Zeroes:
c
c    M-th zero of U(N,X) is X = cos( M*PI/(N+1)), M = 1 to N
c
c  Extrema:
c
c    M-th extremum of U(N,X) is X = cos( M*PI/N), M = 0 to N
c
c  Norm:
c
c    Integral ( -1 <= X <= 1 ) ( 1 - X^2 ) * U(N,X)^2 dX = PI/2
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest polynomial to compute.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision V(M,0:N), the values of the N+1 Chebyshev
c    polynomials.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision v(m,0:n)
      double precision x(m)

      if ( n .lt. 0 ) then
        return
      end if

      do i = 1, m
        v(i,0) = 1.0D+00
      end do

      if ( n .lt. 1 ) then
        return
      end if

      do i = 1, m
        v(i,1) = 2.0D+00 * x(i)
      end do

      do j = 2, n
        do i = 1, m
          v(i,j) = 2.0D+00 * x(i) * v(i,j-1) - v(i,j-2)
        end do
      end do
     
      return
      end
      subroutine u_polynomial_ab ( a, b, m, n, x, v )

c*********************************************************************72
c
cc U_POLYNOMIAL_AB: evaluates Chebyshev polynomials U(n,x) in [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the domain of definition.
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest polynomial to compute.
c
c    Input, double precision X(M), the evaluation points.
c    It must be the case that A <= X(*) <= B.
c
c    Output, double precision V(M,N+1), the values.
c
      implicit none

      integer m
      integer n

      double precision a
      double precision b
      integer i
      double precision v(1:m,0:n)
      double precision x(1:m)
      double precision y(1:m)

      do i = 1, m
        y(i) = ( ( b - x(i)     )   
     &         - (     x(i) - a ) ) 
     &         / ( b        - a )
      end do

      call u_polynomial ( m, n, x, v )
     
      return
      end
      subroutine u_polynomial_coefficients ( n, c )

c*********************************************************************72
c
cc U_POLYNOMIAL_COEFFICIENTS: coefficients of Chebyshev polynomials U(n,x).
c
c  First terms:
c
c    N/K     0     1      2      3       4     5      6    7      8    9   10
c
c     0      1
c     1      0     2
c     2     -1     0      4
c     3      0    -4      0      8
c     4      1     0    -12      0      16
c     5      0     6      0    -32       0    32
c     6     -1     0     24      0     -80     0     64
c     7      0    -8      0     80       0  -192      0   128
c
c  Recursion:
c
c    U(0,X) = 1,
c    U(1,X) = 2*X,
c    U(N,X) = 2 * X * U(N-1,X) - U(N-2,X)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c  Parameters:
c
c    Input, integer N, the highest order polynomial to compute.
c    Note that polynomials 0 through N will be computed.
c
c    Output, double precision C(0:N,0:N), the coefficients.
c
      implicit none

      integer n

      double precision c(0:n,0:n)
      integer i
      integer j

      if ( n .lt. 0 ) then
        return
      end if

      do j = 0, n
        do i = 0, n
          c(i,j) = 0.0D+00
        end do
      end do

      c(0,0) = 1.0D+00

      if ( n .eq. 0 ) then
        return
      end if

      c(1,1) = 2.0D+00
     
      do i = 2, n
        c(i,0) = - c(i-2,0)
        do j = 1, i - 2
          c(i,j) = 2.0D+00 * c(i-1,j-1) - c(i-2,j)
        end do
        c(i,i-1) = 2.0D+00 * c(i-1,i-2)
        c(i,i) = 2.0D+00 * c(i-1,i-1)
      end do
     
      return
      end
      subroutine u_polynomial_values ( n_data, n, x, fx )

c*********************************************************************72
c
cc U_POLYNOMIAL_VALUES returns values of Chebyshev polynomials U(n,x).
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      ChebyshevU[n,x]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0 before the
c    first call.  On each call, the routine increments N_DATA by 1, and
c    returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N, the order of the function.
c
c    Output, double precision X, the point where the function is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 13 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save n_vec
      save x_vec

      data fx_vec /
     &   0.1000000000000000D+01,
     &   0.1600000000000000D+01,
     &   0.1560000000000000D+01,
     &   0.8960000000000000D+00,
     &  -0.1264000000000000D+00,
     &  -0.1098240000000000D+01,
     &  -0.1630784000000000D+01,
     &  -0.1511014400000000D+01,
     &  -0.7868390400000000D+00,
     &   0.2520719360000000D+00,
     &   0.1190154137600000D+01,
     &   0.1652174684160000D+01,
     &   0.1453325357056000D+01 /
      data n_vec /
     &   0,  1,  2,
     &   3,  4,  5,
     &   6,  7,  8,
     &   9, 10, 11,
     &  12 /
      data x_vec /
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00,
     &  0.8D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine u_polynomial_zeros ( n, z )

c*********************************************************************72
c
cc U_POLYNOMIAL_ZEROS returns zeroes of Chebyshev polynomials U(n,x).
c
c  Discussion:
c
c    The I-th zero of U(N,X) is cos((I-1)*PI/(N-1)), I = 1 to N
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the polynomial.
c
c    Output, double precision Z(N), the zeroes of U(N,X).
c
      implicit none

      integer n

      double precision angle
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision z(n)

      do i = 1, n
        angle = dble ( i ) * pi / dble ( n + 1 )
        z(i) = cos ( angle )
      end do

      return
      end
      subroutine u_quadrature_rule ( n, t, w )

c*********************************************************************72
c
cc U_QUADRATURE_RULE: quadrature rule for U(n,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision T(N), W(N), the points and weights of the rule.
c
      implicit none

      integer n

      double precision bj(n)
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision t(n)
      double precision w(n)

      do i = 1, n
        t(i) = 0.0D+00
      end do

      do i = 1, n
        bj(i) = 0.5D+00
      end do

      w(1) = sqrt ( pi / 2.0D+00 )
      do i = 2, n
        w(i) = 0.0D+00
      end do

      call imtqlx ( n, t, bj, w )

      do i = 1, n
        w(i) = w(i)**2
      end do

      return
      end
      function v_double_product_integral ( i, j )

c*********************************************************************72
c
cc V_DOUBLE_PRODUCT_INTEGRAL: int (-1<x<1) V(i,x)*V(j,x)*sqrt(1+x)/sqrt(1-x) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, the polynomial indices.
c    0 <= I, J.
c
c    Output, double precision V_DOUBLE_PRODUCT_INTEGRAL, the integral.
c
      implicit none

      integer i
      integer j
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision v_double_product_integral
      double precision value

      if ( i .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'V_DOUBLE_PRODUCT_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  0 <= I is required.'
        stop 1
      end if

      if ( j .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'V_DOUBLE_PRODUCT_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  0 <= J is required.'
        stop 1
      end if

      if ( i .ne. j ) then
        value = 0.0D+00
      else
        value = pi
      end if

      v_double_product_integral = value

      return
      end
      subroutine v_polynomial ( m, n, x, v )

c*********************************************************************72
c
cc V_POLYNOMIAL evaluates Chebyshev polynomials V(n,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest polynomial to compute.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision V(M,0:N), the values.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision v(m,0:n)
      double precision x(m)

      if ( n .lt. 0 ) then
        return
      end if

      do i = 1, m
        v(i,0) = 1.0D+00
      end do

      if ( n .lt. 1 ) then
        return
      end if

      do i = 1, m
        v(i,1) = 2.0D+00 * x(i) - 1.0D+00
      end do

      do j = 2, n
        do i = 1, m
          v(i,j) = 2.0D+00 * x(i) * v(i,j-1) - v(i,j-2)
        end do
      end do
     
      return
      end
      subroutine v_polynomial_values ( n_data, n, x, fx )

c*********************************************************************72
c
cc V_POLYNOMIAL_VALUES returns values of Chebyshev polynomials V(n,x).
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      u = Sqrt[(x+1)/2],
c      ChebyshevT[2*n+1,u] / u
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0
c    before the first call.  On each call, the routine increments N_DATA by 1,
c    and returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N, the order of the function.
c
c    Output, double precision X, the point where the function is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 13 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save n_vec
      save x_vec

      data fx_vec /
     &   1.0000000000000000D+00, 
     &   0.6000000000000000D+00, 
     &  -0.0400000000000000D+00, 
     &  -0.6640000000000000D+00, 
     &  -1.0224000000000000D+00, 
     &  -0.9718400000000000D+00, 
     &  -0.5325440000000000D+00, 
     &   0.1197696000000000D+00, 
     &   0.7241753600000000D+00, 
     &   1.0389109760000000D+00, 
     &   0.9380822016000000D+00, 
     &   0.4620205465600000D+00, 
     &  -0.1988493271040000D+00 /
      data n_vec /
     &   0,  1,  2, 
     &   3,  4,  5, 
     &   6,  7,  8, 
     &   9, 10, 11, 
     &  12 /
      data x_vec /
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine v_polynomial_zeros ( n, z )

c*********************************************************************72
c
cc V_POLYNOMIAL_ZEROS returns zeroes of Chebyshev polynomials V(n,x).
c
c  Discussion:
c
c    The I-th zero of U(N,X) is cos((I-1/2)*PI/(N+1/2)), I = 1 to N
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the polynomial.
c
c    Output, double precision Z(N), the zeroes.
c
      implicit none

      integer n

      double precision angle
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision z(n)

      do i = 1, n
        angle = dble ( 2 * n - 2 * i + 1 ) * pi / dble ( 2 * n + 1 )
        z(i) = cos ( angle )
      end do

      return
      end
      function w_double_product_integral ( i, j )

c*********************************************************************72
c
cc W_DOUBLE_PRODUCT_INTEGRAL: int (-1<x<1) W(i,x)*W(j,x)*sqrt(1-x)/sqrt(1+x) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, J, the polynomial indices.
c    0 <= I, J.
c
c    Output, double precision W_DOUBLE_PRODUCT_INTEGRAL, the integral.
c
      implicit none

      integer i
      integer j
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision value
      double precision w_double_product_integral

      if ( i .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'W_DOUBLE_PRODUCT_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  0 <= I is required.'
        stop 1
      end if

      if ( j .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'W_DOUBLE_PRODUCT_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  0 <= J is required.'
        stop 1
      end if

      if ( i .ne. j ) then
        value = 0.0D+00
      else
        value = pi
      end if

      w_double_product_integral = value

      return
      end
      subroutine w_polynomial ( m, n, x, v )

c*********************************************************************72
c
cc W_POLYNOMIAL evaluates Chebyshev polynomials W(n,x).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest polynomial to compute.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision V(M,0:N), the values.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision v(m,0:n)
      double precision x(m)

      if ( n .lt. 0 ) then
        return
      end if

      do i = 1, m
        v(i,0) = 1.0D+00
      end do

      if ( n .lt. 1 ) then
        return
      end if

      do i = 1, m
        v(i,1) = 2.0D+00 * x(i) + 1.0D+00
      end do

      do j = 2, n
        do i = 1, m
          v(i,j) = 2.0D+00 * x(i) * v(i,j-1) - v(i,j-2)
        end do
      end do
     
      return
      end
      subroutine w_polynomial_values ( n_data, n, x, fx )

c*********************************************************************72
c
cc W_POLYNOMIAL_VALUES returns values of Chebyshev polynomials W(n,x).
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      u = Sqrt[(x+1)/2],
c      ChebyshevU[2*n,u]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0
c    before the first call.  On each call, the routine increments N_DATA by 1,
c    and returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N, the order of the function.
c
c    Output, double precision X, the point where the function is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 13 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save n_vec
      save x_vec

      data fx_vec /
     &   1.000000000000000D+00, 
     &   2.600000000000000D+00, 
     &   3.160000000000000D+00, 
     &   2.456000000000000D+00, 
     &   0.769600000000000D+00, 
     &  -1.224640000000000D+00, 
     &  -2.729024000000000D+00, 
     &  -3.141798400000000D+00, 
     &  -2.297853440000000D+00, 
     &  -0.534767104000000D+00, 
     &   1.442226073600000D+00, 
     &   2.842328821760000D+00, 
     &   3.105500041216000D+00 /
      data n_vec /
     &   0,  1,  2, 
     &   3,  4,  5, 
     &   6,  7,  8, 
     &   9, 10, 11, 
     &  12 /
      data x_vec /
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00, 
     &  0.8D+00 /


      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine w_polynomial_zeros ( n, z )

c*********************************************************************72
c
cc W_POLYNOMIAL_ZEROS returns zeroes of Chebyshev polynomials W(n,x).
c
c  Discussion:
c
c    The I-th zero of U(N,X) is cos(I*PI/(N+1/2)), I = 1 to N
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the polynomial.
c
c    Output, double precision Z(N), the zeroes.
c
      implicit none

      integer n

      double precision angle
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision z(n)

      do i = 1, n
        angle = dble ( 2 * ( n - i + 1 ) ) * pi / dble ( 2 * n + 1 )
        z(i) = cos ( angle )
      end do

      return
      end
