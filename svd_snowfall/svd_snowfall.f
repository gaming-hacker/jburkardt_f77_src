      subroutine ch_cap ( ch )

c*********************************************************************72
c
cc CH_CAP capitalizes a single character.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character CH, the character to capitalize.
c
      implicit none

      character ch
      integer itemp

      itemp = ichar ( ch )

      if ( 97 .le. itemp .and. itemp .le. 122 ) then
        ch = char ( itemp - 32 )
      end if

      return
      end
      function ch_eqi ( c1, c2 )

c*********************************************************************72
c
cc CH_EQI is a case insensitive comparison of two characters for equality.
c
c  Example:
c
c    CH_EQI ( 'A', 'a' ) is TRUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character C1, C2, the characters to compare.
c
c    Output, logical CH_EQI, the result of the comparison.
c
      implicit none

      character c1
      character c1_cap
      character c2
      character c2_cap
      logical ch_eqi

      c1_cap = c1
      c2_cap = c2

      call ch_cap ( c1_cap )
      call ch_cap ( c2_cap )

      if ( c1_cap .eq. c2_cap ) then
        ch_eqi = .true.
      else
        ch_eqi = .false.
      end if

      return
      end
      subroutine ch_to_digit ( c, digit )

c*********************************************************************72
c
cc CH_TO_DIGIT returns the integer value of a base 10 digit.
c
c  Example:
c
c     C   DIGIT
c    ---  -----
c    '0'    0
c    '1'    1
c    ...  ...
c    '9'    9
c    ' '    0
c    'X'   -1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character C, the decimal digit, '0' through '9' or blank
c    are legal.
c
c    Output, integer DIGIT, the corresponding integer value.  If C was
c    'illegal', then DIGIT is -1.
c
      implicit none

      character c
      integer digit

      if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

        digit = ichar ( c ) - 48

      else if ( c .eq. ' ' ) then

        digit = 0

      else

        digit = -1

      end if

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
        if ( incx .lt. 0 ) ix = (-n+1)*incx + 1
        if ( incy .lt. 0 ) iy = (-n+1)*incy + 1

        do i = 1, n
          dy(iy) = dy(iy) + da*dx(ix)
          ix = ix + incx
          iy = iy + incy
        end do

      else

        m = mod(n,4)

        do i = 1, m
          dy(i) = dy(i) + da*dx(i)
        end do

        do i = m + 1, n, 4
          dy(i) = dy(i) + da*dx(i)
          dy(i + 1) = dy(i + 1) + da*dx(i + 1)
          dy(i + 2) = dy(i + 2) + da*dx(i + 2)
          dy(i + 3) = dy(i + 3) + da*dx(i + 3)
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
      integer i,incx,incy,ix,iy,m,n

      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c  code for unequal increments or equal increments not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
      end do
      ddot = dtemp
      return
c
c  code for both increments equal to 1
c
c
c  clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
      end do
      if( n .lt. 5 ) go to 60
   40 continue
      do i = m+1, n, 5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     &   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
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

      integer                           incx, n

      double precision dnrm2 
      double precision                  x( * )


      double precision      one         , zero
      parameter           ( one = 1.0d+0, zero = 0.0d+0 )

      integer               ix
      double precision      absxi, norm, scale, ssq

      intrinsic             abs, sqrt

      if( n.lt.1 .or. incx.lt.1 )then
         norm  = zero
      else if( n.eq.1 )then
         norm  = abs( x( 1 ) )
      else
         scale = zero
         ssq   = one
c
c  The following loop is equivalent to this call to the LAPACK
c  auxiliary routine:
c  call dlassq( n, x, incx, scale, ssq )
c
         do ix = 1, 1 + ( n - 1 )*incx, incx
            if( x( ix ).ne.zero )then
               absxi = abs( x( ix ) )
               if( scale.lt.absxi )then
                  ssq   = one   + ssq*( scale/absxi )**2
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

      double precision dx(*),dy(*),dtemp,c,s
      integer i,incx,incy,ix,iy,n

      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c  code for unequal increments or equal increments not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
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
c    If abs ( Z ) < 1, set C = sqrt ( 1 - Z * Z ) and S = Z;
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

      double precision da,db,c,s,roe,scale,r,z

      roe = db
      if( dabs(da) .gt. dabs(db) ) roe = da
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
      integer i,incx,m,n,nincx

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
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
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

      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,n

      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c  code for unequal increments or equal increments not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
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
      subroutine file_column_count ( input_filename, column_num )

c*********************************************************************72
c
cc FILE_COLUMN_COUNT counts the number of columns in the first line of a file.
c
c  Discussion:
c
c    The file is assumed to be a simple text file.
c
c    Most lines of the file is presumed to consist of COLUMN_NUM words,
c    separated by spaces.  There may also be some blank lines, and some
c    comment lines,
c    which have a "#" in column 1.
c
c    The routine tries to find the first non-comment non-blank line and
c    counts the number of words in that line.
c
c    If all lines are blanks or comments, it goes back and tries to analyze
c    a comment line.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the file.
c
c    Output, integer COLUMN_NUM, the number of columns in the file.
c
      implicit none

      integer column_num
      logical got_one
      character * ( * ) input_filename
      integer input_unit
      character * ( 255 ) line
      integer s_len_trim
c
c  Open the file.
c
      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_filename,
     &  status = 'old', form = 'formatted', access = 'sequential' )
c
c  Read one line, but skip blank lines and comment lines.
c
      got_one = .false.

10    continue

        read ( input_unit, '(a)', err = 20 ) line

        if ( s_len_trim ( line ) .eq. 0 ) then
          go to 10
        end if

        if ( line(1:1) .eq. '#' ) then
          go to 10
        end if

        got_one = .true.
        go to 20

      go to 10

20    continue

      if ( .not. got_one ) then

        rewind ( input_unit )

30      continue

          read ( input_unit, '(a)', err = 40 ) line

          if ( s_len_trim ( line ) .eq. 0 ) then
            go to 30
          end if

          got_one = .true.
          go to 40

        go to 30

40    continue

      end if

      close ( unit = input_unit )

      if ( .not. got_one ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILE_COLUMN_COUNT - Warning.'
        write ( *, '(a)' ) '  The file does not contain any data.'
        column_num = -1
        return
      end if

      call s_word_count ( line, column_num )

      return
      end
      subroutine file_row_count ( input_filename, row_num )

c*********************************************************************72
c
cc FILE_ROW_COUNT counts the number of row records in a file.
c
c  Discussion:
c
c    It does not count lines that are blank, or that begin with a
c    comment symbol '#'.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the input file.
c
c    Output, integer ROW_NUM, the number of rows found.
c
      implicit none

      integer bad_num
      integer comment_num
      integer ierror
      character * ( * ) input_filename
      integer input_status
      integer input_unit
      character * ( 255 ) line
      integer record_num
      integer row_num
      integer s_len_trim

      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_filename,
     &  status = 'old' )

      comment_num = 0
      row_num = 0
      record_num = 0
      bad_num = 0

10    continue

        read ( input_unit, '(a)', err = 20, end = 20 ) line

        record_num = record_num + 1

        if ( line(1:1) .eq. '#' ) then
          comment_num = comment_num + 1
          go to 10
        end if

        if ( s_len_trim ( line ) .eq. 0 ) then
          comment_num = comment_num + 1
          go to 10
        end if

        row_num = row_num + 1

      go to 10

20    continue

      close ( unit = input_unit )

      return
      end
      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      subroutine r8col_normalize_li ( m, n, a )

c*********************************************************************72
c
cc R8COL_NORMALIZE_LI normalizes an R8COL with the column infinity norm.
c
c  Discussion:
c
c    Each column is scaled so that the entry of maximum norm has the value 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input/output, double precision A(M,N), the array to be normalized.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision c
      integer i
      integer j

      do j = 1, n

        c = a(1,j)

        do i = 2, m
          if ( abs ( c ) .lt. abs ( a(i,j) ) ) then
            c = a(i,j)
          end if
        end do

        if ( c .ne. 0.0D+00 ) then
          do i = 1, m
            a(i,j) = a(i,j) / c
          end do
        end if

      end do

      return
      end
      subroutine r8col_reverse ( m, n, a )

c*********************************************************************72
c
cc R8COL_REVERSE reverses the order of columns in an R8COL.
c
c  Discussion:
c
c    To reverse the columns is to start with something like
c
c      11 12 13 14 15
c      21 22 23 24 25
c      31 32 33 34 35
c      41 42 43 44 45
c      51 52 53 54 55
c
c    and return
c
c      15 14 13 12 11
c      25 24 23 22 21
c      35 34 33 32 31
c      45 44 43 42 41
c      55 54 53 52 51
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input/output, double precision A(M,N), the matrix.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      integer jhi
      double precision t

      jhi = n / 2

      do j = 1, jhi
        do i = 1, m
          t          = a(i,j)
          a(i,j)     = a(i,n+1-j)
          a(i,n+1-j) = t
        end do
      end do

      return
      end
      subroutine r8mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_COPY copies an R8MAT.
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
c    26 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the order of the matrix.
c
c    Input, double precision A1(M,N), the matrix to be copied.
c
c    Output, double precision A2(M,N), a copy of the matrix.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
      end do

      return
      end
      subroutine r8mat_data_read ( input_filename, m, n, table )

c*********************************************************************72
c
cc R8MAT_DATA_READ reads data from an R8MAT file.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c    The file may contain more than N points, but this routine will
c    return after reading N of them.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the input file.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Output, double precision TABLE(M,N), the data.
c
      implicit none

      integer m
      integer  n

      integer i
      integer ierror
      character * ( * ) input_filename
      integer input_status
      integer input_unit
      integer j
      character * ( 255 ) line
      integer s_len_trim
      double precision table(m,n)
      double precision x(m)

      ierror = 0

      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_filename,
     &  status = 'old' )

      j = 0

10    continue

      if ( j .lt. n ) then

        read ( input_unit, '(a)' ) line

        if ( line(1:1) .eq. '#' .or. s_len_trim ( line ) .eq. 0 ) then
          go to 10
        end if

        call s_to_r8vec ( line, m, x, ierror )

        if ( ierror .ne. 0 ) then
          go to 10
        end if

        j = j + 1

        do i = 1, m
          table(i,j) = x(i)
        end do

        go to 10

      end if

      close ( unit = input_unit )

      return
      end
      subroutine r8mat_header_read ( input_filename, m, n )

c*********************************************************************72
c
cc R8MAT_HEADER_READ reads the header from an R8MAT file.
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
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the input file.
c
c    Output, integer M, spatial dimension.
c
c    Output, integer N, the number of points.
c
      implicit none

      character * ( * ) input_filename
      integer m
      integer n

      call file_column_count ( input_filename, m )

      if ( m .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) '  There was an I/O problem while trying'
        write ( *, '(a)' ) '  to count the number of data columns in'
        write ( *, '(a,a,a)' )
     &    '  the file "', trim ( input_filename ), '".'
        stop
      end if

      call file_row_count ( input_filename, n )

      if ( n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_HEADER_READ - Fatal error!'
        write ( *, '(a)' ) '  There was an I/O problem while trying'
        write ( *, '(a)' ) '  to count the number of data rows in'
        write ( *, '(a,a,a)' )
     &    '  the file "', trim ( input_filename ), '".'
        stop
      end if

      return
      end
      subroutine r8mat_mm ( n1, n2, n3, a, b, c )

c*********************************************************************72
c
cc R8MAT_MM multiplies two R8MAT's.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c    In FORTRAN90, this operation is more efficiently done by the
c    command:
c
c      C(1:N1,1:N3) = MATMUL ( A(1:N1,1;N2), B(1:N2,1:N3) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, N3, the order of the matrices.
c
c    Input, double precision A(N1,N2), B(N2,N3), the matrices to multiply.
c
c    Output, double precision C(N1,N3), the product matrix C = A * B.
c
      implicit none

      integer n1
      integer n2
      integer n3

      double precision a(n1,n2)
      double precision b(n2,n3)
      double precision c(n1,n3)
      double precision c1(n1,n3)
      integer i
      integer j
      integer k

      do i = 1, n1
        do j = 1, n3
          c1(i,j) = 0.0D+00
          do k = 1, n2
            c1(i,j) = c1(i,j) + a(i,k) * b(k,j)
          end do
        end do
      end do

      call r8mat_copy ( n1, n3, c1, c )

      return
      end
      subroutine r8mat_mmt ( n1, n2, n3, a, b, c )

c*********************************************************************72
c
cc R8MAT_MMT multiplies computes C = A * B' for two R8MAT's.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c    In FORTRAN90, this operation is more efficiently done by the
c    command:
c
c      C(1:N1,1:N3) = matmul ( A(1:N1,1;N2), transpose ( B(1:N3,1:N2) ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, N3, the order of the matrices.
c
c    Input, double precision A(N1,N2), B(N3,N2), the matrices to multiply.
c
c    Output, double precision C(N1,N3), the product matrix C = A * B'.
c
      implicit none

      integer n1
      integer n2
      integer n3

      double precision a(n1,n2)
      double precision b(n3,n2)
      double precision c(n1,n3)
      double precision c1(n1,n3)
      integer i
      integer j
      integer k

      do i = 1, n1
        do j = 1, n3
          c1(i,j) = 0.0D+00
          do k = 1, n2
            c1(i,j) = c1(i,j) + a(i,k) * b(j,k)
          end do
        end do
      end do

      call r8mat_copy ( n1, n3, c1, c )

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
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character ( len = * ) title

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
c    Input, character ( len = * ) TITLE, a title.
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
      subroutine r8mat_svd_linpack ( m, n, a, u, s, v )

c*********************************************************************72
c
cc R8MAT_SVD_LINPACK gets the SVD of a matrix using a call to LINPACK.
c
c  Discussion:
c
c    The singular value decomposition of a real MxN matrix A has the form:
c
c      A = U * S * V'
c
c    where
c
c      U is MxM orthogonal,
c      S is MxN, and entirely zero except for the diagonal;
c      V is NxN orthogonal.
c
c    Moreover, the nonzero entries of S are positive, and appear
c    in order, from largest magnitude to smallest.
c
c    This routine calls the LINPACK routine DSVDC to compute the
c    factorization.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 June 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns
c    in the matrix A.
c
c    Input, double precision A(M,N), the matrix whose singular value
c    decomposition we are investigating.
c
c    Output, double precision U(M,M), S(M,N), V(N,N), the factors
c    that form the singular value decomposition of A.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision a_copy(m,n)
      double precision e(max(m+1,n))
      integer i
      integer info
      integer j
      integer lda
      integer ldu
      integer ldv
      integer job
      integer lwork
      double precision s(m,n)
      double precision sdiag(max(m+1,n))
      double precision u(m,m)
      double precision v(n,n)
      double precision work(m)
c
c  Compute the eigenvalues and eigenvectors.
c
      job = 11
      lda = m
      ldu = m
      ldv = n
c
c  The input matrix is destroyed by the routine.  Since we need to keep
c  it around, we only pass a copy to the routine.
c
      do j = 1, n
        do i = 1, m
          a_copy(i,j) = a(i,j)
        end do
      end do

      call dsvdc ( a_copy, lda, m, n, sdiag, e, u, ldu, v, ldv, work, 
     &  job, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_SVD_LINPACK - Failure!'
        write ( *, '(a)' ) '  The SVD could not be calculated.'
        write ( *, '(a)' ) '  LINPACK routine DSVDC returned a nonzero'
        write ( *, '(a,i8)' ) '  value of the error flag, INFO = ', info
        return
      end if
c
c  Make the MxN matrix S from the diagonal values in SDIAG.
c
      do j = 1, n
        do i = 1, m
          s(i,j) = 0.0D+00
        end do
      end do

      do i = 1, min ( m, n )
        s(i,i) = sdiag(i)
      end do
c
c  Note that we do NOT need to transpose the V that comes out of LINPACK!
c
      return
      end
      subroutine r8mat_svd_low_rank ( m, n, r, u, s, v, a )

c*********************************************************************72
c
cc R8MAT_SVD_LOW_RANK forms a rank R approximation using the SVD.
c
c  Discussion:
c
c    The singular value decomposition of a real MxN matrix A has the form:
c
c      A = U * S * V'
c
c    where 
c
c      U is MxM orthogonal,
c      S is MxN, and entirely zero except for the diagonal;
c      V is NxN orthogonal.
c
c    For R <= min ( M, N ), we can form a rank R approximation to A:
c
c      Ar = Ur * Sr * Vr'
c
c    where 
c
c      Ur is MxR orthogonal,
c      Sr is RxR, and entirely zero except for the diagonal;
c      Vr is NxR orthogonal.
c
c    Strictly speaking, we use RM = min ( R, M ), RN = min ( R, N ):
c
c      Ur is M  x RM,
c      Sr is RM x RN,
c      Vr is N  x RN
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns in the matrix A.
c
c    Input, integer R, the desired rank of the approximation.
c
c    Input, double precision U(M,M), S(M,N), V(N,N), the factors
c    that form the singular value decomposition of A.
c
c    Output, double precision A(M,N), the rank R SVD approximation to A.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer r
      integer rm
      integer rn
      double precision s(m,n)
      double precision sv(m,n)
      double precision u(m,m)
      double precision v(n,n)

      rm = min ( r, m )
      rn = min ( r, n )
c
c  SV = Sr * Vr'
c
      call r8mat_mmt ( rm, rn, n, s(1:rm,1:rn), v(1:n,1:rn), sv )
c
c  A = Ur * SV = Ur * Sr * Vr'
c
      call r8mat_mm ( m, rm, n, u(1:m,1:rm), sv, a )

      return
      end
      subroutine r8row_reverse ( m, n, a )

c********************************************************************72
c
cc R8ROW_REVERSE reverses the order of the rows of an R8MAT.
c
c  Discussion:
c
c    To reverse the rows is to start with something like
c
c      11 12 13 14 15
c      21 22 23 24 25
c      31 32 33 34 35
c      41 42 43 44 45
c      51 52 53 54 55
c
c    and return
c
c      51 52 53 54 55
c      41 42 43 44 45
c      31 32 33 34 35
c      21 22 23 24 25
c      11 12 13 14 15
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input/output, double precision A(M,N), the matrix.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer ihi
      integer j
      double precision t

      ihi = m / 2

      do i = 1, ihi
        do j = 1, n
          t          = a(i,j)
          a(i,j)     = a(m+1-i,j)
          a(m+1-i,j) = t
        end do
      end do

      return
      end
      subroutine r8vec_cum0 ( n, a, a_cum )

c*********************************************************************72
c
cc R8VEC_CUM0 computes the cumulutive sums of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    Input:
c
c      A = (/ 1.0, 2.0, 3.0, 4.0 /)
c
c    Output:
c
c      A_CUM = (/ 0.0, 1.0, 3.0, 6.0, 10.0 /)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 May 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A(N), the vector to be summed.
c
c    Output, double precision A_CUM(0:N), the cumulative sums.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_cum(0:n)
      integer i

      a_cum(0) = 0.0D+00

      do i = 1, n
        a_cum(i) = a_cum(i-1) + a(i)
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
      function r8vec_sum ( n, v1 )

c*********************************************************************72
c
cc R8VEC_SUM sums the entries of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    In FORTRAN90, the system routine SUM should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), the vector.
c
c    Output, double precision R8VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer i
      double precision r8vec_sum
      double precision v1(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i)
      end do

      r8vec_sum = value

      return
      end
      function s_len_trim ( s )

c*********************************************************************72
c
cc S_LEN_TRIM returns the length of a string to the last nonblank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S, a string.
c
c    Output, integer S_LEN_TRIM, the length of the string to the last nonblank.
c
      implicit none

      integer i
      character*(*) s
      integer s_len_trim

      do i = len ( s ), 1, -1

        if ( s(i:i) .ne. ' ' ) then
          s_len_trim = i
          return
        end if

      end do

      s_len_trim = 0

      return
      end
      subroutine s_to_r8 ( s, dval, ierror, length )

c*********************************************************************72
c
cc S_TO_R8 reads an R8 from a string.
c
c  Discussion:
c
c    The routine will read as many characters as possible until it reaches
c    the end of the string, or encounters a character which cannot be
c    part of the number.
c
c    Legal input is:
c
c       1 blanks,
c       2 '+' or '-' sign,
c       2.5 blanks
c       3 integer part,
c       4 decimal point,
c       5 fraction part,
c       6 'E' or 'e' or 'D' or 'd', exponent marker,
c       7 exponent sign,
c       8 exponent integer part,
c       9 exponent decimal point,
c      10 exponent fraction part,
c      11 blanks,
c      12 final comma or semicolon,
c
c    with most quantities optional.
c
c  Example:
c
c    S                 DVAL
c
c    '1'               1.0
c    '     1   '       1.0
c    '1A'              1.0
c    '12,34,56'        12.0
c    '  34 7'          34.0
c    '-1E2ABCD'        -100.0
c    '-1X2ABCD'        -1.0
c    ' 2E-1'           0.2
c    '23.45'           23.45
c    '-4.2E+2'         -420.0
c    '17d2'            1700.0
c    '-14e-2'         -0.14
c    'e2'              100.0
c    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string containing the
c    data to be read.  Reading will begin at position 1 and
c    terminate at the end of the string, or when no more
c    characters can be read to form a legal real.  Blanks,
c    commas, or other nonnumeric data will, in particular,
c    cause the conversion to halt.
c
c    Output, double precision DVAL, the value read from the string.
c
c    Output, integer IERROR, error flag.
c    0, no errors occurred.
c    1, 2, 6 or 7, the input number was garbled.  The
c    value of IERROR is the last type of input successfully
c    read.  For instance, 1 means initial blanks, 2 means
c    a plus or minus sign, and so on.
c
c    Output, integer LENGTH, the number of characters read
c    to form the number, including any terminating
c    characters such as a trailing comma or blanks.
c
      implicit none

      logical ch_eqi
      character c
      double precision dval
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer length
      integer nchar
      integer ndig
      double precision rbot
      double precision rexp
      double precision rtop
      character * ( * ) s
      integer s_len_trim

      nchar = s_len_trim ( s )

      ierror = 0
      dval = 0.0D+00
      length = -1
      isgn = 1
      rtop = 0
      rbot = 1
      jsgn = 1
      jtop = 0
      jbot = 1
      ihave = 1
      iterm = 0

10    continue

        length = length + 1

        if ( nchar .lt. length + 1 ) then
          go to 20
        end if

        c = s(length+1:length+1)
c
c  Blank character.
c
        if ( c .eq. ' ' ) then

          if ( ihave .eq. 2 ) then

          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            iterm = 1
          else if ( 1 .lt. ihave ) then
            ihave = 11
          end if
c
c  Comma.
c
        else if ( c .eq. ',' .or. c .eq. ';' ) then

          if ( ihave .ne. 1 ) then
            iterm = 1
            ihave = 12
            length = length + 1
          end if
c
c  Minus sign.
c
        else if ( c .eq. '-' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
            isgn = -1
          else if ( ihave .eq. 6 ) then
            ihave = 7
            jsgn = -1
          else
            iterm = 1
          end if
c
c  Plus sign.
c
        else if ( c .eq. '+' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
          else if ( ihave .eq. 6 ) then
            ihave = 7
          else
            iterm = 1
          end if
c
c  Decimal point.
c
        else if ( c .eq. '.' ) then

          if ( ihave .lt. 4 ) then
            ihave = 4
          else if ( 6 .le. ihave .and. ihave .le. 8 ) then
            ihave = 9
          else
            iterm = 1
          end if
c
c  Scientific notation exponent marker.
c
        else if ( ch_eqi ( c, 'E' ) .or. ch_eqi ( c, 'D' ) ) then

          if ( ihave .lt. 6 ) then
            ihave = 6
          else
            iterm = 1
          end if
c
c  Digit.
c
        else if ( ihave .lt. 11 .and. lle ( '0', c )
     &    .and. lle ( c, '9' ) ) then

          if ( ihave .le. 2 ) then
            ihave = 3
          else if ( ihave .eq. 4 ) then
            ihave = 5
          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            ihave = 8
          else if ( ihave .eq. 9 ) then
            ihave = 10
          end if

          call ch_to_digit ( c, ndig )

          if ( ihave .eq. 3 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
          else if ( ihave .eq. 5 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
            rbot = 10.0D+00 * rbot
          else if ( ihave .eq. 8 ) then
            jtop = 10 * jtop + ndig
          else if ( ihave .eq. 10 ) then
            jtop = 10 * jtop + ndig
            jbot = 10 * jbot
          end if
c
c  Anything else is regarded as a terminator.
c
        else
          iterm = 1
        end if
c
c  If we haven't seen a terminator, and we haven't examined the
c  entire string, go get the next character.
c
        if ( iterm .eq. 1 ) then
          go to 20
        end if

        go to 10

20    continue
c
c  If we haven't seen a terminator, and we have examined the
c  entire string, then we're done, and LENGTH is equal to NCHAR.
c
      if ( iterm .ne. 1 .and. length+1 .eq. nchar ) then
        length = nchar
      end if
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7.
c
      if ( ihave .eq. 1 .or. ihave .eq. 2 .or.
     &     ihave .eq. 6 .or. ihave .eq. 7 ) then
        ierror = ihave
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_TO_R8 - Serious error!'
        write ( *, '(a)' ) '  Illegal or nonnumeric input:'
        write ( *, '(a,a)' ) '    ', s
        return
      end if
c
c  Number seems OK.  Form it.
c
      if ( jtop .eq. 0 ) then
        rexp = 1.0D+00
      else
        if ( jbot .eq. 1 ) then
          rexp = 10.0D+00 ** ( jsgn * jtop )
        else
          rexp = 10.0D+00 ** ( dble ( jsgn * jtop ) / dble ( jbot ) )
        end if
      end if

      dval = dble ( isgn ) * rexp * rtop / rbot

      return
      end
      subroutine s_to_r8vec ( s, n, rvec, ierror )

c*********************************************************************72
c
cc S_TO_R8VEC reads an R8VEC from a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string to be read.
c
c    Input, integer N, the number of values expected.
c
c    Output, double precision RVEC(N), the values read from the string.
c
c    Output, integer IERROR, error flag.
c    0, no errors occurred.
c    -K, could not read data for entries -K through N.
c
      implicit none

      integer  n

      integer i
      integer ierror
      integer ilo
      integer lchar
      double precision rvec(n)
      character * ( * ) s

      i = 0
      ierror = 0
      ilo = 1

10    continue

      if ( i .lt. n ) then

        i = i + 1

        call s_to_r8 ( s(ilo:), rvec(i), ierror, lchar )

        if ( ierror .ne. 0 ) then
          ierror = -i
          go to 20
        end if

        ilo = ilo + lchar

        go to 10

      end if

20    continue

      return
      end
      subroutine s_word_count ( s, nword )

c*********************************************************************72
c
cc S_WORD_COUNT counts the number of "words" in a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string to be examined.
c
c    Output, integer NWORD, the number of "words" in the string.
c    Words are presumed to be separated by one or more blanks.
c
      implicit none

      logical blank
      integer i
      integer lens
      integer nword
      character * ( * ) s

      nword = 0
      lens = len ( s )

      if ( lens .le. 0 ) then
        return
      end if

      blank = .true.

      do i = 1, lens

        if ( s(i:i) .eq. ' ' ) then
          blank = .true.
        else if ( blank ) then
          nword = nword + 1
          blank = .false.
        end if

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
