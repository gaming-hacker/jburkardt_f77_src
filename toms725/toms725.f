      subroutine dmv ( m, k, h, r, prob, eps, ier, err, cut )

c*********************************************************************72
c
cc DMV calculates the multivariate normal integral.
C
c  Discussion:
c
C    THE INTEGRAL LOWER LIMITS ARE -INFINITY FOR ALL VARIABLES.
C    THE UPPER LIMITS ARE GIVEN BY THE VECTOR H (INPUT).
C    THE CORRELATION MATRIX IS R (INPUT).
C
c  Reference:
c
c    Zvi Drezner,
c    Algorithm 725: Computation of the Multivariate Normal Integral,
c    Transactions on Mathematical Software,
c    Volume 18, Number 4, December 1992, pages 470-480.
c
c  Parameters:
C
c    Input, integer M, the number of variables.
c    M <= 20.
C
C     K IS A FLAG CONTROLLING THE INTEGRATION METHOD (INPUT).
C       2<=K<=10 ONE GAUSIAN QUADRATURE WITH K POINTS FOR EACH
C                VARIABLE IS PERFORMED. EPS IS DISREGARDED.
C      12<=K<=20 PROGRESSIVE GAUSIAN QUADRATURES ARE PERFORMED FOR
C                K=2,3,...,KMAX, WHERE KMAX IS 10 LESS THAN THE INPUT
C                VALUE OF K, UNTIL THE DIFFERENCE BETWEEN THE
C                CALCULATIONS FOR TWO SUCCESSIVE K'S DOES NOT EXCEED EPS.
C                THE LAST VALUE OF K IS RETURNED IN IER.
C                IF KMAX IS REACHED BEFORE A DIFFERENCE OF EPS IS
C                ACHIEVED, THEN IER=0 AND THIS DIFFERENCE IS
C                RETURNED IN ERR.
C       EVERY OTHER K IS EQUIVALENT TO K=20.
C
C     H IS THE VECTOR OF UPPER LIMITS FOR THE INTEGRATION (INPUT).
C
C     R IS THE CORRELATION MATRIX DIMENSIONED R(20,20) (INPUT).
C       IT MUST BE A SYMMETRIC, POSITIVE SEMI-DEFINITE MATRIX.
C       THE CODE DOES NOT CHECK FOR THAT.
C       IF R IS SINGULAR, NUMERICAL PROBLEMS MAY OCCUR.
C
C     PROB IS THE CALCULATED PROBABILITY (OUTPUT).
C
C     EPS IS THE REQUIRED ERROR LIMIT (INPUT).
C
C     IER IS AN ERROR INDICATOR. ON NORMAL TERMINATION 0<IER<=10 IS THE
C         VALUE OF K CORRESPONDING TO THE RETURNED VALUE PROB, (OUTPUT).
C         IER=0 IS RETURNED WHEN THE ERROR IS GREATER THAN EPS.
C         IER=98 MEANS AN ILLEGAL M VALUE (M SHOULD NOT EXCEED 20).
C         IER=99 MEANS THAT R IS SINGULAR OR ITS DETERMINANT IS NEGATIVE.
C
C     ERR IS THE DIFFERENCE IN CALCULATING BY K-1 AND K. CALCULATED
C         ONLY WHEN THE INPUT K>10 (OUTPUT).
C
C     CUT - IF X<-CUT THEN EXP(X) IS IGNORED (ASSUMED ZERO).
C           SUGGESTED VALUE CUT=15 (INPUT).
C
C
C     TWO SUBROUTINES (DPOFA, DPODI) ARE TAKEN FROM THE NAAS:LINPACK
C     LIBRARY. THEY CAN BE REPLACED BY A SUBROUTINE THAT INVERTS A
C     SYMMETRIC MATRIX AND CALCULATES ITS DETERMINANT.
C
      IMPLICIT DOUBLE PRECISION  (A-H,O-Z)

      DIMENSION H(20)
      dimension R(20,20)
C
C  DMV1 CALCULATES THE PROBABILITY FOR A GIVEN K
C
      if ( 2 .le. k .and. k .le. 10 ) then

        prob = dmv1 ( m, k, h, r, cut, ier )

        if ( ier .eq. 0 ) then
          ier = k
        end if

      else

        kmax=10
        if ( 11 .lt. k .and. k .lt. 21 ) then
          kmax = k - 10
        end if

        pold = -1.0D+00

        do ker = 2, kmax

          prob = dmv1 ( m, ker, h, r, cut, ier )

          if ( 0 .lt. ier ) then
            return
          end if

          ier = ker
          err = prob - pold

          if ( dabs ( err ) .le. eps ) then
            return
          end if

          pold = prob

        end do

        ier=0

      end if

      return
      end
      double precision function dmv1 ( m, k, h, r, cut, ier )

c*********************************************************************72
c
cc DMV1 calculates the probability for a given K.
c
c  Discussion:
c
c    DMV1 calculates the probability for a given K BY APPLYING
C     THEOREM 1. IT CALLS SUBROUTINE SOLVE TO CALCULATE INDIVIDUAL
C     VALUES BY (1) and (2).  DMV1 CAN BE CALLED DIRECTLY.
C
cc  Reference:
c
c    Zvi Drezner,
c    Algorithm 725: Computation of the Multivariate Normal Integral,
c    Transactions on Mathematical Software,
c    Volume 18, Number 4, December 1992, pages 470-480.
c
c  Parameters:
c
C     K IS THE NUMBER OF POINTS IN THE GAUSS QUADRATURE (INPUT).
C         IF K<2 OR K>10 THEN K IS SET TO 10.
C
C     IER IS AN ERROR INDICATOR.
C         IER=0 IS NORMAL TERMINATION.
C         IER=98 INDICATES THAT M IS OUT OF RANGE.
C         IER=99 MEANS THAT R IS SINGULAR OR ITS DETERMINANT IS NEGATIVE.
C
C     THE OTHER VARIABLES ARE DESCRIBED ABOVE IN DMV.
C
C
      implicit double precision  (a-h,o-z)
      dimension h(20),r(20,20),h1(20),r1(20,20),list(20),list1(20)
      ier=0
      if(m.lt.0.or.m.gt.20)ier=98
      if(ier.gt.0)return
      dmv1=0.
C
C     WE EXPAND UP TO 2 TO THE POWER OF M TERMS DESCRIBED IN THEOREM 1.
C     THE VECTOR LIST REPRESENTS THE "+" "-" AND INFINITY IN THE TERM.
C     LIST(I) HAS THE VALUE OF 0 IF H(I)<=0, 1 IF H(I)>0, AND
C     THE VALUE OF 2 IF INFINITY IS USED AS THE UPPER LIMIT AND THE
C     VARIABLE IS DROPPED FROM THE INTEGRATION.
C     LIST1 IS THE LIST OF VARIABLES THAT PARTICIPATE IN THE
C     INTEGRATION (THAT DO NOT HAVE INFINITY AS AN UPPER LIMIT).
C
      do i=1,m
        list(i)=0
        if(h(i).gt.0.) list(i)=1
      end do

 20   ipos=0

      l=0
      do i=1,m
        if(list(i).ne.2) then
          l=l+1
          list1(l)=i
        end if
      end do

      do i=1,l
        l1=list1(i)
        h1(i)=h(l1)
        if(list(l1).eq.1)h1(i)=-h1(i)
        if(list(l1).eq.1)ipos=1-ipos
        do j=1,l
          l2=list1(j)
          r1(i,j)=r(l1,l2)
          if(list(l1).eq.1)r1(i,j)=-r1(i,j)
          if(list(l2).eq.1)r1(i,j)=-r1(i,j)
        end do
      end do

      s=solve(l,k,h1,r1,cut,ier)
      if(ier.gt.0)return
      if(ipos.eq.1)s=-s
      dmv1=dmv1+s

      do i=1,m
        if (list(i).ne.0) then
          list(i)=list(i)+1
          if(list(i).eq.2) go to 20
          list(i)=1
        end if
      end do

      return
      end
      double precision function solve ( m, k, h, r, cut, ier )

c*********************************************************************72
c
cc SOLVE calculates probability for a given vector H and value K.
c
C     SOLVE CALCULATES THE MULTIVARIATE PROBABILITY OF A GIVEN VECTOR H
C     AND GIVEN K.  SOLVE CAN BE CALLED DIRECTLY.
C
C     ALL THE ARGUMENTS ARE AS DESCRIBED ABOVE IN DMV1 OR DMV.
C
C     THE INTEGRATION PARAMETERS TAKEN FROM [8] HAVE 15 DIGITS ACCURACY.
C
c  Reference:
c
c    Zvi Drezner,
c    Algorithm 725: Computation of the Multivariate Normal Integral,
c    Transactions on Mathematical Software,
c    Volume 18, Number 4, December 1992, pages 470-480.
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION H(20),H1(20),H2(20),LIST(20),R(20,20),R1(20,20)
      DIMENSION COEF(10,10),X(10,10),Y(20),D(2)
C
C     THE VECTORS COEF AND X CONTAIN THE GAUSS QUADRATURE COEFFICIENTS
C     AND POINTS RESPECTIVELY AND ARE OBTAINED FROM [6].
C
      DATA COEF/10*0.D0,.6405291796843790D0,.245697745768379D0,8*0.D0
     *,.446029770466658D0,.396468266998335D0,4.37288879877644D-2,7*0.D0,
     *.325302999756919D0,.421107101852062D0,.133442500357520D0,
     *.637432348625728D-2,6*0.D0,.248406152028443D0,.392331066652399D0,
     *.211418193076057D0,.332466603513439D-1,.824853344515628D-3,5*0.D0,
     *.196849675488598D0,.349154201525395D0,.257259520584421D0,
     *.0760131375840057D0,.685191862513596D-2,9.84716452019267D-5,
     *4*0.D0,.160609965149261D0,.306319808158099D0,.275527141784905D0,
     *.120630193130784D0,.0218922863438067D0,.00123644672831056D0,
     *1.10841575911059D-5,3*0.D0,.13410918845336D0,.26833075447264D0,
     *.275953397988422D0,.15744828261879D0,.0448141099174625D0,
     *.00536793575602526D0,2.02063649132407D-4,1.19259692659532D-6,
     *2*0.D0,.114088970242118D0,.235940791223685D0,.266425473630253D0,
     *.183251679101663D0,.0713440493066916D0,.0139814184155604D0,
     *.00116385272078519D0,.305670214897831D-4,1.23790511337496D-7,
     *0.D0,0.0985520975191087D0,.208678066608185D0,.252051688403761D0,
     *.198684340038387D0,.097198422760062D0,.0270244164355446D0,
     *.00380464962249537D0,2.28886243044656D-4,4.34534479844469D-6,
     *1.24773714817825D-8/
      DATA X/10*0.D0,.300193931060839D0,1.25242104533372D0,8*0.D0,
     *.190554149798192D0,.848251867544577D0,1.79977657841573D0,7*0.D0,
     *.133776446996068D0,.624324690187190D0,1.34253782564499D0,
     *2.26266447701036D0,6*0.D0,.100242151968216D0,.482813966046201D0,
     *1.06094982152572D0,1.77972941852026D0,2.66976035608766D0,5*0.D0
     *,.0786006594130979D0,.386739410270631D0,.866429471682044D0,
     *1.46569804966352D0,2.172707796939D0,3.03682016932287D0,4*0.D0,
     *.0637164846067008D0,.318192018888619D0,.724198989258373D0,
     *1.23803559921509D0,1.83852822027095D0,2.53148815132768D0,
     *3.37345643012458D0,3*0.D0,.0529786439318514D0,.267398372167767D0,
     *.616302884182402D0,1.06424631211623D0,1.58885586227006D0
     *,2.18392115309586D0,2.86313388370808D0,3.6860071627244D0,2*0.D0,
     *.0449390308011934D0,.228605305560535D0,.532195844331646D0,
     *.927280745338081D0,1.39292385519588D0,1.91884309919743D0,
     *2.50624783400574D0,3.17269213348124D0,3.97889886978978D0,0.D0,
     *.0387385243257289D0,.198233304013083D0,.465201111814767D0
     *,.816861885592273D0,1.23454132402818D0,1.70679814968913D0,
     *2.22994008892494D0,2.80910374689875D0,3.46387241949586D0,
     *4.25536180636608D0/

      pi = 4.0d0*datan(1.d0)
      ier=0
      if(m.lt.0.or.m.gt.20)ier=98
      if(ier.gt.0)return
      if(k.lt.2.or.k.gt.10)k=10

      if ( m .le. 0 ) then
        solve = 1.0d+00
        ier = 0
        return
      end if

      do i=1,m
        do j=1,m
          r1(i,j)=r(i,j)
        end do
      end do
c
c  Factor the matrix and compute its inverse and determinant.
c
      call dpofa(r1,20,m,ier)
      if(ier.gt.0)ier=99
      if(ier.eq.99)return
      call dpodi(r1,20,m,d,11)
      if((d(1).le.0.).or.(d(2).le.-30))ier=99
      if(ier.eq.99)return
c
c     note that another subroutine may give the determinant directly
c     in det and therefore the next statement may be unnecessary.
c     r1 contains the inverse of r. loop 30 caters to the possibility
c     that the inverse is stored only in the lower triangle.
c
      det=d(1)*10.d0**d(2)

      do i=1,m
        do j=i,m
          r1(j,i)=r1(i,j)
        end do
      end do

      prod=1.d0
      do i=1,m
        h1(i)=dsqrt(2./r1(i,i))
        h2(i)=h(i)/h1(i)
        prod=prod*pi*r1(i,i)
        list(i)=1
      end do
      prod=1.d0/dsqrt(det*prod)

      do i=1,m
        do j=1,m
          r1(i,j)=r1(i,j)*h1(i)*h1(j)*0.5d0
        end do
      end do

      solve=0.0d+00

 60   continue

      sum=0.
      do i=1,m
        l=list(i)
        sum=sum+x(l,k)*x(l,k)
        y(i)=x(l,k)-h2(i)
      end do

      do i=1,m
        do j=1,m
          sum=sum-y(i)*y(j)*r1(i,j)
        end do
      end do

      if ( -cut .le. sum ) then

        sum=dexp(sum)
        do i=1,m
          sum=sum*coef(list(i),k)
        end do
 
        solve=solve+sum

      end if

      do i=1,m
        list(i)=list(i)+1
        if(list(i).le.k) go to 60
        list(i)=1
      end do

      solve=solve*prod

      return
      end
      subroutine dpofa(a,lda,n,info)

c*********************************************************************72
c
cc DPOFA factors a double precision symmetric positive definite matrix.
c
c     dpofa is usually called by dpoco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dpoco) = (1 + 18/n)*(time for dpofa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the symmetric matrix to be factored.  only the
c                diagonal and upper triangle are used.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix  r  so that  a = trans(r)*r
c                where  trans(r)  is the transpose.
c                the strict lower triangle is unaltered.
c                if  info .ne. 0 , the factorization is not complete.
c
c        info    integer
c                = 0  for normal return.
c                = k  signals an error condition.  the leading minor
c                     of order  k  is not positive definite.
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas ddot
c     fortran dsqrt
c
      integer lda,n,info
      double precision a(lda,1)
      double precision ddot,t
      double precision s
      integer j,jm1,k
c     begin block with ...exits to 40
c
c
         do 30 j = 1, n
            info = j
            s = 0.0d0
            jm1 = j - 1
            if (jm1 .lt. 1) go to 20
            do 10 k = 1, jm1
               t = a(k,j) - ddot(k-1,a(1,k),1,a(1,j),1)
               t = t/a(k,k)
               a(k,j) = t
               s = s + t*t
   10       continue
   20       continue
            s = a(j,j) - s
c     ......exit
            if (s .le. 0.0d0) go to 40
            a(j,j) = dsqrt(s)
   30    continue
         info = 0
   40 continue
      return
      end
      subroutine dpodi(a,lda,n,det,job)

c*********************************************************************72
c
cc DPODI computes determinant or inverse of symmetric positive definite matrices.
c
c     dpodi computes the determinant and inverse of a certain
c     double precision symmetric positive definite matrix (see below)
c     using the factors computed by dpoco, dpofa or dqrdc.
c
c     on entry
c
c        a       double precision(lda, n)
c                the output  a  from dpoco or dpofa
c                or the output  x  from dqrdc.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        job     integer
c                = 11   both determinant and inverse.
c                = 01   inverse only.
c                = 10   determinant only.
c
c     on return
c
c        a       if dpoco or dpofa was used to factor  a  then
c                dpodi produces the upper half of inverse(a) .
c                if dqrdc was used to decompose  x  then
c                dpodi produces the upper half of inverse(trans(x)*x)
c                where trans(x) is the transpose.
c                elements of  a  below the diagonal are unchanged.
c                if the units digit of job is zero,  a  is unchanged.
c
c        det     double precision(2)
c                determinant of  a  or of  trans(x)*x  if requested.
c                otherwise not referenced.
c                determinant = det(1) * 10.0**det(2)
c                with  1.0 .le. det(1) .lt. 10.0
c                or  det(1) .eq. 0.0 .
c
c     error condition
c
c        a division by zero will occur if the input factor contains
c        a zero on the diagonal and the inverse is requested.
c        it will not occur if the subroutines are called correctly
c        and if dpoco or dpofa has set info .eq. 0 .
c
c     linpack.  this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,dscal
c     fortran mod
c
      integer lda,n,job
      double precision a(lda,1)
      double precision det(2)
      double precision t
      double precision s
      integer i,j,jm1,k,kp1
c
c     compute determinant
c
      if (job/10 .eq. 0) go to 70
         det(1) = 1.0d0
         det(2) = 0.0d0
         s = 10.0d0
         do 50 i = 1, n
            det(1) = a(i,i)**2*det(1)
c        ...exit
            if (det(1) .eq. 0.0d0) go to 60
   10       if (det(1) .ge. 1.0d0) go to 20
               det(1) = s*det(1)
               det(2) = det(2) - 1.0d0
            go to 10
   20       continue
   30       if (det(1) .lt. s) go to 40
               det(1) = det(1)/s
               det(2) = det(2) + 1.0d0
            go to 30
   40       continue
   50    continue
   60    continue
   70 continue
c
c     compute inverse(r)
c
      if (mod(job,10) .eq. 0) go to 140
         do 100 k = 1, n
            a(k,k) = 1.0d0/a(k,k)
            t = -a(k,k)
            call dscal(k-1,t,a(1,k),1)
            kp1 = k + 1
            if (n .lt. kp1) go to 90
            do 80 j = kp1, n
               t = a(k,j)
               a(k,j) = 0.0d0
               call daxpy(k,t,a(1,k),1,a(1,j),1)
   80       continue
   90       continue
  100    continue
c
c        form  inverse(r) * trans(inverse(r))
c
         do 130 j = 1, n
            jm1 = j - 1
            if (jm1 .lt. 1) go to 120
            do 110 k = 1, jm1
               t = a(k,j)
               call daxpy(k,t,a(1,j),1,a(1,k),1)
  110       continue
  120       continue
            t = a(j,j)
            call dscal(j,t,a(1,j),1)
  130    continue
  140 continue
      return
      end
      subroutine daxpy(n,da,dx,incx,dy,incy)

c*********************************************************************72
c
cc DAXPY computes a constant times a vector plus a vector.
c
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dy(1),da
      integer i,incx,incy,ixiy,m,mp1,n
c
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end
      subroutine  dscal(n,da,dx,incx)

c*********************************************************************72
c
cc DSCAL scales a vector by a constant.
c
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision da,dx(1)
      integer i,incx,m,mp1,n,nincx
c
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end
      double precision function ddot(n,dx,incx,dy,incy)

c*********************************************************************72
c
cc DDOT forms the dot product of two vectors.
c
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dy(1),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
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

