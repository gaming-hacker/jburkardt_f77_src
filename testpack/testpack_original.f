      program main

c*********************************************************************72
c
cc MAIN is the main program for testing TESTPACK.
c
C     MULTST Test Package with example test program and early ADAPT
C
      external adapt
      character *6 sbname
      real dif(6), exp(6), eps
      integer tst(6), ndims(5), i, maxcls
      data sbname/' adapt'/
      data (dif(i),i=1,6)/110.0,600.0,600.0,100.0,150.0,100.0/
      data (exp(i),i=1,6)/1.5,2.0,2.0,1.0,2.0,2.0/
      data (tst(i),i=1,6)/1,2,3,4,5,6/
      data (ndims(i),i=1,5)/2,3,4,6,8/

      eps = 1e-6
      maxcls = 20000

      call multst(20,6,tst,6,dif,exp,5,ndims,sbname,adapt,eps,10000)

      stop
      end
      subroutine multst(nsamp, tstlim, tstfns, tstmax, difclt, expnts,
     & ndiml, ndims, sbname, subrtn, eps, maxcls)
C
C     Subroutine for testing multidimensional integration routines
C
C      AS DESCRIBED IN 
C       Genz, A. (1987), A Package for Testing Multiple Integration 
C        Subroutines, in {\it Numerical Integration}, P. Keast and 
C        G. Fairweather (Eds.), D. Riedel, pp. 337--340.
C      BY ALAN GENZ, DEPARTMENT of MATHEMATICS, WASHINGTON STATE
C         UNIVERSITY, PULLMAN, WASHINGTON  99164-3113, U.S.A.
C                   alangenz@wsu.edu
C
C************** PARAMETERS *********************************************
C  NSAMP   INTEGER NUMBER OF SAMPLES, MUST NOT EXCEED 50.
C  TSTLIM  INTEGER NUMBER OF TEST INTEGRANDS TO BE USED.
C  TSTFNS  INTEGER ARRAY OF DIMENSION(TSTLIM) OF TEST INTEGRAND INDICES.
C          TSTFNS(I) SPECIFIES THE INDEX OF THE ITH INTEGRAND TO BE
C          TESTED.  TSTFNS(I) MUST NOT EXCEED 6.
C  TSTMAX  INTEGER
C  DIFCLT  REAL ARRAY OF DIMENSION(TSTMAX) OF DIFFICULTY LEVELS.
C          DIFCLT(I) SPECIFIES THE DIFFICULTY LEVEL FOR THE INTEGRAND
C          WITH INDEX I.
C  EXPNTS  REAL ARRAY OF DIMENSION(TSTMAX) OF DIFFICULTY EXPONENTS.
C          EXPNTS(I) SPECIFIES THE DIFFICULTY EXPONENT FOR THE INTEGRAND
C          WITH INDEX I.
C  NDIML   INTEGER
C  NDIMS   INTEGER ARRAY OF DIMENSION(NDIML).  NDIMS(J) SPECIFIES THE
C          NUMBER OF VARIABLES FOR THE INTEGRALS IN THE JTH SERIES OF
C          TESTS.  NDIMS(J) MUST NOT EXCEED 20.
C  SBNAME  CHARACTER STRING OF LENGTH SIX USED TO STORE THE
C          NAME OF THE INTEGRATION SUBROUTINE TO BE TESTED.
C  SUBRTN  EXTERNALLY DECLARED INTEGRATION SUBROUTINE TO BE TESTED.
C          THE CALLING SEQUENCE SHOULD BE THE SAME AS NAG ROUTINE
C          D01FCF(MARK10).
C  EPS     REAL REQUIRED RELATIVE ACCURACY PARAMETER FOR ALL TESTS.
C  MAXCLS  INTEGER MAXIMUM NUMBER OF INTEGRAND CALLS FOR ALL TESTS.
C***********************************************************************
C
      external functn, subrtn
      integer i, itst, ifail, itest, j, k, lenwrk, maxcls, n,
     & nconf, ndiml, nsamp, ndim, ndimv, mxtsfn, maxdim,
     & rcalsa, rcalsb, tstlim, tstfns(tstlim), tstmax, maxsmp, it,
     & digits, ifails, mincls
      parameter (mxtsfn = 6, maxdim = 20, maxsmp = 50, lenwrk = 50000) 
      integer idfclt(mxtsfn), ndims(ndiml)
      real medact(maxsmp), medest(maxsmp), meddsc(maxsmp), 
     & medcla(mxtsfn), medclb(mxtsfn), medcls(maxsmp), concof,
     & callsa(mxtsfn,mxtsfn), callsb(mxtsfn,mxtsfn), dfclt, exn, eps,
     & errest, expons(mxtsfn), errlog, estlog, medrel, medrll(maxsmp),
     & ersrel(mxtsfn,mxtsfn), ersdsc(mxtsfn,mxtsfn), 
     & ersact(mxtsfn,mxtsfn), ersest(mxtsfn,mxtsfn), one, 
     & finest, random, relerr, small, trelib(mxtsfn), 
     & tactrs(mxtsfn), testrs(mxtsfn), value, terdsc(mxtsfn), 
     & wrkstr(lenwrk), zero, tqualt(mxtsfn), qualty(mxtsfn,mxtsfn), 
     & tcalsa(mxtsfn), tcalsb(mxtsfn), qality, 
     & qallty(maxsmp), tactrb(mxtsfn), testrb(mxtsfn), terdsb(mxtsfn), 
     & ersesb(mxtsfn,mxtsfn), ersacb(mxtsfn,mxtsfn), 
     & ersdsb(mxtsfn,mxtsfn), medacb(mxtsfn), medesb(mxtsfn), 
     & meddsb(mxtsfn), difclt(tstmax), expnts(tstmax), valint
      real a(maxdim), b(maxdim), alpha(maxdim), beta(maxdim), functn
      character*6 sbname
      character*14 type(mxtsfn)
      common /prmblk/ ndim, a, b
      common /tstblk/ itst, alpha, beta, dfclt, exn, value
      save type
      data (type(i), i = 1,mxtsfn)/ 'Oscillatory', 'Product Peak',
     &                              'Corner Peak', 'Gaussian',
     &                              'C0 Function', 'Discontinuous'/
C
C     Initialise and compute confidence coefficient
C
      zero = 0
      one = 1
      concof = 0
      nconf = max( 1, (2*nsamp)/5-2 )
      do 10 i = 1,nconf
         concof = 1 + (nsamp-nconf+i)*concof/(nconf-i+1)
 10   continue
      concof = 1 - concof/2**float(nsamp-1)
      small = 1
 20   small = small/2
      if ( 1 + small .ne. 1 ) go to 20
      small = 2*small
      do 30 it = 1,tstlim
         itest = tstfns(it)
         idfclt(it) = difclt(itest)
         expons(it) = expnts(itest)
 30   continue
C     
C     Begin main loop for different numbers of variables
c     
      do 140 ndimv = 1,ndiml
         ndim = ndims(ndimv)
         if ( mod(ndimv-1,6) .eq. 0 ) then
            write (*,99999) nsamp, (idfclt(j),j=1,tstlim)
99999       format (9X, 'Test results with', I4,' samples per test'/
     *           '  Difficulty levels', 10I6)
            write (*,99998) (expons(j),j=1,tstlim)
99998       format ('      Exponents    ', 10F6.1)
            digits = -log10(eps)
            write (*,99997) digits, maxcls
99997       format ('   Requested digits =',I3, ', Maximum values =',I8)
            write (*,99996) sbname, concof
99996       format (/9X, A6, ' Tests, Variable results with confidence',
     &           F5.2/' Vari-  integrand     Correct digits   Relia-',
     &           '  Wrong   Integrand   Quality total'/' ables    type',
     &           6X, 'Estimated   Actual bility digits    Values ',12X,
     &           'Fails')
         end if
C
C     Begin loop for different test integrands
C     
         do 130 it = 1,tstlim
            itest = tstfns(it)
            itst = itest
            exn = expnts(itest)
            dfclt = difclt(itest)
            do 40 j = 1,ndim
               a(j) = 0
               b(j) = 1
 40         continue
            ifails = 0
            medrel = 0
C     
C     Begin loop for different samples
c     
            do 120 k = 1,nsamp
               ifail = 1
               do 50 n = 1,ndim
                  alpha(n) = random()
                  beta(n) = random()
 50            continue
               value = valint(itest)
C     
C     Call integration subroutine
c     
               mincls = 4*2**ndim
               call subrtn(ndim, a, b, mincls, maxcls, functn, eps,
     &              errest, lenwrk, wrkstr, finest, ifail)
               relerr = abs( (finest-value)/value )
               ifails = ifails + min( ifail, 1 )
               relerr = max( min(one, relerr), small )
               errlog = max( zero, -log10(relerr) )
               errest = max( min(one, errest), small)
               estlog = max( zero, -log10(errest) )
               meddsc(k) = max( zero, estlog-errlog )
               medest(k) = estlog
               medact(k) = errlog
               medcls(k) = mincls
               if ( errest .ge. relerr ) medrel = medrel + 1
 120        continue
C     
C     End loop for different samples and compute medians
C     
            call median(nsamp, medest)
            call median(nsamp, medact)
            call median(nsamp, medcls)
            call median(nsamp, meddsc)
            medrel = medrel/nsamp
            trelib(it) = medrel
            tactrs(it) = medact(2)
            testrs(it) = medest(2)
            terdsc(it) = meddsc(2)
            tcalsa(it) = medcls(2)
            tcalsb(it) = medcls(3)
            tactrb(it) = medact(3)
            testrb(it) = medest(3)
            terdsb(it) = meddsc(3)
            ersrel(itest,ndimv) = medrel
            ersest(itest,ndimv) = medest(2)
            ersact(itest,ndimv) = medact(2)
            ersdsc(itest,ndimv) = meddsc(2)
            ersesb(itest,ndimv) = medest(3)
            ersacb(itest,ndimv) = medact(3)
            ersdsb(itest,ndimv) = meddsc(3)
            callsa(itest,ndimv) = medcls(2)
            callsb(itest,ndimv) = medcls(3)
            qality = 0
            if ( medcls(1) .ne. zero ) qality = (medact(1)+1)*
     &           ( medest(1) + 1 - meddsc(1) )/log(medcls(1))
            tqualt(it) = qality
            qualty(itest,ndimv) = qality
            rcalsa = medcls(2)
            rcalsb = medcls(3)
            write (*,99995) ndim, type(itest), medest(2),
     &           medest(3), medact(2), medact(3), medrel, meddsc(2),
     &           meddsc(3), rcalsa, rcalsb, qality, ifails
99995       format (2x, i2, 2x, a14, 2(f4.1, ',', f4.1, 1x), f4.2, 
     &           f4.1, ',', f3.1, i7, ',', i7, f6.2, i5)
 130     continue
C     
C     End loop for different test integrands
C     
         call median(tstlim, tactrs)
         call median(tstlim, trelib)
         call median(tstlim, testrs)
         call median(tstlim, terdsc)
         call median(tstlim, tactrb)
         call median(tstlim, testrb)
         call median(tstlim, terdsb)
         call median(tstlim, tqualt)
         call median(tstlim, tcalsa)
         call median(tstlim, tcalsb)
         rcalsa = tcalsa(1)
         rcalsb = tcalsb(1)
         write (*,99994) ndim, testrs(1), testrb(1), tactrs(1),
     &        tactrb( 1), trelib(1), terdsc(1), terdsb(1), 
     &        rcalsa, rcalsb, tqualt(1)
99994    format (2x, i2, '   Medians', 6x, 2(f4.1, ',', f4.1, 1x),
     &        f4.2, f4.1, ',', f3.1, i7, ',', i7, f6.2/1x)
 140  continue
C
C     End loop for different numbers of variables
C
      if (ndiml.gt.1) then
         write (*,99993) sbname, (ndims(ndimv),ndimv=1,ndiml)
99993    format (/6x, a6, ' Test integrand medians for variables', 12I3)
         write (*,99992)
99992    format (
     &        8X, 'Integrand     Correct digits   Relia-  Wrong',
     &        '   Integrand   Quality'/8X, '  Type      Estimated',
     &        '   Actual bility digits     Values')
         do 160 it = 1,tstlim
            itest = tstfns(it)
            do 150 ndimv = 1,ndiml
               medact(ndimv) = ersact(itest,ndimv)
               medest(ndimv) = ersest(itest,ndimv)
               meddsc(ndimv) = ersdsc(itest,ndimv)
               medacb(ndimv) = ersacb(itest,ndimv)
               medesb(ndimv) = ersesb(itest,ndimv)
               meddsb(ndimv) = ersdsb(itest,ndimv)
               medrll(ndimv) = ersrel(itest,ndimv)
               qallty(ndimv) = qualty(itest,ndimv)
               medcla(ndimv) = callsa(itest,ndimv)
               medclb(ndimv) = callsb(itest,ndimv)
 150        continue
            call median(ndiml, medrll)
            call median(ndiml, medact)
            call median(ndiml, medest)
            call median(ndiml, meddsc)
            call median(ndiml, medacb)
            call median(ndiml, medesb)
            call median(ndiml, meddsb)
            call median(ndiml, qallty)
            call median(ndiml, medcla)
            call median(ndiml, medclb)
            rcalsa = medcla(1)
            rcalsb = medclb(1)
            write (*,99991) type(itest), medest(1), medesb(1), 
     &           medact(1), medacb(1),medrll(1), meddsc(1), meddsb(1), 
     &           rcalsa, rcalsb, qallty(1)
99991       format (6x, a14, 2(f4.1, ',', f4.1, 1x), f4.2, f4.1, ',', 
     &           f3.1, i7, ',', i7, f6.2)
            tactrs(it) = medact(1)
            testrs(it) = medest(1)
            terdsc(it) = meddsc(1)
            tactrb(it) = medacb(1)
            testrb(it) = medesb(1)
            terdsb(it) = meddsb(1)
            tcalsa(it) = medcla(1)
            tcalsb(it) = medclb(1)
            trelib(it) = medrll(1)
            tqualt(it) = qallty(1)
 160     continue
         call median(tstlim, tactrs)
         call median(tstlim, testrs)
         call median(tstlim, terdsc)
         call median(tstlim, tactrb)
         call median(tstlim, testrb)
         call median(tstlim, terdsb)
         call median(tstlim, trelib)
         call median(tstlim, tqualt)
         call median(tstlim, tcalsa)
         call median(tstlim, tcalsb)
         rcalsa = tcalsa(1)
         rcalsb = tcalsb(1)
         write (*,99990) testrs(1), testrb(1), tactrs(1), tactrb(1),
     &        trelib(1), terdsc(1), terdsb(1), rcalsa, rcalsb, tqualt(1)
99990    format (5X, 'Global medians ', 2(f4.1, ',', f4.1, 1x), f4.2,
     &        f4.1, ',', f3.1, i7, ',', i7, f6.2/1x)
      end if

      return
      end
      subroutine median(n, r)
C     
C     Subroutine to compute medians for array r of reals.
c     on exit r(1) contains the medians, (r(2),r(3)) specifies
c     the confidence interval
C
      integer j, k, kmax, n, nconf, nd
      real r(*), rmax, rmed
      do 20 j = 1,n
         kmax = j
         do 10 k = j , n
            if ( r(k) .gt. r(kmax) ) kmax = k
 10      continue
         rmax = r(kmax)
         r(kmax) = r(j)
         r(j) = rmax
 20   continue
      nd = n/2
      if ( mod(n, 2) .eq. 0 ) then
         rmed = (r(nd)+r(nd+1))/2
      else
         rmed = r(nd+1)
      end if
      nconf = max( 1, (2*n)/5-2 )
      rmax = r(n-nconf+1)
      r(3) = r(nconf)
      r(2) = rmax
      r(1) = rmed

      return
      end
      real function valint(itest)
C
C     Function to compute correct values for integrals
c
      integer itest, isum, maxdim, ndim, itst, j
      parameter ( maxdim = 20 )
      real a(maxdim), b(maxdim), alpha(maxdim), beta(maxdim)
      real twopi, fndim, sum, dfclt, exn, value,
     & sign, dfact, sgndm, ab, pi, phi
      integer ic(maxdim)
      parameter ( pi = 3.14159 26535 89793 23844, twopi = 2*pi )
      common /prmblk/ ndim, a, b
      common /tstblk/ itst, alpha, beta, dfclt, exn, value
      fndim = ndim
      sum = 0
      do 10 j=1,ndim
         sum = sum + alpha(j)
 10   continue
      dfact = sum*fndim**exn/dfclt
      do 20 j=1,ndim
         alpha(j) = alpha(j)/dfact
 20   continue
      if ( itest .eq. 1 ) then 
         do 40 j=1,ndim
            b(j) = alpha(j)
            ic(j) = 0
 40      continue
         value = 0
 50      isum = 0
         sum = twopi*beta(1)
         do 70 j = 1,ndim
            if ( ic(j) .ne. 1 ) sum = sum + alpha(j)
            isum = isum + ic(j)
 70       continue
          sign = 1 + 2*((isum/2)*2-isum)
          if ( (ndim/2)*2 .eq. ndim ) value = value + sign*cos(sum)
          if ( (ndim/2)*2 .ne. ndim ) value = value + sign*sin(sum)
          do 80 j = 1,ndim
             ic(j) = ic(j) + 1
             if ( ic(j) .lt. 2 ) go to 50
             ic(j) = 0
 80       continue
          value = value*float((-1)**(ndim/2))
      else if ( itest .eq. 2 ) then 
         value = 1
         do 100 j=1,ndim
            value = value*(atan((1-beta(j))*alpha(j))-atan(-beta(j)*
     &           alpha(j)))*alpha(j)
 100     continue
      else if ( itest .eq. 3 ) then 
         value = 0
         sgndm = 1
         do 120 j=1,ndim
            sgndm = -sgndm/float(j)
            b(j) = alpha(j)
            ic(j) = 0
 120     continue
 130     sum = 1
         isum = 0
         do 150 j=1,ndim
            if ( ic(j) .ne. 1 ) sum = sum + alpha(j)
            isum = isum + ic(j)
 150     continue
         sign = 1 + 2*((isum/2)*2-isum)
         value = value + sign/sum
         do 160 j=1,ndim
            ic(j) = ic(j) + 1
            if ( ic(j) .lt. 2 ) go to 130
            ic(j) = 0
 160     continue
         value = value*sgndm
      else if ( itest .eq. 4 ) then 
         value = 1
         ab = sqrt(2d0)
         do 180 j = 1, ndim
            value = value*(sqrt(pi)/alpha(j))
     &             * (  phi( (1-beta(j))*ab*alpha(j) )
     &                - phi(   -beta(j) *ab*alpha(j) ) ) 
 180     continue
      else if ( itest .eq. 5 ) then 
         value = 1
         do 200 j=1,ndim
            ab = alpha(j)*beta(j)
            value = value*(2-exp(-ab)-exp(ab-alpha(j)))/alpha(j)
 200     continue
      else if ( itest .eq. 6 ) then 
         value = 1
         do 220 j=1,ndim
            if ( j.gt.2 ) beta(j) = 1
            value = value*(exp(alpha(j)*beta(j))-1)/alpha(j)
 220     continue
      end if
      valint = value

      return
      end
      real function functn(ndim, z)
C
C     Test integrand family function
c
      integer ndim, maxdim, itst, j
      parameter ( maxdim = 20 )
      real sum, dfclt, exn, z(ndim), twopi, expmax, 
     &     alpha(maxdim), beta(maxdim), value, f
      common /tstblk/ itst, alpha, beta, dfclt, exn, value
      parameter ( twopi = 6.28318 53072, expmax = 100 )
      f = 0
      if ( itst .eq. 1 ) then
         sum = twopi*beta(1)
         do 20 j = 1,ndim
            sum = sum + z(j)
 20      continue
         f = cos(sum)
      else if ( itst .eq. 2 ) then
         sum = 1
         do 40 j = 1,ndim
            sum = sum/( 1/alpha(j)**2 + (z(j)-beta(j))**2 )
 40      continue
         f = sum
      else if ( itst .eq. 3 ) then
C
C     Note: for this case, the BETA's are used to randomly select 
C          (from 2**NDIM possibilities) a corner for the peak
C
         sum = 1
         do 60 j = 1,ndim
            if ( beta(j) .lt. 0.5 ) then
               sum = sum + z(j)
            else
               sum = sum + alpha(j) - z(j)
            end if
 60      continue
         f = sum**(-ndim-1)
      else if ( itst .eq. 4 ) then
         sum = 0
         do 80 j = 1,ndim
            sum = sum + (alpha(j)*(z(j)-beta(j)))**2
 80      continue
         if ( sum .lt. expmax ) f = exp(-sum)
      else if ( itst .eq. 5 ) then
         sum = 0
         do 100 j = 1,ndim
            sum = sum + alpha(j)*abs(z(j)-beta(j))
 100     continue
         if ( sum .lt. expmax ) f = exp(-sum)
      else if ( itst .eq. 6 ) then
         do 120 j = 1,ndim
            if ( z(j) .gt. beta(j) ) go to 140
 120     continue
         sum = 0
         do 130 j = 1,ndim
            sum = sum + alpha(j)*z(j)
 130     continue
         f = exp(sum)
      end if
 140  functn = f

      return
      end
      real function random()
C
C     Portable random number generator
c
      integer seed, a, p, b15, b16, xhi, xalo, leftlo, fhi, k
      real prime
      parameter (a = 16807, b15 = 32768, b16 = 65536, p = 2147483647)
      parameter (prime = p)
      save seed
      data seed/123456/
      xhi = seed/b16
      xalo = (seed-xhi*b16)*a
      leftlo = xalo/b16
      fhi = xhi*a + leftlo
      k = fhi/b15
      seed = (((xalo-leftlo*b16)-p)+(fhi-k*b15)*b16) + k
      if (seed.lt.0) seed = seed + p
      random = seed/prime

      return
      end
      real function phi(z)
C
C	Normal distribution probabilities accurate to 1.e-7.
C	Z = no. of standard deviations from the mean.
C	PHI = probability to the left of Z.  
C       EXPNTL = the probability density.
C
C       Based upon algorithm 5666 for the error function, from:
C       Hart, J.F. et al, 'Computer Approximations', Wiley 1968
C
C       Programmer: Alan Miller
C
C	Latest revision - 30 March 1986
C
	real p0, p1, p2, p3, p4, p5, p6, 
     &       q0, q1, q2, q3, q4, q5, q6, q7,
     &       z, p, expntl, rootpi, zabs
        parameter(p0 = 220.20 68679 12376 1,
     &	          p1 = 221.21 35961 69931 1, 
     &            p2 = 112.07 92914 97870 9,
     &	          p3 = 33.912 86607 83830 0,
     &            p4 = 6.3739 62203 53165 0,
     &	          p5 = .70038 30644 43688 1, 
     &            p6 = .035262 49659 98910 9)
        parameter(q0 = 440.41 37358 24752 2,
     &	          q1 = 793.82 65125 19948 4, 
     &            q2 = 637.33 36333 78831 1,
     &	          q3 = 296.56 42487 79673 7, 
     &            q4 = 86.780 73220 29460 8,
     &	          q5 = 16.064 17757 92069 5, 
     &            q6 = 1.7556 67163 18264 2,
     &	          q7 = .088388 34764 83184 4)
        parameter(rootpi = 2.5066 28274 63100 1)
c
	zabs = abs(z)
C
C	|Z| > 12
C
	if ( zabs .gt. 12 ) then
           p = 0
        else
C
C	|Z| <= 12
C
           expntl = exp(-zabs**2/2)
C
C	|Z| < 7
C
           if ( zabs .lt. 7 ) then
              p = expntl*((((((p6*zabs + p5)*zabs + p4)*zabs + p3)*zabs
     &           + p2)*zabs + p1)*zabs + p0)/(((((((q7*zabs + q6)*zabs
     &           + q5)*zabs + q4)*zabs + q3)*zabs + q2)*zabs + q1)*zabs
     &           + q0)
C
C	|Z| >= CUTOFF.
C
           else
              p = expntl/(zabs + 1/(zabs + 2/(zabs + 3/(zabs + 4/
     &             (zabs + 0.65)))))/rootpi
           end if
        end if
        if (z .gt. 0) p = 1 - p
        phi = p

      return
      end
      subroutine adapt(ndim,a,b,minpts,maxpts,functn,eps,relerr,lenwrk,
     & wrkstr,finest,ifail)
C
C  Adaptive multidimensional integration subroutine
C               
C         Author: Alan Genz
C                 MATHEMATICS DEPARTMENT
C                 WASHINGTON STATE UNIVERSITY
C                 PULMAN, WA 99164-3130
C
C**************  PARAMETERS FOR ADAPT  ********************************
C***** INPUT PARAMETERS
C  NDIM    NUMBER OF VARIABLES, MUST EXCEED 1, BUT NOT EXCEED 100
C  A       REAL ARRAY OF LOWER LIMITS, WITH DIMENSION NDIM
C  B       REAL ARRAY OF UPPER LIMITS, WITH DIMENSION NDIM
C  MINPTS  MINIMUM NUMBER OF FUNCTION EVALUATIONS TO BE ALLOWED,
C          MINPTS MUST NOT EXCEED MAXPTS.  IF MINPTS<0 THEN THE
C          ROUTINE ASSUMES A PREVIOUS CALL HAS BEEN MADE WITH THE
C          SAME INTEGRAND AND CONTINUES THAT CALCULATION.
C  MAXPTS  MAXIMUM NUMBER OF FUNCTION EVALUATIONS TO BE ALLOWED,
C          WHICH MUST BE AT LEAST RULCLS, WHERE
C          RULCLS  =   2**NDIM+2*NDIM**2+2*NDIM+1, WHEN NDIM <16 AND
C          RULCLS  =  (NDIM*(14-NDIM*(6-4*NDIM))/3+1, WHEN NDIM >15.
C          FOR NDIM  =  2   3   4   5   6   7   8   9   10   11   12
C           RULCLS  =  17  33  57  93 149 241 401 693 1245 2313 4409     
C          A SUGGESTED STARTING VALUE FOR MAXPTS IS 100*RULCLS. IF
C          THIS IS NOT LARGE ENOUGH FOR THE REQUIRED ACCURACY, THEN
C          MAXPTS (AND LENWRK) SHOULD BE INCREASED ACCORDINGLY.
C  FUNCTN  EXTERNALLY DECLARED USER DEFINED FUNCTION TO BE INTEGRATED.
C          IT MUST HAVE PARAMETERS (NDIM,Z), WHERE Z IS A REAL ARRAY
C          OF DIMENSION NDIM.
C  EPS     REQUIRED RELATIVE ACCURACY
C  LENWRK  LENGTH OF ARRAY WRKSTR OF WORKING STORAGE, THE ROUTINE
C          NEEDS (2*NDIM+3)*(1+MAXPTS/RULCLS)/2 FOR LENWRK IF
C          MAXPTS FUNCTION CALLS ARE USED.
C***** OUTPUT PARAMETERS
C  MINPTS  ACTUAL NUMBER OF FUNCTION EVALUATIONS USED BY ADAPT
C  WRKSTR  REAL ARRAY OF WORKING STORAGE OF DIMENSION (LENWRK).
C  RELERR  ESTIMATED RELATIVE ACCURACY OF FINEST
C  FINEST  ESTIMATED VALUE OF INTEGRAL
C  IFAIL   IFAIL = 0 FOR NORMAL EXIT, WHEN ESTIMATED RELATIVE ACCURACY
C                  RELERR IS LESS THAN EPS WITH MAXPTS OR LESS FUNCTION
C                  CALLS MADE.
C          IFAIL = 1 IF MAXPTS WAS TOO SMALL FOR ADAPT TO OBTAIN THE
C                  REQUIRED RELATIVE ACCURACY EPS.  IN THIS CASE ADAPT
C                  RETURNS A VALUE OF FINEST WITH ESTIMATED RELATIVE
C                  ACCURACY RELERR.
C          IFAIL = 2 IF LENWRK TOO SMALL FOR MAXPTS FUNCTION CALLS.  IN
C                  THIS CASE ADAPT RETURNS A VALUE OF FINEST WITH
C                  ESTIMATED ACCURACY RELERR USING THE WORKING STORAGE
C                  AVAILABLE, BUT RELERR WILL BE GREATER THAN EPS.
C          IFAIL = 3 IF NDIM < 2, NDIM > 100, MINPTS > MAXPTS,
C                  OR MAXPTS < RULCLS.
C***********************************************************************
C*****  FOR DOUBLE PRECISION CHANGE REAL TO DOUBLE PRECISION IN THE
C        NEXT STATEMENT.
      integer divaxo,divaxn,divflg,funcls,ifail,index1,index2,i,j,k,
     & l,m,n,lenwrk,maxpts,minpts,ndim,rgnstr,rulcls,sbrgns,sbtmpp,
     & subrgn,subtmp
      real a(ndim),b(ndim),center(100),df1,df2,dif,
     & difmax,eps,finest,functn,f1,f2,f3,f4,lamda2,lamda4,lamda5,
     & nine, one,ratio,relerr,rgncmp,rgnerr,rgnval,rgnvol,
     & sum1,sum2,sum3,sum4,sum5,two,twondm,weitp1,weitp2,
     & weitp3,weitp4,weit1,weit2,weit3,weit4,weit5,width(100),
     & widthl(100),wrkstr(lenwrk),z(100)
      external functn
      ifail = 3
      relerr = 1
      funcls = 0
      if(ndim.lt.2.or.ndim.gt.100) goto 300
      if(minpts.gt.maxpts) goto 300
C
C*****  Initialisation of subroutine
c
      one = 1
      two = 2
      nine = 9
      twondm = two**ndim
      rgnstr = 2*ndim+3
      divaxo = 0
C
C*****  End subroutine initialisation
C*****  Basic rule initialisation
C
      lamda5 = nine/19
      if(ndim.gt.15) goto 10
      rulcls = 2**ndim+2*ndim*ndim+2*ndim+1
      lamda4 = nine/10
      lamda2 = nine/70
      weit5 = 1/(3*lamda5)**3/twondm
      goto 20
   10 rulcls = 1+(ndim*(12+(ndim-1)*(6+(ndim-2)*4)))/3
      ratio = (ndim-2)/nine
      lamda4 = (one/5-ratio)/(one/3-ratio/lamda5)
      ratio = (1-lamda4/lamda5)*(ndim-1)*ratio/6
      lamda2 = (one/7-lamda4/5-ratio)/(one/5-lamda4/3-ratio/lamda5)
      weit5 = 1/(6*lamda5)**3
   20 weit4 = (one/15-lamda5/9)/(4*(lamda4-lamda5)*lamda4**2)
      weit3 = (one/7-(lamda5+lamda2)/5+lamda5*lamda2/3)/
     &     (2*lamda4*(lamda4-lamda5)*(lamda4-lamda2))-2*(ndim-1)*weit4
      weit2 = (one/7-(lamda5+lamda4)/5+lamda5*lamda4/3)/
     &       (2*lamda2*(lamda2-lamda5)*(lamda2-lamda4))
      if(ndim.gt.15) weit1 = 1-2*ndim*(weit2+weit3+(ndim-1)*
     &      (weit4+2*(ndim-2)*weit5/3))
      if(ndim.lt.16) weit1 = 1-2*ndim*(weit2+weit3+(ndim-1)*
     &      weit4)-twondm*weit5
      weitp4 = 1/(6*lamda4)**2
      weitp3 = (one/5-lamda2/3)/(2*lamda4*(lamda4-lamda2))-
     &        2*(ndim-1)*weitp4
      weitp2 = (one/5-lamda4/3)/(two*lamda2*(lamda2-lamda4))
      weitp1 = 1-2*ndim*(weitp2+weitp3+(ndim-1)*weitp4)
      ratio = lamda2/lamda4
      lamda5 = sqrt(lamda5)
      lamda4 = sqrt(lamda4)
      lamda2 = sqrt(lamda2)
      if(maxpts.lt.rulcls) return
C
C*****  End basic rule initialisation
      if(minpts.lt.0) sbrgns = wrkstr(lenwrk-1)
      if(minpts.lt.0) goto 280
      do 30 j = 1,ndim
        width(j) = (b(j)-a(j))/2
   30   center(j) = a(j)+width(j)
      finest = 0
      wrkstr(lenwrk) = 0
      divflg = 1
      subrgn = rgnstr
      sbrgns = rgnstr
C
C*****  BEGIN BASIC RULE
   40 RGNVOL = TWONDM
      DO 50 J = 1,NDIM
        RGNVOL = RGNVOL*WIDTH(J)
   50   Z(J) = CENTER(J)
      SUM1 = FUNCTN(NDIM,Z)
C*****  Compute symmetric sums of functn(lamda2,0,0,...,0) and
c       functn(lamda4,0,0,...,0), and maximum fourth difference
      difmax = -1
      sum2 = 0
      sum3 = 0
      do 60 j = 1,ndim
        z(j) = center(j)-lamda2*width(j)
        f1 = functn(ndim,z)
        z(j) = center(j)+lamda2*width(j)
        f2 = functn(ndim,z)
        widthl(j) = lamda4*width(j)
        z(j) = center(j)-widthl(j)
        f3 = functn(ndim,z)
        z(j) = center(j)+widthl(j)
        f4 = functn(ndim,z)
        sum2 = sum2+f1+f2
        sum3 = sum3+f3+f4
        df1 = f1+f2-2*sum1
        df2 = f3+f4-2*sum1
        dif = abs(df1-ratio*df2)
        if(dif.le.difmax) go to 60
         difmax = dif
         divaxn = j
   60   z(j) = center(j)
      if(sum1.eq.sum1+difmax/8) divaxn = mod(divaxo,ndim)+1
C*****  Compute symmetric sum of functn(lamda4,lamda4,0,0,...,0)
      sum4 = 0
      do 90 j = 2,ndim
        do 80 k = j,ndim
          do 70 l = 1,2
            widthl(j-1) = -widthl(j-1)
            z(j-1) = center(j-1)+widthl(j-1)
            do 70 m = 1,2
              widthl(k) = -widthl(k)
              z(k) = center(k)+widthl(k)
   70         sum4 = sum4+functn(ndim,z)
   80     z(k) = center(k)
   90   z(j-1) = center(j-1)
C*****  If ndim < 16 compute symmetric sum of
c*****  functn(lamda5,lamda5,...,lamda5)
      sum5 = 0
      if(ndim.gt.15) goto 130
      do 100 j = 1,ndim
        widthl(j) = -lamda5*width(j)
  100   z(j) = center(j)+widthl(j)
  110 sum5 = sum5+functn(ndim,z)
      do 120 j = 1,ndim
        widthl(j) = -widthl(j)
        z(j) = center(j)+widthl(j)
        if(widthl(j).gt.0) goto 110
  120   continue
      goto 190
C*****  If NDIM > 15 compute symmetric sum of
C*****  FUNCTN(LAMDA5,LAMDA5,LAMDA5,0,0,...,0)
  130 do 140 j = 1,ndim
  140  widthl(j) = lamda5*width(j)
      do 180 i = 3,ndim
        do 170 j = i,ndim
          do 160 k = j,ndim
            do 150 l = 1,2
              widthl(i-2) = -widthl(i-2)
              z(i-2) = center(i-2)+widthl(i-2)
              do 150 m = 1,2
                widthl(j-1) = -widthl(j-1)
                z(j-1) = center(j-1)+widthl(j-1)
                do 150 n = 1,2
                  widthl(k) = -widthl(k)
                  z(k) = center(k)+widthl(k)
  150             sum5 = sum5+functn(ndim,z)
  160       z(k) = center(k)
  170     z(j-1) = center(j-1)
  180   z(i-2) = center(i-2)
C*****  Compute fifth and seventh degree rules and error
  190 rgncmp = rgnvol*(weitp1*sum1+weitp2*sum2+weitp3*sum3+weitp4*sum4)
      rgnval = rgnvol*(weit1*sum1+weit2*sum2+weit3*sum3+weit4*sum4+
     *               weit5*sum5)
      rgnerr = abs(rgnval-rgncmp)
c
C*****  End basic rule
      finest = finest+rgnval
      wrkstr(lenwrk) = wrkstr(lenwrk)+rgnerr
      funcls = funcls+rulcls
c
C*****  Place results of basic rule into partially ordered list
c*****  according to subregion error
      if(divflg.eq.1) go to 230
C
C*****  When DIVFLG = 0 start at top of list and move down list tree to
c       find correct position for results from first half of recently
c       divided subregion
  200 subtmp = 2*subrgn
      if(subtmp.gt.sbrgns) go to 250
       if(subtmp.eq.sbrgns) go to 210
       sbtmpp = subtmp+rgnstr
       if(wrkstr(subtmp).lt.wrkstr(sbtmpp)) subtmp = sbtmpp
  210  if(rgnerr.ge.wrkstr(subtmp)) go to 250
        do 220 k = 1,rgnstr
          index1 = subrgn-k+1
          index2 = subtmp-k+1
  220     wrkstr(index1) = wrkstr(index2)
        subrgn = subtmp
      goto 200
c
C*****  When DIVFLG = 1 start at bottom right branch and move up list
c       tree to find correct position for results from second half of
c       recently divided subregion
  230 subtmp = (subrgn/(rgnstr*2))*rgnstr
      if(subtmp.lt.rgnstr) go to 250
      if(rgnerr.le.wrkstr(subtmp)) go to 250
       do 240 k = 1,rgnstr
         index1 = subrgn-k+1
         index2 = subtmp-k+1
  240    wrkstr(index1) = wrkstr(index2)
       subrgn = subtmp
      goto 230
C*****  Store results of basic rule in correct position in list
  250 wrkstr(subrgn) = rgnerr
      wrkstr(subrgn-1) = rgnval
      wrkstr(subrgn-2) = divaxn
      do 260 j = 1,ndim
        subtmp = subrgn-2*(j+1)
        wrkstr(subtmp+1) = center(j)
  260   wrkstr(subtmp) = width(j)
      if(divflg.eq.1) go to 270
C*****  When DIVFLG = 0 prepare for second application of basic rule
      center(divaxo) = center(divaxo)+2*width(divaxo)
      sbrgns = sbrgns+rgnstr
      subrgn = sbrgns
      divflg = 1
C*****  Loop back to apply basic rule to other half of subregion
      go to 40
C
C*****  End ordering and storage of basic rule results
C*****  Make checks for possible termination of routine
c
  270 relerr = one
      if(wrkstr(lenwrk).le.0) wrkstr(lenwrk) = 0
      if(abs(finest).ne.0) relerr = wrkstr(lenwrk)/abs(finest)
      if(relerr.gt.1) relerr = 1
      if(sbrgns+rgnstr.gt.lenwrk-2) ifail = 2
      if(funcls+2*rulcls.gt.maxpts) ifail = 1
      if(relerr.lt.eps.and.funcls.ge.minpts) ifail = 0
      if(ifail.lt.3) goto 300
C
C*****  Prepare to use basic rule on each half of subregion with largest
c       error
  280 divflg = 0
      subrgn = rgnstr
      wrkstr(lenwrk) = wrkstr(lenwrk)-wrkstr(subrgn)
      finest = finest-wrkstr(subrgn-1)
      divaxo = wrkstr(subrgn-2)
      do 290 j = 1,ndim
        subtmp = subrgn-2*(j+1)
        center(j) = wrkstr(subtmp+1)
  290   width(j) = wrkstr(subtmp)
      width(divaxo) = width(divaxo)/2
      center(divaxo) = center(divaxo)-width(divaxo)
C
C*****  Loop back to apply basic rule
c
      goto 40
c
C*****  Termination point
c
  300 minpts = funcls
      wrkstr(lenwrk-1) = sbrgns

      return
      end




