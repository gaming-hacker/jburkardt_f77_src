! sample driver program for subroutine cl1.
!
! this program solves a k by n overdetermined system
!
!    ax=b
!
! in the l1 sense subject to l equality constraints
!
!    cx=d
!
! and m inequality constraints
!
!    ex.le.f.
!
! complete details of the parameters may be
! found in the documentation of the subroutine.
!
! the arrays are currently dimensioned to allow problems
! for which k+l+m .le. 100, n .le. 10.
!
! the program may be tested on the following data.
!
!     k = 8
!     l = 3
!     m = 2
!     n = 5
!
!      q = 2  0  1  3  1  7
!          7  4  4 15  7  4
!          9  4  7 20  6  7
!          2  2  1  5  3  4
!          9  3  2 14 10  0
!          4  5  0  9  9  4
!          4  4  9 17 -1  9
!          1  6  2  9  5  6
!          0  4  5  9 -1  5
!          3  2  7 12 -2  1
!          3  6 12 21 -3  6
!          0  3  6  9 -3  5
!          6  2  4 12  4  6
!
!     kode = 0
!     toler = 1.e-5
!     iter = 130
!
!
      dimension q(102,12), x(12), res(100), cu(2,110)
      integer iu(2,110), s(100)
      data klmd, klm2d, nklmd, n2d /100,102,110,12/
! input data.
      read (5,99999) k, l, m, n, kode, toler, iter
      klm = k + l + m
      n1 = n + 1
      do 10 i=1,klm
         read (5,99998) (q(i,j),j=1,n1)
         write (6,99994) (q(i,j),j=1,n1)
   10 continue
      call cl1(k, l, m, n, klmd, klm2d, nklmd, n2d, q,
     * kode, toler, iter, x, res, error, cu, iu, s)
! output kode, iteration count and error norm.
      write (6,99997) kode, iter, error
! output solution vector.
      write (6,99996) (i,x(i),i=1,n)
! output residual error at each point.
      write (6,99995) (i,res(i),i=1,klm)
      stop
99999 format (5i3, e10.0, i3)
99998 format (8f3.0)
99997 format (16h kode,iter,error, 2i10, e18.7)
99996 format (4h sol, i5, e18.7)
99995 format (6h error, i5, e18.7)
99994 format (2h  , 8f5.0)
      end
      subroutine cl1(k, l, m, n, klmd, klm2d, nklmd, n2d,
     * q, kode, toler, iter, x, res, error, cu, iu, s)
! this subroutine uses a modification of the simplex
! method of linear programming to calculate an l1 solution
! to a k by n system of linear equations
!             ax=b
! subject to l linear equality constraints
!             cx=d
! and m linear inequality constraints
!             ex.le.f.
! description of parameters
! k      number of rows of the matrix a (k.ge.1).
! l      number of rows of the matrix c (l.ge.0).
! m      number of rows of the matrix e (m.ge.0).
! n      number of columns of the matrices a,c,e (n.ge.1).
! klmd   set to at least k+l+m for adjustable dimensions.
! klm2d  set to at least k+l+m+2 for adjustable dimensions.
! nklmd  set to at least n+k+l+m for adjustable dimensions.
! n2d    set to at least n+2 for adjustable dimensions
! q      two dimensional real array with klm2d rows and
!        at least n2d columns.
!        on entry the matrices a,c and e, and the vectors
!        b,d and f must be stored in the first k+l+m rows
!        and n+1 columns of q as follows
!             a b
!         q = c d
!             e f
!        these values are destroyed by the subroutine.
! kode   a code used on entry to, and exit
!        from, the subroutine.
!        on entry, this should normally be set to 0.
!        however, if certain nonnegativity constraints
!        are to be included implicitly, rather than
!        explicitly in the constraints ex.le.f, then kode
!        should be set to 1, and the nonnegativity
!        constraints included in the arrays x and
!        res (see below).
!        on exit, kode has one of the
!        following values
!             0- optimal solution found,
!             1- no feasible solution to the
!                constraints,
!             2- calculations terminated
!                prematurely due to rounding errors,
!             3- maximum number of iterations reached.
! toler  a small positive tolerance. empirical
!        evidence suggests toler = 10**(-d*2/3),
!        where d represents the number of decimal
!        digits of accuracy available. essentially,
!        the subroutine cannot distinguish between zero
!        and any quantity whose magnitude does not exceed
!        toler. in particular, it will not pivot on any
!        number whose magnitude does not exceed toler.
! iter   on entry iter must contain an upper bound on
!        the maximum number of iterations allowed.
!        a suggested value is 10*(k+l+m). on exit iter
!        gives the number of simplex iterations.
! x      one dimensional real array of size at least n2d.
!        on exit this array contains a
!        solution to the l1 problem. if kode=1
!        on entry, this array is also used to include
!        simple nonnegativity constraints on the
!        variables. the values -1, 0, or 1
!        for x(j) indicate that the j-th variable
!        is restricted to be .le.0, unrestricted,
!        or .ge.0 respectively.
! res    one dimensional real array of size at least klmd.
!        on exit this contains the residuals b-ax
!        in the first k components, d-cx in the
!        next l components (these will be =0),and
!        f-ex in the next m components. if kode=1 on
!        entry, this array is also used to include simple
!        nonnegativity constraints on the residuals
!        b-ax. the values -1, 0, or 1 for res(i)
!        indicate that the i-th residual (1.le.i.le.k) is
!        restricted to be .le.0, unrestricted, or .ge.0
!        respectively.
! error  on exit, this gives the minimum sum of
!        absolute values of the residuals.
! cu     a two dimensional real array with two rows and
!        at least nklmd columns used for workspace.
! iu     a two dimensional integer array with two rows and
!        at least nklmd columns used for workspace.
! s      integer array of size at least klmd, used for
!        workspace.
! if your fortran compiler permits a single column of a two
! dimensional array to be passed to a one dimensional array
! through a subroutine call, considerable savings in
! execution time may be achieved through the use of the
! following subroutine, which operates on column vectors.
!     subroutine col(v1, v2, xmlt, notrow, k)
! this subroutine adds to the vector v1 a multiple of the
! vector v2 (elements 1 through k excluding notrow).
!     dimension v1(k), v2(k)
!     kend = notrow - 1
!     kstart = notrow + 1
!     if (kend .lt. 1) go to 20
!     do 10 i=1,kend
!        v1(i) = v1(i) + xmlt*v2(i)
!  10 continue
!     if(kstart .gt. k) go to 40
!  20 do 30 i=kstart,k
!       v1(i) = v1(i) + xmlt*v2(i)
!  30 continue
!  40 return
!     end
! see comments following statement labelled 440 for
! instructions on the implementation of this modification.
      double precision sum
      double precision dble
      real q, x, z, cu, sn, zu, zv, cuv, res, xmax, xmin,
     * error, pivot, toler, tpivot
      real abs
      integer i, j, k, l, m, n, s, ia, ii, in, iu, js, kk,
     * nk, n1, n2, jmn, jpn, klm, nkl, nk1, n2d, iimn,
     * iout, iter, klmd, klm1, klm2, kode, nklm, nkl1,
     * klm2d, maxit, nklmd, iphase, kforce, iineg
      integer iabs
      dimension q(klm2d,n2d), x(n2d), res(klmd),
     * cu(2,nklmd), iu(2,nklmd), s(klmd)
!
! initialization.
!
      maxit = iter
      n1 = n + 1
      n2 = n + 2
      nk = n + k
      nk1 = nk + 1
      nkl = nk + l
      nkl1 = nkl + 1
      klm = k + l + m
      klm1 = klm + 1
      klm2 = klm + 2
      nklm = n + klm
      kforce = 1
      iter = 0
      js = 1
      ia = 0
! set up labels in q.
      do 10 j=1,n
         q(klm2,j) = j
   10 continue
      do 30 i=1,klm
         q(i,n2) = n + i
         if (q(i,n1).ge.0.) go to 30
         do 20 j=1,n2
            q(i,j) = -q(i,j)
   20    continue
   30 continue
! set up phase 1 costs.
      iphase = 2
      do 40 j=1,nklm
         cu(1,j) = 0.
         cu(2,j) = 0.
         iu(1,j) = 0
         iu(2,j) = 0
   40 continue
      if (l.eq.0) go to 60
      do 50 j=nk1,nkl
         cu(1,j) = 1.
         cu(2,j) = 1.
         iu(1,j) = 1
         iu(2,j) = 1
   50 continue
      iphase = 1
   60 if (m.eq.0) go to 80
      do 70 j=nkl1,nklm
         cu(2,j) = 1.
         iu(2,j) = 1
         jmn = j - n
         if (q(jmn,n2).lt.0.) iphase = 1
   70 continue
   80 if (kode.eq.0) go to 150
      do 110 j=1,n
         if (x(j)) 90, 110, 100
   90    cu(1,j) = 1.
         iu(1,j) = 1
         go to 110
  100    cu(2,j) = 1.
         iu(2,j) = 1
  110 continue
      do 140 j=1,k
         jpn = j + n
         if (res(j)) 120, 140, 130
  120    cu(1,jpn) = 1.
         iu(1,jpn) = 1
         if (q(j,n2).gt.0.0) iphase = 1
         go to 140
  130    cu(2,jpn) = 1.
         iu(2,jpn) = 1
         if (q(j,n2).lt.0.0) iphase = 1
  140 continue
  150 if (iphase.eq.2) go to 500
! compute the marginal costs.
  160 do 200 j=js,n1
         sum = 0.d0
         do 190 i=1,klm
            ii = q(i,n2)
            if (ii.lt.0) go to 170
            z = cu(1,ii)
            go to 180
  170       iineg = -ii
            z = cu(2,iineg)
  180       sum = sum + dble(q(i,j))*dble(z)
  190    continue
         q(klm1,j) = sum
  200 continue
      do 230 j=js,n
         ii = q(klm2,j)
         if (ii.lt.0) go to 210
         z = cu(1,ii)
         go to 220
  210    iineg = -ii
         z = cu(2,iineg)
  220    q(klm1,j) = q(klm1,j) - z
  230 continue
! determine the vector to enter the basis.
  240 xmax = 0.
      if (js.gt.n) go to 490
      do 280 j=js,n
         zu = q(klm1,j)
         ii = q(klm2,j)
         if (ii.gt.0) go to 250
         ii = -ii
         zv = zu
         zu = -zu - cu(1,ii) - cu(2,ii)
         go to 260
  250    zv = -zu - cu(1,ii) - cu(2,ii)
  260    if (kforce.eq.1 .and. ii.gt.n) go to 280
         if (iu(1,ii).eq.1) go to 270
         if (zu.le.xmax) go to 270
         xmax = zu
         in = j
  270    if (iu(2,ii).eq.1) go to 280
         if (zv.le.xmax) go to 280
         xmax = zv
         in = j
  280 continue
      if (xmax.le.toler) go to 490
      if (q(klm1,in).eq.xmax) go to 300
      do 290 i=1,klm2
         q(i,in) = -q(i,in)
  290 continue
      q(klm1,in) = xmax
! determine the vector to leave the basis.
  300 if (iphase.eq.1 .or. ia.eq.0) go to 330
      xmax = 0.
      do 310 i=1,ia
         z = abs(q(i,in))
         if (z.le.xmax) go to 310
         xmax = z
         iout = i
  310 continue
      if (xmax.le.toler) go to 330
      do 320 j=1,n2
         z = q(ia,j)
         q(ia,j) = q(iout,j)
         q(iout,j) = z
  320 continue
      iout = ia
      ia = ia - 1
      pivot = q(iout,in)
      go to 420
  330 kk = 0
      do 340 i=1,klm
         z = q(i,in)
         if (z.le.toler) go to 340
         kk = kk + 1
         res(kk) = q(i,n1)/z
         s(kk) = i
  340 continue
  350 if (kk.gt.0) go to 360
      kode = 2
      go to 590
  360 xmin = res(1)
      iout = s(1)
      j = 1
      if (kk.eq.1) go to 380
      do 370 i=2,kk
         if (res(i).ge.xmin) go to 370
         j = i
         xmin = res(i)
         iout = s(i)
  370 continue
      res(j) = res(kk)
      s(j) = s(kk)
  380 kk = kk - 1
      pivot = q(iout,in)
      ii = q(iout,n2)
      if (iphase.eq.1) go to 400
      if (ii.lt.0) go to 390
      if (iu(2,ii).eq.1) go to 420
      go to 400
  390 iineg = -ii
      if (iu(1,iineg).eq.1) go to 420
  400 ii = iabs(ii)
      cuv = cu(1,ii) + cu(2,ii)
      if (q(klm1,in)-pivot*cuv.le.toler) go to 420
! bypass intermediate vertices.
      do 410 j=js,n1
         z = q(iout,j)
         q(klm1,j) = q(klm1,j) - z*cuv
         q(iout,j) = -z
  410 continue
      q(iout,n2) = -q(iout,n2)
      go to 350
! gauss-jordan elimination.
  420 if (iter.lt.maxit) go to 430
      kode = 3
      go to 590
  430 iter = iter + 1
      do 440 j=js,n1
         if (j.ne.in) q(iout,j) = q(iout,j)/pivot
  440 continue
! if permitted, use subroutine col of the description
! section and replace the following seven statements down
! to and including statement number 460 by..
!     do 460 j=js,n1
!        if(j .eq. in) go to 460
!        z = -q(iout,j)
!        call col(q(1,j), q(1,in), z, iout, klm1)
! 460 continue
      do 460 j=js,n1
         if (j.eq.in) go to 460
         z = -q(iout,j)
         do 450 i=1,klm1
            if (i.ne.iout) q(i,j) = q(i,j) + z*q(i,in)
  450    continue
  460 continue
      tpivot = -pivot
      do 470 i=1,klm1
         if (i.ne.iout) q(i,in) = q(i,in)/tpivot
  470 continue
      q(iout,in) = 1./pivot
      z = q(iout,n2)
      q(iout,n2) = q(klm2,in)
      q(klm2,in) = z
      ii = abs(z)
      if (iu(1,ii).eq.0 .or. iu(2,ii).eq.0) go to 240
      do 480 i=1,klm2
         z = q(i,in)
         q(i,in) = q(i,js)
         q(i,js) = z
  480 continue
      js = js + 1
      go to 240
! test for optimality.
  490 if (kforce.eq.0) go to 580
      if (iphase.eq.1 .and. q(klm1,n1).le.toler) go to 500
      kforce = 0
      go to 240
! set up phase 2 costs.
  500 iphase = 2
      do 510 j=1,nklm
         cu(1,j) = 0.
         cu(2,j) = 0.
  510 continue
      do 520 j=n1,nk
         cu(1,j) = 1.
         cu(2,j) = 1.
  520 continue
      do 560 i=1,klm
         ii = q(i,n2)
         if (ii.gt.0) go to 530
         ii = -ii
         if (iu(2,ii).eq.0) go to 560
         cu(2,ii) = 0.
         go to 540
  530    if (iu(1,ii).eq.0) go to 560
         cu(1,ii) = 0.
  540    ia = ia + 1
         do 550 j=1,n2
            z = q(ia,j)
            q(ia,j) = q(i,j)
            q(i,j) = z
  550    continue
  560 continue
      go to 160
  570 if (q(klm1,n1).le.toler) go to 500
      kode = 1
      go to 590
  580 if (iphase.eq.1) go to 570
! prepare output.
      kode = 0
  590 sum = 0.d0
      do 600 j=1,n
         x(j) = 0.
  600 continue
      do 610 i=1,klm
         res(i) = 0.
  610 continue
      do 640 i=1,klm
         ii = q(i,n2)
         sn = 1.
         if (ii.gt.0) go to 620
         ii = -ii
         sn = -1.
  620    if (ii.gt.n) go to 630
         x(ii) = sn*q(i,n1)
         go to 640
  630    iimn = ii - n
         res(iimn) = sn*q(i,n1)
         if (ii.ge.n1 .and. ii.le.nk) sum = sum +
     *    dble(q(i,n1))
  640 continue
      error = sum
      return
      end










