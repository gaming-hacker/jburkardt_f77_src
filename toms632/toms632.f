      subroutine mkp ( n, m, p, w, k, bck, xstar, vstar )

c*********************************************************************72
c
cc MKP solves a 0-1 multiple knapsack problem.
c
c  Discussion:
c
c    This routine solves a 0-1 multiple knapsack problem of N
c    items (with 2 <= N) and M knapsacks (with 1 <= M).
c    Hence, it is also suitable for a 0-1 single knapsack problem.
c
c    The problem to be solved is:
c
c      Maximize:
c
c        VSTAR = P(1)*(X(1,1) + ... + X(M,1)) + ...
C          ... + P(N)*(X(1,N) + ... + X(M,N))
c
c      subject to:
c
c        W(1)*X(I,1) + ... + W(N)*X(I,N) <= K(I)   for  I=1,...,M
C        X(1,J) + ... + X(M,J) <= 1                for  J=1,...,N
C        X(I,J) = 0 or 1                           for  I=1,...,M and J=1,...,N,
c
c    with all P(J), W(J) and K(I) positive integers.
c
c    MKP requires that the arrays P and W are sorted so that 
c      P(1)/W(1) >= P(2)/W(2) >= ... >= P(N)/W(N),
c    and array K must be sorted so that 
c      K(1) <= K(2) <= ... <= K(M).
c
c  Modified:
c
c    23 October 2013
c
c  Author:
c
c    Silvano Martello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    MKP: 0-1 multiple knapsack problem,
c    ACM Transactions on Mathematical Software,
c    Volume 11, Number 2, June 1985, pages 135-140.
c
c  Parameters:
c
c    Input, integer N, the number of items.
c
c    Input, integer M, the number of knapsacks.
c
c    Input, integer P(N), the profit of each item.
c    0 < P(*).  
c
c    Input, integer W(N), the weight of each item.
c    0 < W(*).
c
c    Input, integer K(M), the capacity of each knapsack.
c    0 < K(*), and the entries of K must be in ascending order.
c
c    Input/output, integer BCK.
c    On input, BCK is -1 or a positive value.
c    * -1,  if exact solution is required.
c    * BCKMAX, the maximum number of backtrack steps to be performed, if
c      a heuristic solution is required.
c    On output, BCK is the number of backtrack steps actually performed.
c
c    Output, integer XSTAR(N), is 0, or else 1 <= I <= M.
c    0, item J is not to be used.
c    I, item J is to be used in knapsack I.
c
c    Output, integer VSTAR, a positive value, or a negative error flag.
c    * Positive, the value of the optimal solution.
c    * -1  if  n .lt. 2  or  m .lt. 1 .
c    * -2  if some  p(j) ,  w(j)  or  k(i) are not positive.
c    * -3  if a knapsack cannot contain any item.
c    * -4  if an item cannot fit into any knapsack.
c    * -5  if knapsack  m  contains all the items.
c    * -6  if arrays  p  and  w  are not correctly sorted.
c    * -7  if array  k is not correctly sorted.
c
c  Local parameters:
c
c    Local, integer B(N):
c    * 1, if item J is not inserted in any knapsack.
c    * 0, if item J is inserted in a knapsack.
c
c    Local, integer BB(M,N).  BB(I,J) is a pointer to the item inserted in
c    knapsack I just before item J.  It is -1 if J is the first item inserted
     in knapsack I.
c
c    Local, integer F(M), pointer to the last item inserted in knapsack I.
c    * -1, if knapsack I is empty.
c
c    Local, integer I, knapsack currently considered.
c
c    Local, integer LB, the lower bound on the optimal solution.
c
c    Local, integer PBL(M), the number of items which can be inserted in
c    each knapsack.
c
c    Local, integer Q(M), the current available capacity of each knapsack.
c
c    Local, integer UB, the upper bound on the optimal solution.
c
c    Local, integer VB, the value of the current solution.
c
c    Local, integer X(M,N), assignment of items to knapsacks. 
c    * 1, item J is in knapsack I in the current solution.
c    * 0, otherwise.
c
c    Local, integer BL(M,N+1).  BL(I,J) is a pointer to the J-th item which can
c    be inserted in knapsack I.
c
c    Local, integer XL(M,N)
c    * 1, if item J was inserted in knapsack I in the last execution of
       subroutine pi.
c    * 0, otherwise.
c
      implicit none

      integer m
      integer n

      integer ap
      integer aw
      integer b(n+1)
      integer bb(m,n)
      integer bck
      integer bl(m,n+1)
      integer bs(n)
      integer f(m)
      integer i
      integer i1
      integer ibv
      integer iflag
      integer ii
      integer ip
      integer ip1
      integer isumw
      integer k1
      integer kub
      integer iuv
      integer j
      integer jbck
      integer jj
      integer k(m)
      integer lb
      integer li
      integer lj
      integer lr
      integer lri
      integer lub
      integer lubi
      integer lx(n)
      integer lxi(n)
      integer m1
      integer maxw
      integer minw
      integer n1
      integer p(n)
      integer pbl(m)
      integer ps(n+1)
      integer q(m)
      integer r
      integer rr
      integer s
      integer u
      integer ub
      integer ubb(n)
      integer v(m)
      integer vb
      integer vstar
      integer w(n)
      integer ws(n+1)
      integer x(m,n)
      integer xl(m,n)
      integer xs(n)
      integer xstar(n)
c
c  Step 0 (check on the input data)
c
      vstar = 0

      if ( n .le. 1 ) then
        vstar = - 1
        return
      end if

      if ( m .le. 0 ) then
        vstar = - 1
        return
      end if

      maxw = w(1)
      minw = w(1)
      isumw = w(1)
      ap = p(1)
      aw = w(1)
      rr = ap / aw

      if ( p(1) .le. 0 ) then
        vstar = - 2
      end if

      if ( w(1) .le. 0 ) then
        vstar = - 2
      end if

      do j = 2, n

        if ( p(j) .le. 0 ) then
          vstar = - 2
        end if

        if ( w(j) .le. 0 ) then
          vstar = - 2
        end if

        r = rr

        if ( w(j) .gt. maxw ) then
          maxw = w(j)
        end if

        if ( w(j) .lt. minw ) then
          minw = w(j)
        end if

        isumw = isumw + w(j)
        ap = p(j)
        aw = w(j)
        rr = ap/aw

        if ( rr .gt. r ) then
          vstar = - 6
          return
        end if

      end do

      if ( k(1) .le. 0 ) then
        vstar = - 2
      end if

      if ( m .eq. 1 ) then
        go to 60
      end if

      do i = 2, m
        if ( k(i) .le. 0 ) then
          vstar = - 2
        end if
        if ( k(i) .lt. k(i-1) ) then
          vstar = - 7
          return
        end if
      end do

      if ( minw .gt. k(1) ) then
        vstar = - 3
      end if

      if ( maxw .gt. k(m) ) then
        vstar = - 4
      end if

      if ( isumw .le. k(m) ) then
        vstar = - 5
      end if

      if ( vstar .lt. 0 ) then
        return
      end if
c
c  Step 1: initialization.
c
      jbck = bck
      bck = 0
      kub = 0
      n1 = n + 1
      b(n1) = 1
      m1 = m - 1

      do j = 1, n
        b(j) = 1
        do i = 1, m
          x(i,j) = 0
          bb(i,j) = 0
        end do
      end do

      do i = 1, m1
        q(i) = k(i)
        f(i) = -1
      end do

      q(m) = k(m)
      vstar = 0
      vb = 0
      i = 1
      call sigma ( n, m, p, w, k, 1, b, kub, ub, bs, ps, ws, xs, 
     &  lx, lxi, lr, lri, lubi )

      do j = 1, n
        lxi(j) = lx(j)
      end do

      lri = lr
      lubi = ub
      iflag = 0
c
c  Step 2: heuristic.
c
10    continue

      kub = vstar - vb

      call pi ( n, m, p, w, q, i, b, bb, kub, bl, lb, pbl, v, xl, 
     &  bs, ps, ws, xs )

      if ( lb + vb .le. vstar ) then
        go to 30
      end if

      vstar = lb + vb

      do j = 1, n
        xstar(j) = 0
        do s = 1, i
          if ( x(s,j) .ne. 0 ) then
            xstar(j) = s
            go to 20
          end if
        end do
20      continue
      end do

      ip = pbl(i)

      do j = 1, ip
        jj = bl(i,j)
        if ( xl(i,j) .eq. 1 ) then
          xstar(jj) = i
        end if
      end do

      i1 = i + 1

      do ii = i1, m
        ip = pbl(ii)
        do j = 1, ip
          jj = bl(ii,j)
          if ( xl(ii,j) .eq. 1 ) then
            xstar(jj) = ii
          end if
        end do
      end do

      if ( ub .eq. lb ) then
        go to 50
      end if
c
c  Step 3: updating.
c
30    continue

      if ( v(i) .ne. 0 ) then

        iuv = ub + vb
        u = pbl(i)
        ibv = 0
  
        do s = 1, u

          if ( xl(i,s) .ne. 0 ) then

            j = bl(i,s)
            x(i,j) = 1
            q(i) = q(i) - w(j)
            vb = vb + p(j)
            b(j) = 0
            bb(i,j) = f(i)
            ubb(j) = iuv

            if ( iflag .ne. 1 ) then
              lub = iuv
              lj = j
              li = i
            end if

            f(i) = j
            ibv = ibv + p(j)

            if ( ibv .eq. v(i) ) then
              go to 40
            end if

            call par ( i, i, ub, iflag, vb, lub, lj, li, f, bb, q, b, n, 
     &        lx, lxi, lr, lri, lubi, m )

            if ( iflag .ne. 1 ) then
              kub = vstar - vb
              call sigma ( n, m, p, w, q, i, b, kub, ub, bs, ps, ws, xs, 
     &          lx, lxi, lr, lri, lubi )
              lj = n1
            end if

            iuv = ub + vb
            if ( iuv .le. vstar ) then
              go to 50
            end if

          end if

        end do

      end if

40    continue

      if ( i .ne. m - 1 ) then

        ip1 = i + 1
        call par ( ip1, i, ub, iflag, vb, lub, lj, li, f, bb, q, b, n, 
     &    lx, lxi, lr, lri, lubi, m )

        if ( iflag .ne. 1 ) then
          kub = vstar - vb
          call sigma ( n, m, p, w, q, ip1, b, kub, ub, bs, ps, ws, xs, 
     &      lx, lxi, lr, lri, lubi )
          lj = n1
        end if

        if ( ub + vb .gt. vstar ) then
          i = i + 1
          go to 30
        end if

      end if
c
c  Step 4: backtracking.
c
50    continue

      if ( i .le. 0 ) then
        bck = bck - 1
        return
      end if

      if ( bck .eq. jbck ) then
        return
      end if

      bck = bck + 1

      if ( f(i) .eq. (-1) ) then

        do j = 1, n
          bb(i,j) = 0
        end do
        i = i - 1
        go to 50

      end if

      j = f(i)
      x(i,j) = 0
      b(j) = 1
      vb = vb - p(j)
      q(i) = q(i) + w(j)

      do s = 1, n
        if ( bb(i,s) .eq. j ) then
          bb(i,s) = 0
        end if
      end do

      f(i) = bb(i,j)

      if ( ubb(j) .le. vstar ) then
        go to 50
      end if

      ub = ubb(j) - vb
      iflag = 1
      go to 10
c
c  Particular case ( 0-1 single knapsack problem)
c
60    continue

      if ( maxw .gt. k(1) ) then
        vstar = - 4
      end if

      if ( isumw .le. k(1) ) then
        vstar = - 5
      end if

      if ( vstar .lt. 0 ) then
        return
      end if

      k1 = k(1)
      do j = 1, n
        ps(j) = p(j)
        ws(j) = w(j)
      end do

      call skp ( n, n, k1, 0, vstar, bs, ps, ws, xs )

      do j = 1, n
        xstar(j) = xs(j)
      end do

      bck = 0

      return
      end
      subroutine sigma ( n, m, p, w, q, i, b, kub, ub, bs, ps, ws, xs, 
     &  lx, lxi, lr, lri, lubi )

c*********************************************************************72
c
cc SIGMA computes an upper bound on the best final solution.
c
c  Discussion:
c
c    This routine computes an upper bound UB on the best final solution
c    which can be obtained from the current solution.
c
c  Modified:
c
c    23 October 2013
c
c  Author:
c
c    Silvano Martello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    MKP: 0-1 multiple knapsack problem,
c    ACM Transactions on Mathematical Software,
c    Volume 11, Number 2, June 1985, pages 135-140.
c
c  Parameters:
c
c    Input, integer N, the number of items.
c
c    Input, integer M, the number of knapsacks.
c
c    Input, integer P(N), the profit of each item.
c    0 < P(*).
c
c    Input, integer W(N), the weight of each item.
c    0 < W(*).
c
c    Input, integer Q(M), the current available capacity of each knapsack.
c
c    Input, integer B(N):
c    * 1, if item J is not inserted in any knapsack.
c    * 0, if item J is inserted in a knapsack.
c
      implicit none

      integer m
      integer n

      integer b(n+1)
      integer bs(n)
      integer i
      integer j
      integer jj
      integer kub
      integer lr
      integer lri
      integer lubi
      integer lx(n)
      integer lxi(n)
      integer ns
      integer p(n)
      integer ps(n+1)
      integer q(m)
      integer qs
      integer sb
      integer ub
      integer w(n)
      integer ws(n+1)
      integer xs(n)

      ns = 0

      qs = 0
      do j = i, m
        qs = qs + q(j)
      end do

      sb = 0

      do j = 1, n
        lx(j) = 0
        if ( b(j) .ne. 0 ) then
          ns = ns + 1
          bs(ns) = j
          ps(ns) = p(j)
          ws(ns) = w(j)
          sb = sb + w(j)
        end if
      end do

      if ( sb .le. qs ) then

        lr = qs - sb
        ub = 0

        do j = 1, ns
          ub = ub + ps(j)
          xs(j) = 1
        end do

      else

        call skp ( n, ns, qs, kub, ub, bs, ps, ws, xs )
        lr = qs

      end if

      do j = 1, ns
        jj = bs(j)
        lx(jj) = xs(j)
      end do

      return
      end
      subroutine pi ( n, m, p, w, q, i, b, bb, kub, bl, lb, pbl, v, xl, 
     &  bs, ps, ws, xs )

c*********************************************************************72
c
cc PI computes a feasible solution to the problem.
c
c  Discussion:
c
c    This routine computes a feasible solution to the current problem.
c    The solution is stored in array XL, the corresponding value in LB.
c
c  Modified:
c
c    23 October 2013
c
c  Author:
c
c    Silvano Martello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    MKP: 0-1 multiple knapsack problem,
c    ACM Transactions on Mathematical Software,
c    Volume 11, Number 2, June 1985, pages 135-140.
c
c  Parameters:
c
c    Input, integer N, the number of items.
c
c    Input, integer M, the number of knapsacks.
c
c    Input, integer P(N), the profit of each item.
c    0 < P(*).
c
c    Input, integer W(N), the weight of each item.
c    0 < W(*).
c
c    Input, integer Q(M), the current available capacity of each knapsack.
c
c    Input, integer I, the index of the knapsack that is being filled.
c
c    Input, integer B(N):
c    *1, if item J is not inserted in any knapsack.
c    *0, if item J is inserted in a knapsack.
c
c    Input, integer BB(M,N).  BB(I,J) is a pointer to the item inserted in
c    knapsack I just before item J.  It is -1 if J is the first item inserted
c    in knapsack I.
c
c    Input, integer BL(M,N+1).  BL(I,J) is a pointer to the J-th item which can
c    be inserted in knapsack I.
c
c    Input, integer PBL(M), the number of items which can be inserted in
c    each knapsack.
c
c    Input, integer XL(M,N)
c    * 1, if item J was inserted in knapsack I in the last execution of
c      subroutine pi.
c    * 0, otherwise.
c
      implicit none

      integer m
      integer n

      integer b(n+1)
      integer bb(m,n)
      integer bl(m,n+1)
      integer bs(n)
      integer i
      integer ii
      integer ikub
      integer j
      integer jb
      integer jbs
      integer jj
      integer kub
      integer lb
      integer ns
      integer p(n)
      integer pb
      integer pbl(m)
      integer ps(n+1)
      integer q(m)
      integer qs
      integer sb
      integer u
      integer v(m)
      integer w(n)
      integer ws(n+1)
      integer xl(m,n)
      integer xs(n)
c
c  Step 1
c
      u = 0

      do j = 1, n
        if ( b(j) .ne. 0 ) then
          u = u + 1
          bs(u) = j
        end if
      end do

      do j = i, m
        pbl(j) = 0
        v(j) = 0
      end do

      lb = 0
      ikub = kub

      if ( u .eq. 0 ) then
        return
      end if

      ns = 0
      sb = 0
      do j = 1, u
        jj = bs(j)
        if ( bb(i,jj) .eq. 0 ) then
          if ( w(jj) .le. q(i) ) then
            ns = ns + 1
            sb = sb + w(jj)
            bl(i,ns) = jj
            ps(ns) = p(jj)
            ws(ns) = w(jj)
          end if
        end if
      end do

      ii = i
c
c  Step 2
c
10    continue

      pbl(ii) = ns

      if ( sb .le. q(ii) ) then

        pb = 0
        do j = 1, ns
          pb = pb + ps(j)
          xl(ii,j) = 1
        end do

      else

        qs = q(ii)

        if ( ii .eq. m ) then
          kub = ikub
        else
          kub = 0
        end if

        call skp ( n, ns, qs, kub, pb, bs, ps, ws, xs )

        do j = 1, ns
          xl(ii,j) = xs(j)
        end do

      end if

      lb = lb + pb
      ikub = ikub - pb
      v(ii) = pb
      bl(ii,ns+1) = n + 1
c
c  Step 3
c
      if ( ii .eq. m ) then
        return
      end if

      jb = 1
      jbs = 0

      do j = 1, u

        if ( bs(j) .lt. bl(ii,jb) ) then
          jbs = jbs + 1
          bs(jbs) = bs(j)
        else
          jb = jb + 1
          if ( xl(ii,jb-1) .ne. 1 ) then
            jbs = jbs + 1
            bs(jbs) = bs(j)
          end if
        end if

      end do

      u = jbs
      if ( u .eq. 0 ) then
        return
      end if

      ns = 0
      sb = 0
      ii = ii + 1

      do j = 1, u
        jj = bs(j)
        if( w(jj) .le. q(ii) ) then
          ns = ns + 1
          sb = sb + w(jj)
          bl(ii,ns) = jj
          ps(ns) = p(jj)
          ws(ns) =  w(jj)
        end if
      end do

      go to 10

      end
      subroutine par ( i, ii, ub, iflag, vb, lub, lj, li, f, bb, q,
     &  b, n, lx, lxi, lr, lri, lubi, m )

c*********************************************************************72
c
cc PAR carries out parametric computation of the upper bounds.
c
c  Modified:
c
c    23 October 2013
c
c  Author:
c
c    Silvano Martello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    MKP: 0-1 multiple knapsack problem,
c    ACM Transactions on Mathematical Software,
c    Volume 11, Number 2, June 1985, pages 135-140.
c
c  Parameters:
c
c    Input, integer F(M), pointer to the last item inserted in knapsack I.
c    * -1, if knapsack I is empty.
c
c    Input, integer BB(M,N).  BB(I,J) is a pointer to the item inserted in
c    knapsack I just before item J.  It is -1 if J is the first item inserted
c    in knapsack I.
c
c    Input, integer Q(M), the current available capacity of each knapsack.
c
c    Input, integer B(N):
c    *1, if item J is not inserted in any knapsack.
c    *0, if item J is inserted in a knapsack.
c
c    Input, integer N, the number of items.
c
c    Input, integer M, the number of knapsacks.
c
      implicit none

      integer m
      integer n

      integer b(n+1)
      integer bb(m,n)
      integer f(m)
      integer i
      integer i1
      integer iflag
      integer ii
      integer ip1
      integer iq
      integer j
      integer li
      integer lj
      integer lr
      integer lri
      integer lub
      integer lubi
      integer lx(n)
      integer lxi(n)
      integer q(m)
      integer r
      integer s
      integer ub
      integer vb

      iflag = 0

      if ( b(lj) .eq. 0 ) then

        i1 = i - 1

        if ( i1 .ge. li ) then

          iq = 0
          do r = li, i1
            iq = iq + q(r)
          end do

          if ( iq .gt. lr ) then
            return
          end if

        end if

        r = ii
        s = f(r)

10      continue

        if ( s .eq. (-1) ) then
          r = r - 1
          s = f(r)
          go to 10
        end if

        if ( lx(s) .eq. 0 ) then
          return
        end if

        if ( s .ne. lj ) then
          s = bb(r,s)
          go to 10
        end if

        ub = lub - vb

      else

        i1 = i - 1

        if ( i1 .ge. 1 ) then

          iq = 0
          do r = 1, i1
            iq = iq + q(r)
          end do

          if ( iq .gt. lri ) then
            return
          end if

        end if

        do j = 1, n
          if ( b(j) .ne. 1 ) then
            if ( lxi(j) .eq. 0 ) then
              return
            end if
          end if
        end do

        ub = lubi - vb

      end if

      iflag = 1

      return
      end
      subroutine skp ( n, ns, qs, kub, vs, bs, ps, ws, xs )

c*********************************************************************72
c
cc SKP solves the 0-1 single knapsack problem.
c
c  Discussion:
c
c    Maximize
c
c      VS = PS(1)*XS(1) + ... + PS(NS)*XS(NS)
c
c    subject to
c
c      WS(1)*XS(1) + ... + WS(NS)*XS(NS) <= QS
c      XS(J) = 0 or 1   for  J=1,...,NS
c      KUB < VS
c
c    This routine is a modified version of subroutine KP01
c    which appeared in Computing, Volume 21, pages 81-86, 1978.
c
c  Modified:
c
c    23 October 2013
c
c  Author:
c
c    Silvano Martello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    MKP: 0-1 multiple knapsack problem,
c    ACM Transactions on Mathematical Software,
c    Volume 11, Number 2, June 1985, pages 135-140.
c
c  Parameters:
c
c    Input, integer N, the number of items for the multiple knapsack
c    problem.
c
c    Input, integer NS, the number of items for the single knapsack problem.
c
c    Input, integer QS, the current capacity of the knapsack.
c
c    Input, integer KUB, a lower bound on the total profit to be made.
c
c    Output, integer VS, the profit made for the computed solution
c    to the given single knapsack problem.
c
      implicit none

      integer n

      integer a
      integer b
      integer bs(n)
      integer c
      integer d(n)
      integer diff
      integer ii
      integer ii1
      integer in
      integer ip
      integer j
      integer j1
      integer jj
      integer kk
      integer kub
      integer l
      integer lim
      integer lim1
      integer ll
      integer lold
      integer mfirst
      integer min(n)
      integer mink
      integer ms
      integer n1
      integer nn
      integer ns
      integer pbar(n)
      integer pr
      integer ps(n+1)
      integer qs
      integer r
      integer t
      integer vs
      integer wbar(n)
      integer ws(n+1)
      integer xs(n)
      integer zbar(n)

      vs = kub
      ip = 0
      ms = qs

      do l = 1, ns
        ll = l
        if ( ws(l) .gt. ms ) then
          go to 10
        end if
        ip = ip + ps(l)
        ms = ms - ws(l)
      end do

10    continue

      ll = ll - 1

      if ( ms .ne. 0 ) then

        ps(ns+1) = 0
        ws(ns+1) = qs + 1
        lim = ip + ms * ps(ll+2) / ws(ll+2)
        a = ip + ps(ll+1)
        b = ( ws(ll+1) - ms ) * ps(ll)
        c = ws(ll)
        lim1 = a - b / c

        if ( lim1 .gt. lim ) then
          lim = lim1
        end if

        if ( lim .le. vs ) then
          return
        end if

        mink = qs + 1
        min(ns) = mink

        do j = 2, ns
          kk = ns + 2 - j
          if ( ws(kk) .lt. mink ) then
            mink = ws(kk)
          end if
          min(kk-1) = mink
        end do

        do j = 1, ns
          d(j) = 0
        end do

        pr = 0
        lold = ns
        ii = 1
        go to 50

      end if

      if ( vs .ge. ip ) then
        return
      end if

      vs = ip
      do j = 1, ll
        xs(j) = 1
      end do

      nn = ll + 1
      do j = nn, ns
        xs(j) = 0
      end do

      qs = 0
      return

20    continue

      if ( ws(ii) .gt. qs ) then
        ii1 = ii + 1
        if ( vs .ge. qs * ps(ii1) / ws(ii1) + pr ) then
          go to 60
        end if
        ii = ii1
        go to 20
      end if

      ip = pbar(ii)
      ms = qs - wbar(ii)
      in = zbar(ii)
      ll = ns

      do l = in, ns
        ll = l
        if ( ws(l) .gt. ms ) then
          go to 40
        end if
        ip = ip + ps(l)
        ms = ms - ws(l)
      end do

30    continue

      if ( vs .ge. ip + pr ) then
        go to 60
      end if

      vs = ip + pr
      mfirst = ms
      nn = ii - 1
      do j = 1, nn
        xs(j) = d(j)
      end do

      do j = ii, ll
        xs(j) = 1
      end do

      nn = ll + 1
      do j = nn, ns
        xs(j) = 0
      end do

      if ( vs .ne. lim ) then
        go to 60
      end if

      qs = mfirst
      return

40    continue

      ll = ll - 1

      if ( ms .eq. 0 ) then
        go to 30
      end if

      if ( vs .ge. pr + ip + ms * ps(l) / ws(l) ) then
        go to 60
      end if

50    continue

      wbar(ii) = qs - ms
      pbar(ii) = ip
      zbar(ii) = ll + 1
      d(ii) = 1
      nn = ll - 1

      do j = ii, nn
        wbar(j+1) = wbar(j) - ws(j)
        pbar(j+1) = pbar(j) - ps(j)
        zbar(j+1) = ll + 1
        d(j+1) = 1
      end do

      j1 = ll + 1
      do j = j1, lold
        wbar(j) = 0
        pbar(j) = 0
        zbar(j) = j
      end do

      lold = ll
      qs = ms
      pr = pr + ip

      if ( ll - ( ns - 2 ) .gt. 0 ) then

        ii = ns

      else if ( ll - ( ns - 2 ) .eq. 0 ) then

        if ( qs .ge. ws(ns) ) then
          qs = qs - ws(ns)
          pr = pr + ps(ns)
          d(ns) = 1
        end if

        ii = ns - 1

      else

        ii = ll + 2
        if ( qs .ge. min(ii-1) ) then
          go to 20
        end if

      end if

      if ( vs .lt. pr ) then

        vs = pr
        do j = 1, ns
          xs(j) = d(j)
        end do

        mfirst = qs

        if ( vs .eq. lim ) then
          return
        end if

      end if

      if ( d(ns) .ne. 0 ) then
        d(ns) = 0
        qs = qs + ws(ns)
        pr = pr - ps(ns)
      end if

60    continue

      nn = ii - 1

      do j = 1, nn
        kk = ii - j
        if ( d(kk) .eq. 1 ) then
          go to 70
        end if
      end do

      qs = mfirst
      return

70    continue

      r = qs
      qs = qs + ws(kk)
      pr = pr - ps(kk)
      d(kk) = 0

      if ( r .ge. min(kk) ) then
        ii = kk + 1
        go to 20
      end if

      nn = kk + 1
      ii = kk

80    continue

      if ( vs .ge. pr + qs * ps(nn) / ws(nn) ) then
        go to 60
      end if

      diff = ws(nn) - ws(kk)

      if ( diff .eq. 0 ) then

        nn = nn + 1
        go to 80

      else if ( 0 < diff ) then

        if ( diff .gt. r ) then
          nn = nn + 1
          go to 80
        end if

        if ( vs .ge. pr + ps(nn) ) then
          nn = nn + 1
          go to 80
        end if

        vs = pr + ps(nn)
        do j = 1, kk
          xs(j) = d(j)
        end do
        jj = kk + 1
        do j = jj, ns
          xs(j) = 0
        end do
        xs(nn) = 1
        mfirst = qs - ws(nn)

        if ( vs .eq. lim ) then
          qs = mfirst
          return
        end if

        r = r - diff
        kk = nn
        nn = nn + 1
        go to 80

      else

        t = r - diff

        if ( t .lt. min(nn) ) then
          nn = nn + 1
          go to 80
        end if

        n = nn + 1

        if ( vs .ge. pr + ps(nn) + t * ps(n) / ws(n) ) then
          go to 60
        end if

        qs = qs - ws(nn)
        pr = pr + ps(nn)
        d(nn) = 1
        ii = nn + 1
        wbar(nn) = ws(nn)
        pbar(nn) = ps(nn)
        zbar(nn) = ii
        n1 = nn + 1

        do j = n1, lold
          wbar(j) = 0
          pbar(j) = 0
          zbar(j) = j
        end do

        lold = nn
        go to 20

      end if

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
