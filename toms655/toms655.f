      subroutine cawiq ( nt, t, mlt, nwts, ndx, key, nst, aj, bj, jdf, 
     &  zemu, wts )

c*********************************************************************72
c
cc CAWIQ computes quadrature weights for a given set of knots.
c
c  Discussion:
c
c    This routine is given a set of distinct knots, T, their multiplicities MLT,
c    the Jacobi matrix associated with the polynomials orthogonal with respect
c    to the weight function W(X), and the zero-th moment of W(X).
c
c    It computes the weights of the quadrature formula
c
c      sum ( 1 <= J <= NT ) sum ( 0 <= I <= MLT(J) - 1 ) wts(j) d^i/dx^i f(t(j))
c
c    which is to approximate
c
c      integral ( a < x < b ) f(t) w(t) dt
c
c    The routine makes various checks, as indicated below, sets up
c    various vectors and, if necessary, calls for the diagonalization
c    of the Jacobi matrix that is associated with the polynomials
c    orthogonal with respect to W(X) on the interval A, B.
c
c    Then for each knot, the weights of which are required, it calls the
c    routine CWIQD which to compute the weights.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the knots.
c
c    Input, integer MLT(NT), the multiplicity of the knots.
c
c    Input, integer NWTS, the number of weights.
c
c    Input/output, integer NDX(NT), associates with each distinct
c    knot T(J), an integer NDX(J) which is such that the weight to the I-th
c    derivative value of F at the J-th knot, is stored in
c      WTS(abs(NDX(J))+I) for J = 1,2,...,NT, and I = 0,1,2,...,MLT(J)-1.
c    The sign of NDX includes the following information:
c    > 0, weights are wanted for this knot
c    .lt. 0, weights not wanted for this knot but it is included in the quadrature
c    = 0. means ignore this knot completely.
c
c    Input, integer KEY, indicates structure of WTS and NDX.
c    KEY is an integer with absolute value between 1 and 4.
c    The sign of KEY choosed the form of WTS:
c    0 .lt. KEY, WTS in standard form.
c    0 > KEY, J]WTS(J) required.
c    The absolute value has the following effect:
c    1, set up pointers in NDX for all knots in T array (routine CAWIQ does
c    this).  the contents of NDX are not tested on input and weights are
c    packed sequentially in WTS as indicated above.
c    2, set up pointers only for knots which have nonzero NDX on input.  All
c    knots which have a non-zero flag are allocated space in WTS.
c    3, set up pointers only for knots which have NDX > 0 on input.  Space in
c    WTS allocated only for knots with NDX > 0.
c    4, NDX assumed to be preset as pointer array on input.
c
c    Input, integer NST, the dimension of the Jacobi matrix.
c    NST should be between (N+1)/2 and N.  The usual choice will be (N+1)/2.
c
c    Input/output, double precision AJ(NST), BJ(NST).
c    If JDF = 0 then AJ contains the  diagonal of the Jacobi matrix and
c    BJ(1:NST-1) contains the subdiagonal.
c    If JDF = 1, AJ contains the eigenvalues of the Jacobi matrix and
c    BJ contains the squares of the elements of the first row of U, the
c    orthogonal matrix which diagonalized the Jacobi matrix as U*D*U'.
c
c    Input/output, integer JDF, indicates whether the Jacobi
c    matrix needs to be diagonalized.
c    0, diagonalization required;
c    1, diagonalization not required.
c
c    Input, double precision ZEMU, the zero-th moment of the weight
c    function W(X).
c
c    Output, double precision WTS(NWTS), the weights.
c
      implicit none

      integer nst
      integer nt
      integer nwts

      double precision aj(nst)
      double precision bj(nst)
      integer i
      integer ip
      integer j
      integer jdf
      integer jj
      integer jp
      integer k
      integer key
      integer l
      integer m
      integer mlt(nt)
      integer mnm
      integer mtj
      integer n
      integer ndx(nt)
      double precision p
      double precision prec
      double precision r8_epsilon
      double precision t(nt)
      double precision tmp
      double precision wts(nwts)
      double precision z(nst)
      double precision zemu

      prec = r8_epsilon ( )

      if ( nt .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CAWIQ - Fatal error!'
        write ( *, '(a)' ) '  NT .lt. 1.'
        stop 1
      end if
c
c  Check for indistinct knots.
c
      if ( 1 .lt. nt ) then

        k = nt - 1

        do i = 1, k
          tmp = t(i)
          l = i + 1
          do j = l, nt
            if ( abs ( tmp - t(j) ) .le. prec ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'CAWIQ - Fatal error!'
              write ( *, '(a)' ) '  Knots too close.'
              stop 1
            end if
          end do

        end do

      end if
c
c  Check multiplicities,
c  Set up various useful parameters and
c  set up or check pointers to WTS array.
c
      l = abs ( key )

      if ( l .lt. 1 .or. 4 .lt. l ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CAWIQ - Fatal error!'
        write ( *, '(a)' ) '  Magnitude of KEY not between 1 and 4.'
        stop 1
      end if

      k = 1

      if ( l .eq. 1 ) then

        do i = 1, nt
          ndx(i) = k
          if ( mlt(i) .lt. 1 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'CAWIQ - Fatal error!'
            write ( *, '(a)' ) '  MLT(I) .lt. 1.'
            stop 1
          end if
          k = k + mlt(i)
        end do

        n = k - 1

      else if ( l .eq. 2 .or. l .eq. 3 ) then

        n = 0

        do i = 1, nt

          if ( ndx(i) .eq. 0 ) then
            go to 10
          end if

          if ( mlt(i) .lt. 1 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'CAWIQ - Fatal error!'
            write ( *, '(a)' ) '  MLT(I) .lt. 1.'
            stop 1
          end if

          n = n + mlt(i)

          if ( ndx(i) .lt. 0 .and. l .eq. 3 ) then
            go to 10
          end if

          ndx(i) = sign ( k, ndx(i) )
          k = k + mlt(i)

10        continue

        end do

        if ( nwts + 1 .lt. k ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'CAWIQ - Fatal error!'
          write ( *, '(a)' ) '  NWTS + 1 .lt. K.'
          stop 1
        end if

      else if ( l .eq. 4 ) then

        do i = 1, nt

          ip = abs ( ndx(i) )

          if ( ip .eq. 0 ) then
            go to 20
          end if

          if ( nwts .lt. ip + mlt(i) ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'CAWIQ - Fatal error!'
            write ( *, '(a)' ) '  NWTS .lt. IPM.'
            stop 1
          end if

          if ( i .eq. nt ) then
            go to 30
          end if

          l = i + 1
          do j = l, nt
            jp = abs ( ndx(j) )
            if ( jp .ne. 0 ) then
              if ( jp .le. ip + mlt(i) .and. ip .le. jp + mlt(j) ) then
                exit
              end if
            end if
          end do

20        continue

        end do

30      continue

      end if
c
c  Test some parameters.
c
      if ( nst .lt. ( n + 1 ) / 2 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CAWIQ - Fatal error!'
        write ( *, '(a)' ) '  NST .lt. ( N + 1 ) / 2.'
        stop 1
      end if

      if ( zemu .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CAWIQ - Fatal error!'
        write ( *, '(a)' ) '  ZEMU .le. 0.'
        stop 1
      end if
c
c  Treat a quadrature formula with 1 simple knot first.
c
      if ( n .le. 1 ) then

        do i = 1, nt
          if ( 0 .lt. ndx(i) ) then
            wts( abs ( ndx(i) ) ) = zemu
            return
          end if
        end do

      end if
c
c  Carry out diagonalization if not already done.
c
      if ( jdf .eq. 0 ) then
c
c  Set unit vector in work field to get back first row of Q.
c
        do i = 1, nst
          z(i) = 0.0D+00
        end do
        z(1) = 1.0D+00
c
c  Diagonalize the Jacobi matrix.
c
        call imtqlx ( nst, aj, bj, z )
c
c  Signal Jacobi matrix now diagonalized successfully.
c
        jdf = 1
c
c  Save squares of first row of U in subdiagonal array.
c
        do i = 1, nst
          bj(i) = z(i) * z(i)
        end do

      end if
c
c  Find all the weights for each knot flagged.
c
      do i = 1, nt

        if ( ndx(i) .le. 0 ) then
          go to 40
        end if

        m = mlt(i)
        mnm = max ( n - m, 1 )
        l = min ( m, n - m + 1 )

        call cawiq_sub ( l, mnm, nst, nt, nwts, i, m, aj, bj, mlt, 
     &    ndx, t, wts, zemu )

        if ( key .lt. 0 ) then
          go to 40
        end if
c
c  Divide by factorials for weights in standard form.
c
        tmp = 1.0D+00
        do j = 2, m - 1
          p = j
          tmp = tmp * p
          wts(k+j) = wts(k+j) / tmp
        end do

40      continue

      end do

      return
      end
      subroutine cawiq_sub ( l, mnm, nst, nt, nwts, i, m, aj, bj, mlt, 
     &  ndx, t, wts, zemu )

c*********************************************************************72
c
cc CAWIQ_SUB is called by CAWIQ, to create some temporary arrays.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
      implicit none

      integer l
      integer mnm
      integer nst
      integer nt
      integer nwts

      double precision aj(nst)
      double precision bj(nst)
      integer i
      integer j
      integer jj
      integer k
      integer m
      integer mlt(nt)
      integer ndx(nt)
      double precision r(l)
      double precision t(nt)
      double precision wts(nwts)
      double precision xk(mnm)
      double precision zemu
c
c  Set up K-hat matrix for CWIQD with knots according to their multiplicities.
c
      k = 1
      do j = 1, nt
        if ( ndx(j) .ne. 0 ) then
          if ( j .ne. i ) then
            do jj = 1, mlt(j)
              xk(k) = t(j)
              k = k + 1
            end do
          end if
        end if
      end do
c
c  Set up the right principal vector.
c
      r(1) = 1.0D+00 / zemu
      do j = 2, l
        r(j) = 0.0D+00
      end do
c
c  Pick up pointer for the location of the weights to be output.
c
      k = ndx(i)
c
c  Find all the weights for this knot.
c
      call cwiqd ( m, mnm, l, t(i), xk, nst, aj, bj, r, wts(k) )

      return
      end
      subroutine cdgqf ( nt, kind, alpha, beta, t, wts )

c*********************************************************************72
c
cc CDGQF computes a Gauss quadrature formula with default A, B and simple knots.
c
c  Discussion:
c
c    This routine computes all the knots and weights of a Gauss quadrature
c    formula with a classical weight function with default values for A and B,
c    and only simple knots.
c
c    There are no moments checks and no printing is done.
c
c    Use routine EIQFS to evaluate a quadrature computed by CGQFS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Output, double precision T(NT), the knots.
c
c    Output, double precision WTS(NT), the weights.
c
      implicit none

      integer nt

      double precision aj(nt)
      double precision alpha
      double precision beta
      double precision bj(nt)
      integer kind
      double precision t(nt)
      double precision wts(nt)
      double precision zemu

      call parchk ( kind, 2 * nt, alpha, beta )
c
c  Get the Jacobi matrix and zero-th moment.
c
      call class_matrix ( kind, nt, alpha, beta, aj, bj, zemu )
c
c  Compute the knots and weights.
c
      call sgqf ( nt, aj, bj, zemu, t, wts )

      return
      end
      subroutine cegqf ( nt, kind, alpha, beta, a, b, f, qfsum )

c*********************************************************************72
c
cc CEGQF computes a quadrature formula and applies it to a function.
c
c  Discussion:
c
c    The user chooses the quadrature formula to be used, as well as the
c    interval (A,B) in which it is applied.
c
c    Note that the knots and weights of the quadrature formula are not
c    returned to the user.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, double precision A, B, the interval endpoints.
c
c    Input, double precision, external F, the name of a routine which
c    evaluates the function and some of its derivatives.  The routine
c    must have the form
c      function f ( x, i )
c      double precision f
c      integer i
c      double precision x
c    and return in F the value of the I-th derivative of the function
c    at X.  The value I will always be 0.  The value X will always be a knot.
c
c    Output, double precision QFSUM, the value of the quadrature formula
c    applied to F.
c
      implicit none

      integer nt

      double precision a
      double precision alpha
      double precision b
      double precision beta
      double precision f
      external f
      integer kind
      integer lo
      double precision qfsum
      double precision t(nt)
      double precision wts(nt)

      lo = 0
      call cgqf ( nt, kind, alpha, beta, a, b, lo, t, wts )
c
c  Evaluate the quadrature sum.
c
      call eiqfs ( nt, t, wts, f, qfsum )

      return
      end
      subroutine cegqfs ( nt, kind, alpha, beta, f, qfsum )

c*********************************************************************72
c
cc CEGQFS estimates an integral using a standard quadrature formula.
c
c  Discussion:
c
c    The user chooses one of the standard quadrature rules
c    with the default values of A and B.  This routine determines
c    the corresponding weights and evaluates the quadrature formula
c    on a given function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, double precision, external F, the name of a routine which
c    evaluates the function and some of its derivatives.  The routine
c    must have the form
c      function f ( x, i )
c      double precision f
c      integer i
c      double precision x
c    and return in F the value of the I-th derivative of the function
c    at X.  The value  I will always be 0.  The value X will always be a knot.
c
c    Output, double precision QFSUM, the value of the quadrature formula
c    applied to F.
c
      implicit none

      integer nt

      double precision alpha
      double precision beta
      double precision f
      external f
      integer kind
      integer lu
      double precision qfsum
      double precision t(nt)
      double precision wts(nt)
c
c  Assign workspace for knots and weights.
c
      lu = 0

      call cgqfs ( nt, kind, alpha, beta, lu, t, wts )
c
c  Evaluate the quadrature sum.
c
      call eiqfs ( nt, t, wts, f, qfsum )

      return
      end
      subroutine ceiqf ( nt, t, mlt, kind, alpha, beta, a, b, f, qfsum )

c*********************************************************************72
c
cc CEIQF constructs and applies a quadrature formula based on user knots.
c
c  Discussion:
c
c    The knots may have multiplicity.  The quadrature interval is over
c    any valid A, B.  A classical weight function is selected by the user.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the knots.
c
c    Input, integer MLT(NT), the multiplicity of the knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, double precision A, B, the interval endpoints.
c
c    Input, double precision, external F, the name of a routine which
c    evaluates the function and some of its derivatives.  The routine
c    must have the form
c      function f ( x, i )
c      double precision f
c      integer i
c      double precision x
c    and return in F the value of the I-th derivative of the function
c    at X.  The highest value of I will be the maximum value in MLT minus
c    one.  The value X will always be a knot.
c
c    Output, double precision QFSUM, the value of the quadrature formula
c    applied to F.
c
      implicit none

      integer nt

      double precision a
      double precision alpha
      double precision b
      double precision beta
      double precision f
      external f
      integer i4vec_sum
      integer kind
      integer m
      integer mlt(nt)
      integer nwts
      double precision qfsum
      double precision t(nt)

      nwts = i4vec_sum ( nt, mlt )

      call ceiqf_sub ( nt, t, mlt, kind, alpha, beta, a, b, f, 
     &  qfsum, nwts )

      return
      end
      subroutine ceiqf_sub ( nt, t, mlt, kind, alpha, beta, a, b, f, 
     &  qfsum, nwts )

c*********************************************************************72
c
cc CEIQF_SUB sets up some temporary arrays for CEIQF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the knots.
c
c    Input, integer MLT(NT), the multiplicity of the knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, double precision A, B, the interval endpoints.
c
c    Input, double precision, external F, the name of a routine which
c    evaluates the function and some of its derivatives.  The routine
c    must have the form
c      function f ( x, i )
c      double precision f
c      integer i
c      double precision x
c    and return in F the value of the I-th derivative of the function
c    at X.  The highest value of I will be the maximum value in MLT minus
c    one.  The value X will always be a knot.
c
c    Output, double precision QFSUM, the value of the quadrature formula
c    applied to F.
c
c    Input, integer NWTS, the sum of the MLT entries.
c
      implicit none

      integer nt
      integer nwts

      double precision a
      double precision alpha
      double precision b
      double precision beta
      double precision f
      external f
      integer key
      integer kind
      integer lu
      integer m
      integer mlt(nt)
      integer ndx(nt)
      double precision qfsum
      double precision t(nt)
      double precision wts(nwts)

      lu = 0
      key = 1

      call ciqf ( nt, t, mlt, nwts, ndx, key, kind, alpha, beta, a, b, 
     &  lu, wts )

      call eiqf ( nt, t, mlt, wts, nwts, ndx, key, f, qfsum )

      return
      end
      subroutine ceiqfs ( nt, t, mlt, kind, alpha, beta, f, qfsum )

c*********************************************************************72
c
cc CEIQFS computes and applies a quadrature formula based on user knots.
c
c  Discussion:
c
c    The knots may have multiplicity.  The quadrature interval is over
c    the standard interval A, B for the classical weight function selected
c    by the user.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the knots.
c
c    Input, integer MLT(NT), the multiplicity of the knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, double precision, external F, the name of a routine which
c    evaluates the function and some of its derivatives.  The routine
c    must have the form
c      function f ( x, i )
c      double precision f
c      integer i
c      double precision x
c    and return in F the value of the I-th derivative of the function
c    at X.  The highest value of I will be the maximum value in MLT minus
c    one.  The value X will always be a knot.
c
c    Output, double precision QFSUM, the value of the quadrature formula
c    applied to F.
c
      implicit none

      integer nt

      double precision alpha
      double precision beta
      double precision f
      external f
      integer i4vec_sum
      integer kind
      integer mlt(nt)
      integer n
      double precision qfsum
      double precision t(nt)

      n = i4vec_sum ( nt, mlt )

      call ceiqfs_sub ( nt, t, mlt, kind, alpha, beta, f, 
     &  qfsum, n )

      return
      end
      subroutine ceiqfs_sub ( nt, t, mlt, kind, alpha, beta, f, 
     &  qfsum, n )

c*********************************************************************72
c
cc CEIQFS_SUB sets up some arrays for CEIQFS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
      implicit none

      integer n
      integer nt

      double precision alpha
      double precision beta
      double precision f
      external f
      integer key
      integer kind
      integer lu
      integer mlt(nt)
      integer ndx(nt)
      double precision qfsum
      double precision t(nt)
      double precision wts(n)

      lu = 0
      key = 1
 
      call ciqfs ( nt, t, mlt, n, ndx, key, kind, alpha, beta, lu, wts )

      call eiqf ( nt, t, mlt, wts, n, ndx, key, f, qfsum )

      return
      end
      subroutine cgqf ( nt, kind, alpha, beta, a, b, lo, t, wts )

c*********************************************************************72
c
cc CGQF computes knots and weights of a Gauss quadrature formula.
c
c  Discussion:
c
c    The user may specify the interval (A,B).
c
c    Only simple knots are produced.
c
c    Use routine EIQFS to evaluate this quadrature formula.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, double precision A, B, the interval endpoints, or
c    other parameters.
c
c    Input, integer LO, defines the actions:
c    < 0, compute knots and weights, and print.
c    = 0, compute knots and weights.
c    > 0, compute knots and weights, print, and do moment check.
c
c    Output, double precision T(NT), the knots.
c
c    Output, double precision WTS(NT), the weights.
c
      implicit none

      integer nt

      double precision a
      double precision alpha
      double precision b
      double precision beta
      integer i
      integer key
      integer kind
      integer lo
      integer m
      integer mex
      integer mlt(nt)
      integer mmex
      integer mop
      integer ndx(nt)
      double precision t(nt)
      double precision wts(nt)

      key = 1
      mop = 2 * nt
      m = mop + 1
      mex = m + 2
      mmex = max ( mex, 1 )

      if ( lo .le. 0 ) then
        mex = 0
      end if
c
c  Compute the Gauss quadrature formula for default values of A and B.
c
      call cdgqf ( nt, kind, alpha, beta, t, wts )
c
c  Prepare to scale the quadrature formula to other weight function with
c  valid A and B.
c
      do i = 1, nt
        mlt(i) = 1
      end do

      do i = 1, nt
        ndx(i) = i
      end do

      call scqf ( nt, t, mlt, wts, nt, ndx, wts, t, kind, alpha, beta, 
     &  a, b )
c
c  Print if requested.
c
      if ( lo .ne. 0 ) then
        call chkqf ( t, wts, mlt, nt, nt, ndx, key, mop, mmex, 
     &    kind, alpha, beta, lo, a, b )
      end if

      return
      end
      subroutine cgqfs ( nt, kind, alpha, beta, lo, t, wts )

c*********************************************************************72
c
cc CGQFS computes knots and weights of a Gauss quadrature formula.
c
c  Discussion:
c
c    This routine computes the knots and weights of a Gauss quadrature
c    formula with:
c
c    * a classical weight function with default values for A and B;
c    * only simple knots
c    * optionally print knots and weights and a check of the moments
c
c    Use routine EIQFS to evaluate a quadrature formula computed by
c    this routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, integer LO, selects the action.
c    > 0, compute and print knots and weights.  Print moments check.
c    = 0, compute knots and weights.
c    .lt. 0, compute and print knots and weights.
c
c    Output, double precision T(NT), the knots.
c
c    Output, double precision WTS(NT), the weights.
c
      implicit none

      integer nt

      double precision alpha
      double precision beta
      integer i
      integer key
      integer kind
      integer lo
      integer m
      integer mlt(nt)
      integer mmex
      integer mop
      integer ndx(nt)
      double precision t(nt)
      double precision w(2*nt+3)
      double precision wts(nt)
c
c  Check there is enough workfield and assign workfield
c
      key = 1
      mop = 2 * nt
      m = mop + 1
      mmex = max ( m + 2, 1 )
c
c  Compute the Gauss quadrature formula for default values of A and B.
c
      call cdgqf ( nt, kind, alpha, beta, t, wts )
c
c  Exit if no print required.
c
      if ( lo .ne. 0 ) then

        do i = 1, nt
          mlt(i) = 1
        end do

        do i = 1, nt
          ndx(i) = i
        end do

        call chkqfs ( t, wts, mlt, nt, nt, ndx, key, w, mop, mmex, kind,
     &    alpha, beta, lo )

      end if

      return
      end
      subroutine chkqf ( t, wts, mlt, nt, nwts, ndx, key, mop, mex, 
     &  kind, alpha, beta, lo, a, b )

c*********************************************************************72
c
cc CHKQF computes and prints the moments of a quadrature formula.
c
c  Discussion:
c
c    The quadrature formula is based on a clasical weight function with
c    any valid A, B.
c
c    No check can be made for non-classical weight functions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, double precision T(NT), the knots.
c
c    Input, double precision WTS(NWTS), the weights.
c
c    Input, integer MLT(NT), the multiplicity of the knots.
c
c    Input, integer NT, the number of knots.
c
c    Input, integer NWTS, the number of weights.
c
c    Input, integer NDX(NT), used to index the array WTS.
c    If KEY = 1, then NDX need not be preset.  For more details see the
c    comments in CAWIQ.
c
c    Input, integer KEY, indicates the structure of the WTS
c    array.  It will normally be set to 1.  This will cause the weights to be
c    packed sequentially in array WTS.  For more details see the comments
c    in CAWIQ.
c
c    Input, integer MOP, the expected order of precision of the
c    quadrature formula.
c
c    Input, integer MEX, the number of moments required to be
c    tested.  Set MEX = 1 and LO .lt. 0 for no moments check.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, integer LO, selects the action to carry out.
c     > 0, print weights and moment tests.
c     = 0, print nothing. compute moment test.
c     .lt. 0, print weights only. don't compute moment tests.
c
c    Input, double precision A, B, the interval endpoints.
c
      implicit none

      integer mex
      integer nt
      integer nwts

      double precision a
      double precision alpha
      double precision b
      double precision beta
      integer i
      integer izero
      integer key
      integer kind
      integer lo
      integer mop
      integer neg
      integer mlt(nt)
      integer ndx(nt)
      double precision t(nt)
      double precision t2(nt)
      double precision tmp
      double precision w(mex)
      double precision wts(nwts)

      call parchk ( kind, mex, alpha, beta )

      if ( lo .ne. 0 ) then

        izero = 0

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Interpolatory quadrature formula'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Type  Interval       Weight function             Name'
        write ( *, '(a)' ) ' '
        if ( kind .eq. 1 ) then
          write ( *, '(a)' )
     &      '    1  (a,b)              1.0                  ' 
     &      // 'Legendre'
        else if ( kind .eq. 2 ) then
          write ( *, '(a)' )
     &      '    2  (a,b)      ((b-x)*(x-a))^(-0.5)          ' 
     &      // 'Chebyshev Type 1'
        else if ( kind .eq. 3 ) then
          write ( *, '(a)' )
     &      '    3  (a,b)      ((b-x)*(x-a))^alpha           ' 
     &      // 'Gegenbauer'
        else if ( kind .eq. 4 ) then
          write ( *, '(a)' )
     &      '    4  (a,b)    (b-x)^alpha*(x-a)^beta          ' 
     &      // 'Jacobi'
        else if ( kind .eq. 5 ) then
          write ( *, '(a)' )
     &      '    5  (a,+oo)  (x-a)^alpha*exp(-b*(x-a))      ' 
     &      // 'Gen Laguerre'
        else if ( kind .eq. 6 ) then
          write ( *, '(a)' )
     &      '    6  (-oo,+oo) |x-a|^alpha*exp(-b*(x-a)^2)    ' 
     &      // 'Gen Hermite'
        else if ( kind .eq. 7 ) then
          write ( *, '(a)' )
     &      '    7  (a,b)      |x-(a+b)/2.0|^alpha        ' 
     &      // 'Exponential'
        else if ( kind .eq. 8 ) then
          write ( *, '(a)' )
     &      '    8  (a,+oo)   (x-a)^alpha*(x+b)^beta         ' 
     &      // 'Rational'
        else if ( kind .eq. 9 ) then
          write ( *, '(a)' )
     &      '    9  (a,b)      ((b-x)*(x-a))^(+0.5)          ' 
     &      // 'Chebyshev Type 2'
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,f12.5)' ) '     Parameters   A          ', a
        write ( *, '(a,f12.5)' ) '                  B          ', b
        if ( 3 .le. kind .and. kind .le. 8 ) then
          write ( *, '(a,f12.5)' ) 
     &      '                  alpha      ', alpha
        end if

        if ( kind .eq. 4 .or. kind .eq. 8 ) then
          write ( *, '(a,f12.5)' ) '                  beta       ', beta
        end if

        call chkqfs ( t, wts, mlt, nt, nwts, ndx, key, w, mop, mex, 
     &    izero, alpha, beta, - abs ( lo ) )

      end if

      if ( 0 .le. lo ) then
c
c  Compute the moments in W.
c
        call scmm ( mex, kind, alpha, beta, a, b, w )

        if ( kind .eq. 1 .or. kind .eq. 2 .or. kind .eq. 3 .or. 
     &    kind .eq. 4 .or. kind .eq. 7 .or. kind .eq. 9 ) then
          tmp = ( b + a ) / 2.0D+00
        else if ( kind .eq. 5 .or. kind .eq. 6 .or. kind .eq. 8 ) then
          tmp = a
        end if

        do i = 1, nt
          t2(i) = t(i) - tmp
        end do

        neg = -1
c
c  Check moments.
c
        call chkqfs ( t2, wts, mlt, nt, nwts, ndx, key, w, mop, mex, 
     &    neg, alpha, beta, lo  )

      end if

      return
      end
      subroutine chkqfs ( t, wts, mlt, nt, nwts, ndx, key, w, mop, mex, 
     &  kind, alpha, beta, lo )

c*********************************************************************72
c
cc CHKQFS checks the polynomial accuracy of a quadrature formula.
c
c  Discussion:
c
c    This routine will optionally print weights, and results of a moments test.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, double precision T(NT), the knots.
c
c    Input, double precision WTS(NWTS), the weights.
c
c    Input, integer MLT(NT), the multiplicity of the knots.
c
c    Input, integer NT, the number of knots.
c
c    Input, integer NWTS, the number of weights.
c
c    Input, integer NDX(NT), used to index the array WTS.
c    If KEY = 1, then NDX need not be preset.  For more details see the
c    comments in CAWIQ.
c
c    Input, integer KEY, indicates the structure of the WTS
c    array.  It will normally be set to 1.  This will cause the weights to be
c    packed sequentially in array WTS.  For more details see the comments
c    in CAWIQ.
c
c    Input/output, double precision W(MEX), the moments array.
c    This is input only if KIND = 0.
c
c    Input, integer MOP, the expected order of precision of the
c    quadrature formula.
c
c    Input, integer MEX, the number of moments to be tested.
c    MEX must be at least 1.  Set MEX = 1 and LO .lt. 0 for no moment check.
c
c    Input, integer KIND, the rule.
c    0, unknown weight function (the user must set the first MEX moments in
c       array W in this case.)
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, integer LO, selects the action to carry out.
c     > 0, print weights and moment tests.
c     = 0, print nothing. compute moment test.
c     .lt. 0, print weights only. don't compute moment tests.
c
c  Local Parameters:
c
c    Local, double precision E(MEX), ER(MEX), the absolute and relative
c    errors of the quadrature formula applied to (X-DEL)^n.
c
c    Local, double precision QM(MEX), the values of the quadrature formula
c    applied to (X-DEL)^N.
c
      implicit none

      integer mex
      integer nt
      integer nwts

      double precision alpha
      double precision beta
      double precision e(mex)
      double precision ek
      double precision emn
      double precision emx
      double precision erest
      double precision ern
      double precision erx
      double precision er(mex)
      integer i
      integer j
      integer jl
      integer k
      integer key
      integer kind
      integer kindp
      integer kjl
      integer l
      integer lo
      integer m
      integer mlt(nt)
      integer mop
      integer mx
      integer ndx(nt)
      double precision prec
      double precision px
      double precision qm(mex)
      double precision r8_epsilon
      double precision r8vec_amax
      double precision r8vec_amin
      double precision t(nt)
      double precision tmp
      double precision tmpx
      double precision w(mex)
      double precision wts(nwts)
c
c  KIND may be set to -1 to allow printing of moments only.
c
c  This feature is only used internally, by CHKQF.
c
      kindp = max ( 0, kind )

      if ( lo .ne. 0 .and. kind .ne. -1 ) then

        if ( kindp .ne. 0 ) then

          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  Interpolatory quadrature formula'
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 
     &      '  Type  Interval       Weight function               Name'
          write ( *, '(a)' ) ' '
          if ( kindp .eq. 1 ) then
            write ( *, '(a)' ) 
     &        '    1  (-1,+1)            1.0                    '
     &        // 'Legendre'
          else if ( kindp .eq. 2 ) then
            write ( *, '(a)' ) 
     &        '    2  (-1,+1)    ((b-x)*(x-a))^(-0.5)          ' 
     &        // 'Chebyshev Type 1'
          else if ( kindp .eq. 3 ) then
            write ( *, '(a)' ) 
     &        '    3  (-1,+1)    ((b-x)*(x-a))^alpha           ' 
     &        // 'Gegenbauer'
          else if ( kindp .eq. 4 ) then
            write ( *, '(a)' ) 
     &        '    4  (-1,+1)  (b-x)^alpha*(x-a)^beta          ' 
     &        // 'Jacobi'
          else if ( kindp .eq. 5 ) then
            write ( *, '(a)' ) 
     &        '    5  (a,+oo)   (x-a)^alpha*exp(-b*(x-a))     ' 
     &        // 'Gen Laguerre'
          else if ( kindp .eq. 6 ) then
            write ( *, '(a)' ) 
     &        '    6  (-oo,+oo) |x-a|^alpha*exp(-b*(x-a)^2)  ' 
     &        // 'Gen Hermite'
          else if ( kindp .eq. 7 ) then
            write ( *, '(a)' ) 
     &        '    7  (-1,+1)    |x-(a+b)/2.0|^alpha        ' 
     &        // 'Exponential'
          else if ( kindp .eq. 8 ) then
            write ( *, '(a)' ) 
     &        '    8  (0,+oo)    (x-a)^alpha*(x+b)^beta         ' 
     &        // 'Rational'
          else if ( kindp .eq. 9 ) then
            write ( *, '(a)' ) 
     &        '    9  (-1,+1)    ((b-x)*(x-a))^(+0.5)          ' 
     &        // 'Chebyshev Type 2'
          end if

          if ( 3 .le. kindp .and. kindp .le. 8 ) then
            write ( *, '(a,f12.5)' ) 
     &        '                  alpha      ', alpha
          end if

          if ( kindp .eq. 4 .or. kindp .eq. 8 ) then
            write ( *, '(a,f12.5)' ) 
     &        '                  beta       ', beta
          end if

        end if

        if ( kind .ne. -1 ) then
          prec = r8_epsilon ( )
          write ( *, '(a)' ) ' '
          write ( *, '(a,d13.1)' ) '  Machine precision = ', prec
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a)' )
     &    '           Knots               Mult               Weights'
        write ( *, '(a)' ) ' '

        do i = 1, nt
          k = abs ( ndx(i) )
          if ( k .ne. 0 ) then
            write ( *, '(i4,d26.17,i4,d26.17)' ) i, t(i), mlt(i), wts(k)
            do j = k + 1, k + mlt(i) - 1
              write ( *, '(34x,d26.17)' ) wts(j)
            end do
          end if
        end do

      end if

      if ( 0 .le. lo ) then
c
c  Compute the moments in W.
c
        if ( kindp .ne. 0 ) then

          call wm ( mex, kindp, alpha, beta, w )

        end if

        do i = 1, mex
          qm(i) = 0.0D+00
        end do

        erest = 0.0D+00

        do k = 1, nt

          tmp = 1.0D+00
          l = abs ( ndx(k) )

          if ( l .eq. 0 ) then
            go to 10
          end if

          erest = erest + abs ( wts(l) )
          do j = 1, mex
            qm(j) = qm(j) + tmp * wts(l)
            tmpx = tmp
            px = 1.0D+00
            do jl = 2, min ( mlt(k), mex - j + 1 )
              kjl = j + jl - 1
              tmpx = tmpx * ( kjl - 1 )
              qm(kjl) = qm(kjl) + tmpx * wts(l+jl-1) / px
              if ( key .le. 0 ) then
                px = px * jl
              end if
            end do
            tmp = tmp * t(k)
          end do

10        continue

        end do

        do i = 1, mex
          e(i) = w(i) - qm(i)
          er(i) = e(i) / ( abs ( w(i) ) + 1.0D+00 )
        end do

        erest = erest / ( abs ( w(1) ) + 1.0D+00 )

      end if

      if ( 0 .lt. lo ) then

        m = mop + 1
        mx = min ( mop, mex )

        emx = r8vec_amax ( mx, e )
        emn = r8vec_amin ( mx, e )
        erx = r8vec_amax ( mx, er )
        ern = r8vec_amin ( mx, e )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Comparison of moments'
        write ( *, '(a)' ) ' '
        write ( *, '(a,i4)' ) '  Order of precision ', mop
        write ( *, '(a)' ) '  Errors :    Absolute    Relative'
        write ( *, '(a)' ) '  ---------+-------------------------'
        write ( *, '(a,2d12.3)' ) '  Minimum :', emn, ern
        write ( *, '(a,2d12.3)' ) '  Maximum :', emx, erx
        write ( *, '(a)' ) ' '
        write ( *, '(a,d13.3)' ) '  Weights ratio       ', erest

        if ( m .le. mex ) then

          ek = e(m)
          do j = 1, mop
            ek = ek / dble ( j )
          end do

          write ( *, '(a,i2,a,d13.3)' ) 
     &      '  Error in ', mop, 'th power ', e(m)
          write ( *, '(a,d13.3)' ) '  Error constant      ', ek

        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Moments:'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '            True             from QF  ' 
     &    // '        Error      Relative'
        write ( *, '(a)' ) ' '
        do j = 1, mx
          write ( *, '(i4,2d19.10,2d12.3)' ) j, w(j), qm(j), e(j), er(j)
        end do
        write ( *, '(a)' ) ' '
        do j = m, mex
          write ( *, '(i4,2d19.10,2d12.3)' ) j, w(j), qm(j), e(j), er(j)
        end do

      end if

      return
      end
      subroutine ciqf ( nt, t, mlt, nwts, ndx, key, kind, alpha, beta, 
     &  a, b, lo, wts )

c*********************************************************************72
c
cc CIQF computes weights for a classical weight function and any interval.
c
c  Discussion:
c
c    This routine compute somes or all the weights of a quadrature formula
c    for a classical weight function with any valid A, B and a given set of
c    knots and multiplicities.
c
c    The weights may be packed into the output array WTS according to a
c    user-defined pattern or sequentially.
c
c    The routine will also optionally print knots and weights and a check
c    of the moments.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the knots.
c
c    Input, integer MLT(NT), the multiplicity of the knots.
c
c    Input, integer NWTS, the number of weights.
c
c    Input/output, integer NDX(NT), used to index the output
c    array WTS.  If KEY = 1, then NDX need not be preset.  For more
c    details see the comments in CAWIQ.
c
c    Input, integer KEY, indicates the structure of the WTS
c    array.  It will normally be set to 1.  This will cause the weights to be
c    packed sequentially in array WTS.  For more details see the comments
c    in CAWIQ.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, double precision A, B, the interval endpoints.
c
c    Input, integer LO, selects the actions to perform.
c     > 0, compute and print weights.  Print moments check.
c     = 0, compute weights.
c     .lt. 0, compute and print weights.
c
c    Output, double precision WTS(NWTS), the weights.
c
      implicit none

      integer nt
      integer nwts

      double precision a
      double precision alpha
      double precision b
      double precision beta
      integer j
      integer k
      integer key
      integer kind
      integer l
      integer lo
      integer lu
      integer m
      integer mex
      integer mlt(nt)
      integer mop
      integer ndx(nt)
      double precision st(nt)
      double precision t(nt)
      double precision wts(nwts)

      m = 1
      l = abs ( key )

      do j = 1, nt
        if ( l .eq. 1 .or. abs ( ndx(j) ) .ne. 0 ) then
          m = m + mlt(j)
        end if
      end do

      if ( nwts + 1 .lt. m ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CIQF - Fatal error!'
        write ( *, '(a)' ) '  NWTS + 1 .lt. M.'
        stop 1
      end if

      mex = 2 + m
c
c  Scale the knots to default A, B.
c
      call sct ( nt, t, kind, a, b, st )

      lu = 0

      call ciqfs ( nt, st, mlt, nwts, ndx, key, kind, alpha, beta, lu, 
     &  wts )
c
c  Don't scale user's knots - only scale weights.
c
      call scqf ( nt, st, mlt, wts, nwts, ndx, wts, st, kind, alpha,    
     &  beta, a, b )

      if ( lo .ne. 0 ) then

        mop = m - 1

        call chkqf ( t, wts, mlt, nt, nwts, ndx, key, mop, mex, kind,   
     &    alpha, beta, lo, a, b )

      end if

      return
      end
      subroutine ciqfs ( nt, t, mlt, nwts, ndx, key, kind, alpha, 
     &  beta, lo, wts )

c*********************************************************************72
c
cc CIQFS computes some weights of a quadrature formula in the default interval.
c
c  Discussion:
c
c    This routine computes some or all the weights of a quadrature formula
c    for a classical weight function with default values of A and B,
c    and a given set of knots and multiplicities.
c
c    The weights may be packed into the output array WTS according to a
c    user-defined pattern or sequentially.
c
c    The routine will also optionally print knots and weights and a check of
c    the moments.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the knots.
c
c    Input, integer MLT(NT), the multiplicity of the knots.
c
c    Input, integer NWTS, the number of weights.
c
c    Input/output, integer NDX(NT),  used to index the output
c    array WTS.  If KEY = 1, then NDX need not be preset.  For more
c    details see the comments in CAWIQ.
c
c    Input, integer KEY, indicates the structure of the WTS
c    array.  It will normally be set to 1.  For more details see
c    the comments in CAWIQ.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, integer LO, selects the actions to perform.
c     > 0, compute and print weights.  Print moments check.
c     = 0, compute weights.
c     .lt. 0, compute and print weights.
c
c    Output, double precision WTS(NWTS), the weights.
c
      implicit none

      integer nt
      integer nwts

      double precision alpha
      double precision beta
      integer j
      integer jdf
      integer key
      integer kind
      integer l
      integer lo
      integer mex
      integer mlt(nt)
      integer mmex
      integer mop
      integer n
      integer ndx(nt)
      integer nst
      double precision t(nt)
      double precision wts(nwts)
      double precision zemu

      jdf = 0
      n = 0
      l = abs ( key )

      do j = 1, nt

        if ( l .eq. 1 .or. abs ( ndx(j) ) .ne. 0 ) then
          n = n + mlt(j)
        end if

      end do
c
c  N knots when counted according to multiplicity.
c
      if ( nwts .lt. n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CIQFS - Fatal error!'
        write ( *, '(a)' ) '  NWTS .lt. N.'
        stop 1
      end if

      mex = n + 3
      nst = ( n + 1 ) / 2

      call ciqfs_sub ( nt, t, mlt, nwts, ndx, key, kind, alpha, 
     &  beta, lo, wts, jdf, n, mex, nst, zemu )

      return
      end
      subroutine ciqfs_sub ( nt, t, mlt, nwts, ndx, key, kind, alpha, 
     &  beta, lo, wts, jdf, n, mex, nst, zemu )

c*********************************************************************72
c
cc CIQFS_SUB can allocate some temporary arrays for CIQFS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
c
c  Author:
c
c    Original FORTRAN77 version by Sylvan Elhay, Jaroslav Kautsky.
c    This FORTRAN77 version by John Burkardt.
c
c  Parameters:
c
      implicit none

      integer mex
      integer n
      integer nst
      integer nt
      integer nwts

      double precision aj(nst)
      double precision alpha
      double precision beta
      double precision bj(nst)
      integer j
      integer jdf
      integer key
      integer kind
      integer l
      integer lo
      integer mlt(nt)
      integer mmex
      integer mop
      integer ndx(nt)
      double precision t(nt)
      double precision w(mex)
      double precision wts(nwts)
      double precision zemu
c
c  Get the Jacobi matrix.
c
      call class_matrix ( kind, nst, alpha, beta, aj, bj, zemu )
c
c  Call weights routine.
c
      call cawiq ( nt, t, mlt, n, ndx, key, nst, aj, bj, jdf, zemu, 
     &  wts )
c
c  Return if no printing or checking required.
c
      if ( lo .ne. 0 ) then

        mop = n

        call chkqfs ( t, wts, mlt, nt, n, ndx, key, w, mop, mex, kind,  
     &    alpha, beta, lo )

      end if

      return
      end
      subroutine class_matrix ( kind, m, alpha, beta, aj, bj, zemu )

c*********************************************************************72
c
cc CLASS_MATRIX computes the Jacobi matrix for a quadrature rule.
c
c  Discussion:
c
c    This routine computes the diagonal AJ and sub-diagonal BJ
c    elements of the order M tridiagonal symmetric Jacobi matrix
c    associated with the polynomials orthogonal with respect to
c    the weight function specified by KIND.
c
c    For weight functions 1-7, M elements are defined in BJ even
c    though only M-1 are needed.  For weight function 8, BJ(M) is
c    set to zero.
c
c    The zero-th moment of the weight function is returned in ZEMU.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, integer M, the order of the Jacobi matrix.
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Output, double precision AJ(M), BJ(M), the diagonal and subdiagonal
c    of the Jacobi matrix.
c
c    Output, double precision ZEMU, the zero-th moment.
c
      implicit none

      integer m

      double precision a2b2
      double precision ab
      double precision aba
      double precision abi
      double precision abj
      double precision abti
      double precision aj(m)
      double precision alpha
      double precision apone
      double precision beta
      double precision bj(m)
      integer i
      integer kind
      double precision pi
      parameter ( pi = 3.14159265358979323846264338327950D+00 )
      double precision r8_gamma
      double precision r8_epsilon
      double precision temp
      double precision temp2
      double precision zemu

      temp = r8_epsilon ( )

      call parchk ( kind, 2 * m - 1, alpha, beta )

      temp2 = r8_gamma ( 0.5D+00 )

      if ( 500.0D+00 * temp .lt. abs ( temp2 * temp2 - pi ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CLASS_MATRIX - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Gamma function does not match machine parameters.'
        stop 1
      end if

      if ( kind .eq. 1 ) then

        ab = 0.0D+00

        zemu = 2.0D+00 / ( ab + 1.0D+00 )

        do i = 1, m
          aj(i) = 0.0D+00
        end do

        do i = 1, m
          abi = i + ab * mod ( i, 2 )
          abj = 2 * i + ab
          bj(i) = abi * abi / ( abj * abj - 1.0D+00 )
        end do

        do i = 1, m
          bj(i) = sqrt ( bj(i) )
        end do

      else if ( kind .eq. 2 ) then

        zemu = pi

        do i = 1, m
          aj(i) = 0.0D+00
        end do

        bj(1) =  sqrt ( 0.5D+00 )
        do i = 2, m
          bj(i) = 0.5D+00
        end do

      else if ( kind .eq. 3 ) then

        ab = alpha * 2.0D+00
        zemu = 2.0D+00**( ab + 1.0D+00 ) 
     &    * ( r8_gamma ( alpha + 1.0D+00 ) )**2
     &    / r8_gamma ( ab + 2.0D+00 )

        do i = 1, m
          aj(i) = 0.0D+00
        end do

        bj(1) = 1.0D+00 / ( 2.0D+00 * alpha + 3.0D+00 )
        do i = 2, m
          bj(i) = i * ( i + ab )
     &      / ( 4.0D+00 * ( i + alpha ) * ( i + alpha ) - 1.0D+00 )
        end do
        do i = 1, m
          bj(i) =  sqrt ( bj(i) )
        end do

      else if ( kind .eq. 4 ) then

        ab = alpha + beta
        abi = 2.0D+00 + ab
        zemu = 2.0D+00**( ab + 1.0D+00 ) * r8_gamma ( alpha + 1.0D+00 ) 
     &      * r8_gamma ( beta + 1.0D+00 ) / r8_gamma ( abi )
        aj(1) = ( beta - alpha ) / abi
        bj(1) = 4.0D+00 * ( 1.0 + alpha ) * ( 1.0D+00 + beta )
     &    / ( ( abi + 1.0D+00 ) * abi * abi )
        a2b2 = beta * beta - alpha * alpha

        do i = 2, m
          abi = 2.0D+00 * i + ab
          aj(i) = a2b2 / ( ( abi - 2.0D+00 ) * abi )
          abi = abi * abi
          bj(i) = 4.0D+00 * i * ( i + alpha ) * ( i + beta ) 
     &      * ( i + ab ) / ( ( abi - 1.0D+00 ) * abi )
        end do

        do i = 1, m
          bj(i) =  sqrt ( bj(i) )
        end do

      else if ( kind .eq. 5 ) then

        zemu = r8_gamma ( alpha + 1.0D+00 )

        do i = 1, m
          aj(i) = 2.0D+00 * i - 1.0D+00 + alpha
          bj(i) = i * ( i + alpha )
        end do

        do i = 1, m
          bj(i) = sqrt ( bj(i) )
        end do

      else if ( kind .eq. 6 ) then

        zemu = r8_gamma ( ( alpha + 1.0D+00 ) / 2.0D+00 )

        do i = 1, m
          aj(i) = 0.0D+00
        end do

        do i = 1, m
          bj(i) = ( i + alpha * mod ( i, 2 ) ) / 2.0D+00
        end do

        do i = 1, m
          bj(i) =  sqrt ( bj(i) )
        end do

      else if ( kind .eq. 7 ) then

        ab = alpha
        zemu = 2.0D+00 / ( ab + 1.0D+00 )

        do i = 1, m
          aj(i) = 0.0D+00
        end do

        do i = 1, m
          abi = i + ab * mod ( i, 2 )
          abj = 2 * i + ab
          bj(i) = abi * abi / ( abj * abj - 1.0D+00 )
        end do

        do i = 1, m
          bj(i) =  sqrt ( bj(i) )
        end do

      else if ( kind .eq. 8 ) then

        ab = alpha + beta
        zemu = r8_gamma ( alpha + 1.0D+00 ) 
     &    * r8_gamma ( - ( ab + 1.0D+00 ) )
     &    / r8_gamma ( - beta )
        apone = alpha + 1.0D+00
        aba = ab * apone
        aj(1) = - apone / ( ab + 2.0D+00 )
        bj(1) = - aj(1) * ( beta + 1.0D+00 ) / ( ab + 2.0D+00 ) 
     &    / ( ab + 3.0D+00 )
        do i = 2, m
          abti = ab + 2.0D+00 * i
          aj(i) = aba + 2.0D+00 * ( ab + i ) * ( i - 1 )
          aj(i) = - aj(i) / abti / ( abti - 2.0D+00 )
        end do

        do i = 2, m - 1
          abti = ab + 2.0D+00 * i
          bj(i) = i * ( alpha + i ) / ( abti - 1.0D+00 ) * ( beta + i ) 
     &      / ( abti * abti ) * ( ab + i ) / ( abti + 1.0D+00 )
        end do

        bj(m) = 0.0D+00

        do i = 1, m
          bj(i) =  sqrt ( bj(i) )
        end do

      else if ( kind .eq. 9 ) then

        zemu = pi / 2.0D+00

        do i = 1, m
          aj(i) = 0.0D+00
          bj(i) = 0.5D+00
        end do

      end if

      return
      end
      subroutine cliqf ( nt, t, kind, alpha, beta, a, b, lo, wts )

c*********************************************************************72
c
cc CLIQF computes a classical quadrature formula, with optional printing.
c
c  Discussion:
c
c    This routine computes all the weights of an interpolatory
c    quadrature formula with
c    1. only simple knots and
c    2. a classical weight function with any valid A and B, and
c    3. optionally prints the knots and weights and a check of the moments.
c
c    To evaluate this quadrature formula for a given function F,
c    call routine EIQFS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, double precision A, B, the interval endpoints.
c
c    Input, integer LO, indicates what is to be done.
c    > 0, compute and print weights and moments check.
c    = 0, compute weights.
c    .lt. 0, compute and print weights.
c
c    Output, double precision WTS(NT), the weights.
c
      implicit none

      integer nt

      double precision a
      double precision alpha
      double precision b
      double precision beta
      integer i
      integer key
      integer kind
      integer lo
      integer mlt(nt)
      integer ndx(nt)
      double precision t(nt)
      double precision wts(nt)

      key = 1

      do i = 1, nt
        mlt(i) = 1
      end do

      call ciqf ( nt, t, mlt, nt, ndx, key, kind, alpha, beta, a, b, lo,
     &  wts )

      return
      end
      subroutine cliqfs ( nt, t, kind, alpha, beta, lo, wts )

c*********************************************************************72
c
cc CLIQFS computes the weights of a quadrature formula in the default interval.
c
c  Discussion:
c
c    This routine computes the weights of an interpolatory quadrature formula
c    with a classical weight function, in the default interval A, B,
c    using only simple knots.
c
c    It can optionally print knots and weights and a check of the moments.
c
c    To evaluate a quadrature computed by CLIQFS, call EIQFS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, integer LO, chooses the printing option.
c     > 0, compute weights, print them, print the moment check results.
c     0, compute weights.
c     .lt. 0, compute weights and print them.
c
c    Output, double precision WTS(NT), the weights.
c
      implicit none

      integer nt

      double precision alpha
      double precision beta
      integer i
      integer key
      integer kind
      integer lo
      integer mlt(nt)
      integer ndx(nt)
      double precision t(nt)
      double precision wts(nt)

      key = 1

      do i = 1, nt
        mlt(i) = 1
      end do

      call ciqfs ( nt, t, mlt, nt, ndx, key, kind, alpha, beta, lo, 
     &  wts )

      return
      end
      subroutine cwiqd ( m, nm, l, v, xk, nstar, phi, a, r, d )

c*********************************************************************72
c
cc CWIQD computes all the weights for a given knot.
c
c  Discussion:
c
c    The variable names correspond to the 1982 reference, and explanations of
c    some of the terminology may be found there.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c    Jaroslav Kautsky, Sylvan Elhay,
c    Calculation of the Weights of Interpolatory Quadratures,
c    Numerische Mathematik,
c    Volume 40, 1982, pages 407-422.
c
c  Parameters:
c
c    Input, integer M, the multiplicity of the knot in question.
c
c    Input, integer NM, is equal to max ( N - M, 1 ), where N is
c    the number of knots used, counted according to multiplicity.
c
c    Input, integer L, min ( M, N - M + 1), where N is the number
c    of knots used, counted according to multiplicity.
c
c    Input, double precision V,  the knot in question.
c
c    Input, double precision XK(NM), all but the last M entries in the
c    diagonal of K-hat.
c
c    Input, integer NSTAR, the dimension of the Jacobi matrix.
c
c    Input, double precision PHI(NSTAR), the eigenvalues of the Jacobi matrix.
c
c    Input, double precision A(NSTAR), the square of the first row of the
c    orthogonal matrix that diagonalizes the Jacobi matrix.
c
c    Input, double precision R(L), used to compute the right
c    principal vectors.
c
c    Output, double precision D(M), the weights.
c
      implicit none

      integer l
      integer m
      integer nm
      integer nstar

      double precision a(nstar)
      double precision d(m)
      integer i
      integer j
      integer jr
      integer k
      integer last
      integer minil
      double precision phi(nstar)
      double precision r(l)
      double precision sum
      double precision tmp
      double precision v
      double precision wf(nstar)
      double precision xk(nm)
      double precision y(m)
      double precision z(m)
c
c  Compute products required for Y-hat.
c
      do j = 1, nstar
        wf(j) = a(j)
        do i = 1, nm
          wf(j) = wf(j) * ( phi(j) - xk(i) )
        end do
      end do
c
c  Compute Y-hat.
c
      do i = 1, m
        sum = 0.0D+00
        do j = 1, nstar
          sum = sum + wf(j)
          wf(j) = wf(j) * ( phi(j) - v )
        end do
        y(i) = sum
      end do
c
c  If N = 1 the right principal vector is already in R.
c  Otherwise compute the R-principal vector of grade M-1.
c
      do i = 1, nm

        tmp = v - xk(i)

        last = min ( l, i + 1 )
        do j = last, 2, -1
          r(j) = tmp * r(j) + r(j-1)
        end do

        r(1) = tmp * r(1)

      end do
c
c  Compute left principal vector(s) and weight for highest derivative.
c  The following statement contains the only division in this
c  routine.  Any test for overflow should be made after it.
c
      d(m) = y(m) / r(1)

      if ( m .eq. 1 ) then
        return
      end if
c
c  Compute left principal vector.
c
      z(1) = 1.0D+00 / r(1)
      do i = 2, m
        sum = 0.0D+00
        minil = min ( i, l )
        do j = 2, minil
          k = i - j + 1
          sum = sum + r(j) * z(k)
        end do
        z(i) = - sum * z(1)
      end do
c
c  Accumulate weights.
c
      do i = 2, m
        sum = 0.0D+00
        do j = 1, i
          k = m - i + j
          sum = sum + z(j) * y(k)
        end do
        k = m - i + 1
        d(k) = sum
      end do

      return
      end
      subroutine eiqf ( nt, t, mlt, wts, nwts, ndx, key, f, qfsum )

c*********************************************************************72
c
cc EIQF evaluates an interpolatory quadrature formula.
c
c  Discussion:
c
c   The knots, weights and integrand are supplied.
c
c   All knots with nonzero NDX are used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the knots.
c
c    Input, integer MLT(NT), the multiplicity of the knots.
c
c    Input, double precision WTS(NWTS), the weights.
c
c    Input, integer NWTS, the number of weights.
c
c    Input, integer NDX(NT), used to index the array WTS.
c    If KEY = 1, then NDX need not be preset.  For more details see the
c    comments in CAWIQ.
c
c    Input, integer KEY, indicates the structure of the WTS
c    array.  It will normally be set to 1.  This will cause the weights to be
c    packed sequentially in array WTS.  For more details see the comments
c    in CAWIQ.
c
c    Input, double precision, external F, the name of a routine which
c    evaluates the function and some of its derivatives.  The routine
c    must have the form
c      function f ( x, i )
c      double precision f
c      integer i
c      double precision x
c    and return in F the value of the I-th derivative of the function
c    at X.  The highest value of I will be the maximum value in MLT minus
c    one.  The value X will always be a knot.
c
c    Output, double precision QFSUM, the value of the quadrature formula
c    applied to F.
c
      implicit none

      integer nt
      integer nwts

      double precision, external :: f
      integer i
      integer j
      integer key
      integer l
      integer mlt(nt)
      integer ndx(nt)
      double precision p
      double precision qfsum
      double precision t(nt)
      double precision wts(nwts)

      l = abs ( key )

      if ( l .lt. 1 .or. 4 .lt. l ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'EIQF - Fatal error!'
        write ( *, '(a)' ) '  Magnitude of KEY must be between 1 and 4.'
        stop 1
      end if

      qfsum = 0.0D+00
      do j = 1, nt
        l = abs ( ndx(j) )
        if ( l .ne. 0 ) then
          p = 1.0D+00
          do i = 1, mlt(j)
            qfsum = qfsum + wts(l+i-1) * f ( t(j), i - 1 ) / p
            if ( key .le. 0 ) then
              p = p * i
            end if
          end do
        end if
      end do

      return
      end
      subroutine eiqfs ( nt, t, wts, f, qfsum )

c*********************************************************************72
c
cc EIQFS evaluates a quadrature formula defined by CLIQF or CLIQFS.
c
c  Discussion:
c
c    This routine evaluates an interpolatory quadrature formula with all knots
c    simple and all knots included in the quadrature.  This routine will be used
c    typically after CLIQF or CLIQFS has been called.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the knots.
c
c    Input, double precision WTS(NT), the weights.
c
c    Input, double precision, external F, the name of a routine which
c    evaluates the function and some of its derivatives.  The routine
c    must have the form
c      function f ( x, i )
c      double precision f
c      integer i
c      double precision x
c    and return in F the value of the I-th derivative of the function
c    at X.  The value of I will always be 0.  The value X will always be a knot.
c
c    Output, double precision QFSUM, the value of the quadrature formula
c    applied to F.
c
      implicit none

      integer nt

      double precision, external :: f
      integer j
      double precision qfsum
      double precision t(nt)
      double precision wts(nt)

      qfsum = 0.0D+00
      do j = 1, nt
        qfsum = qfsum + wts(j) * f ( t(j), 0 )
      end do

      return
      end
      function i4vec_sum ( n, a )

c*********************************************************************72
c
cc I4VEC_SUM returns the sum of the entries of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    In FORTRAN90, this facility is offered by the built in
c    SUM function:
c
c      I4VEC_SUM ( N, A ) = SUM ( A(1:N) )
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
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer I4VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer i4vec_sum

      i4vec_sum = 0

      do i = 1, n
        i4vec_sum = i4vec_sum + a(i)
      end do

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
c    17 September 2013
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
      subroutine parchk ( kind, m, alpha, beta )

c*********************************************************************72
c
cc PARCHK checks parameters ALPHA and BETA for classical weight functions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, integer M, the order of the highest moment to
c    be calculated.  This value is only needed when KIND = 8.
c
c    Input, double precision ALPHA, BETA, the parameters, if required
c    by the value of KIND.
c
      implicit none

      double precision alpha
      double precision beta
      integer kind
      integer m
      double precision tmp

      if ( kind .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PARCHK - Fatal error!'
        write ( *, '(a)' ) '  KIND .le. 0.'
        stop 1
      end if
c
c  Check ALPHA for Gegenbauer, Jacobi, Laguerre, Hermite, Exponential.
c
      if ( 3 .le. kind .and. kind .le. 8 .and. 
     &  alpha .le. -1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PARCHK - Fatal error!'
        write ( *, '(a)' ) '  3 .le. KIND and ALPHA .le. -1.'
        stop 1
      end if
c
c  Check BETA for Jacobi.
c
      if ( kind .eq. 4 .and. beta .le. -1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'PARCHK - Fatal error!'
        write ( *, '(a)' ) '  KIND .eq. 4 and BETA .le. -1.0.'
        stop 1
      end if
c
c  Check ALPHA and BETA for rational.
c
      if ( kind .eq. 8 ) then
        tmp = alpha + beta + m + 1.0D+00
        if ( 0.0D+00 .le. tmp .or. tmp .le. beta ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'PARCHK - Fatal error!'
          write ( *, '(a)' ) 
     &      '  KIND .eq. 8 but condition on ALPHA and BETA fails.'
          stop 1
        end if
      end if

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
      function r8_gamma ( x )

c*********************************************************************72
c
cc R8_GAMMA evaluates Gamma(X) for a real argument.
c
c  Discussion:
c
c    This routine calculates the gamma function for a real argument X.
c    Computation is based on an algorithm outlined in reference 1.
c    The program uses rational functions that approximate the gamma
c    function to at least 20 significant decimal digits.  Coefficients
c    for the approximation over the interval (1,2) are unpublished.
c    Those for the approximation for 12 .le. X are from reference 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 January 2008
c
c  Author:
c
c    Original FORTRAN77 version by William Cody, Laura Stoltz.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    William Cody,
c    An Overview of Software Development for Special Functions,
c    in Numerical Analysis Dundee, 1975,
c    edited by GA Watson,
c    Lecture Notes in Mathematics 506,
c    Springer, 1976.
c
c    John Hart, Ward Cheney, Charles Lawson, Hans Maehly, 
c    Charles Mesztenyi, John Rice, Henry Thatcher, 
c    Christoph Witzgall,
c    Computer Approximations,
c    Wiley, 1968,
c    LC: QA297.C64.
c
c  Parameters:
c
c    Input, double precision X, the argument of the function.
c
c    Output, double precision R8_GAMMA, the value of the function.
c
      implicit none

      double precision c(7)
      double precision eps
      double precision fact
      integer i
      integer n
      double precision p(8)
      logical parity
      double precision pi
      double precision q(8)
      double precision r8_gamma
      double precision res
      double precision sqrtpi
      double precision sum
      double precision x
      double precision xbig
      double precision xden
      double precision xinf
      double precision xminin
      double precision xnum
      double precision y
      double precision y1
      double precision ysq
      double precision z
c
c  Mathematical constants
c
      data sqrtpi / 0.9189385332046727417803297D+00 /
      data pi / 3.1415926535897932384626434D+00 /
c
c  Machine dependent parameters
c
      data xbig / 171.624D+00 /
      data xminin / 2.23D-308 /
      data eps / 2.22D-16 /
      data xinf /1.79D+308 /
c
c  Numerator and denominator coefficients for rational minimax
c  approximation over (1,2).
c
      data p /
     & -1.71618513886549492533811d+00,
     &  2.47656508055759199108314d+01,
     & -3.79804256470945635097577d+02,
     &  6.29331155312818442661052d+02,
     &  8.66966202790413211295064d+02,
     & -3.14512729688483675254357d+04,
     & -3.61444134186911729807069d+04,
     &  6.64561438202405440627855d+04 /

      data q /
     & -3.08402300119738975254353d+01,
     &  3.15350626979604161529144d+02,
     & -1.01515636749021914166146d+03,
     & -3.10777167157231109440444d+03,
     &  2.25381184209801510330112d+04,
     &  4.75584627752788110767815d+03,
     & -1.34659959864969306392456d+05,
     & -1.15132259675553483497211d+05 /
c
c  Coefficients for minimax approximation over (12, INF).
c
      data c /
     & -1.910444077728D-03,
     &  8.4171387781295D-04,
     & -5.952379913043012D-04,
     &  7.93650793500350248D-04,
     & -2.777777777777681622553D-03,
     &  8.333333333333333331554247D-02,
     &  5.7083835261D-03 /

      parity = .false.
      fact = 1.0D+00
      n = 0
      y = x
c
c  Argument is negative.
c
      if ( y .le. 0.0D+00 ) then

        y = - x
        y1 = aint ( y )
        res = y - y1

        if ( res .ne. 0.0D+00 ) then

          if ( y1 .ne. aint ( y1 * 0.5D+00 ) * 2.0D+00 ) then
            parity = .true.
          end if

          fact = - pi / sin ( pi * res )
          y = y + 1.0D+00

        else

          res = xinf
          r8_gamma = res
          return

        end if

      end if
c
c  Argument is positive.
c
      if ( y .lt. eps ) then
c
c  Argument < EPS.
c
        if ( xminin .le. y ) then
          res = 1.0D+00 / y
        else
          res = xinf
          r8_gamma = res
          return
        end if

      else if ( y .lt. 12.0D+00 ) then

        y1 = y
c
c  0.0 < argument < 1.0.
c
        if ( y .lt. 1.0D+00 ) then

          z = y
          y = y + 1.0D+00
c
c  1.0 < argument < 12.0.
c  Reduce argument if necessary.
c
        else

          n = int ( y ) - 1
          y = y - dble ( n )
          z = y - 1.0D+00

        end if
c
c  Evaluate approximation for 1.0 < argument < 2.0.
c
        xnum = 0.0D+00
        xden = 1.0D+00
        do i = 1, 8
          xnum = ( xnum + p(i) ) * z
          xden = xden * z + q(i)
        end do

        res = xnum / xden + 1.0D+00
c
c  Adjust result for case  0.0 < argument < 1.0.
c
        if ( y1 .lt. y ) then

          res = res / y1
c
c  Adjust result for case 2.0 < argument < 12.0.
c
        else if ( y .lt. y1 ) then

          do i = 1, n
            res = res * y
            y = y + 1.0D+00
          end do

        end if

      else
c
c  Evaluate for 12.0 .le. argument.
c
        if ( y .le. xbig ) then

          ysq = y * y
          sum = c(7)
          do i = 1, 6
            sum = sum / ysq + c(i)
          end do
          sum = sum / y - y + sqrtpi
          sum = sum + ( y - 0.5D+00 ) * log ( y )
          res = exp ( sum )

        else

          res = xinf
          r8_gamma = res
          return

        end if

      end if
c
c  Final adjustments and return.
c
      if ( parity ) then
        res = - res
      end if

      if ( fact .ne. 1.0D+00 ) then
        res = fact / res
      end if

      r8_gamma = res

      return
      end
      function r8vec_amax ( n, a )

c*********************************************************************72
c
cc R8VEC_AMAX returns the maximum absolute value in an R8VEC.
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
c    02 June 2009
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
c    Output, double precision R8VEC_AMAX, the value of the entry
c    of largest magnitude.
c
      implicit none

      integer n

      double precision a(n)
      double precision r8vec_amax
      integer i
      double precision value

      value = abs ( a(1) )
      do i = 2, n
        value = max ( value, abs ( a(i) ) )
      end do

      r8vec_amax = value

      return
      end
      function r8vec_amin ( n, a )

c*********************************************************************72
c
cc R8VEC_AMIN returns the minimum absolute value in an R8VEC.
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
c    02 June 2009
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
c    Output, double precision R8VEC_AMIN, the value of the entry
c    of smallest magnitude.
c
      implicit none

      integer n

      double precision a(n)
      double precision r8vec_amin
      integer i
      double precision value

      value = abs ( a(1) )
      do i = 2, n
        value = min ( value, abs ( a(i) ) )
      end do

      r8vec_amin = value

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
      subroutine scmm ( m, kind, alpha, beta, a, b, w )

c*********************************************************************72
c
cc SCMM computes moments of a classical weight function scaled to [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer M, the number of moments.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, double precision A, B, the interval endpoints.
c
c    Output, double precision W(M), the scaled moments.
c
      implicit none

      integer m

      double precision a
      double precision al
      double precision alpha
      double precision b
      double precision be
      double precision beta
      integer i
      integer kind
      double precision p
      double precision q
      double precision r8_epsilon
      double precision temp
      double precision tmp
      double precision w(m)

      temp = r8_epsilon ( )

      if ( kind .eq. 1 ) then

        al = 0.0D+00
        be = 0.0D+00

        if ( abs ( b - a ) .le. temp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCMM - Fatal error!'
          write ( *, '(a)' ) '  B - A too small!'
          stop 1
        end if

        q = ( b - a ) / 2.0D+00
        p = q ** ( al + be + 1.0D+00 )

      else if ( kind .eq. 2 ) then

        al = -0.5D+00
        be = -0.5D+00

        if ( abs ( b - a ) .le. temp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCMM - Fatal error!'
          write ( *, '(a)' ) '  B - A too small!'
          stop 1
        end if

        q = ( b - a ) / 2.0D+00
        p = q ** ( al + be + 1.0D+00 )

      else if ( kind .eq. 3 ) then

        al = alpha
        be = alpha

        if ( abs ( b - a ) .le. temp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCMM - Fatal error!'
          write ( *, '(a)' ) '  B - A too small!'
          stop 1
        end if

        q = ( b - a ) / 2.0D+00
        p = q ** ( al + be + 1.0D+00 )

      else if ( kind .eq. 4 ) then

        al = alpha
        be = beta

        if ( abs ( b - a ) .le. temp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCMM - Fatal error!'
          write ( *, '(a)' ) '  B - A too small!'
          stop 1
        end if

        q = ( b - a ) / 2.0D+00
        p = q ** ( al + be + 1.0D+00 )

      else if ( kind .eq. 5 ) then

        if ( b .le. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCMM - Fatal error!'
          write ( *, '(a)' ) '  B .le. 0!'
          stop 1
        end if

        q = 1.0D+00 / b
        p = q ** ( alpha + 1.0D+00 )

      else if ( kind .eq. 6 ) then

        if ( b .le. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCMM - Fatal error!'
          write ( *, '(a)' ) '  B .le. 0!'
          stop 1
        end if

        q = 1.0D+00 / sqrt ( b )
        p = q ** ( alpha + 1.0D+00 )

      else if ( kind .eq. 7 ) then

        al = alpha
        be = 0.0D+00

        if ( abs ( b - a ) .le. temp) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCMM - Fatal error!'
          write ( *, '(a)' ) '  B - A too small!'
          stop 1
        end if

        q = ( b - a ) / 2.0D+00
        p = q ** ( al + be + 1.0D+00 )

      else if ( kind .eq. 8 ) then

        if ( a + b .le. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCMM - Fatal error!'
          write ( *, '(a)' ) '  A + B .le. 0'
          stop 1
        end if

        q = a + b
        p = q ** ( alpha + beta + 1.0D+00 )

      else if ( kind .eq. 9 ) then

        if ( abs ( b - a ) .le. temp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCMM - Fatal error!'
          write ( *, '(a)' ) '  B - A too small!'
          stop 1
        end if

        q = ( b - a ) / 2.0D+00
        p = q * q

      end if
c
c  Compute the moments in W.
c
      call wm ( m, kind, alpha, beta, w )

      tmp = p

      do i = 1, m
        w(i) = w(i) * tmp
        tmp = tmp * q
      end do

      return
      end
      subroutine scqf ( nt, t, mlt, wts, nwts, ndx, swts, st, kind, 
     &  alpha, beta, a, b )

c*********************************************************************72
c
cc SCQF scales a quadrature formula to a nonstandard interval.
c
c  Discussion:
c
c    The arrays WTS and SWTS may coincide.
c
c    The arrays T and ST may coincide.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the original knots.
c
c    Input, integer MLT(NT), the multiplicity of the knots.
c
c    Input, double precision WTS(NWTS), the weights.
c
c    Input, integer NWTS, the number of weights.
c
c    Input, integer NDX(NT), used to index the array WTS.
c    For more details see the comments in CAWIQ.
c
c    Output, double precision SWTS(NWTS), the scaled weights.
c
c    Output, double precision ST(NT), the scaled knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Input, double precision A, B, the interval endpoints.
c
      implicit none

      integer nt
      integer nwts

      double precision a
      double precision al
      double precision alpha
      double precision b
      double precision be
      double precision beta
      integer i
      integer k
      integer kind
      integer l
      integer mlt(nt)
      integer ndx(nt)
      double precision p
      double precision r8_epsilon
      double precision shft
      double precision slp
      double precision st(nt)
      double precision swts(nwts)
      double precision t(nt)
      double precision temp
      double precision tmp
      double precision wts(nwts)

      temp = r8_epsilon ( )

      call parchk ( kind, 1, alpha, beta )

      if ( kind .eq. 1 ) then

        al = 0.0D+00
        be = 0.0D+00

        if ( abs ( b - a ) .le. temp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCQF - Fatal error!'
          write ( *, '(a)' ) '  |B - A| too small.'
          stop 1
        end if

        shft = ( a + b ) / 2.0D+00
        slp = ( b - a ) / 2.0D+00

      else if ( kind .eq. 2 ) then

        al = -0.5D+00
        be = -0.5D+00

        if ( abs ( b - a ) .le. temp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCQF - Fatal error!'
          write ( *, '(a)' ) '  |B - A| too small.'
          stop 1
        end if

        shft = ( a + b ) / 2.0D+00
        slp = ( b - a ) / 2.0D+00

      else if ( kind .eq. 3 ) then

        al = alpha
        be = alpha

        if ( abs ( b - a ) .le. temp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCQF - Fatal error!'
          write ( *, '(a)' ) '  |B - A| too small.'
          stop 1
        end if

        shft = ( a + b ) / 2.0D+00
        slp = ( b - a ) / 2.0D+00

      else if ( kind .eq. 4 ) then

        al = alpha
        be = beta

        if ( abs ( b - a ) .le. temp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCQF - Fatal error!'
          write ( *, '(a)' ) '  |B - A| too small.'
          stop 1
        end if

        shft = ( a + b ) / 2.0D+00
        slp = ( b - a ) / 2.0D+00

      else if ( kind .eq. 5 ) then

        if ( b .le. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCQF - Fatal error!'
          write ( *, '(a)' ) '  B .le. 0'
          stop 1
        end if

        shft = a
        slp = 1.0D+00 / b
        al = alpha
        be = 0.0D+00

      else if ( kind .eq. 6 ) then

        if ( b .le. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCQF - Fatal error!'
          write ( *, '(a)' ) '  B .le. 0.'
          stop 1
        end if

        shft = a
        slp = 1.0D+00 / sqrt ( b )
        al = alpha
        be = 0.0D+00

      else if ( kind .eq. 7 ) then

        al = alpha
        be = 0.0D+00

        if ( abs ( b - a ) .le. temp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCQF - Fatal error!'
          write ( *, '(a)' ) '  |B - A| too small.'
          stop 1
        end if

        shft = ( a + b ) / 2.0D+00
        slp = ( b - a ) / 2.0D+00

      else if ( kind .eq. 8 ) then

        if ( a + b .le. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCQF - Fatal error!'
          write ( *, '(a)' ) '  A + B .le. 0.'
          stop 1
        end if

        shft = a
        slp = a + b
        al = alpha
        be = beta

      else if ( kind .eq. 9 ) then

        al = 0.5D+00
        be = 0.5D+00

        if ( abs ( b - a ) .le. temp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCQF - Fatal error!'
          write ( *, '(a)' ) '  |B - A| too small.'
          stop 1
        end if

        shft = ( a + b ) / 2.0D+00
        slp = ( b - a ) / 2.0D+00

      end if

      p = slp ** ( al + be + 1.0D+00 )

      do k = 1, nt

        st(k) = shft + slp * t(k)
        l = abs ( ndx(k) )

        if ( l .ne. 0 ) then
          tmp = p
          do i = l, l + mlt(k) - 1
            swts(i) = wts(i) * tmp
            tmp = tmp * slp
          end do
        end if

      end do

      return
      end
      subroutine sct ( nt, t, kind, a, b, st )

c*********************************************************************72
c
cc SCT rescales distinct knots to an interval [A,B].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision T(NT), the original knots.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision A, B, the interval endpoints for which the
c    knots ST should be scaled.
c
c    Output, double precision ST(NT), the scaled knots.
c
      implicit none

      integer nt

      double precision a
      double precision b
      double precision bma
      integer i
      integer kind
      double precision r8_epsilon
      double precision shft
      double precision slp
      double precision st(nt)
      double precision t(nt)
      double precision tmp

      if ( kind .lt. 1 .or. 9 .lt. kind ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SCT - Fatal error!'
        write ( *, '(a)' ) '  KIND falls outside range of 1 to 9.'
        stop 1
      end if

      if ( kind .eq. 1 .or. kind .eq. 2 .or. kind .eq. 3 .or. 
     &  kind .eq. 4 .or. kind .eq. 7 .or. kind .eq. 9 ) then

        tmp = r8_epsilon ( )
        bma = b - a

        if ( bma .le. tmp ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCT - Fatal error!'
          write ( *, '(a)' ) '  B - A too small.'
          stop 1
        end if

        slp = 2.0D+00 / bma
        shft = - ( a + b ) / bma

      else if ( kind .eq. 5 ) then

        if ( b .lt. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCT - Fatal error!'
          write ( *, '(a)' ) '  B .lt. 0.'
          stop 1
        end if

        slp = b
        shft = - a * b

      else if ( kind .eq. 6 ) then

        if ( b .lt. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCT - Fatal error!'
          write ( *, '(a)' ) '  B .lt. 0.'
          stop 1
        end if

        slp = sqrt ( b )
        shft = - a * slp

      else if ( kind .eq. 8 ) then

        slp = 1.0D+00 / ( a + b )

        if ( slp .le. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'SCT - Fatal error.'
          write ( *, '(a)' ) '  1 / ( A + B ) .le. 0.'
          stop 1
        end if

        shft = - a * slp

      end if

      do i = 1, nt
        st(i) = shft + slp * t(i)
      end do

      return
      end
      subroutine sgqf ( nt, aj, bj, zemu, t, wts )

c*********************************************************************72
c
cc SGQF computes knots and weights of a Gauss Quadrature formula.
c
c  Discussion:
c
c    This routine computes all the knots and weights of a Gauss quadrature
c    formula with simple knots from the Jacobi matrix and the zero-th
c    moment of the weight function, using the Golub-Welsch technique.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer NT, the number of knots.
c
c    Input, double precision AJ(NT), the diagonal of the Jacobi matrix.
c
c    Input/output, double precision BJ(NT), the subdiagonal of the Jacobi
c    matrix, in entries 1 through NT-1.  On output, BJ has been overwritten.
c
c    Input, double precision ZEMU, the zero-th moment of the weight function.
c
c    Output, double precision T(NT), the knots.
c
c    Output, double precision WTS(NT), the weights.
c
      implicit none

      integer nt

      double precision aj(nt)
      double precision bj(nt)
      integer i
      double precision t(nt)
      double precision wts(nt)
      double precision zemu
c
c  Exit if the zero-th moment is not positive.
c
      if ( zemu .le. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SGQF - Fatal error!'
        write ( *, '(a)' ) '  ZEMU .le. 0.'
        stop 1
      end if
c
c  Set up vectors for IMTQLX.
c
      do i = 1, nt
        t(i) = aj(i)
      end do

      wts(1) = sqrt ( zemu )
      do i = 2, nt
        wts(i) = 0.0D+00
      end do
c
c  Diagonalize the Jacobi matrix.
c
      call imtqlx ( nt, t, bj, wts )

      do i = 1, nt
        wts(i) = wts(i) ** 2
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
      subroutine wm ( m, kind, alpha, beta, w )

c*********************************************************************72
c
cc WM evaluates the first M moments of classical weight functions.
c
c  Discussion:
c
c    W(K) = Integral ( A .le. X .le. B ) X^(K-1) * W(X) dx
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, integer M, the number of moments to evaluate.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Output, double precision W(M), the first M moments.
c
      implicit none

      integer m

      double precision alpha
      double precision als
      double precision beta
      integer i
      integer ja
      integer jb
      integer k
      integer kind
      double precision pi
      parameter ( pi = 3.14159265358979323846264338327950D+00 )
      double precision r8_gamma
      double precision rk
      double precision sum
      double precision tmpa
      double precision tmpb
      double precision trm
      double precision w(m)

      call parchk ( kind, m, alpha, beta )

      do k = 2, m, 2
        w(k) = 0.0D+00
      end do

      if ( kind .eq. 1 ) then

        do k = 1, m, 2
          rk = dble ( k )
          w(k) = 2.0D+00 / rk
        end do

      else if ( kind .eq. 2 ) then

        w(1) = pi
        do k = 3, m, 2
          rk = dble ( k )
          w(k) = w(k-2) * ( rk - 2.0D+00 ) / ( rk - 1.0D+00 )
        end do

      else if ( kind .eq. 3 ) then

        w(1) = sqrt ( pi ) * r8_gamma ( alpha + 1.0D+00 )
     &    / r8_gamma ( alpha + 3.0D+00 / 2.0D+00 )

        do k = 3, m, 2
          rk = dble ( k )
          w(k) = w(k-2) * ( rk - 2.0D+00 ) / ( 2.0D+00 * alpha + rk )
        end do

      else if ( kind .eq. 4 ) then

        als = alpha + beta + 1.0D+00
        w(1) = 2.0D+00**als * r8_gamma ( alpha + 1.0D+00 )
     &    / r8_gamma ( als + 1.0D+00 ) * r8_gamma ( beta + 1.0D+00 )

        do k = 2, m

          sum = 0.0D+00
          trm = 1.0D+00
          rk = dble ( k )

          do i = 0, ( k - 2 ) / 2

            tmpa = trm
            do ja = 1, 2 * i
              tmpa = tmpa * ( alpha + ja ) / ( als + ja )
            end do

            do jb = 1, k - 2 * i - 1
              tmpa = tmpa * ( beta + jb ) / ( als + 2 * i + jb )
            end do

            tmpa = tmpa / ( 2 * i + 1.0D+00 ) *
     &        ( 2 * i * ( beta + alpha ) + beta - ( rk - 1.0D+00 ) 
     &        * alpha ) / ( beta + rk - 2 * i - 1.0D+00 )
            sum = sum + tmpa

            trm = trm * ( rk - 2 * i - 1.0D+00 )
     &        / ( 2 * i + 1.0D+00 ) * ( rk - 2 * i - 2.0D+00 ) 
     &        / ( 2 * i + 2.0D+00 )

          end do

          if ( mod ( k, 2 ) .ne. 0 ) then
            tmpb = 1.0D+00
            do i = 1, k - 1
              tmpb = tmpb * ( alpha + i ) / ( als + i )
            end do
            sum = sum + tmpb
          end if

          w(k) = sum * w(1)

        end do

      else if ( kind .eq. 5 ) then

        w(1) = r8_gamma ( alpha + 1.0D+00 )

        do k = 2, m
          rk = dble ( k )
          w(k) = ( alpha + rk - 1.0D+00 ) * w(k-1)
        end do

      else if ( kind .eq. 6 ) then

        w(1) = r8_gamma ( ( alpha + 1.0D+00 ) / 2.0D+00 )

        do k = 3, m, 2
          rk = dble ( k )
          w(k) = w(k-2) * ( alpha + rk - 2.0D+00 ) / 2.0D+00
        end do

      else if ( kind .eq. 7 ) then

        als = alpha
        do k = 1, m, 2
          rk = dble ( k )
          w(k) = 2.0D+00 / ( rk + als )
        end do

      else if ( kind .eq. 8 ) then

        w(1) = r8_gamma ( alpha + 1.0D+00 )
     &    * r8_gamma ( - alpha - beta - 1.0D+00 )
     &    / r8_gamma ( - beta )

        do k = 2, m
          rk = dble ( k )
          w(k) = - w(k-1) * ( alpha + rk - 1.0D+00 ) 
     &      / ( alpha + beta + rk )
        end do

      else if ( kind .eq. 9 ) then

        w(1) = pi / 2.0D+00

        do k = 3, m, 2
          rk = dble ( k )
          w(k) = w(k-2) * ( rk - 2.0D+00 ) / ( rk + 1.0D+00 )
        end do

      end if

      return
      end
      subroutine wtfn ( t, nt, kind, alpha, beta, w )

c*********************************************************************72
c
cc WTFN evaluates the classical weight functions at given points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 September 2013
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
c  Parameters:
c
c    Input, double precision T(NT), the points where the weight function
c    is to be evaluated.
c
c    Input, integer NT, the number of evaluation points.
c
c    Input, integer KIND, the rule.
c    1, Legendre,             (a,b)       1.0
c    2, Chebyshev Type 1,     (a,b)       ((b-x)*(x-a))^(-0.5)
c    3, Gegenbauer,           (a,b)       ((b-x)*(x-a))^alpha
c    4, Jacobi,               (a,b)       (b-x)^alpha*(x-a)^beta
c    5, Generalized Laguerre, (a,+oo)     (x-a)^alpha*exp(-b*(x-a))
c    6, Generalized Hermite,  (-oo,+oo)   |x-a|^alpha*exp(-b*(x-a)^2)
c    7, Exponential,          (a,b)       |x-(a+b)/2.0|^alpha
c    8, Rational,             (a,+oo)     (x-a)^alpha*(x+b)^beta
c    9, Chebyshev Type 2,     (a,b)       ((b-x)*(x-a))^(+0.5)
c
c    Input, double precision ALPHA, the value of Alpha, if needed.
c
c    Input, double precision BETA, the value of Beta, if needed.
c
c    Output, double precision W(NT), the value of the weight function.
c
      implicit none

      integer nt

      double precision alpha
      double precision beta
      integer i
      integer kind
      double precision t(nt)
      double precision w(nt)

      call parchk ( kind, 1, alpha, beta )

      if ( kind .eq. 1 ) then

        do i = 1, nt
          w(i) = 1.0D+00
        end do

      else if ( kind .eq. 2 ) then

        do i = 1, nt
          w(i) = 1.0D+00 / sqrt ( ( 1.0D+00 - t(i) ) 
     &      * ( 1.0D+00 + t(i) ) )
        end do

      else if ( kind .eq. 3 ) then

        if ( alpha .eq. 0.0D+00 ) then
          do i = 1, nt
            w(i) = 1.0D+00
          end do
        else
          do i = 1, nt
            w(i) = ( ( 1.0D+00 - t(i) ) 
     &        * ( 1.0D+00 + t(i) ) ) ** alpha
          end do
        end if

      else if ( kind .eq. 4 ) then

        if ( alpha .eq. 0.0D+00 ) then
          do i = 1, nt
            w(i) = 1.0D+00
          end do
        else
          do i = 1, nt
            w(i) = ( 1.0D+00 - t(i) )**alpha
          end do
        end if

        if ( beta .ne. 0.0D+00 ) then
          do i = 1, nt
            w(i) = w(i) * ( 1.0D+00 + t(i) )**beta
          end do
        end if

      else if ( kind .eq. 5 ) then

        if ( alpha .eq. 0.0D+00 ) then
          do i = 1, nt
            w(i) = exp ( - t(i) )
          end do
        else
          do i = 1, nt
            w(i) = exp ( - t(i) ) * t(i)**alpha
          end do
        end if

      else if ( kind .eq. 6 ) then

        if ( alpha .eq. 0.0D+00 ) then
          do i = 1, nt
            w(i) = exp ( - t(i)**2 )
          end do
        else
          do i = 1, nt
            w(i) = exp ( - t(i)**2 ) * abs ( t(i) )**alpha
          end do
        end if

      else if ( kind .eq. 7 ) then

        if ( alpha .ne. 0.0D+00 ) then
          do i = 1, nt
            w(i) = abs ( t(i) )**alpha
          end do
        else
          do i = 1, nt
            w(i) = 1.0D+00
          end do
        end if

      else if ( kind .eq. 8 ) then

        if ( alpha .eq. 0.0D+00 ) then
          do i = 1, nt
            w(i) = 1.0D+00
          end do
        else
          do i = 1, nt
            w(i) = t(i)**alpha
          end do
        end if

        if ( beta .ne. 0.0D+00 ) then
          do i = 1, nt
            w(i) = w(i) * ( 1.0D+00 + t(i) )**beta
          end do
        end if

      else if ( kind .eq. 9 ) then

        do i = 1, nt
          w(i) = sqrt ( ( 1.0D+00 - t(i) ) * ( 1.0D+00 + t(i) ) )
        end do

      end if

      return
      end
