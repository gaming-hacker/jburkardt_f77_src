      program main

c*****************************************************************************80
c
cc MAIN is the main program for LAGUERRE_RULE.
c
c  Discussion:
c
c    This program computes a Gauss-Laguerre quadrature rule 
c    and writes it to a file.
c
c    The user specifies:
c    * the ORDER (number of points) in the rule;
c    * A, the left endpoint;
c    * B, the scale factor;
c    * FILENAME, the root name of the output files.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    14 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer order_max
      parameter ( order_max = 1025 )

      double precision a
      double precision alpha
      integer arg_num
      double precision b
      double precision beta
      character * ( 255 ) filename
      integer iarg
      integer iargc
      integer ierror
      integer kind
      integer last
      integer order
      double precision r(2)
      double precision r8_huge
      character ( len = 255 ) string
      double precision w(order_max)
      double precision x(order_max)

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_RULE'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Compute a Gauss-Laguerre rule for approximating'
      write ( *, '(a)' ) 
     &  '    Integral ( A .le. x < +oo ) exp(-b*(x-a)) f(x) dx'
      write ( *, '(a)' ) '  of order ORDER.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  The user specifies ORDER, A, B, and FILENAME.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ORDER is the number of points in the rule.'
      write ( *, '(a)' ) '  A is the left endpoint, (typically 0).'
      write ( *, '(a)' ) '  B is the scale factor, (typically 1).'
      write ( *, '(a)' ) '  FILENAME is used to generate 3 files:'
      write ( *, '(a)' ) '  * filename_w.txt - the weight file'
      write ( *, '(a)' ) '  * filename_x.txt - the abscissa file.'
      write ( *, '(a)' ) '  * filename_r.txt - the region file.'
c
c  Initialize parameters.
c
      alpha = 0.0D+00
      beta = 0.0D+00
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Get the order.
c
      if ( 1 .le. arg_num ) then
        iarg = 1
        call getarg ( iarg, string )
        call s_to_i4 ( string, order, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Enter the rule order ORDER:'
        read ( *, * ) order
      end if
c
c  Get A.
c
      if ( 2 .le. arg_num ) then
        iarg = 2
        call getarg ( iarg, string )
        call s_to_r8 ( string, a )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Enter the left endpoint, A:'
        read ( *, * ) a
      end if
c
c  Get B.
c
      if ( 3 .le. arg_num ) then
        iarg = 3
        call getarg ( iarg, string )
        call s_to_r8 ( string, b )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Enter the scale factor B:'
        read ( *, * ) b
      end if
c
c  Get FILENAME.
c
      if ( 4 .le. arg_num ) then
        iarg = 4
        call getarg ( iarg, filename )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Enter FILENAME, the "root name" of the quadrature files).'
        read ( *, '(a)' ) filename
      end if
c
c  Input summary.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'      ) '  ORDER = ', order
      write ( *, '(a,g14.6,a)' ) '  A = ', a
      write ( *, '(a,g14.6,a)' ) '  B = ', b
      write ( *, '(a)'         ) 
     &  '  FILENAME = "' // trim ( filename ) // '".'
c
c  Construct the rule.
c
      kind = 5
      call cgqf ( order, kind, alpha, beta, a, b, x, w )
c
c  Write the rule.
c
      r(1) = a
      r(2) = r8_huge ( ) 

      call rule_write ( order, x, w, r, filename )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAGUERRE_RULE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
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
      subroutine cgqf ( nt, kind, alpha, beta, a, b, t, wts )

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

      return
      end
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

      if ( c1_cap == c2_cap ) then
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
      subroutine r8mat_write ( output_filename, m, n, table )

c*********************************************************************72
c
cc R8MAT_WRITE writes a R8MAT file.
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
c    22 October 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision TABLE(M,N), the data.
c
      implicit none

      integer m
      integer n

      integer j
      character * ( * ) output_filename
      integer output_unit
      character * ( 30 ) string
      double precision table(m,n)
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename,
     &  status = 'replace' )
c
c  Create the format string.
c
      if ( 0 .lt. m .and. 0 .lt. n ) then

        write ( string, '(a1,i8,a1,i8,a1,i8,a1)' )
     &    '(', m, 'g', 24, '.', 16, ')'
c
c  Write the data.
c
        do j = 1, n
          write ( output_unit, string ) table(1:m,j)
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

      return
      end
      subroutine rule_write ( n, x, w, r, filename )

c*********************************************************************72
c
cc RULE_WRITE writes a quadrature rule to a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
c    Input, double precision X(N), the abscissas.
c
c    Input, double precision W(N), the weights.
c
c    Input, double precision R(2), defines the region.
c
c    Input, character * ( * ) FILENAME, specifies the output.
c    'filename_w.txt', 'filename_x.txt', 'filename_r.txt' defining weights,
c    abscissas, and region.
c 
      implicit none

      integer n

      character ( len = * ) filename
      character * ( 255 ) filename_r
      character * ( 255 ) filename_w
      character * ( 255 ) filename_x
      integer i
      integer kind
      double precision r(2)
      double precision w(n)
      double precision x(n)

      filename_w = trim ( filename ) // '_w.txt'
      filename_x = trim ( filename ) // '_x.txt'
      filename_r = trim ( filename ) // '_r.txt'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Creating quadrature files.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  "Root" file name is   "' // trim ( filename ) // '".'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Weight file will be   "' // trim ( filename_w ) // '".'
      write ( *, '(a)' ) 
     &  '  Abscissa file will be "' // trim ( filename_x ) // '".'
      write ( *, '(a)' ) 
     &  '  Region file will be   "' // trim ( filename_r ) // '".'
                
      call r8mat_write ( filename_w, 1, n, w )
      call r8mat_write ( filename_x, 1, n, x )
      call r8mat_write ( filename_r, 1, 2, r )

      return
      end
      subroutine s_to_i4 ( s, ival, ierror, length )

c*********************************************************************72
c
cc S_TO_I4 reads an I4 from a string.
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
c    Input, character * ( * ) S, a string to be examined.
c
c    Output, integer IVAL, the integer value read from the string.
c    If the string is blank, then IVAL will be returned 0.
c
c    Output, integer IERROR, an error flag.
c    0, no error.
c    1, an error occurred.
c
c    Output, integer LENGTH, the number of characters of S
c    used to make IVAL.
c
      implicit none

      character c
      integer i
      integer ierror
      integer isgn
      integer istate
      integer ival
      integer length
      character * ( * ) s
      integer s_len

      ierror = 0
      istate = 0
      isgn = 1
      ival = 0 
      s_len = len_trim ( s )

      do i = 1, s_len

        c = s(i:i)
c
c  Haven't read anything.
c
        if ( istate .eq. 0 ) then

          if ( c .eq. ' ' ) then

          else if ( c .eq. '-' ) then
            istate = 1
            isgn = -1
          else if ( c .eq. '+' ) then
            istate = 1
            isgn = + 1
          else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            istate = 2
            ival = ichar ( c ) - ichar ( '0' )
          else
            ierror = 1
            return
          end if
c
c  Have read the sign, expecting digits.
c
        else if ( istate .eq. 1 ) then

          if ( c .eq. ' ' ) then

          else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            istate = 2
            ival = ichar ( c ) - ichar ( '0' )
          else
            ierror = 1
            return
          end if
c
c  Have read at least one digit, expecting more.
c
        else if ( istate .eq. 2 ) then

          if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            ival = 10 * ival + ichar ( c ) - ichar ( '0' )
          else
            ival = isgn * ival
            length = i - 1
            return
          end if

        end if

      end do
c
c  If we read all the characters in the string, see if we're OK.
c
      if ( istate .eq. 2 ) then
        ival = isgn * ival
        length = len_trim ( s )
      else
        ierror = 1
        length = 0
      end if

      return
      end
      subroutine s_to_r8 ( s, r8 )

c*********************************************************************72
c
cc S_TO_R8 reads an R8 value from a string.
c
c  Discussion:
c
c    An "R8" value is simply a real number to be stored as a
c    variable of type "double precision".
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
c    S                 R8
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
c    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2013
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
c    Output, double precision R8, the value read from the string.
c
      implicit none

      character c
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer length
      integer ndig
      double precision r8
      double precision rbot
      double precision rexp
      double precision rtop
      character * ( * ) s
      integer s_length
      character TAB
      parameter ( TAB = char ( 9 ) )

      s_length = len_trim ( s )

      ierror = 0
      r8 = 0.0D+00
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

        if ( s_length .lt. length + 1 ) then
          go to 20
        end if

        c = s(length+1:length+1)
c
c  Blank character.
c
        if ( c .eq. ' ' .or. c .eq. TAB ) then

          if ( ihave .eq. 2 ) then

          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            iterm = 1
          else if ( 1 < ihave ) then
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
        else if ( c .eq. 'E' .or. c .eq. 'e' .or. 
     &            c .eq. 'D' .or. c .eq. 'd' ) then

          if ( ihave .lt. 6 ) then
            ihave = 6
          else
            iterm = 1
          end if
c
c  Digit.
c
        else if (  ihave .lt. 11 .and. lle ( '0', c ) 
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

          ndig = ichar ( c ) - 48

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
c  entire string, then we're done, and LENGTH is equal to S_LENGTH.
c
      if ( iterm .ne. 1 .and. length + 1 .eq. s_length ) then
        length = s_length
      end if
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7!
c
      if ( ihave .eq. 1 .or. ihave .eq. 2 .or. ihave .eq. 6 .or. 
     &  ihave .eq. 7 ) then
        ierror = ihave
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_TO_R8 - Serious errorc'
        write ( *, '(a)' ) '  Illegal or nonnumeric input:'
        write ( *, '(a)' ) '    ' // trim ( s )
        stop 1
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

      r8 = dble ( isgn ) * rexp * rtop / rbot

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
