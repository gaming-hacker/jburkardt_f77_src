      program main

c*********************************************************************72
c
cc MAIN is the main program for TRUNCATED_NORMAL_RULE.
c
c  Discussion:
c
c    This program computes a truncated normal quadrature rule
c    and writes it to a file.
c
c    The user specifies:
c    * option: 0/1/2/3 for none, lower, upper, double truncation.
c    * N, the number of points in the rule;
c    * MU, the mean of the original normal distribution;
c    * SIGMA, the standard deviation of the original normal distribution,
c    * A, the left endpoint (for options 1 or 3)
c    * B, the right endpoint (for options 2 or 3);
c    * FILENAME, the root name of the output files.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 50 )

      double precision a
      integer arg_num
      double precision b
      character * ( 255 ) filename
      integer iarg
      integer iargc
      integer ierror
      integer last
      double precision moment(0:2*n_max)
      double precision mu
      integer n
      integer option
      double precision r(2)
      double precision r8_huge
      double precision sigma
      character ( len = 255 ) string
      double precision w(n_max)
      double precision x(n_max)

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRUNCATED_NORMAL_RULE'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  For the (truncated) Gaussian probability density function'
      write ( *, '(a)' ) 
     &  '    pdf(x) = exp(-0.5*((x-MU)/SIGMA)^2) ' //
     &  '/ SIGMA / sqrt ( 2 * pi )'
      write ( *, '(a)' ) 
     &  '  compute an N-point quadrature rule for approximating'
      write ( *, '(a)' ) '    Integral ( A <= x <= B ) f(x) pdf(x) dx'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  OPTION determines the truncation interval [A,B]:'
      write ( *, '(a)' ) '  0: (-oo,+oo)'
      write ( *, '(a)' ) '  1: [A,+oo)'
      write ( *, '(a)' ) '  2: (-oo,B]'
      write ( *, '(a)' ) '  3: [A,B]'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  The user specifies OPTION, N, MU, SIGMA, A, B and FILENAME.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  FILENAME is used to generate 3 files:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    filename_w.txt - the weight file'
      write ( *, '(a)' ) '    filename_x.txt - the abscissa file.'
      write ( *, '(a)' ) 
     &  '    filename_r.txt - the region file, listing A and B.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
      iarg = 0
c
c  Get OPTION.
c
      iarg = iarg + 1
      if ( iarg .le. arg_num ) then
        call getarg ( iarg, string )
        call s_to_i4 ( string, option, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Enter OPTION, 0/1/2/3:'
        read ( *, * ) option
      end if

      if ( option .lt. 0 .or. 3 .lt. option ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_RULE - Fatal error!'
        write ( *, '(a)' ) '  0 <= OPTION <= 3 was required.'
        stop 1
      end if
c
c  Get N
c
      iarg = iarg + 1
      if ( iarg .le. arg_num ) then
        call getarg ( iarg, string )
        call s_to_i4 ( string, n, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Enter the rule order N:'
        read ( *, * ) n
      end if

      if ( n_max < n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_RULE - Fatal error!'
        write ( *, '(a,i8)' ) '  Program requires N <= N_MAX = ', n_max
        stop 1
      end if
c
c  Get MU.
c
      iarg = iarg + 1
      if ( iarg .le. arg_num ) then
        call getarg ( iarg, string )
        call s_to_r8 ( string, mu, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Enter MU, the mean value of the normal distribution:'
        read ( *, * ) mu
      end if
c
c  Get SIGMA.
c
      iarg = iarg + 1
      if ( iarg .le. arg_num ) then
        call getarg ( iarg, string )
        call s_to_r8 ( string, sigma, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Enter SIGMA, the standard deviation:'
        read ( *, * ) sigma
      end if

      sigma = abs ( sigma )
c
c  Get A.
c
      if ( option .eq. 1 .or. option .eq. 3 ) then
        iarg = iarg + 1
        if ( iarg .le. arg_num ) then
          call getarg ( iarg, string )
          call s_to_r8 ( string, a, ierror, last )
        else
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  Enter the left endpoint A:'
          read ( *, * ) a
        end if
      else
        a = - r8_huge ( )
      end if
c
c  Get B.
c
      if ( option .eq. 2 .or. option .eq. 3 ) then
        iarg = iarg + 1
        if ( iarg .le. arg_num ) then
          call getarg ( iarg, string )
          call s_to_r8 ( string, b, ierror, last )
        else
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) '  Enter the right endpoint B:'
          read ( *, * ) b
        end if
      else
        b = r8_huge ( )
      end if

      if ( b .le. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_RULE - Fatal error!'
        write ( *, '(a)' ) '  A < B required.'
        stop 1
      end if
c
c  Get FILENAME.
c
      iarg = iarg + 1
      if ( iarg .le. arg_num ) then
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
      write ( *, '(a,i8)' ) '  OPTION = ', option
      write ( *, '(a,i8)' ) '  N = ', n
      write ( *, '(a,g14.6)' ) '  MU = ', mu
      write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
      if ( option .eq. 1 .or. option .eq. 3 ) then
        write ( *, '(a,g14.6)' ) '  A = ', a
      else
        write ( *, '(a)' ) '  A = -oo'
      end if
      if ( option .eq. 2 .or. option .eq. 3 ) then
        write ( *, '(a,g14.6)' ) '  B = ', b
      else
        write ( *, '(a)' ) '  B = +oo'
      end if
      write ( *, '(a)' ) '  FILENAME = "' // trim ( filename ) // '".'
!
!  Compute the moments.
!
      if ( option .eq. 0 ) then
        call moments_normal ( 2 * n + 1, mu, sigma, moment )
      else if ( option .eq. 1 ) then
        call moments_truncated_normal_a ( 2 * n + 1, mu, sigma, a, 
     &    moment )
      else if ( option .eq. 2 ) then
        call moments_truncated_normal_b ( 2 * n + 1, mu, sigma, b, 
     &    moment )
      else if ( option .eq. 3 ) then
        call moments_truncated_normal_ab ( 2 * n + 1, mu, sigma, a, b, 
     &    moment )
      end if
c
c  Construct the rule from the moments.
c
      call moment_method ( n, moment, x, w )
c
c  Write the rule to a file.
c
      r(1) = a
      r(2) = b

      call rule_write ( n, x, w, r, filename )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CLENSHAW_CURTIS_RULE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
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
      subroutine jacobi_eigenvalue ( n, a, it_max, v, d, it_num, 
     &  rot_num )

c*********************************************************************72
c
cc JACOBI_EIGENVALUE carries out the Jacobi eigenvalue iteration.
c
c  Discussion:
c
c    This function computes the eigenvalues and eigenvectors of a
c    real symmetric matrix, using Rutishauser's modfications of the classical
c    Jacobi rotation method with threshold pivoting.
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
c    FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(N,N), the matrix, which must be square, real,
c    and symmetric.
c
c    Input, integer IT_MAX, the maximum number of iterations.
c
c    Output, double precision V(N,N), the matrix of eigenvectors.
c
c    Output, double precision D(N), the eigenvalues, in descending order.
c
c    Output, integer IT_NUM, the total number of iterations.
c
c    Output, integer ROT_NUM, the total number of rotations.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision bw(n)
      double precision c
      double precision d(n)
      double precision g
      double precision gapq
      double precision h
      integer i
      integer it_max
      integer it_num
      integer j
      integer k
      integer l
      integer m
      integer p
      integer q
      integer rot_num
      double precision s
      double precision t
      double precision tau
      double precision term
      double precision termp
      double precision termq
      double precision theta
      double precision thresh
      double precision v(n,n)
      double precision w(n)
      double precision zw(n)

      do j = 1, n 
        do i = 1, n 
          v(i,j) = 0.0D+00
        end do
        v(j,j) = 1.0D+00
      end do

      do i = 1, n
        d(i) = a(i,i)
      end do

      do i = 1, n
        bw(i) = d(i)
        zw(i) = 0.0D+00
      end do

      it_num = 0
      rot_num = 0

10    continue

      if ( it_num .lt. it_max ) then

        it_num = it_num + 1
c
c  The convergence threshold is based on the size of the elements in
c  the strict upper triangle of the matrix.
c
        thresh = 0.0D+00
        do j = 1, n
          do i = 1, j - 1
            thresh = thresh + a(i,j) ** 2
          end do
        end do

        thresh = sqrt ( thresh ) / dble ( 4 * n )

        if ( thresh .eq. 0.0D+00 ) then
          go to 20
        end if

        do p = 1, n
          do q = p + 1, n

            gapq = 10.0D+00 * abs ( a(p,q) )
            termp = gapq + abs ( d(p) )
            termq = gapq + abs ( d(q) )
c
c  Annihilate tiny offdiagonal elements.
c
            if ( 4 .lt. it_num .and.
     &           termp .eq. abs ( d(p) ) .and.
     &           termq .eq. abs ( d(q) ) ) then

              a(p,q) = 0.0D+00
c
c  Otherwise, apply a rotation.
c
            else if ( thresh .le. abs ( a(p,q) ) ) then

              h = d(q) - d(p)
              term = abs ( h ) + gapq

              if ( term .eq. abs ( h ) ) then
                t = a(p,q) / h
              else
                theta = 0.5D+00 * h / a(p,q)
                t = 1.0D+00 / 
     &            ( abs ( theta ) + sqrt ( 1.0D+00 + theta * theta ) )
                if ( theta .lt. 0.0D+00 ) then
                  t = - t
                end if
              end if

              c = 1.0D+00 / sqrt ( 1.0D+00 + t * t )
              s = t * c
              tau = s / ( 1.0D+00 + c )
              h = t * a(p,q)
c
c  Accumulate corrections to diagonal elements.
c
              zw(p) = zw(p) - h
              zw(q) = zw(q) + h
              d(p) = d(p) - h
              d(q) = d(q) + h

              a(p,q) = 0.0D+00
c
c  Rotate, using information from the upper triangle of A only.
c
              do j = 1, p - 1
                g = a(j,p)
                h = a(j,q)
                a(j,p) = g - s * ( h + g * tau )
                a(j,q) = h + s * ( g - h * tau )
              end do

              do j = p + 1, q - 1
                g = a(p,j)
                h = a(j,q)
                a(p,j) = g - s * ( h + g * tau )
                a(j,q) = h + s * ( g - h * tau )
              end do

              do j = q + 1, n
                g = a(p,j)
                h = a(q,j)
                a(p,j) = g - s * ( h + g * tau )
                a(q,j) = h + s * ( g - h * tau )
              end do
c
c  Accumulate information in the eigenvector matrix.
c
              do j = 1, n
                g = v(j,p)
                h = v(j,q)
                v(j,p) = g - s * ( h + g * tau )
                v(j,q) = h + s * ( g - h * tau )
              end do

              rot_num = rot_num + 1

            end if

          end do
        end do

        do i = 1, n
          bw(i) = bw(i) + zw(i)
          d(i) = bw(i)
          zw(i) = 0.0D+00
        end do

        go to 10

      end if

20    continue
c
c  Restore upper triangle of input matrix.
c
      do j = 1, n
        do i = 1, j - 1
          a(i,j) = a(j,i)
        end do
      end do
c
c  Ascending sort the eigenvalues and eigenvectors.
c
      do k = 1, n - 1

        m = k

        do l = k + 1, n
          if ( d(l) .lt. d(m) ) then
            m = l
          end if
        end do

        if ( m .ne. k ) then

          t    = d(m)
          d(m) = d(k)
          d(k) = t

          do i = 1, n
            w(i)   = v(i,m)
            v(i,m) = v(i,k)
            v(i,k) = w(i)
          end do

        end if

      end do

      return
      end
      subroutine moment_method ( n, moment, x, w )

c*********************************************************************72
c
cc MOMENT_METHOD computes a quadrature rule by the method of moments.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gene Golub, John Welsch,
c    Calculation of Gaussian Quadrature Rules,
c    Mathematics of Computation,
c    Volume 23, Number 106, April 1969, pages 221-230.
c
c  Parameters:
c
c    Input, integer N, the order of the quadrature rule.
c
c    Input, double precision MOMENT(2*N+1), moments 0 through 2*N.
c
c    Output, double precision X(N), W(N), the points and weights of the
c    quadrature rule.
c
      implicit none

      integer n

      double precision alpha(n)
      double precision beta(n-1)
      logical debug
      integer flag
      double precision h(0:n,0:n)
      integer i
      integer it_max
      integer it_num
      integer j
      double precision jacobi(n,n)
      double precision moment(0:2*n)
      double precision r(n+1,n+1)
      integer rot_num
      double precision v(n,n)
      double precision w(n)
      double precision x(n)

      debug = .false.

      if ( debug ) then
        call r8vec_print ( 2 * n + 1, moment, '  Moments:' )
      end if
c
c  Define the N+1 by N+1 Hankel matrix H(I,J) = moment(I+J).
c
      do i = 0, n
        do j = 0, n
          h(i,j) = moment(i+j)
        end do
      end do

      if ( debug ) then
        call r8mat_print ( n + 1, n + 1, h, '  Hankel matrix:' )
      end if
c
c  Compute R, the upper triangular Cholesky factor of H.
c
      call r8mat_cholesky_factor_upper ( n + 1, h, r, flag )

      if ( flag .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'QUADMOM_PRB01 - Fatal error!'
        write ( *, '(a,i6)' ) 
     &    '  R8MAT_CHOLESKY_FACTOR_UPPER returned FLAG = ', flag
        stop 1
      end if
c
c  Compute ALPHA and BETA from R, using Golub and Welsch's formula.
c
      alpha(1) = r(1,2) / r(1,1)
      do i = 2, n
        alpha(i) = r(i,i+1) / r(i,i) - r(i-1,i) / r(i-1,i-1)
      end do

      do i = 1, n - 1
        beta(i) = r(i+1,i+1) / r(i,i)
      end do
c
c  Compute the points and weights from the moments.
c
      do j = 1, n
        do i = 1, n
          jacobi(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        jacobi(i,i) = alpha(i)
      end do

      do i = 1, n - 1
        jacobi(i,i+1) = beta(i)
        jacobi(i+1,i) = beta(i)
      end do

      if ( debug ) then
        call r8mat_print ( n, n, jacobi, '  The Jacobi matrix:' )
      end if
c
c  Get the eigendecomposition of the Jacobi matrix.
c
      it_max = 100

      call jacobi_eigenvalue ( n, jacobi, it_max, v, x, it_num, 
     &  rot_num )

      if ( debug ) then
        call r8mat_print ( n, n, v, '  Eigenvector' )
      end if

      do j = 1, n
        w(j) = moment(0) * v(1,j) ** 2
      end do

      return
      end
      subroutine moments_normal ( m, mu, sigma, w )

c*********************************************************************72
c
cc MOMENTS_NORMAL returns moments of the standard Normal distribution.
c
c  Discussion:
c
c    pdf(x) = exp ( -((x-mu)/sigma)^2/2 ) / sigma / sqrt ( pi * 2 )
c    mu(k) = integral ( -oo < x < +oo ) x^k pdf(x) dx
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
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of moments desired.
c
c    Input, double precision MU, SIGMA, the mean and standard deviation.
c
c    Output, double precision W(0:M-1), the weighted integrals of X^0
c    through X^(M-1).
c
      implicit none

      integer m

      integer j
      integer j_hi
      integer k
      double precision mu
      double precision r8_choose
      double precision r8_factorial2
      double precision sigma
      double precision t
      double precision w(0:m-1)

      do k = 0, m - 1
        t = 0.0D+00
        j_hi = k / 2
        do j = 0, j_hi
          t = t + r8_choose ( k, 2 * j ) * r8_factorial2 ( 2 * j - 1 )  
     &       * sigma ** ( 2 * j ) * mu ** ( k - 2 * j )
        end do
        w(k) = t
      end do

      return
      end
      subroutine moments_truncated_normal_ab ( m, mu, sigma, a, b, w )

c*********************************************************************72
c
cc MOMENTS_TRUNCATED_NORMAL_AB: moments of the truncated Normal distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of moments desired.
c
c    Input, double precision MU, SIGMA, the mean and standard deviation.
c
c    Input, double precision A, B, the lower and upper truncation limits.
c
c    Output, double precision W(0:M-1), the weighted integrals of X^0
c    through X^(M-1).
c
      implicit none

      integer m

      double precision a
      double precision b
      double precision mu
      integer order
      double precision sigma
      double precision w(0:m-1)

      do order = 0, m - 1
        call truncated_normal_ab_moment ( order, mu, sigma, a, b, 
     &    w(order) )
      end do

      return
      end
      subroutine moments_truncated_normal_a ( m, mu, sigma, a, w )

c*********************************************************************72
c
cc MOMENTS_TRUNCATED_NORMAL_A: moments of the lower truncated Normal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of moments desired.
c
c    Input, double precision MU, SIGMA, the mean and standard deviation.
c
c    Input, double precision A, the lower truncation limit.
c
c    Output, double precision W(0:M-1), the weighted integrals of X^0
c    through X^(M-1).
c
      implicit none

      integer m

      double precision a
      double precision mu
      integer order
      double precision sigma
      double precision w(0:m-1)

      do order = 0, m - 1
        call truncated_normal_a_moment ( order, mu, sigma, a, 
     &    w(order) )
      end do

      return
      end
      subroutine moments_truncated_normal_b ( m, mu, sigma, b, w )

c*********************************************************************72
c
cc MOMENTS_TRUNCATED_NORMAL_B: moments of the upper truncated Normal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of moments desired.
c
c    Input, double precision MU, SIGMA, the mean and standard deviation.
c
c    Input, double precision B, the upper truncation limit.
c
c    Output, double precision W(0:M-1), the weighted integrals of X^0
c    through X^(M-1).
c
      implicit none

      integer m

      double precision b
      double precision mu
      integer order
      double precision sigma
      double precision w(0:m-1)

      do order = 0, m - 1
        call truncated_normal_b_moment ( order, mu, sigma, b, 
     &    w(order) )
      end do

      return
      end
      subroutine normal_01_cdf ( x, cdf )

c*********************************************************************72
c
cc NORMAL_01_CDF evaluates the Normal 01 CDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 1999
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    AG Adams,
c    Algorithm 39,
c    Areas Under the Normal Curve,
c    Computer Journal,
c    Volume 12, pages 197-198, 1969.
c
c  Parameters:
c
c    Input, double precision X, the argument of the CDF.
c
c    Output, double precision CDF, the value of the CDF.
c
      implicit none

      double precision a1
      parameter ( a1 = 0.398942280444D+00 )
      double precision a2
      parameter ( a2 = 0.399903438504D+00 )
      double precision, parameter :: a3 = 5.75885480458D+00
      double precision, parameter :: a4 = 29.8213557808D+00
      double precision, parameter :: a5 = 2.62433121679D+00
      double precision, parameter :: a6 = 48.6959930692D+00
      double precision, parameter :: a7 = 5.92885724438D+00
      double precision, parameter :: b0 = 0.398942280385D+00
      double precision, parameter :: b1 = 3.8052D-08
      double precision, parameter :: b2 = 1.00000615302D+00
      double precision, parameter :: b3 = 3.98064794D-04
      double precision, parameter :: b4 = 1.98615381364D+00
      double precision, parameter :: b5 = 0.151679116635D+00
      double precision, parameter :: b6 = 5.29330324926D+00
      double precision, parameter :: b7 = 4.8385912808D+00
      double precision, parameter :: b8 = 15.1508972451D+00
      double precision, parameter :: b9 = 0.742380924027D+00
      double precision, parameter :: b10 = 30.789933034D+00
      double precision b11
      parameter ( b11 = 3.99019417011D+00 )
      double precision cdf
      double precision q
      double precision x
      double precision y
c
c  |X| .le. 1.28.
c
      if ( abs ( x ) .le. 1.28D+00 ) then

        y = 0.5D+00 * x * x

        q = 0.5D+00 - abs ( x ) * ( a1 - a2 * y / ( y + a3 - a4 
     &    / ( y + a5 + a6 / ( y + a7 ) ) ) )
c
c  1.28 .lt. |X| .le. 12.7
c
      else if ( abs ( x ) .le. 12.7D+00 ) then

        y = 0.5D+00 * x * x

        q = exp ( - y ) * b0 / ( abs ( x ) - b1       + b2 / ( abs ( x )
     & + b3       + b4 / ( abs ( x ) - b5       + b6 / ( abs ( x ) + b7 
     &      - b8 / ( abs ( x ) + b9       + b10 / ( abs ( x ) + b11 ) ) 
     &) ) ) )
c
c  12.7 .lt. |X|
c
      else

        q = 0.0D+00

      end if
c
c  Take account of negative X.
c
      if ( x .lt. 0.0D+00 ) then
        cdf = q
      else
        cdf = 1.0D+00 - q
      end if

      return
      end
      subroutine normal_01_pdf ( x, pdf )

c*********************************************************************72
c
cc NORMAL_01_PDF evaluates the Normal 01 PDF.
c
c  Discussion:
c
c    The Normal 01 PDF is also called the "Standard Normal" PDF, or
c    the Normal PDF with 0 mean and variance 1.
c
c    PDF(X) = exp ( - 0.5 * X^2 ) / sqrt ( 2 * PI )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 December 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the argument of the PDF.
c
c    Output, double precision PDF, the value of the PDF.
c
      implicit none

      double precision pdf
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x

      pdf = exp ( -0.5D+00 * x * x ) / sqrt ( 2.0D+00 * pi )

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
      function r8_factorial ( n )

c*********************************************************************72
c
cc R8_FACTORIAL computes the factorial of N.
c
c  Discussion:
c
c    factorial ( N ) = product ( 1 <= I <= N ) I
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
c  Parameters:
c
c    Input, integer N, the argument of the factorial function.
c    If N is less than 1, the function value is returned as 1.
c
c    Output, double precision R8_FACTORIAL, the factorial of N.
c
      implicit none

      integer i
      integer n
      double precision r8_factorial

      r8_factorial = 1.0D+00

      do i = 1, n
        r8_factorial = r8_factorial * dble ( i )
      end do

      return
      end
      function r8_factorial2 ( n )

c*********************************************************************72
c
cc R8_FACTORIAL2 computes the double factorial function.
c
c  Discussion:
c
c    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
c                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
c
c  Example:
c
c     N Value
c
c     0     1
c     1     1
c     2     2
c     3     3
c     4     8
c     5    15
c     6    48
c     7   105
c     8   384
c     9   945
c    10  3840
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the double factorial
c    function.  If N is less than 1, R8_FACTORIAL2 is returned as 1.0.
c
c    Output, double precision R8_FACTORIAL2, the value.
c
      implicit none

      integer n
      double precision r8_factorial2
      double precision r8_n

      if ( n .lt. 1 ) then
        r8_factorial2 = 1.0D+00
        return
      end if

      r8_n = dble ( n )
      r8_factorial2 = 1.0D+00

10    continue

      if ( 1.0D+00 .lt. r8_n ) then
        r8_factorial2 = r8_factorial2 * r8_n
        r8_n = r8_n - 2.0D+00
        go to 10
      end if

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
      function r8_mop ( i )

c*********************************************************************72
c
cc R8_MOP returns the I-th power of -1 as an R8.
c
c  Discussion:
c
c    An R8 is a double precision real value.
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
c    Input, integer I, the power of -1.
c
c    Output, double precision R8_MOP, the I-th power of -1.
c
      implicit none

      integer i
      double precision r8_mop

      if ( mod ( i, 2 ) .eq. 0 ) then
        r8_mop = + 1.0D+00
      else
        r8_mop = - 1.0D+00
      end if

      return
      end
      subroutine r8mat_cholesky_factor_upper ( n, a, c, flag )

!*********************************************************************72
!
!! R8MAT_CHOLESKY_FACTOR_UPPER: upper Cholesky factor of a symmetric matrix.
!
!  Discussion:
!
!    The matrix must be symmetric and positive semidefinite.
!
!    For a positive semidefinite symmetric matrix A, the Cholesky factorization
!    is an upper triangular matrix R such that:
!
!      A = R * R'
!
!    The lower Cholesky factor is a lower triangular matrix L such that
!
!      A = L * L'
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 August 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of rows and columns of
!    the matrix A.
!
!    Input, double precision A(N,N), the N by N matrix.
!
!    Output, double precision C(N,N), the N by N upper triangular
!    Cholesky factor.
!
!    Output, integer FLAG:
!    0, no error occurred.
!    1, the matrix is not positive definite.
!    2, the matrix is not nonnegative definite.
!
      implicit none

      integer n

      double precision a(n,n)
      double precision c(n,n)
      integer flag
      integer i
      integer j
      integer k
      double precision sum2
      double precision tol

      flag = 0

      do j = 1, n
        do i = 1, n
          c(i,j) = a(i,j)
        end do
      end do

      do j = 1, n

        do i = 1, j - 1
          c(j,i) = 0.0D+00
       end do

        do i = j, n

          sum2 = c(i,j)
          do k = 1, j - 1
            sum2 = sum2 - c(k,j) * c(k,i)
          end do

          if ( i .eq. j ) then
            if ( sum2 .le. 0.0D+00 ) then
              flag = 1
              return
            else
              c(j,i) = sqrt ( sum2 )
            end if
          else
            if ( c(j,j) .ne. 0.0D+00 ) then
              c(j,i) = sum2 / c(j,j)
            else
              c(j,i) = 0.0D+00
            end if
          end if

        end do

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
c    18 February 2010
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
c    Input, character ( len = * ) FILENAME, specifies the output.
c    'filename_w.txt', 'filename_x.txt', 'filename_r.txt' defining weights,
c    abscissas, and region.
c 
      implicit none

      integer n

      character * ( * ) filename
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

      ierror = 0
      istate = 0
      isgn = 1
      ival = 0

      do i = 1, len_trim ( s )

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
c    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)
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

      nchar = len_trim ( s )

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

        if ( nchar .lt. length+1 ) then
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
      subroutine truncated_normal_ab_moment ( order, mu, s, a, b, 
     &  moment )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_AB_MOMENT: moments of the truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Phoebus Dhrymes,
c    Moments of Truncated Normal Distributions,
c    May 2005.
c
c  Parameters:
c
c    Input, integer ORDER, the order of the moment.
c    0 <= ORDER.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c    0.0 < S.
c
c    Input, double precision A, B, the lower and upper truncation limits.
c
c    Output, double precision MOMENT, the moment of the PDF.
c
      implicit none

      double precision a
      double precision a_h
      double precision a_cdf
      double precision a_pdf
      double precision b
      double precision b_h
      double precision b_cdf
      double precision b_pdf
      double precision ir
      double precision irm1
      double precision irm2
      double precision moment
      double precision mu
      integer order
      integer r
      double precision r8_choose
      double precision s

      if ( order .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
        write ( *, '(a)' ) '  ORDER < 0.'
        stop 1
      end if

      if ( s .le. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
        write ( *, '(a)' ) '  S <= 0.0.'
        stop 1
      end if

      if ( b .le. a ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
        write ( *, '(a)' ) '  B <= A.'
        stop 1
      end if

      a_h = ( a - mu ) / s
      call normal_01_pdf ( a_h, a_pdf )
      call normal_01_cdf ( a_h, a_cdf )

      if ( a_cdf .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
        write ( *, '(a)' ) 
     &    '  PDF/CDF ratio fails, because A_CDF is too small.'
        write ( *, '(a,g14.6)' ) '  A_PDF = %g\n', a_pdf
        write ( *, '(a,g14.6)' ) '  A_CDF = %g\n', a_cdf
        stop 1
      end if

      b_h = ( b - mu ) / s
      call normal_01_pdf ( b_h, b_pdf )
      call normal_01_cdf ( b_h, b_cdf )

      if ( b_cdf .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_AB_MOMENT - Fatal error!'
        write ( *, '(a)' ) 
     &    '  PDF/CDF ratio fails, because B_CDF is too small.'
        write ( *, '(a,g14.6)' ) '  B_PDF = %g\n', b_pdf
        write ( *, '(a,g14.6)' ) '  B_CDF = %g\n', b_cdf
        stop 1
      end if

      moment = 0.0D+00
      irm2 = 0.0D+00
      irm1 = 0.0D+00

      do r = 0, order

        if ( r .eq. 0 ) then
          ir = 1.0D+00
        else if ( r .eq. 1 ) then
          ir = - ( b_pdf - a_pdf ) / ( b_cdf - a_cdf )
        else
          ir = dble ( r - 1 ) * irm2 
     &      - ( b_h ** ( r - 1 ) * b_pdf - a_h ** ( r - 1 ) * a_pdf ) 
     &      / ( b_cdf - a_cdf )
        end if

        moment = moment + r8_choose ( order, r ) * mu ** ( order - r ) 
     &    * ( s ** r ) * ir

        irm2 = irm1
        irm1 = ir

      end do

      return
      end
      subroutine truncated_normal_a_moment ( order, mu, s, a, moment )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_A_MOMENT: moments of the lower truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Phoebus Dhrymes,
c    Moments of Truncated Normal Distributions,
c    May 2005.
c
c  Parameters:
c
c    Input, integer ORDER, the order of the moment.
c    0 <= ORDER.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c    0.0 < S.
c
c    Input, double precision A, the lower truncation limit.
c
c    Output, double precision MOMENT, the moment of the PDF.
c
      implicit none

      double precision a
      double precision moment
      double precision mu
      integer order
      double precision r8_mop
      double precision s

      call truncated_normal_b_moment ( order, - mu, s, - a, moment )

      moment = r8_mop ( order ) * moment

      return
      end
      subroutine truncated_normal_b_moment ( order, mu, s, b, moment )

c*********************************************************************72
c
cc TRUNCATED_NORMAL_B_MOMENT: moments of the upper truncated Normal PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Phoebus Dhrymes,
c    Moments of Truncated Normal Distributions,
c    May 2005.
c
c  Parameters:
c
c    Input, integer ORDER, the order of the moment.
c    0 <= ORDER.
c
c    Input, double precision MU, S, the mean and standard deviation of the
c    parent Normal distribution.
c    0.0 < S.
c
c    Input, double precision B, the upper truncation limit.
c
c    Output, double precision MOMENT, the moment of the PDF.
c
      implicit none

      double precision b
      double precision f
      double precision h
      double precision h_cdf
      double precision h_pdf
      double precision ir
      double precision irm1
      double precision irm2
      double precision moment
      double precision mu
      integer order
      integer r
      double precision r8_choose
      double precision s

      if ( order .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MOMENT - Fatal error!'
        write ( *, '(a)' ) '  ORDER < 0.'
        stop 1
      end if

      h = ( b - mu ) / s
      call normal_01_pdf ( h, h_pdf )
      call normal_01_cdf ( h, h_cdf )

      if ( h_cdf .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRUNCATED_NORMAL_B_MOMENT - Fatal error!'
        write ( *, '(a)' ) '  CDF((B-MU)/S) = 0.'
        stop 1
      end if

      f = h_pdf / h_cdf

      moment = 0.0D+00
      irm2 = 0.0D+00
      irm1 = 0.0D+00

      do r = 0, order

        if ( r .eq. 0 ) then
          ir = 1.0D+00
        else if ( r .eq. 1 ) then
          ir = - f
        else
          ir = - h ** ( r - 1 ) * f + dble ( r - 1 ) * irm2
        end if

        moment = moment + r8_choose ( order, r ) * mu ** ( order - r ) 
     &    * ( s ** r ) * ir

        irm2 = irm1
        irm1 = ir

      end do

      return
      end
