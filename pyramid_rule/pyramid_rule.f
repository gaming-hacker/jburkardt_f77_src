      program main

c*********************************************************************72
c
cc MAIN is the main program for PYRAMID_RULE.
c
c  Discussion:
c
c    This program computes a quadrature rule for a pyramid
c    and writes it to a file.
c
c    The user specifies:
c    * the LEGENDRE_ORDER (number of points in the X and Y dimensions)
c    * the JACOBI_ORDER (number of points in the Z dimension)
c    * FILENAME, the root name of the output files.
c
c    The integration region is:
c
c      - ( 1 - Z ) .le. X .le. 1 - Z
c      - ( 1 - Z ) .le. Y .le. 1 - Z
c                0 .le. Z .le. 1.
c
c    When Z is zero, the integration region is a square lying in the (X,Y)
c    plane, centered at (0,0,0) with "radius" 1.  As Z increases to 1, the
c    radius of the square diminishes, and when Z reaches 1, the square has
c    contracted to the single point (0,0,1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer arg_num
      character * ( 255 ) filename
      integer iarg
      integer iargc
      integer ierror
      integer jacobi_order
      integer last
      integer legendre_order
      character * ( 255 ) string

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRAMID_RULE'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Compute a quadrature rule for approximating'
      write ( *, '(a)' ) '  the integral of a function over a pyramid.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The user specifies:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  LEGENDRE_ORDER, the order of the Legendre rule for X and Y.'
      write ( *, '(a)' ) 
     &  '  JACOBI_ORDER, the order of the Jacobi rule for Z,'
      write ( *, '(a)' ) 
     &  '  FILENAME, the prefix of the three output files:'
      write ( *, '(a)' ) '    filename_w.txt - the weight file'
      write ( *, '(a)' ) '    filename_x.txt - the abscissa file.'
      write ( *, '(a)' ) '    filename_r.txt - the region file.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Get the Legendre order.
c
      if ( 1 .le. arg_num ) then

        iarg = 1
        call getarg ( iarg, string )
        call s_to_i4 ( string, legendre_order, ierror, last )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Enter the Legendre rule order:'
        read ( *, * ) legendre_order

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  The requested Legendre order of the rule is ', legendre_order
c
c  Get the Jacobi order.
c
      if ( 2 .le. arg_num ) then

        iarg = 2
        call getarg ( iarg, string )
        call s_to_i4 ( string, jacobi_order, ierror, last )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Enter the Jacobi rule order:'
        read ( *, * ) jacobi_order

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  The requested Jacobi order of the rule is', jacobi_order
c
c  Get the output option or quadrature file root name:
c
      if ( 3 .le. arg_num ) then

        iarg = 3
        call getarg ( iarg, filename )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Enter the "root name" of the quadrature files.'

        read ( *, '(a)' ) filename

      end if

      call pyramid_handle ( legendre_order, jacobi_order, filename )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRAMID_RULE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
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
      subroutine jacobi_ss_compute ( n, alpha, beta, x, w )

c*********************************************************************72
c
cc JACOBI_SS_COMPUTE computes a Gauss-Jacobi quadrature rule.
c
c  Discussion:
c
c    The integral:
c
c      integral ( -1 .le. x .le. 1 ) (1-x)^alpha * (1+x)^beta * f(x) dx
c
c    The quadrature rule:
c
c      sum ( 1 .le. i .le. n ) w(i) * f ( x(i) )
c
c    Thanks to Xu Xiang of Fudan University for pointing out that
c    an earlier implementation of this routine was incorrect!
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 May 2007
c
c  Author:
c
c    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Arthur Stroud, Don Secrest,
c    Gaussian Quadrature Formulas,
c    Prentice Hall, 1966,
c    LC: QA299.4G3S7.
c
c  Parameters:
c
c    Input, integer N, the order.
c
c    Input, double precision ALPHA, BETA, the exponents of (1-X) and
c    (1+X) in the quadrature rule.  For simple Gauss-Legendre quadrature,
c    set ALPHA = BETA = 0.0.  -1.0 .lt. ALPHA and -1.0 .lt. BETA are required.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision alpha
      double precision an
      double precision b(n)
      double precision beta
      double precision bn
      double precision c(n)
      double precision cc
      double precision delta
      double precision dp2
      integer i
      double precision p1
      double precision r1
      double precision r2
      double precision r3
      double precision r8_gamma
      double precision r8vec_product
      double precision w(n)
      double precision x(n)
      double precision xval
c
c  Check ALPHA and BETA.
c
      if ( alpha .le. -1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'JACOBI_SS_COMPUTE - Fatal error!'
        write ( *, '(a)' ) '  -1.0 .lt. ALPHA is required.'
        stop 1
      end if

      if ( beta .le. -1.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'JACOBI_SS_COMPUTE - Fatal error!'
        write ( *, '(a)' ) '  -1.0 .lt. BETA is required.'
        stop 1
      end if
c
c  Set the recursion coefficients.
c
      do i = 1, n

        if ( alpha + beta .eq. 0.0D+00 .or. 
     &    beta - alpha .eq. 0.0D+00 ) then

          b(i) = 0.0D+00

        else

          b(i) = ( alpha + beta ) * ( beta - alpha ) / 
     &          ( ( alpha + beta + dble ( 2 * i ) ) 
     &          * ( alpha + beta + dble ( 2 * i - 2 ) ) )

        end if

        if ( i .eq. 1 ) then

          c(i) = 0.0D+00

        else

          c(i) = 4.0D+00 * dble ( i - 1 ) 
     &          * ( alpha + dble ( i - 1 ) ) 
     &          * ( beta + dble ( i - 1 ) ) 
     &          * ( alpha + beta + dble ( i - 1 ) ) / 
     &          ( ( alpha + beta + dble ( 2 * i - 1 ) ) 
     &          * ( alpha + beta + dble ( 2 * i - 2 ) )**2 
     &          * ( alpha + beta + dble ( 2 * i - 3 ) ) )

        end if

      end do

      delta = r8_gamma ( alpha        + 1.0D+00 ) 
     &      * r8_gamma (         beta + 1.0D+00 ) 
     &      / r8_gamma ( alpha + beta + 2.0D+00 )

      cc = delta * 2.0D+00**( alpha + beta + 1.0D+00 ) 
     &  * r8vec_product ( n - 1, c(2) )

      do i = 1, n

        if ( i .eq. 1 ) then

          an = alpha / dble ( n )
          bn = beta / dble ( n )

          r1 = ( 1.0D+00 + alpha ) 
     &      * ( 2.78D+00 / ( 4.0D+00 + dble ( n * n ) ) 
     &      + 0.768D+00 * an / dble ( n ) )

          r2 = 1.0D+00 + 1.48D+00 * an + 0.96D+00 * bn 
     &      + 0.452D+00 * an * an + 0.83D+00 * an * bn

          xval = ( r2 - r1 ) / r2

        else if ( i .eq. 2 ) then

          r1 = ( 4.1D+00 + alpha ) / 
     &      ( ( 1.0D+00 + alpha ) * ( 1.0D+00 + 0.156D+00 * alpha ) )

          r2 = 1.0D+00 + 0.06D+00 * ( dble ( n ) - 8.0D+00 ) * 
     &      ( 1.0D+00 + 0.12D+00 * alpha ) / dble ( n )

          r3 = 1.0D+00 + 0.012D+00 * beta * 
     &      ( 1.0D+00 + 0.25D+00 * abs ( alpha ) ) / dble ( n )

          xval = xval - r1 * r2 * r3 * ( 1.0D+00 - xval )

        else if ( i .eq. 3 ) then

          r1 = ( 1.67D+00 + 0.28D+00 * alpha ) 
     &      / ( 1.0D+00 + 0.37D+00 * alpha )

          r2 = 1.0D+00 + 0.22D+00 * ( dble ( n ) - 8.0D+00 ) 
     &      / dble ( n )

          r3 = 1.0D+00 + 8.0D+00 * beta / 
     &      ( ( 6.28D+00 + beta ) * dble ( n * n ) )

          xval = xval - r1 * r2 * r3 * ( x(1) - xval )

        else if ( i .lt. n - 1 ) then

          xval = 3.0D+00 * x(i-1) - 3.0D+00 * x(i-2) + x(i-3)

        else if ( i .eq. n - 1 ) then

          r1 = ( 1.0D+00 + 0.235D+00 * beta ) 
     &      / ( 0.766D+00 + 0.119D+00 * beta )

          r2 = 1.0D+00 / ( 1.0D+00 + 0.639D+00 
     &      * ( dble ( n ) - 4.0D+00 ) 
     &      / ( 1.0D+00 + 0.71D+00 * ( dble ( n ) - 4.0D+00 ) ) )

          r3 = 1.0D+00 / ( 1.0D+00 + 20.0D+00 * alpha 
     &      / ( ( 7.5D+00 + alpha ) * dble ( n * n ) ) )

          xval = xval + r1 * r2 * r3 * ( xval - x(i-2) )

        else if ( i .eq. n ) then

          r1 = ( 1.0D+00 + 0.37D+00 * beta ) 
     &      / ( 1.67D+00 + 0.28D+00 * beta )

          r2 = 1.0D+00 / 
     &      ( 1.0D+00 + 0.22D+00 * ( dble ( n ) - 8.0D+00 ) 
     &      / dble ( n ) )

          r3 = 1.0D+00 / ( 1.0D+00 + 8.0D+00 * alpha / 
     &      ( ( 6.28D+00 + alpha ) * dble ( n * n ) ) )

          xval = xval + r1 * r2 * r3 * ( xval - x(i-2) )

        end if

        call jacobi_ss_root ( xval, n, alpha, beta, dp2, p1, b, c )

        x(i) = xval
        w(i) = cc / ( dp2 * p1 )

      end do
c
c  Reverse the data.
c
      call r8vec_reverse ( n, x )
      call r8vec_reverse ( n, w )

      return
      end
      subroutine jacobi_ss_recur ( p2, dp2, p1, x, n, alpha, beta, 
     &  b, c )

c*********************************************************************72
c
cc JACOBI_SS_RECUR finds the value and derivative of a Jacobi polynomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 1998
c
c  Author:
c
c    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Arthur Stroud, Don Secrest,
c    Gaussian Quadrature Formulas,
c    Prentice Hall, 1966,
c    LC: QA299.4G3S7.
c
c  Parameters:
c
c    Output, double precision P2, the value of J(N)(X).
c
c    Output, double precision DP2, the value of J'(N)(X).
c
c    Output, double precision P1, the value of J(N-1)(X).
c
c    Input, double precision X, the point at which polynomials are evaluated.
c
c    Input, integer N, the order of the polynomial.
c
c    Input, double precision ALPHA, BETA, the exponents of (1-X) and
c    (1+X) in the quadrature rule.
c
c    Input, double precision B(N), C(N), the recursion coefficients.
c
      implicit none

      integer n

      double precision alpha
      double precision b(n)
      double precision beta
      double precision c(n)
      double precision dp0
      double precision dp1
      double precision dp2
      integer i
      double precision p0
      double precision p1
      double precision p2
      double precision x

      p1 = 1.0D+00
      dp1 = 0.0D+00

      p2 = x + ( alpha - beta ) / ( alpha + beta + 2.0D+00 )
      dp2 = 1.0D+00

      do i = 2, n

        p0 = p1
        dp0 = dp1

        p1 = p2
        dp1 = dp2

        p2 = ( x - b(i) ) * p1 - c(i) * p0
        dp2 = ( x - b(i) ) * dp1 + p1 - c(i) * dp0

      end do

      return
      end
      subroutine jacobi_ss_root ( x, n, alpha, beta, dp2, p1, b, c )

c*********************************************************************72
c
cc JACOBI_SS_ROOT improves an approximate root of a Jacobi polynomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 December 2000
c
c  Author:
c
c    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Arthur Stroud, Don Secrest,
c    Gaussian Quadrature Formulas,
c    Prentice Hall, 1966,
c    LC: QA299.4G3S7.
c
c  Parameters:
c
c    Input/output, double precision X, the approximate root, which
c    should be improved on output.
c
c    Input, integer N, the order of the polynomial.
c
c    Input, double precision ALPHA, BETA, the exponents of (1-X) and
c    (1+X) in the quadrature rule.
c
c    Output, double precision DP2, the value of J'(N)(X).
c
c    Output, double precision P1, the value of J(N-1)(X).
c
c    Input, double precision B(N), C(N), the recursion coefficients.
c
      implicit none

      integer n

      double precision alpha
      double precision b(n)
      double precision beta
      double precision c(n)
      double precision d
      double precision dp2
      double precision eps
      double precision p1
      double precision p2
      double precision r8_epsilon
      integer step
      integer step_max
      parameter ( step_max = 10 )
      double precision x

      eps = r8_epsilon ( )

      do step = 1, step_max

        call jacobi_ss_recur ( p2, dp2, p1, x, n, alpha, beta, b, c )

        d = p2 / dp2
        x = x - d

        if ( abs ( d ) .le. eps * ( abs ( x ) + 1.0D+00 ) ) then
          return
        end if

      end do

      return
      end
      subroutine legendre_ss_compute ( n, x, w )

c*********************************************************************72
c
cc LEGENDRE_SS_COMPUTE: Gauss-Legendre quadrature by Stroud-Secrest method.
c
c  Discussion:
c
c    The Stroud and Secrest reference did not print a specific computer program
c    for the Gauss-Legendre case.  Therefore, this code is based on the
c    printed code for the Gauss-Jacobi case, with ALPHA and BETA set to 0.
c    This means that the LEGENDRE_SS_ROOT and LEGENDRE_SS_RECUR routines,
c    while appropriate for this computation, do not use the standard
c    normalization for the Legendre polynomials in which Pn(1) = 1.
c    The unusual scaling does not, however, affect the location of the
c    roots, which is the primary thing of interest.
c
c    The integral:
c
c      integral ( -1 .le. x .le. 1 ) f(x) dx
c
c    The quadrature rule:
c
c      sum ( 1 .le. i .le. n ) w(i) * f ( x(i) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 November 2009
c
c  Author:
c
c    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Arthur Stroud, Don Secrest,
c    Gaussian Quadrature Formulas,
c    Prentice Hall, 1966,
c    LC: QA299.4G3S7.
c
c  Parameters:
c
c    Input, integer N, the order.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision c(n)
      double precision cc
      double precision dp2
      integer i
      double precision p1
      double precision r
      double precision r8vec_product
      double precision w(n)
      double precision x(n)
      double precision xtemp
c
c  Set the recursion coefficients.
c
      do i = 1, n

        c(i) = dble ( ( i - 1 ) * ( i - 1 ) ) / 
     &         dble ( ( 2 * i - 1 ) * ( 2 * i - 3 ) )

      end do

      cc = 2.0D+00 * r8vec_product ( n - 1, c(2) )

      do i = 1, n

        if ( i .eq. 1 ) then

          r = 2.78D+00 / ( 4.0D+00 + dble ( n * n ) )

          xtemp = 1.0D+00 - r

        else if ( i .eq. 2 ) then

          r = 1.0D+00 + 0.06D+00 * dble ( n - 8 ) 
     &      / dble ( n )

          xtemp = xtemp - 4.1D+00 * r * ( 1.0D+00 - xtemp )

        else if ( i .eq. 3 ) then

          r = 1.0D+00 + 0.22D+00 * dble ( n - 8 )  
     &      / dble ( n )

          xtemp = xtemp - 1.67D+00 * r * ( x(1) - xtemp )

        else if ( i .lt. n - 1 ) then

          xtemp = 3.0D+00 * x(i-1) - 3.0D+00 * x(i-2) + x(i-3)

        else if ( i .eq. n - 1 ) then

          r = 1.0D+00 / ( 1.0D+00 + 0.639D+00 * dble ( n - 4 ) 
     &      / ( 1.0D+00 + 0.71D+00 * dble ( n - 4 ) ) )

          xtemp = xtemp + r * ( xtemp - x(i-2) ) / 0.766D+00

        else if ( i .eq. n ) then

          r = 1.0D+00 / ( 1.0D+00 + 0.22D+00 * dble ( n - 8 ) 
     &      / dble ( n ) )

          xtemp = xtemp + r * ( xtemp - x(i-2) ) / 1.67D+00

        end if

        call legendre_ss_root ( xtemp, n, dp2, p1, c )

        x(i) = xtemp
        w(i) = cc / dp2 / p1

      end do
c
c  Reverse the data.
c
      call r8vec_reverse ( n, x )
      call r8vec_reverse ( n, w )

      return
      end
      subroutine legendre_ss_recur ( p2, dp2, p1, x, n, c )

c*********************************************************************72
c
cc LEGENDRE_SS_RECUR: value and derivative of a scaled Legendre polynomial.
c
c  Discussion:
c
c    The Stroud and Secrest reference did not print a specific computer program
c    for the Gauss-Legendre case.  Therefore, this code is based on the
c    printed code for the Gauss-Jacobi case, with ALPHA and BETA set to 0.
c    This means that the LEGENDRE_SS_ROOT and LEGENDRE_SS_RECUR routines,
c    while appropriate for this computation, do not use the standard
c    normalization for the Legendre polynomials in which Pn(1) = 1.
c    The unusual scaling does not, however, affect the location of the
c    roots, which is the primary thing of interest.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2010
c
c  Author:
c
c    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Arthur Stroud, Don Secrest,
c    Gaussian Quadrature Formulas,
c    Prentice Hall, 1966,
c    LC: QA299.4G3S7.
c
c  Parameters:
c
c    Output, double precision P2, the value of L(N)(X).
c
c    Output, double precision DP2, the value of L'(N)(X).
c
c    Output, double precision P1, the value of L(N-1)(X).
c
c    Input, double precision X, the point at which polynomials are evaluated.
c
c    Input, integer N, the order of the polynomial.
c
c    Input, double precision C(N), the recursion coefficients.
c
      implicit none

      integer n

      double precision c(n)
      double precision dp0
      double precision dp1
      double precision dp2
      integer i
      double precision p0
      double precision p1
      double precision p2
      double precision x

      p1 = 1.0D+00
      dp1 = 0.0D+00

      p2 = x
      dp2 = 1.0D+00

      do i = 2, n

        p0 = p1
        dp0 = dp1

        p1 = p2
        dp1 = dp2

        p2 = x * p1 - c(i) * p0
        dp2 = x * dp1 + p1 - c(i) * dp0

      end do

      return
      end
      subroutine legendre_ss_root ( x, n, dp2, p1, c )

c*********************************************************************72
c
cc LEGENDRE_SS_ROOT: improve approximate root of scaled Legendre polynomial.
c
c  Discussion:
c
c    The Stroud and Secrest reference did not print a specific computer program
c    for the Gauss-Legendre case.  Therefore, this code is based on the
c    printed code for the Gauss-Jacobi case, with ALPHA and BETA set to 0.
c    This means that the LEGENDRE_SS_ROOT and LEGENDRE_SS_RECUR routines,
c    while appropriate for this computation, do not use the standard
c    normalization for the Legendre polynomials in which Pn(1) = 1.
c    The unusual scaling does not, however, affect the location of the
c    roots, which is the primary thing of interest.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2010
c
c  Author:
c
c    Original FORTRAN77 version by Arthur Stroud, Don Secrest.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Arthur Stroud, Don Secrest,
c    Gaussian Quadrature Formulas,
c    Prentice Hall, 1966,
c    LC: QA299.4G3S7.
c
c  Parameters:
c
c    Input/output, double precision X, the approximate root, which
c    should be improved on output.
c
c    Input, integer N, the order of the polynomial.
c
c    Output, double precision DP2, the value of L'(N)(X).
c
c    Output, double precision P1, the value of L(N-1)(X).
c
c    Input, double precision C(N), the recursion coefficients.
c
      implicit none

      integer n

      double precision c(n)
      double precision d
      double precision dp2
      double precision eps
      double precision p1
      double precision p2
      double precision r8_epsilon
      integer step
      integer step_max
      parameter ( step_max = 10 )
      double precision x

      eps = r8_epsilon ( )

      do step = 1, step_max

        call legendre_ss_recur ( p2, dp2, p1, x, n, c )

        d = p2 / dp2
        x = x - d

        if ( abs ( d ) .le. eps * ( abs ( x ) + 1.0D+00 ) ) then
          return
        end if

      end do

      return
      end
      subroutine pyramid_handle ( legendre_order, jacobi_order, 
     &  filename )

c*********************************************************************72
c
cc PYRAMID_HANDLE computes the requested pyramid rule and outputs it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer LEGENDRE_ORDER, JACOBI_ORDER, the orders
c    of the component Legendre and Jacobi rules.
c
c    Input, character * ( * ) FILENAME, the rootname for the files,
c    write files 'file_w.txt' and 'file_x.txt', and 'file_r.txt', weights,
c    abscissas, and region.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )

      character * ( * )  filename
      character * ( 255 ) filename_r
      character * ( 255 ) filename_w
      character * ( 255 ) filename_x
      integer i
      integer j
      double precision jacobi_alpha
      double precision jacobi_beta
      integer jacobi_order
      double precision jacobi_w(jacobi_order)
      double precision jacobi_x(jacobi_order)
      integer k
      integer l
      integer legendre_order
      double precision legendre_w(legendre_order)
      double precision legendre_x(legendre_order)
      integer pyramid_order
      double precision pyramid_r(dim_num,5)
      double precision 
     &  pyramid_w(legendre_order*legendre_order*jacobi_order)
      double precision 
     &  pyramid_x(3,legendre_order*legendre_order*jacobi_order)
      double precision volume
      double precision wi
      double precision wj
      double precision wk
      double precision xi
      double precision xj
      double precision xk
c
c  Compute the factor rules.
c
      call legendre_ss_compute ( legendre_order, legendre_x, 
     &  legendre_w )

      jacobi_alpha = 2.0D+00
      jacobi_beta = 0.0D+00

      call jacobi_ss_compute ( jacobi_order, jacobi_alpha, jacobi_beta, 
     &  jacobi_x, jacobi_w )
c
c  Compute the pyramid rule.
c
      pyramid_order = legendre_order * legendre_order * jacobi_order

      volume = 4.0D+00 / 3.0D+00

      l = 0
      do k = 1, jacobi_order
        xk = ( jacobi_x(k) + 1.0D+00 ) / 2.0D+00
        wk = jacobi_w(k) / 2.0D+00
        do j = 1, legendre_order
          xj = legendre_x(j)
          wj = legendre_w(j)
          do i = 1, legendre_order
            xi = legendre_x(i)
            wi = legendre_w(i)
            l = l + 1
            pyramid_w(l) = wi * wj * wk / 4.0D+00 / volume
            pyramid_x(1,l) = xi * ( 1.0D+00 - xk )          
            pyramid_x(2,l) = xj * ( 1.0D+00 - xk )
            pyramid_x(3,l) = xk
          end do
        end do
      end do

      pyramid_r(1:dim_num,1) = (/ -1.0D+00, -1.0D+00, 0.0D+00 /)
      pyramid_r(1:dim_num,2) = (/ +1.0D+00, -1.0D+00, 0.0D+00 /)
      pyramid_r(1:dim_num,3) = (/ -1.0D+00, +1.0D+00, 0.0D+00 /)
      pyramid_r(1:dim_num,4) = (/ +1.0D+00, +1.0D+00, 0.0D+00 /)
      pyramid_r(1:dim_num,5) = (/  0.0D+00,  0.0D+00, 1.0D+00 /)
c
c  Write the rule to files.
c
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

      call r8mat_write ( filename_w, 1,       pyramid_order, pyramid_w )
      call r8mat_write ( filename_x, dim_num, pyramid_order, pyramid_x )
      call r8mat_write ( filename_r, dim_num, 5,             pyramid_r )

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
c    Those for the approximation for 12 <= X are from reference 2.
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
      double precision q(8)
      double precision r8_gamma
      double precision r8_pi
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
      data r8_pi / 3.1415926535897932384626434D+00 /
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
     & -3.08402300119738975254353D+01,
     &  3.15350626979604161529144D+02,
     & -1.01515636749021914166146D+03,
     & -3.10777167157231109440444D+03,
     &  2.25381184209801510330112D+04,
     &  4.75584627752788110767815D+03,
     & -1.34659959864969306392456D+05,
     & -1.15132259675553483497211D+05 /
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

          fact = - r8_pi / sin ( r8_pi * res )
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
c  Evaluate for 12.0 <= argument.
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
      function r8vec_product ( n, v1 )

c*********************************************************************72
c
cc R8VEC_PRODUCT multiplies the entries of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    In FORTRAN90, the system routine PRODUCT should be called
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
c    Output, double precision R8VEC_PRODUCT, the product of the entries.
c
      implicit none

      integer n

      integer i
      double precision r8vec_product
      double precision v1(n)
      double precision value

      value = 1.0D+00
      do i = 1, n
        value = value * v1(i)
      end do

      r8vec_product = value

      return
      end
      subroutine r8vec_reverse ( n, a )

c*********************************************************************72
c
cc R8VEC_REVERSE reverses the elements of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    In FORTRAN90, calling R8VEC_REVERSE is equivalent to
c
c      A(1:N) = A(N:1:-1)
c
c  Example:
c
c    Input:
c
c      N = 5,
c      A = ( 11.0, 12.0, 13.0, 14.0, 15.0 ).
c
c    Output:
c
c      A = ( 15.0, 14.0, 13.0, 12.0, 11.0 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input/output, double precision A(N), the array to be reversed.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision t

      do i = 1, n / 2
        t        = a(i)
        a(i)     = a(n+1-i)
        a(n+1-i) = t
      end do

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
