      subroutine diffusivity_2d_bnt ( dc0, omega, n, x, y, dc )

c*********************************************************************72
c
cc DIFFUSIVITY_2D_BNT evaluates a 2D stochastic diffusivity function.
c
c  Discussion:
c
c    The 2D diffusion equation has the form
c
c      - Del ( DC(X,Y) Del U(X,Y) ) = F(X,Y)
c
c    where DC(X,Y) is a function called the diffusivity.
c
c    In the stochastic version of the problem, the diffusivity function
c    includes the influence of stochastic parameters:
c
c      - Del ( DC(X,Y;OMEGA) Del U(X,Y;OMEGA) ) = F(X,Y).
c
c    In this function, the domain is the rectangle [-1.5,0]x[-0.4,0.8].
c
c    The four stochastic parameters OMEGA(1:4) are assumed to be independent
c    identically distributed random variables with mean value zero and
c    variance 1.  The distribution is typically taken to be Gaussian or
c    uniform.
c
c    A collocation approach to this problem would then use the roots of
c    Hermite or Legendre polynomials.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Ivo Babuska, Fabio Nobile, Raul Tempone,
c    A stochastic collocation method for elliptic partial differential equations
c    with random input data,
c    SIAM Journal on Numerical Analysis,
c    Volume 45, Number 3, 2007, pages 1005-1034.
c
c  Parameters:
c
c    Input, double precision DC0, the constant term in the expansion of the
c    diffusion coefficient.  Take DC0 = 10.
c
c    Input, double precision OMEGA(4), the stochastic parameters.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision X(N), Y(N), the points where the diffusion
c    coefficient is to be evaluated.
c
c    Output, double precision DC(N), the value of the diffusion coefficient
c    at (X,Y).
c
      implicit none

      integer n

      double precision arg(n)
      double precision dc(n)
      double precision dc0
      integer i
      double precision omega(4)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x(n)
      double precision y(n)

      do i = 1, n
        arg(i) = omega(1) * cos ( pi * x(i) )
     &         + omega(2) * sin ( pi * x(i) )
     &         + omega(3) * cos ( pi * y(i) )   
     &         + omega(4) * sin ( pi * y(i) )
      end do

      do i = 1, n
        arg(i)= exp ( - 0.125D+00 ) * arg(i)
      end do

      do i = 1, n
        dc(i) = dc0 + exp ( arg(i) )
      end do

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
      subroutine interior ( omega, nx, ny, x, y, f, n, a, rhs )

c*********************************************************************72
c
cc INTERIOR sets up the matrix and right hand side at interior nodes.
c
c  Discussion:
c
c    Nodes are assigned a single index K, which increases as:
c
c    (NY-1)*NX+1  (NY-1)*NX+2  ...  NY * NX
c           ....         ....  ...    .....
c           NX+1         NX+2  ...   2 * NX
c              1            2  ...       NX
c
c    Therefore, the neighbors of an interior node numbered C are
c
c             C+NY
c              |
c      C-1 --- C --- C+1
c              |
c             C-NY
c
c    If we number rows from bottom I = 1 to top I = NY
c    and columns from left J = 1 to right J = NX, then the relationship
c    between the single index K and the row and column indices I and J is:
c      K = ( I - 1 ) * NX + J
c    and
c      J = 1 + mod ( K - 1, NX )
c      I = 1 + ( K - J ) / NX
c      
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision OMEGA(4), the stochastic coefficients.
c
c    Input, integer NX, NY, the number of grid points in X and Y.
c
c    Input, double precision X(NX), Y(NY), the coordinates of grid lines.
c
c    Input, double precision function F ( X, Y ), evaluates the heat 
c    source term.
c
c    Input, integer N, the number of nodes.
c
c    Output, double precision A(N,N), the system matrix, with the entries for 
c    the interior nodes filled in.
c
c    Output, double precision RHS(N), the system right hand side, with the 
c    entries for the interior nodes filled in.
c
      implicit none

      integer n
      integer nx
      integer ny

      double precision a(n,n)
      double precision dc0
      double precision dce
      double precision dcn
      double precision dcs
      double precision dcw
      double precision dx
      double precision dy
      double precision f
      external f
      integer ic
      integer in
      integer is
      integer jc
      integer je
      integer jw
      integer kc
      integer ke
      integer kn
      integer ks
      integer kw
      double precision omega(4)
      double precision rhs(n)
      double precision x(nx)
      double precision xce
      double precision xcw
      double precision y(ny)
      double precision ycn
      double precision ycs

      dc0 = 1.0D+00
c
c  For now, assume X and Y are equally spaced.
c
      dx = x(2) - x(1)
      dy = y(2) - y(1)

      do ic = 2, ny - 1
        do jc = 2, nx - 1

          in = ic + 1
          is = ic - 1
          je = jc + 1
          jw = jc - 1

          kc = ( ic - 1 ) * nx + jc
          ke = kc + 1
          kw = kc - 1
          kn = kc + nx
          ks = kc - nx

          xce = 0.5D+00 * ( x(jc) + x(je) )
          call diffusivity_2d_bnt ( dc0, omega, 1, xce,   y(ic), dce )
          xcw = 0.5D+00 * ( x(jc) + x(jw) )
          call diffusivity_2d_bnt ( dc0, omega, 1, xcw,   y(ic), dcw )
          ycn = 0.5D+00 * ( y(ic) + y(in) )
          call diffusivity_2d_bnt ( dc0, omega, 1, x(jc), ycn,   dcn )
          ycs = 0.5D+00 * ( y(ic) + y(is) )
          call diffusivity_2d_bnt ( dc0, omega, 1, x(jc), ycs,   dcs )

          a(kc,kc) = ( dce + dcw ) / dx / dx + ( dcn + dcs ) / dy / dy
          a(kc,ke) = - dce         / dx / dx
          a(kc,kw) =       - dcw   / dx / dx
          a(kc,kn) =                           - dcn         / dy / dy
          a(kc,ks) =                                 - dcs   / dy / dy

          rhs(kc) = f ( x(jc), y(ic) )

        end do
      end do

      return
      end
      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a pseudorandom R8 scaled to [0,1].
c
c  Discussion:
c
c    This routine implements the recursion
c
c      seed = 16807 * seed mod ( 2^31 - 1 )
c      r8_uniform_01 = seed / ( 2^31 - 1 )
c
c    The integer arithmetic never requires more than 32 bits,
c    including a sign bit.
c
c    If the initial seed is 12345, then the first three computations are
c
c      Input     Output      R8_UNIFORM_01
c      SEED      SEED
c
c         12345   207482415  0.096616
c     207482415  1790989824  0.833995
c    1790989824  2035175616  0.947702
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
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
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley Interscience, page 95, 1998.
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
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      double precision r8_uniform_01
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine r8mat_fs ( n, a, b, info )

c*********************************************************************72
c
cc R8MAT_FS factors and solves a system with one right hand side.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
c
c    This routine differs from R8MAT_FSS in two ways:
c    * only one right hand side is allowed;
c    * the input matrix A is not modified.
c
c    This routine uses partial pivoting, but no pivot vector is required.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input, double precision A(N,N), the coefficient matrix.
c
c    Input/output, double precision B(N).
c    On input, B is the right hand side of the linear system.
c    On output, B is the solution of the linear system.
c
c    Output, integer INFO, singularity flag.
c    0, no singularity detected.
c    nonzero, the factorization failed on the INFO-th step.
c
      implicit none

      integer n
      integer nb

      double precision a(n,n)
      double precision a2(n,n)
      double precision b(n)
      integer i
      integer info
      integer ipiv
      integer j
      integer jcol
      integer k
      double precision piv
      double precision temp
c
c  Copy the matrix.
c
      do j = 1, n
        do i = 1, n
          a2(i,j) = a(i,j)
        end do
      end do

      info = 0

      do jcol = 1, n
c
c  Find the maximum element in column I.
c
        piv = abs ( a2(jcol,jcol) )
        ipiv = jcol
        do i = jcol + 1, n
          if ( piv .lt. abs ( a2(i,jcol) ) ) then
            piv = abs ( a2(i,jcol) )
            ipiv = i
          end if
        end do

        if ( piv .eq. 0.0D+00 ) then
          info = jcol
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8MAT_FS - Fatal error!'
          write ( *, '(a,i8)' ) '  Zero pivot on step ', info
          return
        end if
c
c  Switch rows JCOL and IPIV, and B.
c
        if ( jcol .ne. ipiv ) then

          do j = 1, n
            temp = a2(jcol,j)
            a2(jcol,j) = a2(ipiv,j)
            a2(ipiv,j) = temp
          end do

          temp = b(jcol)
          b(jcol) = b(ipiv)
          b(ipiv) = temp

        end if
c
c  Scale the pivot row.
c
        do j = jcol + 1, n
          a2(jcol,j) = a2(jcol,j) / a2(jcol,jcol)
        end do
        b(jcol) = b(jcol) / a2(jcol,jcol)
        a2(jcol,jcol) = 1.0D+00
c
c  Use the pivot row to eliminate lower entries in that column.
c
        do i = jcol + 1, n
          if ( a2(i,jcol) .ne. 0.0D+00 ) then
            temp = - a2(i,jcol)
            a2(i,jcol) = 0.0D+00
            do j = jcol + 1, n
              a2(i,j) = a2(i,j) + temp * a2(jcol,j)
            end do
            b(i) = b(i) + temp * b(jcol)
          end if
        end do

      end do
c
c  Back solve.
c
      do k = n, 2, -1
        do i = 1, k - 1
          b(i) = b(i) - a2(i,k) * b(k)
        end do
      end do

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
      subroutine r8vec_mesh_2d ( nx, ny, xvec, yvec, xmat, ymat )

c*********************************************************************72
c
cc R8VEC_MESH_2D creates a 2D mesh from X and Y vectors.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    NX = 2
c    XVEC = ( 1, 2, 3 )
c    NY = 3
c    YVEC = ( 4, 5 )
c
c    XMAT = (
c      1, 2, 3
c      1, 2, 3 )
c
c    YMAT = (
c      4, 4, 4
c      5, 5, 5 ) 
c    
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 2013
c
c  Parameters:
c
c    Input, integer NX, NY, the number of X and Y values.
c
c    Input, double precision XVEC(NX), YVEC(NY), the X and Y coordinate
c    values.
c
c    Output, double precision XMAT(NX,NY), YMAT(NX,NY), the coordinate
c    values of points on an NX by NY mesh.
c
      implicit none

      integer nx
      integer ny

      integer i
      integer j
      double precision xmat(nx,ny)
      double precision xvec(nx)
      double precision ymat(nx,ny)
      double precision yvec(ny)

      do j = 1, ny
        do i = 1, nx
          xmat(i,j) = xvec(i)
        end do
      end do

      do j = 1, ny
        do i = 1, nx
          ymat(i,j) = yvec(j)
        end do
      end do

      return
      end
      subroutine r8vec_normal_01 ( n, seed, x )

c*********************************************************************72
c
cc R8VEC_NORMAL_01 returns a unit pseudonormal R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The standard normal probability distribution function (PDF) has
c    mean 0 and standard deviation 1.
c
c    This routine can generate a vector of values on one call.  It
c    has the feature that it should provide the same results
c    in the same order no matter how we break up the task.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values desired.  If N is negative,
c    then the code will flush its internal memory; in particular,
c    if there is a saved value to be used on the next call, it is
c    instead discarded.  This is useful if the user has reset the
c    random number seed, for instance.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision X(N), a sample of the standard normal PDF.
c
c  Local parameters:
c
c    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
c    X that we need to compute.  This starts off as 1:N, but is adjusted
c    if we have a saved value that can be immediately stored in X(1),
c    and so on.
c
      implicit none

      integer n

      integer i
      integer m
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r(2)
      double precision r8_uniform_01
      integer seed
      double precision x(n)
      integer x_hi_index
      integer x_lo_index
c
c  Record the range of X we need to fill in.
c
      x_lo_index = 1
      x_hi_index = n
c
c  Maybe we don't need any more values.
c
      if ( x_hi_index - x_lo_index + 1 .eq. 1 ) then

        r(1) = r8_uniform_01 ( seed )

        if ( r(1) .eq. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8VEC_NORMAL_01 - Fatal error!'
          write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
          stop
        end if

        r(2) = r8_uniform_01 ( seed )

        x(x_hi_index) =
     &           sqrt ( -2.0D+00 * log ( r(1) ) )
     &           * cos ( 2.0D+00 * pi * r(2) )
c
c  If we require an even number of values, that's easy.
c
      else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) .eq. 0 ) then

        do i = x_lo_index, x_hi_index, 2

          call r8vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * cos ( 2.0D+00 * pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * sin ( 2.0D+00 * pi * r(2) )

        end do
c
c  If we require an odd number of values, we generate an even number,
c  and handle the last pair specially, storing one in X(N), and
c  saving the other for later.
c
      else

        do i = x_lo_index, x_hi_index - 1, 2

          call r8vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * cos ( 2.0D+00 * pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * sin ( 2.0D+00 * pi * r(2) )

        end do

        call r8vec_uniform_01 ( 2, seed, r )

        x(n) = sqrt ( -2.0D+00 * log ( r(1) ) )
     &    * cos ( 2.0D+00 * pi * r(1) )

      end if

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
      subroutine stochastic_heat2d ( omega, nx, ny, x, y, f, u )

c*********************************************************************72
c
cc STOCHASTIC_HEAT2D solves the steady 2D heat equation.
c
c  Discussion:
c
c    Nodes are assigned a singled index K, which increases as:
c
c    (NY-1)*NX+1  (NY-1)*NX+2  ...  NY * NX
c           ....         ....  ...    .....
c           NX+1         NX+2  ...   2 * NX
c              1            2  ...       NX
c
c    Therefore, the neighbors of an interior node numbered C are
c
c             C+NY
c              |
c      C-1 --- C --- C+1
c              |
c             C-NY
c
c    Nodes on the lower boundary satisfy:
c      1 <= K <= NX
c    Nodes on the upper boundary satisfy:
c      (NY-1)*NX+1 <= K <= NY * NX
c    Nodes on the left boundary satisfy:
c      mod ( K, NX ) = 1
c    Nodes on the right boundary satisfy:
c      mod ( K, NX ) = 0
c
c    If we number rows from bottom I = 1 to top I = NY
c    and columns from left J = 1 to right J = NX, we have
c      K = ( I - 1 ) * NX + J
c    and
c      J = 1 + mod ( K - 1, NX )
c      I = 1 + ( K - J ) / NX
c      
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision OMEGA(4), the stochastic coefficients.
c
c    Input, integer NX, NY, the number of grid points in X and Y.
c
c    Input, double precision X(NX), Y(NY), the coordinates of grid lines.
c
c    Input, double precision function F ( X, Y ), evaluates the heat 
c    source term.
c
c    Output, double precision U(NX,NY), the approximation to the solution at 
c    the grid points.
c
      implicit none

      integer nx
      integer ny

      double precision a(nx*ny,nx*ny)
      double precision f
      external f
      integer info
      integer n
      double precision omega(4)
      double precision u(nx,ny)
      double precision x(nx)
      double precision y(ny)
c
c  Set the total number of unknowns.
c
      n = nx * ny
c
c  Define the matrix at interior points.
c
      call interior ( omega, nx, ny, x, y, f, n, a, u )
c
c  Handle boundary conditions.
c
      call boundary ( nx, ny, x, y, n, a, u )
c
c  Solve the linear system.
c
      call r8mat_fs ( n, a, u, info )

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
