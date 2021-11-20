      subroutine diffusivity_1d_xk ( dc0, m, omega, n, x, dc )

c*********************************************************************72
c
cc DIFFUSIVITY_1D_XK evaluates a 1D stochastic diffusivity function.
c
c  Discussion:
c
c    The 1D diffusion equation has the form
c
c      - d/dx ( DC(X) Del U(X) ) = F(X)
c
c    where DC(X) is a function called the diffusivity.
c
c    In the stochastic version of the problem, the diffusivity function
c    includes the influence of stochastic parameters:
c
c      - d/dx ( DC(X;OMEGA) d/dx U(X) ) = F(X).
c
c    In this function, the domain is assumed to be the unit interval [0.1].
c
c
c    For DC0 = 1 and F(X) = 0, with boundary conditions U(0:OMEGA) = 0,
c    U(1;OMEGA) = 1, the exact solution is
c
c    If OMEGA ~= 0:
c
c      U(X;OMEGA) = log ( 1 + OMEGA * X ) / log ( 1 + OMEGA )
c
c    If OMEGA = 0:
c
c      U(X;OMEGA) = X
c
c    In the numerical experiments described in the paper, OMEGA was taken
c    to be a random variable with a Beta, or Uniform, or Gaussian or
c    Poisson or Binomial distribution.
c
c    For the Gaussian and Poisson distributions, the positivity requirement
c    could not be guaranteed, and the experiments were simply made with a
c    "small" variance of 0.1.
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
c  Reference:
c
c    Dongbin Xiu, George Karniadakis,
c    Modeling uncertainty in steady state diffusion problems via
c    generalized polynomial chaos,
c    Computer Methods in Applied Mechanics and Engineering,
c    Volume 191, 2002, pages 4927-4948.
c
c  Parameters:
c
c    Input, double precision DC0, the constant term in the expansion of the
c    diffusion coefficient.
c
c    Input, integer M, the number of stochastic parameters.
c
c    Input, double precision OMEGA(M), the stochastic parameters.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision X(N), the point where the diffusion coefficient
c    is to be evaluated.
c
c    Output, double precision DC(N), the value of the diffusion coefficient
c    at X.
c
      implicit none

      integer m
      integer n

      double precision dc(n)
      double precision dc0
      integer i
      integer k
      double precision omega(m)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision w
      double precision x(n)

      k = 0
      w = 1.0D+00

      do i = 1, n
        dc(i) = 0.0D+00
      end do

10    continue

      if ( k .lt. m ) then

        if ( k .lt. m ) then
          k = k + 1
          do i = 1, n
            dc(i) = dc(i) + omega(k) * sin ( w * pi * x(i) )
          end do
        end if

        if ( k .lt. m ) then
          k = k + 1
          do i = 1, n
            dc(i) = dc(i) + omega(k) * cos ( w * pi * x(i) )
          end do
        end if

        w = w + 1.0D+00

        go to 10

      end if

      do i = 1, n
        dc(i) = exp ( - 0.125D+00 ) * dc(i)
      end do

      do i = 1, n
        dc(i) = dc0 + exp ( dc(i) )
      end do

      return
      end
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
      subroutine diffusivity_2d_elman ( a, cl, dc0, m_1d, omega, n1, n2,
     & x, y, dc )

c*********************************************************************72
c
cc DIFFUSIVITY_2D_ELMAN evaluates a 2D stochastic diffusivity function.
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
c    In this function, the domain is assumed to be the square [-A,+A]x[-A,+A].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Howard Elman, Darran Furnaval,
c    Solving the stochastic steady-state diffusion problem using multigrid,
c    IMA Journal on Numerical Analysis,
c    Volume 27, Number 4, 2007, pages 675-688.
c
c    Roger Ghanem, Pol Spanos,
c    Stochastic Finite Elements: A Spectral Approach,
c    Revised Edition,
c    Dover, 2003,
c    ISBN: 0486428184,
c    LC: TA347.F5.G56.
c
c  Parameters:
c
c    Input, double precision A, the "radius" of the square region.  The region
c    is assumed to be [-A,+A]x[-A,+A].
c    0 < A.
c
c    Input, double precision CL, the correlation length.
c    0 < CL.
c
c    Input, double precision DC0, the constant term in the expansion of the
c    diffusion coefficient.  Take DC0 = 10.
c
c    Input, integer M_1D, the first and second dimensions of the
c    stochastic parameter array.
c
c    Input, double precision OMEGA(M_1D,M_1D), the stochastic parameters.
c
c    Input, integer N1, N2, the dimensions of the X and Y arrays.
c
c    Input, double precision X(N1,N2), Y(N1,N2), the points where the diffusion
c    coefficient is to be evaluated.
c
c    Output, double precision DC(N1,N2), the value of the diffusion
c    coefficient at X.
c
      implicit none

      integer m_1d
      integer n1
      integer n2

      double precision a
      double precision c_1dx(m_1d,n1,n2)
      double precision c_1dy(m_1d,n1,n2)
      double precision cl
      double precision dc(n1,n2)
      double precision dc0
      integer i
      integer i1
      integer i2
      integer j
      integer k
      double precision lambda_1d(m_1d)
      integer m
      double precision omega(m_1d,m_1d)
      double precision theta_1d(m_1d)
      double precision x(n1,n2)
      double precision y(n1,n2)

      m = m_1d * m_1d
c
c  Compute THETA.
c
      call theta_solve ( a, cl, m_1d, theta_1d )
c
c  Compute LAMBDA_1D.
c
      do i = 1, m_1d
        lambda_1d(i) = 2.0D+00 * cl 
     &    / ( 1.0D+00 + cl * cl * theta_1d(i) ** 2 )
      end do
c
c  Compute C_1DX(1:M1D) and C_1DY(1:M1D) at (X,Y).
c
      do k = 1, n2
        do j = 1, n1
          do i = 1, m_1d
            c_1dx(i,j,k) = 0.0D+00
            c_1dy(i,j,k) = 0.0D+00
          end do
        end do
      end do

      i = 0

10    continue

        if ( m_1d .le. i ) then
          go to 20
        end if

        i = i + 1

        do k = 1, n2
          do j = 1, n1

            c_1dx(i,j,k) = cos ( theta_1d(i) * a * x(j,k) )     
     &        / sqrt ( a + sin ( 2.0D+00 * theta_1d(i) * a )
     &        / ( 2.0D+00 * theta_1d(i) ) )

            c_1dy(i,j,k) = cos ( theta_1d(i) * a * y(j,k) )     
     &        / sqrt ( a + sin ( 2.0D+00 * theta_1d(i) * a )
     &        / ( 2.0D+00 * theta_1d(i) ) )

          end do
        end do

        if ( m_1d .le. i ) then
          go to 20
        end if

        i = i + 1

        do k = 1, n2
          do j = 1, n1

            c_1dx(i,j,k) = sin ( theta_1d(i) * a * x(j,k) )     
     &        / sqrt ( a - sin ( 2.0D+00 * theta_1d(i) * a )
     &        / ( 2.0D+00 * theta_1d(i) ) )

            c_1dy(i,j,k) = sin ( theta_1d(i) * a * y(j,k) )     
     &        / sqrt ( a - sin ( 2.0D+00 * theta_1d(i) * a )
     &        / ( 2.0D+00 * theta_1d(i) ) )

          end do
        end do

      go to 10

20    continue
c
c  Evaluate the diffusion coefficient DC at (X,Y).
c
      do k = 1, n2
        do j = 1, n1
          dc(j,k) = dc0
          do i2 = 1, m_1d
            do i1 = 1, m_1d
              dc(j,k) = dc(j,k) 
     &          + sqrt ( lambda_1d(i1) * lambda_1d(i2) )
     &          * c_1dx(i1,j,k) * c_1dy(i2,j,k) * omega(i1,i2)
            end do
          end do
        end do
      end do

      return
      end
      subroutine diffusivity_2d_ntw ( cl, dc0, m, omega, n, x, y, dc )

c*********************************************************************72
c
cc DIFFUSIVITY_2D_NTW evaluates a 2D stochastic diffusivity function.
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
c    In this function, the domain is the rectangle [0,D]x[0,D] where D = 1.
c
c    Note that in this problem the diffusivity has a one-dimensional
c    spatial dependence on X, but not on Y!
c
c    The random variables OMEGA are independent, have zero mean and unit
c    variance, and are uniformly distributed in [-sqrt(3),+sqrt(3)].
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
c    Xiang Ma, Nicholas Zabaras,
c    An adaptive hierarchical sparse grid collocation algorithm for the solution
c    of stochastic differential equations,
c    Journal of Computational Physics,
c    Volume 228, pages 3084-3113, 2009.
c
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
c  Parameters:
c
c    Input, double precision CL, the desired physical correlation length for
c    the coefficient.
c
c    Input, double precision DC0, the constant term in the expansion of the
c    diffusion coefficient.  Take DC0 = 0.5.
c
c    Input, integer M, the number of terms in the expansion.
c
c    Input, double precision OMEGA(M), the stochastic parameters.
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

      integer m
      integer n

      double precision cl
      double precision d
      double precision dc(n)
      double precision dc_arg(n)
      double precision dc0
      integer i
      double precision ihalf_r8
      integer j
      double precision l
      double precision lp
      double precision omega(m)
      double precision phi(n)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x(n)
      double precision y(n)
      double precision zeta
      double precision zeta_arg

      d = 1.0D+00
      lp = max ( d, 2.0D+00 * cl )
      l = cl / lp

      do j = 1, n 
        dc_arg(j) = 1.0D+00 
     &    + omega(1) * sqrt ( sqrt ( pi ) * l / 2.0D+00 )
      end do

      do i = 2, m

        ihalf_r8 = dble ( i / 2 )
        zeta_arg = - ( ihalf_r8 * pi * l ) ** 2 / 8.0D+00
        zeta = sqrt ( sqrt ( pi ) * l ) * exp ( zeta_arg )

        if ( mod ( i, 2 ) .eq. 0 ) then
          do j = 1, n
            phi(j) = sin ( ihalf_r8 * pi * x(j) / lp )
          end do
        else
          do j = 1, n
            phi(j) = cos ( ihalf_r8 * pi * x(j) / lp )
          end do
        end if

        do j = 1, n
          dc_arg(j) = dc_arg(j) + zeta * phi(j) * omega(i)
        end do

      end do

      do j = 1, n
        dc(j) = dc0 + exp ( dc_arg(j) )
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
      function r8mat_max ( m, n, a )

c*********************************************************************72
c
cc R8MAT_MAX returns the maximum entry of an R8MAT.
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
c    19 May 2008
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
c    Output, double precision R8MAT_MAX, the maximum entry of A.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision r8mat_max
      double precision value

      value = a(1,1)
      do j = 1, n
        do i = 1, m
          value = max ( value, a(i,j) )
        end do
      end do

      r8mat_max = value

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
      subroutine r8vec_max ( n, a, amax )

c*********************************************************************72
c
cc R8VEC_MAX returns the maximum value in an R8VEC.
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
c    31 May 2009
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
c    Output, double precision AMAX, the value of the largest entry.
c
      implicit none

      integer n

      double precision a(n)
      double precision amax
      integer i

      amax = a(1)
      do i = 2, n
        amax = max ( amax, a(i) )
      end do

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
c    The Box-Muller method is used, which is efficient, but
c    generates an even number of values each time.  On any call
c    to this routine, an even number of new values are generated.
c    Depending on the situation, one value may be left over.
c    In that case, it is saved for the next call.
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
      subroutine theta_solve ( a, cl, m, theta )

c*********************************************************************72
c
cc THETA_SOLVE solves a pair of transcendental equations.
c
c  Discussion:
c
c    The vector THETA returned by this function is needed in order to define
c    the terms in a Karhunen-Loeve expansion of a diffusion coefficient.
c
c    The two equations are:
c
c      1/CL - THETA * TAN ( A * THETA ) = 0
c      THETA - 1/CL * TAN ( A * THETA ) = 0
c
c    A and CL are taken to be positive.  Over each open interval
c
c      ( n - 1/2 pi, n + 1/2 pi ) / A, for N = 0, 1, ...
c
c    the function TAN ( A * THETA ) monotonically rises from -oo to +00;
c    therefore, it can be shown that there is one root of each equation
c    in every interval of this form.  Moreover, because of the positivity
c    of A and CL, we can restrict our search to the interval
c
c      [ n pi, n + 1/2 pi ) / A, for N = 0, 1, ...
c
c    This function computes K such roots, starting in the first interval,
c    finding those two roots, moving to the next interval, and so on, until
c    the requested number of roots have been found.  Odd index roots will
c    correspond to the first equation, and even index roots to the second.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Howard Elman, Darran Furnival,
c    Solving the Stochastic Steady-State Diffusion Problem Using Multigrid,
c    University of Maryland Department of Computer Science,
c    Technical Report TR-4786.
c
c  Parameters:
c
c    Input, double precision A, the "radius" of the domain, D = (-A,A)x(-A,A).
c    0 < A.
c
c    Input, double precision CL, the correlation length.
c    0 < CL.
c
c    Input, integer M, the number of values to compute.
c
c    Output, double precision THETA(M), the values of Theta.
c
      implicit none

      integer m

      double precision a
      double precision bmatol
      double precision cl
      double precision eps
      double precision fa
      double precision fb
      double precision fc
      double precision ftol
      integer i
      integer k
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r8_epsilon
      double precision theta(m)
      double precision xa
      double precision xa_init
      double precision xb
      double precision xb_init
      double precision xc

      k = 0
      do i = 1, m
        theta(i) = 0.0D+00
      end do
c
c  [ XA_INIT, XB_INIT] = [ n * pi, n+1/2 pi ] / a, n = 0, 1, 2, ...
c
      xa_init = 0.0D+00
      xb_init = ( pi / 2.0D+00 ) / a
      eps = r8_epsilon ( )

10    continue
c
c  Seek root of equation 1 in interval.
c
        if ( m .le. k ) then
          go to 60
        end if

        k = k + 1
        xa = xa_init
        fa = 1.0D+00 / cl - xa * tan ( a * xa )
        ftol = eps * ( abs ( fa ) + 1.0D+00 )
        xb = xb_init
        fb = - fa
        fc = fa
        bmatol = 100.0D+00 * eps * ( abs ( xa ) + abs ( xb ) )

20      continue

        if ( bmatol .lt. xb - xa ) then

          xc = ( xa + xb ) / 2.0D+00
          fc = 1.0D+00 / cl - xc * tan ( a * xc )

          if ( abs ( fc ) .le. ftol ) then
            go to 30
          else if ( 0.0D+00 .lt. fc ) then
            xa = xc
          else
            xb = xc
          end if

          go to 20

        end if

30      continue

        theta(k) = xc
c
c  Seek root of equation 2 in interval.
c
        if ( m .le. k ) then
          go to 60
        end if

        k = k + 1
c
c  In the first interval, we need to skip the zero root of equation 2.
c
        if ( k .eq. 2 ) then

          k = k - 1

        else

          xa = xa_init
          fa = xa - tan ( a * xa ) / cl
          ftol = eps * ( abs ( fa ) + 1.0D+00 )
          xb = xb_init
          fb = - fa

40        continue

          if ( bmatol .lt. xb - xa ) then

            xc = ( xa + xb ) / 2.0D+00
            fc = xc - tan ( a * xc ) / cl

            if ( abs ( fc ) .le. ftol ) then
              go to 50
            else if ( 0.0D+00 .lt. fc ) then
              xa = xc
            else
              xb = xc
            end if

            go to 40

          end if

50        continue

          theta(k) = xc

        end if
c
c  Advance the interval.
c
        xa_init = xa_init + pi / a
        xb_init = xb_init + pi / a

      go to 10

60    continue

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
