      subroutine cg_rc ( n, b, x, r, z, p, q, job )

c*********************************************************************72
c
cc CG_RC is a reverse communication conjugate gradient routine.
c
c  Discussion:
c
c    This routine seeks a solution of the linear system A*x=b
c    where b is a given right hand side vector, A is an n by n
c    symmetric positive definite matrix, and x is an unknown vector
c    to be determined.
c
c    Under the assumptions that the matrix A is large and sparse,
c    the conjugate gradient method may provide a solution when
c    a direct approach would be impractical because of excessive
c    requirements of storage or even of time.
c
c    The conjugate gradient method presented here does not require the 
c    user to store the matrix A in a particular way.  Instead, it only 
c    supposes that the user has a way of calculating
c      y = alpha * A * x + b * y
c    and of solving the preconditioned linear system
c      M * x = b
c    where M is some preconditioning matrix, which might be merely
c    the identity matrix, or a diagonal matrix containing the
c    diagonal entries of A.
c
c    This routine was extracted from the "templates" package.
c    There, it was not intended for direct access by a user;
c    instead, a higher routine called "cg()" was called once by
c    the user.  The cg() routine then made repeated calls to 
c    cgrevcom() before returning the result to the user.
c
c    The reverse communication feature of cgrevcom() makes it, by itself,
c    a very powerful function.  It allows the user to handle issues of
c    storage and implementation that would otherwise have to be
c    mediated in a fixed way by the function argument list.  Therefore,
c    this version of cgrecom() has been extracted from the templates
c    library and documented as a stand-alone procedure.
c
c    The user sets the value of JOB to 1 before the first call,
c    indicating the beginning of the computation, and to the value of
c    2 thereafter, indicating a continuation call.  
c    The output value of JOB is set by cgrevcom(), which
c    will return with an output value of JOB that requests a particular
c    new action from the user.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Richard Barrett, Michael Berry, Tony Chan, James Demmel,
c    June Donato, Jack Dongarra, Victor Eijkhout, Roidan Pozo,
c    Charles Romine, Henk van der Vorst,
c    Templates for the Solution of Linear Systems:
c    Building Blocks for Iterative Methods,
c    SIAM, 1994,
c    ISBN: 0898714710,
c    LC: QA297.8.T45.
c
c  Parameters:
c
c    Input, integer N, the dimension of the matrix.
c
c    Input, double precision B(N), the right hand side vector.
c
c    Input/output, double precision X(N).  On first call, the user 
c    should store an initial guess for the solution in X.  On return with
c    JOB = 4, X contains the latest solution estimate.
c
c    Input/output, double precision R(N), Z(N), P(N), Q(N),
c    information used by the program during the calculation.  The user
c    does not need to initialize these vectors.  However, specific
c    return values of JOB may require the user to carry out some computation
c    using data in some of these vectors.
c
c    Input/output, integer JOB, communicates the task to be done.
c    The user needs to set the input value of JOB to 1, before the first call,
c    and then to 2 for every subsequent call for the given problem.
c    The output value of JOB indicates the requested user action.
c    * JOB = 1, compute Q = A * P;
c    * JOB = 2: solve M*Z=R, where M is the preconditioning matrix;
c    * JOB = 3: compute R = R - A * X;
c    * JOB = 4: check the residual R for convergence.  
c               If satisfactory, terminate the iteration.
c               If too many iterations were taken, terminate the iteration.
c
      implicit none

      integer n

      double precision alpha
      double precision b(n)
      double precision beta
      integer i
      integer iter
      integer job
      double precision p(n)
      double precision pdotq
      double precision q(n)
      double precision r(n)
      double precision rho
      double precision rho_old
      integer rlbl
      double precision x(n)
      double precision z(n)
c
c  Some local variables must be preserved between calls.
c
      save iter
      save rho
      save rho_old
      save rlbl
c
c  Initialization.
c  Alias the workspace columns.
c  Set indexing information.
c  Ask the user to compute the initial residual.
c
      if ( job .eq. 1 ) then

        do i = 1, n
          r(i) = b(i)
        end do

        job = 3
        rlbl = 2
c
c  Begin first conjugate gradient loop.
c  Ask the user for a preconditioner solve.
c
      else if ( rlbl .eq. 2 ) then

        iter = 1

        job = 2
        rlbl = 3
c
c  Compute the direction.
c  Ask the user to compute ALPHA.
c  Save A*P to Q.
c
      else if ( rlbl .eq. 3 ) then

        rho = 0.0D+00
        do i = 1, n
          rho = rho + r(i) * z(i)
        end do

        if ( 1 .lt. iter ) then
          beta = rho / rho_old
          do i = 1, n
            z(i) = z(i) + beta * p(i)
          end do
        end if

        do i = 1, n
          p(i) = z(i)
        end do

        job = 1
        rlbl = 4
c
c  Compute current solution vector.
c  Ask the user to check the stopping criterion.
c
      else if ( rlbl .eq. 4 ) then

        pdotq = 0.0D+00
        do i = 1, n
          pdotq = pdotq + p(i) * q(i)
        end do

        alpha = rho / pdotq

        do i = 1, n
          x(i) = x(i) + alpha * p(i)
        end do

        do i = 1, n
          r(i) = r(i) - alpha * q(i)
        end do

        job = 4
        rlbl = 5
c
c  Begin the next step.
c  Ask for a preconditioner solve.
c
      else if ( rlbl .eq. 5 ) then

        rho_old = rho
        iter = iter + 1

        job = 2
        rlbl = 3

      end if

      return
      end
      subroutine r8mat_mv ( m, n, a, x, y )

c*********************************************************************72
c
cc R8MAT_MV multiplies a matrix times a vector.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c    In FORTRAN90, this operation can be more efficiently carried
c    out by the command
c
c      Y(1:M) = MATMUL ( A(1:M,1:N), X(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of the matrix.
c
c    Input, double precision A(M,N), the M by N matrix.
c
c    Input, double precision X(N), the vector to be multiplied by A.
c
c    Output, double precision Y(M), the product A*X.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision x(n)
      double precision y(m)

      do i = 1, m
        y(i) = 0.0D+00
        do j = 1, n
          y(i) = y(i) + a(i,j) * x(j)
        end do
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
      subroutine wathen ( nx, ny, n, a )

c*********************************************************************72
c
cc WATHEN returns the WATHEN matrix.
c
c  Discussion:
c
c    The Wathen matrix is a finite element matrix which is sparse.
c
c    The entries of the matrix depend in part on a physical quantity
c    related to density.  That density is here assigned random values between
c    0 and 100.
c
c    The matrix order N is determined by the input quantities NX and NY,
c    which would usually be the number of elements in the X and Y directions.
c    The value of N is
c
c      N = 3*NX*NY + 2*NX + 2*NY + 1,
c
c    and sufficient storage in A must have been set aside to hold
c    the matrix.
c
c    A is the consistent mass matrix for a regular NX by NY grid
c    of 8 node serendipity elements.  
c
c    Here is an illustration for NX = 3, NY = 2:
c
c     23-24-25-26-27-28-29
c      |     |     |     |
c     19    20    21    22
c      |     |     |     |
c     12-13-14-15-16-17-18
c      |     |     |     |
c      8     9    10    11
c      |     |     |     |
c      1--2--3--4--5--6--7
c
c    For this example, the total number of nodes is, as expected,
c
c      N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
c
c  Properties:
c
c    A is symmetric positive definite for any positive values of the
c    density RHO(NX,NY), which is here given the value 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Higham,
c    Algorithm 694: A Collection of Test Matrices in MATLAB,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 3, September 1991, pages 289-305.
c
c    Andrew Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size of A.
c
c    Input, integer N, the order of the matrix.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision em(8,8)

      integer i
      integer j
      integer kcol
      integer krow
      integer nx
      integer ny
      integer node(8)
      double precision r8_uniform_01
      double precision rho
      integer seed

      save em

      data em /
     &   6.0, -6.0,  2.0, -8.0,  3.0, -8.0,  2.0, -6.0, 
     &  -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, 
     &   2.0, -6.0,  6.0, -6.0,  2.0, -8.0,  3.0, -8.0, 
     &  -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, 
     &   3.0, -8.0,  2.0, -6.0,  6.0, -6.0,  2.0, -8.0, 
     &  -8.0, 16.0, -8.0, 20.0, -6.0, 32.0, -6.0, 20.0,
     &   2.0, -8.0,  3.0, -8.0,  2.0, -6.0,  6.0, -6.0,
     &  -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, -6.0, 32.0 /

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      do j = 1, ny

        do i = 1, nx
c
c  For the element (I,J), determine the indices of the 8 nodes.
c
          node(1) = 3 * j * nx + 2 * j + 2 * i + 1
          node(2) = node(1) - 1
          node(3) = node(1) - 2

          node(4) = ( 3 * j - 1 ) * nx + 2 * j + i - 1
          node(8) = node(4) + 1

          node(5) = ( 3 * j - 3 ) * nx + 2 * j + 2 * i - 3
          node(6) = node(5) + 1
          node(7) = node(5) + 2
c
c  The density RHO can also be set to a random positive value.
c
          do krow = 1, 8
            do kcol = 1, 8

              if ( node(krow) .lt. 1 .or. n .lt. node(krow) ) then
                write ( *, '(a)' ) ' '
                write ( *, '(a)' ) 'WATHEN - Fatal error!'
                write ( *, '(a)' ) '  Index NODE(KROW) out of bounds.'
                write ( *, '(a,i8)' ) '  I = ', i
                write ( *, '(a,i8)' ) '  J = ', j
                write ( *, '(a,i8)' ) '  KROW = ', krow
                write ( *, '(a,i8)' ) '  NODE(KROW) = ', node(krow)
                stop
              else if ( node(kcol) .lt. 1 .or. n .lt. node(kcol) ) then
                write ( *, '(a)' ) ' '
                write ( *, '(a)' ) 'WATHEN - Fatal error!'
                write ( *, '(a)' ) '  Index NODE(KCOL) out of bounds.'
                write ( *, '(a,i8)' ) '  I = ', i
                write ( *, '(a,i8)' ) '  J = ', j
                write ( *, '(a,i8)' ) '  KCOL = ', kcol
                write ( *, '(a,i8)' ) '  NODE(KCOL) = ', node(kcol)
                stop
              end if

              rho = 1.0D+00
c             rho = 100.0D+00 * r8_uniform_01 ( seed )

              a(node(krow),node(kcol)) = a(node(krow),node(kcol)) 
     &          + 20.0D+00 * rho * em(krow,kcol) / 9.0D+00

            end do
          end do

        end do
      end do

      return
      end
      subroutine wathen_order ( nx, ny, n )
     
c*********************************************************************72
c
cc WATHEN_ORDER returns the order of the WATHEN matrix.
c
c  Discussion:
c
c    N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Higham,
c    Algorithm 694: A Collection of Test Matrices in MATLAB,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 3, September 1991, pages 289-305.
c
c    Andrew Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size of A.
c
c    Output, integer N, the order of the matrix, 
c    as determined by NX and NY.
c
      implicit none

      integer n
      integer nx
      integer ny

      n = 3 * nx * ny + 2 * nx + 2 * ny + 1

      return
      end


