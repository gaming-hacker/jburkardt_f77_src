      subroutine bc_exact_1d ( x, r, rhs )

!*********************************************************************72
!
!! BC_EXACT_1D returns the boundary condition at X for the exact problem.
!
!  Modified:
!
!    31 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real X, the point at which the exact solution is desired.
!
!    Input, real R, the value of the parameter.
!
!    Output, real RHS, the right hand side of the Dirichlet boundary condition.
!
      implicit none

      real r
      real rhs
      real u
      real x

      call u_exact_1d ( x, u )

      rhs = u

      return
      end
      subroutine jac_sge_fd_1d ( n, lda, x, c, r, a )

!*********************************************************************72
!
!! JAC_SGE_FD_1D computes the jacobian in SGE format of the residual function.
!
!  Modified:
!
!    02 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index of the last of the nodes, which are
!    numbered from 0 to N.
!
!    Input, integer LDA, the leading dimension of the array.
!
!    Input, real X(0:N), the coordinates of the nodes.
!
!    Input, real C(0:N), the finite difference coefficients at the nodes.
!
      implicit none

      integer MAXN
      parameter ( MAXN = 100 )

      integer lda
      integer n

      real a(0:lda,0:n)
      real c(0:n)
      real h1
      real h2
      integer i
      integer j
      real r
      real u(0:MAXN)
      real ux(0:MAXN)
      real x(0:n)
!
      do i = 0, n
        do j = 0, n
          a(i,j) = 0.0
        end do
      end do

      call u_fd_1d ( n, x, c, u )
      call ux_fd_1d ( n, x, c, ux )

      a(0,0) = 1.0

      do i = 1, n - 1

        h1 = x(i-1) - x(i) 
        h2 = x(i+1) - x(i)

        a(i,i-1) = 
     &    + r * u(i) * h2 * abs ( h2 )
     &      / ( h1 * h2 * ( abs ( h1 ) + abs ( h2 ) ) )
     &    - 2.0 * h2 / ( h1 * h2 * ( h2 - h1 ) ) 

        a(i,i) = 
     &      1.0 
     &    + r * ux(i) 
     &    - r * u(i) * ( h2 * abs ( h2 ) + h1 * abs ( h1 )  ) 
     &      / ( h1 * h2 * ( abs ( h1 ) + abs ( h2 ) ) )
     &    + 2.0 * ( + h2 - h1 ) / ( h1 * h2 * ( h2 - h1 ) ) 

        a(i,i+1) = 
     &    - r * u(i) * h1 * abs ( h1 ) 
     &      / ( h1 * h2 * ( abs ( h1 ) + abs ( h2 ) ) )
     &    + 2.0 * h1 / ( h1 * h2 * ( h2 - h1 ) ) 

      end do

      a(n,n) = 1.0

      return
      end
      subroutine jac_s3_fd_1d ( n, x, c, r, a1, a2, a3 )

!*********************************************************************72
!
!! JAC_S3_FD_1D computes the jacobian in S3 format of the residual function.
!
!  Modified:
!
!    02 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index of the last of the nodes, which are
!    numbered from 0 to N.
!
!    Input, integer LDA, the leading dimension of the array.
!
!    Input, real X(0:N), the coordinates of the nodes.
!
!    Input, real C(0:N), the finite difference coefficients at the nodes.
!
      implicit none

      integer MAXN
      parameter ( MAXN = 100 )

      integer n

      real a1(1:n)
      real a2(0:n)
      real a3(0:n-1)
      real c(0:n)
      real h1
      real h2
      integer i
      real r
      real u(0:MAXN)
      real ux(0:MAXN)
      real x(0:n)

      do i = 1, n
        a1(i) = 0.0
      end do

      do i = 0, n
        a2(i) = 0.0
      end do

      do i = 0, n-1
        a3(i) = 0.0
      end do

      call u_fd_1d ( n, x, c, u )
      call ux_fd_1d ( n, x, c, ux )

      a2(0) = 1.0

      do i = 1, n - 1

        h1 = x(i-1) - x(i) 
        h2 = x(i+1) - x(i)

        a1(i) = 
     &    + r * u(i) * h2 * abs ( h2 )
     &      / ( h1 * h2 * ( abs ( h1 ) + abs ( h2 ) ) )
     &    - 2.0 * h2 / ( h1 * h2 * ( h2 - h1 ) ) 

        a2(i) = 
     &      1.0 
     &    + r * ux(i) 
     &    - r * u(i) * ( h2 * abs ( h2 ) + h1 * abs ( h1 )  ) 
     &      / ( h1 * h2 * ( abs ( h1 ) + abs ( h2 ) ) )
     &    + 2.0 * ( + h2 - h1 ) / ( h1 * h2 * ( h2 - h1 ) ) 

        a3(i) = 
     &    - r * u(i) * h1 * abs ( h1 ) 
     &      / ( h1 * h2 * ( abs ( h1 ) + abs ( h2 ) ) )
     &    + 2.0 * h1 / ( h1 * h2 * ( h2 - h1 ) ) 

      end do

      a2(n) = 1.0

      return
      end
      function pi ( )

!*********************************************************************72
!
!! PI returns the value of pi.
!
!  Modified:
!
!    04 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real PI, the value of pi.
!
      implicit none

      real pi

      pi = 3.14159265358979323846264338327950288419716939937510

      return
      end
      subroutine resid_exact_1d ( x, u, ux, uxx, r, resid )

!*********************************************************************72
!
!! RESID_EXACT_1D returns the residual of the exact 1D problem.
!
!
!  Modified:
!
!    31 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
      implicit none

      real r
      real resid
      real rhs
      real u
      real ux
      real uxx
      real x

      if ( x .eq. 0.0 ) then
        call bc_exact_1d ( x, r, rhs )
        resid = u - rhs
      else if ( x .lt. 1.0 ) then
        call rhs_exact_1d ( x, r, rhs )
        resid = uxx + u + r * u * ux - rhs
      else if ( x .eq. 1.0 ) then
        call bc_exact_1d ( x, r, rhs )
        resid = u - rhs
      end if

      return
      end
      subroutine resid_fd_1d ( n, x, c, r, resid )

!*********************************************************************72
!
!! RESID_FD_1D returns the right hand side of the finite difference 1D problem.
!
!  Modified:
!
!    02 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index of the last of the nodes, which are
!    numbered from 0 to N.
!
!    Input, real X(0:N), the coordinates of the nodes.
!
!    Input, real C(0:N), the finite difference coefficients at the nodes.
!
      implicit none

      integer MAXN
      parameter ( MAXN = 100 )

      integer n

      real c(0:n)
      integer i
      real r
      real resid(0:n)
      real rhs
      real u(0:MAXN)
      real ux(0:MAXN)
      real uxx(0:MAXN)
      real x(0:n)

      call u_fd_1d ( n, x, c, u )
      call ux_fd_1d ( n, x, c, ux )
      call uxx_fd_1d ( n, x, c, uxx )

      call bc_exact_1d ( x(0), r, rhs )

      resid(0) = u(0) - rhs

      do i = 1, n - 1
        call rhs_exact_1d ( x(i), r, rhs )
        resid(i) = uxx(i) + u(i) + r * u(i) * ux(i) - rhs
      end do

      call bc_exact_1d ( x(n), r, rhs )
      resid(n) = u(n) - rhs

      return
      end
      subroutine rhs_exact_1d ( x, r, rhs )

!*********************************************************************72
!
!! RHS_EXACT_1D returns the exact right hand side of the 1D problem.
!
!  Modified:
!
!    31 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real X, the point at which the exact solution is desired.
!
!    Input, real R, the value of the parameter.
!
!    Output, real HX, the value of H1(X).
!
      implicit none

      real r
      real rhs
      real u
      real ux
      real uxx
      real x

      call u_exact_1d ( x, u )
      call ux_exact_1d ( x, ux )
      call uxx_exact_1d ( x, uxx )

      rhs = uxx + u + r * ( u * ux )

      return
      end
      subroutine sge_check ( lda, m, n, ierror )

!*********************************************************************72
!
!! SGE_CHECK checks the dimensions of a general matrix.
!
!  Modified:
!
!    11 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer LDA, the leading dimension of the array.
!    LDA must be at least M.
!
!    Input, integer M, the number of rows of the matrix.
!    M must be positive.
!
!    Input, integer N, the number of columns of the matrix.
!    N must be positive.
!
!    Output, integer IERROR, reports whether any errors were detected.
!    IERROR is set to 0 before the checks are made, and then:
!    IERROR = IERROR + 1 if LDA is illegal;
!    IERROR = IERROR + 2 if M is illegal;
!    IERROR = IERROR + 4 if N is illegal.
!
      implicit none

      integer ierror
      integer lda
      integer m
      integer n

      ierror = 0

      if ( lda .lt. m ) then
        ierror = ierror + 1
        write ( *, * ) ' '
        write ( *, * ) 'SGE_CHECK - Illegal LDA = ', lda
      end if

      if ( m .lt. 1 ) then
        ierror = ierror + 2
        write ( *, * ) ' '
        write ( *, * ) 'SGE_CHECK - Illegal M = ', m
      end if

      if ( n .lt. 1 ) then
        ierror = ierror + 4
        write ( *, * ) ' '
        write ( *, * ) 'SGE_CHECK - Illegal N = ', n
      end if

      return
      end
      subroutine sge_fa ( a, lda, n, ipivot, info )

!*********************************************************************72
!
!! SGE_FA factors a general matrix.
!
!  Discussion:
!
!    SGE_FA is a simplified version of the LINPACK routine SGEFA.
!
!  Modified:
!
!    04 March 1999
!
!  Parameters:
!
!    Input/output, real A(LDA,N), the matrix to be factored.
!    On output, A contains an upper triangular matrix and the multipliers
!    which were used to obtain it.  The factorization can be written
!    A = L * U, where L is a product of permutation and unit lower
!    triangular matrices and U is upper triangular.
!
!    Input, integer LDA, the leading dimension of the array.
!    LDA must be at least N.
!
!    Input, integer N, the order of the matrix.
!    N must be positive.
!
!    Output, integer IPIVOT(N), a vector of pivot indices.
!
!    Output, integer INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
      implicit none

      integer lda
      integer n

      real a(lda,n)
      integer i
      integer ierror
      integer info
      integer ipivot(n)
      integer j
      integer k
      integer l
      real t
!
!  Check the dimensions.
!
      call sge_check ( lda, n, n, ierror )

      if ( ierror .ne. 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'SGE_FA - Fatal error!'
        write ( *, * ) '  Illegal dimensions.'
        return
      end if

      info = 0

      do k = 1, n-1
!
!  Find L, the index of the pivot row.
!
        l = k
        do i = k+1, n
          if ( abs ( a(i,k) ) .gt. abs ( a(l,k) ) ) then
            l = i
          end if
        end do

        ipivot(k) = l
!
!  If the pivot index is zero, the algorithm has failed.
!
        if ( a(l,k) .eq. 0.0 ) then
          info = k
          write ( *, * ) ' '
          write ( *, * ) 'SGE_FA - Fatal error!'
          write ( *, * ) '  Zero pivot on step ', info
          return
        end if
!
!  Interchange rows L and K if necessary.
!
        if ( l .ne. k ) then
          t = a(l,k)
          a(l,k) = a(k,k)
          a(k,k) = t
        end if
!
!  Normalize the values that lie below the pivot entry A(K,K).
!
        do i = k+1, n
          a(i,k) = - a(i,k) / a(k,k)
        end do
!
!  Row elimination with column indexing.
!
        do j = k+1, n

          if ( l .ne. k ) then
            t = a(l,j)
            a(l,j) = a(k,j)
            a(k,j) = t
          end if

          do i = k+1, n
            a(i,j) = a(i,j) + a(i,k) * a(k,j)
          end do

        end do

      end do

      ipivot(n) = n

      if ( a(n,n) .eq. 0.0 ) then
        info = n
        write ( *, * ) ' '
        write ( *, * ) 'SGE_FA - Fatal error!'
        write ( *, * ) '  Zero pivot on step ', info
      end if

      return
      end
      subroutine sge_sl ( a, lda, n, ipivot, b, job )

!*********************************************************************72
!
!! SGE_SL solves a system factored by SGE_FA.
!
!  Modified:
!
!    04 March 1999
!
!  Parameters:
!
!    Input, real A(LDA,N), the LU factors from SGE_FA.
!
!    Input, integer LDA, the leading dimension of the array.
!    LDA must be at least N.
!
!    Input, integer N, the order of the matrix.
!    N must be positive.
!
!    Input, integer IPIVOT(N), the pivot vector from SGE_FA.
!
!    Input/output, real B(N).
!    On input, the right hand side vector.
!    On output, the solution vector.
!
!    Input, integer JOB, specifies the operation.
!    0, solve A * x = b.
!    nonzero, solve transpose ( A ) * x = b.
!
      implicit none

      integer lda
      integer n

      real a(lda,n)
      real b(n)
      integer ierror
      integer ipivot(n)
      integer j
      integer job
      integer k
      integer l
      real t
!
!  Check the dimensions.
!
      call sge_check ( lda, n, n, ierror )

      if ( ierror .ne. 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'SGE_SL - Fatal error!'
        write ( *, * ) '  Illegal dimensions.'
        return
      end if
!
!  Solve A * x = b.
!
      if ( job .eq. 0 ) then
!
!  Solve PL * Y = B.
!
        do k = 1, n-1

          l = ipivot(k)

          t = b(l)
          if ( l .ne. k ) then
            b(l) = b(k)
            b(k) = t
          end if

          do j = k+1, n
            b(j) = b(j) + t * a(j,k)
          end do

        end do
!
!  Solve U * X = Y.
!
        do k = n, 1, -1
          b(k) = b(k) / a(k,k)
          do j = 1, k-1
            b(j) = b(j) - a(j,k) * b(k)
          end do
        end do
!
!  Solve transpose ( A ) * X = B.
!
      else
!
!  Solve transpose ( U ) * Y = B.
!
        do k = 1, n
          t = 0.0
          do j = 1, k-1
            t = t + a(j,k) * b(j)
          end do
          b(k) = ( b(k) - t ) / a(k,k)
        end do
!
!  Solve transpose ( PL ) * X = Y.
!
        do k = n-1, 1, -1

          t = 0.0
          do j = k+1, n
            t = t + a(j,k) * b(j)
          end do

          b(k) = b(k) + t

          l = ipivot(k)
          if ( l .ne. k ) then
            t = b(l)
            b(l) = b(k)
            b(k) = t
          end if

        end do

      end if

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
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
      subroutine u_linear_1d ( n, x, c )

!*********************************************************************72
!
!! U_LINEAR_1D returns the linear approximation to the solution.
!
!  Discussion:
!
!    The boundary conditions are linearly interpolated.
!
!  Modified:
!
!    31 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index of the last of the nodes, which are
!    numbered from 0 to N.
!
!    Input, real X(0:N), the coordinates of the nodes.
!
!    Output, real C(0:N), the finite difference coefficients at the nodes.
!
      implicit none

      integer n

      real c(0:n)
      integer i
      real u
      real x(0:n)

      call u_exact_1d ( x(0), u )
      c(0) = u

      call u_exact_1d ( x(n), u )
      c(n) = u

      do i = 1, n-1

        c(i) = ( 
     &      ( x(n) - x(i) ) * c(0) 
     &    + ( x(i) - x(0) ) * c(n) 
     &    ) / ( x(n) - x(0) )

      end do

      return
      end
      subroutine threed

!*********************************************************************72
!
!! THREED ???
!
!  Discussion:
!
!    This problem is a coarse model of a 3-D Navier Stokes equation.
!    The parameter R corresponds to the Reynolds number.  For small R,
!    we have strong ellipticity, while for large R the problem has
!    a hyperbolic character.
!
!  Equations:
!
!    Within the unit cube:
!
!      Uxx + Uyy + U + R * ( U * Ux + V * Uy + W * Uz ) = H1(X,Y,Z)
!      Vxx + Vyy + V + R * ( U * Vx + V * Vy + W * Vz ) = H2(X,Y,Z)
!      Wxx + Wyy + W + R * ( U * Wx + V * Wy + W * Wz ) = H3(X,Y,Z)
!
!    with H1, H2 and H3 chosen so that:
!
!      U_Exact(X,Y,Z) = sin ( 0.5 * PI * X ) * cos ( 0.5 * PI * Y ) * cos ( 0.5 * PI * Z ),
!      V_Exact(X,Y,Z) = cos ( 0.5 * PI * X ) * sin ( 0.5 * PI * Y ) * cos ( 0.5 * PI * Z ),
!      W_Exact(X,Y,Z) = cos ( 0.5 * PI * X ) * cos ( 0.5 * PI * Y ) * sin ( 0.5 * PI * Z ).
!
!    and with Dirichlet boundary conditions.
!
!  Reference:
!
!    Willi Schoenauer and Ruediger Weiss,
!    An Engineering Approach to Generalized Conjugate Gradient Methods
!      and Beyond.
!
!  Modified:
!
!    31 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
      implicit none

      return
      end
      subroutine twod

!*********************************************************************72
!
!! TWOD ???
!
!  Discussion:
!
!    This problem is a coarse model of a 2-D Navier Stokes equation.
!    The parameter R corresponds to the Reynolds number.  For small R,
!    we have strong ellipticity, while for large R the problem has
!    a hyperbolic character.
!
!  Equations:
!
!    Within the unit square:
!
!      Uxx + Uyy + U + R * ( U * Ux + V * Uy ) = H1(X,Y)
!      Vxx + Vyy + V + R * ( U * Vx + V * Vy ) = H2(X,Y)
!
!    with H1 and H2 chosen so that:
!
!      U_Exact(X,Y) = sin ( 0.5 * PI * X ) * cos ( 0.5 * PI * Y ),
!      V_Exact(X,Y) = cos ( 0.5 * PI * X ) * sin ( 0.5 * PI * Y ).
!
!    and with Dirichlet boundary conditions.
!
!  Reference:
!
!    Willi Schoenauer and Ruediger Weiss,
!    An Engineering Approach to Generalized Conjugate Gradient Methods
!      and Beyond.
!
!  Modified:
!
!    31 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
      implicit none

      return
      end
      subroutine u_exact_1d ( x, u )

!*********************************************************************72
!
!! U_EXACT_1D returns the exact solution of the 1D problem.
!
!  Discussion:
!
!    This problem is a coarse model of a 1-D Navier Stokes equation.
!    The parameter R corresponds to the Reynolds number.  For small R,
!    we have strong ellipticity, while for large R the problem has
!    a hyperbolic character.
!
!  Equations:
!
!    Within the unit interval:
!
!      Uxx + U + R * U * Ux = H1(X)
!
!    with H1 chosen so that:
!
!      U_Exact(X) = sin ( 0.5 * PI * X ),
!
!    and with Dirichlet boundary conditions.
!
!  Reference:
!
!    Willi Schoenauer and Ruediger Weiss,
!    An Engineering Approach to Generalized Conjugate Gradient Methods
!    and Beyond.
!
!  Modified:
!
!    31 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real X, the point at which the exact solution is desired.
!
!    Output, real U, the value of the exact solution at the point.
!
      implicit none

      real pi
      real u
      real x

      u = sin ( 0.5 * pi() * x )

      return
      end
      subroutine u_fd_1d ( n, x, c, u )

!*********************************************************************72
!
!! U_FD_1D ???
!
!  Modified:
!
!    31 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index of the last of the nodes, which are
!    numbered from 0 to N.
!
!    Input, real X(0:N), the coordinates of the nodes.
!
!    Input, real C(0:N), the finite difference coefficients at the nodes.
!
      implicit none

      integer n

      real c(0:n)
      integer i
      real u(0:n)
      real x(0:n)

      do i = 0, n
        u(i) = c(i)
      end do

      return
      end
      subroutine ux_exact_1d ( x, ux )

!*********************************************************************72
!
!! UX_EXACT_1D returns the first derivative of the exact solution of the 1D problem.
!
!  Modified:
!
!    31 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real X, the point at which the first derivative is desired.
!
!    Output, real UX, the value of the first derivative at the point.
!
      implicit none

      real pi
      real ux
      real x

      ux = 0.5 * pi() * cos ( 0.5 * pi() * x )

      return
      end
      subroutine ux_fd_1d ( n, x, c, ux )

!*********************************************************************72
!
!! UX_FD_1D returns a finite difference estimate of dUdX at each node.
!
!
!  Modified:
!
!    02 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index of the last of the nodes, which are
!    numbered from 0 to N.
!
!    Input, real X(0:N), the coordinates of the nodes.
!
!    Input, real C(0:N), the finite difference coefficients at the nodes.
!
!    Output, real UX(0:N), the finite difference estimates of dUdX at
!    the nodes.
!
      implicit none

      integer n

      real c(0:n)
      real h1
      real h2
      integer i
      real ux(0:n)
      real x(0:n)

      ux(0) = ( c(1) - c(0) ) / ( x(1) - x(0) )

      do i = 1, n-1

        h1 = x(i-1) - x(i)
        h2 = x(i+1) - x(i)

        ux(i) = 
     &    ( h2 * abs ( h2 ) * ( c(i-1) - c(i) ) 
     &    + h1 * abs ( h1 ) * ( c(i+1) - c(i) ) ) 
     &    / ( h1 * h2 * ( abs ( h1 ) + abs ( h2 ) ) )

      end do

      ux(n) = ( c(n-1) - c(n) ) / ( x(n-1) - x(n) )

      return
      end
      subroutine uxx_exact_1d ( x, uxx )

!*********************************************************************72
!
!! UXX_EXACT_1D returns the second derivative of the exact solution of the 1D problem.
!
!  Modified:
!
!    31 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real X, the point at which the second derivative is desired.
!
!    Output, real UXX, the value of the second derivative at the point.
!
      implicit none

      real pi
      real uxx
      real x

      uxx = - 0.25 * pi()**2 * sin ( 0.5 * pi() * x )

      return
      end
      subroutine uxx_fd_1d ( n, x, c, uxx )

!*********************************************************************72
!
!! UXX_FD_1D ???
!
!  Modified:
!
!    31 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the index of the last of the nodes, which are
!    numbered from 0 to N.
!
!    Input, real X(0:N), the coordinates of the nodes.
!
!    Input, real C(0:N), the finite difference coefficients at the nodes.
!
      implicit none

      integer n

      real c(0:n)
      real h1
      real h2
      integer i
      real uxx(0:n)
      real x(0:n)

      h1 = x(1) - x(0)
      h2 = x(2) - x(0)

      uxx(0) = 2.0 * ( 
     &      ( - h1 + h2 ) * c(0) 
     &             - h2   * c(1) 
     &    +     h1        * c(2)
     &    ) / ( h1 * h2 * ( h2 - h1 ) )

      do i = 1, n-1

        h1 = x(i-1) - x(i) 
        h2 = x(i+1) - x(i)

        uxx(i) = 2.0 * ( 
     &             - h2   * c(i-1) 
     &    + ( - h1 + h2 ) * c(i) 
     &    +     h1        * c(i+1 )
     &    ) / ( h1 * h2 * ( h2 - h1 ) )

      end do

      h1 = x(n-2) - x(n)
      h2 = x(n-1) - x(n)

      uxx(n) = 2.0 * ( 
     &             - h2   * c(n-2)
     &        + h1        * c(n-1)
     &    + ( - h1 + h2 ) * c(n) 
     &  ) / ( h1 * h2 * ( h2 - h1 ) )

      return
      end
      subroutine s3_check ( n, ierror )

!*********************************************************************72
!
!! S3_CHECK checks the dimensions of a tridiagonal matrix.
!
!  Modified:
!
!    06 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the matrix.
!    N must be at least 2.
!
!    Output, integer IERROR, error flag.
!    0, no errors detected.
!    1, N was less than 2.
!
      implicit none

      integer ierror
      integer n

      ierror = 0

      if ( n .lt. 2 ) then
        ierror = ierror + 1
        write ( *, * ) ' '
        write ( *, * ) 'S3_CHECK - Fatal error!'
        write ( *, * ) '  N must be at least 2.'
        write ( *, * ) '  The input N was ', n
      end if

      return
      end
      subroutine s3_np_fa ( a1, a2, a3, n, info )

!*********************************************************************72
!
!! S3_NP_FA factors a tridiagonal system without pivoting.
!
!  Discussion:
!
!    Because this routine does not use pivoting, it can fail even when
!    the matrix is not singular, and it is liable to make larger
!    errors.
!
!    S3_NP_FA and S3_NP_SL may be preferable to the corresponding 
!    LINPACK routine SGTSL for tridiagonal systems, which factors and solves
!    in one step, and does not save the factorization.
!
!  Modified:
!
!    06 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output real A1(2:N), A2(1:N), A3(1:N-1), the subdiagonal,
!    diagonal, and superdiagonal of the matrix.  On output, these are
!    overwritten by factorization information.
!
!    Input, integer N, the order of the matrix.
!    N must be at least 2.
!
!    Output, integer INFO, singularity flag.
!    0, no singularity detected.
!    nonzero, the factorization failed on the INFO-th step.
!
      implicit none

      integer n

      real a1(2:n)
      real a2(1:n)
      real a3(1:n-1)
      integer i
      integer ierror
      integer info
      real t
!
!  Check the dimensions.
!
      call s3_check ( n, ierror )

      if ( ierror .ne. 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'S3_NP_FA - Fatal error!'
        write ( *, * ) '  Illegal dimensions.'
        return
      end if

      info = 0

      do i = 1, n-1

        if ( a2(i) .eq. 0.0 ) then
          info = i
          write ( *, * ) ' '
          write ( *, * ) 'S3_NP_FA - Fatal error!'
          write ( *, * ) '  Zero pivot on step ', info
          return
        end if

        t = a1(i+1) / a2(i)
        a1(i+1) = t
        a2(i+1) = a2(i+1) - t * a3(i)

      end do
 
      if ( a2(n) .eq. 0.0 ) then
        info = n
        write ( *, * ) ' '
        write ( *, * ) 'S3_NP_FA - Fatal error!'
        write ( *, * ) '  Zero pivot on step ', info
        return
      end if
 
      return
      end
      subroutine s3_np_sl ( a1, a2, a3, n, b, job )

!*********************************************************************72
!
!! S3_NP_SL solves a tridiagonal system factored by S3_NP_FA.
!
!  Modified:
!
!    06 December 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real A1(2:N), A2(1:N), A3(1:N-1), the factor information
!    returned by S3_NP_FA.
!
!    Input, integer N, the order of the matrix.
!    N must be at least 2.
!
!    Input/output, real B(N).
!    On input, B contains the right hand side of the linear system.
!    On output, B contains the solution of the linear system.
!
!    Input, integer JOB, specifies the system to solve.
!    0, solve A * x = b.
!    nonzero, solve transpose ( A ) * x = b.
!
      implicit none

      integer n

      real a1(2:n)
      real a2(1:n)
      real a3(1:n-1)
      real b(n)
      integer i
      integer ierror
      integer job
!
!  Check the dimensions.
!
      call s3_check ( n, ierror )

      if ( ierror .ne. 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'S3_NP_SL - Fatal error!'
        write ( *, * ) '  Illegal dimensions.'
        return
      end if

      if ( job .eq. 0 ) then
!
!  Solve L * Y = B.
!
        do i = 2, n
          b(i) = b(i) - a1(i) * b(i-1)
        end do
!
!  Solve U * X = Y.
!
        do i = n, 1, -1
          b(i) = b(i) / a2(i)
          if ( i .gt. 1 ) then
            b(i-1) = b(i-1) - a3(i-1) * b(i)
          end if
        end do

      else
!
!  Solve tranpose ( U ) * Y = B
!
        do i = 1, n
          b(i) = b(i) / a2(i)
          if ( i .lt. n ) then
            b(i+1) = b(i+1) - a3(i) * b(i)
          end if
        end do
!
!  Solve transpose ( L ) * X = Y.
!
        do i = n-1, 1, -1
          b(i) = b(i) - a1(i+1) * b(i+1)
        end do

      end if
 
      return
      end
