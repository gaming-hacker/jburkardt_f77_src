      subroutine fd2d_heat_steady ( nx, ny, x, y, d, f, u )

c*********************************************************************72
c
cc FD2D_HEAT_STEADY solves the steady 2D heat equation.
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
c    27 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NX, NY, the number of grid points in X and Y.
c
c    Input, double precision X(NX), Y(NY), the coordinates of grid lines.
c
c    Input, double precision function D ( X, Y ), evaluates the thermal
c    conductivity.
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
      double precision d
      external d
      double precision f
      external f
      integer info
      integer n
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
      call interior ( nx, ny, x, y, d, f, n, a, u )
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
      subroutine interior ( nx, ny, x, y, d, f, n, a, rhs )

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
c    27 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NX, NY, the number of grid points in X and Y.
c
c    Input, double precision X(NX), Y(NY), the coordinates of grid lines.
c
c    Input, double precision function D ( X, Y ), evaluates the thermal
c    conductivity.
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
      double precision d
      external d
      double precision dc0
      double precision dce
      double precision dcn
      double precision dcs
      double precision dcw
      double precision dx
      double precision dy
      double precision f
      external  f
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
          dce = d ( xce, y(ic) )
          xcw = 0.5D+00 * ( x(jc) + x(jw) )
          dcw = d ( xcw, y(ic) )
          ycn = 0.5D+00 * ( y(ic) + y(in) )
          dcn = d ( x(jc), ycn )
          ycs = 0.5D+00 * ( y(ic) + y(is) )
          dcs = d ( x(jc), ycs )

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
