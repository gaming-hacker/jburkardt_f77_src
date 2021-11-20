      program main

c*****************************************************************************80
c
cc MAIN is the main program for FD2D_HEAT_STEADY_PRB.
c
c  Discussion:
c
c    FD2D_HEAT_STEADY_PRB tests the FD2D_HEAT_STEADY library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FD2D_HEAT_STEADY_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the FD2D_HEAT_STEADY library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FD2D_HEAT_STEADY_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*****************************************************************************80
c
cc TEST01 computes the solution for a steady state heat equation problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nx
      parameter ( nx = 41 )
      integer ny
      parameter ( ny = 21 )

      character * ( 80 ) command_filename
      integer command_unit
      double precision d
      external d
      character * ( 80 ) data_filename
      integer data_unit
      double precision f
      external f
      integer i
      integer j
      double precision umat(nx,ny)
      double precision u_mean
      double precision xmat(nx,ny)
      double precision xmax
      double precision xmin
      double precision xvec(nx)
      double precision ymat(nx,ny)
      double precision ymax
      double precision ymin
      double precision yvec(ny)
c
c  Specify the spatial grid.
c
      call r8vec_linspace ( nx, 0.0D+00, 2.0D+00, xvec )

      call r8vec_linspace ( ny, 0.0D+00, 1.0D+00, yvec )

      call r8vec_mesh_2d ( nx, ny, xvec, yvec, xmat, ymat )
c
c  Solve the finite difference approximation to the steady 2D heat equation.
c
      call fd2d_heat_steady ( nx, ny, xvec, yvec, d, f, umat )
c
c  Create a data file.
c
      data_filename = 'test01_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 1, ny
        do i = 1, nx
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      xmat(i,j), ymat(i,j), umat(i,j)
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Created graphics data file "' 
     &  // trim ( data_filename ) // '".'
c
c  Create the command file.
c
      command_filename = 'test01_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "test01.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
      write ( command_unit, '(a)' ) 'set zlabel "<---U(X,Y)--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Sample Solution"'
      write ( command_unit, '(a)' ) 'set contour'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set cntrparam levels 10'
      write ( command_unit, '(a)' ) 'set view 75, 75'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 
     &  'splot "' // trim ( data_filename ) // '"'

      close ( unit = command_unit )

      write ( *, '(a)' ) 
     &  '  Created graphics command file "' 
     &  // trim ( command_filename ) // '".'
c
c  Report the average value of U.
c
      u_mean = 0.0D+00
      do j = 1, ny
        do i = 1, nx
          u_mean = u_mean + umat(i,j)
        end do
      end do
      u_mean = u_mean / dble ( nx * ny )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Mean value of U is ', u_mean

      return
      end
      function d ( x, y )

c*****************************************************************************80
c
cc D evaluates the heat conductivity coefficient.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, Y, the evaluation point.
c
c    Output, double precision D, the value of the heat conductivity at (X,Y).
c
      implicit none

      double precision d
      double precision x
      double precision y

      d = 1.0D+00

      return
      end
      function f ( x, y )

c*****************************************************************************80
c
cc F evaluates the heat source term.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, Y, the evaluation point.
c
c    Output, double precision F, the value of the heat source term at (X,Y).
c
      implicit none

      double precision f
      double precision x
      double precision y

      f = 0.0D+00

      return
      end
      subroutine boundary ( nx, ny, x, y, n, a, rhs )

c*****************************************************************************80
c
cc BOUNDARY sets up the matrix and right hand side at boundary nodes.
c
c  Discussion:
c
c    For this simple problem, the boundary conditions specify that the solution
c    is 100 on the left side, and insulated on the right, top and bottom.
c
c    Nodes are assigned a single index K, which increases as:
c
c    (NY-1)*NX+1  (NY-1)*NX+2  ...  NY * NX
c           ....         ....  ...    .....
c           NX+1         NX+2  ...   2 * NX
c              1            2  ...       NX
c
c    The index K of a node on the lower boundary satisfies:
c      1 <= K <= NX
c    The index K of a node on the upper boundary satisfies:
c      (NY-1)*NX+1 <= K <= NY * NX
c    The index K of a node on the left boundary satisfies:
c      mod ( K, NX ) = 1
c    The index K of a node on the right boundary satisfies:
c      mod ( K, NX ) = 0
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
c    20 August 2013
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
c    Input, integer N, the number of nodes.
c
c    Input/output, double precision A(N,N).  On input, the system matrix, with the 
c    entries for the interior nodes filled in.  On output, the entries for
c    the boundary nodes have been set as well.
c
c    Input, double precision RHS(N), on input, the system right hand side, 
c    with the entries for the interior nodes filled in.  On output, the entries for
c    the boundary nodes have been set as well.
c
      implicit none

      integer n
      integer nx
      integer ny

      double precision a(n,n)
      integer i
      integer j
      integer kc
      double precision rhs(n)
      double precision x(n)
      double precision y(n)
c
c  Left boundary.
c
      j = 1
      do i = 2, ny - 1
        kc = ( i - 1 ) * nx + j
        a(kc,kc) = a(kc,kc) + 1.0D+00
        rhs(kc) = 10.0D+00
      end do
c
c  Right boundary.
c
      j = nx
      do i = 2, ny - 1
        kc = ( i - 1 ) * nx + j
        a(kc,kc) = a(kc,kc) + 1.0D+00
        rhs(kc) = 100.0D+00
      end do
c
c  Lower boundary.
c
      i = 1
      do j = 1, nx
        kc = ( i - 1 ) * nx + j
        a(kc,kc) = a(kc,kc) + 1.0D+00
        rhs(kc) = 0.0D+00
      end do
c
c  Upper boundary.
c
      i = ny
      do j = 1, nx
        kc = ( i - 1 ) * nx + j
        a(kc,kc) = a(kc,kc) + 1.0D+00
        rhs(kc) = 0.0D+00
      end do

      return
      end

