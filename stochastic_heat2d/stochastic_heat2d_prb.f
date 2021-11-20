      program main

c*********************************************************************72
c
cc STOCHASTIC_HEAT2D_PRB demonstrates the use of STOCHASTIC_HEAT2D.
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
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'STOCHASTIC_HEAT2D_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the STOCHASTIC_HEAT2D library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'STOCHASTIC_HEAT2D_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 plots a sample solution of a 2D stochastic diffusivity equation.
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
      implicit none

      integer nx
      parameter ( nx = 21 )
      integer ny
      parameter ( ny = 21 )

      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      integer i
      integer j
      double precision omega(4)
      integer seed
      double precision test01_f
      external test01_f
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

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) 
     &  '  Consider the steady heat equation in the unit square,'
      write ( *, '(a)' ) '  with 0 Dirichlet boundary conditions, '
      write ( *, '(a)' ) 
     &  '  and a Gaussian heat source term F centered at (0.60,0.80).'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Model the diffusivity coefficient as spatially varying,'
      write ( *, '(a)' ) 
     &  '  with a stochastic dependence on parameters OMEGA(1:4),'
      write ( *, '(a)' ) 
     &  '  as described in Babuska, Nobile, Tempone (BNT).'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Compute and display the solution U for a given choice'
      write ( *, '(a)' ) '  of the parameters OMEGA.'
c
c  Create the X and Y coordinate vectors.
c
      xmin = 0.0D+00
      xmax = 1.0D+00
      call r8vec_linspace ( nx, xmin, xmax, xvec )

      ymin = 0.0D+00
      ymax = 1.0D+00
      call r8vec_linspace ( ny, ymin, ymax, yvec )
c
c  Create the X and Y coordinate matrices.
c
      call r8vec_mesh_2d ( nx, ny, xvec, yvec, xmat, ymat )
c
c  Sample OMEGA:
c
      seed = 123456789
      call r8vec_normal_01 ( 4, seed, omega )
      do i = 1, 4
        omega(i) = 2.0D+00 * omega(i)
      end do
      call r8vec_print ( 4, omega, '  Sampled OMEGA values:' )
c
c  Solve the finite difference approximation to the steady 2D heat equation
c  for this set of OMEGA values.
c
      call stochastic_heat2d ( omega, nx, ny, xvec, yvec, test01_f, 
     &  umat )
c
c  Create a data file.
c
      data_filename = 'solution_data.txt'
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
      write ( *, '(a)' ) '  Created graphics data file "' 
     &  // trim ( data_filename ) // '".'
c
c  Create the command file.
c
      command_filename = 'solution_commands.txt'
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
      write ( command_unit, '(a)' ) 'set output "solution.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
      write ( command_unit, '(a)' ) 'set zlabel "<---U(X,Y)--->"'
      write ( command_unit, '(a)' )     'set title "Sample Solution"'
      write ( command_unit, '(a)' ) 'set contour'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set cntrparam levels 10'
      write ( command_unit, '(a)' ) 'set view 75, 75'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'splot "' 
     &  // trim ( data_filename ) // '"'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Created graphics command file "' 
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
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 looks at mean temperature as a function of OMEGA(1) and OMEGA(2).
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
      implicit none

      integer nx
      parameter ( nx = 21 )
      integer ny
      parameter ( ny = 21 )
      integer omega1_num
      parameter ( omega1_num = 21 )
      integer omega2_num
      parameter ( omega2_num = 21 )

      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      integer i
      integer i1
      integer j
      integer j1
      double precision omega(4)
      double precision omega1_mat(omega1_num,omega2_num)
      double precision omega1_max
      double precision omega1_min
      double precision omega1_vec(omega1_num)
      double precision omega2_mat(omega1_num,omega2_num)
      double precision omega2_max
      double precision omega2_min
      double precision omega2_vec(omega2_num)
      double precision test01_f
      external test01_f
      double precision umat(nx,ny)
      double precision u_mean_mat(nx,ny)
      double precision u_mean_max
      double precision xmat(nx,ny)
      double precision xmax
      double precision xmin
      double precision xvec(nx)
      double precision ymat(nx,ny)
      double precision ymax
      double precision ymin
      double precision yvec(ny)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) '  Fix OMEGA(3) = 4, OMEGA(4) = 0, and'
      write ( *, '(a)' ) 
     &  '  examine dependence of average temperature on OMEGA(1)'
      write ( *, '(a)' ) '  and OMEGA(2) over the range [-10,+10].'
c
c  Create the X and Y coordinate vectors.
c
      xmin = 0.0D+00
      xmax = 1.0D+00
      call r8vec_linspace ( nx, xmin, xmax, xvec )

      ymin = 0.0D+00
      ymax = 1.0D+00
      call r8vec_linspace ( ny, ymin, ymax, yvec )
c
c  Create the X and Y coordinate matrices.
c
      call r8vec_mesh_2d ( nx, ny, xvec, yvec, xmat, ymat )
c
c  Create OMEGA1 and OMEGA2 vectors.
c
      omega1_min = -10.0D+00
      omega1_max = +10.0D+00
      call r8vec_linspace ( omega1_num, omega1_min, omega1_max, 
     &  omega1_vec )

      omega2_min = -10.0D+00
      omega2_max = +10.0D+00
      call r8vec_linspace ( omega2_num, omega2_min, omega2_max, 
     &  omega2_vec )
c
c  Create the OMEGA1 and OMEGA2 coordinate matrices.
c
      call r8vec_mesh_2d ( omega1_num, omega2_num, omega1_vec, 
     &  omega2_vec, omega1_mat, omega2_mat )
c
c  Set OMEGA(3) and OMEGA(4).
c
      omega(3) = 4.0D+00
      omega(4) = 0.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Omega(3) fixed at ', omega(3)
      write ( *, '(a,g14.6)' ) '  Omega(4) fixed at ', omega(4)
c
c  Solve the finite difference approximation to the steady 2D heat equation,
c  and save the mean value of the solution, which is a slightly biased
c  estimate of the heat integral over the unit square.
c
      do j = 1, omega2_num
        omega(2) = omega2_vec(j)
        do i = 1, omega1_num
          omega(1) = omega1_vec(i)
          call stochastic_heat2d ( omega, nx, ny, xvec, yvec, 
     &      test01_f, umat )

          u_mean_mat(i,j) = 0.0D+00
          do j1 = 1, ny
            do i1 = 1, nx
              u_mean_mat(i,j) = u_mean_mat(i,j) + umat(i1,j1)
            end do
          end do
          u_mean_mat(i,j) = u_mean_mat(i,j) / dble ( nx * ny )
        end do
      end do
c
c  Create a data file.
c
      data_filename = 'umean_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 1, ny
        do i = 1, nx
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' )
     &      omega1_mat(i,j), omega2_mat(i,j), u_mean_mat(i,j)
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Created graphics data file "' 
     &  // trim ( data_filename ) // '".'
c
c  Create the command file.
c
      command_filename = 'umean_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' 
     &  // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "umean.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<---OMEGA1--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---OMEGA2--->"'
      write ( command_unit, '(a)' ) 
     &  'set zlabel "<---U_MEAN(OMEGA1,OMEGA2)--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Solution Mean as Function of Omega1, Omega2"'
      write ( command_unit, '(a)' ) 'set contour'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set cntrparam levels 10'
      write ( command_unit, '(a)' ) 'set view 75, 75'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 
     &  'splot "' // trim ( data_filename ) // '"'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Created graphics command file "' 
     &  // trim ( command_filename ) // '".'
c
c  Print the maximum value of the mean.
c
      u_mean_max = 0.0D+00
      do j = 1, omega2_num
        do i = 1, omega1_num
          u_mean_max = max ( u_mean_max, u_mean_mat(i,j) )
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  U_Mean_Max = ', u_mean_max

      return
      end
      subroutine boundary ( nx, ny, x, y, n, a, rhs )

c*********************************************************************72
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
c    28 August 2013
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
c    Input/output, double precision A(N,N).  On input, the system matrix, with
c    the entries for the interior nodes filled in.  On output, the entries for
c    the boundary nodes have been set as well.
c
c    Input, double precision RHS(N), on input, the system right hand side,
c    with the entries for the interior nodes filled in.  On output, the entries
c    for the boundary nodes have been set as well.
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
        rhs(kc) = 0.0D+00
      end do
c
c  Right boundary.
c
      j = nx
      do i = 2, ny - 1
        kc = ( i - 1 ) * nx + j
        a(kc,kc) = a(kc,kc) + 1.0D+00
        rhs(kc) = 0.0D+00
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
      function test01_f ( x, y )

c*********************************************************************72
c
cc TEST01_F evaluates the heat source term.
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
c    Input, double precision X, Y, the evaluation point.
c
c    Output, double precision TEST01_F, the value of the heat source term at (X,Y).
c
      implicit none

      double precision arg
      double precision test01_f
      double precision v
      double precision x
      double precision y

      v = 0.05D+00
      arg = ( ( x - 0.60D+00 )**2 + ( y - 0.80D+00 )**2 ) / v**2
      test01_f = 2000.0D+00 * exp ( - arg )

      return
      end
