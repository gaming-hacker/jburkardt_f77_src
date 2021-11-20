      program main

c*********************************************************************72
c
cc MAIN is the main program for STOCHASTIC_DIFFUSION.
c
c  Discussion:
c
c    STOCHASTIC_DIFFUSION_PRB calls the STOCHASTIC_DIFFUSION test routines.
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
      implicit none

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STOCHASTIC_DIFFUSION_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the STOCHASTIC_DIFFUSION library.'

      call bnt_contour ( )
      call elman_contour ( )
      call ntw_contour ( )
      call xk_contour ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'STOCHASTIC_DIFFUSION_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine bnt_contour ( )

c*********************************************************************72
c
cc BNT_CONTOUR displays contour plots of a 2D stochastic diffusivity function.
c
c  Discussion:
c
c    The diffusivity function is compute by DIFFUSIVITY_2D_BNT.
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
c    Ivo Babuska, Fabio Nobile, Raul Tempone,
c    A stochastic collocation method for elliptic partial differential equations
c    with random input data,
c    SIAM Journal on Numerical Analysis,
c    Volume 45, Number 3, 2007, pages 1005-1034.
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer nx
      parameter ( nx = 41 )
      integer ny
      parameter ( ny = 31 )

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision dc(nx,ny)
      double precision dc0
      integer i
      integer j
      integer n
      double precision omega(m)
      integer seed
      double precision xmat(nx,ny)
      double precision xvec(nx)
      double precision ymat(nx,ny)
      double precision yvec(ny)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BNT_CONTOUR'
      write ( *, '(a)' ) 
     &  '  Display contour or surface plots of the stochastic'
      write ( *, '(a)' ) 
     &  '  diffusivity function defined by DIFFUSIVITY_2D_BNT.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  The first plot uses uniform random values for OMEGA.'
      write ( *, '(a)' ) 
     &  '  The second uses Gaussian (normal) random values.'
c
c  Set the spatial grid.
c
      call r8vec_linspace ( nx, -1.5D+00, 0.0D+00, xvec )
      call r8vec_linspace ( ny, -0.4D+00, 0.8D+00, yvec )

      call r8vec_mesh_2d ( nx, ny, xvec, yvec, xmat, ymat )
c
c  Sample OMEGA.
c
      seed = 123456789
      call r8vec_uniform_01 ( m, seed, omega )
c
c  Compute the diffusivity field.
c
      dc0 = 10.0D+00
      n = nx * ny
      call diffusivity_2d_bnt ( dc0, omega, n, xmat, ymat, dc )
c
c  Create a data file.
c
      data_filename = 'bnt_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 1, ny
        do i = 1, nx
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      xmat(i,j), ymat(i,j), dc(i,j)
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )     '  Created graphics data file "' 
     &  // trim ( data_filename ) // '".'
c
c  Create the command file.
c
      command_filename = 'bnt_commands.txt'
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
      write ( command_unit, '(a)' ) 'set output "bnt_contour.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
      write ( command_unit, '(a)' ) 'set zlabel "<---DC(X,Y)--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "BNT Stochastic diffusivity function"'
      write ( command_unit, '(a)' ) 'set contour'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set cntrparam levels 10'
      write ( command_unit, '(a)' ) '#set view map'
      write ( command_unit, '(a)' ) 'set view 75, 75'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'splot "' 
     &  // trim ( data_filename ) // '"'

      close ( unit = command_unit )

      write ( *, '(a)' )     '  Created graphics command file "' 
     &  // trim ( command_filename ) // '".'

      return
      end
      subroutine elman_contour ( )

c*********************************************************************72
c
cc ELMAN_CONTOUR displays a contour plot of a 2D stochastic diffusivity function.
c
c  Discussion:
c
c    The diffusivity function is compute by DIFFUSIVITY_2D_ELMAN.
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
c    Howard Elman, Darran Furnaval,
c    Solving the stochastic steady-state diffusion problem using multigrid,
c    IMA Journal on Numerical Analysis,
c    Volume 27, Number 4, 2007, pages 675-688.
c
      implicit none

      integer m_1d
      parameter ( m_1d = 5 )
      integer nx
      parameter ( nx = 51 )
      integer ny
      parameter ( ny = 51 )

      double precision a
      double precision cl
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision dc(nx,ny)
      double precision dc0
      integer i
      integer j
      integer m
      double precision omega(m_1d,m_1d)
      integer seed
      double precision xmat(nx,ny)
      double precision xvec(nx)
      double precision ymat(nx,ny)
      double precision yvec(ny)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ELMAN_CONTOUR'
      write ( *, '(a)' ) 
     &  '  Display contour or surface plots of the stochastic'
      write ( *, '(a)' ) 
     &  '  diffusivity function defined by DIFFUSIVITY_2D_ELMAN.'
c
c  Set the spatial grid.
c
      a = 1.0D+00
      call r8vec_linspace ( nx, -a, +a, xvec )
      call r8vec_linspace ( ny, -a, +a, yvec )

      call r8vec_mesh_2d ( nx, ny, xvec, yvec, xmat, ymat )
c
c  Sample OMEGA.
c
      seed = 123456789
      call r8vec_normal_01 ( m_1d * m_1d, seed, omega )
c
c  Compute the diffusivity field.
c
      cl = 0.1D+00
      dc0 = 10.0D+00
      call diffusivity_2d_elman ( a, cl, dc0, m_1d, omega, nx, nx, xmat,
     & ymat, dc )
c
c  Create a data file.
c
      data_filename = 'elman_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 1, ny
        do i = 1, nx
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      xmat(i,j), ymat(i,j), dc(i,j)
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )     '  Created graphics data file "' // 
     &  trim ( data_filename ) // '".'
c
c  Create the command file.
c
      command_filename = 'elman_commands.txt'
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
      write ( command_unit, '(a)' ) 'set output "elman_contour.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
      write ( command_unit, '(a)' ) 'set zlabel "<---DC(X,Y)--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Elman Stochastic diffusivity function"'
      write ( command_unit, '(a)' ) 'set contour'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set cntrparam levels 10'
      write ( command_unit, '(a)' ) '#set view map'
      write ( command_unit, '(a)' ) 'set view 75, 75'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'splot "' // 
     &  trim ( data_filename ) // '"'

      close ( unit = command_unit )

      write ( *, '(a)' )     '  Created graphics command file "' 
     &  // trim ( command_filename ) // '".'

      return
      end
      subroutine ntw_contour ( )

c*********************************************************************72
c
cc NTW_CONTOUR displays a contour plot of a 2D stochastic diffusivity function.
c
c  Discussion:
c
c    The diffusivity function is compute by DIFFUSIVITY_2D_NTW.
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
c    Fabio Nobile, Raul Tempone, Clayton Webster,
c    A Sparse Grid Stochastic Collocation Method for Partial Differential
c    Equations with Random Input Data,
c    SIAM Journal on Numerical Analysis,
c    Volume 46, Number 5, 2008, pages 2309-2345.
c
      implicit none

      integer m
      parameter ( m = 21 )
      integer nx
      parameter ( nx = 101 )
      integer ny
      parameter ( ny = 101 )

      double precision cl
      character * ( 255 ) command_filename
      integer command_unit
      double precision d
      character * ( 255 ) data_filename
      integer data_unit
      double precision dc(nx,ny)
      double precision dc0
      integer i
      integer j
      double precision omega(m)
      integer seed
      double precision xmat(nx,ny)
      double precision xvec(nx)
      double precision ymat(nx,ny)
      double precision yvec(ny)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'NTW_CONTOUR'
      write ( *, '(a)' ) 
     &  '  Display contour or surface plots of the stochastic'
      write ( *, '(a)' ) 
     &  '  diffusivity function defined by DIFFUSIVITY_2D_NTW.'
c
c  Set the spatial grid.
c
      d = 1.0D+00
      call r8vec_linspace ( nx, 0.0D+00, d, xvec )
      call r8vec_linspace ( ny, 0.0D+00, d, yvec )

      call r8vec_mesh_2d ( nx, ny, xvec, yvec, xmat, ymat )
c
c  Sample OMEGA.
c  We rescale to  [-sqrt(3),sqrt(3)].
c
      seed = 123456789
      call r8vec_uniform_01 ( m, seed, omega )
      omega = ( 1.0D+00 - omega ) * ( - sqrt ( 3.0D+00 ) )
     &                  + omega   *     sqrt ( 3.0D+00 )
c
c  Evaluate the diffusivity field.
c
      cl = 0.1D+00
      dc0 = 0.5D+00
      call diffusivity_2d_ntw ( cl, dc0, m, omega, nx * ny, xmat, 
     &  ymat, dc )
c
c  Create a data file.
c
      data_filename = 'ntw_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 1, ny
        do i = 1, nx
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      xmat(i,j), ymat(i,j), dc(i,j)
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )     '  Created graphics data file "' // 
     &  trim ( data_filename ) // '".'
c
c  Create the command file.
c
      command_filename = 'ntw_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // 
     &  trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "ntw_contour.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
      write ( command_unit, '(a)' ) 'set zlabel "<---DC(X,Y)--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "NTW Stochastic diffusivity function"'
      write ( command_unit, '(a)' ) 'set contour'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set cntrparam levels 15'
      write ( command_unit, '(a)' ) '#set view map'
      write ( command_unit, '(a)' ) 'set view 65, 65'
      write ( command_unit, '(a)' ) 'set key'
      write ( command_unit, '(a)' ) 'splot "' // 
     &  trim ( data_filename ) // '"'

      close ( unit = command_unit )

      write ( *, '(a)' )     '  Created graphics command file "' // 
     &  trim ( command_filename ) // '".'

      return
      end
      subroutine xk_contour ( )

c*********************************************************************72
c
cc XK_CONTOUR displays contour plots of a 1D stochastic diffusivity function.
c
c  Discussion:
c
c    The diffusivity function is compute by DIFFUSIVITY_1D_XK.
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
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 51 )

      double precision dc(n)
      double precision dc_max
      double precision dc0
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      integer j
      integer seed
      double precision omega(m)
      double precision x(n)
      double precision x_min
      double precision x_max

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'XK_CONTOUR'
      write ( *, '(a)' ) '  Plot the stochastic diffusivity function'
      write ( *, '(a)' ) '  defined by DIFFUSIVITY_1D_XK.'
c
c  Set up the spatial grid.
c
      x_min = -1.0D+00
      x_max = +1.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
c
c  Sample the OMEGA values.
c
      seed = 123456789
      call r8vec_normal_01 ( m, seed, omega )
c
c  Compute the diffusivity field.
c
      dc0 = 10.0D+00
      call diffusivity_1d_xk ( dc0, m, omega, n, x, dc )
c
c  Create data file.
c
      data_filename = 'xk_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 1, n
        write ( data_unit, '(2x,g14.6,2x,g14.6)' ) x(j), dc(j)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )     '  Created graphics data file "' 
     &  // trim ( data_filename ) // '".'
c
c  Create the command file.
c
      call r8vec_max ( n, dc, dc_max )

      command_filename = 'xk_commands.txt'
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
      write ( command_unit, '(a)' ) 'set output "xk_contour.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---DC(X)--->"'
      write ( command_unit, '(a,g14.6,a)' ) 
     &  'set yrange [0.0:', dc_max, ']'
      write ( command_unit, '(a)' ) 
     &  'set title "XK Stochastic diffusivity function"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) 
     &  //     '" using 1:2 lw 3 linecolor rgb "red"'

      close ( unit = command_unit )

      write ( *, '(a)' )     '  Created graphics command file "' // 
     &  trim ( command_filename ) // '".'

      return
      end
