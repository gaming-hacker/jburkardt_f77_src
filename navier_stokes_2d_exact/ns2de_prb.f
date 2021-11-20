      program main

c*********************************************************************72
c
cc NS2DE_TEST tests the NS2DE library.
c
c  Location:
c
c    http://people.sc.fsu.edu/~jburkardt/f77_src/navier_stokes_2d_exact/ns2de_prb.f
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'NS2DE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the NS2DE library.'
c
c  Taylor Vortex Flow.
c
      call uvp_taylor_test ( )
      call uvp_taylor_test2 ( )
      call rhs_taylor_test ( )
      call resid_taylor_test ( )
      call gnuplot_taylor_test ( )
      call parameter_taylor_test ( )
c
c  Spiral Flow.
c
      call uvp_spiral_test ( )
      call uvp_spiral_test2 ( )
      call rhs_spiral_test ( )
      call resid_spiral_test ( )
      call gnuplot_spiral_test ( )
      call parameter_spiral_test ( )
c
c  Lucas Flow.
c
      call uvp_lucas_test ( )
      call uvp_lucas_test2 ( )
      call rhs_lucas_test ( )
      call resid_lucas_test ( )
      call gnuplot_lucas_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'NS2DE_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      call timestamp ( )

      return
      end
      subroutine uvp_taylor_test ( )

c*********************************************************************72
c
cc UVP_TAYLOR_TEST samples the solution at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision nu
      double precision p(n)
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_max
      double precision r8vec_min
      double precision rho
      integer seed
      double precision t
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'UVP_TAYLOR_TEST'
      write ( *, '(a)' ) '  Taylor Vortex Flow'
      write ( *, '(a)' ) 
     &  '  Estimate the range of velocity and pressure'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, using a region that is'
      write ( *, '(a)' ) 
     &  '  the square centered at (1.5,1.5) with "radius" 1.0,'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.5D+00
      r8_hi = +2.5D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      t = 0.0D+00

      call uvp_taylor ( nu, rho, n, x, y, t, u, v, p )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', 
     &  r8vec_min ( n, u ), r8vec_max ( n, u )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', 
     &  r8vec_min ( n, v ), r8vec_max ( n, v )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', 
     &  r8vec_min ( n, p ), r8vec_max ( n, p )

      return
      end
      subroutine uvp_taylor_test2 ( )

c*********************************************************************72
c
cc UVP_TAYLOR_TEST2 samples the solution on the boundary at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 400 )

      integer i
      double precision nu
      double precision p(n)
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_max
      double precision r8vec_min
      double precision rho
      integer seed
      double precision t
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D+00
      t = 0.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'UVP_TAYLOR_TEST2'
      write ( *, '(a)' ) '  Taylor Vortex Flow'
      write ( *, '(a)' ) 
     &  '  Estimate the range of velocity and pressure'
      write ( *, '(a)' )
     &  '  along the boundary'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, using a region that is'
      write ( *, '(a)' ) 
     &  '  the square centered at (1.5,1.5) with "radius" 1.0,'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.5D+00
      r8_hi = 2.5D+00

      call r8vec_linspace ( 100, r8_lo, r8_hi, x(1:100) )
      do i = 1, 100
        y(i) = r8_lo
      end do

      do i = 101, 200
        x(i) = r8_hi
      end do
      call r8vec_linspace ( 100, r8_lo, r8_hi, y(101:200) )

      call r8vec_linspace ( 100, r8_hi, r8_lo, x(201:300) )
      do i = 201, 300
        y(i) = r8_hi
      end do

      do i = 301, 400
        x(i) = r8_lo
      end do
      call r8vec_linspace ( 100, r8_lo, r8_hi, y(301:400) )

      call uvp_taylor ( nu, rho, n, x, y, t, u, v, p )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', 
     &  r8vec_min ( n, u ), r8vec_max ( n, u )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', 
     &  r8vec_min ( n, v ), r8vec_max ( n, v )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', 
     &  r8vec_min ( n, p ), r8vec_max ( n, p )

      return
      end
      subroutine rhs_taylor_test ( )

c*********************************************************************72
c
cc RHS_TAYLOR_TEST samples the right hand side at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision f(n)
      double precision g(n)
      double precision h(n)
      double precision nu
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_max
      double precision r8vec_min
      double precision rho
      integer seed
      double precision t
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RHS_TAYLOR_TEST'
      write ( *, '(a)' ) '  Taylor Vortex Flow'
      write ( *, '(a)' ) '  Sample the Navier-Stokes right hand sides'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, using a region that is'
      write ( *, '(a)' ) 
     &  '  the square centered at (1.5,1.5) with "radius" 1.0,'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.5D+00
      r8_hi = +2.5D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      t = 0.0D+00

      call rhs_taylor ( nu, rho, n, x, y, t, f, g, h )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  F:  ', 
     &  r8vec_min ( n, f ), r8vec_max ( n, f )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  G:  ', 
     &  r8vec_min ( n, g ), r8vec_max ( n, g )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  H:  ', 
     &  r8vec_min ( n, h ), r8vec_max ( n, h )

      return
      end
      subroutine resid_taylor_test ( )

c*********************************************************************72
c
cc RESID_TAYLOR_TEST samples the residual at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision nu
      double precision pr(n)
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_amax
      double precision r8vec_amin
      double precision rho
      integer seed
      double precision t
      double precision ur(n)
      double precision vr(n)
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RESID_TAYLOR_TEST'
      write ( *, '(a)' ) '  Taylor Vortex Flow'
      write ( *, '(a)' ) '  Sample the Navier-Stokes residuals'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, using a region that is'
      write ( *, '(a)' ) 
     &  '  the square centered at (1.5,1.5) with "radius" 1.0,'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.5D+00
      r8_hi = +2.5D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      t = 0.0D+00

      call resid_taylor ( nu, rho, n, x, y, t, ur, vr, pr )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  Ur:  ', 
     &  r8vec_amin ( n, ur ), r8vec_amax ( n, ur )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  Vr:  ', 
     &  r8vec_amin ( n, vr ), r8vec_amax ( n, vr )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  Pr:  ', 
     &  r8vec_amin ( n, pr ), r8vec_amax ( n, pr )

      return
      end
      subroutine gnuplot_taylor_test ( )

c*********************************************************************72
c
cc GNUPLOT_TAYLOR_TEST generates a field on a regular grid and plots it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer x_num
      parameter ( x_num = 21 )
      integer y_num
      parameter ( y_num = 21 )

      character * ( 255 ) header
      integer n
      double precision nu
      double precision p(x_num,y_num)
      double precision rho
      double precision s
      integer seed
      double precision t
      double precision u(x_num,y_num)
      double precision v(x_num,y_num)
      double precision x(x_num,y_num)
      double precision x_hi
      double precision x_lo
      double precision y(x_num,y_num)
      double precision y_hi
      double precision y_lo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'GNUPLOT_TAYLOR_TEST:'
      write ( *, '(a)' ) '  Taylor Vortex Flow'
      write ( *, '(a)' ) 
     &  '  Generate a Taylor vortex velocity field on a regular grid.'
      write ( *, '(a)' ) '  Store in GNUPLOT data and command files.'
   
      x_lo = 0.5D+00
      x_hi = 2.5D+00

      y_lo = 0.5D+00
      y_hi = 2.5D+00

      call grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

      nu = 1.0D+00
      rho = 1.0D+00
      n = x_num * y_num
      t = 0.0D+00

      call uvp_taylor ( nu, rho, n, x, y, t, u, v, p )

      header = 'taylor'
      s = 0.10D+00
      call ns2de_gnuplot ( header, n, x, y, u, v, s )

      return
      end
      subroutine parameter_taylor_test ( )

c*********************************************************************72
c
cc PARAMETER_TAYLOR_TEST monitors solution norms for various values of NU, RHO.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      integer i
      integer j
      integer k
      double precision nu
      double precision p(n)
      double precision p_norm
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_norm_l2
      double precision rho
      integer seed
      double precision t
      double precision u(n)
      double precision u_norm
      double precision v(n)
      double precision v_norm
      double precision x(n)
      double precision y(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'PARAMETER_TAYLOR_TEST'
      write ( *, '(a)' ) '  Taylor Vortex Flow'
      write ( *, '(a)' ) '  Monitor solution norms over time for'
      write ( *, '(a)' ) '  varous values of NU, RHO.'

      r8_lo = 0.5D+00
      r8_hi = +2.5D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
c
c  Vary RHO.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  RHO affects the pressure scaling.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     RHO         NU           T' //
     &  '     ||U||       ||V||       ||P||'
      write ( *, '(a)' ) ''

      nu = 1.0D+00
      rho = 1.0D+00

      do j = 1, 3

        do k = 0, 5

          t = real ( k, kind = 8 ) / 5.0D+00

          call uvp_taylor ( nu, rho, n, x, y, t, u, v, p )

          u_norm = r8vec_norm_l2 ( n, u ) / dble ( n )
          v_norm = r8vec_norm_l2 ( n, v ) / dble ( n )
          p_norm = r8vec_norm_l2 ( n, p ) / dble ( n )

          write ( *, '(2x,g10.4,2x,g10.4,2x,f8.4,2x,g10.4,' //
     &      '2x,g10.4,2x,g10.4)' ) 
     &      rho, nu, t, u_norm, v_norm, p_norm

        end do

        write ( *, '(a)' ) ''
        rho = rho / 100.0D+00

      end do
c
c  Vary NU.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  NU affects the time scaling.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     RHO         NU           T' //
     &  '     ||U||       ||V||       ||P||'
      write ( *, '(a)' ) ''

      nu = 1.0D+00
      rho = 1.0D+00
  
      do i = 1, 4

        do k = 0, 5

          t = real ( k, kind = 8 ) / 5.0D+00

          call uvp_taylor ( nu, rho, n, x, y, t, u, v, p )

          u_norm = r8vec_norm_l2 ( n, u ) / dble ( n )
          v_norm = r8vec_norm_l2 ( n, v ) / dble ( n )
          p_norm = r8vec_norm_l2 ( n, p ) / dble ( n )

          write ( *, '(2x,g10.4,2x,g10.4,2x,f8.4,2x,g10.4,' //
     &      '2x,g10.4,2x,g10.4)' ) 
     &      rho, nu, t, u_norm, v_norm, p_norm

        end do

        write ( *, '(a)' ) ''

        nu = nu / 10.0D+00

      end do

      return
      end
      subroutine uvp_spiral_test ( )

c*********************************************************************72
c
cc UVP_SPIRAL_TEST samples the Spiral solution at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision nu
      double precision p(n)
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_max
      double precision r8vec_min
      double precision rho
      integer seed
      double precision t
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'UVP_SPIRAL_TEST'
      write ( *, '(a)' ) '  Spiral Flow'
      write ( *, '(a)' ) 
     &  '  Estimate the range of velocity and pressure'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, using a region that is'
      write ( *, '(a)' ) 
     &  '  the unit square.'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.0D+00
      r8_hi = 1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      t = 0.0D+00

      call uvp_spiral ( nu, rho, n, x, y, t, u, v, p )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', 
     &  r8vec_min ( n, u ), r8vec_max ( n, u )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', 
     &  r8vec_min ( n, v ), r8vec_max ( n, v )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', 
     &  r8vec_min ( n, p ), r8vec_max ( n, p )

      return
      end
      subroutine uvp_spiral_test2 ( )

c*********************************************************************72
c
cc UVP_SPIRAL_TEST2 samples the Spiral solution on the boundary at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 400 )

      integer i
      double precision nu
      double precision p(n)
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_max
      double precision r8vec_min
      double precision rho
      integer seed
      double precision t
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D+00
      t = 0.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'UVP_SPIRAL_TEST2'
      write ( *, '(a)' ) '  Spiral Flow'
      write ( *, '(a)' ) 
     &  '  Estimate the range of velocity and pressure'
      write ( *, '(a)' ) '  along the boundary'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, using a region that is'
      write ( *, '(a)' ) 
     &  '  the unit square.'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.0D+00
      r8_hi = 1.0D+00

      call r8vec_linspace ( 100, r8_lo, r8_hi, x(1:100) )
      do i = 1, 100
        y(i) = r8_lo
      end do

      do i = 101, 200
        x(i) = r8_hi
      end do
      call r8vec_linspace ( 100, r8_lo, r8_hi, y(101:200) )

      call r8vec_linspace ( 100, r8_hi, r8_lo, x(201:300) )
      do i = 201, 300
        y(i) = r8_hi
      end do

      do i = 301, 400
        x(i) = r8_lo
      end do

      call r8vec_linspace ( 100, r8_lo, r8_hi, y(301:400) )

      call uvp_spiral ( nu, rho, n, x, y, t, u, v, p )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', 
     &  r8vec_min ( n, u ), r8vec_max ( n, u )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', 
     &  r8vec_min ( n, v ), r8vec_max ( n, v )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', 
     &  r8vec_min ( n, p ), r8vec_max ( n, p )

      return
      end
      subroutine rhs_spiral_test ( )

c*********************************************************************72
c
cc RHS_SPIRAL_TEST samples the right hand side at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision f(n)
      double precision g(n)
      double precision h(n)
      double precision nu
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_max
      double precision r8vec_min
      double precision rho
      integer seed
      double precision t
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RHS_SPIRAL_TEST'
      write ( *, '(a)' ) '  Spiral Flow'
      write ( *, '(a)' ) '  Sample the Navier-Stokes right hand sides'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, in the unit square.'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.0D+00
      r8_hi = 1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      t = 0.0D+00

      call rhs_spiral ( nu, rho, n, x, y, t, f, g, h )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  F:  ', 
     &  r8vec_min ( n, f ), r8vec_max ( n, f )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  G:  ', 
     &  r8vec_min ( n, g ), r8vec_max ( n, g )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  H:  ', 
     &  r8vec_min ( n, h ), r8vec_max ( n, h )

      return
      end
      subroutine resid_spiral_test ( )

c*********************************************************************72
c
cc RESID_SPIRAL_TEST samples the residual at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision nu
      double precision pr(n)
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_amax
      double precision r8vec_amin
      double precision rho
      integer seed
      double precision t
      double precision ur(n)
      double precision vr(n)
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RESID_SPIRAL_TEST'
      write ( *, '(a)' ) '  Spiral Flow'
      write ( *, '(a)' ) '  Sample the Navier-Stokes residuals'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, in the unit square.'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.0D+00
      r8_hi = 1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      t = 0.0D+00

      call resid_spiral ( nu, rho, n, x, y, t, ur, vr, pr )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  Ur:  ', 
     &  r8vec_amin ( n, ur ), r8vec_amax ( n, ur )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  Vr:  ', 
     &  r8vec_amin ( n, vr ), r8vec_amax ( n, vr )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  Pr:  ', 
     &  r8vec_amin ( n, pr ), r8vec_amax ( n, pr )

      return
      end
      subroutine gnuplot_spiral_test ( )

c*********************************************************************72
c
cc GNUPLOT_SPIRAL_TEST generates a field on a regular grid and plots it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer x_num
      parameter ( x_num = 21 )
      integer y_num
      parameter ( y_num = 21 )

      character * ( 255 ) header
      integer n
      double precision nu
      double precision p(x_num,y_num)
      double precision rho
      double precision s
      integer seed
      double precision t
      double precision u(x_num,y_num)
      double precision v(x_num,y_num)
      double precision x(x_num,y_num)
      double precision x_hi
      double precision x_lo
      double precision y(x_num,y_num)
      double precision y_hi
      double precision y_lo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'GNUPLOT_SPIRAL_TEST:'
      write ( *, '(a)' ) '  Spiral Flow'
      write ( *, '(a)' ) 
     &  '  Generate a velocity field on a regular grid.'
      write ( *, '(a)' ) '  Store in GNUPLOT data and command files.'
   
      x_lo = 0.0D+00
      x_hi = 1.0D+00

      y_lo = 0.0D+00
      y_hi = 1.0D+00

      call grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

      nu = 1.0D+00
      rho = 1.0D+00
      n = x_num * y_num
      t = 0.0D+00

      call uvp_spiral ( nu, rho, n, x, y, t, u, v, p )

      header = 'spiral'
      s = 5.00D+00
      call ns2de_gnuplot ( header, n, x, y, u, v, s )

      return
      end
      subroutine parameter_spiral_test ( )

c*********************************************************************72
c
cc PARAMETER_SPIRAL_TEST monitors solution norms for various values of NU, RHO.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      integer i
      integer j
      integer k
      double precision nu
      double precision p(n)
      double precision p_norm
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_norm_l2
      double precision rho
      integer seed
      double precision t
      double precision u(n)
      double precision u_norm
      double precision v(n)
      double precision v_norm
      double precision x(n)
      double precision y(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'PARAMETER_SPIRAL_TEST'
      write ( *, '(a)' ) '  Spiral Flow'
      write ( *, '(a)' ) '  Monitor solution norms over time for'
      write ( *, '(a)' ) '  varous values of NU, RHO.'

      r8_lo = 0.0D+00
      r8_hi = 1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
c
c  Vary RHO.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  RHO affects the pressure scaling.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     RHO         NU           T' //
     &  '     ||U||       ||V||       ||P||'
      write ( *, '(a)' ) ''

      nu = 1.0D+00
      rho = 1.0D+00

      do j = 1, 3

        do k = 0, 5

          t = real ( k, kind = 8 ) / 5.0D+00

          call uvp_spiral ( nu, rho, n, x, y, t, u, v, p )

          u_norm = r8vec_norm_l2 ( n, u ) / dble ( n )
          v_norm = r8vec_norm_l2 ( n, v ) / dble ( n )
          p_norm = r8vec_norm_l2 ( n, p ) / dble ( n )

          write ( *, '(2x,g10.4,2x,g10.4,2x,f8.4,2x,g10.4,' //
     &      '2x,g10.4,2x,g10.4)' ) 
     &      rho, nu, t, u_norm, v_norm, p_norm

        end do

        write ( *, '(a)' ) ''
        rho = rho / 100.0D+00

      end do
c
c  Vary NU.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  NU affects the time scaling.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     RHO         NU           T' //
     &  '     ||U||       ||V||       ||P||'
      write ( *, '(a)' ) ''

      nu = 1.0D+00
      rho = 1.0D+00
  
      do i = 1, 4

        do k = 0, 5

          t = real ( k, kind = 8 ) / 5.0D+00

          call uvp_spiral ( nu, rho, n, x, y, t, u, v, p )

          u_norm = r8vec_norm_l2 ( n, u ) / dble ( n )
          v_norm = r8vec_norm_l2 ( n, v ) / dble ( n )
          p_norm = r8vec_norm_l2 ( n, p ) / dble ( n )

          write ( *, '(2x,g10.4,2x,g10.4,2x,f8.4,2x,g10.4,' //
     &      '2x,g10.4,2x,g10.4)' ) 
     &      rho, nu, t, u_norm, v_norm, p_norm

        end do

        write ( *, '(a)' ) ''

        nu = nu / 10.0D+00

      end do

      return
      end
      subroutine uvp_lucas_test ( )

c*********************************************************************72
c
cc UVP_LUCAS_TEST samples the Lucas Bystricky solution at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision nu
      double precision p(n)
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_max
      double precision r8vec_min
      double precision rho
      integer seed
      double precision t
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'UVP_LUCAS_TEST'
      write ( *, '(a)' ) '  Lucas Bystricky Flow'
      write ( *, '(a)' ) 
     &  '  Estimate the range of velocity and pressure'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, over the unit square.'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.0D+00
      r8_hi = +1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      t = 0.0D+00

      call uvp_lucas ( nu, rho, n, x, y, t, u, v, p )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', 
     &  r8vec_min ( n, u ), r8vec_max ( n, u )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', 
     &  r8vec_min ( n, v ), r8vec_max ( n, v )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', 
     &  r8vec_min ( n, p ), r8vec_max ( n, p )

      return
      end
      subroutine uvp_lucas_test2 ( )

c*********************************************************************72
c
cc UVP_LUCAS_TEST2 samples the Lucas Bystricky solution on the boundary at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 400 )

      integer i
      double precision nu
      double precision p(n)
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_max
      double precision r8vec_min
      double precision rho
      integer seed
      double precision t
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D+00
      t = 0.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'UVP_LUCAS_TEST2'
      write ( *, '(a)' ) '  Lucas Bystricky Flow'
      write ( *, '(a)' ) 
     &  '  Estimate the range of velocity and pressure'
      write ( *, '(a)' ) '  along the boundary'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, over the unit square.'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.0D+00
      r8_hi = 1.0D+00

      call r8vec_linspace ( 100, r8_lo, r8_hi, x(1:100) )
      do i = 1, 100
        y(i) = r8_lo
      end do

      do i = 101, 200
        x(i) = r8_hi
      end do
      call r8vec_linspace ( 100, r8_lo, r8_hi, y(101:200) )

      call r8vec_linspace ( 100, r8_hi, r8_lo, x(201:300) )
      do i = 201, 300
        y(i) = r8_hi
      end do

      do i = 301, 400
        x(i) = r8_lo
      end do
      call r8vec_linspace ( 100, r8_lo, r8_hi, y(301:400) )

      call uvp_lucas ( nu, rho, n, x, y, t, u, v, p )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', 
     &  r8vec_min ( n, u ), r8vec_max ( n, u )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', 
     &  r8vec_min ( n, v ), r8vec_max ( n, v )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', 
     &  r8vec_min ( n, p ), r8vec_max ( n, p )

      return
      end
      subroutine rhs_lucas_test ( )

c*********************************************************************72
c
cc RHS_LUCAS_TEST samples the Lucas Bystricky right hand side.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision f(n)
      double precision g(n)
      double precision h(n)
      double precision nu
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_max
      double precision r8vec_min
      double precision rho
      integer seed
      double precision t
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RHS_LUCAS_TEST'
      write ( *, '(a)' ) '  Lucas Bystricky Flow'
      write ( *, '(a)' ) '  Sample the Navier-Stokes right hand sides'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, in the unit square.'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.0D+00
      r8_hi = 1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      t = 0.0D+00

      call rhs_lucas ( nu, rho, n, x, y, t, f, g, h )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  F:  ', 
     &  r8vec_min ( n, f ), r8vec_max ( n, f )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  G:  ', 
     &  r8vec_min ( n, g ), r8vec_max ( n, g )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  H:  ', 
     &  r8vec_min ( n, h ), r8vec_max ( n, h )

      return
      end
      subroutine resid_lucas_test ( )

c*********************************************************************72
c
cc RESID_LUCAS_TEST samples the Lucas Bystricky residual at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision nu
      double precision pr(n)
      double precision r8_hi
      double precision r8_lo
      double precision r8vec_amax
      double precision r8vec_amin
      double precision rho
      integer seed
      double precision t
      double precision ur(n)
      double precision vr(n)
      double precision x(n)
      double precision y(n)

      nu = 1.0D+00
      rho = 1.0D00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RESID_LUCAS_TEST'
      write ( *, '(a)' ) '  Lucas Bystricky Flow'
      write ( *, '(a)' ) '  Sample the Navier-Stokes residuals'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, in the unit square.'
      write ( *, '(a,g14.6)' ) '  Kinematic viscosity NU = ', nu
      write ( *, '(a,g14.6)' ) '  Fluid density RHO = ', rho

      r8_lo = 0.0D+00
      r8_hi = 1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      t = 0.0D+00

      call resid_lucas ( nu, rho, n, x, y, t, ur, vr, pr )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  Ur:  ', 
     &  r8vec_amin ( n, ur ), r8vec_amax ( n, ur )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  Vr:  ', 
     &  r8vec_amin ( n, vr ), r8vec_amax ( n, vr )
      write ( *, '(a,2x,g14.6,2x,g14.6)' ) '  Pr:  ', 
     &  r8vec_amin ( n, pr ), r8vec_amax ( n, pr )

      return
      end
      subroutine gnuplot_lucas_test ( )

c*********************************************************************72
c
cc GNUPLOT_LUCAS_TEST generates a field on a regular grid and plots it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer x_num
      parameter ( x_num = 21 )
      integer y_num
      parameter ( y_num = 21 )

      character * ( 255 ) header
      integer n
      double precision nu
      double precision p(x_num,y_num)
      double precision rho
      double precision s
      integer seed
      double precision t
      double precision u(x_num,y_num)
      double precision v(x_num,y_num)
      double precision x(x_num,y_num)
      double precision x_hi
      double precision x_lo
      double precision y(x_num,y_num)
      double precision y_hi
      double precision y_lo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'GNUPLOT_LUCAS_TEST:'
      write ( *, '(a)' ) '  Lucas Bystricky Flow'
      write ( *, '(a)' ) 
     &  '  Generate a velocity field on a regular grid.'
      write ( *, '(a)' ) '  Store in GNUPLOT data and command files.'
   
      x_lo = 0.0D+00
      x_hi = 1.0D+00

      y_lo = 0.0D+00
      y_hi = 1.0D+00

      call grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

      nu = 1.0D+00
      rho = 1.0D+00
      n = x_num * y_num
      t = 0.0D+00

      call uvp_lucas ( nu, rho, n, x, y, t, u, v, p )

      header = 'lucas'
      s = 0.25D+00
      call ns2de_gnuplot ( header, n, x, y, u, v, s )

      return
      end