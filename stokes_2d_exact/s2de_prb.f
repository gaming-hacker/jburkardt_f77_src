      program main

c*********************************************************************72
c
cc S2DE_TEST tests the S2DE library.
c
c  Location:
c
c    http://people.sc.fsu.edu/~jburkardt/f77_src/stokes_2d_exact/s2de_prb.f
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'S2DE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the S2DE library.'

      call uvp_stokes1_test ( )
      call resid_stokes1_test ( )
      call gnuplot_stokes1_test ( )

      call uvp_stokes2_test ( )
      call resid_stokes2_test ( )
      call gnuplot_stokes2_test ( )

      call uvp_stokes3_test ( )
      call resid_stokes3_test ( )
      call gnuplot_stokes3_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'S2DE_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      call timestamp ( )

      return
      end
      subroutine uvp_stokes1_test ( )

c*********************************************************************72
c
cc UVP_STOKES1_TEST samples the solution #1.
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

      double precision p(n)
      double precision r8vec_max
      double precision r8vec_min
      integer seed
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision xy_hi
      double precision xy_lo
      double precision y(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'UVP_STOKES1_TEST'
      write ( *, '(a)' ) '  Exact Stokes solution #1:'
      write ( *, '(a)' ) 
     &  '  Estimate the range of velocity and pressure'
      write ( *, '(a)' ) 
     &  '  using a region that is the unit square.'

      xy_lo = 0.0D+00
      xy_hi = 1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

      call uvp_stokes1 ( n, x, y, u, v, p )

      write (  *, '(a)' ) ''
      write (  *, '(a)' ) '           Minimum       Maximum'
      write (  *, '(a)' ) ''
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', 
     &  r8vec_min ( n, u ), r8vec_max ( n, u )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', 
     &  r8vec_min ( n, v ), r8vec_max ( n, v )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', 
     &  r8vec_min ( n, p ), r8vec_max ( n, p )

      return
      end
      subroutine resid_stokes1_test ( )

c*********************************************************************72
c
cc RESID_STOKES1_TEST samples the residual for solution #1.
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

      double precision pr(n)
      double precision r8vec_amax
      double precision r8vec_amin
      integer seed
      double precision ur(n)
      double precision vr(n)
      double precision x(n)
      double precision xy_hi
      double precision xy_lo
      double precision y(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RESID_STOKES1_TEST'
      write ( *, '(a)' ) '  Exact Stokes solution #1:'
      write ( *, '(a)' ) '  Sample the Stokes residuals'
      write ( *, '(a)' ) 
     &  '  using a region that is the unit square.'

      xy_lo = 0.0D+00
      xy_hi = 1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

      call resid_stokes1 ( n, x, y, ur, vr, pr )

      write (  *, '(a)' ) ''
      write (  *, '(a)' ) '           Minimum       Maximum'
      write (  *, '(a)' ) ''
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Ur:  ', 
     &  r8vec_amin ( n, ur ), r8vec_amax ( n, ur )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Vr:  ', 
     &  r8vec_amin ( n, vr ), r8vec_amax ( n, vr )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Pr:  ', 
     &  r8vec_amin ( n, pr ), r8vec_amax ( n, pr )

      return
      end
      subroutine gnuplot_stokes1_test ( )

c*********************************************************************72
c
cc GNUPLOT_STOKES1_TEST plots solution #1 on a regular grid.
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

      integer x_num
      parameter ( x_num = 21 )
      integer y_num
      parameter ( y_num = 21 )

      character * ( 255 ) header
      integer n
      double precision p(x_num,y_num)
      double precision s
      integer seed
      double precision u(x_num,y_num)
      double precision v(x_num,y_num)
      double precision x(x_num,y_num)
      double precision x_hi
      double precision x_lo
      double precision y(x_num,y_num)
      double precision y_hi
      double precision y_lo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'GNUPLOT_STOKES1_TEST:'
      write ( *, '(a)' ) '  Exact Stokes solution #1:'
      write ( *, '(a)' ) 
     &  '  Generate a Stokes velocity field on a regular grid.'
      write ( *, '(a)' ) '  Store in GNUPLOT data and command files.'
   
      x_lo = 0.0D+00
      x_hi = 1.0D+00

      y_lo = 0.0D+00
      y_hi = 1.0D+00

      call grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

      n = x_num * y_num

      call uvp_stokes1 ( n, x, y, u, v, p )

      header = 'stokes1'
      s = 4.0D+00
      call stokes_gnuplot ( header, n, x, y, u, v, s )

      return
      end
      subroutine uvp_stokes2_test ( )

c*********************************************************************72
c
cc UVP_STOKES2_TEST samples the solution #2.
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

      double precision p(n)
      double precision r8vec_max
      double precision r8vec_min
      integer seed
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision xy_hi
      double precision xy_lo
      double precision y(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'UVP_STOKES2_TEST'
      write ( *, '(a)' ) '  Exact Stokes solution #2:'
      write ( *, '(a)' ) 
     &  '  Estimate the range of velocity and pressure'
      write ( *, '(a)' ) 
     &  '  using a region that is the unit square.'

      xy_lo = 0.0D+00
      xy_hi = 1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

      call uvp_stokes2 ( n, x, y, u, v, p )

      write (  *, '(a)' ) ''
      write (  *, '(a)' ) '           Minimum       Maximum'
      write (  *, '(a)' ) ''
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', 
     &  r8vec_min ( n, u ), r8vec_max ( n, u )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', 
     &  r8vec_min ( n, v ), r8vec_max ( n, v )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', 
     &  r8vec_min ( n, p ), r8vec_max ( n, p )

      return
      end
      subroutine resid_stokes2_test ( )

c*********************************************************************72
c
cc RESID_STOKES2_TEST samples the residual for solution #1.
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

      double precision pr(n)
      double precision r8vec_amax
      double precision r8vec_amin
      integer seed
      double precision ur(n)
      double precision vr(n)
      double precision x(n)
      double precision xy_hi
      double precision xy_lo
      double precision y(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RESID_STOKES2_TEST'
      write ( *, '(a)' ) '  Exact Stokes solution #2:'
      write ( *, '(a)' ) '  Sample the Stokes residuals'
      write ( *, '(a)' ) 
     &  '  using a region that is the unit square.'

      xy_lo = 0.0D+00
      xy_hi = 1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

      call resid_stokes2 ( n, x, y, ur, vr, pr )

      write (  *, '(a)' ) ''
      write (  *, '(a)' ) '           Minimum       Maximum'
      write (  *, '(a)' ) ''
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Ur:  ', 
     &  r8vec_amin ( n, ur ), r8vec_amax ( n, ur )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Vr:  ', 
     &  r8vec_amin ( n, vr ), r8vec_amax ( n, vr )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Pr:  ', 
     &  r8vec_amin ( n, pr ), r8vec_amax ( n, pr )

      return
      end
      subroutine gnuplot_stokes2_test ( )

c*********************************************************************72
c
cc GNUPLOT_STOKES2_TEST plots solution #2 on a regular grid.
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

      integer x_num
      parameter ( x_num = 21 )
      integer y_num
      parameter ( y_num = 21 )

      character * ( 255 ) header
      integer n
      double precision p(x_num,y_num)
      double precision s
      integer seed
      double precision u(x_num,y_num)
      double precision v(x_num,y_num)
      double precision x(x_num,y_num)
      double precision x_hi
      double precision x_lo
      double precision y(x_num,y_num)
      double precision y_hi
      double precision y_lo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'GNUPLOT_STOKES2_TEST:'
      write ( *, '(a)' ) '  Exact Stokes solution #2:'
      write ( *, '(a)' ) 
     &  '  Generate a Stokes velocity field on a regular grid.'
      write ( *, '(a)' ) '  Store in GNUPLOT data and command files.'
   
      x_lo = 0.0D+00
      x_hi = 1.0D+00

      y_lo = 0.0D+00
      y_hi = 1.0D+00

      call grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

      n = x_num * y_num

      call uvp_stokes2 ( n, x, y, u, v, p )

      header = 'stokes2'
      s = 0.05D+00
      call stokes_gnuplot ( header, n, x, y, u, v, s )

      return
      end
      subroutine uvp_stokes3_test ( )

c*********************************************************************72
c
cc UVP_STOKES3_TEST samples the solution #3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision p(n)
      double precision r8vec_max
      double precision r8vec_min
      integer seed
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision xy_hi
      double precision xy_lo
      double precision y(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'UVP_STOKES3_TEST'
      write ( *, '(a)' ) '  Exact Stokes solution #3:'
      write ( *, '(a)' ) 
     &  '  Estimate the range of velocity and pressure'
      write ( *, '(a)' ) 
     &  '  using a region that is [-1,+1]x[-1,+1].'

      xy_lo = -1.0D+00
      xy_hi = +1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

      call uvp_stokes3 ( n, x, y, u, v, p )

      write (  *, '(a)' ) ''
      write (  *, '(a)' ) '           Minimum       Maximum'
      write (  *, '(a)' ) ''
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', 
     &  r8vec_min ( n, u ), r8vec_max ( n, u )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', 
     &  r8vec_min ( n, v ), r8vec_max ( n, v )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  P:  ', 
     &  r8vec_min ( n, p ), r8vec_max ( n, p )

      return
      end
      subroutine resid_stokes3_test ( )

c*********************************************************************72
c
cc RESID_STOKES3_TEST samples the residual for solution #3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision pr(n)
      double precision r8vec_amax
      double precision r8vec_amin
      integer seed
      double precision ur(n)
      double precision vr(n)
      double precision x(n)
      double precision xy_hi
      double precision xy_lo
      double precision y(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RESID_STOKES3_TEST'
      write ( *, '(a)' ) '  Exact Stokes solution #3:'
      write ( *, '(a)' ) '  Sample the Stokes residuals'
      write ( *, '(a)' ) 
     &  '  using a region that is [-1,+1]x[-1,+1].'

      xy_lo = -1.0D+00
      xy_hi = +1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )

      call resid_stokes3 ( n, x, y, ur, vr, pr )

      write (  *, '(a)' ) ''
      write (  *, '(a)' ) '           Minimum       Maximum'
      write (  *, '(a)' ) ''
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Ur:  ', 
     &  r8vec_amin ( n, ur ), r8vec_amax ( n, ur )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Vr:  ', 
     &  r8vec_amin ( n, vr ), r8vec_amax ( n, vr )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Pr:  ', 
     &  r8vec_amin ( n, pr ), r8vec_amax ( n, pr )

      return
      end
      subroutine gnuplot_stokes3_test ( )

c*********************************************************************72
c
cc GNUPLOT_STOKES3_TEST plots solution #3 on a regular grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 2015
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
      double precision p(x_num,y_num)
      double precision s
      integer seed
      double precision u(x_num,y_num)
      double precision v(x_num,y_num)
      double precision x(x_num,y_num)
      double precision x_hi
      double precision x_lo
      double precision y(x_num,y_num)
      double precision y_hi
      double precision y_lo

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'GNUPLOT_STOKES3_TEST:'
      write ( *, '(a)' ) '  Exact Stokes solution #3:'
      write ( *, '(a)' ) 
     &  '  Generate a Stokes velocity field on [-1,+1]x[-1,+1].'
      write ( *, '(a)' ) '  Store in GNUPLOT data and command files.'
   
      x_lo = -1.0D+00
      x_hi = +1.0D+00

      y_lo = -1.0D+00
      y_hi = +1.0D+00

      call grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

      n = x_num * y_num

      call uvp_stokes3 ( n, x, y, u, v, p )

      header = 'stokes3'
      s = 0.05D+00
      call stokes_gnuplot ( header, n, x, y, u, v, s )

      return
      end