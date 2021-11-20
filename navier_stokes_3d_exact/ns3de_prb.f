      program main

c*********************************************************************72
c
cc NS3DE_PRB tests the NS3DE library.
c
c  Location:
c
c    http://people.sc.fsu.edu/~jburkardt/f77_src/navier_stokes_3d_exact/ns3de_prb.f
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'NS3DE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the NS3DE library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'NS3DE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 samples the solution at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision a
      double precision d
      double precision p(n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8vec_max
      double precision r8vec_min
      integer seed
      double precision t
      double precision u(n)
      double precision v(n)
      double precision w(n)
      double precision x(n)
      double precision xyz_hi
      double precision xyz_lo
      double precision y(n)
      double precision z(n)

      a = r8_pi / 4.0D+00
      d = r8_pi / 2.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Estimate the range of velocity and pressure'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, in a region that is the'
      write ( *, '(a)' ) '  cube centered at (0,0,0) with "radius" 1.0.'
      write ( *, '(a,g14.6)' ) '  Parameter A = ', a
      write ( *, '(a,g14.6)' ) '  Parameter D = ', d

      xyz_lo = -1.0D+00
      xyz_hi = +1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, x )
      call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, y )
      call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, z )
      t = 0.0D+00

      call uvwp_ethier ( a, d, n, x, y, z, t, u, v, w, p )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6,2x,g14.6)' ) 
     &  '  U:  ', r8vec_min ( n, u ), r8vec_max ( n, u )
      write ( *, '(a,g14.6,2x,g14.6)' ) 
     &  '  V:  ', r8vec_min ( n, v ), r8vec_max ( n, v )
      write ( *, '(a,g14.6,2x,g14.6)' ) 
     &  '  W:  ', r8vec_min ( n, w ), r8vec_max ( n, w )
      write ( *, '(a,g14.6,2x,g14.6)' ) 
     &  '  P:  ', r8vec_min ( n, p ), r8vec_max ( n, p )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 samples the residual at the initial time.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision a
      double precision d
      double precision pr(n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8vec_amax
      double precision r8vec_amin
      integer seed
      double precision t
      double precision ur(n)
      double precision vr(n)
      double precision wr(n)
      double precision x(n)
      double precision xyz_hi
      double precision xyz_lo
      double precision y(n)
      double precision z(n)

      a = r8_pi / 4.0D+00
      d = r8_pi / 2.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Sample the Navier-Stokes residuals'
      write ( *, '(a)' ) 
     &  '  at the initial time T = 0, using a region that is'
      write ( *, '(a)' ) 
     &  '  the cube centered at (0,0,0) with "radius" 1.0,'
      write ( *, '(a,g14.6)' ) '  Parameter A = ', a
      write ( *, '(a,g14.6)' ) '  Parameter D = ', d

      xyz_lo = -1.0D+00
      xyz_hi = +1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, x )
      call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, y )
      call r8vec_uniform_ab ( n, xyz_lo, xyz_hi, seed, z )
      t = 0.0D+00

      call resid_ethier ( a, d, n, x, y, z, t, ur, vr, wr, pr )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '           Minimum       Maximum'
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6,2x,g14.6)' ) 
     &  '  Ur:  ', r8vec_amin ( n, ur ), r8vec_amax ( n, ur )
      write ( *, '(a,g14.6,2x,g14.6)' ) 
     &  '  Vr:  ', r8vec_amin ( n, vr ), r8vec_amax ( n, vr )
      write ( *, '(a,g14.6,2x,g14.6)' ) 
     &  '  Wr:  ', r8vec_amin ( n, wr ), r8vec_amax ( n, wr )
      write ( *, '(a,g14.6,2x,g14.6)' ) 
     &  '  Pr:  ', r8vec_amin ( n, pr ), r8vec_amax ( n, pr )

      return
      end

