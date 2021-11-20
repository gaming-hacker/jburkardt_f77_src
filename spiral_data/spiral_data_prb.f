      program main

c*********************************************************************72
c
cc SPIRAL_DATA_PRB tests the SPIRAL_DATA library.
c
c  Location:
c
c    http://people.sc.fsu.edu/~jburkardt/f77_src/spiral_data/spiral_data_prb.f
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPIRAL_DATA_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SPIRAL_DATA library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPIRAL_DATA_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 generates a field and estimates its range.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision c
      double precision r8vec_max
      double precision r8vec_min
      integer seed
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision xy_hi
      double precision xy_lo
      double precision y(n)

      write (  *, '(a)' ) ''
      write (  *, '(a)' ) 'TEST01'
      write (  *, '(a)' ) 
     &  '  Sample a spiral velocity field and estimate'
      write (  *, '(a)' ) '  the range of the solution values.'

      xy_lo = +0.0D+00
      xy_hi = +1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )
      c = 1.0D+00

      call uv_spiral ( n, x, y, c, u, v )

      write (  *, '(a)' ) ''
      write (  *, '(a)' ) '           Minimum       Maximum'
      write (  *, '(a)' ) ''
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  U:  ', 
     &  r8vec_min ( n, u ), r8vec_max ( n, u )
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  V:  ', 
     &  r8vec_min ( n, v ), r8vec_max ( n, v )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 generates a field and samples its residuals.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision c
      double precision pr(n)
      double precision r8vec_amax
      double precision r8vec_amin
      integer seed
      double precision x(n)
      double precision xy_hi
      double precision xy_lo
      double precision y(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  Sample a spiral velocity field and estimate the'
      write ( *, '(a)' ) 
     &  '  range of residuals in the continuity equation.'

      xy_lo = +0.0D+00
      xy_hi = +1.0D+00
      seed = 123456789

      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, x )
      call r8vec_uniform_ab ( n, xy_lo, xy_hi, seed, y )
      c = 1.0D+00

      call resid_spiral ( n, x, y, c, pr )

      write (  *, '(a)' ) ''
      write (  *, '(a)' ) '           Minimum       Maximum'
      write (  *, '(a)' ) ''
      write (  *, '(a,2x,g14.6,2x,g14.6)' ) '  Pr:  ', 
     &  r8vec_amin ( n, pr ), r8vec_amax ( n, pr )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 generates a field on a regular grid and plots it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 January 2015
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

      double precision c
      character * ( 255 ) header
      integer n
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
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) 
     &  '  Generate a spiral velocity field on a regular grid.'
      write ( *, '(a)' ) '  Store in GNUPLOT data and command files.'

      x_lo = 0.0D+00
      x_hi = 1.0D+00

      y_lo = 0.0D+00
      y_hi = 1.0D+00

      call grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

      n = x_num * y_num
      c = 1.0D+00

      call uv_spiral ( n, x, y, c, u, v )

      header = 'spiral'
      s = 0.05D+00
      call spiral_gnuplot ( header, n, x, y, u, v, s )

      return
      end

