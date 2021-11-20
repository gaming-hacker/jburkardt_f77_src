      program main

c*********************************************************************72
c
cc MAIN is the main program for RK4_PRB.
c
c  Discussion:
c
c    RK4_PRB tests the RK4 library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RK4_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the RK4 library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RK4_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests RK4 for a scalar ODE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision dt
      parameter ( dt = 0.1D+00 )
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision t0
      double precision t1
      external test01_f
      double precision tmax
      parameter ( tmax = 12.0D+00 * pi )
      double precision u0
      double precision u1

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  RK4 solves a scalar ODE.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          T          U(T)'
      write ( *, '(a)' ) ' '

      t0 = 0.0D+00
      u0 = 0.5D+00

10    continue
c
c  Print (T0,U0).
c
        write ( *, '(2x,g14.6,2x,g14.6)' ) t0, u0
c
c  Stop if we've exceeded TMAX.
c
        if ( tmax .le. t0 ) then
          go to 20
        end if
c
c  Otherwise, advance to time T1, and have RK4 estimate 
c  the solution U1 there.
c
        t1 = t0 + dt
        call rk4 ( t0, u0, dt, test01_f, u1 )
c
c  Shift the data to prepare for another step.
c
        t0 = t1
        u0 = u1

      go to 10

20    continue

      return
      end
      subroutine test01_f ( t, u, uprime )

c*********************************************************************72
c
cc TEST01_F evaluates the right hand side of a particular ODE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    31 January 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the current time.
c
c    Input, double precision U, the current solution value.
c
c    Output, double precision UPRIME, the value of the derivative, dU/dT.
c
      implicit none

      double precision t
      double precision u
      double precision uprime
      
      uprime = u * cos ( t )
     
      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests the RK4 routine for a vector ODE.
c
c  Discussion:
c
c    RK4_PRB tests the RK4 library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Local, double precision DT, the time step.
c
c    Local, double precision T0, the time at which the solution is known.
c
c    Local, double precision TMAX, the maximum time at which a solution 
c    is desired.
c
c    Local, double precision U0, the estimated solution at time T0.
c
      implicit none

      integer n
      parameter ( n = 2 )

      double precision dt
      parameter ( dt = 0.1D+00 )
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision t0
      double precision t1
      external test02_f
      double precision tmax
      parameter ( tmax = 12.0D+00 * pi )
      double precision u0(n)
      double precision u1(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  RK4VEC takes a Runge Kutta step for a vector ODE.'
      write ( *, '(a)' ) ' '

      t0 = 0.0D+00
      u0(1) = 0.0D+00
      u0(2) = 1.0D+00

10    continue
c
c  Print (T0,U0).
c
        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) t0, u0(1), u0(2)
c
c  Stop if we've exceeded TMAX.
c
        if ( tmax .le. t0 ) then
          go to 20
        end if
c
c  Otherwise, advance to time T1, and have RK4 estimate 
c  the solution U1 there.
c
        t1 = t0 + dt
        call rk4vec ( t0, n, u0, dt, test02_f, u1 )
c
c  Shift the data to prepare for another step.
c
        t0 = t1
        do i = 1, n
          u0(i) = u1(i)
        end do

      go to 10

20    continue

      return
      end
      subroutine test02_f ( t, n, u, uprime )

c*********************************************************************72
c
cc TEST02_F evaluates the right hand side of a vector ODE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the current time.
c
c    Input, integer ( kind = 4 ) N, the dimension of the system.
c
c    Input, double precision U(N), the current solution value.
c
c    Output, double precision UPRIME(N), the value of the derivative, dU/dT.
c
      implicit none

      integer ( kind = 4 ) n

      double precision t
      double precision u(n)
      double precision uprime(n)
      
      uprime(1) = u(2)
      uprime(2) = - u(1)
     
      return
      end

