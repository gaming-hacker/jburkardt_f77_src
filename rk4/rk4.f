      subroutine rk4 ( t0, u0, dt, f, u )

c*********************************************************************72
c
cc RK4 takes one Runge-Kutta step.
c
c  Discussion:
c
c    It is assumed that an initial value problem, of the form
c
c      du/dt = f ( t, u )
c      u(t0) = u0
c
c    is being solved.
c
c    If the user can supply current values of t, u, a stepsize dt, and a
c    function to evaluate the derivative, this function can compute the
c    fourth-order Runge Kutta estimate to the solution at time t+dt.
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
c    Input, double precision T0, the current time.
c
c    Input, double precision U0, the solution estimate at the current time.
c
c    Input, double precision DT, the time step.
c
c    Input, external F, a subroutine of the form 
c      subroutine f ( t, u, uprime ) 
c    which evaluates the derivative uprime given the time T and
c    solution vector U.
c
c    Output, double precision U, the fourth-order Runge-Kutta solution 
c    estimate at time T0+DT.
c
      implicit none

      double precision dt
      external f
      double precision f0
      double precision f1
      double precision f2
      double precision f3
      double precision t0
      double precision t1
      double precision t2
      double precision t3
      double precision u
      double precision u0
      double precision u1
      double precision u2
      double precision u3
c
c  Get four sample values of the derivative.
c
      call f ( t0, u0, f0 )

      t1 = t0 + dt / 2.0D+00
      u1 = u0 + dt * f0 / 2.0D+00
      call f ( t1, u1, f1 )

      t2 = t0 + dt / 2.0D+00
      u2 = u0 + dt * f1 / 2.0D+00
      call f ( t2, u2, f2 )

      t3 = t0 + dt
      u3 = u0 + dt * f2
      call f ( t3, u3, f3 )
c
c  Combine them to estimate the solution U at time T1.
c
      u = u0 + dt * ( f0 + 2.0D+00 * f1 + 2.0D+00 * f2 + f3 ) / 6.0D+00

      return
      end
      subroutine rk4vec ( t0, m, u0, dt, f, u )

c*********************************************************************72
c
cc RK4VEC takes one Runge-Kutta step for a vector system.
c
c  Discussion:
c
c    Thanks  to Dante Bolatti for correcting the final function call to:
c      call f ( t3, m, u3, f3 )
c    18 August 2016.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 August 2016
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T0, the current time.
c
c    Input, integer M, the spatial dimension.
c
c    Input, double precision U0(M), the solution estimate at the current time.
c
c    Input, double precision DT, the time step.
c
c    Input, external F, a subroutine of the form 
c      subroutine f ( t, m, u, uprime ) 
c    which evaluates the derivative UPRIME(1:M) given the time T and
c    solution vector U(1:M).
c
c    Output, double precision U(M), the fourth-order Runge-Kutta solution 
c    estimate at time T0+DT.
c
      implicit none

      integer m

      double precision dt
      external f
      double precision f0(m)
      double precision f1(m)
      double precision f2(m)
      double precision f3(m)
      integer i
      double precision t0
      double precision t1
      double precision t2
      double precision t3
      double precision u(m)
      double precision u0(m)
      double precision u1(m)
      double precision u2(m)
      double precision u3(m)
c
c  Get four sample values of the derivative.
c
      call f ( t0, m, u0, f0 )

      t1 = t0 + dt / 2.0D+00
      do i = 1, m
        u1(i) = u0(i) + dt * f0(i) / 2.0D+00
      end do
      call f ( t1, m, u1, f1 )

      t2 = t0 + dt / 2.0D+00
      do i = 1, m
        u2(i) = u0(i) + dt * f1(i) / 2.0D+00
      end do
      call f ( t2, m, u2, f2 )

      t3 = t0 + dt
      do i = 1, m
        u3(i) = u0(i) + dt * f2(i)
      end do
      call f ( t3, m, u3, f3 )
c
c  Combine them to estimate the solution U1 at time T1 = T0 + DT.
c
      do i = 1, m
        u(i) = u0(i) + ( dt / 6.0D+00 ) * 
     &    ( f0(i) + 2.0D+00 * f1(i) + 2.0D+00 * f2(i) + f3(i) )
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
