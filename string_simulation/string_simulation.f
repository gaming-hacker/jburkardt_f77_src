      program main

c*********************************************************************72
c
cc MAIN is the main program for STRING_SIMULATION.
c
c  Discussion:
c
c    This program solves the 1D wave equation of the form:
c
c      Utt = c^2 Uxx
c
c    over the spatial interval (X1,X2) and time interval (T1,T2),
c    with initial conditions:
c
c      U(T1,X)  = U_T1(X),
c      Ut(T1,X) = UT_T1(X),
c
c    and boundary conditions of Dirichlet type:
c
c      U(T,X1) = U_X1(T),
c      U(T,X2) = U_X2(T).
c
c    The value C represents the propagation speed of waves.
c
c    The program uses the finite difference method, and marches
c    forward in time, solving for all the values of U at the next
c    time step by using the values known at the previous two time steps.
c
c    Central differences may be used to approximate both the time
c    and space derivatives in the original differential equation.
c
c    Thus, assuming we have available the approximated values of U
c    at the current and previous times, we may write a discretized
c    version of the wave equation as follows:
c
c      Uxx(T,X) = ( U(T,   X+dX) - 2 U(T,X) + U(T,   X-dX) ) / dX^2
c      Utt(T,X) = ( U(T+dt,X   ) - 2 U(T,X) + U(T-dt,X   ) ) / dT^2
c
c    If we multiply the first term by C^2 and solve for the single
c    unknown value U(T+dt,X), we have:
c
c      U(T+dT,X) =        (     C^2 * dT^2 / dX^2 ) * U(T,   X+dX)
c                  +  2 * ( 1 - C^2 * dT^2 / dX^2 ) * U(T,   X   )
c                  +      (     C^2 * dT^2 / dX^2 ) * U(T,   X-dX)
c                  -                                  U(T-dT,X   )
c
c    (Equation to advance from time T to time T+dT, except for FIRST step)
c
c    However, on the very first step, we only have the values of U
c    for the initial time, but not for the previous time step.
c    In that case, we use the initial condition information for dUdT
c    which can be approximated by a central difference that involves
c    U(T+dT,X) and U(T-dT,X):
c
c      dU/dT(T,X) = ( U(T+dT,X) - U(T-dT,X) ) / ( 2 * dT )
c
c    and so we can estimate U(T-dT,X) as
c
c      U(T-dT,X) = U(T+dT,X) - 2 * dT * dU/dT(T,X)
c
c    If we replace the "missing" value of U(T-dT,X) by the known values
c    on the right hand side, we now have U(T+dT,X) on both sides of the
c    equation, so we have to rearrange to get the formula we use
c    for just the first time step:
c
c      U(T+dT,X) =   1/2 * (     C^2 * dT^2 / dX^2 ) * U(T,   X+dX)
c                  +       ( 1 - C^2 * dT^2 / dX^2 ) * U(T,   X   )
c                  + 1/2 * (     C^2 * dT^2 / dX^2 ) * U(T,   X-dX)
c                  +  dT *                         dU/dT(T,   X   )
c
c    (Equation to advance from time T to time T+dT for FIRST step.)
c
c    It should be clear now that the quantity ALPHA = C * DT / DX will affect
c    the stability of the calculation.  If it is greater than 1, then
c    the middle coefficient 1-C^2 DT^2 / DX^2 is negative, and the
c    sum of the magnitudes of the three coefficients becomes unbounded.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 December 2012
c
c  Author:
c
c    John Burkardt
c
c  Local Parameters:
c
c    Local, double precision ALPHA, the CFL stability parameter.
c
c    Local, double precision C, the wave speed.
c
c    Local, double precision DT, the time step.
c
c    Local, double precision DX, the spatial step.
c
c    Local, integer M, the number of time steps.
c
c    Local, integer N, the number of spatial intervals.
c
c    Local, double precision T1, T2, the initial and final times.
c
c    Local, double precision U(M+1,N+1), the computed solution.
c
c    Local, double precision X1, X2, the left and right spatial endpoints.
c
      implicit none

      integer m
      parameter ( m = 30 )
      integer n
      parameter ( n = 40 )

      double precision alpha
      double precision c
      integer command_unit
      integer data_unit
      double precision dt
      double precision dx
      double precision f
      double precision g
      integer i
      integer j
      double precision k
      double precision t
      double precision t1
      double precision t2
      double precision u(0:m,0:n)
      double precision x
      double precision x1
      double precision x2

      c = 0.25D+00
      t1 = 0.0D+00
      t2 = 3.0D+00
      x1 = 0.0D+00
      x2 = 1.0D+00

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'STRING_SIMULATION:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Simulate the behavior of a vibrating string.'

      dx = ( x2 - x1 ) / real ( n, kind = 8 )
      dt = ( t2 - t1 ) / real ( m, kind = 8 )
      alpha = ( c * dt / dx ) ** 2
      write ( *, '(a,g14.6)' ) '  ALPHA = ( C * dT / dX )^2 = ', alpha
c
c  Warn the user if ALPHA will cause an unstable computation.
c
      if ( 1.0D+00 .lt. alpha ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '  Warning!'
        write ( *, '(a)' ) '  ALPHA is greater than 1.'
        write ( *, '(a)' ) '  The computation is unstable.'
      end if
c
c  Time step 0: 
c  Use the initial condition for U.
c
      u(0,0) = 0.0D+00
      do j = 1, n - 1
        x = dble ( j ) * dx
        u(0,j) = f ( x )
      end do
      u(0,n) = 0.0D+00
c
c  Time step 1:
c  Use the initial condition for dUdT.
c
      u(1,0) = 0.0D+00
      do j = 1, n - 1
        x = dble ( j ) * dx
        u(1,j) = 
     &      ( alpha / 2.0D+00 ) * u(0,j-1) 
     &    + ( 1.0D+00 - alpha ) * u(0,j)   
     &    + ( alpha / 2.0D+00 ) * u(0,j+1) 
     &    + dt * g ( x )
      end do
      u(1,n) = 0.0D+00
c
c  Time steps 2 through M:
c
      do i = 2, m
        u(i,0) = 0.0D+00
        do j = 1, n - 1
          u(i,j) = 
     &                              alpha   * u(i-1,j-1) 
     &      + 2.0D+00 * ( 1.0D+00 - alpha ) * u(i-1,j)   
     &      +                       alpha   * u(i-1,j+1) 
     &      -                                 u(i-2,j)
        end do
        u(i,n) = 0.0D+00
      end do
c
c  Write data file.
c
      call get_unit ( data_unit )
      open ( unit = data_unit, file = 'string_data.txt', status = 'replace' )

      do i = 0, m
        t = dble ( i ) * dt
        do j = 0, n
          x = dble ( j ) * dx
          write ( data_unit, '(f10.4,2x,f10.4,2x,f10.4))' ) x, t, u(i,j)
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Plot data written to the file "string_data.txt".'
c
c  Write gnuplot command file.
c
      call get_unit ( command_unit )
      open ( unit = command_unit, file = 'string_commands.txt' )

      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "string.png"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Time--->"'
      write ( command_unit, '(a,i4,a)' ) 
     &  'splot "string_data.txt" using 1:2:3 with lines'
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) 
     &  '  Gnuplot command_unit written to the file "string_commands.txt".'
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'STRING_SIMULATION:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      function f ( x )

c*********************************************************************72
c
cc F supplies the initial condition.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 December 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the location.
c
c    Output, double precision F, the value of the solution at time 0 and location X.
c
      implicit none

      double precision f
      double precision x

      if ( 0.25D+00 <= x .and. x <= 0.50D+00 ) then
        f = ( x - 0.25D+00 ) * ( 0.50D+00 - x )
      else
        f = 0.0D+00
      end if

      return
      end
      function g ( x )

c*********************************************************************72
c
cc G supplies the initial derivative.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 December 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the location.
c
c    Output, double precision G, the value of the time derivative of the solution 
c    at time 0 and location X.
c
      implicit none

      double precision g
      double precision x

      g = 0.0D+00

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
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
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
