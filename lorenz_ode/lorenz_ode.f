      program main

c*********************************************************************72
c
cc MAIN is the main program for LORENZ_ODE2.
c
c  Discussion:
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 October 2013
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

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 200000 )

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision dt
      integer i
      integer j
      external lorenz_rhs
      double precision t(0:n)
      double precision t_final
      double precision x(m,0:n)

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LORENZ_ODE'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Compute solutions of the Lorenz system.'
      write ( *, '(a)' ) '  Write data to a file for use by gnuplot.'
c
c  Data
c
      t_final = 40.0D+00
      dt = t_final / dble ( n )
c
c  Initial conditions.
c
      do j = 0, n
        t(j) = dble ( j ) * t_final / dble ( n )
      end do

      x(1,0) = 8.0D+00
      x(2,0) = 1.0D+00
      x(3,0) = 1.0D+00
c
c  Compute the approximate solution at equally spaced times.
c
      do j = 0, n - 1

        call rk4vec ( t(j), m, x(1:m,j), dt, lorenz_rhs, x(1:m,j+1) )

      end do
c
c  Create the data file.
c
      call get_unit ( data_unit )
      data_filename = 'lorenz_ode_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 0, n, 50
        write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    t(j), ( x(i,j), i = 1, 3 )
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) 
     &  '  Created data file "' // trim ( data_filename ) // '".'
c
c  Create the command file.
c
      call get_unit ( command_unit )
      command_filename = 'lorenz_ode_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "xyz_time.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- T --->"'
      write ( command_unit, '(a)' ) 
     &  'set ylabel "<--- X(T), Y(T), Z(T) --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "X(T), Y(T), Z(T) versus Time"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue",' // 
     &  ' "" using 1:3 lw 3 linecolor rgb "red",' // 
     &  ' "" using 1:4 lw 3 linecolor rgb "green"'
      write ( command_unit, '(a)' ) 
     &  'set output "xyz_3d.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X(T) --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y(T) --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z(T) --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "(X(T),Y(T),Z(T)) trajectory"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'splot "' // trim ( data_filename ) // 
     &  '" using 2:3:4 lw 1 linecolor rgb "blue"'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LORENZ_ODE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
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
      subroutine lorenz_rhs ( t, m, x, dxdt )

c*********************************************************************72
c
cc LORENZ_RHS evaluates the right hand side of the Lorenz ODE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 October 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the value of the independent variable.
c
c    Input, integer M, the spatial dimension.
c
c    Input, double precision X(M), the values of the dependent variables
c    at time T.
c
c    Output, double precision DXDT(M), the values of the derivatives
c    of the dependent variables at time T.
c
      implicit none

      integer m

      double precision beta
      parameter ( beta = 8.0D+00 / 3.0D+00 )
      double precision dxdt(m)
      double precision rho
      parameter ( rho = 28.0D+00 )
      double precision sigma
      parameter ( sigma = 10.0D+00 )
      double precision t
      double precision x(m)

      dxdt(1) = sigma * ( x(2) - x(1) )
      dxdt(2) = x(1) * ( rho - x(3) ) - x(2)
      dxdt(3) = x(1) * x(2) - beta * x(3)

      return
      end
      subroutine rk4vec ( t0, m, u0, dt, f, u )

c*********************************************************************72
c
cc RK4VEC takes one Runge-Kutta step for a vector system.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 October 2013
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
      call f ( t1, m, u1, f3 )
c
c  Combine them to estimate the solution at time T0 + DT.
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
