      program main

c*********************************************************************72
c
cc MAIN is the main program for SPRING_ODE2.
c
c  Discussion:
c
c    This is a revision of the SPRING_ODE code.
c
c    In this revision of the program, we want to use vectors (C arrays) to 
c    store the data, and we want to write the data out to a file in a form 
c    that Gnuplot (or other plotting programs) can use.
c
c    Hooke's law for a spring observes that the restoring force is
c    proportional to the displacement: F = - k x
c
c    Newton's law relates the force to acceleration: F = m a
c
c    Putting these together, we have
c
c      m * d^2 x/dt^2 = - k * x
c
c    We can add a damping force with coefficient c:
c
c      m * d^2 x/dt^2 = - k * x - c * dx/dt
c
c    If we write this as a pair of first order equations for (x,v), we have
c
c          dx/dt = v
c      m * dv/dt = - k * x - c * v
c
c    and now we can approximate these values for small time steps.
c
c    Note that the plotting assumes that the value of X will always be
c    between -1 and +1.  If the initial condition uses V = 0, and X starts
c    between -1 and +1, then this will be OK.
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
c    None
c
      implicit none

      integer n
      parameter ( n = 100 )

      double precision c
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision dt
      integer i
      double precision k
      double precision m
      double precision t(0:n)
      double precision t_final
      double precision v(0:n)
      double precision x(0:n)

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPRING_ODE2'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Approximate the solution of a spring equation.'
      write ( *, '(a)' ) '  Write data to a file for use by gnuplot.'
c
c  Data
c
      m = 1.0D+00
      k = 1.0D+00
      c = 0.3D+00
      t_final = 20.0D+00
      dt = t_final / dble ( n )
c
c  Initial conditions.
c
      t(0) = 0.0D+00
      x(0) = 1.0D+00
      v(0) = 0.0D+00
c
c  Compute the approximate solution at equally spaced times.
c
      do i = 1, n

        t(i) = dble ( i ) * t_final / dble ( n )
        x(i) = x(i-1) + dt * v(i-1)
        v(i) = v(i-1) + ( dt / m ) * ( - k * x(i-1) - c * v(i-1) )

      end do
c
c  Write the plot data file.
c
      call get_unit ( data_unit )
      data_filename = 'spring_ode2_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 0, n
        write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    t(i), x(i), v(i)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) '  Created data file "' 
     &  // trim ( data_filename ) // '".'
c
c  Write the plot command file.
c
      call get_unit ( command_unit )
      command_filename = 'spring_ode2_commands.txt'
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
     &  'set output "xv_time.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- T --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- X(T), V(T) --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Position and Velocity versus Time"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue",' //
     &  '"" using 1:3 lw 3 linecolor rgb "red"'

      write ( command_unit, '(a)' ) 
     &  'set output "xv_phase.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X(T) --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- V(T) --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Position versus Velocity"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 2:3 lw 3 linecolor rgb "green"'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPRING_ODE2:'
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
     &  '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) 
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
