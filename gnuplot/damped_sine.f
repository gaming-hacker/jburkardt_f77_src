      program main

c*********************************************************************72
c
cc DAMPED_SINE evaluates and plots the damped sine correlation function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2013
c
c  Author:
c
c    John Burkardt
c
      integer n
      parameter ( n = 101 )

      double precision c(n)
      double precision rho(n)
      double precision rho0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DAMPED_SINE'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Demonstrating how a correlation function can be'
      write ( *, '(a)' ) '  evaluated and plotted using GNUPLOT.'
c
c  Damped_sine
c
      rho0 = 1.0D+00
      call r8vec_linspace ( n, -12.0D+00, 12.0D+00, rho )
      call correlation_damped_sine ( n, rho, rho0, c )
      call correlation_plot ( n, rho, c, 'damped_sine', 
     &  'Damped sine correlation' )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DAMPED_SINE'
      write ( *, '(a)' ) '  Normal end of execution.'

      stop
      end
      subroutine correlation_damped_sine ( n, rho, rho0, c )

c*********************************************************************72
c
cc CORRELATION_DAMPED_SINE evaluates the damped sine correlation function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Petter Abrahamsen,
c    A Review of Gaussian Random Fields and Correlation Functions,
c    Norwegian Computing Center, 1997.
c
c  Parameters:
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision RHO(N), the arguments.
c
c    Input, double precision RHO0, the correlation length.
c
c    Output, double precision C(N), the correlations.
c
      implicit none

      integer n

      double precision c(n)
      integer i
      double precision rho(n)
      double precision rho0
      double precision rhohat

      do i = 1, n

        if ( rho(i) .eq. 0.0D+00 ) then
          c(i) = 1.0D+00
        else
          rhohat = abs ( rho(i) ) / rho0
          c(i) = sin ( rhohat ) / rhohat
        end if

      end do

      return
      end
      subroutine correlation_plot ( n, rho, c, header, title )

c*********************************************************************72
c
cc CORRELATION_PLOT makes a plot of a correlation function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision RHO(N), the arguments.
c
c    Input, double precision C(N), the correlations.
c
c    Input, character * ( * ) HEADER, an identifier for the files.
c
c    Input, character * ( * ) TITLE, a title for the plot.
c
      implicit none

      integer n

      double precision c(n)
      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      character * ( * ) header
      integer i
      double precision rho(n)
      double precision rho0
      character * ( * ) title

      call get_unit ( data_unit )
      data_filename = trim ( header ) // '_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, n
        write ( data_unit, '(2x,g14.6,2x,g14.6)' ) rho(i), c(i)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) '  Created data file "' 
     &  // trim ( data_filename ) // '".'

      call get_unit ( command_unit )
      command_filename = trim ( header ) // '_commands.txt'
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
     &  'set output "' // trim ( header ) // '.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Distance Rho"'
      write ( command_unit, '(a)' ) 'set ylabel "Correlation C(Rho)"'
      write ( command_unit, '(a)' ) 
     &  'set title "' // trim ( title ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue"'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

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
      subroutine r8vec_linspace ( n, a_first, a_last, a )

c*********************************************************************72
c
cc R8VEC_LINSPACE creates a vector of linearly spaced values.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
c
c    In other words, the interval is divided into N-1 even subintervals,
c    and the endpoints of intervals are used as the points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A_FIRST, A_LAST, the first and last entries.
c
c    Output, double precision A(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_first
      double precision a_last
      integer i

      if ( n .eq. 1 ) then

        a(1) = ( a_first + a_last ) / 2.0D+00

      else

        do i = 1, n
          a(i) = ( dble ( n - i     ) * a_first 
     &           + dble (     i - 1 ) * a_last )
     &           / dble ( n     - 1 )
        end do

      end if

      return
      end
