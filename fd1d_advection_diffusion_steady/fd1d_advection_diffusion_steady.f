      program main

c*********************************************************************72
c
cc FD1D_ADVECTION_DIFFUSION_STEADY solves steady advection diffusion equation.
c
c  Discussion:
c
c    The steady advection diffusion equation has the form:
c
c      v ux - k * uxx = 0
c
c    where V (the advection velocity) and K (the diffusivity) are positive 
c    constants, posed in the region
c
c      a = 0 < x < 1 = b
c
c    with boundary conditions
c
c      u(0) = 0, u(1) = 1.
c
c    The discrete solution is unreliable when dx > 2 * k / v / ( b - a ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nx
      parameter ( nx = 101 )

      double precision a
      double precision a3(nx,3)
      double precision b
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision dx
      double precision f(nx)
      integer i
      integer j
      double precision k
      double precision r
      double precision u(nx)
      double precision v
      double precision w(nx)
      double precision x(nx)

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FD1D_ADVECTION_DIFFUSION_STEADY:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Solve the 1D steady advection diffusion equation:,'
      write ( *, '(a)' ) '    v du/dx - k d2u/dx2 = 0'
      write ( *, '(a)' ) 
     &  '  with constant, positive velocity V and diffusivity K'
      write ( *, '(a)' ) '  over the interval:'
      write ( *, '(a)' ) '    0.0 <= x <= 1.0'
      write ( *, '(a)' ) '  with boundary conditions:'
      write ( *, '(a)' ) '    u(0) = 0, u(1) = 1.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Use finite differences'
      write ( *, '(a)' ) '   d u/dx  = (u(t,x+dx)-u(t,x-dx))/2/dx'
      write ( *, '(a)' ) '   d2u/dx2 = (u(x+dx)-2u(x)+u(x-dx))/dx^2'
c
c  Physical constants.
c
      v = 1.0D+00
      k = 0.05D+00
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Diffusivity K = ', k
      write ( *, '(a,g14.6)' ) '  Velocity V    = ', v
c
c  Spatial discretization.
c
      a = 0.0D+00
      b = 1.0D+00
      dx = ( b - a ) / dble ( nx - 1 )
      call r8vec_linspace ( nx, a, b, x )

      write ( *, '(a,i4)' ) '  Number of nodes NX = ', nx
      write ( *, '(a,g14.6)' ) '  DX = ', dx
      write ( *, '(a,g14.6)' ) '  Maximum safe DX is ', 
     &  2.0D+00 * k / v / ( b - a )
c
c  Set up the tridiagonal linear system corresponding to the boundary 
c  conditions and advection-diffusion equation.
c
      do j = 1, 3
        do i = 1, nx
          a3(i,j) = 0.0D+00
        end do
      end do

      do i = 1, nx
        f(i) = 0.0D+00
      end do

      a3(1,2) = 1.0D+00
      f(1) = 0.0D+00

      do i = 2, nx - 1
        a3(i,1) = - v / dx / 2.0D+00 -           k / dx / dx
        a3(i,2) =                    + 2.0D+00 * k / dx / dx
        a3(i,3) = + v / dx / 2.0D+00 -           k / dx / dx
        f(i) = 0.0D+00
      end do

      a3(nx,2) = 1.0D+00
      f(nx) = 1.0D+00

      call trisolve ( nx, a3, f, u )
c
c  The exact solution to the differential equation is known.
c
      r = v * ( b - a ) / k

      do i = 1, nx
        w(i) = ( 1.0D+00 - exp ( r * x(i) ) ) / ( 1.0D+00 - exp ( r ) )
      end do
c
c  Write data file.
c
      call get_unit ( data_unit )
      data_filename = 'fd1d_advection_diffusion_steady_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 1, nx
        write ( data_unit, '(f10.4,2x,f10.4,2x,f10.4)' ) 
     &    x(j), u(j), w(j)
      end do
      close ( unit = data_unit )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Gnuplot data written to file "' // 
     &  trim ( data_filename ) // '".'
c
c  Write command file.
c
      call get_unit ( command_unit )
      command_filename = 'fd1d_advection_diffusion_steady_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "fd1d_advection_diffusion_steady.png"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---U(X)--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Exact: green line, Approx: red dots"'
      write ( command_unit, '(a,i4,a)' ) 'plot "' // 
     &  trim ( data_filename ) 
     &  // '" using 1:2 with points pt 7 ps 2, \'
      write ( command_unit, '(a)' ) '"" using 1:3 with lines lw 3'
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Gnuplot commands written to "' 
     &  // trim ( command_filename ) // '".'
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FD1D_ADVECTION_DIFFUSION_STEADY'
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
      subroutine r8vec_linspace ( n, a, b, x )

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
c    Input, double precision A, B, the first and last entries.
c
c    Output, double precision X(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision x(n)

      if ( n .eq. 1 ) then

        x(1) = ( a + b ) / 2.0D+00

      else

        do i = 1, n
          x(i) = ( dble ( n - i     ) * a
     &           + dble (     i - 1 ) * b )
     &           / dble ( n     - 1 )
        end do

      end if

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
      subroutine trisolve ( n, a, b, x )

c*********************************************************************72
c
cc TRISOLVE factors and solves a tridiagonal system.
c
c  Discussion:
c
c    The three nonzero diagonals of the N by N matrix are stored as 3
c    columns of an N by 3 matrix.
c
c  Example:
c
c    Here is how a tridiagonal matrix of order 5 would be stored:
c
c       *  A11 A12
c      A21 A22 A23
c      A32 A33 A34
c      A43 A44 A45
c      A54 A55  *
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    02 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the linear system.
c
c    Input/output, double precision A(N,3).
c    On input, the tridiagonal matrix.
c    On output, the data in these vectors has been overwritten
c    by factorization information.
c
c    Input, double precision B(N), the right hand side of the linear system.
c
c    Output, double precision X(N), the solution of the linear system.
c
      implicit none

      integer n

      double precision a(n,3)
      double precision b(n)
      integer i
      double precision x(n)
      double precision xmult
c
c  The diagonal entries can't be zero.
c
      do i = 1, n
        if ( a(i,2) == 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'TRISOLVE - Fatal error!'
          write ( *, '(a,i8,a)' ) '  A(', i, ',2) = 0.'
          stop 1
        end if
      end do

      do i = 1, n
        x(i) = b(i)
      end do

      do i = 2, n
        xmult = a(i,1) / a(i-1,2)
        a(i,2) = a(i,2) - xmult * a(i-1,3)
        x(i)   = x(i)   - xmult * x(i-1)
      end do

      x(n) = x(n) / a(n,2)
      do i = n-1, 1, -1
        x(i) = ( x(i) - a(i,3) * x(i+1) ) / a(i,2)
      end do

      return
      end
