      program main

c*********************************************************************72
c
cc FD1D_ADVECTION_LAX_WENDROFF: advection equation using Lax-Wendroff method.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nx
      parameter ( nx = 101 )

      double precision a
      double precision b
      double precision c
      double precision c1
      double precision c2
      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      double precision dt
      double precision dx
      integer i
      integer i4_wrap
      integer j
      integer jm1
      integer jp1
      integer nt
      integer nt_step
      integer plotstep
      double precision t
      double precision u(nx)
      double precision unew(nx)
      double precision x(nx)

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FD1D_ADVECTION_LAX_WENDROFF:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Solve the constant-velocity advection equation in 1D,'
      write ( *, '(a)' ) '    du/dt = - c du/dx'
      write ( *, '(a)' ) '  over the interval:'
      write ( *, '(a)' ) '    0.0 <= x <= 1.0'
      write ( *, '(a)' ) '  with periodic boundary conditions, and'
      write ( *, '(a)' ) '  with a given initial condition'
      write ( *, '(a)' ) 
     &  '    u(0,x) = (10x-4)^2 (6-10x)^2 for 0.4 <= x <= 0.6'
      write ( *, '(a)' ) '           = 0 elsewhere.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  We modify the FTCS approach using the Lax-Wendroff method:'

      dx = 1.0D+00 / dble ( nx - 1 )
      a = 0.0D+00
      b = 1.0D+00
      call r8vec_linspace ( nx, a, b, x )
      nt = 1000
      dt = 1.0D+00 / dble ( nt )
      c = 1.0D+00
      c1 = 0.5 *   c * dt / dx
      c2 = 0.5 * ( c * dt / dx ) ** 2

      call initial_condition ( nx, x, u )
c
c  Open data file, and write solutions as they are computed.
c
      call get_unit ( data_unit )
      data_filename = 'advection_data.txt'
      open ( unit = data_unit, file = data_filename )

      t = 0.0D+00

      write ( data_unit, '(f10.4,2x,f10.4,2x,f10.4)' ) x(1), t, u(1)
      do j = 1, nx
        write ( data_unit, '(f10.4,2x,f10.4,2x,f10.4)' ) x(j), t, u(j)
      end do
      write ( data_unit, '(a)' ) ''

      nt_step = 100

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of nodes NX = ', nx
      write ( *, '(a,i6)' ) '  Number of time steps NT = ', nt
      write ( *, '(a,g14.6)' ) '  Constant velocity C = ', c
      write ( *, '(a,g14.6,a,g14.6,a)' ) 
     &  '  CFL condition: dt (', dt, ') <= dx / c (', dx / c, ')'

      do i = 1, nt

        do j = 1, nx
          jm1 = i4_wrap ( j - 1, 1, nx )
          jp1 = i4_wrap ( j + 1, 1, nx )
          unew(j) = u(j) - c1 * ( u(jp1) - u(jm1) ) 
     &      + c2 * ( u(jp1) - 2.0D+00 * u(j) + u(jm1) )
        end do

        do j = 1, nx
          u(j) = unew(j)
        end do

        if ( i .eq. nt_step ) then
          t = dble ( i ) * dt
          do j = 1, nx
            write ( data_unit, '(f10.4,2x,f10.4,2x,f10.4)' ) 
     &        x(j), t, u(j)
          end do
          write ( data_unit, '(a)' ) ''
          nt_step = nt_step + 100
        end if

      end do
c
c  Close the data file once the computation is done.
c
      close ( unit = data_unit )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Plot data written to the file "' 
     &  // trim ( data_filename ) // '".'
c
c  Write gnuplot command file.
c
      call get_unit ( command_unit )
      command_filename = 'advection_commands.txt'
      open ( unit = command_unit, file = command_filename )

      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "advection_lax_wendroff.png"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Time--->"'
      write ( command_unit, '(a,i4,a)' ) 
     &  'splot "' // trim ( data_filename ) 
     &  // '" using 1:2:3 with lines'
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Gnuplot command data written to the file "' 
     &  // trim ( command_filename ) // '".'
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FD1D_ADVECTION_LAX_WENDROFF'
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
      function i4_modp ( i, j )

c*********************************************************************72
c
cc I4_MODP returns the nonnegative remainder of integer division.
c
c  Discussion:
c
c    If
c      NREM = I4_MODP ( I, J )
c      NMULT = ( I - NREM ) / J
c    then
c      I = J * NMULT + NREM
c    where NREM is always nonnegative.
c
c    The MOD function computes a result with the same sign as the
c    quantity being divided.  Thus, suppose you had an angle A,
c    and you wanted to ensure that it was between 0 and 360.
c    Then mod(A,360) would do, if A was positive, but if A
c    was negative, your result would be between -360 and 0.
c
c    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
c
c  Example:
c
c        I     J     MOD I4_MODP    Factorization
c
c      107    50       7       7    107 =  2 *  50 + 7
c      107   -50       7       7    107 = -2 * -50 + 7
c     -107    50      -7      43   -107 = -3 *  50 + 43
c     -107   -50      -7      43   -107 =  3 * -50 + 43
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the number to be divided.
c
c    Input, integer J, the number that divides I.
c
c    Output, integer I4_MODP, the nonnegative remainder when I is
c    divided by J.
c
      implicit none

      integer i
      integer i4_modp
      integer j
      integer value

      if ( j .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_MODP - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal divisor J = ', j
        stop
      end if

      value = mod ( i, j )

      if ( value .lt. 0 ) then
        value = value + abs ( j )
      end if

      i4_modp = value

      return
      end
      function i4_wrap ( ival, ilo, ihi )

c*********************************************************************72
c
cc I4_WRAP forces an I4 to lie between given limits by wrapping.
c
c  Example:
c
c    ILO = 4, IHI = 8
c
c    I  Value
c
c    -2     8
c    -1     4
c     0     5
c     1     6
c     2     7
c     3     8
c     4     4
c     5     5
c     6     6
c     7     7
c     8     8
c     9     4
c    10     5
c    11     6
c    12     7
c    13     8
c    14     4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer IVAL, an integer value.
c
c    Input, integer ILO, IHI, the desired bounds for the integer value.
c
c    Output, integer I4_WRAP, a "wrapped" version of IVAL.
c
      implicit none

      integer i4_modp
      integer i4_wrap
      integer ihi
      integer ilo
      integer ival
      integer jhi
      integer jlo
      integer value
      integer wide

      jlo = min ( ilo, ihi )
      jhi = max ( ilo, ihi )

      wide = jhi - jlo + 1

      if ( wide .eq. 1 ) then
        value = jlo
      else
        value = jlo + i4_modp ( ival - jlo, wide )
      end if

      i4_wrap = value

      return
      end
      subroutine initial_condition ( nx, x, u )

c*********************************************************************72
c
cc INITIAL_CONDITION sets the initial condition.
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
c  Parameters:
c
c    Input, integer NX, the number of nodes.
c
c    Input, double precision X(NX), the coordinates of the nodes.
c
c    Output, double precision U(NX), the value of the initial condition.
c
      implicit none

      integer nx

      integer i
      double precision u(nx)
      double precision x(nx)

      do i = 1, nx

        if  ( 0.4D+00 .le. x(i) .and. x(i) .le. 0.6D+00 ) then
          u(i) = ( 10.0D+00 * x(i) - 4.0D+00 ) ** 2 
     &         * ( 6.0D+00 - 10.0D+00 * x(i) ) ** 2
        else
          u(i) = 0.0D+00
        end if

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
