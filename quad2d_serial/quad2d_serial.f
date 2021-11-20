      program main

c*********************************************************************72
c
cc MAIN is the main program for QUAD2D_SERIAL.
c
      double precision a
      double precision b
      double precision error
      double precision exact
      external f
      double precision f
      integer i
      integer j
      integer n
      integer nx
      integer ny
      double precision pi
      double precision total
      double precision wtime
      double precision wtime1
      double precision wtime2
      double precision x
      double precision y

      a = 0.0
      b = 1.0
      nx = 32768
      ny = 32768
      n = nx * ny
      pi = 3.141592653589793D+00
      exact = pi * pi / 6.0D+00

      call timestamp ( )
      write ( *, * ) ' '
      write ( *, * ) 'QUAD2D_SERIAL:'
      write ( *, * ) '  FORTRAN77 version'
      write ( *, * ) 
     &  '  Estimate the integral of f(x,y) over [0,1]x[0,1].'
      write ( *, * ) '  f(x,y) = 1 / ( 1 - x * y ).'
      write ( *, * ) ' '
      write ( *, * ) '  A        = ', a
      write ( *, * ) '  B        = ', b
      write ( *, * ) '  NX       = ', nx
      write ( *, * ) '  NY       = ', ny
      write ( *, * ) '  N        = ', n
      write ( *, * ) '  Exact    = ', exact

      call cpu_time ( wtime1 )

      total = 0.0D+00
      do i = 1, nx
        x = ( ( 2 * nx - 2 * i + 1 ) * a + ( 2 * i - 1 ) * b ) 
     &    / ( 2 * nx )
        do j = 1, ny
          y = ( ( 2 * ny - 2 * j + 1 ) * a + ( 2 * j - 1 ) * b ) 
     &      / ( 2 * ny )
          total = total + f ( x, y )
        end do
      end do

      call cpu_time ( wtime2 )

      total = ( b - a ) * ( b - a ) * total / dble ( nx ) / dble ( ny )
      error = abs ( total - exact )
      wtime = wtime2 - wtime1
     
      write ( *, * ) ' '
      write ( *, * ) '  Estimate = ', total
      write ( *, * ) '  Error    = ', error
      write ( *, * ) '  Time     = ', wtime
c
c  Terminate.
c
      write ( *, * ) ' '
      write ( *, * ) 'QUAD2D_SERIAL:'
      write ( *, * ) '  Normal end of execution.'
      write ( *, * ) ' '
      call timestamp ( )

      stop
      end
      function f ( x, y )

c*********************************************************************72
c
cc F evaluates the function.
c
      double precision f
      double precision x
      double precision y

      f = 1.0D+00 / ( 1.0D+00 - x * y )

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
