      program main

c*****************************************************************************80
c
cc LIFE_GRID uses DISLIN to draw a grid for the game of Life.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 February 2014
c
c  Author:
c
c    This FORTRAN77 version by John Burkardt
c
c  Reference:
c
c    Helmut Michels,
c    The Data Plotting Software DISLIN - version 10.4,
c    Shaker Media GmbH, January 2010,
c    ISBN13: 978-3-86858-517-9.
c
      implicit none

      character * ( 60 ) ctit
      integer i
      integer j
      integer nr
      integer nx
      integer ny
      integer pat
      double precision r
      double precision x
      double precision xvec(2)
      double precision y
      double precision yvec(2)

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LIFE_GRID:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Use DISLIN to plot a grid for Life.'
c
c  Specify the format of the output file.
c
      call metafl ( 'png' )
c
c  Indicate that new data overwrites old data.
c
      call filmod ( 'delete' )
c
c  Specify the name of the output graphics file.
c
      call setfil ( 'life_grid.png' )
c
c  Choose the page size and orientation.
c  'USA' is 2160 plot units wide and 2790 plot units high.
c  'P' requests PROFILE rather than LANDSCAPE orientation.
c
      call setpag ( 'usap' )
c
c  For PNG output, reverse the default black background to white.
c
      call scrmod ( 'reverse' )
c
c  Open DISLIN.
c
      call disini ( )
c
c  Plot a border around the page.
c
      call pagera ( )
c
c  Use the COMPLEX font.
c
      call complx ( )
c
c  Use a color table, which is required if we want to do color graphics.
c  For this color table, in particular,
c  1 = black,
c  2 = red,
c  3 = green,
c  4 = blue.
c
      call setvlt ( 'small' )
c
c  Define the X and Y sizes of the axis system in plot units.
c
      call axslen ( 1000, 1000 )
c
c  Specify how the lower X, left Y, upper X and right Y axes are labeled.
c
      call setgrf ( 'line', 'line', 'line', 'line' )
c
c  Set the axis origin 500 plot units to the right, and 1500 plot units DOWN.
c
      call axspos ( 500, 1500 )
c
c  Relate the physical coordinates to the axes.
c
      call graf ( 0.0D+00, 100.0D+00, 0.0D+00, 0.5D+00, 
     &  0.0D+00, 100.0D+00, 0.0D+00, 0.5D+00 )
c
c  Draw 21 horizontal lines.
c
      do j = 0, 100, 5
        y = dble ( j )
        xvec(1) = 0.0D+00
        xvec(2) = 100.0D+00
        yvec(1) = y
        yvec(2) = y
        call curve ( xvec, yvec, 2 )
      end do
c
c  Draw 21 vertical lines.
c
      do i = 0, 100, 5
        x = dble ( i )
        xvec(1) = x
        xvec(2) = x
        yvec(1) = 0.0D+00
        yvec(2) = 100.0D+00
        call curve ( xvec, yvec, 2 )
      end do
c
c  Select the shading pattern.
c
      pat = 16
      call shdpat ( pat )
c
c  Select color 3 (green) from the color table.
c
      call setclr ( 3 )
c
c  Draw one circle near the origin.
c
      x = 2.5D+00
      y = 2.5D+00
      r = 2.0D+00
      call rlcirc ( x, y, r )
c
c  Select color 2 (red).
c
      call setclr ( 2 )
c
c  Draw a glider.
c
      x = 7.5D+00
      y = 37.5D+00
      r = 2.0D+00
      call rlcirc ( x, y, r )

      x = 12.5D+00
      y = 37.5D+00
      r = 2.0D+00
      call rlcirc ( x, y, r )

      x = 12.5D+00
      y = 47.5D+00
      r = 2.0D+00
      call rlcirc ( x, y, r )

      x = 17.5D+00
      y = 37.5D+00
      r = 2.0D+00
      call rlcirc ( x, y, r )

      x = 17.5D+00
      y = 42.5D+00
      r = 2.0D+00
      call rlcirc ( x, y, r )
c
c  Select color 4 (blue)
c
      call setclr ( 4 )
c
c  Select open shading pattern.
c
      pat = 0
      call shdpat ( pat )
c
c  Draw three open circles.
c
      x = 62.5D+00
      y = 62.5D+00
      r = 2.0D+00
      call rlcirc ( x, y, r )

      x = 67.5D+00
      y = 57.5D+00
      r = 2.0D+00
      call rlcirc ( x, y, r )

      x = 72.5D+00
      y = 52.5D+00
      r = 2.0D+00
      call rlcirc ( x, y, r )
c
c  Select character height in plot units.
c
      call height ( 50 )
c
c  Select color 1 (black) from the color table.
c
      call setclr ( 1 )
c
c  Define axis system titles.
c
      ctit = 'Grid for Game of Life'
      call titlin ( ctit, 1 )
c
c  Draw the title.
c
      call title ( )
c
c  End this plot.
c
      call endgrf ( )
c
c  Close DISLIN.
c
      call disfin ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LIFE_GRID:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
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
