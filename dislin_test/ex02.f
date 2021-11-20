      program main

c*********************************************************************72
c
cc EX02 demonstrates the use of POLAR to plot (R,Theta) data.
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

      integer m
      integer n
      parameter ( m = 10 )
      parameter ( n = 300 )

      double precision a
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision step
      double precision x2(m)
      double precision xray(n)
      double precision y2(m)
      double precision yray(n)

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX02:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Demonstrate the use of POLAR to plot '
      write ( *, '(a)' ) '  (R,Theta) data.'

      step = 360.0D+00 / dble ( n - 1 )

      do i = 1, n
        a = dble ( i - 1 ) * step
        a = a * pi / 180.0D+00
        yray(i) = a
        xray(i) = sin ( 5.0D+00 * a )
      end do

      do i = 1, m
        x2(i) = dble ( i )
        y2(i) = dble ( i )
      end do
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
      call setfil ( 'ex02.png' )
c
c  Choose the page size and orientation.
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
c  Use the HARDWARE font.
c
      call hwfont ( )

      call titlin ( 'Polar Plots', 2 )
      call ticks ( 3, 'Y' )
      call axends ( 'NOENDS', 'X' )
      call labdig ( -1, 'Y' )
      call axslen ( 1000, 1000 )
      call axsorg ( 1050, 900 )

      call polar ( 1.0D+00, 0.0D+00, 0.2D+00, 0.0D+00, 30.0D+00 )
      call curve ( xray, yray, n )
      call htitle ( 50 )
      call title ( )
      call endgrf ( )

      call labdig ( -1, 'X' )
      call axsorg ( 1050, 2250 )
      call labtyp ( 'VERT', 'Y' )
      call barwth ( 5.0 )
      
      call polar ( 10.0D+00, 0.0D+00, 2.0D+00, 0.0D+00, 30.0D+00 )
      call barwth ( -5.0 )
      call polcrv ( 'FBARS' )
      call curve ( x2, y2, m )
c
c  Close DISLIN.
c
      call disfin ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX02:'
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

