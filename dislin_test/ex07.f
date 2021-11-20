      program main

c*********************************************************************72
c
cc EX07 demonstrates 3D bar graphs and pie charts.
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

      character*80 cbuf
      integer ic1ray(5)
      integer ic2ray(5)
      double precision xray(5)
      double precision y1ray(5)
      double precision y2ray(5)

      save ic1ray
      save ic2ray
      save xray
      save y1ray
      save y2ray

      data xray / 2.0, 4.0, 6.0, 8.0, 10.0 /
      data y1ray / 0.0, 0.0, 0.0, 0.0, 0.0 /
      data y2ray / 3.2, 1.5, 2.0, 1.0, 3.0 /
      data ic1ray / 50, 150, 100, 200, 175 /
      data ic2ray / 50, 150, 100, 200, 175 /

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX07:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Demonstrate the creation of 3D bar '
      write ( *, '(a)' ) '  and pie charts.'
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
      call setfil ( 'ex07.png' )
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

      call titlin ( '3D Bar Graph / 3D Pie Chart', 2 )
      call htitle ( 40 )

      call shdpat ( 16 )
      call axslen ( 1500, 1000 )
      call axspos ( 300, 1400 )

      call barwth ( 0.5 )
      call bartyp ( '3DVERT' )
      call labels ( 'SECOND', 'BARS' )
      call labpos ( 'OUTSIDE', 'BARS' )
      call labclr ( 255, 'BARS' )

      call graf ( 0.0D+00, 12.0D+00, 0.0D+00, 2.0D+00, 
     &  0.0D+00, 5.0D+00, 0.0D+00, 1.0D+00 )

      call title ( )
      call color ( 'RED' )
      call bars ( xray, y1ray, y2ray, 5 )
      call endgrf ( )

      call shdpat ( 16 )
      call labels ( 'DATA', 'PIE' )
      call labclr ( 255, 'PIE' )
      call chnpie ( 'NONE' )
      call pieclr ( ic1ray, ic2ray, 5 )
      call pietyp ( '3D' )
      call axspos ( 300, 2700 )
      call piegrf ( cbuf, 0, y2ray, 5 )
c
c  Close DISLIN.
c     
      call disfin ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX07:'
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
