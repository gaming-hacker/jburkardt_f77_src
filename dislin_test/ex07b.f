      program main

c*********************************************************************72
c
cc EX07B demonstrates 3D bar graphs.
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

      integer n
      parameter ( n = 18 )

      character*80 cbuf
      integer i
      integer icray(n)
      double precision xray(n)
      double precision xwray(n)
      double precision yray(n)
      double precision ywray(n)
      double precision z1ray(n)
      double precision z2ray(n)

      save icray
      save xray
      save yray
      save z1ray
      save z2ray

      data icray / 30, 30, 30, 30, 30, 30, 100, 100, 100, 100,
     &  100, 100, 170, 170, 170, 170, 170, 170 /
      data xray / 1.0, 3.0, 8.0, 1.5, 9.0, 6.3, 5.8, 2.3, 8.1, 3.5,
     &  2.2, 8.7, 9.2, 4.8, 3.4, 6.9, 7.5, 3.8 /
      data yray / 5.0, 8.0, 3.5, 2.0, 7.0, 1.0, 4.3, 7.2, 6.0, 8.5,
     &  4.1, 5.0, 7.3, 2.8, 1.6, 8.9, 9.5, 3.2 /
      data z1ray / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     &  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /
      data z2ray / 4.0, 5.0, 3.0, 2.0, 3.5, 4.5, 2.0, 1.6, 3.8, 4.7,
     &  2.1, 3.5, 1.9, 4.2, 4.9, 2.8, 3.6, 4.3 / 

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX07B:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Demonstrate the creation of 3D bar graphs.'

      do i = 1, n
        xwray(i) = 0.5D+00
        ywray(i) = 0.5D+00
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
      call setfil ( 'ex07b.png' )
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
      call axspos ( 200, 2600 )
      call axslen ( 1800, 1800 ) 

      call name ( 'X-axis', 'X' )
      call name ( 'Y-axis', 'Y' )
      call name ( 'Z-axis', 'Z' )

      call titlin ( '3D Bars / BARS3D', 3 )

      call labl3d ( 'HORI' )

      call graf3d ( 0.0D+00, 10.0D+00, 0.0D+00, 2.0D+00, 
     &  0.0D+00, 10.0D+00, 0.0D+00, 2.0D+00, 
     &  0.0D+00, 5.0D+00, 0.0D+00, 1.0D+00 )

      call grid3d ( 1, 1, 'BOTTOM' )

      call bars3d ( xray, yray, z1ray, z2ray, xwray, ywray, icray, n ) 

      call legini ( cbuf, 3, 20 )
      call legtit ( ' ' )
      call legpos ( 1300, 1100 )
      call leglin ( cbuf, 'First', 1 )
      call leglin ( cbuf, 'Second', 2 )
      call leglin ( cbuf, 'Third', 3 )
      call legend ( cbuf, 3 )

      call height ( 50 )
      call title ( )
c
c  Close DISLIN.
c
      call disfin ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX07B:'
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
