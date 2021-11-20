      program main

c*********************************************************************72
c
cc EX04 demonstrates various interpolation methods for data.
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

      character*8 cpol(6)
      character*60 ctit
      integer i
      integer nx
      integer nxposn
      integer ny
      integer nya
      integer nyposn
      double precision x(16)
      double precision y(16)

      save cpol
      save nya
      save x
      save y

      data x / 0.0, 1.0, 3.0, 4.5, 6.0, 8.0, 9.0, 11.0, 12.0, 12.5,
     &  13.0, 15.0, 16.0, 17.0, 19.0, 20.0 /

      data y / 2.0, 4.0, 4.5, 3.0, 1.0, 7.0, 2.0, 3.0, 5.0, 2.0,
     &  2.5, 2.0, 4.0, 6.0, 5.5, 4.0 /

      data cpol / 'SPLINE', 'STEM', 'BARS', 'STAIRS', 'STEP', 'LINEAR' /

      data nya / 2700 /

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX04:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Demonstrate the various interpolation'
      write ( *, '(a)' ) '  methods available for (X,Y) data.'

      ctit = 'Interpolation Methods'
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
      call setfil ( 'ex04.png' )
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
c  Use the COMPLEX font.
c
      call complx ( )
      call incmrk ( 1 )
      call hsymbl ( 25 )
      call titlin ( ctit, 1 )
      call axslen ( 1500, 350 )
      call setgrf ( 'LINE', 'LINE', 'LINE', 'LINE' )

      do i = 1, 6

        call axspos ( 350, nya-(i-1)*350 )
        call polcrv ( cpol(i) )
        call marker ( 0 )

        call graf ( 0.0D+00, 20.0D+00, 0.0D+00, 5.0D+00, 
     &    0.0D+00, 10.0D+00, 0.0D+00, 5.0D+00 )

        nx = nxposn ( 1.0D+00 )
        ny = nyposn ( 8.0D+00 )
        call messag ( cpol(i), nx, ny )
        call curve ( x, y, 16 )

        if ( i .eq. 6 ) then
          call height ( 50 )
          call title ( )
        end if

        call endgrf ( )

      end do
c
c  Close DISLIN.
c
      call disfin ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX04:'
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
