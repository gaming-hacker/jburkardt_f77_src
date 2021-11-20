      program main

c*********************************************************************72
c
cc EX10 demonstrates a 3D surface plot.
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

      character*60 ctit1
      character*60 ctit2
      external zfun

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX10:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Demonstrate the creation of '
      write ( *, '(a)' ) '  a surface plot.'

      ctit1 = 'Surface Plot (SURFUN)'
      ctit2 = 'F(X,Y) = 2*SIN(X)*SIN(Y)' 
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
      call setfil ( 'ex10.png' )
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

      call axspos ( 200, 2600 )
      call axslen ( 1800, 1800 )

      call name ( 'X-axis', 'X' )
      call name ( 'Y-axis', 'Y' )
      call name ( 'Z-axis', 'Z' )

      call titlin ( ctit1, 2 )
      call titlin ( ctit2, 4 )

      call view3d ( -5.0D+00, -5.0D+00, 4.0D+00, 'ABS' )

      call graf3d ( 0.0D+00, 360.0D+00, 0.0D+00, 90.0D+00, 
     &  0.0D+00, 360.0D+00, 0.0D+00, 90.0D+00, 
     &  -3.0D+00, 3.0D+00, -3.0D+00, 1.0D+00 )

      call height ( 50 )
      call title ( )

      call surfun ( zfun, 1, 10.0D+00, 1, 10.0D+00 )
c
c  Close DISLIN.
c
      call disfin ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX10:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      function zfun ( x, y )

c*********************************************************************72
c
cc ZFUN evaluates the function Z(X,Y).
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
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, Y, the evaluation point.
c
c    Output, double precision ZFUN, the value of Z(X,Y).
c
      double precision fpi
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x
      double precision y
      double precision zfun

      fpi = pi / 180.0D+00
      zfun = 2.0D+00 * sin ( x * fpi ) * sin ( y * fpi )

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
