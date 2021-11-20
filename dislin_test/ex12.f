      program main

c*********************************************************************72
c
cc EX12 demonstrates a shaded contour plot.
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
      parameter ( n = 50 )

      integer i
      integer j
      double precision step
      double precision x
      double precision xray(n)
      double precision y
      double precision yray(n)
      double precision zlev(12)
      double precision zmat(n,n)

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX12:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Demonstrate the creation of '
      write ( *, '(a)' ) '  a shaded contour plot.'

      step = 1.6D+00 / dble ( n - 1 )

      do i = 1, n
        x = dble ( i - 1 ) * step
        xray(i) = x
        do j = 1, n
          y = dble ( j - 1 ) * step
          yray(j) = y
          zmat(i,j) = ( x**2 - 1.0D+00 )**2 + ( y**2 - 1.0D+00 )**2
        end do
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
      call setfil ( 'ex12.png' )
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

      call mixalf ( )
      call titlin ( 'Shaded Contour Plot', 1 )
      call titlin ( 'F(X,Y) = (X[2$ - 1)[2$ + (Y[2$ - 1)[2$', 3 )
      call name ( 'X-axis', 'X' )
      call name ( 'Y-axis', 'Y' )

      call shdmod ( 'POLY', 'CONTUR' )
      call axspos ( 450, 2670 )
      call graf ( 0.0D+00, 1.6D+00, 0.0D+00, 0.2D+00, 
     &  0.0D+00, 1.6D+00, 0.0D+00, 0.2D+00 )

      do i = 1, 12
        zlev(13-i) = 0.1D+00 + dble ( i - 1 ) * 0.1D+00
      end do

      call conshd ( xray, n, yray, n, zmat, zlev, 12 )

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
      write ( *, '(a)' ) 'EX12:'
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
