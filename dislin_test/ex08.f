      program main

c*********************************************************************72
c
cc EX08 demonstrates various shading patterns.
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

      character*2 cstr
      character*60 ctit
      integer i
      integer iclr
      integer ii
      integer ix(4)
      integer ixp(4)
      integer iy(4)
      integer iyp(4)
      integer j
      integer k
      integer nl
      integer nlmess
      integer nx
      integer nx0
      integer ny
      integer ny0

      save ix
      save iy

      data ix / 0, 300, 300, 0 /
      data iy / 0, 0, 400, 400 /

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX08:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Demonstrate the use of shading patterns.'

      ctit = 'Shading Patterns (AREAF)'
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
      call setfil ( 'ex08.png' )
c
c  Choose the page size and orientation.
c
      call setpag ( 'usal' )
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
      call setvlt ( 'SMALL' )

      call height ( 50 )
      nl = nlmess ( ctit )
      nx = ( 2970 - nl ) / 2
      call messag ( ctit, nx, 200 )

      nx0 = 335
      ny0 = 350

      do i = 1, 3

        ny = ny0 + ( i - 1 ) * 600

        do j = 1, 6

          iclr = ( i - 1 ) * 6 + j - 1
          iclr = mod ( iclr, 15 )

          if ( iclr .eq. 0 ) then
            iclr = 15
          end if

          call setclr ( iclr )

          nx = nx0 + ( j - 1 ) * 400
          ii = ( i - 1 ) * 6 + j - 1
          call shdpat ( ii )
          write ( cstr, '(i2)' ) ii

          do k = 1, 4
            ixp(k) = ix(k) + nx
            iyp(k) = iy(k) + ny
          end do

          call areaf ( ixp, iyp, 4 )

          nl = nlmess ( cstr )
          nx = nx + ( 300 - nl ) / 2
          call messag ( cstr, nx, ny+460 )

        end do

      end do
c
c  Close DISLIN.
c
      call disfin ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX08:'
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
