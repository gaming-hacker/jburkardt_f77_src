      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS723_PRB.
c
c  Discussion:
c
c    TOMS723_PRB tests the TOMS723 library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 November 2015
c
c  Author:
c
c    W van Snyder
c    Modifications by John Burkardt
c
c  Reference:
c
c    W van Snyder,
c    Algorithm 723: Fresnel Integrals,
c    ACM Transactions on Mathematical Software,
c    Volume 19, Number 4, December 1993, pages 452-456.
c
      implicit none

      double precision dfrenc
      external dfrenc
      double precision dfrenf
      external dfrenf
      double precision dfreng
      external dfreng
      double precision dfrens
      external dfrens
      integer i
      double precision x
      double precision yc
      double precision yf
      double precision yg
      double precision ys

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS723_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS723 library.'

      write ( *, '(a)' ) ''
      write ( *, '(11x,a,12x,a,11x,a,11x,a,11x,a)' ),
     &  'X', 'C(x)', 'S(x)', 'g(x)', 'f(x)'
      write ( *, '(a)' ) ''

      do i = -12, 12
        x = 0.5D+00 * dble ( i )
        yc = dfrenc ( x )
        ys = dfrens ( x )
        yg = dfreng ( x )
        yf = dfrenf ( x )
        print '(1p5e15.07)', x, yc, ys, yg, yf
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS723_PRB'
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
c    12 June 2014
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
     &  d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, 
     &  trim ( ampm )

      return
      end
