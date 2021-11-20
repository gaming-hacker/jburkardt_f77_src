      program main

c*********************************************************************72
c
cc MAIN is the main program for BOUNDER.
c
c  Discussion:
c
c    BOUNDER is a program that generates an out-of-bounds array access.
c
c  Modified:
c
c    17 February 2017
c
c  Author:
c
c    John Burkardt
c
      implicit none

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BOUNDER'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Generate an out-of-bounds array access.'

      call test01
      call test02
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BOUNDER'
      write ( *, '(a)' ) '  Normal end of execution.'

      stop
      end
      subroutine test01

c*********************************************************************72
c
cc TEST01 is the CORRECT code.
c
      implicit none

      integer, parameter :: n = 10

      integer a(n)
      integer b(n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  The CORRECT code.'

      do i = 1, n
        a(i) = i + 1
      end do
      a(n) = 1

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  A(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2i6)' ) i, a(i)
      end do

      do i = 1, n
        b(i) = - 1000 - i
      end do

      do i = 1, n
        j = a(i)
        b(j) = j + 1
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  B(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2i6)' ) i, b(i)
      end do

      return
      end
      subroutine test02

c*****************************************************************************80
c
cc TEST02 is the INCORRECT code.
c
      implicit none

      integer, parameter :: n = 10

      integer a(n)
      integer b(n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  The INCORRECT code.'

      do i = 1, n
        a(i) = i + 1
      end do
      a(n) = 100000

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  A(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2i6)' ) i, a(i)
      end do

      do i = 1, n
        b(i) = - 1000 - i
      end do

      do i = 1, n
        j = a(i)
        b(j) = j + 1
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I  B(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2i6)' ) i, b(i)
      end do

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
