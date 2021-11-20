      program main

c*********************************************************************72
c
cc MAIN is the main program for CELLULAR_AUTOMATON.
c
c  Discussion:
c
c    This program carries out iterations of the 1D cellular automaton
c    known as rule 30.
c
c    Given an initial linear array of 0's and 1's, rule 30 produces a new
c    array using the rules:
c
c      111  110  101  100  011  010  001  000
c       V    V    V    V    V    V    V    V
c       0    0    0    1    1    1    1    0     
c
c    Note that there are 256 = 2^8 possible ways to fill in this output
c    chart, and that rule 30 gets its index by the fact that
c    (0,0,0,1,1,1,1,0) can be interpreted as the binary representation of 30.
c
c    For instance, if the current values of X(4), X(5) and X(6) are
c    0, 1 and 1, respectively, then the new value of X(5) will be 1.
c
c    The first and last entries of the array must be treated specially, since
c    they don't have a left or right neighbor.  One simple treatment is 
c    to assume that there are phantom neighbors whose values are both 0.
c    Another is to enforce periodic boundary conditions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    116 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Stephen Wolfram,
c    A New Kind of Science,
c    Wolfram Media, 2002,
c    ISBN13: 978-1579550080,
c    LC: QA267.5.C45.W67.
c
      implicit none

      integer n
      parameter ( n = 80 )

      integer i
      integer j
      integer step_num
      character x(0:n+1)
      character x_old(0:n+1)

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CELLULAR_AUTOMATON:'
      write ( *, '(a)' ) '  FORTRAN77 version.'

      step_num = 80

      x(0:n+1) = ' '
      x(40) = '*'

      write ( *, '(80a)' ) x(1:n)

      do j = 1, step_num

        do i = 0, n + 1
          x_old(i) = x(i)
        end do

        do i = 1, n
c
c  The transformation rules are:
c
c  111  110  101  100  011  010  001  000
c   |    |    |    |    |    |    |    |
c   0    0    0    1    1    1    1    0
c
c  which means this rule has binary code 00011110 = 16 + 8 + 4 + 2 = 30
c
          if ( ( x_old(i-1) .eq. ' ' .and. 
     &           x_old(i)   .eq. ' ' .and. 
     &           x_old(i+1) .eq. '*' ) .or. 
     &         ( x_old(i-1) .eq. ' ' .and. 
     &           x_old(i)   .eq. '*' .and. 
     &           x_old(i+1) .eq. ' ' ) .or. 
     &         ( x_old(i-1) .eq. ' ' .and. 
     &           x_old(i)   .eq. '*' .and. 
     &           x_old(i+1) .eq. '*' ) .or. 
     &         ( x_old(i-1) .eq. '*' .and. 
     &           x_old(i)   .eq. ' ' .and. 
     &           x_old(i+1) .eq. ' ' ) ) then
            x(i) = '*'
          else
            x(i) = ' '
          end if

        end do
c
c  Enforce periodic boundary conditions.
c
        x(0) = x(n)
        x(n+1) = x(1)

        write ( *, '(80a)' ) x(1:n)

      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CELLULAR_AUTOMATON:'
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
