      subroutine change_making_list ( coin_num, coin_value, target, a )

c*********************************************************************72
c
cc CHANGE_MAKING_LIST solves the change making problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer COIN_NUM, the number of coin denomiations.
c
c    Input, integer COIN_VALUE(COIN_NUM), the value of each coin.
c    These values should be positive integers.
c
c    Input, integer TARGET, the desired sum.
c
c    Output, integer A(0:TARGET), A(T) lists the smallest number
c    of coins needed to form the sum T, or "Inf" if it is not possible to form
c    this sum.
c
      implicit none

      integer coin_num
      integer target

      integer a(0:target)
      integer coin_value(coin_num)
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer j

      a(0) = 0
      do i = 1, target
        a(i) = i4_huge
      end do
c
c  If T is the value of a coin, then A(T) is 1.
c
      do i = 1, coin_num
        a(coin_value(i)) = 1
      end do
c
c  To compute A(T) in general, consider getting there by adding
c  one coin of value V, and looking at A(T-V).
c
      do j = 1, target
        do i = 1, coin_num
          if ( 0 .le. j - coin_value(i) ) then
            a(j) = min ( a(j) - 1, a(j-coin_value(i)) ) + 1
          end if
        end do
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
