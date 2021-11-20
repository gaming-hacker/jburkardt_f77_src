      subroutine i4_to_digits_binary ( i, n, c )

c*********************************************************************72
c
cc I4_TO_DIGITS_BINARY produces the binary digits of an I4.
c
c  Discussion:
c
c    An I4 is an integer.
c
c  Example:
c
c     I    N     C               Binary
c    --  ---   ---         ------------
c     0    1   0                      0
c     0    2   0, 0                  00
c     1    3   1, 0, 0              100
c     2    3   0, 1, 0              010
c     3    3   1, 1, 0              011
c     4    3   0, 0, 1              100
c     8    3   0, 0, 0           (1)000
c     8    5   0, 0, 0, 1, 0      01000
c    -8    5   0, 0, 0, 1, 0  (-) 01000
c
c     0    3   0, 0, 0
c     1    3   1, 0, 0
c     2    3   0, 1, 0
c     3    3   1, 1, 0
c     4    3   0, 0, 1
c     5    3   1, 0, 1
c     6    3   0, 1, 1
c     7    3   1, 1, 1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 December 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, an integer to be represented.
c
c    Input, integer N, the number of binary digits to produce.
c
c    Output, integer C(N), the first N binary digits of I,
c    with C(1) being the units digit.
c
      implicit none

      integer n

      integer c(n)
      integer i
      integer i_copy
      integer j

      i_copy = abs ( i )

      do j = 1, n

        c(j) = mod ( i_copy, 2 )
        i_copy = i_copy / 2

      end do

      return
      end
      function i4vec_dot_product ( n, x, y )

c*********************************************************************72
c
cc I4VEC_DOT_PRODUCT computes the dot product of two I4VEC's.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 December 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the size of the array.
c
c    Input, integer X(N), Y(N), the arrays.
c
c    Output, integer I4VEC_DOT_PRODUCT, the dot product of X and Y.
c
      implicit none

      integer n

      integer i
      integer i4vec_dot_product
      integer value
      integer x(n)
      integer y(n)

      value = 0
      do i = 1, n
        value = value + x(i) * y(i)
      end do

      i4vec_dot_product = value

      return
      end
      subroutine subset_sum_serial ( n, weight, target, choice )

c*********************************************************************72
c
cc SUBSET_SUM_SERIAL seeks a subset of a set that has a given sum.
c
c  Discussion:
c
c    This function tries to compute a target value as the sum of
c    a selected subset of a given set of weights.
c
c    This function works by brute force, that is, it tries every
c    possible subset to see if it sums to the desired value.
c
c    Given N weights, every possible selection can be described by 
c    one of the N-digit binary numbers from 0 to 2^N-1.
c
c    It is possible that there may be multiple solutions of the problem.  
c    This function will only return the first solution found.
c
c  Example:
c
c    n = 6
c    target = 22
c    w = (/ 1, 2, 4, 8, 16, 32 /)
c
c    choice = (/ 0, 1, 1, 0, 1, 0 /)
c    w(choice) = 2 + 4 + 16 = 22
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of weights.
c
c    Input, integer WEIGHT(N), the weights.
c
c    Input, integer TARGET, the target value.
c
c    Output, integer CHOICE(N), contains a 1 for each
c    weight that is chosen.  If no solution was found, all entries
c    are returned as -1.
c
      implicit none

      integer n

      integer choice(n)
      integer i
      integer i_max
      integer i4vec_dot_product
      integer target
      integer w_sum
      integer weight(n)

      i_max = ( 2 ** n ) - 1

      do i = 0, i_max
c
c  Convert I to a string of binary digits.
c
        call i4_to_digits_binary ( i, n, choice )
c
c  Combine the weights whose binary digit is 1.
c
        w_sum = i4vec_dot_product ( n, choice, weight )
c
c  Return if we matched our target sum.
c
        if ( w_sum .eq. target ) then
          return
        end if
    
      end do

      do i = 1, n
        choice(i) = -1
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
