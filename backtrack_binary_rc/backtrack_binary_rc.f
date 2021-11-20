      subroutine backbin_rc ( n, reject, n2, choice )

c*********************************************************************72
c
cc BACKBIN_RC uses reverse communication for binary backtracking.
c
c  Discussion:
c
c    If this procedure returns a solution with N2 = N, which is acceptable
c    to the user, then a full solution has been found.
c
c    If this procedure returns N2 = -1, no more potential solutions are
c    available to consider.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the full solution.
c
c    Input, logical REJECT, is TRUE if the proposed partial solution
c    in the first N2 entries of CHOICE must be rejected.
c
c    Input/output, integer N2, the length of the current
c    partial solution.  On first call for a given problem, the user
c    should set N2 to -1.  If the program has exhausted the search space,
c    the value of N2 will be returned as -1.
c
c    Input/output, integer CHOICE(N), indicates the current
c    partial solution in entries 1 through N2, which will contain 0 or 1.
c
      implicit none

      integer n

      integer choice(n)
      integer i
      integer n2
      logical reject
c
c  N2 = -1 means an initialization call.
c
      if ( n2 .eq. -1 ) then

        do i = 1, n
          choice(i) = -1
        end do
        n2 = 1
        choice(n2) = 1
c
c  1 <= FOCUS means we asked the user to evaluate CHOICE(1:N2).
c
c  N2 = N means we returned a full prospective solution
c  so in any case we must increment CHOICE.
c
c  Returning REJECT = 1 means no solution begins this way
c  so we must increment CHOICE.
c
      else if ( n2 .eq. n .or. reject ) then

10      continue

        if ( 1 .lt. n2 ) then
          if ( choice(n2) .eq. 1 ) then
            choice(n2) = 0
            go to 20
          end if
          choice(n2) = -1
          n2 = n2 - 1
          go to 10
        end if

20      continue
c
c  Have we exhausted the solution space?
c
        if ( n2 .eq. 1 ) then
          if ( choice(n2) .eq. 1 ) then
            choice(n2) = 0
          else
            choice(n2) = -1
            n2 = -1
          end if
        end if
c
c  N2 < N and not REJECT means we can increment N2.
c
      else

        n2 = n2 + 1
        choice(n2) = 1

      end if

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
