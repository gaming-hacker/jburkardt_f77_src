      subroutine knapsack_01 ( n, w, c, s )

c*********************************************************************72
c
cc KNAPSACK_01 seeks a solution of the 0/1 Knapsack problem.
c
c  Discussion:
c
c    In the 0/1 knapsack problem, a knapsack of capacity C is given,
c    as well as N items, with the I-th item of weight W(I).
c
c    A selection is "acceptable" if the total weight is no greater than C.
c
c    It is desired to find an optimal acceptable selection, that is,
c    an acceptable selection such that there is no acceptable selection
c    of greater weight.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of weights.
c
c    Input, integer W(N), the weights.
c
c    Input, integer C, the maximum weight.
c
c    Output, integer S(N), is a binary vector which defines an
c    optimal selection.  It is 1 for the weights to be selected, and
c    0 otherwise.
c
      implicit none

      integer n

      integer c
      integer i
      integer iadd
      logical more
      integer ncard
      integer s(n)
      integer s_test(n)
      integer t
      integer t_test
      integer w(n)

      more = .false.
      ncard = 0

      do i = 1, n
        s_test(i) = 0
      end do
      t_test = 0

      do i = 1, n
        s(i) = s_test(i)
      end do
      t = 0

10    continue

        call subset_gray_next ( n, s_test, more, ncard, iadd )

        t_test = 0
        do i = 1, n
          t_test = t_test + s_test(i) * w(i)
        end do

        if ( t .lt. t_test .and. t_test .le. c ) then
          t = t_test
          do i = 1, n
            s(i) = s_test(i)
          end do
        end if

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
      subroutine subset_gray_next ( n, a, more, ncard, iadd )

c*********************************************************************72
c
cc SUBSET_GRAY_NEXT generates all subsets of a set of order N, one at a time.
c
c  Discussion:
c
c    It generates the subsets one at a time, by adding or subtracting
c    exactly one element on each step.
c
c    This uses a Gray code ordering of the subsets.
c
c    The user should set MORE = FALSE and the value of N before
c    the first call.  On return, the user may examine A which contains
c    the definition of the new subset, and must check MORE, because
c    as soon as it is FALSE on return, all the subsets have been
c    generated and the user probably should cease calling.
c
c    The first set returned is the empty set.
c
c  Example:
c
c    N = 4
c
c    0 0 0 0
c    1 0 0 0
c    1 1 0 0
c    0 1 0 0
c    0 1 1 0
c    1 1 1 0
c    1 0 1 0
c    0 0 1 0
c    0 0 1 1
c    1 0 1 1
c    1 1 1 1
c    0 1 1 1
c    0 1 0 1
c    1 1 0 1
c    1 0 0 1
c    0 0 0 1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 January 2007
c
c  Author:
c
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms for Computers and Calculators,
c    Second Edition,
c    Academic Press, 1978,
c    ISBN: 0-12-519260-6,
c    LC: QA164.N54.
c
c  Parameters:
c
c    Input, integer N, the order of the total set from which
c    subsets will be drawn.
c
c    Input/output, integer A(N).  On each return, the Gray code for the newly
c    generated subset.  A(I) = 0 if element I is in the subset, 1 otherwise.
c
c    Input/output, logical MORE.  Set this variable FALSE before
c    the first call.  Normally, MORE will be returned TRUE but once
c    all the subsets have been generated, MORE will be
c    reset FALSE on return and you should stop calling the program.
c
c    Input/output, integer NCARD, the cardinality of the set returned,
c    which may be any value between 0 (the empty set) and N (the
c    whole set).
c
c    Output, integer IADD, the element which was added or removed to the
c    previous subset to generate the current one.  Exception:
c    the empty set is returned on the first call, and IADD is set to 0.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer iadd
      logical more
      integer ncard
c
c  The first set returned is the empty set.
c
      if ( .not. more ) then

        do i = 1, n
          a(i) = 0
        end do

        iadd = 0
        ncard = 0
        more = .true.

      else

        iadd = 1

        if ( mod ( ncard, 2 ) .ne. 0 ) then

10        continue

            iadd = iadd + 1
            if ( a(iadd-1) .ne. 0 ) then
              go to 20
            end if

          go to 10

20        continue

        end if

        a(iadd) = 1 - a(iadd)
        ncard = ncard + 2 * a(iadd) - 1
c
c  The last set returned is the singleton A(N).
c
        if ( ncard .eq. a(n) ) then
          more = .false.
        end if

      end if

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
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
