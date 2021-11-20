      program main

c*********************************************************************72
c
cc MAIN is the main program for BACKTRACK_BINARY_RC_PRB.
c
c  Discussion:
c
c    BACKTRACK_BINARY_RC_PRB tests BACKTRACK_BINARY_RC.
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
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BACKTRACK_BINARY_RC_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BACKTRACK_BINARY_RC library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BACKTRACK_BINARY_RC_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 seeks a selection of binary powers that have a given sum.
c
c  Discussion:
c
c    We consider the binary powers 1, 2, 4, ... 2^(n-1).
c
c    We wish to select some of these powers, so that the sum is equal
c    to a given target value.  We are actually simply seeking the binary
c    representation of an integer.
c
c    A partial solution is acceptable if it is less than the target value.
c
c    We list the powers in descending order, so that the bactracking
c    procedure makes the most significant choices first, thus quickly
c    eliminating many unsuitable choices.
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
      implicit none

      integer n
      parameter ( n = 8 )
      integer test_num
      parameter ( test_num = 3 )

      integer call_num
      integer choice(n)
      integer factor
      integer i
      integer n2
      logical reject
      integer result
      integer target
      integer targets(test_num)
      integer test

      save targets

      data targets / 73, 299, -3 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Use BACKBIN_RC to find the binary expansion of'
      write ( *, '(a)' ) '  an integer between 0 and 255.'
      write ( *, '(a)' ) '  The choices are 0/1 for the 8 digits.'

      do test = 1, test_num

        target = targets(test)
        write ( *, '(a)' ) ' '
        write ( *, '(a,i4)' ) '  TARGET = ', target
        call_num = 0
        n2 = -1

10      continue

          call backbin_rc ( n, reject, n2, choice )
          call_num = call_num + 1

          if ( n2 .eq. -1 ) then
            write ( *, '(a)' ) '  Termination without solution.'
            go to 20
          end if
c
c  Evaluate the integer determined by the choices.
c
          factor = 1
          do i = n, n2 + 1, -1
            factor = factor * 2
          end do

          result = 0
          do i = 1, n2
            result = result * 2 + choice(i)
          end do

          result = result * factor
c
c  If the integer is too big, then we reject it, and
c  all the related integers formed by making additional choices.
c
          reject = ( target .lt. result )
c
c  If we hit the target, then in this case, we can exit because
c  the solution is unique.
c
          if ( result .eq. target ) then
            go to 20
          end if

        go to 10

20      continue

        write ( *, '(a,i6)' ) '  Number of calls = ', call_num
        write ( *, '(a,i10)' ) '  Binary search space = ', 2 ** n
        write ( *, '(2x,10i2)' ) choice(1:n)

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 seeks a subset of a set of numbers which add to a given sum.
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
      implicit none

      integer n
      parameter ( n = 8 )

      integer call_num
      integer choice(n)
      integer i
      integer n2
      logical reject
      integer result
      integer target
      parameter ( target = 53 )
      integer test
      integer w(n)

      save w

      data w / 15, 22, 14, 26, 32, 9, 16, 8 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Use BACKBIN_RC to seek subsets of a set W'
      write ( *, '(a)' ) '  that sum to a given target value.'
      write ( *, '(a)' ) 
     &  '  The choices are 0/1 to select each element of W.'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) '  TARGET = ', target
      write ( *, '(a)' ) ' '
      call_num = 0
      n2 = -1

10    continue

        call backbin_rc ( n, reject, n2, choice )
        call_num = call_num + 1

        if ( n2 .eq. -1 ) then
          go to 20
        end if
c
c  Evaluate the partial sum.
c
        result = 0
        do i = 1, n2
          result = result + choice(i) * w(i)
        end do
c
c  If the sum is too big, then we reject it, and
c  all the related sums formed by making additional choices.
c
        reject = ( target .lt. result )
c
c  If we hit the target, print out the information.
c
        if ( result .eq. target .and. n2 .eq. n ) then
          write ( *, '(2x,10i2)' ) choice(1:n)
        end if

      go to 10

20    continue

      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of calls = ', call_num
      write ( *, '(a,i10)' ) '  Binary search space = ', 2 ** n

      return
      end
