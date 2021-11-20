      program main

c*********************************************************************72
c
cc MAIN is the main program for CHANGE_MAKING_PRB.
c
c  Discussion:
c
c    CHANGE_MAKING_PRB tests the CHANGE_MAKING library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CHANGE_MAKING_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CHANGE_MAKING library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CHANGE_MAKING_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc CHANGE_MAKING_TEST01 lists the problem data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer coin_num_max
      parameter ( coin_num_max = 7 )
      integer coin_value_list_num
      parameter ( coin_value_list_num = 33 )
      integer test_num
      parameter ( test_num = 7 )

      integer coin_num
      integer coin_num_list(test_num)
      integer coin_offset
      integer coin_offset_list(test_num)
      integer coin_value(coin_num_max)
      integer coin_value_list(coin_value_list_num)
      integer i
      integer target
      integer target_list(test_num)
      integer test

      save coin_num_list
      save coin_offset_list
      save coin_value
      save target_list

      data coin_num_list /
     &  3, 
     &  5, 
     &  6, 
     &  7, 
     &  3, 
     &  6, 
     &  3 /
      data coin_offset_list /
     &   1, 
     &   4, 
     &   9, 
     &  15, 
     &  22, 
     &  25, 
     &  31 /
      data coin_value_list /
     &   5,  9, 13, 
     &   1,  4,  5,  8, 11, 
     &   1,  5, 10, 25, 50, 100, 
     &   1,  2,  6, 12, 24,  48,  60, 
     &   1,  3,  4, 
     &  16, 17, 23, 24, 39,  40, 
     &   6,  9, 20 /
      data target_list /
     &   19, 
     &   29, 
     &   96, 
     &   96, 
     &    6, 
     &  100, 
     &   43 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) '  List the problem data.'

      do test = 1, test_num

        coin_num = coin_num_list(test)
        coin_offset = coin_offset_list(test)
        do i = 1, coin_num
          coin_value(i) = coin_value_list(i+coin_offset-1)
        end do
        target = target_list(test)

        write ( *, '(a)' ) ''
        write ( *, '(a,i1,a)' ) '  Test ', test, ':'
        write ( *, '(a,i4)' ) '  Number of coins = ', coin_num
        write ( *, '(a)' ) '  Values = '
        do i = 1, coin_num
          write ( *, '(2x,i4)' ) coin_value(i)
        end do
        write ( *, '(a)' ) ''
        write ( *, '(a,i4)' ) '  Target = ', target

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc CHANGE_MAKING_TEST02 uses CHANGE_MAKING_LIST on the problems.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer coin_num_max
      parameter ( coin_num_max = 7 )
      integer coin_value_list_num
      parameter ( coin_value_list_num = 33 )
      integer target_max
      parameter ( target_max = 100 )
      integer test_num
      parameter ( test_num = 7 )

      integer a(0:target_max)
      integer coin_num
      integer coin_num_list(test_num)
      integer coin_offset
      integer coin_offset_list(test_num)
      integer coin_value(coin_num_max)
      integer coin_value_list(coin_value_list_num)
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer target
      integer target_list(test_num)
      integer test

      save coin_num_list
      save coin_offset_list
      save coin_value
      save target_list

      data coin_num_list /
     &  3, 
     &  5, 
     &  6, 
     &  7, 
     &  3, 
     &  6, 
     &  3 /
      data coin_offset_list /
     &   1, 
     &   4, 
     &   9, 
     &  15, 
     &  22, 
     &  25, 
     &  31 /
      data coin_value_list /
     &   5,  9, 13, 
     &   1,  4,  5,  8, 11, 
     &   1,  5, 10, 25, 50, 100, 
     &   1,  2,  6, 12, 24,  48,  60, 
     &   1,  3,  4, 
     &  16, 17, 23, 24, 39,  40, 
     &   6,  9, 20 /
      data target_list /
     &   19, 
     &   29, 
     &   96, 
     &   96, 
     &    6, 
     &  100, 
     &   43 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  CHANGE_MAKING LIST computes A(T), the smallest number'
      write ( *, '(a)' ) 
     &  '  of coins needed to form a given sum T, by computing'
      write ( *, '(a)' ) '  the list A(0) through A(T).'

      do test = 1, test_num

        coin_num = coin_num_list(test)
        coin_offset = coin_offset_list(test)
        do i = 1, coin_num
          coin_value(i) = coin_value_list(i+coin_offset-1)
        end do
        target = target_list(test)

        write ( *, '(a)' ) ''
        write ( *, '(a,i1,a)' ) '  Test ', test, ':'
        write ( *, '(a,i4)' ) '  Number of coins = ', coin_num
        write ( *, '(a)' ) '  Values = '
        do i = 1, coin_num
          write ( *, '(2x,i4)' ) coin_value(i)
        end do
        write ( *, '(a)' ) ''
        write ( *, '(a,i4)' ) '  Target = ', target

        call change_making_list ( coin_num, coin_value, target, a )

        write ( *, '(a)' ) ''
        do i = 0, target
          if ( a(i) .eq. i4_huge ) then
            write ( *, '(2x,i4,a)' ) i, '  Not possible!'
          else
            write ( *, '(2x,i4,2x,i4)' ) i, a(i)
          end if
        end do

      end do

      return
      end

