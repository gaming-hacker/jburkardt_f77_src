      program main

c*********************************************************************72
c
cc SUBSET_SUM_SERIAL_TEST tests SUBSET_SUM_SERIAL.
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
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SUBSET_SUM_SERIAL_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the SUBSET_SUM_SERIAL library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SUBSET_SUM_SERIAL_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc SUBSET_SUM_SERIAL_TEST01 tests the SUBSET_SUM_SERIAL program.
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
      implicit none

      integer n
      parameter ( n = 21 )

      integer choice(n)
      integer i
      integer target
      integer w_sum
      integer weight(n)

      save weight

      data weight /
     &  518533, 1037066, 2074132, 1648264, 796528, 
     & 1593056,  686112, 1372224,  244448, 488896, 
     &  977792, 1955584, 1411168,  322336, 644672, 
     & 1289344,   78688,  157376,  314752, 629504, 
     & 1259008 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Test the SUBSET_SUM_SERIAL function,'
      write ( *, '(a)' ) '  which looks for a selection from a set'
      write ( *, '(a)' ) '  of weights that adds up to a given target.'
c
c  Define the problem data.
c
      target = 2463098
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Target value:'
      write ( *, '(2x,i8)' ) target
 
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   I      W(I)'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,i2,2x,i8)' ) i, weight(i)
      end do

      call subset_sum_serial ( n, weight, target, choice )

      if ( choice(1) .eq. -1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '  No solution was found.'
      else
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '   I*     W*'
        write ( *, '(a)' ) ''
        w_sum = 0
        do i = 1, n
          if ( choice(i) .eq. 1 ) then
            w_sum = w_sum + weight(i)
            write ( *, '(2x,i2,2x,i8)' ) i, weight(i)
          end if
        end do
        write ( *, '(a)' ) ''
        write ( *, '(a,i12)' ) '  Sum:    ', w_sum
       write ( *, '(a,i12)' ) '  Target: ', target
      end if

      return
      end

