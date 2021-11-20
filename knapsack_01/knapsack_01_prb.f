      program main

c*********************************************************************72
c
cc KNAPSACK_01_TEST tests the KNAPSACK_01 library.
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
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'KNAPSACK_01_TEST'
      write ( *, '(a)' ) '  FORTRAN90 version.'
      write ( *, '(a)' ) '  Test the KNAPSACK_01 library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'KNAPSACK_01_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 seeks a solution of the 0/1 Knapsack problem.
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
c    22 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 6 )

      integer c
      integer i
      integer s(n)
      integer t
      integer w(n)

      save w

      data w /
     &  16, 17, 23, 24, 39, 40 /

      c = 100

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a,i4)' ) '  Knapsack maximum capacity is ', c
      write ( *, '(a)' ) 
     &  '  Come as close as possible to filling the knapsack.'

      call knapsack_01 ( n, w, c, s )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   # 0/1  Weight'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,i2,2x,i1,2x,i4)' ) i, s(i), w(i)
      end do
      t = 0
      do i = 1, n
        t = t + s(i) * w(i)
      end do
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Total: ', t

      return
      end
