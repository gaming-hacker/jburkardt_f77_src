      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS632_PRB.
c
c  Discussion:
c
c    TOMS632_PRB tests the TOMS632 library.
c
c    This program solves a 0-1 multiple knapsack problem of N items
c    and M knapsacks.
c
c    The arrays in the test program and the library are currently
c    dimensioned to allow problems for which N <= 200 and M <= 4.
c
c    The program may be tests on the following sets of profits and weights:
c
c      N =    10
c      P =    92   57   49   68   60   43   67   84   87   72
c      W =    23   31   29   44   53   38   63   85   89   82
c
c    Four test problems are given with the following knapsack capacities K:
c
c      Problem 1
c      M =     2
c      K =    70  127
c
c      Problem 2
c      M =     3
c      K =    50   81  120
c
c      Problem 3
c      M =     4
c      K =    31   37   48  152
c
c      Problem 4 (0-1 single knapsack problem)
c      M =     1
c      K =   165
c
c  Modified:
c
c    24 October 2013
c
c  Author:
c
c    Silvano Martello, Paolo Toth
c
c  Reference:
c
c    Silvano Martello, Paolo Toth,
c    MKP: 0-1 multiple knapsack problem,
c    ACM Transactions on Mathematical Software,
c    Volume 11, Number 2, June 1985, pages 135-140.
c

      implicit none

      integer m_max
      parameter ( m_max = 4 )
      integer n_max
      parameter ( n_max = 200 )

      integer bck
      integer i
      integer j
      integer k(m_max)
      integer m
      integer n
      integer p(n_max)
      integer vstar
      integer w(n_max)
      integer xstar(n_max)

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS632_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS632 library.'
c
c  Read the profits and weights.
c
      read ( *, * ) n

      if ( n_max < n ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TOMS632_PRB - Fatal error!'
        write ( *, '(a)' ) '  Input value of N exceeds N_MAX.'
        stop 1
      end if
      write ( *, '(a)' ) ''
      write ( *, '(2x,a6,i5)' ) 'N:    ', n

      read ( *, * ) ( p(j), j = 1, n )
      write ( *, '(2x,a6,10i5)' ) 'P:    ', ( p(j), j = 1, n )

      read ( *, * ) ( w(j), j = 1, n )
      write ( *, '(2x,a6,10i5)' ) 'W:    ', ( w(j), j = 1, n )
c
c  Problem 1.
c
      read ( *, * ) m
      if ( m_max < m ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TOMS632_PRB - Fatal error!'
        write ( *, '(a)' ) '  Input value of M exceeds M_MAX.'
        stop 1
      end if

      write ( *, '(a)' ) ''
      write ( *, '(2x,a6,i5)' ) 'M:    ', m

      read ( *, * ) ( k(i), i = 1, m )
      write ( *, '(2x,a6,10i5)' ) 'K:    ', ( k(i), i = 1, m )

      bck = - 1
      call mkp ( n, m, p, w, k, bck, xstar, vstar )
      write ( *, '(2x,a6,i5)' ) 'VSTAR:', vstar
      write ( *, '(2x,a6,10i5)' ) 'XSTAR:', ( xstar(j), j = 1, n )
      write ( *, '(2x,a6,i5)' ) 'BCK:  ', bck
c
c  Problem 2.
c
      read ( *, * ) m
      if ( m_max < m ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TOMS632_PRB - Fatal error!'
        write ( *, '(a)' ) '  Input value of M exceeds M_MAX.'
        stop 1
      end if

      write ( *, '(a)' ) ''
      write ( *, '(2x,a6,i5)' ) 'M:    ', m

      read ( *, * ) ( k(i), i = 1, m )
      write ( *, '(2x,a6,10i5)' ) 'K:    ', ( k(i), i = 1, m )

      bck = - 1
      call mkp ( n, m, p, w, k, bck, xstar, vstar )
      write ( *, '(2x,a6,i5)' ) 'VSTAR:', vstar
      write ( *, '(2x,a6,10i5)' ) 'XSTAR:', ( xstar(j), j = 1, n )
      write ( *, '(2x,a6,i5)' ) 'BCK:  ', bck
c
c  Problem 3.
c
      read ( *, * ) m
      if ( m_max < m ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TOMS632_PRB - Fatal error!'
        write ( *, '(a)' ) '  Input value of M exceeds M_MAX.'
        stop 1
      end if

      write ( *, '(a)' ) ''
      write ( *, '(2x,a6,i5)' ) 'M:    ', m

      read ( *, * ) ( k(i), i = 1, m )
      write ( *, '(2x,a6,10i5)' ) 'K:    ', ( k(i), i = 1, m )
c
c  Optimal solution.
c
      bck = - 1
      call mkp ( n, m, p, w, k, bck, xstar, vstar )
      write ( *, '(2x,a6,i5)' ) 'VSTAR:', vstar
      write ( *, '(2x,a6,10i5)' ) 'XSTAR:', ( xstar(j), j = 1, n )
      write ( *, '(2x,a6,i5)' ) 'BCK:  ', bck
c
c  Heuristic solution.
c
      bck = 10
      write ( *, '(a)' ) ''
      write ( *, '(a,i4,a)' ) '  Retry using no more than BCK = ',
     &  bck, ' steps'
      write ( *, '(a)' ) ''

      call mkp ( n, m, p, w, k, bck, xstar, vstar )
      write ( *, '(2x,a6,i5)' ) 'VSTAR:', vstar
      write ( *, '(2x,a6,10i5)' ) 'XSTAR:', ( xstar(j), j = 1, n )
      write ( *, '(2x,a6,i5)' ) 'BCK:  ', bck
c
c  Problem 4.
c
      read ( *, * ) m
      if ( m_max < m ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TOMS632_PRB - Fatal error!'
        write ( *, '(a)' ) '  Input value of M exceeds M_MAX.'
        stop 1
      end if

      write ( *, '(a)' ) ''
      write ( *, '(2x,a6,i5)' ) 'M:    ', m

      read ( *, * ) ( k(i), i = 1, m )
      write ( *, '(2x,a6,10i5)' ) 'K:    ', ( k(i), i = 1, m )

      bck = - 1
      call mkp ( n, m, p, w, k, bck, xstar, vstar )
      write ( *, '(2x,a6,i5)' ) 'VSTAR:', vstar
      write ( *, '(2x,a6,10i5)' ) 'XSTAR:', ( xstar(j), j = 1, n )
      write ( *, '(2x,a6,i5)' ) 'BCK:  ', bck
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS632_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end

