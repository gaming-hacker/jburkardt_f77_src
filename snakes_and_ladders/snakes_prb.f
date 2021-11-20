      program main

c*********************************************************************72
c
cc MAIN is the main program for SNAKES_PRB.
c
c  Discussion:
c
c    SNAKES_PRB tests the SNAKES library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SNAKES_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SNAKES library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SNAKES_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests SPY_GE for the SNAKES matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a(0:100,0:100)
      character * ( 255 ) header
      integer m
      integer n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  SNAKES sets up the snakes and ladders matrix.'
      write ( *, '(a)' ) 
     &  '  SPY_GE generates a sparsity plot for a matrix stored'
      write ( *, '(a)' ) '  in general (GE) format.'
 
      call snakes ( a )
      header = 'snakes'

      m = 101
      n = 101
      call spy_ge ( m, n, a, header )

      return
      end

