      program main

c*********************************************************************72
c
cc MAIN is the main program for LATIN_RANDOM_PRB.
c
c  Discussion:
c
c    LATIN_RANDOM_PRB tests the LATIN_RANDOM library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer seed
      integer test

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LATIN_RANDOM_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the LATIN_RANDOM library.'

      seed = 123456789

      do test = 1, 3

        call test01 ( seed )

      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LATIN_RANDOM_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( seed )

c*********************************************************************72
c
cc TEST01 tests LATIN_RANDOM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random number generator.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer point_num
      parameter ( point_num = 10 )

      integer j
      integer seed
      double precision x(dim_num,point_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  LATIN_RANDOM chooses a random Latin Square'
      write ( *, '(a)' ) '  cell arrangement, and then returns'
      write ( *, '(a)' ) '  a random point from each cell.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)'  ) '  Spatial dimension =  ', dim_num
      write ( *, '(a,i6)'  ) '  Number of points =   ', point_num
      write ( *, '(a,i12)' ) '  Random number SEED = ', seed

      call latin_random ( dim_num, point_num, seed, x )

      call r8mat_transpose_print ( dim_num, point_num, x,
     &  '  The Latin Random points:' )

      return
      end
