      program main

c*********************************************************************72
c
cc MAIN is the main program for PYRAMID_GRID_PRB.
c
c  Discussion:
c
c    PYRAMID_GRID_PRB tests the PYRAMID_GRID library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRAMID_GRID_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the PYRAMID_GRID library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PYRAMID_GRID_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests PYRAMID_GRID_SIZE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      integer ng
      integer pyramid_grid_size

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  PYRAMID_GRID_SIZE determines the size of a'
      write ( *, '(a)' ) '  pyramid grid with N+1 points on each edge.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   N    Size'
      write ( *, '(a)' ) ''
      do n = 0, 10
        ng = pyramid_grid_size ( n )
        write ( *, '(2x,i2,2x,i6)' ) n, ng
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests PYRAMID_UNIT_GRID.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ng_max
      parameter ( ng_max = 100 )

      integer n
      integer ng
      double precision pg(3,ng_max)
      integer pyramid_grid_size

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  PYRAMID_UNIT_GRID determines a unit pyramid'
      write ( *, '(a)' ) '  grid with N+1 points along each edge.'

      n = 4
      call r8_print ( n, '  Grid parameter N:' )

      ng = pyramid_grid_size ( n )
      call r8_print ( ng, '  Grid size NG:' )

      call pyramid_unit_grid ( n, ng, pg )

      call r8mat_transpose_print ( 3, ng, pg, '  Pyramid grid points:' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests PYRAMID_UNIT_GRID_PLOT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ng_max
      parameter ( ng_max = 100 )

      character * ( 255 ) header
      integer n
      integer ng
      double precision pg(3,ng_max)
      integer pyramid_grid_size

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  PYRAMID_UNIT_GRID_PLOT plots a unit pyramid'
      write ( *, '(a)' ) '  grid with N+1 points along each edge.'

      n = 5
      call r8_print ( n, '  Grid parameter N:' )

      ng = pyramid_grid_size ( n )
      call r8_print ( ng, '  Grid size NG:' )

      call pyramid_unit_grid ( n, ng, pg )

      header = 'pyramid_unit'
      call pyramid_unit_grid_plot ( n, ng, pg, header )

      return
      end
