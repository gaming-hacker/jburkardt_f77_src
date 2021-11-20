      program main

c*********************************************************************72
c
cc SIMPLEX_GRID_TEST tests the SIMPLEX_GRID library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SIMPLEX_GRID_TEST:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SIMPLEX_GRID library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SIMPLEX_GRID_TEST:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests SIMPLEX_GRID_SIZE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n
      integer ng
      integer temp(0:5)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) 
     &  '  SIMPLEX_GRID_SIZE counts the points in a regular grid'
      write ( *, '(a)' ) 
     &  '  with N+1 points on a side, in an M-dimensional simplex.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '        M: 0     1     2     3     4     5'
      write ( *, '(a)' ) '    N:'
      do n = 0, 10
        do m = 0, 5
          call simplex_grid_size ( m, n, ng )
          temp(m) = ng
        end do
        write ( *, '(a,i3,a,6i6)' ) '  ', n, ':', temp(0:5)
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests SIMPLEX_GRID_INDEX_NEXT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )

      integer g(m+1)
      integer j
      integer n

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  SIMPLEX_GRID_INDEX_NEXT lists, one by one, the indices'
      write ( *, '(a)' ) 
     &  '  of a simplex grid that uses N+1 points on a side, '
      write ( *, '(a)' ) '  in an M-dimensional simplex.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   #:  1  2  3  (*)'
      write ( *, '(a)' ) ''

      n = 3

      j = 0
      g(1:m) = 0
      g(m+1) = n

10    continue

        write ( *, '(2x,i2,a,3(i3),1x,a,i3,a)' ) 
     &    j, ':', g(1:m), '(', g(m+1), ')'

        if ( g(1) .eq. n ) then
          go to 20
        end if

        call simplex_grid_index_next ( m, n, g )

        j = j + 1

      go to 10

20    continue

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests SIMPLEX_GRID_INDEX_SAMPLE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )

      integer g(m+1)
      integer j
      integer n
      integer seed

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) 
     &  '  SIMPLEX_GRID_INDEX_SAMPLE returns a randomly selected'
      write ( *, '(a)' ) '  index of a simplex grid that uses N+1 points
     & on a side, '
      write ( *, '(a)' ) '  in an M-dimensional simplex.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   #:  1  2  3  (*)'
      write ( *, '(a)' ) ''

      n = 3
      seed = 123456789

      do j = 1, 20

        call simplex_grid_index_sample ( m, n, seed, g )

        write ( *, '(2x,i2,a,3(i3),1x,a,i3,a)' ) 
     &    j, ':', g(1:m), '(', g(m+1), ')'

      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests SIMPLEX_GRID_INDEX_TO_POINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m 
      parameter ( m = 2 )

      integer g(m+1)
      integer j
      integer n
      integer seed
      double precision v(m,m+1)
      double precision x(m)

      save v

      data v /
     &  20.0,  0.0,
     &  30.0, 40.0,
     &  10.0, 20.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST04:'
      write ( *, '(a)' ) 
     &  '  SIMPLEX_GRID_INDEX_TO_POINT returns the physical point'
      write ( *, '(a)' ) 
     &  '  corresponding to a grid index of a simplex grid that '
      write ( *, '(a)' ) '  that uses N+1 points on a side, '
      write ( *, '(a)' ) '  in an M-dimensional simplex.'

      n = 5

      call r8mat_transpose_print ( m, m + 1, v, '  Simplex vertices:' )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Choosing random simplex indices to convert:'
      write ( *, '(a)' ) '   #:  1  2  3     X        Y'
      write ( *, '(a)' ) ''

      seed = 123456789

      do j = 1, 10

        call simplex_grid_index_sample ( m, n, seed, g )
        call simplex_grid_index_to_point ( m, n, 1, g, v, x )

        write ( *, '(2x,i2,a,3(i3),2(2x,f8.4))' )
     &    j, ':', g(1:m+1), x(1:m)

      end do

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests SIMPLEX_GRID_INDEX_ALL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer grid(4,20)
      integer m
      integer n
      integer ng

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST05:'
      write ( *, '(a)' ) 
     &  '  SIMPLEX_GRID_INDEX_ALL returns all the indices'
      write ( *, '(a)' ) 
     &  '  of a simplex grid that uses N+1 points on a side, '
      write ( *, '(a)' ) '  in an M-dimensional simplex.'

      m = 3
      n = 3
      call simplex_grid_size ( m, n, ng )

      call simplex_grid_index_all ( m, n, ng, grid )

      call i4mat_transpose_print ( m + 1, ng, grid,
     &  '  Transposed Simplex Grid Index Matrix:' )

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests SIMPLEX_GRID_INDEX_TO_POINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 2 )

      integer grid(m+1,21)
      integer n
      integer ng
      double precision v(m,m+1)
      double precision x(m,21)

      save v

      data v /
     &  20.0,  0.0,
     &  30.0, 40.0,
     &  10.0, 20.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST06:'
      write ( *, '(a)' ) 
     &  '  SIMPLEX_GRID_INDEX_TO_POINT returns the physical point'
      write ( *, '(a)' ) 
     &  '  corresponding to a grid index of a simplex grid that '
      write ( *, '(a)' ) '  that uses N+1 points on a side, '
      write ( *, '(a)' ) '  in an M-dimensional simplex.'

      n = 5
      call simplex_grid_size ( m, n, ng )

      call r8mat_transpose_print ( m, m + 1, v, '  Simplex vertices:' )

      call simplex_grid_index_all ( m, n, ng, grid )

      call simplex_grid_index_to_point ( m, n, ng, grid, v, x )

      call r8mat_transpose_print ( m, ng, x, 
     &  '  Grid Point Coordinates:' )

      return
      end

