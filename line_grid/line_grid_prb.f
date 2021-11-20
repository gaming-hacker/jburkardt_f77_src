      program main

c*********************************************************************72
c
cc MAIN is the main program for LINE_GRID_PRB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LINE_GRID_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the LINE_GRID library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LINE_GRID_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests LINE_GRID using the same parameters for all dimensions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 11 )

      double precision a
      double precision b
      integer c
      double precision x(n)

      a = -1.0D+00
      b = +1.0D+00
      c = 1

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Create a grid using LINE_GRID.'
      write ( *, '(a)' ) '  Use simple parameters.'
      write ( *, '(a,i4)' ) '  Number of grid points N = ', n
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     N      C      A         B'
      write ( *, '(a)' ) ''
      write ( *, '(2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) 
     &    n, c, a, b

      call line_grid ( n, a, b, c, x )
      call r8vec_print ( n, x, '  Grid points:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses a different number of points in each coordinate.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      integer c
      integer n
      integer test
      double precision x(39)

      a = 0.0D+00
      b = 1.0D+00
      c = 2

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Create a grid using LINE_GRID.'
      write ( *, '(a)' ) 
     &  '  Try an increasing number of points'
      
      n = 4

      do test = 1, 3

        n = 2 * n + 1

        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '     N      C      A         B'
        write ( *, '(a)' ) ''
        write ( *, '(2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) 
     &    n, c, a, b

        call line_grid ( n, a, b, c, x )
        call r8vec_print ( n, x, '  Grid points:' )

      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 uses a line with different sizes in each dimension.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 11 )

      double precision a
      double precision b
      integer c
      double precision x(n)

      a = 0.0D+00
      b = 10.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  Try the different centerings.'
      write ( *, '(a,i4)' ) '  Number of grid points N = ', n

      do c = 1, 5

        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '     N      C      A         B'
        write ( *, '(a)' ) ''
        write ( *, '(2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) 
     &    n, c, a, b

        call line_grid ( n, a, b, c, x )
        call r8vec_print ( n, x, '  Grid points:' )

      end do

      return
      end

