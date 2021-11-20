      program main

c*********************************************************************72
c
cc MAIN is the main program for CUBE_GRID_PRB.
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
      write ( *, '(a)' ) 'CUBE_GRID_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CUBE_GRID library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CUBE_GRID_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests CUBE_GRID using the same parameters for all dimensions.
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

      double precision a(3)
      double precision b(3)
      integer c(3)
      integer i
      integer n
      integer ns(3)
      double precision x(3,27)

      save a
      save b
      save c
      save ns

      data a / -1.0D+00, -1.0D+00, -1.0D+00 /
      data b / +1.0D+00, +1.0D+00, +1.0D+00 /
      data c / 1, 1, 1 /
      data ns / 3, 3, 3 /

      n = ns(1) * ns(2) * ns(3)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Create a grid using CUBE_GRID.'
      write ( *, '(a)' ) '  Use the same parameters in every dimension.'
      write ( *, '(a,i4)' ) '  Number of grid points N = ', n
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I    NS     C      A         B'
      write ( *, '(a)' ) ''
      do i = 1, 3
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) 
     &    i, ns(i), c(i), a(i), b(i)
      end do

      call cube_grid ( n, ns, a, b, c, x )
      call r8mat_transpose_print ( 3, n, x, '  Grid points:' )

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

      double precision a(3)
      double precision b(3)
      integer c(3)
      integer i
      integer n
      integer ns(3)
      double precision x(3,24)

      save a
      save b
      save c
      save ns

      data a /  0.0D+00,  0.0D+00,  0.0D+00 /
      data b / +1.0D+00, +1.0D+00, +1.0D+00 /
      data c / 2, 2, 2/
      data ns / 4, 2, 3 /

      n = ns(1) * ns(2) * ns(3)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Create a grid using CUBE_GRID.'
      write ( *, '(a)' ) 
     &  '  Use a different number of points in each dimension.'
      write ( *, '(a,i4)' ) '  Number of grid points N = ', n
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I    NS     C      A         B'
      write ( *, '(a)' ) ''
      do i = 1, 3
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) 
     &    i, ns(i), c(i), a(i), b(i)
      end do

      call cube_grid ( n, ns, a, b, c, x )
      call r8mat_transpose_print ( 3, n, x, '  Grid points:' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 uses a cube with different sizes in each dimension.
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

      double precision a(3)
      double precision b(3)
      integer c(3)
      integer i
      integer n
      integer ns(3)
      double precision x(3,27)

      save a
      save b
      save c
      save ns

      data a /  0.0D+00,  -2.0D+00,  50.0D+00 /
      data b / 10.0D+00,  +2.0D+00,  51.0D+00 /
      data c / 3, 4, 5 /
      data ns / 3, 3, 3 /

      n = ns(1) * ns(2) * ns(3)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  Use a different physical size in each dimension.'
      write ( *, '(a,i4)' ) '  Number of grid points N = ', n
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I    NS     C      A         B'
      write ( *, '(a)' ) ''
      do i = 1, 3
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) 
     &    i, ns(i), c(i), a(i), b(i)
      end do

      call cube_grid ( n, ns, a, b, c, x )
      call r8mat_transpose_print ( 3, n, x, '  Grid points:' )

      return
      end

