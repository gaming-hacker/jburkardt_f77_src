      program main

c*********************************************************************72
c
cc MAIN is the main program for HYPERCUBE_GRID_PRB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HYPERCUBE_GRID_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the HYPERCUBE_GRID library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HYPERCUBE_GRID_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests HYPERCUBE_GRID on a two dimensional example.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer n
      parameter ( n = 20 )

      double precision a(m)
      double precision b(m)
      integer c(m)
      integer i
      integer i4vec_product
      integer ns(m)
      double precision x(m,n)

      save a
      save b
      save c
      save ns

      data a / 0.0D+00, 0.0D+00 /
      data b / 1.0D+00, 10.0D+00 /
      data c / 2, 4 /
      data ns / 4, 5 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Create a grid using HYPERCUBE_GRID.'
      write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
      write ( *, '(a,i4)' ) '  Number of grid points N = ', n
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I    NS     C      A         B'
      write ( *, '(a)' ) ''
      do i = 1, m
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) 
     &    i, ns(i), c(i), a(i), b(i)
      end do

      call hypercube_grid ( m, n, ns, a, b, c, x )
      call r8mat_transpose_print ( m, n, x, '  Grid points:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests HYPERCUBE_GRID on a five dimensional example.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 32 )

      double precision a(m)
      double precision b(m)
      integer c(m)
      integer i
      integer i4vec_product
      integer ns(m)
      double precision x(m,n)

      save a
      save b
      save c
      save ns

      data a / 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00 /
      data b / 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00, 1.0D+00 /
      data c / 1, 2, 3, 4, 5 /
      data ns / 2, 2, 2, 2, 2 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Create a grid using HYPERCUBE_GRID.'
      write ( *, '(a)' ) '  Use a two point grid in each dimension.'
      write ( *, '(a)' ) 
     &  '  Use a different centering option in each dimension.'
      write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
      write ( *, '(a,i4)' ) '  Number of grid points N = ', n
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I    NS     C      A         B'
      write ( *, '(a)' ) ''
      do i = 1, m
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) 
     &    i, ns(i), c(i), a(i), b(i)
      end do

      call hypercube_grid ( m, n, ns, a, b, c, x )
      call r8mat_transpose_print ( m, n, x, '  Grid points:' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests HYPERCUBE_GRID on a three dimensional example.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 27 )

      double precision a(m)
      double precision b(m)
      integer c(m)
      integer i
      integer i4vec_product
      integer ns(m)
      double precision x(m,n)

      save a
      save b
      save c
      save ns

      data a / -1.0D+00, -1.0D+00, -1.0D+00 /
      data b / 1.0D+00, 1.0D+00, 1.0D+00 /
      data c / 1, 1, 1 /
      data ns / 3, 3, 3 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Create a grid using HYPERCUBE_GRID.'
      write ( *, '(a)' ) '  Use the same parameters in every dimension.'
      write ( *, '(a,i4)' ) '  Spatial dimension M = ', m
      write ( *, '(a,i4)' ) '  Number of grid points N = ', n
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '     I    NS     C      A         B'
      write ( *, '(a)' ) ''
      do i = 1, m
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,f8.4,2x,f8.4)' ) 
     &    i, ns(i), c(i), a(i), b(i)
      end do

      call hypercube_grid ( m, n, ns, a, b, c, x )
      call r8mat_transpose_print ( m, n, x, '  Grid points:' )

      return
      end
