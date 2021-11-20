      program main

c*********************************************************************72
c
cc MAIN is the main program for QUAD2D_OPENMP.
c
      include 'omp_lib.h'

      double precision a
      double precision b
      double precision error
      double precision exact
      external f
      double precision f
      integer i
      integer j
      integer n
      integer nx
      integer ny
      double precision pi
      double precision total
      double precision wtime
      double precision x
      double precision y

      a = 0.0
      b = 1.0
      nx = 32768
      ny = 32768
      n = nx * ny
      pi = 3.141592653589793D+00
      exact = pi * pi / 6.0D+00

      write ( *, * ) ' '
      write ( *, * ) 'QUAD2D_OPENMP:'
      write ( *, * ) '  FORTRAN77/OpenMP version'
      write ( *, * ) 
     &  '  Estimate the integral of f(x,y) over [0,1]x[0,1].'
      write ( *, * ) '  f(x,y) = 1 / ( 1 - x * y ).'
      write ( *, * ) ' '
      write ( *, * ) '  A        = ', a
      write ( *, * ) '  B        = ', b
      write ( *, * ) '  NX       = ', nx
      write ( *, * ) '  NY       = ', ny
      write ( *, * ) '  N        = ', n
      write ( *, * ) '  Exact    = ', exact

      wtime = omp_get_wtime ( )

      total = 0.0D+00
c$omp parallel shared ( a, b, n ) private ( i, j, x, y )  

c$omp do reduction ( + : total )
      do i = 1, nx
        x = ( ( 2 * nx - 2 * i + 1 ) * a + ( 2 * i - 1 ) * b ) 
     &    / ( 2 * nx )
        do j = 1, ny
          y = ( ( 2 * ny - 2 * j + 1 ) * a + ( 2 * j - 1 ) * b ) 
     &      / ( 2 * ny )
          total = total + f ( x, y )
        end do
      end do
c$omp end do

c$omp end parallel
      wtime = omp_get_wtime ( ) - wtime

      total = ( b - a ) * ( b - a ) * total / dble ( nx ) / dble ( ny )
      error = abs ( total - exact )
     
      write ( *, * ) ' '
      write ( *, * ) '  Estimate = ', total
      write ( *, * ) '  Error    = ', error
      write ( *, * ) '  Time     = ', wtime
c
c  Terminate.
c
      write ( *, * ) ' '
      write ( *, * ) 'QUAD2D_OPENMP:'
      write ( *, * ) '  Normal end of execution.'

      stop
      end
      function f ( x, y )

c*********************************************************************72
c
cc F evaluates the function.
c
      double precision f
      double precision x
      double precision y

      f = 1.0D+00 / ( 1.0D+00 - x * y )

      return
      end
