      program main

c*********************************************************************72
c
cc MAIN is the main program for SPIRAL_PRB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPIRAL_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SPIRAL library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPIRAL_PRB'
      write ( *, '(a)' ) '  Normal end of execution'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 evaluates the solution on a grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nx
      parameter ( nx = 11 )
      integer ny
      parameter ( ny = 11 )

      integer mn
      parameter ( mn = nx * ny )

      integer i
      integer j
      integer k
      double precision nu
      parameter ( nu = 1.0D+00 )
      double precision p(mn)
      integer p_commands
      integer p_data
      double precision t
      parameter ( t = 3.0D+00 )
      double precision u(mn)
      integer uv_commands
      double precision uv_norm
      integer uv_data
      double precision v(mn)
      double precision x(mn)
      double precision y(mn)
c
c  Set the location of the grid points.
c
      k = 0

      do j = 1, ny
        do i = 1, nx
          k = k + 1
          x(k) = dble ( i - 1 ) / dble ( nx - 1 )
          y(k) = dble ( j - 1 ) / dble ( ny - 1 )
        end do
      end do

      call uvp ( mn, nu, x, y, t, u, v, p )
c
c  Write P commands to a file.
c
      call get_unit ( p_commands )
      open ( unit = p_commands, file = 'p_commands.txt', 
     &  status = 'replace' )

      write ( p_commands, '(a)' ) '# p_commands.txt'
      write ( p_commands, '(a)' ) '# usage: gnuplot < p_commands.txt'
      write ( p_commands, '(a)' ) '#'
      write ( p_commands, '(a)' ) 'set term png'
      write ( p_commands, '(a)' ) 'set size ratio -1'
      write ( p_commands, '(a)' ) 'set output "p.png"'
      write ( p_commands, '(a)' ) 'unset surface'
      write ( p_commands, '(a)' ) 'set view map'
      write ( p_commands, '(a)' ) 'set contour'
      write ( p_commands, '(a)' ) 'set cntrparam levels 10'
      write ( p_commands, '(a)' ) 'set xlabel "<--X-->"'
      write ( p_commands, '(a)' ) 'set ylabel "<--Y-->"'
      write ( p_commands, '(a)' ) 'set title "Pressure contours"'
      write ( p_commands, '(a)' ) 'set timestamp'
      write ( p_commands, '(a)' ) 'splot "p_data.txt" with linesp'

      close ( unit = p_commands )
c
c  Write P data to a file.
c
      call get_unit ( p_data )
      open ( unit = p_data, file = 'p_data.txt', status = 'replace' )
      k = 0
      do j = 1, ny
        do i = 1, nx
          k = k + 1
          write ( p_data, '(3f10.4)' ) x(k), y(k), p(k)
        end do
        if ( j .lt. ny ) then
          write ( p_data, '(a)' ) ''
        end if
      end do
      close ( unit = p_data )
c
c  Write UV commands to a file.
c
      call get_unit ( uv_commands )
      open ( unit = uv_commands, file = 'uv_commands.txt', 
     &  status = 'replace' )

      write ( uv_commands, '(a)' ) '# uv_commands.txt'
      write ( uv_commands, '(a)' ) '# usage: gnuplot < uv_commands.txt'
      write ( uv_commands, '(a)' ) '#'
      write ( uv_commands, '(a)' ) 'set term png'
      write ( uv_commands, '(a)' ) 'set size ratio -1'
      write ( uv_commands, '(a)' ) 'set output "uv.png"'
      write ( uv_commands, '(a)' ) 'set grid'
      write ( uv_commands, '(a)' ) 'set xlabel "<--X-->"'
      write ( uv_commands, '(a)' ) 'set ylabel "<--Y-->"'
      write ( uv_commands, '(a)' ) 'set title "Velocity field"'
      write ( uv_commands, '(a)' ) 'set timestamp'
      write ( uv_commands, '(a)' ) 'plot "uv_data.txt" with vectors'

      close ( unit = uv_commands )
c
c  Get scale factor for UV data.
c
      uv_norm = 0.0D+00
      do i = 1, mn
        uv_norm = max ( uv_norm, sqrt ( u(i)**2 + v(i)**2 ) )
      end do

      uv_norm = uv_norm * sqrt ( dble ( nx * ny ) )

      if ( 0.0D+00 .lt. uv_norm ) then
        do i = 1, mn
          u(i) = u(i) / uv_norm
          v(i) = v(i) / uv_norm
        end do
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 
     &  '  Dividing by velocity scale factor ', uv_norm
c
c  Write UV data to a file.
c
      call get_unit ( uv_data )
      open ( unit = uv_data, file = 'uv_data.txt', status = 'replace' )
      k = 0
      do j = 1, ny
        do i = 1, nx
          k = k + 1
          write ( uv_data, '(4f10.4)' ) x(k), y(k), u(k), v(k)
        end do
      end do
      close ( unit = uv_data )

      return
      end

