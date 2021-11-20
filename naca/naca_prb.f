      program main

c*********************************************************************72
c
cc MAIN is the main program for NACA_PRB.
c
c  Discussion:
c
c    NACA_PRB tests the NACA library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'NACA_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the NACA library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'NACA_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests NACA4_SYMMETRIC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 May 2014
c
c  Author:
c  
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 51 )

      double precision c
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer i
      double precision r8vec_max
      double precision r8vec_min
      double precision ratio
      double precision t
      double precision x(n)
      double precision x_max
      double precision x_min
      double precision xy(2,2*n)
      double precision y(n)
      double precision y_max
      double precision y_min

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  NACA4_SYMMETRIC evaluates y(x) for a NACA'
      write ( *, '(a)' ) 
     &  '  symmetric airfoil defined by a 4-digit code.'

      c = 10.0D+00
      t = 0.15D+00
      call r8vec_linspace ( n, 0.0D+00, c, x )
      call naca4_symmetric ( t, c, n, x, y )
c
c  Reorganize data into a single object.
c
      do i = 1, n
        xy(1,i) = x(i)
        xy(2,i) = -y(i)
      end do

      do i = 1, n
        xy(1,n+i) = x(n+1-i)
        xy(2,n+i) = y(n+1-i)
      end do
c
c  Determine size ratio.
c
      x_min = r8vec_min ( n, x )
      x_max = r8vec_max ( n, x )
      y_max = r8vec_max ( n, y )
      y_min = - y_max
      ratio = ( y_max - y_min ) / ( x_max - x_min )
c
c  Save data to a file.
c
      data_filename = 'symmetric_data.txt'
      call r8mat_write ( data_filename, 2, 2 * n, xy )
      write ( *, '(a)' ) '  Data saved in file "' 
     &  // trim ( data_filename ) // '"'
c
c  Create the command file.
c
      command_filename = 'symmetric_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a,g14.6)' ) 'set size ratio ', ratio
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set output "symmetric.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "NACA Symmetric Airfoil"'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) 
     &  // '" using 1:2 with lines lw 3'
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )
    
      write ( *, '(a)' ) '  Created command file "' 
     &  // trim ( command_filename ) // '".'

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests NACA4_CAMBERED.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real M, the maximum camber.
c    0 < M.
c
c    Input, real P, the location of maximum camber.
c    0.0 < P < 1.0.
c
c    Input, real T, the maximum relative thickness.
c    0.0 < T <= 1.0.
c
      implicit none

      integer n
      parameter ( n = 51 )

      double precision c
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer i
      double precision m
      double precision p
      double precision r8vec_max
      double precision r8vec_min
      double precision ratio
      double precision t
      double precision x_max
      double precision x_min
      double precision xc(n)
      double precision xl(n)
      double precision xu(n)
      double precision xy(2,2*n)
      double precision y_max
      double precision y_min
      double precision yl(n)
      double precision yu(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  NACA4_CAMBERED evaluates (xu,yu) and (xl,yl) for a NACA'
      write ( *, '(a)' ) 
     &  '  cambered airfoil defined by a 4-digit code.'

      m = 0.02D+00
      p = 0.4D+00
      t = 0.12D+00
      c = 10.0D+00

      call r8vec_linspace ( n, 0.0D+00, c, xc )
      call naca4_cambered ( m, p, t, c, n, xc, xu, yu, xl, yl )
c
c  Reorganize data into a single object.
c
      do i = 1, n
        xy(1,i) = xl(i)
        xy(2,i) = yl(i)
      end do

      do i = 1, n 
        xy(1,n+i) = xu(n+1-i)
        xy(2,n+i) = yu(n+1-i)
      end do
c
c  Determine size ratio.
c
      x_min = min ( r8vec_min ( n, xl ), r8vec_min ( n, xu ) )
      x_max = max ( r8vec_max ( n, xl ), r8vec_max ( n, xu ) )
      y_min = min ( r8vec_min ( n, yl ), r8vec_min ( n, yu ) )
      y_max = max ( r8vec_max ( n, yl ), r8vec_max ( n, yu ) )
      ratio = ( y_max - y_min ) / ( x_max - x_min )
c
c  Save data to a file.
c
      data_filename = 'cambered_data.txt'
      call r8mat_write ( data_filename, 2, 2 * n, xy )
      write ( *, '(a)' ) '  Data saved in file "' 
     &  // trim ( data_filename ) // '"'
c
c  Create the command file.
c
      command_filename = 'cambered_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a,g14.6)' ) 'set size ratio ', ratio
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set output "cambered.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
      write ( command_unit, '(a)' ) 'set title "NACA Cambered Airfoil"'
      write ( command_unit, '(a)' ) 'plot "' 
     &  // trim ( data_filename ) 
     &  // '" using 1:2 with lines lw 3'
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Created command file "' 
     &  // trim ( command_filename ) // '".'

      return
      end

