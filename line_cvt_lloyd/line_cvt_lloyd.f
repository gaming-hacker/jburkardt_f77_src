      subroutine energy_plot ( it_num, e_plot, header )

c*********************************************************************72
c
cc ENERGY_PLOT plots the energy as a function of the iterations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer IT_NUM, the number of iterations to take.
c
c    Input, double precision E_PLOT(0:IT_NUM), the energy per iteration.
c
c    Input, character * ( * ) HEADER, an identifying string.
c
      implicit none

      integer it_num

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision e_plot(0:it_num)
      character * ( * ) header
      integer it
      character * ( 255 ) plot_filename
c
c  Write data file.
c
      call get_unit ( data_unit )
      data_filename = trim ( header ) // '_energy_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do it = 0, it_num
        if ( 0.0D+00 .lt. e_plot(it) ) then
          write ( data_unit, '(i6,2x,g14.6)' ) it, log ( e_plot(it) )
        end if
      end do
      close ( unit = data_unit )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Gnuplot data written to file "' // 
     &  trim ( data_filename ) // '".'
c
c  Write command file.
c
      call get_unit ( command_unit )
      command_filename = trim ( header ) // '_energy_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      plot_filename = trim ( header ) // '_energy.png'

      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "' // trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set xlabel "<---Iteration--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Log(Energy)--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Energy Decrease with Iteration"'
      write ( command_unit, '(a,i4,a)' ) 
     &  'plot "' // trim ( data_filename )     
     &  // '" using 1:2 with points pt 7 ps 1'
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Gnuplot commands written to "'
     &  // trim ( command_filename ) // '".'

      return
      end
      subroutine evolution_plot ( n, it_num, x_plot, header )

c*********************************************************************72
c
cc EVOLUTION_PLOT plots all points as a function of the iterations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, integer IT_NUM, the number of iterations to take.
c
c    Input, double precision X_PLOT(N,IT_NUM), the point locations over time.
c
c    Input, character * ( * ) HEADER, an identifying string.
c
      implicit none

      integer it_num
      integer n

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      character * ( * ) header
      integer i
      integer it
      character * ( 255 ) plot_filename
      double precision x_plot(n,0:it_num)
c
c  Write data file.
c
      call get_unit ( data_unit )
      data_filename = trim ( header ) // '_evolution_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )

      do it = 0, it_num
        write ( data_unit, '(i6)', advance = 'no' ) it
        do i = 1, n
          write ( data_unit, '(g14.6)', advance = 'no' ) x_plot(i,it)
        end do
        write ( data_unit, '(a)', advance = 'yes' )
      end do

      close ( unit = data_unit )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Gnuplot data written to file "' 
     &  // trim ( data_filename ) // '".'
c
c  Write command file.
c
      call get_unit ( command_unit )
      command_filename = trim ( header ) // '_evolution_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      plot_filename = trim ( header ) // '_evolution.png'

      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "' // trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Iteration--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Point Motion with Iteration"'
      write ( command_unit, '(a,i4,a)' ) '
     &  plot for [i=2:',     n + 1,   '] "' 
     &  // trim ( data_filename ) 
     &  // '" using i:1 with points pt 7 ps 1'

      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Gnuplot commands written to "'
     &  // trim ( command_filename ) // '".'

      return
      end
      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      subroutine line_ccvt_lloyd ( n, a, b, it_num, header, x )

c*********************************************************************72
c
cc LINE_CCVT_LLOYD carries out the constrained Lloyd algorithm.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of generators.
c
c    Input, double precision A, B, the left and right endpoints.
c
c    Input, integer IT_NUM, the number of iterations to take.
c
c    Input, character * ( * ) HEADER, an identifying string.
c
c    Input/output, double precision X(N), the point locations.
c
      implicit none

      integer it_num
      integer n

      double precision a
      double precision b
      double precision e
      double precision e_plot(0:it_num)
      character * ( * ) header
      integer it
      integer j
      double precision x(n)
      double precision x_old(n)
      double precision x_plot(n,0:it_num)
      double precision xm
      double precision xm_plot(1:it_num)

      call line_cvt_energy ( n, a, b, x, e )
      e_plot(0) = e
      x_plot(1:n,0) = x(1:n)

      do it = 1, it_num

        x_old(1:n) = x(1:n)

        call line_ccvt_lloyd_step ( n, a, b, x )

        x_plot(1:n,it) = x(1:n)

        call line_cvt_energy ( n, a, b, x, e )
        e_plot(it) = e

        xm = 0.0D+00
        do j = 1, n
          xm = xm + ( x_old(j) - x(j) ) ** 2
        end do
        xm = xm / dble ( n )

        xm_plot(it) = xm

      end do

      call energy_plot ( it_num, e_plot, header )
      call motion_plot ( it_num, xm_plot, header )
      call evolution_plot ( n, it_num, x_plot, header )

      return
      end
      subroutine line_ccvt_lloyd_step ( n, a, b, x )

c*********************************************************************72
c
cc LINE_CCVT_LLOYD_STEP takes one step of Lloyd's constrained CVT algorithm.
c
c  Discussion:
c
c    Each step of Lloyd's algorithm replaces a point by the center of mass
c    of the associated region.  For points on a line, with a uniform
c    density, the associated region is demarcated by the midways between
c    successive points.
c
c    Here, we include the additional constraint that we want the first and last
c    points to be fixed at the endpoints of the line, that is, X(1) = A
c    and X(2) = B.  In that case, the calculation of the updates for the
c    first two and last two points must be handled differently.
c
c    For points away from the boundary, a step of Lloyd's method can be
c    regarded as replacing each point by the average of the left and right
c    midways.  The midways, of course, are the average of two points.
c    So for point J, we have:
c
c      M(J-1,J) = ( X(J-1) + X(J) ) / 2
c      M(J,J+1) = ( X(J) + X(J+1) ) / 2
c      X*(J) = ( M(J-1,J) + M(J,J+1) ) / 2 = ( X(J-1) + 2 X(J) + X(J+1) ) / 4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    1 <= N.
c
c    Input, double precision A, B, the left and right endpoints.
c
c    Input/output, double precision X(N), the point locations.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer j
      double precision x(n)
      double precision x_old(n)

      x_old(1:n) = x(1:n)

      if ( n .eq. 1 ) then

        x(1) = ( a + b ) / 2.0D+00

      else if ( n .eq. 2 ) then

        x(1) = a
        x(2) = b

      else

        x(1) = a

        do j = 2, n - 1
          x(j) = ( 0.5D+00 * ( x_old(j-1) + x_old(j) )
     &           + 0.5D+00 * ( x_old(j) + x_old(j+1) ) ) / 2.0D+00
        end do

        x(n) = b

      end if

      return
      end
      subroutine line_cvt_energy ( n, a, b, x, e )

c*********************************************************************72
c
cc LINE_CVT_ENERGY computes the CVT energy for a given set of generators.
c
c  Discussion:
c
c    Given a set of generators G over the line [A,B], then the energy
c    is defined as
c      E = integral ( a <= x <= b ) ( x - g(x) )^2 dx
c    where g(x) is the nearest generator to the point x.
c
c    For the 1D case, this integral can be evaluated exactly as the
c    sum of integrals over each subinterval:
c
c      E(i) = integral ( xl <= x <= xr ) ( x - x(i) )^2 dx
c           = ( ( x(i) - xl )^3 + ( xr - x(i) )^3 ) / 3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of generators.
c
c    Input, double precision A, B, the left and right endpoints.
c
c    Input, double precision X(N), the generator locations.
c
c    Output, double precision E, the energy of the generator distribution.
c
      implicit none

      integer n

      double precision a
      double precision b
      double precision e
      integer j
      double precision x(n)
      double precision xl
      double precision xr

      e = 0.0D+00

      do j = 1, n

        if ( j .eq. 1 ) then
          xl = a
        else
          xl = ( x(j-1) + x(j) ) / 2.0D+00
        end if

        if ( j .eq. n ) then
          xr = b
        else
          xr = ( x(j) + x(j+1) ) / 2.0D+00
        end if

        e = e + ( ( x(j) - xl ) ** 3 + ( xr - x(j) ) ** 3  ) / 3.0D+00

      end do

      return
      end
      subroutine line_cvt_lloyd ( n, a, b, it_num, header, x )

c*********************************************************************72
c
cc LINE_CVT_LLOYD carries out the Lloyd algorithm.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of generators.
c
c    Input, double precision A, B, the left and right endpoints.
c
c    Input, integer IT_NUM, the number of iterations to take.
c
c    Input, character * ( * ) HEADER, an identifying string.
c
c    Input/output, double precision X(N), the point locations.
c
      implicit none

      integer it_num
      integer n

      double precision a
      double precision b
      double precision e
      double precision e_plot(0:it_num)
      character * ( * ) header
      integer it
      integer j
      double precision x(n)
      double precision x_old(n)
      double precision x_plot(n,0:it_num)
      double precision xm
      double precision xm_plot(1:it_num)

      call line_cvt_energy ( n, a, b, x, e )

      e_plot(0) = e
      x_plot(1:n,0) = x(1:n)

      do it = 1, it_num

        x_old(1:n) = x(1:n)

        call line_cvt_lloyd_step ( n, a, b, x )
 
        x_plot(1:n,it) = x(1:n)

        call line_cvt_energy ( n, a, b, x, e )
        e_plot(it) = e

        xm = 0.0D+00
        do j = 1, n
          xm = xm + ( x_old(j) - x(j) ) ** 2
        end do
        xm = xm / dble ( n )

        xm_plot(it) = xm

      end do

      call energy_plot ( it_num, e_plot, header )
      call motion_plot ( it_num, xm_plot, header )
      call evolution_plot ( n, it_num, x_plot, header )
 
      return
      end
      subroutine line_cvt_lloyd_step ( n, a, b, x )

c*********************************************************************72
c
cc LINE_CVT_LLOYD_STEP takes one step of Lloyd's unconstrained CVT algorithm.
c
c  Discussion:
c
c    Each step of Lloyd's algorithm replaces a point by the center of mass
c    of the associated region.  For points on a line, with a uniform
c    density, the associated region is demarcated by the midways between
c    successive points.
c
c    For points away from the boundary, a step of Lloyd's method can be
c    regarded as replacing each point by the average of the left and right
c    midways.  The midways, of course, are the average of two points.
c    So for point J, we have:
c
c      M(J-1,J) = ( X(J-1) + X(J) ) / 2
c      M(J,J+1) = ( X(J) + X(J+1) ) / 2
c      X*(J) = ( M(J-1,J) + M(J,J+1) ) / 2 = ( X(J-1) + 2 X(J) + X(J+1) ) / 4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c    1 <= N.
c
c    Input, double precision A, B, the left and right endpoints.
c
c    Input, double precision X(N), the point locations.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer j
      double precision x(n)
      double precision x_old(n)

      x_old(1:n) = x(1:n)

      if ( n .eq. 1 ) then

        x(1) = ( a + b ) / 2.0D+00

      else

        j = 1
        x(j) = (               a                                  
     &           + 0.5D+00 * ( x_old(j) + x_old(j+1) ) ) / 2.0D+00

        do j = 2, n - 1
          x(j) = ( 0.5D+00 * ( x_old(j-1) + x_old(j) )
     &           + 0.5D+00 * ( x_old(j) + x_old(j+1) ) ) / 2.0D+00
        end do

        j = n
        x(j) =   ( 0.5D+00 * ( x_old(j-1) + x_old(j) )
     &           +                                 b )   / 2.0D+00

      end if

      return
      end
      subroutine motion_plot ( it_num, xm_plot, header )

c*********************************************************************72
c
cc MOTION_PLOT plots the motion as a function of the iterations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer IT_NUM, the number of iterations to take.
c
c    Input, double precision XM_PLOT(IT_NUM), the average motion per iteration.
c
c    Input, character * ( * ) HEADER, an identifying string.
c
      implicit none

      integer it_num

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      character * ( * ) header
      integer it
      character * ( 255 ) plot_filename
      double precision xm_plot(0:it_num)
c
c  Write data file.
c
      call get_unit ( data_unit )
      data_filename = trim ( header ) // '_motion_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do it = 1, it_num
        if ( 0.0D+00 .lt. xm_plot(it) ) then
          write ( data_unit, '(i6,2x,g14.6)' ) it, log ( xm_plot(it) )
        end if
      end do
      close ( unit = data_unit )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Gnuplot data written to file "' 
     &  // trim ( data_filename ) // '".'
c
c  Write command file.
c
      call get_unit ( command_unit )
      command_filename = trim ( header ) // '_motion_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      plot_filename = trim ( header ) // '_motion.png'

      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "' 
     &  // trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set xlabel "<---Iteration--->"'
      write ( command_unit, '(a)' ) 
     &  'set ylabel "<---Average Motion--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Generator Motion with Iteration"'
      write ( command_unit, '(a,i4,a)' ) 
     &  'plot "' // trim ( data_filename )
     &  // '" using 1:2 with points pt 7 ps 1'
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Gnuplot commands written to "'
     &  // trim ( command_filename ) // '".'

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do

      return
      end
      subroutine r8vec_sort_insert_a ( n, a )

c*********************************************************************72
c
cc R8VEC_SORT_INSERT_A ascending sorts an R8VEC using an insertion sort.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 August 2010
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Kreher, Douglas Simpson,
c    Algorithm 1.1,
c    Combinatorial Algorithms,
c    CRC Press, 1998, page 11.
c
c  Parameters:
c
c    Input, integer N, the number of items in the vector.
c    N must be positive.
c
c    Input/output, double precision A(N).
c    On input, the array to be sorted;
c    On output, the array has been sorted.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      integer j
      double precision x

      do i = 2, n

        x = a(i)

        j = i - 1

10      continue

        if ( 1 .le. j ) then

          if ( a(j) .le. x ) then
            go to 20
          end if

          a(j+1) = a(j)
          j = j - 1

          go to 10

        end if

20      continue

        a(j+1) = x

      end do

      return
      end
      subroutine r8vec_uniform_ab ( n, a, b, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_AB returns a scaled pseudorandom R8VEC.
c
c  Discussion:
c
c    Each dimension ranges from A to B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 January 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Second Edition,
c    Springer, 1987,
c    ISBN: 0387964673,
c    LC: QA76.9.C65.B73.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, December 1986, pages 362-376.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998,
c    ISBN: 0471134031,
c    LC: T57.62.H37.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, Number 2, 1969, pages 136-143.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A, B, the lower and upper limits.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      integer seed
      double precision r(n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge
        end if

        r(i) = a + ( b - a ) * dble ( seed ) * 4.656612875D-10

      end do

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ',
     &  'May      ', 'June     ', 'July     ', 'August   ',
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *,
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' )
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
