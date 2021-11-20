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
      subroutine grid_2d ( x_num, x_lo, x_hi, y_num, y_lo, y_hi, x, y )

c*********************************************************************72
c
cc GRID_2D returns a regular 2D grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X_NUM, the number of X values to use.
c
c    Input, double precision X_LO, X_HI, the range of X values.
c
c    Input, integer Y_NUM, the number of Y values to use.
c
c    Input, double precision Y_LO, Y_HI, the range of Y values.
c
c    Output, double precision X(X_NUM,Y_NUM), Y(X_NUM,Y_NUM),
c    the coordinates of the grid.
c
      implicit none

      integer x_num
      integer y_num

      integer i
      integer j
      double precision x(x_num,y_num)
      double precision x_hi
      double precision x_lo
      double precision xi
      double precision y(x_num,y_num)
      double precision y_hi
      double precision y_lo
      double precision yj

      if ( x_num .eq. 1 ) then
        do j = 1, y_num
          do i = 1, x_num
            x(i,j) = ( x_lo + x_hi ) / 2.0D+00
          end do
        end do
      else
        do i = 1, x_num
          xi = ( dble ( x_num - i     ) * x_lo   
     &         + dble (         i - 1 ) * x_hi ) 
     &         / dble ( x_num     - 1 )
          do j = 1, y_num
            x(i,j) = xi
          end do
        end do
      end if

      if ( y_num .eq. 1 ) then
        do j = 1, y_num
          do i = 1, x_num
            y(i,j) = ( y_lo + y_hi ) / 2.0D+00
          end do
        end do
      else
        do j = 1, y_num
          yj = ( dble ( y_num - j     ) * y_lo   
     &         + dble (         j - 1 ) * y_hi ) 
     &         / dble ( y_num     - 1 )
          do i = 1, x_num
            y(i,j) = yj
          end do
        end do
      end if

      return
      end
      subroutine ns2de_gnuplot ( header, n, x, y, u, v, s )

c*********************************************************************72
c
cc NS2DE_GNUPLOT writes the Navier-Stokes velocity field to files for GNUPLOT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) HEADER, a header to be used to name the files.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision X(N), Y(N), the coordinates of the 
c    evaluation points.
c
c    Input, double precision U(N), V(N), the velocity components.
c
c    Input, double precision S, a scale factor for the velocity vectors.
c
      implicit none

      integer n

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      character * ( * ) header
      integer i
      character * ( 255 ) plot_filename
      double precision s
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)
c
c  Write the data file.
c
      data_filename = trim ( header ) // '_data.txt'

      call get_unit ( data_unit )

      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )

      do i = 1, n
        write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    x(i), y(i), s * u(i), s * v(i)
      end do

      close ( unit = data_unit )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Data written to "' // trim ( data_filename ) // '".'
c
c  Write the command file.
c
      command_filename = trim ( header ) // '_commands.txt'
      call get_unit ( command_unit )

      plot_filename = trim ( header ) // '.png'

      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '#  ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "' // 
     &  trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '#  Add titles and labels.'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Navier-Stokes velocity field"'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '#  Add grid lines.'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set size ratio -1'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '#  Timestamp the plot.'
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'plot "' // 
     &  trim ( data_filename ) // 
     &  '" using 1:2:3:4 with vectors \'
      write ( command_unit, '(a)' ) 
     &  '  head filled lt 2 linecolor rgb "blue"'
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Commands written to "' // 
     &  trim ( command_filename ) // '".'

      return
      end
      function r8vec_amax ( n, a )

c*********************************************************************72
c
cc R8VEC_AMAX returns the maximum absolute value in an R8VEC.
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
c    02 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision R8VEC_AMAX, the value of the entry
c    of largest magnitude.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_amax
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = max ( value, abs ( a(i) ) )
      end do

      r8vec_amax = value

      return
      end
      function r8vec_amin ( n, a )

c*********************************************************************72
c
cc R8VEC_AMIN returns the minimum absolute value in an R8VEC.
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
c    02 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precisionA(N), the array.
c
c    Output, double precision R8VEC_AMIN, the value of the entry
c    of smallest magnitude.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8_huge
      parameter ( r8_huge = 1.79769313486231571D+308 )
      double precision r8vec_amin
      double precision value

      value = r8_huge
      do i = 1, n
        value = min ( value, abs ( a(i) ) )
      end do

      r8vec_amin = value

      return
      end
      subroutine r8vec_linspace ( n, a, b, x )

c*********************************************************************72
c
cc R8VEC_LINSPACE creates a vector of linearly spaced values.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
c
c    In other words, the interval is divided into N-1 even subintervals,
c    and the endpoints of intervals are used as the points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A, B, the first and last entries.
c
c    Output, double precision X(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision x(n)

      if ( n .eq. 1 ) then

        x(1) = ( a + b ) / 2.0D+00

      else

        do i = 1, n
          x(i) = ( dble ( n - i     ) * a
     &           + dble (     i - 1 ) * b )
     &           / dble ( n     - 1 )
        end do

      end if

      return
      end
      function r8vec_max ( n, a )

c*********************************************************************72
c
cc R8VEC_MAX returns the maximum value in an R8VEC.
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
c    12 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision R8VEC_MAX, the value of the largest entry.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_max
      double precision value

      value = a(1)
      do i = 2, n
        value = max ( value, a(i) )
      end do

      r8vec_max = value

      return
      end
      function r8vec_min ( n, a )

c*********************************************************************72
c
cc R8VEC_MIN returns the minimum value in an R8VEC.
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
c    31 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision R8VEC_MIN, the value of the smallest entry.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_min
      double precision value

      value = a(1)
      do i = 2, n
        value = min ( value, a(i) )
      end do

      r8vec_min = value

      return
      end
      function r8vec_norm_l2 ( n, a )

c*********************************************************************72
c
cc R8VEC_NORM_L2 returns the L2 norm of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    The vector L2 norm is defined as:
c
c      R8VEC_NORM_L2 = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in A.
c
c    Input, double precision A(N), the vector whose L2 norm is desired.
c
c    Output, double precision R8VEC_NORM_L2, the L2 norm of A.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_norm_l2
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + a(i) * a(i)
      end do
      value = sqrt ( value )

      r8vec_norm_l2 = value

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
      subroutine resid_lucas ( nu, rho, n, x, y, t, ur, vr, pr )

c*****************************************************************************80
c
cc RESID_LUCAS returns Lucas Bystricky residuals.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision RHO, the density.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision X(N), Y(N), the coordinates of the points.
c
c    Input, double precision T, the time coordinate or coordinates.
c
c    Output, double precision UR(N), VR(N), PR(N), the residuals in the U,
c    V and P equations.
c
      implicit none

      integer n

      double precision dpdx(n)
      double precision dpdy(n)
      double precision dudt(n)
      double precision dudx(n)
      double precision dudxx(n)
      double precision dudy(n)
      double precision dudyy(n)
      double precision dvdt(n)
      double precision dvdx(n)
      double precision dvdxx(n)
      double precision dvdy(n)
      double precision dvdyy(n)
      double precision f(n)
      double precision g(n)
      double precision h(n)
      integer i
      double precision nu
      double precision p(n)
      double precision pr(n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision rho
      double precision t
      double precision u(n)
      double precision ur(n)
      double precision v(n)
      double precision vr(n)
      double precision x(n)
      double precision y(n)
c
c  Get the right hand side functions.
c
      call rhs_lucas ( nu, rho, n, x, y, t, f, g, h )
c
c  Form the functions and derivatives for the left hand side.
c
      do i = 1, n

        u(i) = - cos ( r8_pi * x(i) ) / r8_pi
        dudt(i) = 0.0D+00
        dudx(i) = sin ( r8_pi * x(i) )
        dudxx(i) = r8_pi * cos ( r8_pi * x(i) )
        dudy(i) = 0.0D+00
        dudyy(i) = 0.0D+00

        v(i) = - y(i) * sin ( r8_pi * x(i) )
        dvdt(i) = 0.0D+00
        dvdx(i) = - r8_pi * y(i) * cos ( r8_pi * x(i) )
        dvdxx(i) = + r8_pi ** 2 * y(i) * sin ( r8_pi * x(i) )
        dvdy(i) = - sin ( r8_pi * x(i) )
        dvdyy(i) = 0.0D+00

        p(i) = 0.0D+00
        dpdx(i) = 0.0D+00
        dpdy(i) = 0.0D+00
c
c  Evaluate the residuals.
c
        ur(i) = dudt(i) 
     &    + u(i) * dudx(i) + v(i) * dudy(i)     
     &    + ( 1.0D+00 / rho ) * dpdx(i) 
     &    - nu * ( dudxx(i) + dudyy(i) )
     &    - f(i)

        vr(i) = dvdt(i) 
     &    + u(i) * dvdx(i) + v(i) * dvdy(i)     
     &    + ( 1.0D+00 / rho ) * dpdy(i) 
     &    - nu * ( dvdxx(i) + dvdyy(i) )
     &    - g(i)

        pr(i) = dudx(i) + dvdy(i) - h(i)

      end do

      return
      end
      subroutine resid_spiral ( nu, rho, n, x, y, t, ur, vr, pr )

c*********************************************************************72
c
cc RESID_SPIRAL evaluates the pointwise residual of the spiral flow problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Maxim Olshanskii, Leo Rebholz,
c    Application of barycenter refined meshes in linear elasticity
c    and incompressible fluid dynamics,
c    ETNA: Electronic Transactions in Numerical Analysis, 
c    Volume 38, pages 258-274, 2011.
c
c  Parameters:
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision RHO, the density.
c
c    Input, integer N, the number of nodes.
c
c    Input, double precision X(N), Y(N), the coordinates of nodes.
c
c    Input, double precision T, the current time.
c
c    Input, double precision U(N), V(N), the X and Y velocity.
c
c    Input, double precision P(N), the pressure.
c
c    Output, double precision UR(N), the U-equation residual.
c
c    Output, double precision VR(N), the V-equation residual.
c
c    Output, double precision PR(N), the P-equation residual.
c
      implicit none

      integer ( kind = 4 ) n

      double precision dpdx(n)
      double precision dpdy(n)
      double precision dudt(n)
      double precision dudx(n)
      double precision dudxx(n)
      double precision dudy(n)
      double precision dudyy(n)
      double precision dvdt(n)
      double precision dvdx(n)
      double precision dvdxx(n)
      double precision dvdy(n)
      double precision dvdyy(n)
      double precision f(n)
      double precision g(n)
      double precision h(n)
      integer i
      double precision nu
      double precision p(n)
      double precision pr(n)
      double precision rho
      double precision t
      double precision u(n)
      double precision ur(n)
      double precision v(n)
      double precision vr(n)
      double precision x(n)
      double precision y(n)
c
c  Get the right hand side functions.
c
      call rhs_spiral ( nu, rho, n, x, y, t, f, g, h )
c
c  Form the functions and derivatives for the left hand side.
c
      do i = 1, n

        u(i) = ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( x(i) ** 4 - 2.0D+00 * x(i) ** 3 + x(i) ** 2 ) 
     &    * ( 2.0D+00 * y(i) ** 3 - 3.0D+00 * y(i) ** 2 + y(i) )

        dudt(i) = nu * 2.0D+00 
     &    * ( x(i) ** 4 - 2.0D+00 * x(i) ** 3 + x(i) ** 2 ) 
     &    * ( 2.0D+00 * y(i) ** 3  - 3.0D+00 * y(i) ** 2 + y(i) )

        dudx(i) = ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 4.0D+00 * x(i) ** 3 - 6.0D+00 * x(i) ** 2 
     &      + 2.0D+00 * x(i) )
     &    * ( 2.0D+00 * y(i) ** 3 - 3.0D+00 * y(i) ** 2 + y(i) )

        dudxx(i) = ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 12.0D+00 * x(i) ** 2 - 12.0D+00 * x(i) + 2.0D+00 ) 
     &    * ( 2.0D+00 * y(i) ** 3 - 3.0D+00 * y(i) ** 2 + y(i) )

        dudy(i) = ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( x(i) ** 4 - 2.0D+00 * x(i) ** 3 + x(i) ** 2 ) 
     &    * ( 6.0D+00 * y(i) ** 2  - 6.0D+00 * y(i) + 1.0D+00 )

        dudyy(i) = ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( x(i) ** 4 - 2.0D+00 * x(i) ** 3 + x(i) ** 2 ) 
     &    * ( 12.0D+00 * y(i) - 6.0D+00 )

        v(i) = - ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 2.0D+00 * x(i) ** 3 - 3.0D+00 * x(i) ** 2 + x(i) ) 
     &    * ( y(i) ** 4 - 2.0D+00 * y(i) ** 3 + y(i) ** 2 )

        dvdt(i) = - nu * 2.0D+00 
     &    * ( 2.0D+00 * x(i) ** 3 - 3.0D+00 * x(i) ** 2 + x(i) ) 
     &    * ( y(i) ** 4 - 2.0D+00 * y(i) ** 3 + y(i) ** 2 )

        dvdx(i) = - ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 6.0D+00 * x(i) ** 2 - 6.0D+00 * x(i) + 1.0D+00 ) 
     &    * ( y(i) ** 4 - 2.0D+00 * y(i) ** 3 + y(i) ** 2 )

        dvdxx(i) = - ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 12.0D+00 * x(i) - 6.0D+00 ) 
     &    * ( y(i) ** 4 - 2.0D+00 * y(i) ** 3 + y(i) ** 2 )

        dvdy(i) = - ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 2.0D+00 * x(i) ** 3 - 3.0D+00 * x(i) ** 2 + x(i) ) 
     &    * ( 4.0D+00 * y(i) ** 3 - 6.0D+00 * y(i) ** 2 
     &      + 2.0D+00 * y(i) )

        dvdyy(i) = - ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 2.0D+00 * x(i) ** 3 - 3.0D+00 * x(i) ** 2 + x(i) ) 
     &    * ( 12.0D+00 * y(i) ** 2 - 12.0D+00 * y(i) + 2.0D+00 )

        p(i) = rho * y(i)
        dpdx(i) = 0.0D+00
        dpdy(i) = rho
c
c  Evaluate the residuals.
c
        ur(i) = dudt(i) - nu * ( dudxx(i) + dudyy(i) ) 
     &    + u(i) * dudx(i) + v(i) * dudy(i) + dpdx(i) / rho - f(i)

        vr(i) = dvdt(i) - nu * ( dvdxx(i) + dvdyy(i) ) 
     &    + u(i) * dvdx(i) + v(i) * dvdy(i) + dpdy(i) / rho - g(i)

        pr(i) = dudx(i) + dvdy(i) - h(i)

      end do

      return
      end
      subroutine resid_taylor ( nu, rho, n, x, y, t, ur, vr, pr )

c*********************************************************************72
c
cc RESID_TAYLOR returns residuals of the Taylor exact Navier Stokes solution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Geoffrey Taylor,
c    On the decay of vortices in a viscous fluid,
c    Philosophical Magazine,
c    Volume 46, 1923, pages 671-674.
c
c    Geoffrey Taylor, A E Green,
c    Mechanism for the production of small eddies from large ones,
c    Proceedings of the Royal Society of London,
c    Series A, Volume 158, 1937, pages 499-521.
c
c  Parameters:
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision RHO, the density.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision X(N), Y(N), the coordinates of the points.
c
c    Input, double precision T, the time coordinate or coordinates.
c
c    Output, double precision UR(N), VR(N), PR(N), the residuals in the U,
c    V and P equations.
c
      implicit none

      integer n

      double precision dpdx(n)
      double precision dpdy(n)
      double precision dudt(n)
      double precision dudx(n)
      double precision dudxx(n)
      double precision dudy(n)
      double precision dudyy(n)
      double precision dvdt(n)
      double precision dvdx(n)
      double precision dvdxx(n)
      double precision dvdy(n)
      double precision dvdyy(n)
      double precision f(n)
      double precision g(n)
      double precision h(n)
      integer i
      double precision nu
      double precision p(n)
      double precision pr(n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision rho
      double precision t
      double precision u(n)
      double precision ur(n)
      double precision v(n)
      double precision vr(n)
      double precision x(n)
      double precision y(n)
c
c  Get the right hand sides.
c
      call rhs_taylor ( nu, rho, n, x, y, t, f, g, h )
c
c  Form the functions and derivatives for the left hand side.
c
      do i = 1, n
        u(i)  =  -                                
     &      cos ( r8_pi * x(i) ) * sin ( r8_pi * y(i) )
        dudx(i) =                       r8_pi
     &    * sin ( r8_pi * x(i) ) * sin ( r8_pi * y(i) )
        dudxx(i) =              r8_pi * r8_pi
     &    * cos ( r8_pi * x(i) ) * sin ( r8_pi * y(i) )
        dudy(i) =  -                    r8_pi
     &    * cos ( r8_pi * x(i) ) * cos ( r8_pi * y(i) )
        dudyy(i) =              r8_pi * r8_pi
     &    * cos ( r8_pi * x(i) ) * sin ( r8_pi * y(i) )
        dudt(i) =  + 2.0 * nu * r8_pi * r8_pi
     &    * cos ( r8_pi * x(i) ) * sin ( r8_pi * y(i) )

        v(i)  =                                   sin ( r8_pi * x(i) )
     &   * cos ( r8_pi * y(i) )
        dvdx(i) =                       r8_pi     * cos ( r8_pi * x(i) )
     &   * cos ( r8_pi * y(i) )
        dvdxx(i) = -            r8_pi * r8_pi     * sin ( r8_pi * x(i) )
     &   * cos ( r8_pi * y(i) )
        dvdy(i) =  -                    r8_pi     * sin ( r8_pi * x(i) )
     &   * sin ( r8_pi * y(i) )
        dvdyy(i) = -            r8_pi * r8_pi     * sin ( r8_pi * x(i) )
     &   * cos ( r8_pi * y(i) )
        dvdt(i) =  - 2.0 * nu * r8_pi * r8_pi     * sin ( r8_pi * x(i) )
     &   * cos ( r8_pi * y(i) )

        p(i) =  - 0.25D+00 * rho *
     &     ( cos ( 2.0D+00 * r8_pi * x(i) ) 
     &     + cos ( 2.0D+00 * r8_pi * y(i) ) )
        dpdx(i) = + 0.5D+00  * rho * r8_pi 
     &    * sin ( 2.0D+00 * r8_pi * x(i) )
        dpdy(i) = + 0.5D+00  * rho * r8_pi 
     &    * sin ( 2.0D+00 * r8_pi * y(i) )
c
c  Time scaling.
c
        u(i)     = u(i)     * exp ( - 2.0 * r8_pi ** 2 * nu * t )
        dudx(i)  = dudx(i)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )
        dudxx(i) = dudxx(i) * exp ( - 2.0 * r8_pi ** 2 * nu * t )
        dudy(i)  = dudy(i)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )
        dudyy(i) = dudyy(i) * exp ( - 2.0 * r8_pi ** 2 * nu * t )
        dudt(i)  = dudt(i)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )

        v(i)     = v(i)     * exp ( - 2.0 * r8_pi ** 2 * nu * t )
        dvdx(i)  = dvdx(i)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )
        dvdxx(i) = dvdxx(i) * exp ( - 2.0 * r8_pi ** 2 * nu * t )
        dvdy(i)  = dvdy(i)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )
        dvdyy(i) = dvdyy(i) * exp ( - 2.0 * r8_pi ** 2 * nu * t )
        dvdt(i)  = dvdt(i)  * exp ( - 2.0 * r8_pi ** 2 * nu * t )

        p(i)    =  p(i)     * exp ( - 4.0 * r8_pi ** 2 * nu * t )
        dpdx(i) =  dpdx(i)  * exp ( - 4.0 * r8_pi ** 2 * nu * t )
        dpdy(i) =  dpdy(i)  * exp ( - 4.0 * r8_pi ** 2 * nu * t )
c
c  Evaluate the residuals.
c
        ur(i) = dudt(i) + u(i) * dudx(i) + v(i) * dudy(i)     
     &    + ( 1.0D+00 / rho ) * dpdx(i) 
     &    - nu * ( dudxx(i) + dudyy(i) ) - f(i)

        vr(i) = dvdt(i) + u(i) * dvdx(i) + v(i) * dvdy(i)     
     &    + ( 1.0D+00 / rho ) * dpdy(i) 
     &    - nu * ( dvdxx(i) + dvdyy(i) ) - g(i)

        pr(i) = dudx(i) + dvdy(i) - h(i)

      end do

      return
      end
      subroutine rhs_lucas ( nu, rho, n, x, y, t, f, g, h )

c*****************************************************************************80
c
cc RHS_LUCAS evaluates the right hand side of Lucas Bystricky's problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision RHO, the density.
c
c    Input, integer N, the number of nodes.
c
c    Input, double precision X(N), Y(N), the coordinates of nodes.
c
c    Input, double precision T, the current time.
c
c    Output, double precision F(N), G(N), H(N), the right hand sides.
c
      implicit none

      integer n

      double precision dpdx(n)
      double precision dpdy(n)
      double precision dudt(n)
      double precision dudx(n)
      double precision dudxx(n)
      double precision dudy(n)
      double precision dudyy(n)
      double precision dvdt(n)
      double precision dvdx(n)
      double precision dvdxx(n)
      double precision dvdy(n)
      double precision dvdyy(n)
      double precision f(n)
      double precision g(n)
      double precision h(n)
      integer i
      double precision nu
      double precision p(n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision rho
      double precision t
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      do i = 1, n

        u(i) = - cos ( r8_pi * x(i) ) / r8_pi
        dudt(i) = 0.0D+00
        dudx(i) = sin ( r8_pi * x(i) )
        dudxx(i) = r8_pi * cos ( r8_pi * x(i) )
        dudy(i) = 0.0D+00
        dudyy(i) = 0.0D+00

        v(i) = - y(i) * sin ( r8_pi * x(i) )
        dvdt(i) = 0.0D+00
        dvdx(i) = - r8_pi * y(i) * cos ( r8_pi * x(i) )
        dvdxx(i) = + r8_pi ** 2 * y(i) * sin ( r8_pi * x(i) )
        dvdy(i) = - sin ( r8_pi * x(i) )
        dvdyy(i) = 0.0D+00

        p(i) = 0.0D+00
        dpdx(i) = 0.0D+00
        dpdy(i) = 0.0D+00

        f(i) = dudt(i) 
     &    - nu * ( dudxx(i) + dudyy(i) )
     &    + u(i) * dudx(i) + v(i) * dudy(i) + dpdx(i) / rho

        g(i) = dvdt(i) 
     &    - nu * ( dvdxx(i) + dvdyy(i) )
     &    + u(i) * dvdx(i) + v(i) * dvdy(i) + dpdy(i) / rho

        h(i) = dudx(i) + dvdy(i)

      end do

      return
      end
      subroutine rhs_spiral ( nu, rho, n, x, y, t, f, g, h )

c*********************************************************************72
c
cc RHS_SPIRAL evaluates the right hand side of the spiral flow problem.
c
c  Discussion:
c
c    The right hand side is artificially determined by the requirement
c    that the specified values of U, V and P satisfy the discretized
c    Navier Stokes and continuity equations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Maxim Olshanskii, Leo Rebholz,
c    Application of barycenter refined meshes in linear elasticity
c    and incompressible fluid dynamics,
c    ETNA: Electronic Transactions in Numerical Analysis,
c    Volume 38, pages 258-274, 2011.
c
c  Parameters:
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision RHO, the fluid density.
c
c    Input, integer N, the number of nodes.
c
c    Input, double precision X(N), Y(N), the coordinates of nodes.
c
c    Input, double precision T, the current time.
c
c    Output, double precision F(N), G(N), H(N), the right hand sides.
c
      implicit none

      integer n

      double precision dpdx(n)
      double precision dpdy(n)
      double precision dudt(n)
      double precision dudx(n)
      double precision dudxx(n)
      double precision dudy(n)
      double precision dudyy(n)
      double precision dvdt(n)
      double precision dvdx(n)
      double precision dvdxx(n)
      double precision dvdy(n)
      double precision dvdyy(n)
      double precision f(n)
      double precision g(n)
      double precision h(n)
      integer i
      double precision nu
      double precision p(n)
      double precision rho
      double precision t
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      do i = 1, n

        u(i) = ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( x(i) ** 4 - 2.0D+00 * x(i) ** 3 + x(i) ** 2 ) 
     &    * ( 2.0D+00 * y(i) ** 3 - 3.0D+00 * y(i) ** 2 + y(i) )

        dudt(i) = nu * 2.0D+00 
     &    * ( x(i) ** 4 - 2.0D+00 * x(i) ** 3 + x(i) ** 2 ) 
     &    * ( 2.0D+00 * y(i) ** 3  - 3.0D+00 * y(i) ** 2 + y(i) )

        dudx(i) = ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 4.0D+00 * x(i) ** 3 - 6.0D+00 * x(i) ** 2 
     &      + 2.0D+00 * x(i) )
     &    * ( 2.0D+00 * y(i) ** 3 - 3.0D+00 * y(i) ** 2 + y(i) )

        dudxx(i) = ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 12.0D+00 * x(i) ** 2 - 12.0D+00 * x(i) + 2.0D+00 ) 
     &    * ( 2.0D+00 * y(i) ** 3 - 3.0D+00 * y(i) ** 2 + y(i) )

        dudy(i) = ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( x(i) ** 4 - 2.0D+00 * x(i) ** 3 + x(i) ** 2 ) 
     &    * ( 6.0D+00 * y(i) ** 2  - 6.0D+00 * y(i) + 1.0D+00 )

        dudyy(i) = ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( x(i) ** 4 - 2.0D+00 * x(i) ** 3 + x(i) ** 2 ) 
     &    * ( 12.0D+00 * y(i) - 6.0D+00 )

        v(i) = - ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 2.0D+00 * x(i) ** 3 - 3.0D+00 * x(i) ** 2 + x(i) ) 
     &    * ( y(i) ** 4 - 2.0D+00 * y(i) ** 3 + y(i) ** 2 )

        dvdt(i) = - nu * 2.0D+00 
     &    * ( 2.0D+00 * x(i) ** 3 - 3.0D+00 * x(i) ** 2 + x(i) ) 
     &    * ( y(i) ** 4 - 2.0D+00 * y(i) ** 3 + y(i) ** 2 )

        dvdx(i) = - ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 6.0D+00 * x(i) ** 2 - 6.0D+00 * x(i) + 1.0D+00 ) 
     &    * ( y(i) ** 4 - 2.0D+00 * y(i) ** 3 + y(i) ** 2 )

        dvdxx(i) = - ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 12.0D+00 * x(i) - 6.0D+00 ) 
     &    * ( y(i) ** 4 - 2.0D+00 * y(i) ** 3 + y(i) ** 2 )

        dvdy(i) = - ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 2.0D+00 * x(i) ** 3 - 3.0D+00 * x(i) ** 2 + x(i) ) 
     &    * ( 4.0D+00 * y(i) ** 3 - 6.0D+00 * y(i) ** 2 
     &      + 2.0D+00 * y(i) )

        dvdyy(i) = - ( 1.0D+00 + nu * t ) * 2.0D+00 
     &    * ( 2.0D+00 * x(i) ** 3 - 3.0D+00 * x(i) ** 2 + x(i) ) 
     &    * ( 12.0D+00 * y(i) ** 2 - 12.0D+00 * y(i) + 2.0D+00 )

        p(i) = rho * y(i)
        dpdx(i) = 0.0D+00
        dpdy(i) = rho

        f(i) = dudt(i) - nu * ( dudxx(i) + dudyy(i) ) 
     &    + u(i) * dudx(i) + v(i) * dudy(i) + dpdx(i) / rho

        g(i) = dvdt(i) - nu * ( dvdxx(i) + dvdyy(i) ) 
     &    + u(i) * dvdx(i) + v(i) * dvdy(i) + dpdy(i) / rho

        h(i) = dudx(i) + dvdy(i)

      end do

      return
      end
      subroutine rhs_taylor ( nu, rho, n, x, y, t, f, g, h )

c*********************************************************************72
c
cc RHS_TAYLOR returns right hand sides of the Taylor vortex equations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Geoffrey Taylor,
c    On the decay of vortices in a viscous fluid,
c    Philosophical Magazine,
c    Volume 46, 1923, pages 671-674.
c
c    Geoffrey Taylor, A E Green,
c    Mechanism for the production of small eddies from large ones,
c    Proceedings of the Royal Society of London,
c    Series A, Volume 158, 1937, pages 499-521.
c
c  Parameters:
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision RHO, the density.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision X(N), Y(N), the coordinates of the points.
c
c    Input, double precision T, the time coordinate or coordinates.
c
c    Output, double precision F(N), G(N), H(N), the right hand sides in the U,
c    V and P equations.
c
      implicit none

      integer n

      double precision f(n)
      double precision g(n)
      double precision h(n)
      integer i
      double precision nu
      double precision rho
      double precision t
      double precision x(n)
      double precision y(n)

      do i = 1, n
        f(i) = 0.0D+00
        g(i) = 0.0D+00
        h(i) = 0.0D+00
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
      subroutine uvp_lucas ( nu, rho, n, x, y, t, u, v, p )

c*****************************************************************************80
c
cc UVP_LUCAS evaluates Lucas Bystricky's exact Navier Stokes solution.
c
c  Discussion:
c
c    There is no time dependence.
c
c    The pressure is 0.
c
c    The preferred domain is the unit square.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision RHO, the density.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision X(N), Y(N), the coordinates of the points.
c
c    Input, double precision T, the time coordinate or coordinates.
c
c    Output, double precision U(N), V(N), P(N), the velocity components and
c    pressure at each of the points.
c
      implicit none

      integer n

      integer i
      double precision nu
      double precision p(n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision rho
      double precision t
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      do i = 1, n

        u(i) = - cos ( r8_pi * x(i) ) / r8_pi
        v(i) = - y(i) * sin ( r8_pi * x(i) )
        p(i) = 0.0D+00

      end do

      return
      end
      subroutine uvp_spiral ( nu, rho, n, x, y, t, u, v, p )

c*********************************************************************72
c
cc UVP_SPIRAL returns velocity and pressure for the spiral flow.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Maxim Olshanskii, Leo Rebholz,
c    Application of barycenter refined meshes in linear elasticity
c    and incompressible fluid dynamics,
c    ETNA: Electronic Transactions in Numerical Analysis,
c    Volume 38, pages 258-274, 2011.
c
c  Parameters:
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision RHO, the fluid density.
c
c    Input, integer N, the number of nodes.
c
c    Input, double precision X(N), Y(N), the coordinates of nodes.
c
c    Input, double precision T, the current time.
c
c    Output, double precision U(N), V(N), the X and Y velocity.
c
c    Output, double precision P(N), the pressure.
c
      implicit none

      integer n

      integer i
      double precision nu
      double precision p(n)
      double precision rho
      double precision t
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      do i = 1, n

        u(i) = ( 1.0D+00 + nu * t ) * 2.0D+00
     &    * x(i)**2 * ( x(i) - 1.0D+00 )**2
     &    * y(i) * ( 2.0D+00 * y(i) - 1.0D+00 ) * ( y(i) - 1.0D+00 )

        v(i) = - ( 1.0D+00 + nu * t ) * 2.0D+00
     &    * x(i) * ( 2.0D+00 * x(i) - 1.0D+00 ) * ( x(i) - 1.0D+00 )
     &    * y(i)**2 * ( y(i) - 1.0D+00 )**2

        p(i) = rho * y(i)

      end do

      return
      end
      subroutine uvp_taylor ( nu, rho, n, x, y, t, u, v, p )

c*****************************************************************************80
c
cc UVP_TAYLOR evaluates the Taylor exact Navier Stokes solution.
c
c  Discussion:
c
c    This flow is known as a Taylor-Green vortex.
c
c    The given velocity and pressure fields are exact solutions for the 2D
c    incompressible time-dependent Navier Stokes equations over any region.
c
c    To define a typical problem, one chooses a bounded spatial region
c    and a starting time, and then imposes boundary and initial conditions
c    by referencing the exact solution appropriately.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Geoffrey Taylor,
c    On the decay of vortices in a viscous fluid,
c    Philosophical Magazine,
c    Volume 46, 1923, pages 671-674.
c
c    Geoffrey Taylor, A E Green,
c    Mechanism for the production of small eddies from large ones,
c    Proceedings of the Royal Society of London,
c    Series A, Volume 158, 1937, pages 499-521.
c
c  Parameters:
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision RHO, the density.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision X(N), Y(N), the coordinates of the points.
c
c    Input, double precision T, the time coordinate or coordinates.
c
c    Output, double precision U(N), V(N), P(N), the velocity components and
c    pressure at each of the points.
c
      implicit none

      integer n

      integer i
      double precision nu
      double precision p(n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision rho
      double precision t
      double precision u(n)
      double precision v(n)
      double precision x(n)
      double precision y(n)

      do i = 1, n
        u(i) = - cos ( r8_pi * x(i) ) * sin ( r8_pi * y(i) )
        v(i) =   sin ( r8_pi * x(i) ) * cos ( r8_pi * y(i) )
        p(i) = - 0.25D+00 * rho     
     &    * ( cos ( 2.0D+00 * r8_pi * x(i) )
     &      + cos ( 2.0D+00 * r8_pi * y(i) ) )
      end do

      do i = 1, n
        u(i) = u(i) * exp ( - 2.0D+00 * r8_pi ** 2 * nu * t )
        v(i) = v(i) * exp ( - 2.0D+00 * r8_pi ** 2 * nu * t )
        p(i) = p(i) * exp ( - 4.0D+00 * r8_pi ** 2 * nu * t )
      end do

      return
      end
