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
      subroutine ou_euler ( theta, mu, sigma, x0, tmax, n, seed )

c*********************************************************************72
c
cc OU_EULER applies the Euler method to the Ornstein-Uhlenbeck SDE.
c
c  Discussion:
c
c    The stochastic differential equation (SDE) is:
c
c      dx(t) = theta * ( mu - x(t) ) dt + sigma dW,   
c      x(0) = x0.
c
c    The discretized Brownian path uses a constant stepsize.
c
c    For an SDE of the form:
c
c      dx = f(x(t)) dt + g(x(t)) dW(t),
c
c    the Euler method has the form:
c
c      x(j) = x(j-1) + f(x(j-1)) * dt + g(x(j-1)) * dW(j-1)
c
c    Note that if SIGMA is zero, the problem becomes deterministic,
c    with solution:
c
c      x(t) = mu + ( x0 - mu ) * exp ( - theta * t )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Desmond Higham,
c    An Algorithmic Introduction to Numerical Simulation of
c    Stochastic Differential Equations,
c    SIAM Review,
c    Volume 43, Number 3, September 2001, pages 525-546
c
c  Parameters:
c
c    Input, double precision THETA, MU, SIGMA, the value of problem parameters.
c
c    Input, double precision X0, the initial condition.  When studying many
c    realizations of this problem, it is usual for X0 to be chosen
c    from a normal distribution.
c
c    Input, double precision TMAX, the final time.
c
c    Input, integer N, the number of time steps.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
      implicit none

      integer n

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision dt
      double precision dw(n)
      integer j
      double precision mu
      integer seed
      double precision sigma
      double precision t(0:n)
      double precision theta
      double precision tmax
      double precision x(0:n)
      double precision x0

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'OU_EULER:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Use an Euler method to approximate the solution of'
      write ( *, '(a)' ) 
     &  '  the Ornstein-Uhlenbeck stochastic differential equation:'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '    d x(t) = theta * ( mu - x(t) ) dt + sigma dW'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  with initial condition x(0) = x0.'
c
c  Set the discrete time stepsize.
c
      dt = tmax / dble ( n )
c
c  Compute the Brownian increments.
c
      call r8vec_normal_01 ( n, seed, dw )
      do j = 1, n
        dw(j) = dw(j) * sqrt ( dt )
      end do
c
c  Carry out the Euler approximate integration process.
c
      call r8vec_linspace ( n + 1, 0.0D+00, tmax, t )

      x(0) = x0
      do j = 1, n
        x(j) = x(j-1) + dt * theta * ( mu - x(j-1) ) + sigma * dw(j)
      end do
c
c  Write the plot data file.
c
      call get_unit ( data_unit )
      data_filename = 'ou_euler_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 0, n
        write ( data_unit, '(2x,g14.6,2x,g14.6)' ) t(j), x(j)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) '  Created data file "' 
     &  // trim ( data_filename ) // '".'
c
c  Write the plot command file.
c
      call get_unit ( command_unit )
      command_filename = 'ou_euler_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "ou_euler.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- T --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- X(T) --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Euler Solution of Ornstein-Uhlenbeck SDE"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue"'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
      subroutine ou_euler_maruyama ( theta, mu, sigma, x0, tmax, n, 
     &  r, seed )

c*********************************************************************72
c
cc OU_EULER_MARUYAMA applies Euler-Maruyama to the Ornstein-Uhlenbeck SDE.
c
c  Discussion:
c
c    The stochastic differential equation (SDE) is:
c
c      dx = theta * ( mu - x(t) ) dt + sigma dW,   
c      x(0) = x0,
c
c    The discretized Brownian path uses a constant stepsize.
c
c    A "large" time step DT_LARGE is used for the smooth portion of the
c    equation, while a smaller time step DT_SMALL is used for the
c    discretized Brownian path.  We take R small steps to equal one 
c    large step, so that:
c
c      dt_large = r * dt_small = tmax / n
c
c    For an SDE of the form:
c
c      dx = f(x(t)) dt + g(x(t)) dW(t)
c
c    the Euler-Maruyama method has the form:
c
c      x(j) = x(j-1) + f(X(j-1)) * dt_large + g(X(j-1)) * dW(j-1)
c
c    where dW(j-1) is approximated by the sum of R normal random values
c    multiplied by the square root of DT_SMALL.
c
c    Note that if SIGMA is zero, the problem becomes deterministic,
c    with solution
c
c      x(t) = mu + ( x0 - mu ) * exp ( - theta * t )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Desmond Higham,
c    An Algorithmic Introduction to Numerical Simulation of
c    Stochastic Differential Equations,
c    SIAM Review,
c    Volume 43, Number 3, September 2001, pages 525-546
c
c  Parameters:
c
c    Input, double precision THETA, MU, SIGMA, the value of problem parameters.
c
c    Input, double precision X0, the initial condition.  When studying many
c    realizations of this problem, it is usual for X0 to be chosen
c    from a normal distribution.
c
c    Input, double precision TMAX, the final time.
c
c    Input, integer N, the number of large scale time steps.
c
c    Input, integer R, the number of small scale time steps per single
c    large scale time step.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
      implicit none

      integer n
      integer r

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision dt_large
      double precision dt_small
      double precision dw(r)
      double precision dw_sum
      integer i
      integer j
      double precision mu
      integer seed
      double precision sigma
      double precision t(0:n)
      double precision theta
      double precision tmax
      double precision x(0:n)
      double precision x0

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'OU_EULER_MARUYAMA:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Use an Euler-Maruyama method to approximate the solution of'
      write ( *, '(a)' ) 
     &  '  the Ornstein-Uhlenbeck stochastic differential equation:'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '    d x(t) = theta * ( mu - x(t) ) dt + sigma dW'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  with initial condition x(0) = x0.'
c
c  Set time steps.
c
      dt_large = tmax / dble ( n )
      dt_small = tmax / dble ( n ) / dble ( r )
c
c  Carry out the Euler-Maruyama approximate integration process.
c
      call r8vec_linspace ( n + 1, 0.0D+00, tmax, t )

      x(0) = x0
      do j = 1, n
        call r8vec_normal_01 ( r, seed, dw )
        dw_sum = 0.0D+00
        do i = 1, r
          dw_sum = dw_sum + dw(i)
        end do
        dw_sum = dw_sum * sqrt ( dt_small )
        x(j) = x(j-1) + dt_large * theta * ( mu - x(j-1) ) 
     &    + sigma * dw_sum
      end do
c
c  Plot the approximate solution.
c
      call get_unit ( data_unit )
      data_filename = 'ou_euler_maruyama_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 0, n
        write ( data_unit, '(2x,g14.6,2x,g14.6)' ) t(j), x(j)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) '  Created data file "' 
     &  // trim ( data_filename ) // '".'

      call get_unit ( command_unit )
      command_filename = 'ou_euler_maruyama_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "ou_euler_maruyama.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- T --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- X(T) --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Euler-Maruyama Solution of Ornstein-Uhlenbeck SDE"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue"'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a pseudorandom R8 scaled to [0,1].
c
c  Discussion:
c
c    This routine implements the recursion
c
c      seed = 16807 * seed mod ( 2^31 - 1 )
c      r8_uniform_01 = seed / ( 2^31 - 1 )
c
c    The integer arithmetic never requires more than 32 bits,
c    including a sign bit.
c
c    If the initial seed is 12345, then the first three computations are
c
c      Input     Output      R8_UNIFORM_01
c      SEED      SEED
c
c         12345   207482415  0.096616
c     207482415  1790989824  0.833995
c    1790989824  2035175616  0.947702
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley Interscience, page 95, 1998.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      double precision r8_uniform_01
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine r8vec_linspace ( n, a_first, a_last, a )

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
c    Input, double precision A_FIRST, A_LAST, the first and last entries.
c
c    Output, double precision A(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_first
      double precision a_last
      integer i

      if ( n .eq. 1 ) then

        a(1) = ( a_first + a_last ) / 2.0D+00

      else

        do i = 1, n
          a(i) = ( dble ( n - i     ) * a_first 
     &           + dble (     i - 1 ) * a_last )
     &           / dble ( n     - 1 )
        end do

      end if

      return
      end
      subroutine r8vec_normal_01 ( n, seed, x )

c*********************************************************************72
c
cc R8VEC_NORMAL_01 returns a unit pseudonormal R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The standard normal probability distribution function (PDF) has
c    mean 0 and standard deviation 1.
c
c    This routine can generate a vector of values on one call.  It
c    has the feature that it should provide the same results
c    in the same order no matter how we break up the task.
c
c    The Box-Muller method is used, which is efficient, but
c    generates an even number of values each time.  On any call
c    to this routine, an even number of new values are generated.
c    Depending on the situation, one value may be left over.
c    In that case, it is saved for the next call.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values desired.  If N is negative,
c    then the code will flush its internal memory; in particular,
c    if there is a saved value to be used on the next call, it is
c    instead discarded.  This is useful if the user has reset the
c    random number seed, for instance.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision X(N), a sample of the standard normal PDF.
c
c  Local parameters:
c
c    Local, integer MADE, records the number of values that have
c    been computed.  On input with negative N, this value overwrites
c    the return value of N, so the user can get an accounting of
c    how much work has been done.
c
c    Local, integer SAVED, is 0 or 1 depending on whether there is a
c    single saved value left over from the previous call.
c
c    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
c    X that we need to compute.  This starts off as 1:N, but is adjusted
c    if we have a saved value that can be immediately stored in X(1),
c    and so on.
c
c    Local, double precision Y, the value saved from the previous call, if
c    SAVED is 1.
c
      implicit none

      integer n

      integer i
      integer m
      integer made
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r(2)
      double precision r8_uniform_01
      integer saved
      integer seed
      double precision x(n)
      integer x_hi_index
      integer x_lo_index
      double precision y

      save made
      save saved
      save y

      data made / 0 /
      data saved / 0 /
      data y / 0.0D+00 /
c
c  I'd like to allow the user to reset the internal data.
c  But this won't work properly if we have a saved value Y.
c  I'm making a crock option that allows the user to signal
c  explicitly that any internal memory should be flushed,
c  by passing in a negative value for N.
c
      if ( n .lt. 0 ) then
        n = made
        made = 0
        saved = 0
        y = 0.0D+00
        return
      else if ( n .eq. 0 ) then
        return
      end if
c
c  Record the range of X we need to fill in.
c
      x_lo_index = 1
      x_hi_index = n
c
c  Use up the old value, if we have it.
c
      if ( saved .eq. 1 ) then
        x(1) = y
        saved = 0
        x_lo_index = 2
      end if
c
c  Maybe we don't need any more values.
c
      if ( x_hi_index - x_lo_index + 1 .eq. 0 ) then
c
c  If we need just one new value, do that here to avoid null arrays.
c
      else if ( x_hi_index - x_lo_index + 1 .eq. 1 ) then

        r(1) = r8_uniform_01 ( seed )

        if ( r(1) .eq. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8VEC_NORMAL_01 - Fatal errorc'
          write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
          stop
        end if

        r(2) = r8_uniform_01 ( seed )

        x(x_hi_index) =
     &           sqrt ( -2.0D+00 * log ( r(1) ) )
     &           * cos ( 2.0D+00 * pi * r(2) )
        y =      sqrt ( -2.0D+00 * log ( r(1) ) )
     &           * sin ( 2.0D+00 * pi * r(2) )

        saved = 1

        made = made + 2
c
c  If we require an even number of values, that's easy.
c
      else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) .eq. 0 ) then

        do i = x_lo_index, x_hi_index, 2

          call r8vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * cos ( 2.0D+00 * pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * sin ( 2.0D+00 * pi * r(2) )

        end do

        made = made + x_hi_index - x_lo_index + 1
c
c  If we require an odd number of values, we generate an even number,
c  and handle the last pair specially, storing one in X(N), and
c  saving the other for later.
c
      else

        do i = x_lo_index, x_hi_index - 1, 2

          call r8vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * cos ( 2.0D+00 * pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * sin ( 2.0D+00 * pi * r(2) )

        end do

        call r8vec_uniform_01 ( 2, seed, r )

        x(n) = sqrt ( -2.0D+00 * log ( r(1) ) )
     &    * cos ( 2.0D+00 * pi * r(1) )

        y = sqrt ( -2.0D+00 * log ( r(2) ) )
     &    * sin ( 2.0D+00 * pi * r(2) )

        saved = 1

        made = made + x_hi_index - x_lo_index + 2

      end if

      return
      end
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
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
c    17 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

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
