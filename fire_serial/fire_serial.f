      program main

c*********************************************************************72
c
cc MAIN is the main program for FIRE_SERIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 June 2013.
c
c  Author:
c
c    FORTRAN77 version by John Burkardt
c
      implicit none

      integer forest_size
      parameter ( forest_size = 20 )

      integer forest(forest_size,forest_size)
      logical forest_is_burning
      integer i
      integer i_ignite
      integer i4_uniform_ab
      integer j_ignite
      integer offset
      double precision percent
      double precision prob_spread
      parameter ( prob_spread = 0.5D+00 )
      integer seed
      double precision u

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FIRE_SERIAL'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  A probabilistic simulation of a forest fire.'
      write ( *, '(a,g14.6)' ) 
     &  '  The probability of tree-to-tree spread is ', prob_spread
c
c  Initialize the random number generator.
c
      call get_seed ( seed )
      seed = seed + offset
      write ( *, '(a,i12)' ) 
     &  '  The random number generator is seeded by ', seed
c
c  Initialize the values in the forest.
c
      call forest_initialize ( forest_size, forest )
c
c  Choose a tree at random where the fire will start.
c
      i_ignite = i4_uniform_ab ( 1, forest_size, seed )
      j_ignite = i4_uniform_ab ( 1, forest_size, seed )
      call tree_ignite ( forest_size, forest, i_ignite, j_ignite )
      write ( *, '(a)' ) ''
      write ( *, '(a,i2,a,i2,a)' ) 
     &  '  Fire starts at tree(', i_ignite, ',', j_ignite, ')'
c
c  Let time run until nothing is burning any more.
c
10    continue

      if ( forest_is_burning ( forest_size, forest ) ) then
        call forest_burns ( forest_size, forest, seed, prob_spread )
        go to 10
      end if
c
c  Display the final forest state.
c
      call forest_print ( forest_size, forest, i_ignite, j_ignite )
c
c  Report the percentage of forest burned.
c
      call get_percent_burned ( forest_size, forest, percent )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) 
     &  '  Percentage of forest burned = ', percent
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FIRE_SERIAL:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      function fire_spreads ( seed, prob_spread ) 

c*********************************************************************72
c
cc FIRE_SPREADS determines whether the fire spreads.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 June 2013
c
c  Author:
c
c    FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Input, double precision PROB_SPREAD, the probability of spreading.
c
c    Output, logical FIRE_SPREADS, is TRUE if the fire spreads.
c
      implicit none

      double precision prob_spread
      logical fire_spreads
      double precision r8_uniform_01
      integer seed
      double precision u

      u = r8_uniform_01 ( seed )

      if ( u .lt. prob_spread ) then
        fire_spreads = .true.
      else
        fire_spreads = .false.
      end if
     
      return
      end
      subroutine forest_burns ( forest_size, forest, seed, prob_spread )

c*********************************************************************72
c
cc FOREST_BURNS models a single time step of the burning forest.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 June 2013
c
c  Author:
c
c    FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer FOREST_SIZE, the linear dimension of the forest.
c
c    Input/output, integer FOREST(FOREST_SIZE,FOREST_SIZE), an
c    array with an entry for each tree in the forest.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Input, double precision PROB_SPREAD, the probability that the fire will 
c    spread from a burning tree to an unburnt one.
c
      implicit none

      integer forest_size

      integer BURNING
      parameter ( BURNING = 2 )
      integer BURNT
      parameter ( BURNT = 3 )
      logical fire_spreads
      integer forest(forest_size,forest_size)
      integer i
      integer j
      double precision prob_spread
      integer seed
      integer SMOLDERING
      parameter ( SMOLDERING = 1 )
      integer UNBURNT
      parameter ( UNBURNT = 0 )
c
c  Burning trees burn down;
c  Smoldering trees ignite;
c
      do j = 1, forest_size
        do i = 1, forest_size
          if ( forest(i,j) .eq. BURNING ) then
            forest(i,j) = BURNT
          else if ( forest(i,j) .eq. SMOLDERING ) then
            forest(i,j) = BURNING
          end if
        end do
      end do
c
c  Unburnt trees might catch fire.
c
      do j = 1, forest_size
        do i = 1, forest_size

          if ( forest(i,j) .eq. BURNING ) then
c
c  North.
c
            if ( 1 .lt. i ) then
              if ( fire_spreads ( seed, prob_spread ) .and. 
     &             forest(i-1,j) .eq. UNBURNT ) then
                forest(i-1,j) = SMOLDERING
              end if
            end if
c
c  South.
c
            if ( i .lt. forest_size ) then
              if ( fire_spreads ( seed, prob_spread ) .and. 
     &             forest(i+1,j) .eq. UNBURNT ) then
                forest(i+1,j) = SMOLDERING
              end if
            end if
c
c  West.
c
            if ( 1 .lt. j ) then
              if ( fire_spreads ( seed, prob_spread ) .and. 
     &             forest(i,j-1) .eq. UNBURNT ) then
                forest(i,j-1) = SMOLDERING
              end if
            end if
c
c  East.
c
            if ( j .lt. forest_size ) then
              if ( fire_spreads ( seed, prob_spread ) .and. 
     &             forest(i,j+1) .eq. UNBURNT ) then
                forest(i,j+1) = SMOLDERING
              end if
            end if

          end if

        end do
      end do

      return
      end
      subroutine forest_initialize ( forest_size, forest ) 

c*********************************************************************72
c
cc FOREST_INITIALIZE initializes the forest values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 June 2013
c
c  Author:
c
c    FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer FOREST_SIZE, the linear dimension of the forest.
c
c    Output, integer FOREST(FOREST_SIZE,FOREST_SIZE), an array
c    with an entry for each tree in the forest.
c
      implicit none

      integer forest_size

      integer forest(forest_size,forest_size)
      integer UNBURNT
      parameter ( UNBURNT = 0 )

      forest(1:forest_size,1:forest_size) = UNBURNT

      return
      end
      function forest_is_burning ( forest_size, forest ) 

c*********************************************************************72
c
cc FOREST_IS_BURNING reports whether any trees in the forest are burning.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 June 2013
c
c  Author:
c
c    FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer FOREST_SIZE, the linear dimension of the forest.
c
c    Input, integer FOREST(FOREST_SIZE,FOREST_SIZE), an array
c    with an entry for each tree in the forest.
c
c    Output, logical FOREST_IS_BURNING, is TRUE if any tree in the forest
c    is in the SMOLDERING or BURNING state.
c
      implicit none

      integer forest_size

      integer BURNING
      parameter ( BURNING = 2 )
      integer forest(forest_size,forest_size)
      logical forest_is_burning
      integer i
      integer j
      integer SMOLDERING
      parameter ( SMOLDERING = 1 )
      logical value

      value = .false.

      do j = 1, forest_size
        do i = 1, forest_size
          if ( forest(i,j) .eq. SMOLDERING .or. 
     &         forest(i,j) .eq. BURNING ) then
            value = .true.
          end if
        end do
      end do

      forest_is_burning = value

      return
      end
      subroutine forest_print ( forest_size, forest, i_ignite, 
     &  j_ignite )

c*********************************************************************72
c
cc FOREST_PRINT prints the state of the trees in the forest.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 June 2013
c
c  Author:
c
c    FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer FOREST_SIZE, the linear dimension of the forest.
c
c    Input, integer FOREST(FOREST_SIZE,FOREST_SIZE), an array
c    with an entry for each tree in the forest.
c
c    Input, integer I_IGNITE, J_IGNITE, the location of the start 
c    of the fire.
c
      implicit none

      integer forest_size

      integer BURNT
      parameter ( BURNT = 3 )
      integer forest(forest_size,forest_size)
      integer i
      integer i_ignite
      integer j
      integer j_ignite
      character line(forest_size)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Map of fire damage.'
      write ( *, '(a)' ) '  Fire started at "*".'
      write ( *, '(a)' ) '  Burned trees are indicated by ".".'
      write ( *, '(a)' ) '  Unburned trees are indicated by "X".'
      write ( *, '(a)' ) ''

      do i = 1, forest_size
        do j = 1, forest_size
          if ( i .eq. i_ignite .and. j .eq. j_ignite ) then
            line(j) = '*'
          else if ( forest(i,j) .eq. BURNT ) then
            line(j) = '.'
          else
            line(j) = 'X'
          end if
        end do
        write ( *, '(2x,80a)' ) line(1:forest_size)
      end do

      return
      end
      subroutine get_percent_burned ( forest_size, forest, percent ) 

c*********************************************************************72
c
cc GET_PERCENT_BURNED computes the percentage of the forest that burned.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 June 2013
c
c  Author:
c
c    FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer FOREST_SIZE, the linear dimension of the forest.
c
c   Input, integer FOREST(FOREST_SIZE,FOREST_SIZE), an array
c    with an entry for each tree in the forest.
c
c    Output, double precision PERCENT, the percentage of the forest
c    that burned.
c
      implicit none

      integer forest_size

      integer BURNT
      parameter ( BURNT = 3 )
      integer forest(forest_size,forest_size)
      integer i
      integer j
      double precision percent
      integer total

      total = 0
      do j = 1, forest_size
        do i = 1, forest_size
          if ( forest(i,j) .eq. BURNT ) then
            total = total + 1
          end if
        end do
      end do

      percent = dble ( total ) / dble ( forest_size * forest_size )

      return
      end
      subroutine get_seed ( seed )

c*********************************************************************72
c
cc GET_SEED returns a seed for the random number generator.
c
c  Discussion:
c
c    The seed depends on the current time, and ought to be (slightly)
c    different every millisecond.  Thus, calling this routine several
c    times in succession will probably return the SAME seed, but
c    calling it a few minutes or days apart will turn a suitably
c    "random" seed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    11 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer SEED, a pseudorandom seed value.
c
      implicit none

      integer day
      integer hour
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer milli
      integer minute
      integer month
      integer second
      integer seed
      double precision temp
      character * ( 10 ) time
      character * ( 8 ) date
      integer year

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) year, month, day
      read ( time, '(i2,i2,i2,1x,i3)' ) hour, minute, second, milli

      temp = 0.0D+00
      temp = temp + dble ( month - 1 ) / 11.0D+00
      temp = temp + dble ( day   - 1 ) / 30.0D+00
      temp = temp + dble ( hour      ) / 23.0D+00
      temp = temp + dble ( minute    ) / 59.0D+00
      temp = temp + dble ( second    ) / 59.0D+00
      temp = temp + dble ( milli     ) / 999.0D+00

      temp = temp / 6.0D+00
c
c  Force 0 < TEMP <= 1.
c
10    continue

      if ( temp .le. 0.0D+00 ) then
        temp = temp + 1.0D+00
        go to 10
      end if

20    continue

      if ( 1.0D+00 .lt. temp ) then
        temp = temp - 1.0D+00
        go to 20
      end if

      seed = int ( dble ( i4_huge ) * temp )
c
c  Never use a seed of 0 or maximum integer.
c
      if ( seed .eq. 0 ) then
        seed = 1
      end if

      if ( seed .eq. i4_huge ) then
        seed = seed - 1
      end if

      return
      end
      function i4_uniform_ab ( a, b, seed )

c*********************************************************************72
c
cc I4_UNIFORM_AB returns a scaled pseudorandom I4 between A and B.
c
c  Discussion:
c
c    An I4 is an integer value.
c
c    The pseudorandom number should be uniformly distributed
c    between A and B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 November 2006
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
c    Input, integer A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, integer I4_UNIFORM_AB, a number between A and B.
c
      implicit none

      integer a
      integer b
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4_uniform_ab
      integer k
      real r
      integer seed
      integer value

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge
      end if

      r = real ( seed ) * 4.656612875E-10
c
c  Scale R to lie between A-0.5 and B+0.5.
c
      r = ( 1.0E+00 - r ) * ( real ( min ( a, b ) ) - 0.5E+00 )
     &  +             r   * ( real ( max ( a, b ) ) + 0.5E+00 )
c
c  Use rounding to convert R to an integer between A and B.
c
      value = nint ( r )

      value = max ( value, min ( a, b ) )
      value = min ( value, max ( a, b ) )

      i4_uniform_ab = value

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
      subroutine tree_ignite ( forest_size, forest, i_ignite, j_ignite )

c*********************************************************************72
c
cc TREE_IGNITE sets a given tree to the SMOLDERING state.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 June 2013
c
c  Author:
c
c    FORTRAN77 version by John Burkardt
c
c  Parameters:
c
c    Input, integer FOREST_SIZE, the linear dimension of 
c    the forest.
c
c    Input, integer FOREST(FOREST_SIZE,FOREST_SIZE), an array
c    with an entry for each tree in the forest.
c
c    Input, integer I_IGNITE, J_IGNITE, the coordinates of the 
c    tree which is to be set to SMOLDERING.
c
      implicit none

      integer forest_size

      integer forest(forest_size,forest_size)
      integer i_ignite
      integer j_ignite
      integer SMOLDERING
      parameter ( SMOLDERING = 1 )

      forest(i_ignite,j_ignite) = SMOLDERING

      return
      end
