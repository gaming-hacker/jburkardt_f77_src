      program main

c*********************************************************************72
c
cc FAIR_DICE_SIMULATION simulates N throws of two fair dice.
c
c  Usage:
c
c    fair_dice n
c
c    where 
c
c    * n is the number of times the dice should be thrown.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Command line, integer N, the number of times the dice are thrown.
c
      implicit none

      integer arg_num
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      integer die1
      integer die2
      integer i
      integer i4_uniform
      integer iarg
      integer ierror
      integer last
      integer n
      integer seed
      integer score
      integer score_count(12)
      character * ( 80 ) string

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FAIR_DICE_SIMULATION:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Simulate N throws of a pair of fair dice.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Get the number of rolls.
c
      if ( 1 .le. arg_num ) then

        iarg = 1
        call getarg ( iarg, string )
        call s_to_i4 ( string, n, ierror, last )
      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Enter N, the number of times the dice are thrown: '

        read ( *, * ) n

      end if
c
c  Initialize the random number generator.
c
      call get_seed ( seed )
c
c  For convenience, include slots for 0 and 1, even though they never occur.
c
      do i = 1, 12
        score_count(i) = 0
      end do
c
c  Roll N times.
c
      do i = 1, n
        die1 = i4_uniform ( 1, 6, seed )
        die2 = i4_uniform ( 1, 6, seed )
        score = die1 + die2
        score_count(score) = score_count(score) + 1
      end do
c
c  Write the data file.
c
      data_filename = 'fair_dice_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )

      do score = 2, 12
        write ( data_unit, '(2x,i2,2x,i6)' ) score, score_count(score)
      end do

      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Created data file "' // trim ( data_filename ) // '".'
c
c  Write the command file.
c
      command_filename = 'fair_dice_commands.txt'

      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot << ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "fair_dice.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<---Score--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Frequency--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Score frequency for a pair of fair dice"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style fill solid'
      write ( command_unit, '(a)' ) 'set yrange [0:*]'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a,i4,a)' ) 
     &  'plot "fair_dice_data.txt" using ' //
     &  '1:2:(0.90):xtic(3) with boxes'
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'FAIR_DICE_SIMULATION:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

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
      function i4_uniform ( a, b, seed )

c*********************************************************************72
c
cc I4_UNIFORM returns a scaled pseudorandom I4.
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
c    Peter Lewis, Allen Goodman, James Miller
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, integer I4_UNIFORM, a number between A and B.
c
      implicit none

      integer a
      integer b
      integer i4_uniform
      integer k
      real r
      integer seed
      integer value

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_UNIFORM - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
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

      i4_uniform = value

      return
      end
      function s_len_trim ( s )

c*********************************************************************72
c
cc S_LEN_TRIM returns the length of a string to the last nonblank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S, a string.
c
c    Output, integer S_LEN_TRIM, the length of the string to the last nonblank.
c
      implicit none

      integer i
      character*(*) s
      integer s_len_trim

      do i = len ( s ), 1, -1

        if ( s(i:i) .ne. ' ' ) then
          s_len_trim = i
          return
        end if

      end do

      s_len_trim = 0

      return
      end
      subroutine s_to_i4 ( s, ival, ierror, length )

c*********************************************************************72
c
cc S_TO_I4 reads an I4 from a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, a string to be examined.
c
c    Output, integer IVAL, the integer value read from the string.
c    If the string is blank, then IVAL will be returned 0.
c
c    Output, integer IERROR, an error flag.
c    0, no error.
c    1, an error occurred.
c
c    Output, integer LENGTH, the number of characters of S
c    used to make IVAL.
c
      implicit none

      character c
      integer i
      integer ierror
      integer isgn
      integer istate
      integer ival
      integer length
      character * ( * ) s
      integer s_len_trim

      ierror = 0
      istate = 0
      isgn = 1
      ival = 0

      do i = 1, s_len_trim ( s )

        c = s(i:i)
c
c  Haven't read anything.
c
        if ( istate .eq. 0 ) then

          if ( c .eq. ' ' ) then

          else if ( c .eq. '-' ) then
            istate = 1
            isgn = -1
          else if ( c .eq. '+' ) then
            istate = 1
            isgn = + 1
          else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            istate = 2
            ival = ichar ( c ) - ichar ( '0' )
          else
            ierror = 1
            return
          end if
c
c  Have read the sign, expecting digits.
c
        else if ( istate .eq. 1 ) then

          if ( c .eq. ' ' ) then

          else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            istate = 2
            ival = ichar ( c ) - ichar ( '0' )
          else
            ierror = 1
            return
          end if
c
c  Have read at least one digit, expecting more.
c
        else if ( istate .eq. 2 ) then

          if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            ival = 10 * ival + ichar ( c ) - ichar ( '0' )
          else
            ival = isgn * ival
            length = i - 1
            return
          end if

        end if

      end do
c
c  If we read all the characters in the string, see if we're OK.
c
      if ( istate .eq. 2 ) then
        ival = isgn * ival
        length = s_len_trim ( s )
      else
        ierror = 1
        length = 0
      end if

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
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
