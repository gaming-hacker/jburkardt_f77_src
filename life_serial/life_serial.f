      program main

c*********************************************************************72
c
cc MAIN is the main program for LIFE_SERIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Martin Gardner,
c    Mathematical Games:
c    The Fantastic Combinations of John Conway's new solitaire game "Life",
c    Scientific American,
c    Volume 223, Number 4, October 1970, pages 120-123.
c
      implicit none

      integer m
      parameter ( m = 10 )
      integer n
      parameter ( n = 10 )

      character * ( 80 ) filename
      integer it
      integer it_max
      integer grid(1+m+1,1+n+1)
      double precision prob
      integer seed

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LIFE_SERIAL'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Carry out a few steps of John Conway''s'
      write ( *, '(a)' ) '  Game of Life.'
      write ( *, '(a)' ) ''

      filename = 'life_000.txt'
      it_max = 10
      prob = 0.20D+00
      seed = 123456789

      do it = 0, it_max
        if ( it .eq. 0 ) then
          call life_init ( prob, m, n, seed, grid )
        else
          call life_update ( m, n, grid )
        end if
        call life_write ( filename, m, n, grid )
        write ( *, '(2x,a)' ) trim ( filename )
        call filename_inc ( filename )
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LIFE_SERIAL'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine filename_inc ( filename )

c*********************************************************************72
c
cc FILENAME_INC increments a partially numeric filename.
c
c  Discussion:
c
c    It is assumed that the digits in the name, whether scattered or
c    connected, represent a number that is to be increased by 1 on
c    each call.  Non-numeric letters of the name are unaffected.
c
c    If the name is empty, then the routine stops.
c
c    If the name contains no digits, the empty string is returned.
c
c  Example:
c
c      Input          Output
c      -----          ------
c      a7to11.txt     a7to12.txt
c      a7to99.txt     a8to00.txt
c      a9to99.txt     a0to00.txt
c      cat.txt        ' '
c      ' '            STOP!
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character * ( * ) FILENAME.
c    On input, a character string to be incremented.
c    On output, the incremented string.
c
      implicit none

      character c
      integer change
      integer digit
      character * ( * ) filename
      integer i
      integer lens

      lens = len_trim ( filename )

      if ( lens .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILENAME_INC - Fatal error!'
        write ( *, '(a)' ) '  The input string is empty.'
        stop
      end if

      change = 0

      do i = lens, 1, -1

        c = filename(i:i)

        if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

          change = change + 1

          digit = ichar ( c ) - 48
          digit = digit + 1

          if ( digit .eq. 10 ) then
            digit = 0
          end if

          c = char ( digit + 48 )

          filename(i:i) = c

          if ( c .ne. '0' ) then
            return
          end if

        end if

      end do
c
c  No digits were found.  Return blank.
c
      if ( change .eq. 0 ) then
        filename = ' '
        return
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
      subroutine life_init ( prob, m, n, seed, grid )

c*********************************************************************72
c
cc LIFE_INIT initializes the life grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision PROB, the probability that a grid cell
c    should be alive.
c
c    Input, integer M, N, the number of rows and columns
c    of interior grid cells.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer GRID(1+M+1,1+N+1), the initial grid.
c
      implicit none

      integer m
      integer n

      integer grid(0:m+1,0:n+1)
      integer i
      integer j
      double precision prob
      double precision r
      double precision r8_uniform_01
      integer seed

      do j = 0, n + 1
        do i = 0, m + 1
          grid(i,j) = 0
        end do
      end do

      do j = 1, n
       do i = 1, m
          r = r8_uniform_01 ( seed )
          if ( r .le. prob ) then
            grid(i,j) = 1
          end if
        end do
      end do

      return
      end
      subroutine life_update (  m, n, grid )

c*********************************************************************72
c
cc LIFE_UPDATE updates a Life grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns
c    of interior grid cells.
c
c    Input/output, integer GRID(1+M+1,1+N+1), the data.
c
      implicit none

      integer m
      integer n

      integer grid(0:m+1,0:n+1)
      integer i
      integer j
      integer s(1:m,1:n)

      do j = 1, n
        do i = 1, m

          s(i,j) = grid(i-1,j-1) + grid(i-1,j) + grid(i-1,j+1)
     &           + grid(i,  j-1)               + grid(i,  j+1)
     &           + grid(i+1,j-1) + grid(i+1,j) + grid(i+1,j+1)

        end do
      end do
c
c  Any dead cell with 3 live neighbors becomes alive.
c  Any living cell with less than 2 or more than 3 neighbors dies.
c
      do j = 1, n
        do i = 1, m

          if ( grid(i,j) .eq. 0 ) then
            if ( s(i,j) .eq. 3 ) then
              grid(i,j) = 1
            end if
          else if ( grid(i,j) .eq. 1 ) then
            if ( s(i,j) .lt. 2 .or. 3 .lt. s(i,j) ) then
              grid(i,j) = 0
            end if
          end if

        end do
      end do

      return
      end
      subroutine life_write ( output_filename, m, n, grid )

c*********************************************************************72
c
cc LIFE_WRITE writes a grid to a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer M, N, the number of rows and columns
c    of interior grid cells.
c
c    Input, integer GRID(1+M+1,1+N+1), the data.
c
      implicit none

      integer m
      integer n

      integer grid(0:m+1,0:n+1)
      integer i
      integer j
      character * ( * ) output_filename
      integer output_unit
      character * ( 30 ) string
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename, 
     &  status = 'replace' )
c
c  Create a format string.
c
      if ( 0 .lt. m .and. 0 .lt. n ) then

        write ( string, '(a1,i8,a8)' ) '(', m+2, '(1x,i1))'
c
c  Write the data.
c
        do j = 0, n + 1
          write ( output_unit, string ) ( grid(i,j), i = 0, m + 1 )
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

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
