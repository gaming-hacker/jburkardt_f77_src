      program main

c*********************************************************************72
c
cc MAIN is the main program for MONTE_CARLO_RULE.
c
c  Discussion:
c
c    MONTE_CARLO_RULE generates N points in the M-dimensional unit hypercube,
c    and writes out files so that the data can be regarded as a quadrature rule.
c
c  Usage:
c
c    monte_carlo_rule m n seed
c
c    where
c
c    * M, the spatial dimension,
c    * N, the number of points to generate,
c    * SEED, the seed, a positive integer.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer arg_num
      integer iarg
      integer iargc
      integer ierror
      integer ios
      integer last
      integer m
      integer n
      integer s
      integer seed
      character * ( 255 ) string

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MONTE_CARLO_RULE'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Compute the abscissas and weights of a quadrature rule'
      write ( *, '(a)' ) '  that is simply a Monte Carlo sampling.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  The program requests input values from the user:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  * M, the spatial dimension,'
      write ( *, '(a)' ) '  * N, the number of points to generate,'
      write ( *, '(a)' ) '  * SEED, a positive integer.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Output from the program includes'
      write ( *, '(a)' ) 
     &  '  a set of 3 files that define the quadrature rule.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    (1) "mc_m?_n?_s?_r.txt", the ranges;'
      write ( *, '(a)' ) '    (2) "mc_m?_n?_s?_w.txt", the weights;'
      write ( *, '(a)' ) '    (3) "mc_m?_n?_s?_x.txt", the abscissas.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Get the spatial dimension M.
c
      if ( 1 .le. arg_num ) then
        iarg = 1
        call getarg ( iarg, string )
        call s_to_i4 ( string, m, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Enter the spatial dimension M (1 or greater)'
        read ( *, * ) m
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Spatial dimension M = ', m
c
c  Get the number of points N.
c
      if ( 2 .le. arg_num ) then
        iarg = 2
        call getarg ( iarg, string )
        call s_to_i4 ( string, n, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Enter the number of points N (1 or greater)'
        read ( *, * ) n
      end if

      write ( *, '(a,i8)' ) '  Number of points N = ', n
c
c  Get the seed, S
c
      if ( 3 .le. arg_num ) then
        iarg = 3
        call getarg ( iarg, string )
        call s_to_i4 ( string, s, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Enter the seed S (1 or greater)'
        read ( *, * ) s
      end if

      write ( *, '(a,i12)' ) '  Seed S = ', s
c
c  Call a subroutine which is able to allocate the arrays.
c
      call main_sub ( m, n, s )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MONTE_CARLO_RULE'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine main_sub ( m, n, s ) 

c*********************************************************************72
c
cc MAIN_SUB is a part of the main program with allocated memory.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    19 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, integer S, the random number seed.
c
      implicit none

      integer i
      integer ierror
      integer j
      integer m
      character * ( 3 ) m_string
      integer n
      character * ( 6 ) n_string
      double precision r(m,2)
      character * ( 255 ) r_filename
      integer s
      character * ( 9 ) s_string
      integer seed
      double precision w(n)
      character * ( 255 ) w_filename
      double precision x(m,n)
      character * ( 255 ) x_filename
c
c  Compute the data.
c
      do i = 1, m
        r(i,1) =  0.0D+00
        r(i,2) = +1.0D+00
      end do

      do j = 1, n
        w(j) = 1.0D+00 / dble ( n )
      end do

      seed = s
      call r8mat_uniform_01 ( m, n, seed, x )
c
c  Construct appropriate file names.
c
      write ( n_string, '(i6)' ) n
      write ( m_string, '(i3)' ) m
      write ( s_string, '(i9)' ) s

      r_filename = 'mc_d' // m_string // '_n' 
     &  // n_string // '_s' // s_string // '_r.txt'
      w_filename = 'mc_d' // m_string // '_n' 
     &  // n_string // '_s' // s_string // '_w.txt'
      x_filename = 'mc_d' // m_string // '_n' 
     &  // n_string // '_s' // s_string // '_x.txt'

      call s_blank_delete ( r_filename )
      call s_blank_delete ( w_filename )
      call s_blank_delete ( x_filename )
c
c  Write the rule to files.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Creating R file = "' // trim ( r_filename ) // '".'
      call r8mat_write ( r_filename, m, 2, r )
      write ( *, '(a)' ) 
     &  '  Creating W file = "' // trim ( w_filename ) // '".'
      call r8mat_write ( w_filename, 1, n, w )
      write ( *, '(a)' ) 
     &  '  Creating X file = "' // trim ( x_filename ) // '".'
      call r8mat_write ( x_filename, m, n, x )

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
      subroutine r8mat_uniform_01 ( m, n, seed, r )

c*********************************************************************72
c
cc R8MAT_UNIFORM_01 returns a unit pseudorandom R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
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
c    Input, integer M, N, the number of rows and columns in the array.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(M,N), the array of pseudorandom values.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      integer k
      integer seed
      double precision r(m,n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      do j = 1, n

        do i = 1, m

          k = seed / 127773

          seed = 16807 * ( seed - k * 127773 ) - k * 2836

          if ( seed .lt. 0 ) then
            seed = seed + 2147483647
          end if

          r(i,j) = dble ( seed ) * 4.656612875D-10

        end do
      end do

      return
      end
      subroutine r8mat_write ( output_filename, m, n, table )

c*********************************************************************72
c
cc R8MAT_WRITE writes a R8MAT file.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 October 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision TABLE(M,N), the data.
c
      implicit none

      integer m
      integer n

      integer j
      character * ( * ) output_filename
      integer output_unit
      character * ( 30 ) string
      double precision table(m,n)
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename,
     &  status = 'replace' )
c
c  Create the format string.
c
      if ( 0 .lt. m .and. 0 .lt. n ) then

        write ( string, '(a1,i8,a1,i8,a1,i8,a1)' )
     &    '(', m, 'g', 24, '.', 16, ')'
c
c  Write the data.
c
        do j = 1, n
          write ( output_unit, string ) table(1:m,j)
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

      return
      end
      subroutine s_blank_delete ( s )

c*********************************************************************72
c
cc S_BLANK_DELETE removes blanks from a string, left justifying the remainder.
c
c  Discussion:
c
c    All TAB characters are also removed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character*(*) S, the string to be transformed.
c
      implicit none

      character ch
      integer get
      integer put
      character*(*) s
      integer s_len_trim
      integer s_length
      character tab

      tab = char ( 9 )

      put = 0
      s_length = s_len_trim ( s )

      do get = 1, s_length

        ch = s(get:get)

        if ( ch .ne. ' ' .and. ch .ne. tab ) then
          put = put + 1
          s(put:put) = ch
        end if

      end do

      s(put+1:s_length) = ' '

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
