      program main

c*********************************************************************72
c
cc MAIN is the main program for LATIN_RANDOM_DATASET.
c
c  Discussion:
c
c    LATIN_RANDOM_DATASET generates and saves a Latin Random Square dataset.
c
c    A "Latin Random Square" dataset is created by dividing each side
c    of the unit hypercube into N subintervals, and then choosing
c    a set of N subcubes.  The subcubes are selected in such a way
c    that if we project the subcubes onto any coordinate direction,
c    there is exactly one subcube in each subinterval.
c
c    That's the "Latin Square" part of the dataset.
c
c    Then we choose, uniformly at random, one point within each subcube
c    as a "representative".  This set of points constitutes the
c    "Latin Random Square" dataset, a terminology I made up to
c    distinguish it from the "Latin Random Square" and "Latin Edge Square"
c    datasets.
c
c  Usage:
c
c    latin_random_dataset m n seed
c
c    where
c
c    * M, the spatial dimension,
c    * N, the number of points to generate,
c    * SEED, the seed, a positive integer.
c
c    creates an M by N dataset and writes it to the
c    file "latin_random_M_N.txt".
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 November 2014
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
      integer seed
      character * ( 255 ) string

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LATIN_RANDOM_DATASET'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Generate a Latin Random Square dataset.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Get the spatial dimension M.
c
      if ( 1 <= arg_num ) then
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
      if ( 2 <= arg_num ) then
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
c  Get the seed, SEED
c
      if ( 3 <= arg_num ) then
        iarg = 3
        call getarg ( iarg, string )
        call s_to_i4 ( string, seed, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Enter the seed SEED (1 or greater)'
        read ( *, * ) seed
      end if

      write ( *, '(a,i12)' ) '  Seed SEED = ', seed

      if ( seed == 0 ) then
        call get_seed ( seed )
        write ( *, '(a,i12)' ) '  Randomized SEED = ', seed
      end if

      call main_sub ( m, n, seed )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LATIN_RANDOM_DATASET'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine main_sub ( m, n, seed )

c*********************************************************************72
c
cc MAIN_SUB is an auxilliary routine for the main program.
c
c  Discussion:
c
c    FORTRAN77 didn't allow for allocatable arrays.  This subroutine
c    may help us get around that problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 November 2014
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
c    Input/output, integer SEED, a seed for the random number generator.
c
      implicit none

      integer m
      integer n

      character * ( 255 ) output_filename
      double precision r(m,n)
      integer seed
c
c  Compute the data
c
      call latin_random ( m, n, seed, r )
c
c  Write it to a file.
c
      write ( output_filename, '(a,i2.2,a,i5.5,a)' ) 
     &  'latin_random_', m, '_', n, '.txt'

      call r8mat_write ( output_filename, m, n, r )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The data was written to "' // 
     &  trim ( output_filename ) // '".'

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
        write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal errorc'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
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
      subroutine latin_random ( dim_num, point_num, seed, x )

c*********************************************************************72
c
cc LATIN_RANDOM returns points in a Latin Random square.
c
c  Discussion:
c
c    In each spatial dimension, there will be exactly one
c    point whose coordinate value lies between consecutive
c    values in the list:
c
c      ( 0, 1, 2, ..., point_num ) / point_num
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer POINT_NUM, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(DIM_NUM,POINT_NUM), the points.
c
      implicit none

      integer dim_num
      integer point_num

      integer i
      integer j
      integer perm(point_num)
      integer seed
      double precision x(dim_num,point_num)
c
c  Pick DIM_NUM * POINT_NUM random numbers between 0 and 1.
c
      call r8mat_uniform_01 ( dim_num, point_num, seed, x )
c
c  For spatial dimension I,
c    pick a random permutation of 1 to POINT_NUM,
c    force the corresponding I-th components of X to lie in the
c    interval ( PERM(J)-1, PERM(J) ) / POINT_NUM.
c
      do i = 1, dim_num

        call perm_uniform ( point_num, seed, perm )

        do j = 1, point_num
          x(i,j) = ( dble ( perm(j) - 1 ) + x(i,j) ) 
     &             / dble ( point_num )
        end do

      end do

      return
      end
      subroutine perm_uniform ( n, seed, p )

c*********************************************************************72
c
cc PERM_UNIFORM selects a random permutation of N objects.
c
c  Discussion:
c
c    The routine assumes the objects are labeled 1, 2, ... N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms for Computers and Calculators,
c    Academic Press, 1978,
c    ISBN: 0-12-519260-6,
c    LC: QA164.N54.
c
c  Parameters:
c
c    Input, integer N, the number of objects to be permuted.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, integer P(N), a permutation of ( 1, 2, ..., N ), in standard
c    index form.
c
      implicit none

      integer n

      integer i
      integer i4_uniform_ab
      integer j
      integer p(n)
      integer pk
      integer seed

      do i = 1, n
        p(i) = i
      end do

      do i = 1, n - 1
        j = i4_uniform_ab ( i, n, seed )
        pk = p(i)
        p(i) = p(j)
        p(j) = pk
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
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer j
      integer k
      integer seed
      double precision r(m,n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_01 - Fatal errorc'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do j = 1, n

        do i = 1, m

          k = seed / 127773

          seed = 16807 * ( seed - k * 127773 ) - k * 2836

          if ( seed .lt. 0 ) then
            seed = seed + i4_huge
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
      integer s_len

      ierror = 0
      istate = 0
      isgn = 1
      ival = 0 
      s_len = len_trim ( s )

      do i = 1, s_len

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
        length = len_trim ( s )
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
