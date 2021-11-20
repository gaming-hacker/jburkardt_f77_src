      program main

c*********************************************************************72
c
cc MAIN is the main program for MD.
c
c  Discussion:
c
c    MD implements a simple molecular dynamics simulation.
c
c    The velocity Verlet time integration scheme is used. 
c
c    The particles interact with a central pair potential.
c
c    Based on a FORTRAN90 program by Bill Magro.
c
c  Usage:
c
c    md nd np step_num dt
c
c    where
c
c    * nd is the spatial dimension (2 or 3);
c    * np is the number of particles (500, for instance);
c    * step_num is the number of time steps (500, for instance).
c    * dt is the time step (0.1 for instance )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 December 2014
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    None
c
      implicit none

      integer arg_num
      double precision ctime
      double precision ctime1
      double precision ctime2
      double precision dt
      integer i
      integer iarg
      integer iargc
      integer ierror
      integer last
      integer nd
      integer np
      integer step_num
      character * ( 255 ) string

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MD'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  A molecular dynamics program.'
c
c  Get the number of command line arguments.
c
      arg_num = iargc ( )
c
c  Get ND, the number of spatial dimensions.
c
      if ( 1 .le. arg_num ) then
        iarg = 1
        call getarg ( iarg, string )
        call s_to_i4 ( string, nd, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Enter ND, spatial dimension (2 or 3 ):'
        read ( *, * ) nd
      end if
c
c  Get NP, the number of particles.
c
      if ( 2 .le. arg_num ) then
        iarg = 2
        call getarg ( iarg, string )
        call s_to_i4 ( string, np, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Enter NP, the number of particles (500, for instance):'
        read ( *, * ) np
      end if
c
c  Get STEP_NUM, the number of time steps.
c
      if ( 3 .le. arg_num ) then
        iarg = 3
        call getarg ( iarg, string )
        call s_to_i4 ( string, step_num, ierror, last )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Enter STEP_NUM, number of time steps (500, for instance):'
        read ( *, * ) step_num
      end if
c
c  Get DT, the time step.
c
      if ( 4 .le. arg_num ) then
        iarg = 4
        call getarg ( iarg, string )
        call s_to_r8 ( string, dt  )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  Enter DT, the time step size (0.1, for instance):'
        read ( *, * ) dt
      end if
c
c  Report.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  NP, the number of particles in the simulation, is ', np
      write ( *, '(a,i8)' ) 
     &  '  ND, the spatial dimension, is ', nd
      write ( *, '(a,i8)' ) 
     &  '  STEP_NUM, the number of time steps, is ', step_num
      write ( *, '(a,g14.6)' ) 
     &  '  DT, the size of each time step, is ', dt
c
c  Do computation.
c
      call cpu_time ( ctime1 )

      call main_sub ( nd, np, step_num, dt )

      call cpu_time ( ctime2 )
      ctime = ctime2 - ctime1 

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Elapsed cpu time for main computation:'
      write ( *, '(2x,g14.6,a)' ) ctime, ' seconds'
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MD:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine main_sub ( nd, np, step_num, dt )

c*********************************************************************72
c
cc MAIN_SUB is the rest of the main program.
c
c  Discussion:
c
c    We need to break the main program into two pieces to get around the
c    fact that FORTRAN77 doesn't know how to allocate memory.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    26 December 2014
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer ND, the number of spatial dimensions.
c
c    Input, integer NP, the number of particles.
c
c    Input, integer STEP_NUM, the number of time steps to take.
c
c    Input, double precision DT, the size of each time step.
c
      implicit none

      integer nd
      integer np

      double precision acc(nd,np)
      integer arg_num
      double precision dt
      double precision e0
      double precision force(nd,np)
      integer i
      integer id
      double precision kinetic
      double precision mass
      parameter ( mass = 1.0D+00 )
      double precision pos(nd,np)
      double precision potential
      double precision rel
      integer step
      integer step_num
      integer step_print
      integer step_print_index
      integer step_print_num
      double precision vel(nd,np)
c
c  This is the main time stepping loop.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  At each step, we report the potential and kinetic energies.'
      write ( *, '(a)' ) 
     &  '  The sum of these energies should be a constant.'
      write ( *, '(a)' ) 
     &  '  As an accuracy check, we also print the relative error'
      write ( *, '(a)' ) '  in the total energy.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      Step      Potential       Kinetic        (P+K-E0)/E0'
      write ( *, '(a)' ) 
     &  '                Energy P        Energy K       ' //
     &  'Relative Energy Error'
      write ( *, '(a)' ) ' '

      step_print = 0
      step_print_index = 0
      step_print_num = 10

      do step = 0, step_num

        if ( step .eq. 0 ) then
          call initialize ( np, nd, pos, vel, acc )
        else
          call update ( np, nd, pos, vel, force, acc, mass, dt )
        end if

        call compute ( np, nd, pos, vel, mass, force, potential, 
     &    kinetic )

        if ( step .eq. 0 ) then
          e0 = potential + kinetic
        end if

        if ( step .eq. step_print ) then
          rel = ( potential + kinetic - e0 ) / e0
          write ( *, '(2x,i8,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      step, potential, kinetic, rel
          step_print_index = step_print_index + 1
          step_print = ( step_print_index * step_num ) / step_print_num
        end if

      end do

      return
      end
      subroutine compute ( np, nd, pos, vel, mass, f, pot, kin )

c*********************************************************************72
c
cc COMPUTE computes the forces and energies.
c
c  Discussion:
c
c    The computation of forces and energies is fully parallel.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 November 2007
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer NP, the number of particles.
c
c    Input, integer ND, the number of spatial dimensions.
c
c    Input, double precision POS(ND,NP), the positions.
c
c    Input, double precision VEL(ND,NP), the velocitiese.
c
c    Input, double precision MASS, the mass.
c
      implicit none

      integer np
      integer nd

      double precision d
      double precision f(nd,np)
      integer i
      integer j
      integer k
      double precision kin
      double precision mass
      double precision PI2
      parameter ( PI2 = 3.141592653589793D+00 / 2.0D+00 )
      double precision pos(nd,np)
      double precision pot
      double precision rij(nd)
      double precision vel(nd,np)
      double precision x

      pot = 0.0D+00
      kin = 0.0D+00

      do i = 1, np
c
c  Compute the potential energy and forces.
c
        do k = 1, nd
          f(k,i) = 0.0D+00
        end do

        do j = 1, np

          if ( i .ne. j ) then

            call dist ( nd, pos(1,i), pos(1,j), rij, d )
c
c  Attribute half of the potential energy to particle J.
c
            pot = pot + 0.5D+00 * ( sin ( min ( d, PI2 ) ) )**2

            do k = 1, nd
              f(k,i) = f(k,i) - rij(k) * 2.0D+00 
     &          * sin ( min ( d, PI2 ) ) * cos ( min ( d, PI2 ) ) / d
            end do

          end if

        end do
c
c  Compute the kinetic energy.
c
        do k = 1, nd
          kin = kin + vel(k,i) * vel(k,i)
        end do

      end do

      kin = kin * 0.5D+00 * mass
      
      return
      end
      subroutine dist ( nd, r1, r2, dr, d )

c*********************************************************************72
c
cc DIST computes the displacement (and its norm) between two particles.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 November 2007
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer ND, the number of spatial dimensions.
c
c    Input, double precision R1(ND), R2(ND), the positions of the particles.
c
c    Output, double precision DR(ND), the displacement vector.
c
c    Output, double precision D, the Euclidean norm of the displacement.
c
      implicit none

      integer nd

      double precision d
      double precision dr(nd)
      integer i
      double precision r1(nd)
      double precision r2(nd)

      do i = 1, nd
        dr(i) = r1(i) - r2(i)
      end do

      d = 0.0D+00
      do i = 1, nd
        d = d + dr(i) * dr(i)
      end do
      d = sqrt ( d )

      return
      end
      subroutine initialize ( np, nd, pos, vel, acc )

c*********************************************************************72
c
cc INITIALIZE initializes the positions, velocities, and accelerations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    26 December 2014
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer NP, the number of particles.
c
c    Input, integer ND, the number of spatial dimensions.
c
c    Output, double precision POS(ND,NP), the position of each particle.
c
c    Output, double precision VEL(ND,NP), the velocity of each particle.
c
c    Output, double precision ACC(ND,NP), the acceleration of each particle.
c
      implicit none

      integer np
      integer nd

      double precision acc(nd,np)
      integer i
      integer j
      double precision pos(nd,np)
      integer seed
      double precision vel(nd,np)
c
c  Set the positions.
c
      seed = 123456789
      call r8mat_uniform_ab ( nd, np, 0.0D+00, 10.0D+00, seed, pos )
c
c  Set the velocities.
c
      do j = 1, np
        do i = 1, nd
          vel(i,j) = 0.0D+00
        end do
      end do
c
c  Set the velocities.
c
      do j = 1, np
        do i = 1, nd
          acc(i,j) = 0.0D+00
        end do
      end do

      return
      end
      subroutine r8mat_uniform_ab ( m, n, a, b, seed, r )

c*********************************************************************72
c
cc R8MAT_UNIFORM_AB returns a scaled pseudorandom R8MAT.
c
c  Discussion:
c
c    A <= R(I,J) <= B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    05 February 2005
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
c    Input, integer M, N, the number of rows and columns in the array.
c
c    Input, double precision A, B, the lower and upper limits.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(M,N), the array of pseudorandom values.
c
      implicit none

      integer m
      integer n

      double precision a
      double precision b
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer j
      integer k
      integer seed
      double precision r(m,n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_AB - Fatal error!'
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

          r(i,j) = a + ( b - a ) * dble ( seed ) * 4.656612875D-10

        end do
      end do

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
      subroutine s_to_r8 ( s, r8 )

c*********************************************************************72
c
cc S_TO_R8 reads an R8 value from a string.
c
c  Discussion:
c
c    An "R8" value is simply a real number to be stored as a
c    variable of type "double precision".
c
c    The routine will read as many characters as possible until it reaches
c    the end of the string, or encounters a character which cannot be
c    part of the number.
c
c    Legal input is:
c
c       1 blanks,
c       2 '+' or '-' sign,
c       2.5 blanks
c       3 integer part,
c       4 decimal point,
c       5 fraction part,
c       6 'E' or 'e' or 'D' or 'd', exponent marker,
c       7 exponent sign,
c       8 exponent integer part,
c       9 exponent decimal point,
c      10 exponent fraction part,
c      11 blanks,
c      12 final comma or semicolon,
c
c    with most quantities optional.
c
c  Example:
c
c    S                 R8
c
c    '1'               1.0
c    '     1   '       1.0
c    '1A'              1.0
c    '12,34,56'        12.0
c    '  34 7'          34.0
c    '-1E2ABCD'        -100.0
c    '-1X2ABCD'        -1.0
c    ' 2E-1'           0.2
c    '23.45'           23.45
c    '-4.2E+2'         -420.0
c    '17d2'            1700.0
c    '-14e-2'         -0.14
c    'e2'              100.0
c    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string containing the
c    data to be read.  Reading will begin at position 1 and
c    terminate at the end of the string, or when no more
c    characters can be read to form a legal real.  Blanks,
c    commas, or other nonnumeric data will, in particular,
c    cause the conversion to halt.
c
c    Output, double precision R8, the value read from the string.
c
      implicit none

      character c
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer length
      integer ndig
      double precision r8
      double precision rbot
      double precision rexp
      double precision rtop
      character * ( * ) s
      integer s_length
      character TAB
      parameter ( TAB = char ( 9 ) )

      s_length = len_trim ( s )

      ierror = 0
      r8 = 0.0D+00
      length = -1
      isgn = 1
      rtop = 0
      rbot = 1
      jsgn = 1
      jtop = 0
      jbot = 1
      ihave = 1
      iterm = 0

10    continue

        length = length + 1

        if ( s_length .lt. length + 1 ) then
          go to 20
        end if

        c = s(length+1:length+1)
c
c  Blank character.
c
        if ( c .eq. ' ' .or. c .eq. TAB ) then

          if ( ihave .eq. 2 ) then

          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            iterm = 1
          else if ( 1 < ihave ) then
            ihave = 11
          end if
c
c  Comma.
c
        else if ( c .eq. ',' .or. c .eq. ';' ) then

          if ( ihave .ne. 1 ) then
            iterm = 1
            ihave = 12
            length = length + 1
          end if
c
c  Minus sign.
c
        else if ( c .eq. '-' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
            isgn = -1
          else if ( ihave .eq. 6 ) then
            ihave = 7
            jsgn = -1
          else
            iterm = 1
          end if
c
c  Plus sign.
c
        else if ( c .eq. '+' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
          else if ( ihave .eq. 6 ) then
            ihave = 7
          else
            iterm = 1
          end if
c
c  Decimal point.
c
        else if ( c .eq. '.' ) then

          if ( ihave .lt. 4 ) then
            ihave = 4
          else if ( 6 .le. ihave .and. ihave .le. 8 ) then
            ihave = 9
          else
            iterm = 1
          end if
c
c  Scientific notation exponent marker.
c
        else if ( c .eq. 'E' .or. c .eq. 'e' .or. 
     &            c .eq. 'D' .or. c .eq. 'd' ) then

          if ( ihave .lt. 6 ) then
            ihave = 6
          else
            iterm = 1
          end if
c
c  Digit.
c
        else if (  ihave .lt. 11 .and. lle ( '0', c ) 
     &    .and. lle ( c, '9' ) ) then

          if ( ihave .le. 2 ) then
            ihave = 3
          else if ( ihave .eq. 4 ) then
            ihave = 5
          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            ihave = 8
          else if ( ihave .eq. 9 ) then
            ihave = 10
          end if

          ndig = ichar ( c ) - 48

          if ( ihave .eq. 3 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
          else if ( ihave .eq. 5 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
            rbot = 10.0D+00 * rbot
          else if ( ihave .eq. 8 ) then
            jtop = 10 * jtop + ndig
          else if ( ihave .eq. 10 ) then
            jtop = 10 * jtop + ndig
            jbot = 10 * jbot
          end if
c
c  Anything else is regarded as a terminator.
c
        else
          iterm = 1
        end if
c
c  If we haven't seen a terminator, and we haven't examined the
c  entire string, go get the next character.
c
        if ( iterm .eq. 1 ) then
          go to 20
        end if

      go to 10

20    continue
c
c  If we haven't seen a terminator, and we have examined the
c  entire string, then we're done, and LENGTH is equal to S_LENGTH.
c
      if ( iterm .ne. 1 .and. length + 1 .eq. s_length ) then
        length = s_length
      end if
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7!
c
      if ( ihave .eq. 1 .or. ihave .eq. 2 .or. ihave .eq. 6 .or. 
     &  ihave .eq. 7 ) then
        ierror = ihave
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_TO_R8 - Serious errorc'
        write ( *, '(a)' ) '  Illegal or nonnumeric input:'
        write ( *, '(a)' ) '    "' // trim ( s ) // '"'
        stop 1
      end if
c
c  Number seems OK.  Form it.
c
      if ( jtop .eq. 0 ) then
        rexp = 1.0D+00
      else
        if ( jbot .eq. 1 ) then
          rexp = 10.0D+00 ** ( jsgn * jtop )
        else
          rexp = 10.0D+00 ** ( dble ( jsgn * jtop ) / dble ( jbot ) )
        end if
      end if

      r8 = dble ( isgn ) * rexp * rtop / rbot

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
      subroutine update ( np, nd, pos, vel, f, acc, mass, dt )

c*********************************************************************72
c
cc UPDATE performs the time integration, using a velocity Verlet algorithm.
c
c  Discussion:
c
c    The time integration is fully parallel.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 November 2007
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer NP, the number of particles.
c
c    Input, integer ND, the number of spatial dimensions.
c
c    Input/output, double precision POS(ND,NP), the positions.
c
c    Input/output, double precision VEL(ND,NP), the velocities.
c
c    Input, double precision MASS, the mass.
c
c    Input/output, double precision ACC(ND,NP), the accelerations.
c
      implicit none

      integer np
      integer nd

      double precision acc(nd,np)
      double precision dt
      double precision f(nd,np)
      integer i
      integer j
      double precision mass
      double precision pos(nd,np)
      double precision rmass
      double precision vel(nd,np)

      rmass = 1.0D+00 / mass

      do j = 1, np
        do i = 1, nd

          pos(i,j) = pos(i,j) 
     &      + vel(i,j) * dt + 0.5D+00 * acc(i,j) * dt * dt

          vel(i,j) = vel(i,j) 
     &      + 0.5D+00 * dt * ( f(i,j) * rmass + acc(i,j) )

          acc(i,j) = f(i,j) * rmass

        end do
      end do

      return
      end
