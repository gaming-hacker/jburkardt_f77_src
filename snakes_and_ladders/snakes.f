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
      subroutine snakes ( a )

c*****************************************************************************80
c
cc SNAKES sets up the Snakes and Ladders matrix.
c
c  Discussion:
c
c    Snakes and Ladders, also known as Chutes and Ladders, is a game
c    played on a 10x10 board of 100 squares.  A player can be said to
c    start at square 0, that is, off the board.  The player repeatedly
c    rolls a die, and advances between 1 and 6 steps accordingly.
c    The game is won when the player reaches square 100.  In some versions,
c    the player must reach 100 by exact die count, forfeiting the move
c    if 100 is exceeded; in others, reaching or exceeding 100 counts as
c    a win.
c
c    Play is complicated by the existence of snakes and ladders.  On
c    landing on a square that begins a snake or ladder, the player is
c    immediately tranported to another square, which will be lower for
c    a snake, or higher for a ladder.
c
c    Typically, several players play, alternating turns.
c
c    Given a vector V(0:100) which is initially all zero except for the
c    first entry, the matrix-vector product A'*V represents the probabilities
c    that a player starting on square 0 will be on any given square after one
c    roll.  Correspondingly, (A')^2*V considers two moves, and so on.  Thus,
c    repeatedly multiplying by A' reveals the probability distribution 
c    associated with the likelihood of occupying any particular square at a 
c    given turn in the game.  
c
c    There is a single eigenvalue of value 1, whose corresponding eigenvector
c    is all zero except for a final entry of 1, representing a player who
c    has reached square 100.  All other eigenvalues have norm less than 1,
c    corresponding to the fact that there are no other long term steady
c    states or cycles in the game.
c
c    Note that no two descriptions of the Snakes and Ladders board seem to
c    agree.  This is the board described by Nick Berry.  The board described 
c    by Higham and Higham is close to this one, but differs in the description 
c    of two of the jumps.
c
c    While most commentators elect to move immediately from a snake mouth or
c    ladder foot, I have decide there are reasons to treat the game in such a
c    way that when you land on a ladder foot or snake mouth, you stay there
c    as though you had landed on an ordinary square; the difference arises on
c    your next turn, when, instead of rolling a die, you move up the ladder
c    or down the snake.  This allows the player to "register" a stop at the
c    given square, may be suitable for certain applications, and makes for
c    a transition matrix whose structure is more obvious to understand.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Steve Althoen, Larry King, Kenneth Schilling,
c    How long is a game of Snakes and Ladders?,
c    The Mathematical Gazette,
c    Volume 77, Number 478, March 1993, pages 71-76.
c
c    Nick Berry,
c    Mathematical Analysis of Chutes and Ladders,
c    http://www.datagenetics.com/blog/november12011/index.html
c
c    Desmond Higham, Nicholas Higham,
c    MATLAB Guide,
c    SIAM, 2005,
c    ISBN13: 9780898717891.
c
c  Parameters:
c
c    Output, double precision A(0:100,0:100), the matrix.
c
      implicit none

      double precision a(0:100,0:100)
      integer d
      integer i
      integer j
      integer j1
      integer j2
      integer jump(0:100)
      integer k

      do i = 0, 100
        jump(i) = i
      end do

      jump( 1) =  38
      jump( 4) =  14
      jump( 9) =  31
      jump(16) =   6
      jump(21) =  42
      jump(28) =  84
      jump(36) =  44
      jump(48) =  26
      jump(49) =  11
      jump(51) =  67
      jump(56) =  53
      jump(62) =  19
      jump(64) =  60
      jump(71) =  91
      jump(80) = 100
      jump(87) =  24
      jump(93) =  73
      jump(95) =  75
      jump(98) =  78

      do j = 0, 100
        do i = 0, 100
          a(i,j) = 0.0D+00
        end do
      end do
c
c  A(I,J) represents the probablity that a dice roll will take you from
c  square I to square J.
c
c  Starting in square I...
c
      do i = 0, 100
c
c  If I is a snake or ladder, go to the next spot.
c
        if ( i .ne. jump(i) ) then

          j = jump(i)
          a(i,j) = 1.0D+00
c
c  Otherwise, roll a die
c
        else

          do d = 1, 6
c
c  so theoretically, our new location J will be I + D,
c
            j = i + d
c
c  but if J is greater than 100, move us back to J,
c
            if ( 100 .lt. j ) then
              j = 100
            end if
  
            a(i,j) = a(i,j) + 1.0D+00 / 6.0D+00

          end do

        end if

      end do

      return
      end
      subroutine spy_ge ( m, n, a, header )

c*********************************************************************72
c
cc SPY_GE plots a sparsity pattern for a general storage (GE) matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns
c    in the matrix.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, character * ( * ) HEADER, the name to be used for the
c    title of the plot, and as part of the names of the data, command
c    and plot files.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      character * ( * ) header
      integer i
      integer j
      character * ( 6 ) m_s
      character * ( 6 ) n_s
      integer nz_num
      character * ( 255 ) png_filename
c
c  Create data file.
c
      data_filename = trim ( header ) // '_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      nz_num = 0
      do j = 1, n
        do i = 1, m
          if ( a(i,j) .ne. 0.0D+00 ) then
            write ( data_unit, '(2x,i6,2x,i6)' ) j, i
            nz_num = nz_num + 1
          end if
        end do
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )     '  Created sparsity data file "' 
     &  // trim ( data_filename ) // '".'
c
c  Create command file.
c
      command_filename = trim ( header ) // '_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // 
     &  trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set term png'

      png_filename = trim ( header ) // '.png'
      write ( command_unit, '(a)' ) 'set output "' // 
     &  trim ( png_filename ) // '"'
      write ( command_unit, '(a)' ) 'set size ratio -1'
      write ( command_unit, '(a)' ) 'set xlabel "<--- J --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- I --->"'

      write ( command_unit, '(a,i6,a)' )
     &  'set title "', nz_num, ' nonzeros for ''' // 
     &  trim ( header ) // '''"'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( n_s, '(i6)' ) n
      write ( m_s, '(i6)' ) m
      m_s = adjustl ( m_s )
      n_s = adjustl ( n_s )
      write ( command_unit, '(a)' )
     &  'plot [x=1:' // trim ( n_s) // '] [y=' // trim ( m_s ) // 
     &  ':1] "' //     trim ( data_filename ) //
     &  '" with points pt 5'

      close ( unit = command_unit )
      write ( *, '(a)' )     '  Created graphics command file "' // 
     &  trim ( command_filename ) // '".'

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
