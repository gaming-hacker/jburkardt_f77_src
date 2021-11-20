      program main

c*********************************************************************72
c
cc MAIN is the main program for HIGH_CARD_SIMULATION_PRB.
c
c  Discussion:
c
c    HIGH_CARD_SIMULATION_PRB tests the HIGH_CARD_SIMULATION library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HIGH_CARD_SIMULATION_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the HIGH_CARD_SIMULATION library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HIGH_CARD_SIMULATION_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 varies the skip number.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer deck_size
      integer i
      double precision p
      integer seed
      integer skip_num
      integer trial_num

      deck_size = 100
      trial_num = 100

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Estimate the chances of picking the high'
      write ( *, '(a)' ) 
     &  '  card by skipping a given number of initial cards,'
      write ( *, '(a,i4,a)' ) '  using a deck of ', deck_size, ' cards'
      write ( *, '(a,i4,a)' ) '  and simulating ', trial_num, ' trials.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Skip   Deck Size    Chance of Winning'
      write ( *, '(a)' ) ''

      seed = 123456789

      do i = 1, 10

        skip_num = 1 + ( ( i - 1 ) * deck_size ) / 10

        call high_card_simulation ( deck_size, skip_num, trial_num, 
     &    seed, p )

        write ( *, '(2x,i3,2x,i3,2x,g14.6)' ) skip_num, deck_size, p

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 plots the results for a deck of 100 cards.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer deck_size
      parameter ( deck_size = 100 )

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      integer i
      double precision p(deck_size)
      integer seed
      integer skip_num
      integer trial_num

      trial_num = 1000
      seed = 123456789

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Compute the changes of picking the high card'
      write ( *, '(a)' ) '  after skipping 0 through 99 cards,'
      write ( *, '(a,i4,a)' ) 
     &  '  using a deck with ', deck_size, ' cards'
      write ( *, '(a,i4,a)' ) '  and taking ', trial_num, ' trials.'

      do skip_num = 0, deck_size - 1

        call high_card_simulation ( deck_size, skip_num, trial_num, 
     &    seed, p(skip_num+1) )

      end do
c
c  Create data file.
c
      data_filename = 'test02_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, deck_size
        write ( data_unit, '(2x,g14.6,2x,g14.6)' ) i - 1, p(i)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )     '  Created graphics data file "' // 
     &  trim ( data_filename ) // '".'
c
c  Plot the data and the interpolant.
c
      command_filename = 'test02_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // 
     &  trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "test02.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- Skip --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- P(Correct) --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Estimated Probability of Correct Guess ' //
     &  'after Skipping K Cards"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'plot "' // 
     &  trim ( data_filename ) // '" using 1:2 lw 3 linecolor rgb "red"'

      close ( unit = command_unit )
      write ( *, '(a)' ) '  Created graphics command file "' // 
     &  trim ( command_filename ) // '".'

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 plots the results for a deck of 100 cards.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer deck_size
      parameter ( deck_size = 100 )

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      integer i
      double precision p(deck_size)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  HIGH_CARD_PROBABILITY computes the exact probability of '
      write ( *, '(a,i4,a)' ) 
     &  '  winning a high card game with a deck of', deck_size, ' cards'
      write ( *, '(a)' ) 
     &  '  assuming we skip first K-1 cards and select the next card'
      write ( *, '(a)' ) '  that is bigger.'

      call high_card_probability ( deck_size, p )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '    K   Prob(K)'
      write ( *, '(a)' ) ''
      do i = 1, deck_size
        write ( *, '(2x,i3,2x,f8.4)' ) i, p(i)
      end do
c
c  Create data file.
c
      data_filename = 'test03_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, status = 
     &  'replace' )
      do i = 1, deck_size
        write ( data_unit, '(2x,g14.6,2x,g14.6)' ) i - 1, p(i)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )     '  Created graphics data file "' // 
     &  trim ( data_filename ) // '".'
c
c  Plot the data and the interpolant.
c
      command_filename = 'test03_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // 
     &  trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "test03.png"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- Skip --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- P(Correct) --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Prob of Correct Guess after Skipping K Cards"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'plot "' // 
     &  trim ( data_filename ) // '" using 1:2 lw 3 linecolor rgb "red"'

      close ( unit = command_unit )
      write ( *, '(a)' ) '  Created graphics command file "' // 
     &  trim ( command_filename ) // '".'

      return
      end
