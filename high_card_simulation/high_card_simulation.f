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
      subroutine high_card_probability ( n, p )

c*********************************************************************72
c
cc HIGH_CARD_PROBABILITY: winning probabilities for the high card game.
c
c  Discussion:
c
c    The high card game presents the player with a deck of cards, each
c    having an unknown value.  The player is allowed to go throught the
c    deck once, looking at the cards one at a time.  At any time, the player
c    may decide to take a particular card, winning that amount and stopping
c    the game.  If the player continues to the end, by default the last card
c    indicates the amount won.
c
c    An optimal strategy for selecting the highest card is as follows:
c    * look at, but do not select, the first k-1 cards;
c    * stop at the first card, from k to n, that is higher than the first
c      k-1 cards.
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
c  Parameters:
c
c    Input, integer N, the number of cards.
c
c    Output, double precision P(N).  P(K) is the probability that a strategy 
c    that skips K-1 cards will win, given that the deck has N cards.
c
      implicit none

      integer n

      integer i
      integer j
      double precision p(n)
      double precision t

      do i = 1, n
        t = 0.0D+00
        do j = i, n - 1
          t = t + 1.0D+00 / dble ( j )
        end do
        p(i) = ( 1.0D+00 + dble ( i - 1 ) * t ) / dble ( n )
      end do

      return
      end
      subroutine high_card_shuffle ( n, seed, sequence )

c*********************************************************************72
c
cc HIGH_CARD_SHUFFLE generates a sequence of numeric "cards" for a game.
c
c  Discussion:
c
c    In this game, you know that the deck contains N cards.  You win by
c    choosing the highest card in the deck.  You don't know what this card
c    is, and you must choose your card by saying "stop" as, one by one,
c    the cards of the deck are exposed.
c
c    A random guesser would get the high card with probability 1/N.
c
c    An intelligent guesser can do much better.
c
c    It is the goal of this program so "shuffle" a deck of cards suitable
c    for this game.  The problem is that we know the highest card in an
c    ordinary deck.  Let's replace the cards by integers.  Then if we know
c    in advance the range of the cards (say, they must lie between 1 and
c    1,000), it may be true that we can guess the card that is the maximum.
c
c    However, this program produces a sequence of integer card values for
c    which no information can be gained from the values.  It does this
c    by regarding the card values as binary integers between 1 and 2^N - 1.
c    We can make a perfectly information-free sequence as follows:
c
c      Card 1 sets bit N-1 to 1.
c      Card 2 sets bit N-2 to 1, bit  N-1 randomly.
c      ...
c      Card I sets bit N-I to 1, bits N-1 down to N-I+1 randomly.
c      ...
c      Card N sets bit N-N to 1, bits N-1 down to 1 randomly.
c
c    The I-th card has equal probability to fall in any of the I intervals
c    defined by the I-1 previous cards.  So, knowing the previous cards tells
c    you absolutely nothing about where the next card will fall, and each
c    card is, at the moment you see it, as good a guess for the maximum as
c    the unseen cards.
c
c    For example, the command "high_card_shuffle(7)" might yield
c
c      64    96    80     8     4    82    29
c    or
c      64    32    16    24    12    58    73
c    or
c      64    96    48     8   100    26    93
c    or
c      64    96    16    56    76   122    71
c
c    in which the highest card is #2, #7, #5, or #6 respectively.
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
c  Parameters:
c
c    Input, integer N, the number of cards.  N probably needs to
c    be less than 32.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer SEQUENCE(N), a set of N integer values
c    that can be used as the cards in the high card guessing game.
c
      implicit none

      integer n

      integer c
      integer i
      integer i4_uniform_ab
      integer j
      integer k
      integer seed
      integer sequence(n)

      if ( 32 .le. n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HIGH_CARD_SHUFFLE - Fatal error!'
        write ( *, '(a)' ) '  This program can only handle N < 32.'
        stop 1
      end if

      do i = 1, n
        c = 2 ** ( n - i )
        do j = 1, i - 1
          k = i4_uniform_ab ( 0, 1, seed )
          c = c + k * 2 ** ( n - i + j )
        end do
        sequence(i) = c
      end do

      return
      end
      subroutine high_card_simulation ( deck_size, skip_num, trial_num, 
     &  seed, p )

c*********************************************************************72
c
cc HIGH_CARD_SIMULATION simulates a game of choosing the highest card in a deck.
c
c  Discussion:
c
c    You are given a deck of DECK_SIZE cards.
c
c    Your goal is to select the high card.  For convenience, we can assume
c    the cards are a permutation of the integers from 1 to DECK_SIZE, but in
c    fact the user mustn't see such values or else it's obvious which is the
c    largest card.
c
c    However, your choice is made under the following rules:  You may turn over
c    one card at a time.  When a card is turned over, you may declare that to be
c    your choice, or else turn over another card.  If you have not chosen a card
c    by the end, then your choice is the final card.
c
c    If you have no idea what to do, and simply decide in advance to pick
c    a card "at random", that is, for example, you decide to pick the 15th card
c    before having seen any cards, then your probability of winning is
c    1/DECK_SIZE.
c
c    The question is, can you do better than that?
c
c    Your strategy is as follows: always look at the first SKIP_NUM cards
c    without choosing them.  Then choose the very next card you encounter
c    that is larger than the cards you skipped.
c
c    Using this program, you can easily see that skipping 5 cards is much better
c    than picking one at random, skipping 10 is even better, and so on.
c    Of course, you can't skip too many cards, and in fact, the results seem
c    to be best for somewhere around 30 to 35 cards skipped.  For problems
c    like this, the optimal value is somewhere around 1 / e, where E is the
c    base of the natural logarithm system.
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
c  Parameters:
c
c    Input, integer DECK_SIZE, the number of cards in the deck.
c    2 <= DECK_SIZE.  Default value is 52;
c
c    Input, integer SKIP_NUM, the number of initial cards you plan
c    to examine but will NOT select.  If SKIP_NUM is 0, you don't look at any
c    cards first.  0 <= SKIP_NUM < DECK_SIZE.
c
c    Input, integer TRIAL_NUM, the number of times we will
c    simulate this process.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision P, the estimated probability that your strategy
c    of skipping SKIP_NUM cards and then selecting the next card that is
c    bigger, will result in choosing the highest card.
c
      implicit none

      integer deck_size
      integer n

      integer card
      integer cards(deck_size)
      integer choice
      integer correct
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      double precision p
      integer seed
      integer skip_max
      integer skip_num
      integer trial
      integer trial_num
      integer true_max
c
c  Check values.
c
      if ( deck_size .lt. 2 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'HIGH_CARD_SIMULATION - Fatal error!'
        write ( *, '(a)' ) '  DECK_SIZE must be at least 2.'
        write ( *, '(a,i6)' ) '  Your value was ', deck_size
        stop 1
      end if

      if ( skip_num .lt. 0 ) then
        skip_num = 0
      end if

      if ( deck_size .le. skip_num ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'HIGH_CARD_SIMULATION - Fatal error!'
        write ( *, '(a)' ) '  SKIP_NUM must be less than DECK_SIZE.'
        write ( *, '(a,i6)' ) '  Your DECK_SIZE = ', deck_size
        write ( *, '(a,i6)' ) '  Your SKIP_NUM = ', skip_num
        stop 1
      end if

      if ( trial_num .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'HIGH_CARD_SIMULATION - Fatal error!'
        write ( *, '(a)' ) '  TRIAL_NUM must be at least 1.'
        write ( *, '(a,i6)' ) '  Your TRIAL_NUM was ', trial_num
        stop 1
      end if

      correct = 0

      do trial = 1, trial_num

        call permutation_random ( deck_size, seed, cards )

        if ( 1 .le. skip_num ) then
          skip_max = -i4_huge
          do i = 1, skip_num
            skip_max = max ( skip_max, cards(i) )
          end do
        else
          skip_max = - i4_huge
        end if

        true_max = - i4_huge
        do i = 1, deck_size
          true_max = max ( true_max, cards(i) )
        end do
c
c  In case you don't encounter a card larger than SKIP_MAX,
c  we'll assume you pick the last card in the deck, even though
c  you know it's a loser.
c
        choice = cards(deck_size)
c
c  Turn over the remaining cards in the deck, but stop
c  immediately when you find one bigger than SKIP_MAX.
c
        do card = skip_num + 1, deck_size
          if ( skip_max .lt. cards(card) ) then
            choice = cards(card)
            go to 10
          end if
        end do

10      continue
c
c  Record successful choices.
c
        if ( choice .eq. true_max ) then
          correct = correct + 1
        end if

      end do
c
c  Estimate the probability.
c
      p = dble ( correct ) / dble ( trial_num )

      return
      end
      subroutine permutation_random ( n, seed, p )

c*********************************************************************72
c
cc PERMUTATION_RANDOM returns a random permutation.
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
c  Parameters:
c
c    Input, integer N, the number of objects to permute.
c
c    Input, integer SEED, a seed for the random
c    number generator.
c
c    Output, integer P(N), a permutation of the integers
c    from 1 to N.
c
      implicit none

      integer n

      integer i
      integer i4_uniform_ab
      integer k
      integer p(n)
      integer p1
      integer seed

      do i = 1, n
        p(i) = i
      end do

      do i = 1, n - 1

        k = i4_uniform_ab ( i, n, seed )

        p1   = p(i)
        p(i) = p(k)
        p(k) = p1

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
