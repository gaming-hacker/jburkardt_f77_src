      function binom ( n, k )

c*********************************************************************72
c
cc BINOM computes the binomial coefficient.
c
c  Discussion:
c
C    This is ACM ALGORITHM 160 TRANSLATED TO FORTRAN.  
c
c    It CALCULATES THE NUMBER OF COMBINATIONS OF M THINGS TAKEN N AT A TIME.
C
c  Modified:
c
c    27 March 2016
c
c  Author:
c
c    Bill Buckles, Matthew Lybanon
c
c  Reference:
c
c    Bill Buckles, Matthew Lybanon,
c    Algorithm 515: Generation of a Vector from the Lexicographical Index,
c    ACM Transactions on Mathematical Software,
c    Volume 3, Number 2, June 1977, pages 180-182.
c
c  Parameters:
c
c    Input, integer N, K, the parameters for the binomial coefficient.
c
c    Output, integer BINOM, the binomial coefficient.
c
      implicit none

      integer binom
      integer i
      integer k
      integer k1
      integer n
      integer n1
      integer p
      integer r

      k1 = k
      p = n - k1

      if ( k1 .lt. p ) then
        p = k1
        k1 = n - p
      end if

      if ( p .eq. 0 ) then
        r = 1
      else
        r = k1 + 1
      end if

      do i = 2, p
        r = ( r * ( k1 + i ) ) / i
      end do

      binom = r

      return
      end
      subroutine comb ( N, P, L, C )

c*********************************************************************72
c
cc COMB selects a subset of order P from a set of order N.
c
c  Discussion:
c
c    This subroutine finds the combination set of N things taken
c    P at a time for a given lexicographic index.
c
c  Modified:
c
c    27 March 2016
c
c  Author:
c
c    Bill Buckles, Matthew Lybanon
c
c  Reference:
c
c    Bill Buckles, Matthew Lybanon,
c    Algorithm 515: Generation of a Vector from the Lexicographical Index,
c    ACM Transactions on Mathematical Software,
c    Volume 3, Number 2, June 1977, pages 180-182.
c
c  Parameters:
c
C    Input, integer N, the number of things in the set.
c
C    Input, integer P, the number of things in each combination.
c    0 < P < N.
c
C    Input, integer L, the lexicographi index of the desired combination.
c    1 <= L <= choose(N,P).
c
C    Output, integer C(P), the combination set.
C
      implicit none

      integer p

      integer binom
      integer c(p)
      integer i
      integer k
      integer l
      integer n
      integer p1
      integer r
C
C  SPECIAL CASE IF P = 1
C
      if ( p .eq. 1 ) then
        c(1) = l
        return
      end if
C
C  INITIALIZE LOWER BOUND INDEX AT ZERO
C
      k = 0
C
C  LOOP TO SELECT ELEMENTS IN ASCENDING ORDER
C
      p1 = p - 1
      c(1) = 0

      do i = 1, p1
C
C  SET LOWER BOUND ELEMENT NUMBER FOR NEXT ELEMENT VALUE
C
        if ( 1 < i ) then
          c(i) = c(i-1)
        end if
C
C  LOOP TO CHECK VALIDITY OF EACH ELEMENT VALUE
C
   10   continue

        c(i) = c(i) + 1
        r = binom ( n - c(i), p - i )
        k = k + r

        if ( k < l ) then
          go to 10
        end if

        k = k - r

      end do

      c(p) = c(p1) + l - k

      return
      end
      function i4_choose_check ( n, k )

c*********************************************************************72
c
cc I4_CHOOSE_CHECK reports whether the binomial coefficient can be computed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 March 2016
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, K, the parameters in the binomial coefficient.
c
c    Output, logical I4_CHOOSE_CHECK is:
c    TRUE, if C(N,K) < maximum integer.
c    FALSE, otherwise.
c
      implicit none

      logical i4_choose_check
      double precision choose_nk_log
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      double precision i4_huge_log
      integer n
      integer k
      double precision r8_gamma_log

      i4_huge_log = log ( dble ( i4_huge ) )

      choose_nk_log = 
     &    r8_gamma_log ( dble ( n + 1 ) )
     &  - r8_gamma_log ( dble ( k + 1 ) )
     &  - r8_gamma_log ( dble ( n - k + 1 ) )

      i4_choose_check = ( choose_nk_log < i4_huge_log )
        
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
      function r8_epsilon ( )

c*********************************************************************72
c
cc R8_EPSILON returns the R8 roundoff unit.
c
c  Discussion:
c
c    The roundoff unit is a number R which is a power of 2 with the
c    property that, to the precision of the computer's arithmetic,
c      1 .lt. 1 + R
c    but
c      1 = ( 1 + R / 2 )
c
c    FORTRAN90 provides the superior library routine
c
c      EPSILON ( X )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_EPSILON, the R8 roundoff unit.
c
      implicit none

      double precision r8_epsilon

      r8_epsilon = 2.220446049250313D-016

      return
      end
      function r8_gamma_log ( x )

c*********************************************************************72
c
cc R8_GAMMA_LOG evaluates the logarithm of the gamma function.
c
c  Discussion:
c
c    This routine calculates the LOG(GAMMA) function for a positive real
c    argument X.  Computation is based on an algorithm outlined in
c    references 1 and 2.  The program uses rational functions that
c    theoretically approximate LOG(GAMMA) to at least 18 significant
c    decimal digits.  The approximation for X > 12 is from reference
c    3, while approximations for X < 12.0 are similar to those in
c    reference 1, but are unpublished.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by William Cody, Laura Stoltz.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    William Cody, Kenneth Hillstrom,
c    Chebyshev Approximations for the Natural Logarithm of the
c    Gamma Function,
c    Mathematics of Computation,
c    Volume 21, Number 98, April 1967, pages 198-203.
c
c    Kenneth Hillstrom,
c    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
c    May 1969.
c
c    John Hart, Ward Cheney, Charles Lawson, Hans Maehly,
c    Charles Mesztenyi, John Rice, Henry Thatcher,
c    Christoph Witzgall,
c    Computer Approximations,
c    Wiley, 1968,
c    LC: QA297.C64.
c
c  Parameters:
c
c    Input, double precision X, the argument of the function.
c
c    Output, double precision R8_GAMMA_LOG, the value of the function.
c
      implicit none

      double precision c(7)
      double precision corr
      double precision d1 
      parameter ( d1 = -5.772156649015328605195174D-01 )
      double precision d2
      parameter ( d2 = 4.227843350984671393993777D-01 )
      double precision d4
      parameter ( d4 = 1.791759469228055000094023D+00 )
      double precision frtbig
      parameter ( frtbig = 2.25D+76 )
      integer i
      double precision p1(8)
      double precision p2(8)
      double precision p4(8)
      double precision q1(8)
      double precision q2(8)
      double precision q4(8)
      double precision r8_epsilon
      double precision r8_gamma_log
      double precision res
      double precision sqrtpi 
      parameter ( sqrtpi = 0.9189385332046727417803297D+00 )
      double precision x
      double precision xbig
      parameter ( xbig = 2.55D+305 )
      double precision xden
      double precision xinf
      parameter ( xinf = 1.79D+308 )
      double precision xm1
      double precision xm2
      double precision xm4
      double precision xnum
      double precision y
      double precision ysq

      save c
      save p1
      save p2
      save p4
      save q1
      save q2
      save q4

      data c /
     &  -1.910444077728D-03, 
     &   8.4171387781295D-04, 
     &  -5.952379913043012D-04, 
     &   7.93650793500350248D-04, 
     &  -2.777777777777681622553D-03, 
     &   8.333333333333333331554247D-02, 
     &   5.7083835261D-03 /
      data p1 /
     &  4.945235359296727046734888D+00, 
     &  2.018112620856775083915565D+02, 
     &  2.290838373831346393026739D+03, 
     &  1.131967205903380828685045D+04, 
     &  2.855724635671635335736389D+04, 
     &  3.848496228443793359990269D+04, 
     &  2.637748787624195437963534D+04, 
     &  7.225813979700288197698961D+03 /
      data p2 /
     &  4.974607845568932035012064D+00, 
     &  5.424138599891070494101986D+02, 
     &  1.550693864978364947665077D+04, 
     &  1.847932904445632425417223D+05, 
     &  1.088204769468828767498470D+06, 
     &  3.338152967987029735917223D+06, 
     &  5.106661678927352456275255D+06, 
     &  3.074109054850539556250927D+06 /
      data p4 /
     &  1.474502166059939948905062D+04, 
     &  2.426813369486704502836312D+06, 
     &  1.214755574045093227939592D+08, 
     &  2.663432449630976949898078D+09, 
     &  2.940378956634553899906876D+10, 
     &  1.702665737765398868392998D+11, 
     &  4.926125793377430887588120D+11, 
     &  5.606251856223951465078242D+11 /
      data q1 /
     &  6.748212550303777196073036D+01, 
     &  1.113332393857199323513008D+03, 
     &  7.738757056935398733233834D+03, 
     &  2.763987074403340708898585D+04, 
     &  5.499310206226157329794414D+04, 
     &  6.161122180066002127833352D+04, 
     &  3.635127591501940507276287D+04, 
     &  8.785536302431013170870835D+03 /
      data q2 /
     &  1.830328399370592604055942D+02, 
     &  7.765049321445005871323047D+03, 
     &  1.331903827966074194402448D+05, 
     &  1.136705821321969608938755D+06, 
     &  5.267964117437946917577538D+06, 
     &  1.346701454311101692290052D+07, 
     &  1.782736530353274213975932D+07, 
     &  9.533095591844353613395747D+06 /
      data q4 /
     &  2.690530175870899333379843D+03, 
     &  6.393885654300092398984238D+05, 
     &  4.135599930241388052042842D+07, 
     &  1.120872109616147941376570D+09, 
     &  1.488613728678813811542398D+10, 
     &  1.016803586272438228077304D+11, 
     &  3.417476345507377132798597D+11, 
     &  4.463158187419713286462081D+11 /

      y = x

      if ( 0.0D+00 .lt. y .and. y .le. xbig ) then

        if ( y .le. r8_epsilon ( ) ) then

          res = - log ( y )
c
c  EPS < X <= 1.5.
c
        else if ( y .le. 1.5D+00 ) then

          if ( y .lt. 0.6796875D+00 ) then
            corr = -log ( y )
            xm1 = y
          else
            corr = 0.0D+00
            xm1 = ( y - 0.5D+00 ) - 0.5D+00
          end if

          if ( y .le. 0.5D+00 .or. 0.6796875D+00 .le. y ) then

            xden = 1.0D+00
            xnum = 0.0D+00
            do i = 1, 8
              xnum = xnum * xm1 + p1(i)
              xden = xden * xm1 + q1(i)
            end do

            res = corr + ( xm1 * ( d1 + xm1 * ( xnum / xden ) ) )

          else

            xm2 = ( y - 0.5D+00 ) - 0.5D+00
            xden = 1.0D+00
            xnum = 0.0D+00
            do i = 1, 8
              xnum = xnum * xm2 + p2(i)
              xden = xden * xm2 + q2(i)
            end do

            res = corr + xm2 * ( d2 + xm2 * ( xnum / xden ) )

          end if
c
c  1.5 < X <= 4.0.
c
        else if ( y .le. 4.0D+00 ) then

          xm2 = y - 2.0D+00
          xden = 1.0D+00
          xnum = 0.0D+00
          do i = 1, 8
            xnum = xnum * xm2 + p2(i)
            xden = xden * xm2 + q2(i)
          end do

          res = xm2 * ( d2 + xm2 * ( xnum / xden ) )
c
c  4.0 < X <= 12.0.
c
        else if ( y .le. 12.0D+00 ) then

          xm4 = y - 4.0D+00
          xden = -1.0D+00
          xnum = 0.0D+00
          do i = 1, 8
            xnum = xnum * xm4 + p4(i)
            xden = xden * xm4 + q4(i)
          end do

          res = d4 + xm4 * ( xnum / xden )
c
c  Evaluate for 12 <= argument.
c
        else

          res = 0.0D+00

          if ( y .le. frtbig ) then

            res = c(7)
            ysq = y * y

            do i = 1, 6
              res = res / ysq + c(i)
            end do

          end if

          res = res / y
          corr = log ( y )
          res = res + sqrtpi - 0.5D+00 * corr
          res = res + y * ( corr - 1.0D+00 )

        end if
c
c  Return for bad arguments.
c
      else

        res = xinf

      end if
c
c  Final adjustments and return.
c
      r8_gamma_log = res

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

