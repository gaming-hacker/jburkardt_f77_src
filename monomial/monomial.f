      function i4_choose ( n, k )

c*********************************************************************72
c
cc I4_CHOOSE computes the binomial coefficient C(N,K).
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in integer arithmetic.
c
c    The formula used is:
c
c      C(N,K) = N! / ( K! * (N-K)! )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 June 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    ML Wolfson, HV Wright,
c    Algorithm 160:
c    Combinatorial of M Things Taken N at a Time,
c    Communications of the ACM,
c    Volume 6, Number 4, April 1963, page 161.
c
c  Parameters:
c
c    Input, integer N, K, are the values of N and K.
c
c    Output, integer I4_CHOOSE, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer i
      integer i4_choose
      integer k
      integer mn
      integer mx
      integer n
      integer value

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        value = 0

      else if ( mn .eq. 0 ) then

        value = 1

      else

        mx = max ( k, n - k )
        value = mx + 1

        do i = 2, mn
          value = ( value * ( mx + i ) ) / i
        end do

      end if

      i4_choose = value

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
      function i4vec_sum ( n, a )

c*********************************************************************72
c
cc I4VEC_SUM returns the sum of the entries of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    In FORTRAN90, this facility is offered by the built in
c    SUM function:
c
c      I4VEC_SUM ( N, A ) = SUM ( A(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer I4VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer i4vec_sum

      i4vec_sum = 0

      do i = 1, n
        i4vec_sum = i4vec_sum + a(i)
      end do

      return
      end
      subroutine i4vec_uniform_ab ( n, a, b, seed, x )

c*********************************************************************72
c
cc I4VEC_UNIFORM_AB returns a scaled pseudorandom I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    The pseudorandom numbers should be uniformly distributed
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
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c
c    Input, integer A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, integer X(N), a vector of numbers between A and B.
c
      implicit none

      integer n

      integer a
      integer b
      integer i
      integer k
      real r
      integer seed
      integer value
      integer x(n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4VEC_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do i = 1, n

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
     &    +             r   * ( real ( max ( a, b ) ) + 0.5E+00 )
c
c  Use rounding to convert R to an integer between A and B.
c
        value = nint ( r )

        value = max ( value, min ( a, b ) )
        value = min ( value, max ( a, b ) )

        x(i) = value

      end do

      return
      end
      function mono_between_enum ( m, n1, n2 )

c*********************************************************************72
c
cc MONO_BETWEEN_ENUM enumerates monomials in M dimensions of degrees in a range.
c
c  Discussion:
c
c    For M = 3, we have the following table:
c
c     N2 0  1  2  3  4  5  6   7   8
c   N1 +----------------------------
c    0 | 1  4 10 20 35 56 84 120 165
c    1 | 0  3  9 19 34 55 83 119 164
c    2 | 0  0  6 16 31 52 80 116 161
c    3 | 0  0  0 10 25 46 74 110 155
c    4 | 0  0  0  0 15 36 64 100 145
c    5 | 0  0  0  0  0 21 49  85 130
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N1, N2, the minimum and maximum degrees.
c    0 <= N1 <= N2.
c
c    Output, integer MONO_BETWEEN_ENUM, the number of monomials
c    in D variables, of total degree between N1 and N2 inclusive.
c
      implicit none

      integer m
      integer i4_choose
      integer mono_between_enum
      integer n0
      integer n1
      integer n1_copy
      integer n2
      integer value

      n1_copy = max ( n1, 0 )

      if ( n2 .lt. n1_copy ) then
        mono_between_enum = 0
        return
      end if

      if ( n1_copy .eq. 0 ) then
        value = i4_choose ( n2 + m, n2 )
      else if ( n1_copy .eq. n2 ) then
        value = i4_choose ( n2 + m - 1, n2 )
      else
        n0 = n1_copy - 1
        value = i4_choose ( n2 + m, n2 ) - i4_choose ( n0 + m, n0 )
      end if

      mono_between_enum = value

      return
      end
      subroutine mono_between_next_grevlex ( m, n1, n2, x )

c*********************************************************************72
c
cc MONO_BETWEEN_NEXT_GREVLEX: grevlex next monomial, degree between N1 and N2.
c
c  Discussion:
c
c    We consider all monomials in an M dimensional space, with total
c    degree N between N1 and N2, inclusive.
c
c    For example:
c
c    M = 3
c    N1 = 2
c    N2 = 3
c
c    #  X(1)  X(2)  X(3)  Degree
c      +------------------------
c    1 |  0     0     2        2
c    2 |  0     1     1        2
c    3 |  1     0     1        2
c    4 |  0     2     0        2
c    5 |  1     1     0        2
c    6 |  2     0     0        2
c      |
c    7 |  0     0     3        3
c    8 |  0     1     2        3
c    9 |  1     0     2        3
c   10 |  0     2     1        3
c   11 |  1     1     1        3
c   12 |  2     0     1        3
c   13 |  0     3     0        3
c   14 |  1     2     0        3
c   15 |  2     1     0        3
c   16 |  3     0     0        3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N1, N2, the minimum and maximum degrees.
c    0 <= N1 <= N2.
c
c    Input, integer X(M), the current monomial.
c    To start the sequence, set X = [ 0, 0, ..., 0, N1 ].
c
c    Output, integer X(M), the next monomial.
c    The last value in the sequence is X = [ N2, 0, ..., 0, 0 ].
c
      implicit none

      integer m

      integer i
      integer i4vec_sum
      integer j
      integer n1
      integer n2
      integer t
      integer x(m)

      if ( n1 .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GREVLEX - Fatal error!'
        write ( *, '(a)' ) '  N1 .lt. 0.'
        stop 1
      end if

      if ( n2 .lt. n1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GREVLEX - Fatal error!'
        write ( *, '(a)' ) '  N2 .lt. N1.'
        stop 1
      end if

      if ( i4vec_sum ( m, x ) .lt. n1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GREVLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X sums to less than N1.'
        stop 1
      end if

      if ( n2 .lt. i4vec_sum ( m, x ) ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GREVLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X sums to more than N2.'
        stop 1
      end if

      if ( n2 .eq. 0 ) then
        return
      end if

      if ( x(1) .eq. n2 ) then
        x(1) = 0
        x(m) = n1
      else
        call mono_next_grevlex ( m, x )
      end if

      return
      end
      subroutine mono_between_next_grlex ( m, n1, n2, x )

c*********************************************************************72
c
cc MONO_BETWEEN_NEXT_GRLEX: grlex next monomial, degree between N1 and N2.
c
c  Discussion:
c
c    We consider all monomials in an M dimensional space, with total
c    degree N between N1 and N2, inclusive.
c
c    For example:
c
c    M = 3
c    N1 = 2
c    N2 = 3
c
c    #  X(1)  X(2)  X(3)  Degree
c      +------------------------
c    1 |  0     0     2        2
c    2 |  0     1     1        2
c    3 |  0     2     0        2
c    4 |  1     0     1        2
c    5 |  1     1     0        2
c    6 |  2     0     0        2
c      |
c    7 |  0     0     3        3
c    8 |  0     1     2        3
c    9 |  0     2     1        3
c   10 |  0     3     0        3
c   11 |  1     0     2        3
c   12 |  1     1     1        3
c   13 |  1     2     0        3
c   14 |  2     0     1        3
c   15 |  2     1     0        3
c   16 |  3     0     0        3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N1, N2, the minimum and maximum degrees.
c    0 <= N1 <= N2.
c
c    Input/output, integer X(M), the current monomial.
c    To start the sequence, set X = [ 0, 0, ..., 0, N1 ].
c    The last value in the sequence is X = [ N2, 0, ..., 0, 0 ].
c
      implicit none

      integer m

      integer i
      integer i4vec_sum
      integer im1
      integer j
      integer n1
      integer n2
      integer t
      integer x(m)

      if ( n1 .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  N1 .lt. 0.'
        stop 1
      end if

      if ( n2 .lt. n1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  N2 .lt. N1.'
        stop 1
      end if

      if ( i4vec_sum ( m, x ) .lt. n1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X sums to less than N1.'
        stop 1
      end if

      if ( n2 .lt. i4vec_sum ( m, x ) ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_BETWEEN_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X sums to more than N2.'
        stop 1
      end if

      if ( n2 .eq. 0 ) then
        return
      end if

      if ( x(1) .eq. n2 ) then
        x(1) = 0
        x(m) = n1
      else
        call mono_next_grlex ( m, x )
      end if

      return
      end
      subroutine mono_between_random ( m, n1, n2, seed, rank, x )

c*********************************************************************72
c
cc MONO_BETWEEN_RANDOM: random monomial with total degree between N1 and N2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N1, N2, the minimum and maximum degrees.
c    0 <= N1 <= N2.
c
c    Input/output, integer SEED, the random number seed.
c
c    Output, integer RANK, the rank of the monomial.
c
c    Output, integer X(M), the random monomial.
c
      implicit none

      integer m

      integer i4_uniform_ab
      integer mono_upto_enum
      integer n1
      integer n1_copy
      integer n2
      integer rank
      integer rank_max
      integer rank_min
      integer seed
      integer x(m)

      n1_copy = max ( n1, 0 )
      rank_min = mono_upto_enum ( m, n1_copy - 1 ) + 1
      rank_max = mono_upto_enum ( m, n2 )
      rank = i4_uniform_ab ( rank_min, rank_max, seed )
      call mono_unrank_grlex ( m, rank, x )

      return
      end
      subroutine mono_next_grevlex ( m, x )

c*********************************************************************72
c
cc MONO_NEXT_GREVLEX: grevlex next monomial.
c
c  Discussion:
c
c    Example:
c
c    M = 3
c
c    #  X(1)  X(2)  X(3)  Degree
c      +------------------------
c    1 |  0     0     0        0
c      |
c    2 |  0     0     1        1
c    3 |  0     1     0        1
c    4 |  1     0     0        1
c      |
c    5 |  0     0     2        2
c    6 |  0     1     1        2
c    7 |  1     0     1        2
c    8 |  0     2     0        2
c    9 |  1     1     0        2
c   10 |  2     0     0        2
c      |
c   11 |  0     0     3        3
c   12 |  0     1     2        3
c   13 |  1     0     2        3
c   14 |  0     2     1        3
c   15 |  1     1     1        3
c   16 |  2     0     1        3
c   17 |  0     3     0        3
c   18 |  1     2     0        3
c   19 |  2     1     0        3
c   20 |  3     0     0        3
c
c    Thanks to Stefan Klus for pointing out a discrepancy in a previous
c    version of this code, 05 February 2015.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 February 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer X(M), the current monomial.
c    The first element is X = [ 0, 0, ..., 0, 0 ].
c
c    Output, integer X(M), the next monomial.
c
      implicit none

      integer m

      integer i
      integer i4vec_sum
      integer j
      integer t
      integer x(m)

      if ( i4vec_sum ( m, x ) .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UPTO_NEXT_GREVLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X sums to less than 0.'
        stop 1
      end if
c
c  Seek the first index 1 .lt. I for which 0 .lt. X(I).
c
      j = 1

      do i = 2, m
        if ( 0 .lt. x(i) ) then
          j = i
          go to 10
        end if
      end do

10    continue

      if ( j .eq. 1 ) then
        t = x(1)
        x(1) = 0
        x(m) = t + 1
      else if ( j .lt. m ) then
        x(j) = x(j) - 1
        t = x(1) + 1
        x(1) = 0
        x(j-1) = x(j-1) + t
      else if ( j .eq. m ) then
        t = x(1)
        x(1) = 0
        x(j-1) = t + 1
        x(j) = x(j) - 1
      end if

      return
      end
      subroutine mono_next_grlex ( m, x )

c*********************************************************************72
c
cc MONO_NEXT_GRLEX returns the next monomial in grlex order.
c
c  Discussion:
c
c    Example:
c
c    M = 3
c
c    #  X(1)  X(2)  X(3)  Degree
c      +------------------------
c    1 |  0     0     0        0
c      |
c    2 |  0     0     1        1
c    3 |  0     1     0        1
c    4 |  1     0     0        1
c      |
c    5 |  0     0     2        2
c    6 |  0     1     1        2
c    7 |  0     2     0        2
c    8 |  1     0     1        2
c    9 |  1     1     0        2
c   10 |  2     0     0        2
c      |
c   11 |  0     0     3        3
c   12 |  0     1     2        3
c   13 |  0     2     1        3
c   14 |  0     3     0        3
c   15 |  1     0     2        3
c   16 |  1     1     1        3
c   17 |  1     2     0        3
c   18 |  2     0     1        3
c   19 |  2     1     0        3
c   20 |  3     0     0        3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer X(M), the current monomial.
c    The first element is X = [ 0, 0, ..., 0, 0 ].
c
      implicit none

      integer m

      integer i
      integer i4vec_sum
      integer im1
      integer j
      integer t
      integer x(m)
c
c  Ensure that 1 <= M.
c
      if ( m .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'COMP_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  M .lt. 1'
        stop 1
      end if
c
c  Ensure that 0 <= X(I).
c
      do i = 1, m
        if ( x(i) .lt. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'COMP_NEXT_GRLEX - Fatal error!'
          write ( *, '(a)' ) '  X(I) .lt. 0'
          stop 1
        end if
      end do
c
c  Find I, the index of the rightmost nonzero entry of X.
c
      i = 0
      do j = m, 1, -1

        if ( 0 .lt. x(j) ) then
          i = j
          go to 10
        end if

      end do

10    continue
c
c  set T = X(I)
c  set X(I) to zero,
c  increase X(I-1) by 1,
c  increment X(M) by T-1.
c
      if ( i .eq. 0 ) then
        x(m) = 1
        return
      else if ( i .eq. 1 ) then
        t = x(1) + 1
        im1 = m
      else if ( 1 .lt. i ) then
        t = x(i)
        im1 = i - 1
      end if

      x(i) = 0
      x(im1) = x(im1) + 1
      x(m) = x(m) + t - 1

      return
      end
      subroutine mono_print ( m, f, title )

c*********************************************************************72
c
cc MONO_PRINT prints a monomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer F(M), the exponents.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m

      integer f(m)
      integer i
      character * ( * ) title

      write ( *, '(a)', advance = 'no' ) title
      write ( *, '(a)', advance = 'no' ) 'x^('
      do i = 1, m
        write ( *, '(i2)', advance = 'no' ) f(i)
        if ( i .lt. m ) then
          write (  *, '(a)', advance = 'no' ) ','
        else
          write (  *, '(a)', advance = 'yes' ) ').'
        end if
      end do

      return
      end
      subroutine mono_rank_grlex ( m, x, rank )

c*********************************************************************72
c
cc MONO_RANK_GRLEX computes the graded lexicographic rank of a monomial.
c
c  Discussion:
c
c    The graded lexicographic ordering is used, over all monomials of
c    dimension M, with degree NM = 0, then 1, then 2, ...
c
c    For example, if M = 3, the ranking begins:
c
c    Rank  Sum    1  2  3
c    ----  ---   -- -- --
c       1    0    0  0  0
c
c       2    1    0  0  1
c       3    1    0  1  0
c       4    1    1  0  1
c
c       5    2    0  0  2
c       6    2    0  1  1
c       7    2    0  2  0
c       8    2    1  0  1
c       9    2    1  1  0
c      10    2    2  0  0
c
c      11    3    0  0  3
c      12    3    0  1  2
c      13    3    0  2  1
c      14    3    0  3  0
c      15    3    1  0  2
c      16    3    1  1  1
c      17    3    1  2  0
c      18    3    2  0  1
c      19    3    2  1  0
c      20    3    3  0  0
c
c      21    4    0  0  4
c      ..   ..   .. .. ..
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c    1 <= D.
c
c    Input, integer X(M), the composition.
c    For each 1 <= I <= M, we have 0 <= X(I).
c
c    Output, integer RANK, the rank of the composition.
c
      implicit none

      integer m

      integer i
      integer i4_choose
      integer i4vec_sum
      integer j
      integer ks
      integer n
      integer nm
      integer ns
      integer rank
      integer tim1
      integer x(m)
      integer xs(m-1)
c
c  Ensure that 1 <= M.
c
      if ( m .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_RANK_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  M .lt. 1'
        stop 1
      end if
c
c  Ensure that 0 <= X(I).
c
      do i = 1, m
        if ( x(i) .lt. 0 ) then
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'MONO_RANK_GRLEX - Fatal error!'
          write ( *, '(a)' ) '  X(I) .lt. 0'
          stop 1
        end if
      end do
c
c  NM = sum ( X )
c
      nm = i4vec_sum ( m, x )
c
c  Convert to KSUBSET format.
c
      ns = nm + m - 1
      ks = m - 1

      xs(1) = x(1) + 1
      do i = 2, ks
        xs(i) = xs(i-1) + x(i) + 1
      end do
c
c  Compute the rank.
c
      rank = 1

      do i = 1, ks

        if ( i == 1 ) then
          tim1 = 0
        else
          tim1 = xs(i-1)
        end if

        if ( tim1 + 1 .le. xs(i) - 1 ) then
          do j = tim1 + 1, xs(i) - 1
            rank = rank + i4_choose ( ns - j, ks - i )
          end do
        end if

      end do

      do n = 0, nm - 1
        rank = rank + i4_choose ( n + m - 1, n )
      end do

      return
      end
      function mono_total_enum ( m, n )

c*********************************************************************72
c
cc MONO_TOTAL_ENUM enumerates monomials in M dimensions of degree equal to N.
c
c  Discussion:
c
c    For M = 3, we have the following values:
c
c    N  VALUE
c
c    0    1
c    1    3
c    2    6
c    3   10
c    4   15
c    5   21
c
c    In particular, VALUE(3,3) = 10 because we have the 10 monomials:
c
c      x^3, x^2y, x^2z, xy^2, xyz, xz^3, y^3, y^2z, yz^2, z^3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the maximum degree.
c
c    Output, integer VALUE, the number of monomials in D variables,
c    of total degree N.
c
      implicit none

      integer m
      integer i4_choose
      integer mono_total_enum
      integer n
      integer value

      value = i4_choose ( n + m - 1, n )

      mono_total_enum = value

      return
      end
      subroutine mono_total_next_grevlex ( m, n, x )

c*********************************************************************72
c
cc MONO_TOTAL_NEXT_GREVLEX: grevlex next monomial with total degree equal to N.
c
c  Discussion:
c
c    We consider all monomials in an M dimensional space, with total degree N.
c
c    For example:
c
c    M = 3
c    N = 3
c
c    #  X(1)  X(2)  X(3)  Degree
c      +------------------------
c    1 |  0     0     3        3
c    2 |  0     1     2        3
c    3 |  1     0     2        3
c    4 |  0     2     1        3
c    5 |  1     1     1        3
c    6 |  2     0     1        3
c    7 |  0     3     0        3
c    8 |  1     2     0        3
c    9 |  2     1     0        3
c   10 |  3     0     0        3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the degree.
c    0 <= N.
c
c    Input, integer X(M), the current monomial.
c    To start the sequence, set X = [ 0, 0, ..., 0, N ].
c
c    Output, integer X(M), the next monomial.
c    The last value in the sequence is X = [ N, 0, ..., 0, 0 ].
c
      implicit none

      integer m

      integer i
      integer i4vec_sum
      integer j
      integer n
      integer t
      integer x(m)

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_TOTAL_NEXT_GREVLEX - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 0.'
        stop 1
      end if

      if ( i4vec_sum ( m, x ) .ne. n ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_TOTAL_NEXT_GREVLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X does not sum to N.'
        stop 1
      end if

      if ( n .eq. 0 ) then
        return
      end if

      if ( x(1) .eq. n ) then
        x(1) = 0
        x(m) = n
      else
        call mono_next_grevlex ( m, x )
      end if

      return
      end
      subroutine mono_total_next_grlex ( m, n, x )

c*********************************************************************72
c
cc MONO_TOTAL_NEXT_GRLEX: grlex next monomial with total degree equal to N.
c
c  Discussion:
c
c    We consider all monomials in an M dimensional space, with total degree N.
c
c    For example:
c
c    M = 3
c    N = 3
c
c    #  X(1)  X(2)  X(3)  Degree
c      +------------------------
c    1 |  0     0     3        3
c    2 |  0     1     2        3
c    3 |  0     2     1        3
c    4 |  0     3     0        3
c    5 |  1     0     2        3
c    6 |  1     1     1        3
c    7 |  1     2     0        3
c    8 |  2     0     1        3
c    9 |  2     1     0        3
c   10 |  3     0     0        3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the degree.
c    0 <= N.
c
c    Input/output, integer X(M), the current monomial.
c    To start the sequence, set X = [ 0, 0, ..., 0, N ].
c    The last value in the sequence is X = [ N, 0, ..., 0, 0 ].
c
      implicit none

      integer m

      integer i
      integer i4vec_sum
      integer im1
      integer j
      integer n
      integer t
      integer x(m)

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_TOTAL_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 0.'
        stop 1
      end if

      if ( i4vec_sum ( m, x ) .ne. n ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_TOTAL_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X does not sum to N.'
        stop 1
      end if

      if ( n .eq. 0 ) then
        return
      end if

      if ( x(1) .eq. n ) then
        x(1) = 0
        x(m) = n
      else
        call mono_next_grlex ( m, x )
      end if

      return
      end
      subroutine mono_total_random ( m, n, seed, rank, x )

c*********************************************************************72
c
cc MONO_TOTAL_RANDOM: random monomial with total degree equal to N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the degree.
c    0 <= N.
c
c    Input/output, integer SEED, the random number seed.
c
c    Output, integer RANK, the rank of the monomial.
c
c    Output, integer X(M), the random monomial.
c
      implicit none

      integer m

      integer i4_uniform_ab
      integer mono_upto_enum
      integer n
      integer rank
      integer rank_max
      integer rank_min
      integer seed
      integer x(m)

      rank_min = mono_upto_enum ( m, n - 1 ) + 1
      rank_max = mono_upto_enum ( m, n )
      rank = i4_uniform_ab ( rank_min, rank_max, seed )
      call mono_unrank_grlex ( m, rank, x )

      return
      end
      subroutine mono_unrank_grlex ( m, rank, x )

c*********************************************************************72
c
cc MONO_UNRANK_GRLEX computes the monomial of given grlex rank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c    1 <= M.
c
c    Input, integer RANK, the rank of the monomial.
c    1 <= RANK.
c
c    Output, integer X(M), the composition of the given rank.
c    For each I, 0 <= X(I) <= NM, and 
c    sum ( 1 <= I <= M ) X(I) = NM.
c
      implicit none

      integer m

      integer i
      integer i4_choose
      integer j
      integer ks
      integer nksub
      integer nm
      integer ns
      integer r
      integer rank
      integer rank1
      integer rank2
      integer x(m)
      integer xs(m-1)
c
c  Ensure that 1 <= M.
c
      if ( m .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UNRANK_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  M .lt. 1'
        stop 1
      end if
c
c  Ensure that 1 <= RANK.
c
      if ( rank .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UNRANK_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  RANK .lt. 1'
        stop 1
      end if
c
c  Special case M == 1.
c
      if ( m .eq. 1 ) then
        x(1) = rank - 1
        return
      end if
c
c  Determine the appropriate value of NM.
c  Do this by adding up the number of compositions of sum 0, 1, 2, 
c  ..., without exceeding RANK.  Moreover, RANK - this sum essentially
c  gives you the rank of the composition within the set of compositions
c  of sum NM.  And that's the number you need in order to do the
c  unranking.
c
      rank1 = 1
      nm = -1

10    continue

        nm = nm + 1
        r = i4_choose ( nm + m - 1, nm )
        if ( rank .lt. rank1 + r ) then
          go to 20
        end if
        rank1 = rank1 + r
      go to 10

20    continue

      rank2 = rank - rank1
c
c  Convert to KSUBSET format.
c  Apology: an unranking algorithm was available for KSUBSETS,
c  but not immediately for compositions.  One day we will come back
c  and simplify all this.
c
      ks = m - 1
      ns = nm + m - 1

      nksub = i4_choose ( ns, ks )

      j = 1

      do i = 1, ks

        r = i4_choose ( ns - j, ks - i )

30      continue

        if ( r <= rank2 .and. 0 .lt. r ) then
          rank2 = rank2 - r
          j = j + 1
          r = i4_choose ( ns - j, ks - i )
          go to 30
        end if

        xs(i) = j
        j = j + 1

      end do
c
c  Convert from KSUBSET format to COMP format.
c
      x(1) = xs(1) - 1
      do i = 2, m - 1
        x(i) = xs(i) - xs(i-1) - 1
      end do
      x(m) = ns - xs(ks)

      return
      end
      function mono_upto_enum ( m, n )

c*********************************************************************72
c
cc MONO_UPTO_ENUM enumerates monomials in M dimensions of degree up to N.
c
c  Discussion:
c
c    For M = 2, we have the following values:
c
c    N  VALUE
c
c    0    1
c    1    3
c    2    6
c    3   10
c    4   15
c    5   21
c
c    In particular, VALUE(2,3) = 10 because we have the 10 monomials:
c
c      1, x, y, x^2, xy, y^2, x^3, x^2y, xy^2, y^3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the maximum degree.
c
c    Output, integer MONO_UPTO_ENUM, the number of monomials in
c    D variables, of total degree N or less.
c
      implicit none

      integer m
      integer i4_choose
      integer mono_upto_enum
      integer n
      integer value

      value = i4_choose ( n + m, n )

      mono_upto_enum = value

      return
      end
      subroutine mono_upto_next_grevlex ( m, n, x )

c*********************************************************************72
c
cc MONO_UPTO_NEXT_GREVLEX: grevlex next monomial with total degree up to N.
c
c  Discussion:
c
c    We consider all monomials in an M dimensional space, with total
c    degree up to N.
c
c    For example:
c
c    M = 3
c    N = 3
c
c    #  X(1)  X(2)  X(3)  Degree
c      +------------------------
c    1 |  0     0     0        0
c      |
c    2 |  0     0     1        1
c    3 |  0     1     0        1
c    4 |  1     0     0        1
c      |
c    5 |  0     0     2        2
c    6 |  0     1     1        2
c    7 |  1     0     1        2
c    8 |  0     2     0        2
c    9 |  1     1     0        2
c   10 |  2     0     0        2
c      |
c   11 |  0     0     3        3
c   12 |  0     1     2        3
c   13 |  1     0     2        3
c   14 |  0     2     1        3
c   15 |  1     1     1        3
c   16 |  2     0     1        3
c   17 |  0     3     0        3
c   18 |  1     2     0        3
c   19 |  2     1     0        3
c   20 |  3     0     0        3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the maximum degree.
c    0 <= N.
c
c    Input, integer X(M), the current monomial.
c    To start the sequence, set X = [ 0, 0, ..., 0, 0 ].
c
c    Output, integer X(M), the next monomial.
c    The last value in the sequence is X = [ N, 0, ..., 0, 0 ].
c
      implicit none

      integer m

      integer i
      integer i4vec_sum
      integer j
      integer n
      integer t
      integer x(m)

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UPTO_NEXT_GREVLEX - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 0.'
        stop 1
      end if

      if ( i4vec_sum ( m, x ) .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UPTO_NEXT_GREVLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X sums to less than 0.'
        stop 1
      end if

      if ( n .lt. i4vec_sum ( m, x ) ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UPTO_NEXT_GREVLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X sums to more than N.'
        stop 1
      end if

      if ( n .eq. 0 ) then
        return
      end if

      if ( x(1) .eq. n ) then
        x(1) = 0
      else
        call mono_next_grevlex ( m, x )
      end if

      return
      end
      subroutine mono_upto_next_grlex ( m, n, x )

c*********************************************************************72
c
cc MONO_UPTO_NEXT_GRLEX: grlex next monomial with total degree up to N.
c
c  Discussion:
c
c    We consider all monomials in an M dimensional space, with total
c    degree up to N.
c
c    For example:
c
c    M = 3
c    N = 3
c
c    #  X(1)  X(2)  X(3)  Degree
c      +------------------------
c    1 |  0     0     0        0
c      |
c    2 |  0     0     1        1
c    3 |  0     1     0        1
c    4 |  1     0     0        1
c      |
c    5 |  0     0     2        2
c    6 |  0     1     1        2
c    7 |  0     2     0        2
c    8 |  1     0     1        2
c    9 |  1     1     0        2
c   10 |  2     0     0        2
c      |
c   11 |  0     0     3        3
c   12 |  0     1     2        3
c   13 |  0     2     1        3
c   14 |  0     3     0        3
c   15 |  1     0     2        3
c   16 |  1     1     1        3
c   17 |  1     2     0        3
c   18 |  2     0     1        3
c   19 |  2     1     0        3
c   20 |  3     0     0        3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the maximum degree.
c    0 <= N.
c
c    Input/output, integer X(M), the current monomial.
c    To start the sequence, set X = [ 0, 0, ..., 0, 0 ].
c    The last value in the sequence is X = [ N, 0, ..., 0, 0 ].
c
      implicit none

      integer m

      integer i
      integer i4vec_sum
      integer im1
      integer j
      integer n
      integer t
      integer x(m)

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UPTO_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  N .lt. 0.'
        stop 1
      end if

      if ( i4vec_sum ( m, x ) .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UPTO_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X sums to less than 0.'
        stop 1
      end if

      if ( n .lt. i4vec_sum ( m, x ) ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'MONO_UPTO_NEXT_GRLEX - Fatal error!'
        write ( *, '(a)' ) '  Input X sums to more than N.'
        stop 1
      end if

      if ( n .eq. 0 ) then
        return
      end if

      if ( x(1) .eq. n ) then
        x(1) = 0
      else
        call mono_next_grlex ( m, x )
      end if

      return
      end
      subroutine mono_upto_random ( m, n, seed, rank, x )

c*********************************************************************72
c
cc MONO_UPTO_RANDOM: random monomial with total degree less than or equal to N.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 November 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the degree.
c    0 <= N.
c
c    Input/output, integer SEED, the random number seed.
c
c    Output, integer RANK, the rank of the monomial.
c
c    Output, integer X(M), the random monomial.
c
      implicit none

      integer m

      integer i4_uniform_ab
      integer mono_upto_enum
      integer n
      integer rank
      integer rank_max
      integer rank_min
      integer seed
      integer x(m)

      rank_min = 1
      rank_max = mono_upto_enum ( m, n )
      rank = i4_uniform_ab ( rank_min, rank_max, seed )
      call mono_unrank_grlex ( m, rank, x )

      return
      end
      subroutine mono_value ( m, n, f, x, v )

c*********************************************************************72
c
cc MONO_VALUE evaluates a monomial.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, integer F(M), the exponents of the monomial.
c
c    Input, double precision X(M,N), the coordinates of the evaluation points.
c
c    Output, double precision V(N), the value of the monomial at X.
c
      implicit none

      integer m
      integer n

      integer f(m)
      integer i
      integer j
      double precision v(n)
      double precision x(m,n)
  
      do j = 1, n
        v(j) = 1.0D+00
        do i = 1, m
          v(j) = v(j) * x(i,j) ** f(i)
        end do
      end do

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
