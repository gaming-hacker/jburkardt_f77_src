      subroutine bvec_add ( n, bvec1, bvec2, bvec3 )

c*********************************************************************72
c
cc BVEC_ADD adds two (signed) binary vectors.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
c
c  Example:
c
c    N = 5
c
c      BVEC1       +   BVEC2       =   BVEC3
c
c    ( 0 0 0 0 1 ) + ( 0 0 0 1 1 ) = ( 0 0 1 0 0 )
c
c              1   +           3   =           4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, integer BVEC1(N), BVEC2(N), the vectors to be added.
c
c    Output, integer BVEC3(N), the sum of the two input vectors.
c
      implicit none

      integer n

      integer base
      parameter ( base = 2 )
      integer bvec1(n)
      integer bvec2(n)
      integer bvec3(n)
      integer i
      logical overflow

      overflow = .false.
    
      do i = 1, n
        bvec3(i) = bvec1(i) + bvec2(i)
      end do

      do i = 1, n

10      continue

        if ( base .le. bvec3(i) ) then

          bvec3(i) = bvec3(i) - base

          if ( 1 .lt. i ) then
            bvec3(i-1) = bvec3(i-1) + 1
          else
            overflow = .true.
          end if

        end if

      end do

      return
      end
      subroutine bvec_and ( n, bvec1, bvec2, bvec3 )

c*********************************************************************72
c
cc BVEC_AND computes the AND of two binary vectors.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
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
c    Input, integer N, the length of the vectors.
c
c    Input, integer BVEC1(N), BVEC2(N), the binary vectors.
c
c    Input, integer BVEC3(N), the AND of the two vectors.
c
      implicit none

      integer n

      integer bvec1(n)
      integer bvec2(n)
      integer bvec3(n)
      integer i

      do i = 1, n
        bvec3(i) = min ( bvec1(i), bvec2(i) )
      end do

      return
      end
      subroutine bvec_check ( n, bvec, ierror )

c*********************************************************************72
c
cc BVEC_CHECK checks a binary vector.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
c
c    The only check made is that the entries are all 0 or 1.
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
c    Input, integer N, the length of the vectors.
c
c    Input, integer BVEC(N), the vector to be checked.
c
c    Output, integer IERROR, is nonzero if an error occurred.
c
      implicit none

      integer n

      integer base
      parameter ( base = 2 )
      integer bvec(n)
      integer i
      integer ierror

      ierror = 0

      do i = 1, n
        if ( bvec(i) .lt. 0 .or. base .le. bvec(i) ) then
          ierror = i
          return
        end if
      end do

      return
      end
      subroutine bvec_complement2 ( n, bvec1, bvec2 )

c*********************************************************************72
c
cc BVEC_COMPLEMENT2 computes the two's complement of a binary vector.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, integer BVEC1(N), the vector to be complemented.
c
c    Output, integer BVEC2(N), the two's complemented vector.
c
      implicit none

      integer n
      integer n_max
      parameter ( n_max = 100 )

      integer base
      parameter ( base = 2 )
      integer bvec1(n)
      integer bvec2(n)
      integer bvec3(n_max)
      integer bvec4(n_max)
      integer i

      if ( n_max .lt. n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BVEC_COMPLEMENT2 - Fatal error!'
        write ( *, '(a)' ) '  Internal size limit N_MAX exceeded.'
        stop 1
      end if

      do i = 1, n
        bvec3(i) = ( base - 1 ) - bvec1(i)
      end do

      do i = 1, n - 1
        bvec4(i) = 0
      end do
      bvec4(n) = 1

      call bvec_add ( n, bvec3, bvec4, bvec2 )

      return
      end
      function bvec_enum ( n )

c*********************************************************************72
c
cc BVEC_ENUM enumerates the binary vectors of length N.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Output, integer BVEC_ENUM, the number of binary vectors.
c
      implicit none

      integer bvec_enum
      integer n
      integer value

      value = 2 ** n

      bvec_enum = value

      return
      end
      subroutine bvec_mul ( n, bvec1, bvec2, bvec3 )

c*********************************************************************72
c
cc BVEC_MUL computes the product of two binary vectors.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
c
c    Since the user may want to make calls like
c
c      call bvec_mul ( n, bvec1, bvec1, bvec3 )
c    or even
c      call bvec_mul ( n, bvec1, bvec1, bvec1 )
c
c    we need to copy the arguments, work on them, and then copy out the result.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, integer BVEC1(N), BVEC2(N), the vectors to be multiplied.
c
c    Output, integer BVEC3(N), the product of the two input vectors.
c
      implicit none

      integer n
      integer n_max
      parameter ( n_max = 100 )

      integer base
      parameter ( base = 2 )
      integer carry
      integer bvec1(n)
      integer bvec2(n)
      integer bvec3(n)
      integer bveca(n_max)
      integer bvecb(n_max)
      integer bvecc(n_max)
      integer i
      integer j
      integer product_sign

      if ( n_max .lt. n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BVEC_MUL - Fatal error!'
        write ( *, '(a)' ) '  Internal size limit N_MAX exceeded.'
        stop 1
      end if
c
c  Copy the input.
c
      do i = 1, n
        bveca(i) = bvec1(i)
      end do

      do i = 1, n
        bvecb(i) = bvec2(i)
      end do
c
c  Record the sign of the product.
c  Make the factors positive.
c
      product_sign = 1

      if ( bveca(n) .ne. 0 ) then
        product_sign = - product_sign
        call bvec_complement2 ( n, bveca, bveca )
      end if

      if ( bvecb(n) .ne. 0 ) then
        product_sign = - product_sign
        call bvec_complement2 ( n, bvecb, bvecb )
      end if

      do i = 1, n
        bvecc(i) = 0
      end do
c
c  Multiply.
c
      do i = 2, n
        do j = 2, n + 2 - i
          bvecc(j) = bvecc(j) + bveca(n+2-i) * bvecb(j+i-2)
        end do
      end do
c
c  Take care of carries.
c
      do i = n, 2, -1

        carry = bvecc(i) / base
        bvecc(i) = bvecc(i) - carry * base
c
c  Unlike the case of BVEC_ADD, we do NOT allow carries into
c  the sign position when multiplying.
c
        if ( 2 .lt. i ) then
          bvecc(i-1) = bvecc(i-1) + carry
        end if

      end do
c
c  Take care of the sign of the product.
c
      if ( product_sign .lt. 0 ) then
        call bvec_complement2 ( n, bvecc, bvecc )
      end if
c
c  Copy the output.
c
      do i = 1, n
        bvec3(i) = bvecc(i)
      end do

      return
      end
      subroutine bvec_next ( n, bvec )

c*********************************************************************72
c
cc BVEC_NEXT generates the next binary vector.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
c
c    The vectors have the order
c
c      (0,0,...,0),
c      (0,0,...,1),
c      ...
c      (1,1,...,1)
c
c    and the "next" vector after (1,1,...,1) is (0,0,...,0).  That is,
c    we allow wrap around.
c
c  Example:
c
c    N = 3
c
c    Input      Output
c    -----      ------
c    0 0 0  =>  0 0 1
c    0 0 1  =>  0 1 0
c    0 1 0  =>  0 1 1
c    0 1 1  =>  1 0 0
c    1 0 0  =>  1 0 1
c    1 0 1  =>  1 1 0
c    1 1 0  =>  1 1 1
c    1 1 1  =>  0 0 0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input/output, integer BVEC(N), on output, the successor to the
c    input vector.
c
      implicit none

      integer n

      integer bvec(n)
      integer i

      do i = n, 1, -1

        if ( bvec(i) == 0 ) then
          bvec(i) = 1
          return
        end if

        bvec(i) = 0

      end do

      return
      end
      subroutine bvec_next_grlex ( n, bvec )

c*********************************************************************72
c
cc BVEC_NEXT_GRLEX generates the next binary vector in GRLEX order.
c
c  Discussion:
c
c    N = 3
c
c    Input      Output
c    -----      ------
c    0 0 0  =>  0 0 1
c    0 0 1  =>  0 1 0
c    0 1 0  =>  1 0 0
c    1 0 0  =>  0 1 1
c    0 1 1  =>  1 0 1
c    1 0 1  =>  1 1 0
c    1 1 0  =>  1 1 1
c    1 1 1  =>  0 0 0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension.
c
c    Input, integer BVEC(N), the binary vector whose 
c    successor is desired.
c
c    Output, integer BVEC(N), the successor to the input vector.
c
      implicit none

      integer n

      integer bvec(n)
      integer i
      integer o
      integer s
      integer z
c
c  Initialize locations of 0 and 1.
c
      if ( bvec(1) .eq. 0 ) then
        z = 1
        o = 0
      else
        z = 0
        o = 1
      end if
c
c  Moving from right to left, search for a "1", preceded by a "0".
c
      do i = n, 2, -1
        if ( bvec(i) .eq. 1 ) then
          o = i
          if ( bvec(i-1) .eq. 0 ) then
            z = i - 1
            go to 10
          end if
        end if
      end do

10    continue
c
c  BVEC = 0
c
      if ( o .eq. 0 ) then
        bvec(n) = 1
c
c  01 never occurs.  So for sure, B(1) = 1.
c
      else if ( z .eq. 0 ) then
        s = sum ( bvec(1:n) )
        if ( s .eq. n ) then
          bvec(1:n) = 0
        else
          bvec(1:n-s-1) = 0
          bvec(n-s:n) = 1
        end if
c
c  Found the rightmost "01" string.
c  Replace it by "10".
c  Shift following 1's to the right.
c
      else
        bvec(z) = 1
        bvec(o) = 0
        s = sum ( bvec(o+1:n) )
        bvec(o+1:n-s) = 0
        bvec(n+1-s:n) = 1
      end if

      return
      end
      subroutine bvec_not ( n, bvec1, bvec2 )

c*********************************************************************72
c
cc BVEC_NOT "negates" or takes the 1's complement of a binary vector.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
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
c    Input, integer N, the length of the vectors.
c
c    Input, integer BVEC1(N), the vector to be negated.
c
c    Output, integer BVEC2(N), the negated vector.
c
      implicit none

      integer n

      integer base
      parameter ( base = 2 )
      integer bvec1(n)
      integer bvec2(n)
      integer i
 
      do i = 1, n
        bvec2(i) = ( base - 1 ) - bvec1(i)
      end do

      return
      end
      subroutine bvec_or ( n, bvec1, bvec2, bvec3 )

c*********************************************************************72
c
cc BVEC_OR computes the inclusive OR of two binary vectors.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
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
c    Input, integer N, the length of the vectors.
c
c    Input, integer BVEC1(N), BVEC2(N), the binary vectors.
c
c    Input, integer BVEC3(N), the inclusive OR of the two vectors.
c
      implicit none

      integer n

      integer bvec1(n)
      integer bvec2(n)
      integer bvec3(n)
      integer i

      do i = 1, n
        bvec3(i) = max ( bvec1(i), bvec2(i) )
      end do

      return
      end
      subroutine bvec_print ( n, bvec, title )

c*********************************************************************72
c
cc BVEC_PRINT prints a binary integer vector, with an optional title.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, integer BVEC(N), the vector to be printed.
c
c    Input, character*(*) TITLE, a title to be printed first.
c    TITLE may be blank.
c
      implicit none

      integer n

      integer bvec(n)
      integer ihi
      integer ilo
      integer i
      character * ( * ) title

      if ( 0 .lt. len_trim ( title ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) trim ( title )
      end if

      do ilo = 1, n, 70
        ihi = min ( ilo + 70 - 1, n )
        write ( *, '(2x,80i1)' ) ( bvec(i), i = ilo, ihi )
      end do

      return
      end
      subroutine bvec_reverse ( n, bvec1, bvec2 )

c*********************************************************************72
c
cc BVEC_REVERSE reverses a binary vector.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
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
c    Input, integer N, the length of the vectors.
c
c    Input, integer BVEC1(N), the vector to be reversed.
c
c    Output, integer BVEC2(N), the reversed vector.
c
      implicit none

      integer n

      integer bvec1(n)
      integer bvec2(n)
      integer i

      do i = 1, n
        bvec2(i) = bvec1(n+1-i)
      end do

      return
      end
      subroutine bvec_sub ( n, bvec1, bvec2, bvec3 )

c*********************************************************************72
c
cc BVEC_SUB subtracts two binary vectors.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
c
c  Example:
c
c    N = 4
c
c    BVEC1         BVEC2         BVEC3
c    -------       -------       -------
c    0 1 0 0   -   0 0 0 1   =   0 0 1 1
c          4             1             3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, integer BVEC1(N), BVEC2(N), the vectors to be subtracted.
c
c    Output, integer BVEC3(N), the value of BVEC1 - BVEC2.
c
      implicit none

      integer n
      integer n_max
      parameter ( n_max = 100 )

      integer bvec1(n)
      integer bvec2(n)
      integer bvec3(n)
      integer bvec4(n_max)

      if ( n_max .lt. n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BVEC_SUB - Fatal error!'
        write ( *, '(a)' ) '  Internal size limit N_MAX exceeded.'
        stop 1
      end if

      call bvec_complement2 ( n, bvec2, bvec4 )

      call bvec_add ( n, bvec1, bvec4, bvec3 )

      return
      end
      subroutine bvec_to_i4 ( n, bvec, i4 )

c*********************************************************************72
c
cc BVEC_TO_I4 makes an integer from a (signed) binary vector.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
c
c  Example:
c
c         BVEC   binary  I
c    ----------  -----  --
c    1  2  3  4
c    ----------
c    0  0  0  1       1  1
c    0  0  1  0      10  2
c    1  1  0  0    -100 -4
c    0  1  0  0     100  4
c    1  0  0  1    -111 -9
c    1  1  1  1      -0  0
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c
c    Input, integer BVEC(N), the binary representation.
c
c    Output, integer I4, the integer.
c
      implicit none

      integer n
      integer n_max
      parameter ( n_max = 100 )

      integer base
      parameter ( base = 2 )
      integer bvec(n)
      integer bvec2(n_max)
      integer i
      integer i_sign
      integer i4

      if ( n_max .lt. n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BVEC_TO_I4 - Fatal error!'
        write ( *, '(a)' ) '  Internal size limit N_MAX exceeded.'
        stop 1
      end if

      do i = 1, n
        bvec2(i) = bvec(i)
      end do

      if ( bvec2(1) .eq. base - 1 ) then
        i_sign = -1
        bvec2(1) = 0
        call bvec_complement2 ( n - 1, bvec2(2:n), bvec2(2:n) )
      else 
        i_sign = 1
      end if

      i4 = 0
      do i = 2, n
        i4 = base * i4 + bvec2(i)
      end do

      i4 = i_sign * i4

      return
      end
      subroutine bvec_uniform ( n, seed, bvec )

c*********************************************************************72
c
cc BVEC_UNIFORM returns a pseudorandom BVEC.
c
c  Discussion:
c
c    A BVEC is a vector of binary (0/1) values representing an integer.
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
c    Input, integer N, the order of the vector.
c
c    Input/output, integer SEED, the "seed" value, which should
c    NOT be 0.  On output, SEED has been updated.
c
c    Output, integer BVEC(N), a pseudorandom binary vector.
c
      implicit none

      integer n

      integer bvec(n)
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4_huge_half
      parameter ( i4_huge_half = 1073741823 )
      integer i
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'BVEC_UNIFORM - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge
        end if

        if ( i4_huge_half .lt. seed ) then
          bvec(i) = 0
        else
          bvec(i) = 1
        end if

      end do
 
      return
      end
      subroutine bvec_xor ( n, bvec1, bvec2, bvec3 )

c*********************************************************************72
c
cc BVEC_XOR computes the exclusive OR of two binary vectors.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
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
c    Input, integer N, the length of the vectors.
c
c    Input, integer BVEC1(N), BVEC2(N), the binary vectors to be XOR'ed.
c
c    Input, integer BVEC3(N), the exclusive OR of the two vectors.
c
      implicit none

      integer n

      integer bvec1(n)
      integer bvec2(n)
      integer bvec3(n)
      integer i

      do i = 1, n
        bvec3(i) = mod ( bvec1(i) + bvec2(i), 2 )
      end do

      return
      end
      function i4_bclr ( i4, pos )

c*********************************************************************72
c
cc I4_BCLR returns a copy of an I4 in which the POS-th bit is set to 0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Military Standard 1753,
c    FORTRAN, DoD Supplement To American National Standard X3.9-1978,
c    9 November 1978.
c
c  Parameters:
c
c    Input, integer I4, the integer.
c
c    Input, integer POS, the bit position, between 0 and 31.
c
c    Output, integer I4_BCLR, a copy of I4, but with the POS-th bit
c    set to 0.
c
      implicit none

      integer i4
      integer i4_bclr
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer j
      integer k
      integer pos
      integer sub
      integer value

      value = i4

      if ( pos .lt. 0 ) then

      else if ( pos .lt. 31 ) then

        sub = 1

        if ( 0 .le. i4 ) then
          j = i4
        else
          j = ( i4_huge + i4 ) + 1
        end if

        do k = 1, pos
          j = j / 2
          sub = sub * 2
        end do

        if ( mod ( j, 2 ) .eq. 1 ) then
          value = i4 - sub
        end if

      else if ( pos .eq. 31 ) then

        if ( i4 .lt. 0 ) then
          value = ( i4_huge + i4 ) + 1
        end if

      else if ( 31 .lt. pos ) then

        value = i4

      end if

      i4_bclr = value

      return
      end
      function i4_bset ( i4, pos )

c*********************************************************************72
c
cc I4_BSET returns a copy of an I4 in which the POS-th bit is set to 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Military Standard 1753,
c    FORTRAN, DoD Supplement To American National Standard X3.9-1978,
c    9 November 1978.
c
c  Parameters:
c
c    Input, integer I4, the integer to be tested.
c
c    Input, integer POS, the bit position, between 0 and 31.
c
c    Output, integer I4_BSET, a copy of I4, but with the POS-th bit
c    set to 1.
c
      implicit none

      integer add
      integer i4
      integer i4_bset
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer j
      integer k
      integer pos
      integer value

      value = i4

      if ( pos .lt. 0 ) then

      else if ( pos .lt. 31 ) then

        add = 1

        if ( 0 .le. i4 ) then
          j = i4
        else
          j = ( i4_huge + i4 ) + 1
        end if

        do k = 1, pos
          j = j / 2
          add = add * 2
        end do

        if ( mod ( j, 2 ) .eq. 0 ) then
          value = i4 + add
        end if

      else if ( pos .eq. 31 ) then

        if ( 0 .lt. i4 ) then
          value = - ( i4_huge - i4 ) - 1
        end if

      else if ( 31 .lt. pos ) then

        value = i4

      end if

      i4_bset = value

      return
      end
      function i4_btest ( i4, pos )

c*********************************************************************72
c
cc I4_BTEST returns TRUE if the POS-th bit of an I4 is 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Military Standard 1753,
c    FORTRAN, DoD Supplement To American National Standard X3.9-1978,
c    9 November 1978.
c
c  Parameters:
c
c    Input, integer I4, the integer to be tested.
c
c    Input, integer POS, the bit position, between 0 and 31.
c
c    Output, logical I4_BTEST, is TRUE if the POS-th bit is 1.
c
      implicit none

      integer i4
      logical i4_btest
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer j
      integer k
      integer pos

      if ( pos .lt. 0 ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_BTEST - Fatal error!'
        write ( *, '(a)' ) '  POS < 0.'
        stop 1

      else if ( pos .lt. 31 ) then

        if ( 0 .le. i4 ) then
          j = i4
        else
          j = ( i4_huge + i4 ) + 1
        end if

        do k = 1, pos
          j = j / 2
        end do

        if ( mod ( j, 2 ) .eq. 0 ) then
          i4_btest = .false.
        else
          i4_btest = .true.
        end if

      else if ( pos .eq. 31 ) then

        if ( i4 .lt. 0 ) then
          i4_btest = .true.
        else
          i4_btest = .false.
        end if

      else if ( 31 .lt. pos ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_BTEST - Fatal error!'
        write ( *, '(a)' ) '  31 < POS.'
        stop 1

      end if

      return
      end
      subroutine i4_to_bvec ( i4, n, bvec )

c*********************************************************************72
c
cc I4_TO_BVEC makes a signed binary vector from an I4.
c
c  Discussion:
c
c    A BVEC is a vector of binary digits representing an integer.  
c
c    BVEC(1) is 0 for positive values and 1 for negative values, which
c    are stored in 2's complement form.
c
c    For positive values, BVEC(N) contains the units digit, BVEC(N-1)
c    the coefficient of 2, BVEC(N-2) the coefficient of 4 and so on,
c    so that printing the digits in order gives the binary form of the number.
c
c    Negative values have a two's complement operation applied.
c
c    To guarantee that there will be enough space for any
c    value of I, it would be necessary to set N = 32.
c
c  Example:
c
c    I4       IVEC         binary
c    --  ----------------  ------
c     1  1  0  0  0  0  1      1
c     2  0  0  0  0  1  0     10
c     3  0  0  0  0  1  1     11
c     4  0  0  0  1  0  0    100
c     9  0  0  1  0  0  1   1001
c    -9  1  1  0  1  1  1  -1001 = 110111 (2's complement)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 March 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I4, an integer to be represented.
c
c    Input, integer N, the dimension of the vector.
c
c    Output, integer BVEC(N), the signed binary representation.
c
      implicit none

      integer n

      integer base
      parameter ( base = 2 )
      integer bvec(n)
      integer i4
      integer i4_copy
      integer j

      i4_copy = abs ( i4 )

      do j = n, 2, -1

        bvec(j) = mod ( i4_copy, base )

        i4_copy = i4_copy / base

      end do

      bvec(1) = 0

      if ( i4 .lt. 0 ) then
        call bvec_complement2 ( n, bvec, bvec )
      end if

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
