      function c4_normal_01 ( seed )

c*********************************************************************72
c
cc C4_NORMAL_01 returns a unit pseudonormal C4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, complex C4_NORMAL_01, a unit pseudonormal value.
c
      implicit none

      complex c4_normal_01
      real r4_uniform_01
      real r4_pi
      parameter ( r4_pi = 3.141592653589793E+00 )
      integer seed
      real v1
      real v2
      real x_c
      real x_r

      v1 = r4_uniform_01 ( seed )
      v2 = r4_uniform_01 ( seed )

      x_r = sqrt ( - 2.0E+00 * log ( v1 ) ) 
     &  * cos ( 2.0E+00 * r4_pi * v2 )
      x_c = sqrt ( - 2.0E+00 * log ( v1 ) ) 
     &  * sin ( 2.0E+00 * r4_pi * v2 )

      c4_normal_01 = cmplx ( x_r, x_c )

      return
      end
      function c8_normal_01 ( seed )

c*********************************************************************72
c
cc C8_NORMAL_01 returns a unit pseudonormal C8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double complex C8_NORMAL_01, a sample of the PDF.
c
      implicit none

      double complex c8_normal_01
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8_uniform_01
      integer seed
      double precision v1
      double precision v2
      double precision x_c
      double precision x_r

      v1 = r8_uniform_01 ( seed )
      v2 = r8_uniform_01 ( seed )

      x_r = sqrt ( - 2.0D+00 * log ( v1 ) ) 
     &  * cos ( 2.0D+00 * r8_pi * v2 )
      x_c = sqrt ( - 2.0D+00 * log ( v1 ) ) 
     &  * sin ( 2.0D+00 * r8_pi * v2 )

      c8_normal_01 = cmplx ( x_r, x_c )

      return
      end
      function i4_normal_ab ( a, b, seed )

c*********************************************************************72
c
cc I4_NORMAL_AB returns a scaled pseudonormal I4.
c
c  Discussion:
c
c    The normal probability distribution function (PDF) is sampled,
c    with mean A and standard deviation B.
c
c    The result is then rounded to the nearest integer.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A, the mean of the PDF.
c
c    Input, real B, the standard deviation of the PDF.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, integer I4_NORMAL_AB, a sample of the normal PDF.
c
      implicit none

      real a
      real b
      integer i4_normal_ab
      real r4_uniform_01
      real r1
      real r2
      real r4_pi
      parameter ( r4_pi = 3.141592653589793E+00 )
      integer seed
      real x

      r1 = r4_uniform_01 ( seed )
      r2 = r4_uniform_01 ( seed )
      x = sqrt ( - 2.0E+00 * log ( r1 ) ) 
     &  * cos ( 2.0E+00 * r4_pi * r2 )

      i4_normal_ab = nint ( a + b * x )

      return
      end
      function i8_normal_ab ( a, b, seed )

c*********************************************************************72
c
cc I8_NORMAL_AB returns a scaled pseudonormal I8.
c
c  Discussion:
c
c    The normal probability distribution function (PDF) is sampled,
c    with mean A and standard deviation B.
c
c    The result is then rounded to the nearest integer.
c
c    I changed my mind, and backed down from using an integer*8 as
c    the seed to r8_uniform_01, so this routine won't work until
c    I decide how to redo it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the mean of the PDF.
c
c    Input, double precision B, the standard deviation of the PDF.
c
c    Input/output, integer*8 SEED, a seed for the
c    random number generator.
c
c    Output, integer*8 I8_NORMAL_AB, a sample of the normal PDF.
c
      implicit none

      double precision a
      double precision b
      integer*8 i8_normal_ab
      double precision r1
      double precision r2
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8_uniform_01
      integer*8 seed
      double precision x

      r1 = r8_uniform_01 ( seed )
      r2 = r8_uniform_01 ( seed )
      x = sqrt ( - 2.0D+00 * log ( r1 ) ) 
     &  * cos ( 2.0D+00 * r8_pi * r2 )

      i8_normal_ab = nint ( a + b * x )

      return
      end
      function r4_normal_01 ( seed )

c*********************************************************************72
c
cc R4_NORMAL_01 returns a unit pseudonormal real R4.
c
c  Discussion:
c
c    The standard normal probability distribution function (PDF) has
c    mean 0 and standard deviation 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, real R4_NORMAL_01, a sample of the standard normal PDF.
c
      implicit none

      real r1
      real r2
      real r4_normal_01
      real r4_pi
      parameter ( r4_pi = 3.141592653589793E+00 )
      real r4_uniform_01
      integer seed
      real x

      r1 = r4_uniform_01 ( seed )
      r2 = r4_uniform_01 ( seed )
      x = sqrt ( -2.0E+00 * log ( r1 ) ) 
     &  * cos ( 2.0E+00 * r4_pi * r2 )

      r4_normal_01 = x

      return
      end
      function r4_normal_ab ( a, b, seed )

c*********************************************************************72
c
cc R4_NORMAL_AB returns a scaled pseudonormal R4.
c
c  Discussion:
c
c    The normal probability distribution function (PDF) is sampled,
c    with mean A and standard deviation B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A, the mean of the PDF.
c
c    Input, real B, the standard deviation of the PDF.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, real R4_NORMAL_AB, a sample of the normal PDF.
c
      implicit none

      real a
      real b
      real r1
      real r2
      real r4_normal_ab
      real r4_pi
      parameter ( r4_pi = 3.141592653589793E+00 )
      real r4_uniform_01
      integer seed
      real x

      r1 = r4_uniform_01 ( seed )
      r2 = r4_uniform_01 ( seed )
      x = sqrt ( - 2.0E+00 * log ( r1 ) ) 
     &  * cos ( 2.0E+00 * r4_pi * r2 )

      r4_normal_ab = a + b * x

      return
      end
      function r4_uniform_01 ( seed )

c*********************************************************************72
c
cc R4_UNIFORM_01 returns a unit pseudorandom R4.
c
c  Discussion:
c
c    This routine implements the recursion
c
c      seed = 16807 * seed mod ( 2^31 - 1 )
c      r4_uniform_01 = seed / ( 2^31 - 1 )
c
c    The integer arithmetic never requires more than 32 bits,
c    including a sign bit.
c
c    If the initial seed is 12345, then the first three computations are
c
c      Input     Output      R4_UNIFORM_01
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
c    17 July 2006
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
c    Output, real R4_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      integer i4_huge 
      parameter ( i4_huge = 2147483647 )
      integer k
      integer seed
      real r4_uniform_01

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge
      end if
c
c  Although SEED can be represented exactly as a 32 bit integer,
c  it generally cannot be represented exactly as a 32 bit real number!
c
      r4_uniform_01 = real ( dble ( seed ) * 4.656612875D-10 )

      return
      end
      subroutine r4vec_normal_01 ( n, seed, x )

c*********************************************************************72
c
cc R4VEC_NORMAL_01 returns a unit pseudonormal R4VEC.
c
c  Discussion:
c
c    The standard normal probability distribution function (PDF) has
c    mean 0 and standard deviation 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values desired.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, real X(N), a sample of the standard normal PDF.
c
c  Local parameters:
c
c    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
c    X that we need to compute.  This starts off as 1:N, but is adjusted
c    if we have a saved value that can be immediately stored in X(1),
c    and so on.
c
      implicit none

      integer n

      integer i
      integer m
      real r(2)
      real r4_pi
      parameter ( r4_pi = 3.141592653589793E+00 )
      real r4_uniform_01
      integer seed
      real x(n)
      integer x_hi_index
      integer x_lo_index
c
c  Record the range of X we need to fill in.
c
      x_lo_index = 1
      x_hi_index = n
c
c  If we need just one new value, do that here to avoid null arrays.
c
      if ( x_hi_index - x_lo_index + 1 .eq. 1 ) then

        r(1) = r4_uniform_01 ( seed )

        if ( r(1) .eq. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R4VEC_NORMAL_01 - Fatal errorc'
          write ( *, '(a)' ) '  R4_UNIFORM_01 returned a value of 0.'
          stop
        end if

        r(2) = r4_uniform_01 ( seed )

        x(x_hi_index) =
     &           sqrt ( -2.0E+00 * log ( r(1) ) )
     &           * cos ( 2.0E+00 * r4_pi * r(2) )
c
c  If we require an even number of values, that's easy.
c
      else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) .eq. 0 ) then

        do i = x_lo_index, x_hi_index, 2

          call r4vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0E+00 * log ( r(1) ) )
     &      * cos ( 2.0E+00 * r4_pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0E+00 * log ( r(1) ) )
     &      * sin ( 2.0E+00 * r4_pi * r(2) )

        end do
c
c  If we require an odd number of values, we generate an even number,
c  and handle the last pair specially, storing one in X(N), and
c  saving the other for later.
c
      else

        do i = x_lo_index, x_hi_index - 1, 2

          call r4vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0E+00 * log ( r(1) ) )
     &      * cos ( 2.0E+00 * r4_pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0E+00 * log ( r(1) ) )
     &      * sin ( 2.0E+00 * r4_pi * r(2) )

        end do

        call r4vec_uniform_01 ( 2, seed, r )

        x(n) = sqrt ( -2.0E+00 * log ( r(1) ) )
     &    * cos ( 2.0E+00 * r4_pi * r(1) )

      end if

      return
      end
      subroutine r4vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R4VEC_UNIFORM_01 returns a unit pseudorandom R4VEC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2006
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
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, real R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      integer seed
      real r(n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R4VEC_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge
        end if

        r(i) = real ( seed ) * 4.656612875E-10

      end do

      return
      end
      function r8_normal_01 ( seed )

c*********************************************************************72
c
cc R8_NORMAL_01 returns a unit pseudonormal R8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision R8_NORMAL_01, a sample of the standard normal PDF.
c
      implicit none

      double precision r1
      double precision r2
      double precision r8_normal_01
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8_uniform_01
      integer seed
      double precision x

      r1 = r8_uniform_01 ( seed )
      r2 = r8_uniform_01 ( seed )
      x = sqrt ( - 2.0D+00 * log ( r1 ) ) 
     &  * cos ( 2.0D+00 * r8_pi * r2 )

      r8_normal_01 = x

      return
      end
      function r8_normal_ab ( a, b, seed )

c*********************************************************************72
c
cc R8_NORMAL_AB returns a scaled pseudonormal R8.
c
c  Discussion:
c
c    The normal probability distribution function (PDF) is sampled,
c    with mean A and standard deviation B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, the mean of the PDF.
c
c    Input, double precision B, the standard deviation of the PDF.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision R8_NORMAL_AB, a sample of the normal PDF.
c
      implicit none

      double precision a
      double precision b
      double precision r1
      double precision r2
      double precision r8_normal_ab
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8_uniform_01
      integer seed
      double precision x

      r1 = r8_uniform_01 ( seed )
      r2 = r8_uniform_01 ( seed )
      x = sqrt ( -2.0D+00 * log ( r1 ) ) 
     &  * cos ( 2.0D+00 * r8_pi * r2 )

      r8_normal_ab = a + b * x

      return
      end
      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a unit pseudorandom R8.
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
c    17 July 2006
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

      integer i4_huge 
      parameter ( i4_huge = 2147483647 )
      integer k
      double precision r8_uniform_01
      integer seed

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge
      end if
c
c  Although SEED can be represented exactly as a 32 bit integer,
c  it generally cannot be represented exactly as a 32 bit real number!
c
      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine r8mat_normal_01 ( m, n, seed, r )

c*********************************************************************72
c
cc R8MAT_NORMAL_01 returns a unit pseudonormal R8MAT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 November 2010
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
c    Output, double precision R(M,N), the array of pseudonormal values.
c
      implicit none

      integer m
      integer n

      integer seed
      double precision r(m,n)

      call r8vec_normal_01 ( m * n, seed, r )

      return
      end
      subroutine r8mat_normal_ab ( m, n, a, b, seed, r )

c*********************************************************************72
c
cc R8MAT_NORMAL_AB returns a scaled pseudonormal R8MAT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 August 2013
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
c    Input, double precision A, B, the mean and standard deviation.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(M,N), the array of pseudonormal values.
c
      implicit none

      integer m
      integer n

      double precision a
      double precision b
      integer seed
      double precision r(m,n)

      call r8vec_normal_ab ( m * n, a, b, seed, r )

      return
      end
      subroutine r8mat_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_PRINT prints an R8MAT.
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
c    20 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character * ( * ) title

      call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R8MAT_PRINT_SOME prints some of an R8MAT.
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
c    25 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(g14.6)' ) a(i,j)

          end do

          write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

      end do

      return
      end
      subroutine r8vec_normal_01 ( n, seed, x )

c*********************************************************************72
c
cc R8VEC_NORMAL_01 returns a unit pseudonormal R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The standard normal probability distribution function (PDF) has
c    mean 0 and standard deviation 1.
c
c    This routine can generate a vector of values on one call.  It
c    has the feature that it should provide the same results
c    in the same order no matter how we break up the task.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values desired.  If N is negative,
c    then the code will flush its internal memory; in particular,
c    if there is a saved value to be used on the next call, it is
c    instead discarded.  This is useful if the user has reset the
c    random number seed, for instance.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision X(N), a sample of the standard normal PDF.
c
c  Local parameters:
c
c    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
c    X that we need to compute.  This starts off as 1:N, but is adjusted
c    if we have a saved value that can be immediately stored in X(1),
c    and so on.
c
      implicit none

      integer n

      integer i
      integer m
      double precision r(n+1)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8_uniform_01
      integer seed
      double precision x(n)
      integer x_hi_index
      integer x_lo_index
c
c  Record the range of X we need to fill in.
c
      x_lo_index = 1
      x_hi_index = n
c
c  Maybe we don't need any more values.
c
      if ( x_hi_index - x_lo_index + 1 .eq. 1 ) then

        r(1) = r8_uniform_01 ( seed )

        if ( r(1) .eq. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8VEC_NORMAL_01 - Fatal error!'
          write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
          stop 1
        end if

        r(2) = r8_uniform_01 ( seed )

        x(x_hi_index) =
     &           sqrt ( -2.0D+00 * log ( r(1) ) )
     &           * cos ( 2.0D+00 * r8_pi * r(2) )
c
c  If we require an even number of values, that's easy.
c
      else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) .eq. 0 ) then

        m = ( x_hi_index - x_lo_index + 1 ) / 2

        call r8vec_uniform_01 ( 2 * m, seed, r )

        do i = 1, 2 * m, 2

          x(x_lo_index+i-1) =
     &      sqrt ( -2.0D+00 * log ( r(i) ) )
     &      * cos ( 2.0D+00 * r8_pi * r(i+1) )

          x(x_lo_index+i) =
     &      sqrt ( -2.0D+00 * log ( r(i) ) )
     &      * sin ( 2.0D+00 * r8_pi * r(i+1) )

        end do
c
c  If we require an odd number of values, we generate an even number,
c  and handle the last pair specially, storing one in X(N), and
c  saving the other for later.
c
      else

        x_hi_index = x_hi_index - 1

        m = ( x_hi_index - x_lo_index + 1 ) / 2 + 1

        call r8vec_uniform_01 ( 2 * m, seed, r )

        do i = 1, 2 * m - 3, 2

          x(x_lo_index+i-1) =
     &      sqrt ( -2.0D+00 * log ( r(i) ) )
     &      * cos ( 2.0D+00 * r8_pi * r(i+1) )

          x(x_lo_index+i) =
     &      sqrt ( -2.0D+00 * log ( r(i) ) )
     &      * sin ( 2.0D+00 * r8_pi * r(i+1) )

        end do

        x(n) = sqrt ( -2.0D+00 * log ( r(2*m-1) ) )
     &    * cos ( 2.0D+00 * r8_pi * r(2*m) )

      end if

      return
      end
      subroutine r8vec_normal_ab ( n, a, b, seed, x )

c*********************************************************************72
c
cc R8VEC_NORMAL_AB returns a scaled pseudonormal R8VEC.
c
c  Discussion:
c
c    The standard normal probability distribution function (PDF) has
c    mean 0 and standard deviation 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 August 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values desired.
c
c    Input, double precision A, B, the mean and standard deviation.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision X(N), a sample of the standard normal PDF.
c
c  Local parameters:
c
c    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
c    X that we need to compute.  This starts off as 1:N, but is adjusted
c    if we have a saved value that can be immediately stored in X(1),
c    and so on.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      integer m
      double precision r(2)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8_uniform_01
      integer seed
      double precision x(n)
      integer x_hi_index
      integer x_lo_index
c
c  Record the range of X we need to fill in.
c
      x_lo_index = 1
      x_hi_index = n
c
c  If we need just one new value, do that here to avoid null arrays.
c
      if ( x_hi_index - x_lo_index + 1 .eq. 1 ) then

        r(1) = r8_uniform_01 ( seed )

        if ( r(1) .eq. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8VEC_NORMAL - Fatal errorc'
          write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
          stop
        end if

        r(2) = r8_uniform_01 ( seed )

        x(x_hi_index) =
     &           sqrt ( -2.0D+00 * log ( r(1) ) )
     &           * cos ( 2.0D+00 * r8_pi * r(2) )
c
c  If we require an even number of values, that's easy.
c
      else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) .eq. 0 ) then

        do i = x_lo_index, x_hi_index, 2

          call r8vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * cos ( 2.0D+00 * r8_pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * sin ( 2.0D+00 * r8_pi * r(2) )

        end do
c
c  If we require an odd number of values, we generate an even number,
c  and handle the last pair specially, storing one in X(N), and
c  saving the other for later.
c
      else

        do i = x_lo_index, x_hi_index - 1, 2

          call r8vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * cos ( 2.0D+00 * r8_pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * sin ( 2.0D+00 * r8_pi * r(2) )

        end do

        call r8vec_uniform_01 ( 2, seed, r )

        x(n) = sqrt ( -2.0D+00 * log ( r(1) ) )
     &    * cos ( 2.0D+00 * r8_pi * r(1) )

      end if

      do i = 1, n
        x(i) = a + b * x(i)
      end do

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
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
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do

      return
      end
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2006
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
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer i4_huge 
      parameter ( i4_huge = 2147483647 )
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

      end do

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
