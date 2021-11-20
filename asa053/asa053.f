      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a pseudorandom R8 scaled to [0,1].
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

      double precision r8_uniform_01
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

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
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character ( len = * ) title

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
c    Input, character ( len = * ) TITLE, a title.
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
      subroutine r8pp_print ( n, a, title )

c*********************************************************************72
c
cc R8PP_PRINT prints an R8PP matrix.
c
c  Discussion:
c
c    The R8PP storage format is appropriate for a symmetric positive
c    definite matrix.  Only the upper triangle of the matrix is stored,
c    by successive partial columns, in an array of length (N*(N+1))/2,
c    which contains (A11,A12,A22,A13,A23,A33,A14,...,ANN)  
c
c    R8PP storage is used by LINPACK and LAPACK.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 May 2000
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input, double precision A((N*(N+1))/2), the R8PP matrix.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a((n*(n+1))/2)
      character ( len = * ) title

      call r8pp_print_some ( n, a, 1, 1, n, n, title )

      return
      end
      subroutine r8pp_print_some ( n, a, ilo, jlo, ihi, jhi, title )

c*********************************************************************72
c
cc R8PP_PRINT_SOME prints some of an R8PP matrix.
c
c  Discussion:
c
c    The R8PP storage format is appropriate for a symmetric positive
c    definite matrix.  Only the upper triangle of the matrix is stored,
c    by successive partial columns, in an array of length (N*(N+1))/2,
c    which contains (A11,A12,A22,A13,A23,A33,A14,...,ANN)  
c
c    R8PP storage is used by LINPACK and LAPACK.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 March 2001
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input, double precision A((N*(N+1))/2), the R8PP matrix.
c
c    Input, integer ILO, JLO, IHI, JHI, the first row and
c    column, and the last row and column to be printed.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer, parameter :: incx = 5
      integer n

      double precision a((n*(n+1))/2)
      double precision aij
      character ( len = 14 ) ctemp(incx)
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
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
c
c  Print the columns of the matrix, in strips of 5.
c
      do j2lo = jlo, jhi, incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)' ) j
        end do

        write ( *, '(a,5a14)' ) '  Col: ', ( ctemp(j2), j2 = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) '  ---'
c
c  Determine the range of the rows in this strip.
c
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, n )

        do i = i2lo, i2hi
c
c  Print out (up to) 5 entries in row I, that lie in the current strip.
c
          do j2 = 1, inc

            j = j2lo - 1 + j2

            if ( i .le. j ) then
              aij = a(i+(j*(j-1))/2)
            else
              aij = a(j+(i*(i-1))/2)
            end if

            write ( ctemp(j2), '(g14.6)' ) aij

          end do

          write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j2), j2 = 1, inc )

        end do

      end do

      return
      end
      subroutine r8utp_print ( n, a, title )

c*********************************************************************72
c
cc R8UTP_PRINT prints an R8UTP matrix.
c
c  Discussion:
c
c    The R8UTP storage format is appropriate for an upper triangular
c    matrix.  Only the upper triangle of the matrix is stored,
c    by successive partial columns, in an array of length (N*(N+1))/2,
c    which contains (A11,A12,A22,A13,A23,A33,A14,...,ANN)  
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    16 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input, double precision A((N*(N+1))/2), the R8UTP matrix.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a((n*(n+1))/2)
      character ( len = * ) title

      call r8utp_print_some ( n, a, 1, 1, n, n, title )

      return
      end
      subroutine r8utp_print_some ( n, a, ilo, jlo, ihi, jhi, title )

c*********************************************************************72
c
cc R8UTP_PRINT_SOME prints some of an R8UTP matrix.
c
c  Discussion:
c
c    The R8UTP storage format is appropriate for an upper triangular
c    matrix.  Only the upper triangle of the matrix is stored,
c    by successive partial columns, in an array of length (N*(N+1))/2,
c    which contains (A11,A12,A22,A13,A23,A33,A14,...,ANN)  
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    16 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input, double precision A((N*(N+1))/2), the R8UTP matrix.
c
c    Input, integer ILO, JLO, IHI, JHI, the first row and
c    column, and the last row and column to be printed.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer, parameter :: incx = 5
      integer n

      double precision a((n*(n+1))/2)
      double precision aij
      character ( len = 14 ) ctemp(incx)
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
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
c
c  Print the columns of the matrix, in strips of 5.
c
      do j2lo = jlo, jhi, incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)' ) j
        end do

        write ( *, '(a,5a14)' ) '  Col: ', ( ctemp(j2), j2 = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) '  ---'
c
c  Determine the range of the rows in this strip.
c
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, n )

        do i = i2lo, i2hi
c
c  Print out (up to) 5 entries in row I, that lie in the current strip.
c
          do j2 = 1, inc

            j = j2lo - 1 + j2

            if ( i .le. j ) then
              aij = a(i+(j*(j-1))/2)
            else
              aij = 0.0D+00
            end if

            write ( ctemp(j2), '(g14.6)' ) aij

          end do

          write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j2), j2 = 1, inc )

        end do

      end do

      return
      end
      subroutine rnorm ( seed, u1, u2 )

c*********************************************************************72
c
cc RNORM returns two independent standard random normal deviates.
c
c  Discussion:
c
c    This routine sets U1 and U2 to two independent standardized 
c    random normal deviates.   This is a version of the 
c    method given in Knuth.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2014
c
c  Author:
c
c    Original FORTRAN77 version by William Smith, Ronald Hocking.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Donald Knuth,
c    The Art of Computer Programming,
c    Volume 2, Seminumerical Algorithms,
c    Third Edition,
c    Addison Wesley, 1997,
c    ISBN: 0201896842,
c    LC: QA76.6.K64.
c
c  Parameters:
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision U1, U2, two standard random normal deviates.
c
      implicit none

      double precision r8_uniform_01
      double precision s
      integer seed
      double precision u1
      double precision u2
      double precision x
      double precision y

10    continue

      x = r8_uniform_01 ( seed )
      y = r8_uniform_01 ( seed )

      x = 2.0D+00 * x - 1.0D+00
      y = 2.0D+00 * y - 1.0D+00
      s = x * x + y * y

      if ( 1.0D+00 .lt. s ) then
        go to 10
      end if

      s = sqrt ( - 2.0D+00 * log ( s ) / s )
      u1 = x * s
      u2 = y * s

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
      subroutine wshrt ( d, n, np, nnp, seed, sb, sa )

c*********************************************************************72
c
cc WSHRT returns a random Wishart variate.
c
c  Discussion:
c
c    This routine is a Wishart variate generator.  
c
c    On output, SA is an upper-triangular matrix of size NP * NP,
c    written in linear form, column ordered, whose elements have a 
c    Wishart(N, SIGMA) distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2014
c
c  Author:
c
c    Original FORTRAN77 version by William Smith, Ronald Hocking.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    William Smith, Ronald Hocking,
c    Algorithm AS 53, Wishart Variate Generator,
c    Applied Statistics,
c    Volume 21, Number 3, pages 341-345, 1972.
c
c  Parameters:
c
c    Input, double precision D(NNP), the upper triangular array that
c    represents the Cholesky factor of the correlation matrix SIGMA.
c    D is stored in column-major form.
c
c    Input, integer N, the number of degrees of freedom.
c    1 <= N <= NP.
c
c    Input, integer NP, the size of variables.
c
c    Input, integer NNP, the value (NP*(NP+1))/2.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Workspace, double precision SB(NNP).
c
c    Output, double precision SA(NNP), a sample from the Wishart distribution.
c
      implicit none

      integer nnp

      double precision c
      double precision d(nnp)
      double precision df
      integer i
      integer ii
      integer ip
      integer j
      integer k
      integer n
      integer np
      integer nq
      integer nr
      integer ns
      double precision rn
      double precision sa(nnp)
      double precision sb(nnp)
      integer seed
      double precision u1
      double precision u2

      k = 1
c
c  Load SB with independent normal (0, 1) variates.
c
10    continue

      if ( k .le. nnp ) then

        call rnorm ( seed, u1, u2 )

        sb(k) = u1
        k = k + 1

        if ( k .le. nnp ) then
          sb(k) = u2
          k = k + 1
        end if

        go to 10

      end if
c
c  Load diagonal elements with square root of chi-square variates.
c
      ns = 0

      do i = 1, np
c
c  The original text read "DF = N - I + 1".
c  This should read "DF = NP - I + 1".
c
        df = dble ( np - i + 1 )
        ns = ns + i
        u1 = 2.0D+00 / ( 9.0D+00 * df )
        u2 = 1.0D+00 - u1
        u1 = sqrt ( u1 )
c
c  Wilson-Hilferty formula for approximating chi-square variates:
c  The original code did not take the absolute value!
c
        sb(ns) = sqrt ( df * abs ( ( u2 + sb(ns) * u1 ) ** 3 ) )

      end do

      rn = dble ( n )
      nr = 1

      do i = 1, np
        nr = nr + i - 1
        do j = i, np
          ip = nr
          nq = ( j * ( j - 1 ) ) / 2 + i - 1
          c = 0.0D+00
          do k = i, j
            ip = ip + k - 1
            nq = nq + 1
            c = c + sb(ip) * d(nq)
          end do
          sa(ip) = c
        end do
      end do

      do i = 1, np
        ii = np - i + 1
        nq = nnp - np
        do j = 1, i
          ip = ( ii * ( ii - 1 ) ) / 2
          c = 0.0D+00
          do k = i, np
            ip = ip + 1
            nq = nq + 1
            c = c + sa(ip) * sa(nq)
          end do
          sa(nq) = c / rn
          nq = nq - 2 * np + i + j - 1
        end do
      end do

      return
      end
