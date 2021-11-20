      subroutine haar_1d ( n, x )

c*********************************************************************72
c
cc HAAR_1D computes the Haar transform of a vector.
c
c  Discussion:
c
c    For the classical Haar transform, N should be a power of 2.
c    However, this is not required here.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c
c    Input/output, double precision X(N), on input, the vector to be transformed.
c    On output, the transformed vector.
c
      implicit none

      integer n

      integer i
      integer k
      double precision s
      double precision x(n)
      double precision y(n)

      s = sqrt ( 2.0D+00 )
c
c  Initialize.
c
      do i = 1, n
        y(i) = 0.0D+00
      end do
c
c  Determine K, the largest power of 2 such that K <= N.
c
      k = 1

10    continue

      if ( k * 2 .le. n ) then
        k = k * 2
        go to 10
      end if

20    continue
  
      if ( 1 .lt. k ) then

        k = k / 2

        do i = 1, k
          y(i)   = ( x(2*i-1) + x(2*i) ) / s
          y(i+k) = ( x(2*i-1) - x(2*i) ) / s
        end do

        do i = 1, 2 * k
          x(i) = y(i)
        end do

        go to 20

      end if

      return
      end
      subroutine haar_1d_inverse ( n, x )

c*********************************************************************72
c
cc HAAR_1D_INVERSE computes the inverse Haar transform of a vector.
c
c  Discussion:
c
c    For the classical Haar transform, N should be a power of 2.
c    However, this is not required here.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.  For proper calculation,
c
c    Input/output, double precision X(N), on input, the vector to be transformed.
c    On output, the transformed vector.
c
      implicit none

      integer n

      integer i
      integer k
      double precision s
      double precision x(n)
      double precision y(n)

      s = sqrt ( 2.0D+00 )
c
c  Initialize.
c
      do i = 1, n
        y(i) = 0.0D+00
      end do

      k = 1

10    continue

      if ( k * 2 .le. n ) then

        do i = 1, k
          y(2*i-1) = ( x(i) + x(i+k) ) / s
          y(2*i)   = ( x(i) - x(i+k) ) / s
        end do

        do i = 1, 2 * k
          x(i) = y(i)
        end do

        k = k * 2

        go to 10

      end if

      return
      end
      subroutine haar_2d ( m, n, u )

c*********************************************************************72
c
cc HAAR_2D computes the Haar transform of an array.
c
c  Discussion:
c
c    For the classical Haar transform, M and N should be powers of 2.
c    However, this is not required here.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the dimensions of the array.
c
c    Input/output, double precision U(M,N), the array to be transformed.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      integer k
      double precision s
      double precision u(m,n)
      double precision v(m,n)

      s = sqrt ( 2.0D+00 )

      do j = 1, n
        do i = 1, m
          v(i,j) = u(i,j)
        end do
      end do
c
c  Determine K, the largest power of 2 such that K <= M.
c
      k = 1

10    continue

      if ( k * 2 .le. m ) then
        k = k * 2
        go to 10
      end if
c
c  Transform all columns.
c
20    continue

      if ( 1 .lt. k ) then
      
        k = k / 2

        do j = 1, n
          do i = 1, k
            v(  i,j) = ( u(2*i-1,j) + u(2*i,j) ) / s
            v(k+i,j) = ( u(2*i-1,j) - u(2*i,j) ) / s
          end do
        end do

        do j = 1, n
          do i = 1, 2 * k
            u(i,j) = v(i,j)
          end do
        end do

        go to 20

      end if
c
c  Determine K, the largest power of 2 such that K <= N.
c
      k = 1

30    continue

      if ( k * 2 .le. n ) then
        k = k * 2
        go to 30
      end if
c
c  Transform all rows.
c
40    continue

      if ( 1 .lt. k ) then
      
        k = k / 2

        do j = 1, k
          do i = 1, m
            v(i,  j) = ( u(i,2*j-1) + u(i,2*j) ) / s
            v(i,k+j) = ( u(i,2*j-1) - u(i,2*j) ) / s
          end do
        end do

        do j = 1, 2 * k
          do i = 1, m
            u(i,j) = v(i,j)
          end do
        end do

        go to 40

      end if

      return
      end
      subroutine haar_2d_inverse ( m, n, u )

c*********************************************************************72
c
cc HAAR_2D_INVERSE inverts the Haar transform of an array.
c
c  Discussion:
c
c    For the classical Haar transform, M and N should be powers of 2.
c    However, this is not required here.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the dimensions of the array.
c
c    Input/output, double precision U(M,N), the array to be transformed.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      integer k
      double precision s
      double precision u(m,n)
      double precision v(m,n)

      s = sqrt ( 2.0D+00 )

      do j = 1, n
        do i = 1, m
          v(i,j) = u(i,j)
        end do
      end do
c
c  Inverse transform of all rows.
c
      k = 1

10    continue

      if ( k * 2 .le. n ) then

        do j = 1, k
          do i = 1, m
            v(i,2*j-1) = ( u(i,j) + u(i,k+j) ) / s
            v(i,2*j  ) = ( u(i,j) - u(i,k+j) ) / s
          end do
        end do

        do j = 1, 2 * k
          do i = 1, m
            u(i,j) = v(i,j)
          end do
        end do

        k = k * 2

        go to 10

      end if
c
c  Inverse transform of all columns.
c
      k = 1

20    continue

      if ( k * 2 .le. m ) then

        do j = 1, n
          do i = 1, k
            v(2*i-1,j) = ( u(i,j) + u(k+i,j) ) / s
            v(2*i,  j) = ( u(i,j) - u(k+i,j) ) / s
          end do
        end do

        do j = 1, n
          do i = 1, 2 * k
            u(i,j) = v(i,j)
          end do
        end do

        k = k * 2

        go to 20

      end if

      return
      end
      subroutine r8mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_COPY copies an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the order of the matrix.
c
c    Input, double precision A1(M,N), the matrix to be copied.
c
c    Output, double precision A2(M,N), a copy of the matrix.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
      end do

      return
      end
      function r8mat_diff_frobenius ( m, n, a, b )

c*********************************************************************72
c
cc R8MAT_DIFF_FROBENIUS: Frobenius norm of the difference of two R8MAT's.
c
c  Discussion:
c
c    An R8MAT is a matrix of double precision values.
c
c    The Frobenius norm is defined as
c
c      R8MAT_DIFF_FROBENIUS = sqrt (
c        sum ( 1 <= I <= M ) sum ( 1 <= j <= N ) ( A(I,J) - B(I,J) )^2 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 June 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in A and B.
c
c    Input, integer N, the number of columns in A and B.
c
c    Input, double precision A(M,N), B(M,N), the matrices
c    for which we want the Frobenius norm of the difference.
c
c    Output, double precision R8MAT_DIFF_FROBENIUS, the Frobenius norm of
c    the difference of A and B.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision b(m,n)
      integer i
      integer j
      double precision r8mat_diff_frobenius
      double precision value

      value = 0.0D+00
      do j = 1, n
        do i = 1, m
          value = value + ( a(i,j) - b(i,j) )**2
        end do
      end do

      value = sqrt ( value )

      r8mat_diff_frobenius = value

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
      integer j
      integer k
      integer seed
      double precision r(m,n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      do j = 1, n

        do i = 1, m

          k = seed / 127773

          seed = 16807 * ( seed - k * 127773 ) - k * 2836

          if ( seed .lt. 0 ) then
            seed = seed + 2147483647
          end if

          r(i,j) = dble ( seed ) * 4.656612875D-10

        end do
      end do

      return
      end
      subroutine r8vec_copy ( n, a1, a2 )

c*********************************************************************72
c
cc R8VEC_COPY copies an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, double precision A1(N), the vector to be copied.
c
c    Output, double precision A2(N), a copy of A1.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i

      do i = 1, n
        a2(i) = a1(i)
      end do

      return
      end
      function r8vec_diff_norm ( n, a, b )

c*********************************************************************72
c
cc R8VEC_DIFF_NORM returns the L2 norm of the difference of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    The vector L2 norm is defined as:
c
c      R8VEC_NORM_L2 = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in A.
c
c    Input, double precision A(N), B(N), the vectors.
c
c    Output, double precision R8VEC_DIFF_NORM, the L2 norm of A - B.
c
      implicit none

      integer n

      double precision a(n)
      double precision b(n)
      integer i
      double precision r8vec_diff_norm
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + ( a(i) - b(i) )**2
      end do
      value = sqrt ( value )

      r8vec_diff_norm = value

      return
      end
      subroutine r8vec_linspace ( n, a_first, a_last, a )

c*********************************************************************72
c
cc R8VEC_LINSPACE returns a vector of linearly spaced values.
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
c    14 March 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A_FIRST, A_LAST, the first and last entries of A.
c
c    Output, double precision A(N), the vector.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_first
      double precision a_last
      integer i

      if ( n .eq. 1 ) then
        a(1) = ( a_first + a_last ) / 2.0D+00
      else
  
        do i = 1, n
          a(i) = ( dble ( n - i     ) * a_first 
     &           + dble (     i - 1 ) * a_last )
     &           / dble ( n     - 1 )
        end do

      end if

      return
      end
      subroutine r8vec_ones ( n, a )

c*********************************************************************72
c
cc R8VEC_ONES returns a vector of 1's.
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
c    14 March 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Output, double precision A(N), the vector of 1's.
c
      implicit none

      integer n

      double precision a(n)
      integer i

      do i = 1, n
        a(i) = 1.0D+00
      end do

      return
      end
      subroutine r8vec_transpose_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_TRANSPOSE_PRINT prints an R8VEC "transposed".
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Example:
c
c    A = (/ 1.0, 2.1, 3.2, 4.3, 5.4, 6.5, 7.6, 8.7, 9.8, 10.9, 11.0 /)
c    TITLE = 'My vector:  '
c
c    My vector:
c
c        1.0    2.1    3.2    4.3    5.4
c        6.5    7.6    8.7    9.8   10.9
c       11.0
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
      integer ihi
      integer ilo
      character * ( * )  title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do ilo = 1, n, 5
        ihi = min ( ilo + 5 - 1, n )
        write ( *, '(5g14.6)' ) a(ilo:ihi)
      end do

      return
      end
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
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
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
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



