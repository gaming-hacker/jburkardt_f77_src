      program main

c*********************************************************************72
c
cc MAIN is the main program for TOEPLITZ_PRB.
c
c  Discussion:
c
c    TOEPLITZ_PRB tests the TOEPLITZ library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOEPLITZ_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOEPLITZ library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOEPLITZ_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests CSLZ.
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
      implicit none

      integer n
      parameter ( n = 4 )

      double complex a(n)
      integer seed
      double complex work(n)
      double complex x(n)
      double complex x2(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  CSLZ solves a complex circulant system.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
c
c  Set the matrix.
c
      seed = 123456789
      call c8ci_random ( n, seed, a )

      call c8ci_print ( n, a, '  The circulant matrix:' )
c
c  Set the desired solution.
c
      call c8vec_indicator ( n, x )
c
c  Compute the corresponding right hand side.
c
      call c8ci_mxv ( n, a, x, x2 )
c
c  Solve the linear system.
c
      call cslz ( a, x2, work, n )
c
c  Print the solution.
c
      call c8vec_print ( n, x2, '  Solution:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests C4TO_SL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double complex a(2*n-1)
      double complex b(n)
      integer job
      integer seed
      double complex work(n)
      double complex x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  TSLZ solves a complex Toeplitz system.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
c
c  Set the matrix.
c
      seed = 123456789
      call c8to_random ( n, seed, a )

      call c8to_print ( n, a, '  The Toeplitz matrix:' )
c
c  Set the desired solution.
c
      call c8vec_indicator ( n, x )

      call c8vec_print ( n, x, '  Desired solution:' )
c
c  Compute the corresponding right hand side.
c
      call c8to_mxv ( n, a, x, b )

      call c8vec_print ( n, b, '  Right Hand Side:' )
c
c  Solve the linear system.
c
      call tslz ( a, b, work, n )

      call c8vec_print ( n, b, ' Solution:' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests R4TO_SL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(2*n-1)
      double precision b(n)
      integer seed
      double precision work(2*n-2)
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  TSLD solves a real Toeplitz system.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
!
!  Set the matrix.
!
      seed = 123456789
      call r8to_random ( n, seed, a )

      call r8to_print ( n, a, '  The Toeplitz matrix:' )
!
!  Set the desired solution.
!
      call r8vec_indicator ( n, x )
!
!  Compute the corresponding right hand side.
!
      call r8to_mxv ( n, a, x, b )
!
!  Solve the linear system.
!
      call tsld ( a, b, work, n )

      call r8vec_print ( n, b, '  Solution:' )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests R8BTO_MXV, R8BTO_VXM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer l
      parameter ( l = 3 )
      integer m
      parameter ( m = 2 )

      double precision a1(m,m,l)
      double precision a2(m,m,l-1)
      double precision b(m,l)
      double precision x(m,l)

      save a1
      save a2

      data a1 /
     &  1.0D+00, 5.0D+00, 2.0D+00, 5.0D+00, 
     &  3.0D+00, 6.0D+00, 4.0D+00, 6.0D+00, 
     &  5.0D+00, 7.0D+00, 6.0D+00, 7.0D+00 /
      data a2 /
     &  7.0D+00, 8.0D+00, 8.0D+00, 8.0D+00, 
     &  9.0D+00, 9.0D+00, 0.0D+00, 9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  For a real block Toeplitz matrix,'
      write ( *, '(a)' ) '  R8BTO_MXV computes A * x.'
      write ( *, '(a)' ) '  R8BTO_VXM computes x * A.'

      call r8bto_print ( m, l, a1, a2, '  The block Toeplitz matrix:' )

      call r8vec_indicator ( m*l, x )

      call r8vec_print ( m*l, x, '  The vector x:' )

      call r8bto_mxv ( m, l, a1, a2, x, b )

      call r8vec_print ( m*l, b, '  The product A*x:' )

      call r8bto_vxm ( m, l, a1, a2, x, b )

      call r8vec_print ( m*l, b, '  The product x*A:' )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests R4BTO_SL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer l
      parameter ( l = 3 )
      integer n
      parameter ( n = m * l )

      double precision a(m,m,2*l-1)
      double precision a1(m,m,l)
      double precision a2(m,m,l-1)
      double precision b(n)
      integer i
      integer j
      integer k
      integer lda
      double precision r(m*m*(2*l+3)+m)
      double precision x(n)

      save a1
      save a2

      data a1 /
     &  9.0E+00, 2.0E+00, 1.0E+00, 8.0E+00, 
     &  3.0E+00, 6.0E+00, 4.0E+00, 6.0E+00, 
     &  5.0E+00, 7.0E+00, 6.0E+00, 7.0E+00 /

      data a2 /
     &  7.0E+00, 8.0E+00, 8.0E+00, 8.0E+00, 
     &  9.0E+00, 9.0E+00, 0.0E+00, 9.0E+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  R8BTO_SL solves a block Toeplitz system.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n

      call r8bto_print ( m, l, a1, a2, '  The block Toeplitz matrix:' )
c
c  Set the desired solution.
c
      call r8vec_indicator ( n, x )
c
c  Compute the right hand side.
c
      call r8bto_mxv ( m, l, a1, a2, x, b )

      call r8vec_print ( n, b, '  Right hand side:' )
c
c  Rearrange the data to satisfy the conventions of TGSLD.
c
      do i = 1, n
        x(i) = b(i)
      end do

      do k = 1, l
        do j = 1, m
          do i = 1, m
            a(i,j,k) = a1(i,j,k)
          end do
        end do
      end do

      do k = 1, l - 1
        do j = 1, m
          do i = 1, m
            a(i,j,l+k) = a2(i,j,k)
          end do
        end do
      end do
 
      lda = m * m

      call tgsld ( a, x, r, m, l, lda )

      call r8vec_print ( n, x, '  Computed solution:' )

      return
      end
      subroutine c8ci_mxv ( n, a, x, b )

c*********************************************************************72
c
cc C8CI_MXV multiplies a C8 circulant matrix times a vector.
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
c    Input, integer N, the order of the matrix.
c
c    Input, double complex A(N), the entries of the first row 
c    of the circulant matrix.
c
c    Input, double complex X(N), the vector to be multiplied by A.
c
c    Output, double complex B(N), the product A * x.
c
      implicit none

      integer n

      double complex a(n)
      double complex b(n)
      integer i
      integer j
      double complex x(n)

      do i = 1, n

        b(i) = dcmplx ( 0.0D+00, 0.0D+00 )

        do j = 1, i - 1
          b(i) = b(i) + a(n+j+1-i) * x(j)
        end do

        do j = i, n
          b(i) = b(i) + a(j+1-i) * x(j)
        end do

      end do

      return
      end
      subroutine c8ci_print ( n, a, title )

c*********************************************************************72
c
cc C8CI_PRINT prints a C8 circulant matrix.
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
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input, double complex A(N), the N by N circulant matrix.
c
c    Input, character * ( * ) TITLE, a title to be printed.
c
      implicit none

      integer n

      double complex a(n)
      character * ( * ) title

      call c8ci_print_some ( n, a, 1, 1, n, n, title )

      return
      end
      subroutine c8ci_print_some ( n, a, ilo, jlo, ihi, jhi, title )

c*********************************************************************72
c
cc C8CI_PRINT_SOME prints some of a C8 circulant matrix.
c
c  Discussion:
c
c    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
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
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input, double complex A(N), the N by N circulant matrix.
c
c    Input, integer ILO, JLO, IHI, JHI, designate the first row and
c    column, and the last row and column to be printed.
c
c    Input, character * ( * ) TITLE, a title to be printed.
c
      implicit none

      integer incx
      parameter ( incx = 4 )
      integer n

      double complex a(n)
      double complex aij
      character * ( 20 ) ctemp(incx)
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
      write ( *, '(a)' ) ' '
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
          write ( ctemp(j2), '(i10,10x)' ) j
        end do

        write ( *, '(a,4a20)' ) 'Columns', ( ctemp(j2), j2 = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) '  ---'
c
c  Determine the range of the rows in this strip.
c
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, n )

        do i = i2lo, i2hi
c
c  Print out (up to) INCX entries in row I, that lie in the current strip.
c
          do j2 = 1, inc

            j = j2lo - 1 + j2

            if ( i .le. j ) then
              aij = a(j+1-i)
            else
              aij = a(n+j+1-i)
            end if

            write ( ctemp(j2), '(2g10.3)' ) aij

          end do

          write ( *, '(i5,1x,4a20)' ) i, ( ctemp(j2), j2 = 1, inc )

        end do

      end do

      return
      end
      subroutine c8ci_random ( n, seed, a )

c*********************************************************************72
c
cc C8CI_RANDOM randomizes a C8 circulant matrix.
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
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double complex A(N), the randomized matrix, with entries 
c    between 0 and 1.
c
      implicit none

      integer n

      double complex a(n)
      integer seed

      call c8vec_uniform_01 ( n, seed, a )

      return
      end
      subroutine c8to_mxv ( n, a, x, b )

c*********************************************************************72
c
cc C8TO_MXV multiplies a C8 Toeplitz matrix times a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double complex A(2*N-1), the entries of the first row of the 
c    Toeplitz matrix, followed by the entries of the first column, beginning
c    with the second row.
c
c    Input, double complex X(N), the vector to be multiplied by A.
c
c    Output, double complex B(N), the product A * x.
c
      implicit none

      integer n

      double complex a(2*n-1)
      double complex b(n)
      integer i
      integer j
      double complex x(n)

      do i = 1, n

        b(i) = dcmplx ( 0.0D+00, 0.0D+00 )

        do j = 1, i - 1
          b(i) = b(i) + a(n+i-j) * x(j)
        end do

        do j = i, n
          b(i) = b(i) + a(j+1-i) * x(j)
        end do

      end do

      return
      end
      subroutine c8to_print ( n, a, title )

c*********************************************************************72
c
cc C8TO_PRINT prints a C8 Toeplitz matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 September 2013
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
c    Input, double complex A(2*N-1), the N by N Toeplitz matrix.
c
c    Input, character * ( * ) TITLE, a title to be printed.
c
      implicit none

      integer ( kind = 4 ) n

      complex ( kind = 4 ) a(2*n-1)
      character ( len = * ) title

      call c8to_print_some ( n, a, 1, 1, n, n, title )

      return
      end
      subroutine c8to_print_some ( n, a, ilo, jlo, ihi, jhi, title )

c*********************************************************************72
c
cc C8TO_PRINT_SOME prints some of a C8 Toeplitz matrix.
c
c  Discussion:
c
c    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 September 2013
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
c    Input, double complex A(2*N-1), the N by N Toeplitz matrix.
c
c    Input, integer ILO, JLO, IHI, JHI, designate the first row and
c    column, and the last row and column to be printed.
c
c    Input, character * ( * ) TITLE, a title to be printed.
c
      implicit none

      integer incx
      parameter ( incx = 4 )
      integer n

      double complex a(2*n-1)
      double complex aij
      character * ( 20 ) ctemp(incx)
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
      write ( *, '(a)' ) ' '
c
c  Print the columns of the matrix, in strips of INCX.
c
      do j2lo = jlo, jhi, incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )
   
        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i10,10x)' ) j
        end do

        write ( *, '(a,4a20)' ) 'Columns', ( ctemp(j2), j2 = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) '  ---'
c
c  Determine the range of the rows in this strip.
c
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, n )

        do i = i2lo, i2hi
c
c  Print out (up to) INCX entries in row I, that lie in the current strip.
c
          do j2 = 1, inc

            j = j2lo - 1 + j2

            if ( i .le. j ) then
              aij = a(j+1-i)
            else
              aij = a(n+i-j)
            end if

            write ( ctemp(j2), '(2g10.3)' ) aij

          end do

          write ( *, '(i5,1x,4a20)' ) i, ( ctemp(j2), j2 = 1, inc )

        end do

      end do

      return
      end
      subroutine c8to_random ( n, seed, a )

c*********************************************************************72
c
cc C8TO_RANDOM randomizes a C8 Toeplitz matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 September 2013
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
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double complex A(2*N-1), the randomized matrix, with entries
c    between 0 and 1.
c
      implicit none

      integer n

      double complex a(2*n-1)
      integer seed

      call c8vec_uniform_01 ( 2 * n - 1, seed, a )

      return
      end
      subroutine c8vec_indicator ( n, a )

c*********************************************************************72
c
cc C8VEC_INDICATOR sets a C8VEC to the indicator vector.
c
c  Discussion:
c
c    A C8VEC is a vector of C8's
c
c    X(1:N) = ( 1-1i, 2-2i, 3-3i, 4-4i, ... )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 December 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of elements of A.
c
c    Output, double complex A(N), the array to be initialized.
c
      implicit none

      integer n

      double complex a(n)
      integer i

      do i = 1, n
        a(i) = dcmplx ( i, -i )
      end do

      return
      end
      subroutine c8vec_print ( n, a, title )

c*********************************************************************72
c
cc C8VEC_PRINT prints a C8VEC, with an optional title.
c
c  Discussion:
c
c    A C8VEC is a vector of C8's
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 December 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double complex A(N), the vector to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer n

      double complex a(n)
      integer i
      character*(*) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,2g14.6)' ) i, ':', a(i)
      end do

      return
      end
      subroutine c8vec_uniform_01 ( n, seed, c )

c*********************************************************************72
c
cc C8VEC_UNIFORM_01 returns a unit pseudorandom C8VEC.
c
c  Discussion:
c
c    A C8VEC is a vector of C8's
c
c    The angles should be uniformly distributed between 0 and 2 * PI,
c    the square roots of the radius uniformly distributed between 0 and 1.
c
c    This results in a uniform distribution of values in the unit circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 December 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values to compute.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double complex C(N), the pseudorandom complex vector.
c
      implicit none

      integer n

      double complex c(n)
      integer i
      integer k
      double precision r
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      integer seed
      double precision theta

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'C8VEC_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
      end if

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        r = dsqrt ( dble ( seed ) * 4.656612875D-10 )

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        theta = 2.0D+00 * r8_pi * ( dble ( seed ) * 4.656612875D-10 )

        c(i) = r * dcmplx ( dcos ( theta ), dsin ( theta ) )

      end do

      return
      end
      subroutine r8bto_mxv ( m, l, a1, a2, x, b )

c*********************************************************************72
c
cc R8BTO_MXV computes the real block Toeplitz matrix product A * X = B.
c
c  Discussion:
c
c    The full matrix has order M * L, and can be regarded
c    as comprising L by L blocks.  Each block is of order M.
c
c    Example:
c
c      M = 2, L = 3
c
c      1 2 | 3 4 | 5 6
c      5 5 | 6 6 | 7 7
c      ----+-----+-----
c      7 8 | 1 2 | 3 4
c      8 8 | 5 5 | 6 6
c      ----+-----+-----
c      9 0 | 7 8 | 1 2
c      9 9 | 8 8 | 5 5
c
c    X = (/ 1, 2, 3, 4, 5, 6 /)
c
c    B = (/ 91, 134, 73, 125, 97, 129 /)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 September 2013
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Oleg Arushanian, MK Samarin, Valentin Voevodin, Evgeny Tyrtyshnikov,
c    Burton Garbow, James Boyle, Wayne Cowell, Kenneth Dritz,
c    The TOEPLITZ Package User's Guide,
c    Argonne National Laboratory,
c    ANL-83-16, 1983.
c
c  Parameters:
c
c    Input, integer M, the order of the blocks of the matrix A.
c
c    Input, integer L, the number of blocks in a row or column 
c    of A.
c
c    Input, double precision A1(M,M,L), the M*M by L matrix containing the 
c    first row of blocks of the matrix.  There are L blocks, and each is of 
c    order M*M.
c
c    Input, double precision A2(M,M,L-1), the M*M by L-1 matrix containing the 
c    first column of blocks of the matrix, beginning with the second block.
c
c    Input, double precision X(M*L), the vector to be multiplied.
c
c    Output, double precision B(M*L), the product vector, A * X.
c
      implicit none

      integer l
      integer m

      double precision a1(m,m,l)
      double precision a2(m,m,l-1)
      double precision b(m,l)
      integer i
      integer j
      integer k
      integer k2
      double precision x(m,l)
c
c  Construct the right hand side by blocks.
c
      do i = 1, l

        do k = 1, m
          b(k,i) = 0.0D+00
        end do

        do j = 1, i - 1
          do k = 1, m
            do k2 = 1, m
              b(k,i) = b(k,i) + a2(k,k2,i-j) * x(k2,j)
            end do
          end do
        end do

        do j = i, l
          do k = 1, m
            do k2 = 1, m
              b(k,i) = b(k,i) + a1(k,k2,j+1-i) * x(k2,j)
            end do
          end do
        end do

      end do

      return
      end
      subroutine r8bto_print ( m, l, a1, a2, title )

c*********************************************************************72
c
cc R8BTO_PRINT prints a block Toeplitz matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the order of the blocks of the matrix A.
c
c    Input, integer L, the number of blocks in a row or column 
c    of A.
c
c    Input, double precision A1(M,M,L), the M*M by L matrix containing the 
c    first row of blocks of the matrix.  There are L blocks, and each is of 
c    order M*M.
c
c    Input, double precision A2(M,M,L-1), the M*M by L-1 matrix containing the 
c    first column of blocks of the matrix, beginning with the second block.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer l
      integer m

      double precision a1(m,m,l)
      double precision a2(m,m,l-1)
      character * ( * ) title

      call r8bto_print_some ( m, l, a1, a2, 1, 1, m*l, m*l, title )

      return
      end
      subroutine r8bto_print_some ( m, l, a1, a2, ilo, jlo, ihi, jhi, 
     &  title )

c*********************************************************************72
c
cc R8BTO_PRINT_SOME prints some of a block Toeplitz matrix.
c
c  Discussion:
c
c    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the order of the blocks of the matrix A.
c
c    Input, integer L, the number of blocks in a row or column
c    of A.
c
c    Input, double precision A1(M,M,L), the M*M by L matrix containing the 
c    first row of blocks of the matrix.  There are L blocks, and each is of 
c    order M*M.
c
c    Input, double precision A2(M,M,L-1), the M*M by L-1 matrix containing 
c    the first column of blocks of the matrix, beginning with the second block.
c
c    Input, integer ILO, JLO, IHI, JHI, designate the first row and
c    column, and the last row and column to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer l
      integer m

      double precision a1(m,m,l)
      double precision a2(m,m,l-1)
      double precision aij
      character * ( 14 ) ctemp(incx)
      integer i
      integer i1
      integer i2
      integer i3hi
      integer i3lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j1
      integer j2
      integer j3
      integer j3hi
      integer j3lo
      integer jhi
      integer jlo
      integer n
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '

      n = m * l
c
c  Print the columns of the matrix, in strips of 5.
c
      do j3lo = jlo, jhi, incx

        j3hi = j3lo + incx - 1
        j3hi = min ( j3hi, n )
        j3hi = min ( j3hi, jhi )

        inc = j3hi + 1 - j3lo

        write ( *, '(a)' ) ' '

        do j = j3lo, j3hi
          j3 = j + 1 - j3lo
          write ( ctemp(j3), '(i7,7x)' ) j
        end do

        write ( *, '(''Columns:'',5a14)' ) ( ctemp(j3), j3 = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) '  ---'
c
c  Determine the range of the rows in this strip.
c
        i3lo = max ( ilo, 1 )
        i3hi = min ( ihi, n )

        do i = i3lo, i3hi
c
c  Print out (up to) 5 entries in row I, that lie in the current strip.
c
          do j3 = 1, inc

            j = j3lo - 1 + j3
c
c  i = M * ( i1 - 1 ) + i2
c  j = M * ( j1 - 1 ) + j2
c
            i1 = ( i - 1 ) / m + 1
            i2 = i - m * ( i1 - 1 )
            j1 = ( j - 1 ) / m + 1
            j2 = j - m * ( j1 - 1 )

            if ( i1 .le. j1 ) then
              aij = a1(i2,j2,j1+1-i1)
           else
              aij = a2(i2,j2,i1-j1)
           end if

            write ( ctemp(j3), '(g14.6)' ) aij

          end do

          write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j3), j3 = 1, inc )

        end do

      end do

      write ( *, '(a)' ) ' '

      return
      end
      subroutine r8bto_vxm ( m, l, a1, a2, x, b )

c*********************************************************************72
c
cc R8BTO_VXM computes the real block Toeplitz matrix product X * A = B.
c
c  Discussion:
c
c    The full matrix has order M * L, and can be regarded
c    as comprising L by L blocks.  Each block is of order M.
c
c    Example:
c
c      M = 2, L = 3
c
c      1 2 | 3 4 | 5 6
c      5 5 | 6 6 | 7 7
c      ----+-----+-----
c      7 8 | 1 2 | 3 4
c      8 8 | 5 5 | 6 6
c      ----+-----+-----
c      9 0 | 7 8 | 1 2
c      9 9 | 8 8 | 5 5
c
c    X = (/ 1, 2, 3, 4, 5, 6 /)
c
c    B = (/ ? /)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the order of the blocks of the matrix A.
c
c    Input, integer L, the number of blocks in a row or column 
c    of A.
c
c    Input, double precision A1(M,M,L), the M*M by L matrix containing the 
c    first row of blocks of the matrix.  There are L blocks, and each is of 
c    order M*M.
c
c    Input, double precision A2(M,M,L-1), the M*M by L-1 matrix containing the
c    first column of blocks of the matrix, beginning with the second block.
c
c    Input, double precision X(M*L), the vector to be multiplied.
c
c    Output, double precision B(M*L), the product vector, X * A.
c
      implicit none

      integer l
      integer m

      double precision a1(m,m,l)
      double precision a2(m,m,l-1)
      double precision b(m,l)
      integer i
      integer j
      integer k
      integer k2
      double precision x(m,l)
c
c  Construct the right hand side by blocks.
c
      do i = 1, l

        do k = 1, m
          b(k,i) = 0.0D+00
        end do

        do j = 1, i
          do k = 1, m
            do k2 = 1, m
              b(k,i) = b(k,i) + a1(k2,k,i+1-j) * x(k2,j)
            end do
          end do
        end do

        do j = i + 1, l
          do k = 1, m
            do k2 = 1, m
              b(k,i) = b(k,i) + a2(k2,k,j-i) * x(k2,j)
            end do
          end do
        end do

      end do

      return
      end
      subroutine r8to_mxv ( n, a, x, b )

c*********************************************************************72
c
cc R8TO_MXV multiplies a Toeplitz matrix times a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(2*N-1), the entries of the first row of the 
c    Toeplitz matrix, followed by the entries of the first column, beginning
c    with the second row.
c
c    Input, double precision X(N), the vector to be multiplied by A.
c
c    Output, double precision B(N), the product A * x.
c
      implicit none

      integer n

      double precision a(2*n-1)
      double precision b(n)
      integer i
      integer j
      double precision x(n)

      do i = 1, n
        b(i) = 0.0D+00
        do j = 1, i - 1
          b(i) = b(i) + a(n+i-j) * x(j)
        end do
        do j = i, n
          b(i) = b(i) + a(j+1-i) * x(j)
        end do
      end do

      return
      end
      subroutine r8to_print ( n, a, title )

c*********************************************************************72
c
cc R8TO_PRINT prints a Toeplitz matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 September 2013
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
c    Input, double precision A(2*N-1), the N by N Toeplitz matrix.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(2*n-1)
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '

      call r8to_print_some ( n, a, 1, 1, n, n )

      return
      end
      subroutine r8to_print_some ( n, a, ilo, jlo, ihi, jhi )

c*********************************************************************72
c
cc R8TO_PRINT_SOME prints some of a Toeplitz matrix.
c
c  Discussion:
c
c    Only entries in rows ILO to IHI, columns JLO to JHI are considered.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 September 2013
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
c    Input, double precision A(2*N-1), the N by N Toeplitz matrix.
c
c    Input, integer ILO, JLO, IHI, JHI, designate the first row and
c    column, and the last row and column to be printed.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer n

      double precision a(2*n-1)
      double precision aij
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

        write ( *, '(a,5a14)' ) 'Columns', ( ctemp(j2), j2 = 1, inc )
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
              aij = a(j+1-i)
            else
              aij = a(n+i-j)
            end if

            write ( ctemp(j2), '(g14.6)' ) aij

          end do

          write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j2), j2 = 1, inc )

        end do

      end do

      write ( *, '(a)' ) ' '

      return
      end
      subroutine r8to_random ( n, seed, a )

c*********************************************************************72
c
cc R8TO_RANDOM randomizes a Toeplitz matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 September 2013
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
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision A(2*N-1), the randomized matrix, with entries 
c    between 0 and 1.
c
      implicit none

      integer n

      double precision a(2*n-1)
      integer seed

      call r8vec_uniform_01 ( 2 * n - 1, seed, a )

      return
      end
      subroutine r8vec_indicator ( n, a )

c*********************************************************************72
c
cc R8VEC_INDICATOR sets an R8VEC to the indicator vector.
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
c    22 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of elements of A.
c
c    Output, double precision A(N), the array to be initialized.
c
      implicit none

      integer n

      double precision a(n)
      integer i

      do i = 1, n
        a(i) = dble ( i )
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
      character ( len = * ) title

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

