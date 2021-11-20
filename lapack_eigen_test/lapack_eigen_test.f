      program main

c*********************************************************************72
c
cc MAIN is the main program for LAPACK_EIGEN_TEST.
c
c  Discussion:
c
c    LAPACK_EIGEN_TEST tests some real symmetric eigenproblem routines.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer n

      call timestamp ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LAPACK_EIGEN_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test some of the LAPACK routines for'
      write ( *, '(a)' ) '  real symmetric eigenproblems.'

      n = 1

      do i = 1, 4
        n = n * 4
        call dsyev_test ( n )
      end do

      n = 1

      do i = 1, 4
        n = n * 4
        call dsyevd_test ( n )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LAPACK_EIGEN_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)') ' '
      call timestamp ( )

      stop 0
      end
      subroutine dsyev_test ( n )

c*********************************************************************72
c
cc DSYEV_TEST tests DSYEV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n

      double precision a(n,n)
      double precision aq(n,n)
      integer i
      integer info
      integer j
      character jobz
      double precision lambda(n)
      double precision lambda2(n)
      double precision lambda_dev
      parameter ( lambda_dev = 1.0D+00 )
      double precision lambda_max
      double precision lambda_mean
      parameter ( lambda_mean = 1.0D+00 )
      double precision lambda_min
      integer lwork
      double precision q(n,n)
      double precision q2(n,n)
      double precision r(n,n)
      double precision r8vec_max
      double precision r8vec_min
      double precision s
      integer seed
      double precision t1
      double precision t2
      double precision time_setup
      double precision time_solve
      character uplo
      double precision work(3*n-1)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DSYEV_TEST'
      write ( *, '(a)' ) '  DSYEV computes eigenvalues and eigenvectors'
      write ( *, '(a)' ) '  for a double precision real matrix (D)'
      write ( *, '(a)' ) '  in symmetric storage mode (SY)'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Matrix order = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' )     '  Random number SEED = ', seed
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  R8SYMM_GEN will give us a symmetric matrix'
      write ( *, '(a)' ) '  with known eigenstructure.'

      call cpu_time ( t1 )

      call r8symm_gen ( n, lambda_mean, lambda_dev, seed, a, q, lambda )

      call cpu_time ( t2 )

      time_setup = t2 - t1

      if ( n .le. 5 ) then

        call r8mat_print ( n, n, a, '  The matrix A:' )

        call r8mat_print ( n, n, q, '  The eigenvector matrix Q:' )

      end if

      lambda_min = r8vec_min ( n, lambda )
      lambda_max = r8vec_max ( n, lambda )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  LAMBDA_MIN = ', lambda_min
      write ( *, '(a,g14.6)' ) '  LAMBDA_MAX = ', lambda_max

      if ( n .le. 10 ) then

        call r8vec_print ( n, lambda, '  The eigenvalues LAMBDA:' )

      end if
c
c  Verify the claim that A*Q = Q * LAMBDA.
c
      if ( n .le. 5 ) then

        call r8mat_mm ( n, n, n, a, q, aq )

        do j = 1, n
          s = 0.0D+00
          do i = 1, n
            s = s + aq(i,j)**2
          end do
          lambda2(j) = sqrt ( s )
        end do

        call r8vec_print ( n, lambda2, '  The column norms of A*Q:' )

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now call DSYEV'
      write ( *, '(a)' ) '  and see if it can recover Q and LAMBDA.'
c
c  Copy the matrix.
c
      do j = 1, n
        do i = 1, n
          q2(i,j) = a(i,j)
        end do
      end do

      jobz = 'V'
      uplo = 'U'
      lwork = 3 * n - 1

      call cpu_time ( t1 )

      call dsyev ( jobz, uplo, n, q2, n, lambda2, work, lwork, info )

      call cpu_time ( t2 )

      time_solve = t2 - t1

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DSYEV_TEST - Warning!'
        write ( *, '(a,i6)' ) '  DSYEV returned error INFO = ', info
        return
      end if

      lambda_min = r8vec_min ( n, lambda2 )
      lambda_max = r8vec_max ( n, lambda2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  LAMBDA_MIN = ', lambda_min
      write ( *, '(a,g14.6)' ) '  LAMBDA_MAX = ', lambda_max

      if ( n .le. 10 ) then
        call r8vec_print ( n, lambda2, '  Computed eigenvalues:' )
      end if

      if ( jobz .eq. 'V' ) then

        if ( n .le. 5 ) then

          call r8mat_print ( n, n, q2, '  The eigenvector matrix:' )

          call r8mat_mm ( n, n, n, a, q2, r )
 
          do j = 1, n
            do i = 1, n
              r(i,j) = r(i,j) - lambda2(j) * q2(i,j)
            end do
          end do

          call r8mat_print ( n, n, r, '  The residual (A-Lambda*I)*X:' )

        end if

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Setup time = ', time_setup
      write ( *, '(a,g14.6)' ) '  Solve time = ', time_solve

      return
      end
      subroutine dsyevd_test ( n )

c*********************************************************************72
c
cc DSYEVD_TEST tests DSYEVD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n

      double precision a(n,n)
      double precision aq(n,n)
      integer i
      integer info
      integer iwork(3+5*n)
      integer j
      character jobz
      double precision lambda(n)
      double precision lambda2(n)
      double precision lambda_dev
      parameter ( lambda_dev = 1.0D+00 )
      double precision lambda_max
      double precision lambda_mean
      parameter ( lambda_mean = 1.0D+00 )
      double precision lambda_min
      integer liwork
      integer lwork
      double precision q(n,n)
      double precision q2(n,n)
      double precision r(n,n)
      double precision r8vec_max
      double precision r8vec_min
      double precision s
      integer seed
      double precision t1
      double precision t2
      double precision time_setup
      double precision time_solve
      character uplo
      double precision work(1+6*n+2*n*n)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DSYEVD_TEST'
      write ( *, '(a)' ) '  DSYEVD gets eigenvalues and eigenvectors'
      write ( *, '(a)' ) '  for a double precision real matrix (D)'
      write ( *, '(a)' ) '  in symmetric storage mode (SY)'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  Matrix order = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' )     '  Random number SEED = ', seed
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  R8SYMM_GEN will give us a symmetric matrix'
      write ( *, '(a)' ) '  with known eigenstructure.'

      call cpu_time ( t1 )

      call r8symm_gen ( n, lambda_mean, lambda_dev, seed, a, q, lambda )

      call cpu_time ( t2 )

      time_setup = t2 - t1

      if ( n .le. 5 ) then

        call r8mat_print ( n, n, a, '  The matrix A:' )

        call r8mat_print ( n, n, q, '  The eigenvector matrix Q:' )

      end if

      lambda_min = r8vec_min ( n, lambda )
      lambda_max = r8vec_max ( n, lambda )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  LAMBDA_MIN = ', lambda_min
      write ( *, '(a,g14.6)' ) '  LAMBDA_MAX = ', lambda_max

      if ( n .le. 10 ) then

        call r8vec_print ( n, lambda, '  The eigenvalues:' )

      end if
c
c  Verify the claim that A*Q = Q * LAMBDA.
c
      if ( n .le. 5 ) then

        call r8mat_mm ( n, n, n, a, q, aq )

        do j = 1, n
          s = 0.0D+00
          do i = 1, n
            s = s + aq(i,j)**2
          end do
          lambda2(j) = sqrt ( s )
        end do

        call r8vec_print ( n, lambda2, '  The column norms of A*Q:' )

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Now call DSYEVD'
      write ( *, '(a)' ) '  and see if it can recover Q and LAMBDA.'
c
c  Copy the matrix.
c
      do j = 1, n
        do i = 1, n
          q2(i,j) = a(i,j)
        end do
      end do

      jobz = 'V'
      uplo = 'U'
      lwork = 1 + 6 * n + 2 * n * n
      liwork = 3 + 5 * n

      call cpu_time ( t1 )

      call dsyevd ( jobz, uplo, n, q2, n, lambda2, work, lwork, 
     &  iwork, liwork, info )

      call cpu_time ( t2 )

      time_solve = t2 - t1

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DSYEVD_TEST - Warning!'
        write ( *, '(a,i6)' ) '  DSYEVD returned error INFO = ', info
        return
      end if

      lambda_min = r8vec_min ( n, lambda2 )
      lambda_max = r8vec_max ( n, lambda2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  LAMBDA_MIN = ', lambda_min
      write ( *, '(a,g14.6)' ) '  LAMBDA_MAX = ', lambda_max

      if ( n .le. 10 ) then
        call r8vec_print ( n, lambda2, '  The computed eigenvalues:' )
      end if

      if ( jobz .eq. 'V' ) then

        if ( n .le. 5 ) then
          call r8mat_print ( n, n, q2, '  The eigenvector matrix:' )

          call r8mat_mm ( n, n, n, a, q2, r )

          do j = 1, n
             do i = 1, n
              r(i,j) = r(i,j) - lambda2(j) * q2(i,j)
            end do
          end do

          call r8mat_print ( n, n, r, '  The residual (A-Lambda*I)*X:' )

        end if

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Setup time = ', time_setup
      write ( *, '(a,g14.6)' ) '  Solve time = ', time_solve

      return
      end
      subroutine r8mat_mm ( n1, n2, n3, a, b, c )

c*********************************************************************72
c
cc R8MAT_MM multiplies two R8MAT's.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c    In FORTRAN90, this operation is more efficiently done by the
c    command:
c
c      C(1:N1,1:N3) = MATMUL ( A(1:N1,1;N2), B(1:N2,1:N3) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, N3, the order of the matrices.
c
c    Input, double precision A(N1,N2), B(N2,N3), the matrices to multiply.
c
c    Output, double precision C(N1,N3), the product matrix C = A * B.
c
      implicit none

      integer n1
      integer n2
      integer n3

      double precision a(n1,n2)
      double precision b(n2,n3)
      double precision c(n1,n3)
      double precision c1(n1,n3)
      integer i
      integer j
      integer k

      do i = 1, n1
        do j = 1, n3
          c1(i,j) = 0.0D+00
          do k = 1, n2
            c1(i,j) = c1(i,j) + a(i,k) * b(k,j)
          end do
        end do
      end do

      do j = 1, n3
        do i = 1, n1
          c(i,j) = c1(i,j)
        end do
      end do

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
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer j
      integer k
      integer seed
      double precision r(m,n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do j = 1, n

        do i = 1, m

          k = seed / 127773

          seed = 16807 * ( seed - k * 127773 ) - k * 2836

          if ( seed .lt. 0 ) then
            seed = seed + i4_huge
          end if

          r(i,j) = dble ( seed ) * 4.656612875D-10

        end do
      end do

      return
      end
      function r8vec_max ( n, a )

c*********************************************************************72
c
cc R8VEC_MAX returns the maximum value in an R8VEC.
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
c    12 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision R8VEC_MAX, the value of the largest entry.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8_huge
      parameter ( r8_huge = 1.79769313486231571D+308 )
      double precision r8vec_max
      double precision value

      value = - r8_huge
      do i = 1, n
        value = max ( value, a(i) )
      end do

      r8vec_max = value

      return
      end
      function r8vec_min ( n, a )

c*********************************************************************72
c
cc R8VEC_MIN returns the minimum value in an R8VEC.
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
c    18 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision R8VEC_MIN, the value of the smallest entry.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8_huge
      parameter ( r8_huge = 1.79769313486231571D+308 )
      double precision r8vec_min
      double precision value

      value = r8_huge
      do i = 1, n
        value = min ( value, a(i) )
      end do

      r8vec_min = value

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
