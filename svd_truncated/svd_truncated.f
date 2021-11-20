      program main

c*********************************************************************72
c
cc MAIN is the main program for SVD_TRUNCATED.
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
      implicit none

      integer m
      integer n
      integer seed

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SVD_TRUNCATED:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Demonstrate the use of the truncated or economy-size'
      write ( *, '(a)' ) 
     &  '  Singular Value Decomposition (SVD) for cases where'
      write ( *, '(a)' ) '  the sizes of M and N are very different.'

      m = 4
      n = 3
      seed = 123456789
      call svd_truncated_u_test ( m, n, seed )

      m = 3
      n = 4
      seed = 123456789
      call svd_truncated_v_test ( m, n, seed )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SVD_TRUNCATED:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
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
        stop 1
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
      subroutine svd_truncated_u_test ( m, n, seed )

c*********************************************************************72
c
cc SVD_TRUNCATED_U_TEST tests SVD_TRUNCATED_U.
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
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision a_save(m,n)
      double precision err
      integer i
      integer info
      integer j
      integer k
      integer seed
      double precision sn(n)
      double precision un(m,n)
      double precision v(n,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SVD_TRUNCATED_U_TEST'
      write ( *, '(a,i8)' ) '  M = ', m
      write ( *, '(a,i8)' ) '  N = ', n

      call r8mat_uniform_01 ( m, n, seed, a_save )

      call r8mat_print ( m, n, a_save, '  A:' )

      do j = 1, n
        do i = 1, m
          a(i,j) = a_save(i,j)
        end do
      end do

      call svd_truncated_u ( m, n, a, un, sn, v )
c
c  Check the factorization by computing A = U * S * V'
c
      do j = 1, n
        do i = 1, m
          a(i,j) = 0.0D+00
          do k = 1, n
            a(i,j) = a(i,j) + un(i,k) * sn(k) * v(k,j)
          end do
        end do
      end do

      err = 0.0D+00
      do j = 1, n
        do i = 1, m
          err = max ( err, abs ( a(i,j) - a_save(i,j) ) )
        end do
      end do

      write ( *, '(a)' ) ' '

      write ( *, '(a,g14.6)' ) '  Maximum error |A - U*S*V''| = ', err

      call r8mat_print ( m, n, a, '  Recomputed A = U * S * V'':' )

      return
      end
      subroutine svd_truncated_u ( m, n, a, un, sn, v )

c*********************************************************************72
c
cc SVD_TRUNCATED_U computes the SVD when N <= M.
c
c  Discussion:
c
c    A(mxn) = U(mxm)  * S(mxn)  * V(nxn)'
c           = Un(mxn) * Sn(nxn) * V(nxn)'
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 March 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of A.
c
c    Input, double precision A(M,N), the matrix to be decomposed.
c
c    Output, double precision UN(M,N), the first N left singular vectors.
c
c    Output, double precision SN(N), the first N singular values.
c
c    Output, double precision V(N,N), the right singular vectors.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer info
      character jobu
      character jobv
      integer lda
      integer ldu
      integer ldv
      integer lwork
      double precision sn(n)
      double precision un(m,n)
      double precision v(n,n)
      double precision work(5*n+m)

      if ( m .lt. n ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SVD_TRUNCATED_U - Fatal error!'
        write ( *, '(a)' ) '  Only call this function when N <= M.'
        write ( *, '(a,i8)' ) '  But we have M = ', m
        write ( *, '(a,i8)' ) '  and N = ', n
        stop
      end if

      jobu = 's'
      jobv = 'a'
      lda = m
      ldu = m
      ldv = n
      lwork = 5 * n + m

      call dgesvd ( jobu, jobv, m, n, a, lda, sn, un, ldu, v, ldv, 
     &  work, lwork, info )

      if ( info .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SVD_TRUNCATED_U:'
        write ( *, '(a)' ) '  DGESVD computation was successful.'
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SVD_TRUNCATED_U - Warning!'
        write ( *, '(a,i8)' ) '  DGESVD returned INFO = ', info
      end if

      return
      end
      subroutine svd_truncated_v_test ( m, n, seed )

c*********************************************************************72
c
cc SVD_TRUNCATED_V_TEST tests SVD_TRUNCATED_V.
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
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision a_save(m,n)
      double precision err
      integer i
      integer info
      integer j
      integer k
      integer seed
      double precision sm(m)
      double precision u(m,m)
      double precision vm(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SVD_TRUNCATED_V_TEST'
      write ( *, '(a,i8)' ) '  M = ', m
      write ( *, '(a,i8)' ) '  N = ', n

      call r8mat_uniform_01 ( m, n, seed, a_save )

      call r8mat_print ( m, n, a_save, '  A:' )

      do j = 1, n
        do i = 1, m
          a(i,j) = a_save(i,j)
        end do
      end do

      call svd_truncated_v ( m, n, a, u, sm, vm )
c
c  Check the factorization by computing A = U * S * V'
c
      do j = 1, n
        do i = 1, m
          a(i,j) = 0.0D+00
          do k = 1, m
            a(i,j) = a(i,j) + u(i,k) * sm(k) * vm(k,j)
          end do
        end do
      end do

      err = 0.0D+00
      do j = 1, n
        do i = 1, m
          err = max ( err, abs ( a(i,j) - a_save(i,j) ) )
        end do
      end do

      write ( *, '(a)' ) ' '

      write ( *, '(a,g14.6)' ) '  Maximum error |A - U*S*V''| = ', err

      call r8mat_print ( m, n, a, '  Recomputed A = U * S * V'':' )

      return
      end
      subroutine svd_truncated_v ( m, n, a, u, sm, vm )

c*********************************************************************72
c
cc SVD_TRUNCATED_V computes the SVD when M <= N.
c
c  Discussion:
c
c    A(mxn) = U(mxm) * S(mxn)  * V(nxn)'
c           = U(mxm) * Sm(mxm) * Vm(mxn)'
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
c    Input, integer M, N, the number of rows and columns of A.
c
c    Input, double precision A(M,N), the matrix to be decomposed.
c
c    Output, double precision U(M,M), the left singular vectors.
c
c    Output, double precision SM(M), the first M singular values.
c
c    Output, double precision VM(M,N), the first M right singular vectors.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer info
      character jobu
      character jobv
      integer lda
      integer ldu
      integer ldv
      integer lwork
      double precision sm(m)
      double precision u(m,m)
      double precision vm(m,n)
      double precision work(5*m+n)

      if ( n .lt. m ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SVD_TRUNCATED_V - Fatal error!'
        write ( *, '(a)' ) '  Only call this function when M <= N.'
        write ( *, '(a,i8)' ) '  But we have M = ', m
        write ( *, '(a,i8)' ) '  and N = ', n
        stop
      end if

      jobu = 'a'
      jobv = 's'
      lda = m
      ldu = m
      ldv = m
      lwork = 5 * m + n

      call dgesvd ( jobu, jobv, m, n, a, lda, sm, u, ldu, vm, ldv, 
     &  work, lwork, info )

      if ( info .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SVD_TRUNCATED_V:'
        write ( *, '(a)' ) '  DGESVD computation was successful.'
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SVD_TRUNCATED_V - Warning!'
        write ( *, '(a,i8)' ) '  DGESVD returned INFO = ', info
      end if

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
