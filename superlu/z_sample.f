      program main

c*********************************************************************72
c
cc Z_SAMPLE is a sample calling program for SUPERLU.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer m
      parameter ( m = n )
      integer ncc
      parameter ( ncc = 12 )

      double complex acc(ncc)
      double complex b(n)
      double complex b2(n)
      integer ccc(n+1)
      integer factors(8)
      integer i
      integer icc(ncc)
      integer info
      integer iopt
      integer ldb
      integer nrhs

      save acc
      save ccc
      save icc

      data acc /
     & ( 19.0, 0.0 ), ( 12.0, 0.0 ), ( 12.0, 0.0 ), 
     & ( 21.0, 0.0 ), ( 12.0, 0.0 ), ( 12.0, 0.0 ), 
     & ( 21.0, 0.0 ), ( 16.0, 0.0 ), 
     & ( 21.0, 0.0 ), (  5.0, 0.0 ), 
     & ( 21.0, 0.0 ), ( 18.0, 0.0 ) /
      data ccc /
     &  1, 4, 7, 9, 11, 13 /
      data icc /
     &  1, 2, 5,
     &  2, 3, 5, 
     &  1, 3, 
     &  1, 4, 
     &  4, 5 /

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Z_SAMPLE'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  ZGSSV factors and solves a linear system'
      write ( *, '(a)' ) '  using double precision complex arithmetic.'

      write ( *, '(a,i6)' ) '  Matrix order N = ', n
      write ( *, '(a,i6)' ) '  Matrix nonzeros ncc = ', ncc
c
c  Print the matrix.
c
      call cc_print ( m, n, ncc, icc, ccc, acc, 
     &  '  1-based CC matrix SAMPLE:' )

      nrhs = 1
      ldb = n
      do i = 1, n
        b(i) = ( 1.0E+00, 0.0E+00 )
      end do
c
c  Factor the matrix.
c
      iopt = 1
      call c_fortran_zgssv ( iopt, n, ncc, nrhs, acc, icc,
     &  ccc, b, ldb, factors, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Z_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  Factorization failed'
        write ( *, '(a,i4)' ) '  INFO = ', info
        stop 1
      end if

      write ( *, '(a)' ) '  Factorization succeeded.'
c
c  Solve the factored system.
c
      iopt = 2
      call c_fortran_zgssv ( iopt, n, ncc, nrhs, acc, icc,
     &  ccc, b, ldb, factors, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Z_SAMPLE - Fatal error!'
        write ( *, '(a)' ) '  Backsolve failed'
        write ( *, '(a,i4)' ) '  INFO = ', info
        stop 1
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Computed solution:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2g14.6)' ) b(i)
      end do
c
c  B now contains the solution X.
c  Set B2 = A * X.
c
      call cc_mv ( m, n, ncc, icc, ccc, acc, b, b2 )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Product A*X:'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2g14.6)' ) b2(i)
      end do
c
c  Free memory.
c
      iopt = 3
      call c_fortran_zgssv ( iopt, n, ncc, nrhs, acc, icc,
     &  ccc, b, ldb, factors, info )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Z_SAMPLE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine cc_mv ( m, n, ncc, icc, ccc, acc, x, b )

c*********************************************************************72
c
cc CC_MV multiplies a CC matrix by a vector
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Iain Duff, Roger Grimes, John Lewis,
c    User's Guide for the Harwell-Boeing Sparse Matrix Collection,
c    October 1992
c
c  Parameters:
c
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, integer NCC, the number of CC values.
c
c    Input, integer ICC(NCC), the CC rows.
c
c    Input, integer CCC(N+1), the compressed CC columns
c
c    Input, double complex ACC(NCC), the CC values.
c
c    Input, double complex X(N), the vector to be multiplied.
c
c    Output, double complex B(M), the product A*X.
c
      implicit none

      integer m
      integer n
      integer ncc

      double complex acc(ncc)
      double complex b(m)
      integer ccc(n+1)
      integer i
      integer icc(ncc)
      integer j
      integer k
      double complex x(n)

      do i = 1, m
        b(i) = 0.0E+00
      end do

      do j = 1, n
        do k = ccc(j), ccc(j+1) - 1
          i = icc(k)
          b(i) = b(i) + acc(k) * x(j)
        end do
      end do

      return
      end
      subroutine cc_print ( m, n, ncc, icc, ccc, acc, title )

c*********************************************************************72
c
cc CC_PRINT prints a sparse matrix in CC format.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in the matrix.
c
c    Input, integer N, the number of columns in the matrix.
c
c    Input, integer NCC, the number of CC elements.
c
c    Input, integer ICC(NCC), the CC rows.
c
c    Input, integer CCC(N+1), the compressed CC columns.
c
c    Input, double complex ACC(NCC), the CC values.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n
      integer ncc

      double complex acc(ncc)
      integer ccc(n+1)
      integer i
      integer icc(ncc)
      integer j
      integer jnext
      integer k
      integer m
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) '     #     I     J       Ar                Ai'
      write ( *, '(a)' ) 
     &  '  ----  ----  ----  --------------  --------------'
      write ( *, '(a)' ) ' '

      j = 1
      jnext = ccc(2)

      do k = 1, ncc

        i = icc(k)

10      continue

        if ( jnext .le. k ) then
          j = j + 1
          jnext = ccc(j+1)
          go to 10
        end if
     
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8,2x,g16.8)' ) 
     &    k, i, j, acc(k)

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
