      program main

c*********************************************************************72
c
cc S_SAMPLE_HB is a sample calling program for SUPERLU.
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

      integer n_max
      parameter ( n_max = 5 )
      integer ncc_max
      parameter ( ncc_max = 12 )

      real acc(ncc_max)
      real b(n_max)
      real b2(n_max)
      integer ccc(n_max+1)
      integer factors(8)
      integer i
      integer icc(ncc_max)
      integer info
      integer iopt
      integer ldb
      integer m
      integer n
      integer ncc
      integer nrhs

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'S_SAMPLE_HB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  SGSSV factors and solves a linear system'
      write ( *, '(a)' ) '  using single precision real arithmetic.'
c
c  Read the matrix data from a file.
c
      call shbcode1 ( m, n, ncc, acc, icc, ccc )

      write ( *, '(a)' ) '  Matrix data read from file.'
      write ( *, '(a,i6)' ) '  Matrix order N = ', n
      write ( *, '(a,i6)' ) '  Matrix nonzeros NCC = ', ncc
c
c  Print the matrix.
c
      m = n
      call cc_print ( m, n, ncc, icc, ccc, acc, '  CC matrix:' )

      nrhs = 1
      ldb = n
      do i = 1, n
        b(i) = 1.0E+00
      end do
c
c  Factor the matrix.
c
      iopt = 1
      call c_fortran_sgssv ( iopt, n, ncc, nrhs, acc, icc,
     &  ccc, b, ldb, factors, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_SAMPLE_HB - Fatal error!'
        write ( *, '(a)' ) '  Factorization failed'
        write ( *, '(a,i4)' ) '  INFO = ', info
        stop 1
      end if

      write ( *, '(a)' ) '  Factorization succeeded.'
c
c  Solve the factored system.
c
      iopt = 2
      call c_fortran_sgssv ( iopt, n, ncc, nrhs, acc, icc,
     &  ccc, b, ldb, factors, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_SAMPLE_HB - Fatal error!'
        write ( *, '(a)' ) '  Backsolve failed'
        write ( *, '(a,i4)' ) '  INFO = ', info
        stop 1
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Computed solution:'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(g14.6)' ) b(i)
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
        write ( *, '(g14.6)' ) b2(i)
      end do
c
c  Free memory.
c
      iopt = 3
      call c_fortran_sgssv ( iopt, n, ncc, nrhs, acc, icc,
     &  ccc, b, ldb, factors, info )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'S_SAMPLE_HB:'
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
c    Input, real ACC(NCC), the CC values.
c
c    Input, real X(N), the vector to be multiplied.
c
c    Output, real B(M), the product A*X.
c
      implicit none

      integer m
      integer n
      integer ncc

      real acc(ncc)
      real b(m)
      integer ccc(n+1)
      integer i
      integer icc(ncc)
      integer j
      integer k
      real x(n)

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
c    Input, real ACC(NCC), the CC values.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n
      integer ncc

      real acc(ncc)
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
      write ( *, '(a)' ) '     #     I     J       A'
      write ( *, '(a)' ) '  ----  ----  ----  --------------'
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
     
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) k, i, j, acc(k)

      end do

      return
      end
      subroutine shbcode1 ( m, n, ncc, acc, icc, ccc )

c*********************************************************************72
c
cc SHBCODE1 is a sample code for reading a Harwell-Boeing sparse matrix file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer M, number of rows.
c
c    Output, integer N, number of columns.
c
c    Output, integer NCC, number of nonzeros.
c
c    Output, real ACC(NCC), the nonzero matrix values.
c
c    Output, integer ICC(NCC), the row indices of the nonzeros.
c
c    Output, integer CCC(N+1), the compressed column indices.
c
      implicit none

      real acc(*)
      integer ccc(*)
      integer i
      integer icc(*)
      integer indcrd
      character * ( 16 ) indfmt
      character * ( 8 ) key
      integer m
      character * ( 3 ) mxtype
      integer n
      integer ncc
      integer neltvl
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt
c
c  Read header.
c
      read ( *, '( a72,a8 / 5i14 / a3,11x,4i14 /a16,a16,a20,a20 )' ) 
     &  title, key, 
     &  totcrd, ptrcrd, indcrd, valcrd, rhscrd, 
     &  mxtype, m, n, ncc, neltvl, 
     &  ptrfmt, indfmt, valfmt, rhsfmt
c
c  Read matrix structure.
c
      read ( *, ptrfmt ) ( ccc(i), i = 1, n + 1 )
      read ( *, indfmt ) ( icc(i), i = 1, ncc )
c
c  Read matrix values.
c
      if ( 0 .lt. valcrd ) then
        read ( *, valfmt ) ( acc(i), i = 1, ncc )
      end if

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
