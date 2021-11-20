      program main

c*********************************************************************72
c
cc MAIN is the main program for TEST_MATRIX_EXPONENTIAL_PRB.
c
c  Discussion:
c
c    TEST_MATRIX_EXPONENTIAL_PRB tests the TEST_MATRIX_EXPONENTIAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( );
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_MATRIX_EXPONENTIAL_TEST:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TEST_MATRIX_EXPONENTIAL library.'

      call test_matrix_exponential_test01 ( );
      call test_matrix_exponential_test02 ( );
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_MATRIX_EXPONENTIAL_TEST:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( );

      return
      end
      subroutine test_matrix_exponential_test01 ( )

c*********************************************************************72
c
cc TEST_MATRIX_EXPONENTIAL_TEST01 retrieves real test data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 November 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision a(n_max,n_max)
      double precision expa(n_max,n_max)
      integer n
      integer test
      integer test_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_MATRIX_EXPONENTIAL_TEST01:'
      write ( *, '(a)' ) 
     &  '  Retrieve the data for each matrix exponential test.'

      call r8mat_exp_test_num ( test_num )

      do test = 1, test_num

        write ( *, '(a)' ) ' '
        write ( *, '(a,i4)' ) '  Test #', test

        call r8mat_exp_n ( test, n )

        call r8mat_exp_story ( test );

        write ( *, '(a)' ) ' '
        write ( *, '(a,i4)' ) '  Matrix order N = ', n

        call r8mat_exp_a ( test, n, a )

        call r8mat_print ( n, n, a, '  Matrix A:' );

        call r8mat_exp_expa ( test, n, expa )
        call r8mat_print ( n, n, expa, '  Exact Exponential exp(A):' )

      end do

      return
      end
      subroutine test_matrix_exponential_test02 ( )

c*********************************************************************72
c
cc TEST_MATRIX_EXPONENTIAL_TEST02 retrieves complex test data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double complex a(n_max,n_max)
      double complex expa(n_max,n_max)
      integer n
      integer test
      integer test_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_MATRIX_EXPONENTIAL_TEST02:'
      write ( *, '(a)' ) 
     &  '  Retrieve the data for each matrix exponential test.'

      call c8mat_exp_test_num ( test_num )

      do test = 1, test_num

        write ( *, '(a)' ) ' '
        write ( *, '(a,i4)' ) '  Test #', test

        call c8mat_exp_n ( test, n )

        call c8mat_exp_story ( test );

        write ( *, '(a)' ) ' '
        write ( *, '(a,i4)' ) '  Matrix order N = ', n

        call c8mat_exp_a ( test, n, a )

        call c8mat_print ( n, n, a, '  Matrix A:' );

        call c8mat_exp_expa ( test, n, expa )
        call c8mat_print ( n, n, expa, '  Exact Exponential exp(A):' )

      end do

      return
      end
      subroutine c8mat_copy ( m, n, a, b )

c*********************************************************************72
c
cc C8MAT_COPY copies a C8MAT.
c
c  Discussion:
c
c    A C8MAT is a matrix of C8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the order of the matrix.
c
c    Input, double complex A(M,N), the matrix.
c
c    Output, double complex B(M,N), the copied matrix.
c
      implicit none

      integer m
      integer n

      double complex a(m,n)
      double complex b(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          b(i,j) = a(i,j)
        end do
      end do

      return
      end
      subroutine c8mat_print ( m, n, a, title )

c*********************************************************************72
c
cc C8MAT_PRINT prints a C8MAT.
c
c  Discussion:
c
c    A C8MAT is a matrix of C8's.
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
c    Input, integer M, N, the number of rows and columns 
c    in the matrix.
c
c    Input, double complex A(M,N), the matrix.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double complex a(m,n)
      character * ( * ) title

      call c8mat_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine c8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, 
     &  title )

c*********************************************************************72
c
cc C8MAT_PRINT_SOME prints some of a C8MAT.
c
c  Discussion:
c
c    A C8MAT is a matrix of C8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns 
c    in the matrix.
c
c    Input, double complex A(M,N), the matrix.
c
c    Input, integer ILO, JLO, IHI, JHI, the first row and
c    column, and the last row and column to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 4 )
      integer m
      integer n

      double complex a(m,n)
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
      double complex zero

      zero = dcmplx ( 0.0D+00, 0.0D+00 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)' 
        return
      end if
c
c  Print the columns of the matrix, in strips of INCX.
c
      do j2lo = jlo, min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i10,10x)' ) j
        end do

        write ( *, '(a,4a20)' ) '  Col: ', ( ctemp(j2), j2 = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) '  ---'
c
c  Determine the range of the rows in this strip.
c
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi
c
c  Print out (up to) INCX entries in row I, that lie in the current strip.
c
          do j2 = 1, inc

            j = j2lo - 1 + j2

            if ( a(i,j) .eq. zero ) then
              ctemp(j2) = '       0.0          '
            else if ( dimag ( a(i,j) ) .eq. 0.0D+00 ) then
              write ( ctemp(j2), '(g10.3,10x)' ) dreal ( a(i,j) )
            else
              write ( ctemp(j2), '(2g10.3)' ) a(i,j)
            end if

          end do

          write ( *, '(i5,a,4a20)' ) i, ':', ( ctemp(j2), j2 = 1, inc )

        end do

      end do

      return
      end
      subroutine r8mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_COPY copies an R8MAT.
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
