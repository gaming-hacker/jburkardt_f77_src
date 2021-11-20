      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
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
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      subroutine i4vec_dec ( n, x )

c*********************************************************************72
c
cc I4VEC_DEC decrements an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the size of the array.
c
c    Input/output, integer X(N), the array to be decremented.
c
      implicit none

      integer n

      integer i
      integer x(n)

      do i = 1, n
        x(i) = x(i) - 1
      end do

      return
      end
      subroutine i4vec_inc ( n, x )

c*********************************************************************72
c
cc I4VEC_INC increments an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the size of the array.
c
c    Input/output, integer X(N), the array to be incremented.
c
      implicit none

      integer n

      integer i
      integer x(n)

      do i = 1, n
        x(i) = x(i) + 1
      end do

      return
      end
      function i4vec_max ( n, a )

c*********************************************************************72
c
cc I4VEC_MAX computes the maximum element of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer I4VEC_MAX, the value of the largest entry.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer i4vec_max
      integer value

      value = a(1)

      do i = 2, n
        value = max ( value, a(i) )
      end do

      i4vec_max = value

      return
      end
      function i4vec_min ( n, a )

c*********************************************************************72
c
cc I4VEC_MIN computes the minimum element of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer I4VEC_MIN, the value of the smallest entry.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer i4vec_min
      integer value

      value = a(1)

      do i = 2, n
        value = min ( value, a(i) )
      end do

      i4vec_min = value

      return
      end
      subroutine sort_heap_external ( n, indx, i, j, isgn )

c*********************************************************************72
c
cc SORT_HEAP_EXTERNAL externally sorts a list of items into ascending order.
c
c  Discussion:
c
c    The actual list of data is not passed to the routine.  Hence this
c    routine may be used to sort integers, reals, numbers, names,
c    dates, shoe sizes, and so on.  After each call, the routine asks
c    the user to compare or interchange two items, until a special
c    return value signals that the sorting is completed.
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
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms for Computers and Calculators,
c    Academic Press, 1978,
c    ISBN: 0-12-519260-6,
c    LC: QA164.N54.
c
c  Parameters:
c
c    Input, integer N, the number of items to be sorted.
c
c    Input/output, integer INDX, the main communication signal.
c
c    The user must set INDX to 0 before the first call.
c    Thereafter, the user should not change the value of INDX until
c    the sorting is done.
c
c    On return, if INDX is
c
c      greater than 0,
c      * interchange items I and J;
c      * call again.
c
c      less than 0,
c      * compare items I and J;
c      * set ISGN = -1 if I .lt. J, ISGN = +1 if J .lt. I;
c      * call again.
c
c      equal to 0, the sorting is done.
c
c    Output, integer I, J, the indices of two items.
c    On return with INDX positive, elements I and J should be interchanged.
c    On return with INDX negative, elements I and J should be compared, and
c    the result reported in ISGN on the next call.
c
c    Input, integer ISGN, results of comparison of elements I and J.
c    (Used only when the previous call returned INDX less than 0).
c    ISGN .le. 0 means I is less than or equal to J;
c    0 .le. ISGN means I is greater than or equal to J.
c
      implicit none

      integer i
      integer i_save
      integer indx
      integer isgn
      integer j
      integer j_save
      integer k
      integer k1
      integer n
      integer n1

      save i_save
      save j_save
      save k
      save k1
      save n1

      data i_save / 0 /
      data j_save / 0 /
      data k / 0 /
      data k1 / 0 /
      data n1 / 0 /
c
c  INDX = 0: This is the first call.
c
      if ( indx .eq. 0 ) then

        i_save = 0
        j_save = 0
        k = n / 2
        k1 = k
        n1 = n
c
c  INDX .lt. 0: The user is returning the results of a comparison.
c
      else if ( indx .lt. 0 ) then

        if ( indx .eq. -2 ) then

          if ( isgn .lt. 0 ) then
            i_save = i_save + 1
          end if

          j_save = k1
          k1 = i_save
          indx = -1
          i = i_save
          j = j_save
          return

        end if

        if ( 0 .lt. isgn ) then
          indx = 2
          i = i_save
          j = j_save
          return
        end if

        if ( k .le. 1 ) then

          if ( n1 .eq. 1 ) then
            i_save = 0
            j_save = 0
            indx = 0
          else
            i_save = n1
            n1 = n1 - 1
            j_save = 1
            indx = 1
          end if

          i = i_save
          j = j_save
          return

        end if

        k = k - 1
        k1 = k
c
c  0 .lt. INDX, the user was asked to make an interchange.
c
      else if ( indx .eq. 1 ) then

        k1 = k

      end if

10    continue

        i_save = 2 * k1

        if ( i_save .eq. n1 ) then
          j_save = k1
          k1 = i_save
          indx = -1
          i = i_save
          j = j_save
          return
        else if ( i_save .le. n1 ) then
          j_save = i_save + 1
          indx = -2
          i = i_save
          j = j_save
          return
        end if

        if ( k .le. 1 ) then
          go to 20
        end if

        k = k - 1
        k1 = k

      go to 10

20    continue

      if ( n1 .eq. 1 ) then
        i_save = 0
        j_save = 0
        indx = 0
        i = i_save
        j = j_save
      else
        i_save = n1
        n1 = n1 - 1
        j_save = 1
        indx = 1
        i = i_save
        j = j_save
      end if

      return
      end
      subroutine st_data_read ( input_filename, m, n, nst, ist, jst, 
     &  ast )

c*********************************************************************72
c
cc ST_DATA_READ reads the data of an ST file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the ST file.
c
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, integer NST, the number of nonzeros.
c
c    Output, integer IST(NST), JST(NST), the row and column indices.
c
c    Output, double precision AST(NST), the nonzero values.
c
      implicit none

      integer nst

      double precision aij
      double precision ast(nst)
      integer i
      character * ( * ) input_filename
      integer input_unit
      integer ios
      integer ist(nst)
      integer j
      integer jst(nst)
      integer k
      integer m
      integer n

      call get_unit ( input_unit )

      open ( file = input_filename, unit = input_unit, status = 'old', 
     &  iostat = ios )

      do k = 1, nst

        read ( input_unit, *, iostat = ios ) i, j, aij

        if ( ios .ne. 0 ) then
          go to 10
        end if

        ist(k) = i
        jst(k) = j
        ast(k) = aij

      end do

10    continue

      close ( unit = input_unit )

      return
      end
      subroutine st_header_print ( i_min, i_max, j_min, j_max, m, n, 
     &  nst )

c*********************************************************************72
c
cc ST_HEADER_PRINT prints the header of an ST file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I_MIN, I_MAX, the minimum and maximum rows.
c
c    Input, integer J_MIN, J_MAX, the minimum and maximum columns.
c
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, integer NST, the number of nonzeros.
c
      implicit none

      integer i_max
      integer i_min
      integer j_max
      integer j_min
      integer m
      integer n
      integer nst

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Sparse Triplet (ST) header information:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Minimum row index I_MIN = ', i_min
      write ( *, '(a,i8)' ) '  Maximum row index I_MAX = ', i_max
      write ( *, '(a,i8)' ) '  Minimum col index J_MIN = ', j_min
      write ( *, '(a,i8)' ) '  Maximum col index J_MAX = ', j_max
      write ( *, '(a,i8)' ) '  Number of rows        M = ', m
      write ( *, '(a,i8)' ) '  Number of columns     N = ', n
      write ( *, '(a,i8)' ) '  Number of nonzeros  NST = ', nst

      return
      end
      subroutine st_header_read ( input_filename, i_min, i_max, j_min, 
     &  j_max, m, n, nst )

c*********************************************************************72
c
cc ST_HEADER_READ reads the header of an ST file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILENAME, the name of the ST file.
c
c    Output, integer I_MIN, I_MAX, the minimum and maximum rows.
c
c    Output, integer J_MIN, J_MAX, the minimum and maximum columns.
c
c    Output, integer M, the number of rows.
c
c    Output, integer N, the number of columns.
c
c    Output, integer NST, the number of nonzeros.
c
      implicit none

      double precision aij
      integer i
      integer i_max
      integer i_min
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      character * ( * ) input_filename
      integer input_unit
      integer ios
      integer j
      integer j_max
      integer j_min
      integer m
      integer n
      integer nst

      call get_unit ( input_unit )

      open ( file = input_filename, unit = input_unit, 
     &  status = 'old', iostat = ios )

      nst = 0
      i_min = + i4_huge
      i_max = - i4_huge
      j_min = + i4_huge
      j_max = - i4_huge

10    continue

        read ( input_unit, *, iostat = ios ) i, j, aij

        if ( ios .ne. 0 ) then
          go to 20
        end if

        nst = nst + 1
        i_min = min ( i_min, i )
        i_max = max ( i_max, i )
        j_min = min ( j_min, j )
        j_max = max ( j_max, j )

      go to 10

20    continue

      close ( unit = input_unit )

      m = i_max - i_min + 1
      n = j_max - j_min + 1

      return
      end
      subroutine st_print ( m, n, nst, ist, jst, ast, title )

c*********************************************************************72
c
cc ST_PRINT prints a sparse matrix in ST format.
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
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, integer NST, the number of ST elements.
c
c    Input, integer IST(NST), JST(NST), the ST rows and columns.
c
c    Input, double precision AST(NST), the ST values.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer nst

      double precision ast(nst)
      integer ist(nst)
      integer jst(nst)
      integer k
      integer m
      integer n
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) '     #     I     J       A'
      write ( *, '(a)' ) '  ----  ----  ----  --------------'
      write ( *, '(a)' ) ' '

      do k = 1, nst
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) 
     &    k, ist(k), jst(k), ast(k)
      end do

      return
      end
      subroutine st_print_some ( i_min, i_max, j_min, j_max, nst, ist, 
     &  jst, ast, title )

c*********************************************************************72
c
cc ST_PRINT_SOME prints some of a sparse matrix in ST format.
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
c    Input, integer I_MIN, IMAX, the first and last rows to print.
c
c    Input, integer J_MIN, J_MAX, the first and last columns
c    to print.
c
c    Input, integer NST, the number of ST elements.
c
c    Input, integer IST(NST), JST(NST), the ST rows and columns.
c
c    Input, double precision AST(NST), the ST values.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer nst

      double precision ast(nst)
      integer i_max
      integer i_min
      integer ist(nst)
      integer j_max
      integer j_min
      integer jst(nst)
      integer k
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) '     #     I     J       A'
      write ( *, '(a)' ) '  ----  ----  ----  --------------'
      write ( *, '(a)' ) ' '

      do k = 1, nst
        if ( i_min .le. ist(k) .and. ist(k) .le. i_max .and.
     &       j_min .le. jst(k) .and. jst(k) .le. j_max ) then
          write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) 
     &      k, ist(k), jst(k), ast(k)
        end if
      end do

      return
      end
      subroutine st_sort_a ( m, n, nst, ist, jst, ast )

c*********************************************************************72
c
cc ST_SORT_A sorts the entries of an ST matrix by column.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NST, the number of nonzeros.
c
c    Input/output, integer IST(NST), JST(NST), the row and column indices.
c
c    Input/output, double precision AST(NST), the nonzero values.
c
      implicit none

      integer nst

      double precision aij
      double precision ast(nst)
      integer cij
      integer i
      integer indx
      integer isgn
      integer ist(nst)
      integer j
      integer jst(nst)
      integer m
      integer n
      integer rij
c
c  Initialize.
c
      i = 0
      indx = 0
      isgn = 0
      j = 0
c
c  Call the external heap sorter.
c
10    continue

        call sort_heap_external ( nst, indx, i, j, isgn )
c
c  Interchange the I and J objects.
c
        if ( 0 .lt. indx ) then

          rij    = ist(i)
          ist(i) = ist(j)
          ist(j) = rij

          cij    = jst(i)
          jst(i) = jst(j)
          jst(j) = cij

          aij    = ast(i)
          ast(i) = ast(j)
          ast(j) = aij
c
c  Compare the I and J objects.
c
        else if ( indx .lt. 0 ) then

          if ( jst(i) .eq. jst(j) ) then

            if ( ist(i) .lt. ist(j) ) then
              isgn = - 1
            else if ( ist(i) .eq. ist(j) ) then
              isgn = 0
            else if ( ist(j) .lt. ist(i) ) then
              isgn = + 1
            end if

          else if ( jst(i) .lt. jst(j) ) then

            isgn = - 1

          else if ( jst(j) .lt. jst(i) ) then

            isgn = + 1

          end if

        else if ( indx .eq. 0 ) then

          go to 20

        end if

      go to 10

20    continue

      return
      end
      subroutine st_data_transpose ( m, n, nst, ist, jst, ast )

c*********************************************************************72
c
cc ST_TRANSPOSE transposes an ST matrix.
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
c  Parameters:
c
c    Input/output, integer M, the number of rows.
c
c    Input/output, integer N, the number of columns.
c
c    Input, integer NST, the number of nonzeros.
c
c    Input/output, integer IST(NST), JST(NST), the row and column indices.
c
c    Input, double precision AST(NST), the nonzero values.
c
      implicit none

      integer nst

      double precision ast(nst)
      integer ist(nst)
      integer jst(nst)
      integer k
      integer m
      integer n
      integer t

      t = m
      m = n
      n = t

      do k = 1, nst

        t      = ist(k)
        ist(k) = jst(k)
        jst(k) = t

      end do

      return
      end
      subroutine st_write ( output_filename, m, n, nst, ist, jst, ast )

c*********************************************************************72
c
cc ST_WRITE writes an ST file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the name of the ST file.
c
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, integer NST, the number of nonzeros.
c
c    Input, integer IST(NST), JST(NST), the row and column indices.
c
c    Input, double precision AST(NST), the nonzero values.
c
      implicit none

      integer nst

      double precision aij
      double precision ast(nst)
      integer i
      integer ios
      integer ist(nst)
      integer j
      integer jst(nst)
      integer k
      integer m
      integer n
      character * ( * ) output_filename
      integer output_unit

      call get_unit ( output_unit )

      open ( file = output_filename, unit = output_unit, 
     &  status = 'replace', iostat = ios )

      do k = 1, nst

        write ( output_unit, '(2x,i8,2x,i8,2x,g16.8)', iostat = ios ) 
     &    ist(k), jst(k), ast(k)

      end do

      close ( unit = output_unit )

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
