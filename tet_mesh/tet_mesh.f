      function i4_uniform_ab ( a, b, seed )

c*********************************************************************72
c
cc I4_UNIFORM_AB returns a scaled pseudorandom I4 between A and B.
c
c  Discussion:
c
c    An I4 is an integer value.
c
c    The pseudorandom number should be uniformly distributed
c    between A and B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    12 November 2006
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
c    Input, integer A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, integer I4_UNIFORM_AB, a number between A and B.
c
      implicit none

      integer a
      integer b
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4_uniform_ab
      integer k
      real r
      integer seed
      integer value

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + i4_huge
      end if

      r = real ( seed ) * 4.656612875E-10
c
c  Scale R to lie between A-0.5 and B+0.5.
c
      r = ( 1.0E+00 - r ) * ( real ( min ( a, b ) ) - 0.5E+00 )
     &  +             r   * ( real ( max ( a, b ) ) + 0.5E+00 )
c
c  Use rounding to convert R to an integer between A and B.
c
      value = nint ( r )

      value = max ( value, min ( a, b ) )
      value = min ( value, max ( a, b ) )

      i4_uniform_ab = value

      return
      end
      subroutine i4col_compare ( m, n, a, i, j, isgn )

c*********************************************************************72
c
cc I4COL_COMPARE compares columns I and J of an I4COL.
c
c  Discussion:
c
c    An I4COL is an M by N array of I4 values, regarded
c    as an array of N columns of length M.
c
c  Example:
c
c    Input:
c
c      M = 3, N = 4, I = 2, J = 4
c
c      A = (
c        1  2  3  4
c        5  6  7  8
c        9 10 11 12 )
c
c    Output:
c
c      ISGN = -1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), an array of N columns of
c    vectors of length M.
c
c    Input, integer I, J, the columns to be compared.
c    I and J must be between 1 and N.
c
c    Output, integer ISGN, the results of the comparison:
c    -1, column I .lt. column J,
c     0, column I = column J,
c    +1, column J .lt. column I.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer i
      integer isgn
      integer j
      integer k
c
c  Check.
c
      if ( i .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4COL_COMPARE - Fatal error!'
        write ( *, '(a,i8,a)' )
     &    '  Column index I = ', i, ' is less than 1.'
        stop 1
      end if

      if ( n .lt. i ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4COL_COMPARE - Fatal error!'
        write ( *, '(a,i8,a)' )
     &    '  N = ', n, ' is less than column index I = ', i
        stop 1
      end if

      if ( j .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4COL_COMPARE - Fatal error!'
        write ( *, '(a,i8,a)' )
     &    '  Column index J = ', j, ' is less than 1.'
        stop 1
      end if

      if ( n .lt. j ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4COL_COMPARE - Fatal error!'
        write ( *, '(a,i8,a)' )
     &    '  N = ', n, ' is less than column index J = ', j
        stop 1
      end if

      isgn = 0

      if ( i .eq. j ) then
        return
      end if

      k = 1

10    continue

      if ( k .le. m ) then

        if ( a(k,i) .lt. a(k,j) ) then
          isgn = -1
          return
        else if ( a(k,j) .lt. a(k,i) ) then
          isgn = +1
          return
        end if

        k = k + 1

        go to 10

      end if

      return
      end
      subroutine i4col_sort_a ( m, n, a )

c*********************************************************************72
c
cc I4COL_SORT_A ascending sorts an I4COL.
c
c  Discussion:
c
c    An I4COL is an M by N array of I4 values, regarded
c    as an array of N columns of length M.
c
c    In lexicographic order, the statement "X .lt. Y", applied to two real
c    vectors X and Y of length M, means that there is some index I, with
c    1 <= I <= M, with the property that
c
c      X(J) = Y(J) for J .lt. I,
c    and
c      X(I) .lt. Y(I).
c
c    In other words, the first time they differ, X is smaller.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows of A, and the length of
c    a vector of data.
c
c    Input, integer N, the number of columns of A.
c
c    Input/output, integer A(M,N).
c    On input, the array of N columns of M-vectors.
c    On output, the columns of A have been sorted in ascending
c    lexicographic order.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer i
      integer indx
      integer isgn
      integer j

      if ( m .le. 0 ) then
        return
      end if

      if ( n .le. 1 ) then
        return
      end if
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

        call sort_heap_external ( n, indx, i, j, isgn )
c
c  Interchange the I and J objects.
c
        if ( 0 .lt. indx ) then

          call i4col_swap ( m, n, a, i, j )
c
c  Compare the I and J objects.
c
        else if ( indx .lt. 0 ) then

          call i4col_compare ( m, n, a, i, j, isgn )

        else if ( indx .eq. 0 ) then

          go to 20

        end if

      go to 10

20    continue

      return
      end
      subroutine i4col_sort2_a ( m, n, a )

c*********************************************************************72
c
cc I4COL_SORT2_A ascending sorts the elements of each column of an I4COL.
c
c  Discussion:
c
c    An I4COL is an M by N array of I4 values, regarded
c    as an array of N columns of length M.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 December 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows of A.
c
c    Input, integer N, the number of columns of A, and the length
c    of a vector of data.
c
c    Input/output, integer A(M,N).
c    On input, the array of N columns of M vectors.
c    On output, the elements of each column of A have been sorted in ascending
c    order.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer col
      integer i
      integer indx
      integer isgn
      integer j
      integer t

      if ( m .le. 1 ) then
        return
      end if

      if ( n .le. 0 ) then
        return
      end if
c
c  Initialize.
c
      do col = 1, n

        i = 0
        indx = 0
        isgn = 0
        j = 0
c
c  Call the external heap sorter.
c
10      continue

          call sort_heap_external ( m, indx, i, j, isgn )
c
c  Interchange the I and J objects.
c
          if ( 0 .lt. indx ) then

            t        = a(i,col)
            a(i,col) = a(j,col)
            a(j,col) = t
c
c  Compare the I and J objects.
c
          else if ( indx .lt. 0 ) then

            if ( a(j,col) .lt. a(i,col) ) then
              isgn = +1
            else
              isgn = -1
            end if

          else if ( indx .eq. 0 ) then

            go to 20

          end if

        go to 10

20    continue

      end do

      return
      end
      subroutine i4col_sorted_unique_count ( m, n, a, unique_num )

c*********************************************************************72
c
cc I4COL_SORTED_UNIQUE_COUNT counts unique elements in an I4COL.
c
c  Discussion:
c
c    The columns of the array may be ascending or descending sorted.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), a sorted array, containing
c    N columns of data.
c
c    Output, integer UNIQUE_NUM, the number of unique columns.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer i
      integer j1
      integer j2
      integer unique_num

      if ( n .le. 0 ) then
        unique_num = 0
        return
      end if

      unique_num = 1
      j1 = 1

      do j2 = 2, n

        do i = 1, m
          if ( a(i,j1) .ne. a(i,j2) ) then
            unique_num = unique_num + 1
            j1 = j2
            go to 10
          end if
        end do

10      continue

      end do

      return
      end
      subroutine i4col_swap ( m, n, a, j1, j2 )

c*********************************************************************72
c
cc I4COL_SWAP swaps columns J1 and J2 of an I4COL.
c
c  Discussion:
c
c    An I4COL is an M by N array of I4 values, regarded
c    as an array of N columns of length M.
c
c  Example:
c
c    Input:
c
c      M = 3, N = 4, J1 = 2, J2 = 4
c
c      A = (
c        1  2  3  4
c        5  6  7  8
c        9 10 11 12 )
c
c    Output:
c
c      A = (
c        1  4  3  2
c        5  8  7  6
c        9 12 11 10 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns
c    in the array.
c
c    Input/output, integer A(M,N), an array of N columns
c    of length M.
c
c    Input, integer J1, J2, the columns to be swapped.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer i
      integer j1
      integer j2
      integer t

      if ( j1 .lt. 1 .or. n .lt. j1 .or.
     &     j2 .lt. 1 .or. n .lt. j2 ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4COL_SWAP - Fatal error!'
        write ( *, '(a)' ) '  J1 or J2 is out of bounds.'
        write ( *, '(a,i8)' ) '  J1 =    ', j1
        write ( *, '(a,i8)' ) '  J2 =    ', j2
        write ( *, '(a,i8)' ) '  N =     ', n
        stop 1

      end if

      if ( j1 .eq. j2 ) then
        return
      end if

      do i = 1, m
        t       = a(i,j1)
        a(i,j1) = a(i,j2)
        a(i,j2) = t
      end do

      return
      end
      subroutine i4i4_sort_a ( i1, i2, j1, j2 )

c*********************************************************************72
c
cc I4I4_SORT_A ascending sorts a pair of integers.
c
c  Discussion:
c
c    An I4I4 is a pair of integers, regarded as a single data item.
c
c    The program allows the reasonable call:
c
c      call i4i4_sort_a ( i1, i2, i1, i2 )
c
c    and this will return the reasonable result.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I1, I2, the values to sort.
c
c    Output, integer J1, J2, the sorted values.
c
      implicit none

      integer i1
      integer i2
      integer j1
      integer j2
      integer k1
      integer k2
c
c  Copy arguments, so that the user can make "reasonable" calls like:
c
c    call i4i4_sort_a ( i1, i2, i1, i2 )
c
      k1 = i1
      k2 = i2

      j1 = min ( k1, k2 )
      j2 = max ( k1, k2 )

      return
      end
      subroutine i4i4i4_sort_a ( i1, i2, i3, j1, j2, j3 )

c*********************************************************************72
c
cc I4I4I4_SORT_A ascending sorts a triple of integers.
c
c  Discussion:
c
c    An I4I4I4 is a triple of integers, regarded as a single data item.
c
c    The program allows the reasonable call:
c
c      call i4i4i4_sort_a ( i1, i2, i3, i1, i2, i3 )
c
c    and this will return the reasonable result.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 July 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I1, I2, I3, the values to sort.
c
c    Output, integer J1, J2, J3, the sorted values.
c
      implicit none

      integer i1
      integer i2
      integer i3
      integer j1
      integer j2
      integer j3
      integer k1
      integer k2
      integer k3
!
!  Copy arguments, so that the user can make "reasonable" calls like:
!
!    call i4i4i4_sort_a ( i1, i2, i3, i1, i2, i3 )
!
      k1 = i1
      k2 = i2
      k3 = i3

      j1 = min ( min ( k1, k2 ), min ( k2, k3 ) )
      j2 = min ( max ( k1, k2 ),
     &    min ( max ( k2, k3 ), max ( k3, k1 ) ) )
      j3 = max ( max ( k1, k2 ), max ( k2, k3 ) )

      return
      end
      subroutine i4mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc I4MAT_COPY copies an I4MAT.
c
c  Discussion:
c
c    An I4MAT is an array of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 June 2009
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
c    Input, integer A1(M,N), the matrix to copy.
c
c    Output, integer A2(M,N), the copy.
c
      implicit none

      integer m
      integer n

      integer a1(m,n)
      integer a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
      end do

      return
      end
      function i4mat_max ( m, n, a )

c*********************************************************************72
c
cc I4MAT_MAX returns the maximum of an I4MAT.
c
c  Discussion:
c
c    An I4MAT is a rectangular array of I4 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 June 2010
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
c    Input, integer A(M,N), the M by N matrix.
c
c    Output, integer I4MAT_MAX, the maximum entry of A.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4mat_max
      integer j

      i4mat_max = - i4_huge

      do j = 1, n
        do i = 1, m
          i4mat_max = max ( i4mat_max, a(i,j) )
        end do
      end do

      return
      end
      function i4mat_min ( m, n, a )

c*********************************************************************72
c
cc I4MAT_MIN returns the minimum of an I4MAT.
c
c  Discussion:
c
c    An I4MAT is a rectangular array of I4 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 June 2010
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
c    Input, integer A(M,N), the M by N matrix.
c
c    Output, integer I4MAT_MIN, the minimum entry of A.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4mat_min
      integer j

      i4mat_min = i4_huge

      do j = 1, n
        do i = 1, m
          i4mat_min = min ( i4mat_min, a(i,j) )
        end do
      end do

      return
      end
      subroutine i4mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
c
c  Discussion:
c
c    An I4MAT is an array of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    39 October 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), an M by N matrix to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      character * ( * ) title

      call i4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine i4mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &   jhi, title )

c*********************************************************************72
c
cc I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
c
c  Discussion:
c
c    An I4MAT is an array of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 October 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 10 )
      integer m
      integer n

      integer a(m,n)
      character*8 ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer  j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' )  title

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8)' ) i
        end do

        write ( *, '(''  Row '',10a8)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Col'
        write ( *, '(a)' ) ' '

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc

            i = i2lo - 1 + i2

            write ( ctemp(i2), '(i8)' ) a(i,j)

          end do

          write ( *, '(i5,a,10a8)' ) j, ':', ( ctemp(i), i = 1, inc )

        end do

      end do

      return
      end
      subroutine i4vec_print ( n, a, title )

c*********************************************************************72
c
cc I4VEC_PRINT prints an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of integer values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, integer A(N), the vector to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer n

      integer a(n)
      integer i
      character*(*) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,i12)' ) i, ':', a(i)
      end do

      return
      end
      function i4vec_sum ( n, a )

c*********************************************************************72
c
cc I4VEC_SUM returns the sum of the entries of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    In FORTRAN90, this facility is offered by the built in
c    SUM function:
c
c      I4VEC_SUM ( N, A ) = SUM ( A(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
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
c    Output, integer I4VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer i4vec_sum

      i4vec_sum = 0

      do i = 1, n
        i4vec_sum = i4vec_sum + a(i)
      end do

      return
      end
      subroutine mesh_base_one ( node_num, element_order, element_num, 
     &  element_node )

c*********************************************************************72
c
cc MESH_BASE_ONE ensures that the element definition is one-based.
c
c  Discussion:
c
c    The ELEMENT_NODE array contains nodes indices that form elements.
c    The convention for node indexing might start at 0 or at 1.
c    Since a FORTRAN90 program will naturally assume a 1-based indexing, it is
c    necessary to check a given element definition and, if it is actually
c    0-based, to convert it.
c
c    This function attempts to detect 0-based node indexing and correct it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, int NODE_NUM, the number of nodes.
c
c    Input, int ELEMENT_ORDER, the order of the elements.
c
c    Input, int ELEMENT_NUM, the number of elements.
c
c    Input/output, int ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), the element
c    definitions.
c
      implicit none

      integer element_num
      integer element_order

      integer element
      integer element_node(element_order,element_num)
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4mat_max
      integer i4mat_min
      integer j
      integer node
      integer node_max
      integer node_min
      integer node_num
      integer order

      node_min = + i4_huge
      node_max = - i4_huge
      do j = 1, element_num
        do i = 1, element_order
          node_min = min ( node_min, element_node(i,j) )
          node_max = max ( node_max, element_node(i,j) )
        end do
      end do

      if ( node_min .eq. 0 .and. node_max .eq. node_num - 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ONE:'
        write ( *, '(a)' )
     &    '  The element indexing appears to be 0-based!'
        write ( *, '(a)' ) '  This will be converted to 1-based.'
        do j = 1, element_num
          do i = 1, element_order
            element_node(i,j) = element_node(i,j) + 1
          end do
        end do
      else if ( node_min .eq. 1 .and. node_max .eq. node_num ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ONE:'
        write ( *, '(a)' ) 
     &    '  The element indexing appears to be 1-based!'
        write ( *, '(a)' ) '  No conversion is necessary.'
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ONE - Warning!'
        write ( *, '(a)' ) 
     &    '  The element indexing is not of a recognized type.'
        write ( *, '(a,i8)' ) '  NODE_MIN = ', node_min
        write ( *, '(a,i8)' ) '  NODE_MAX = ', node_max
        write ( *, '(a,i8)' ) '  NODE_NUM = ', node_num
      end if

      return
      end
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

      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      double precision r8_uniform_01
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
        seed = seed + i4_huge
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

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
      function r8mat_det_4d ( a )

c*********************************************************************72
c
cc R8MAT_DET_4D computes the determinant of a 4 by 4 R8MAT.
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
c    31 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(4,4), the matrix whose determinant is desired.
c
c    Output, double precision R8MAT_DET_4D, the determinant of the matrix.
c
      implicit none

      double precision a(4,4)
      double precision r8mat_det_4d

      r8mat_det_4d =
     &       a(1,1) * (
     &           a(2,2) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) )
     &         - a(2,3) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) )
     &         + a(2,4) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) ) )
     &     - a(1,2) * (
     &           a(2,1) * ( a(3,3) * a(4,4) - a(3,4) * a(4,3) )
     &         - a(2,3) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) )
     &         + a(2,4) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) ) )
     &     + a(1,3) * (
     &           a(2,1) * ( a(3,2) * a(4,4) - a(3,4) * a(4,2) )
     &         - a(2,2) * ( a(3,1) * a(4,4) - a(3,4) * a(4,1) )
     &         + a(2,4) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) )
     &     - a(1,4) * (
     &           a(2,1) * ( a(3,2) * a(4,3) - a(3,3) * a(4,2) )
     &         - a(2,2) * ( a(3,1) * a(4,3) - a(3,3) * a(4,1) )
     &         + a(2,3) * ( a(3,1) * a(4,2) - a(3,2) * a(4,1) ) )

      return
      end
      subroutine r8mat_mv ( m, n, a, x, y )

c*********************************************************************72
c
cc R8MAT_MV multiplies a matrix times a vector.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c    In FORTRAN90, this operation can be more efficiently carried
c    out by the command
c
c      Y(1:M) = MATMUL ( A(1:M,1:N), X(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of the matrix.
c
c    Input, double precision A(M,N), the M by N matrix.
c
c    Input, double precision X(N), the vector to be multiplied by A.
c
c    Output, double precision Y(M), the product A*X.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision x(n)
      double precision y(m)
      double precision y1(m)

      do i = 1, m
        y1(i) = 0.0D+00
        do j = 1, n
          y1(i) = y1(i) + a(i,j) * x(j)
        end do
      end do

      do i = 1, m
        y(i) = y1(i)
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
      subroutine r8mat_solve ( n, rhs_num, a, info )

c*********************************************************************72
c
cc R8MAT_SOLVE uses Gauss-Jordan elimination to solve an N by N linear system.
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
c    08 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, integer RHS_NUM, the number of right hand sides.
c    RHS_NUM must be at least 0.
c
c    Input/output, double precision A(N,N+rhs_num), contains in rows and
c    columns 1 to N the coefficient matrix, and in columns N+1 through
c    N+rhs_num, the right hand sides.  On output, the coefficient matrix
c    area has been destroyed, while the right hand sides have
c    been overwritten with the corresponding solutions.
c
c    Output, integer INFO, singularity flag.
c    0, the matrix was not singular, the solutions were computed;
c    J, factorization failed on step J, and the solutions could not
c    be computed.
c
      implicit none

      integer n
      integer rhs_num

      double precision a(n,n+rhs_num)
      double precision apivot
      double precision factor
      integer i
      integer info
      integer ipivot
      integer j
      integer k
      double precision t

      info = 0

      do j = 1, n
c
c  Choose a pivot row.
c
        ipivot = j
        apivot = a(j,j)

        do i = j+1, n
          if ( abs ( apivot ) .lt. abs ( a(i,j) ) ) then
            apivot = a(i,j)
            ipivot = i
          end if
        end do

        if ( apivot .eq. 0.0D+00 ) then
          info = j
          return
        end if
c
c  Interchange.
c
        do i = 1, n + rhs_num
          t = a(ipivot,i)
          a(ipivot,i) = a(j,i)
          a(j,i) = t
        end do
c
c  A(J,J) becomes 1.
c
        a(j,j) = 1.0D+00
        do k = j + 1, n + rhs_num
          a(j,k) = a(j,k) / apivot
        end do
c
c  A(I,J) becomes 0.
c
        do i = 1, n

          if ( i .ne. j ) then

            factor = a(i,j)
            a(i,j) = 0.0D+00
            do k = j + 1, n + rhs_num
              a(i,k) = a(i,k) - factor * a(j,k)
            end do

          end if

        end do

      end do

      return
      end
      subroutine r8mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
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
c    28 April 2008
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
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character*(*) title

      call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &  jhi, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT transposed.
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
c    28 April 2008
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
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
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

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8,6x)') i
        end do

        write ( *, '(''       Row'',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '       Col'

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc
            i = i2lo - 1 + i2
            write ( ctemp(i2), '(g14.6)' ) a(i,j)
          end do

          write ( *, '(2x,i8,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

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
      subroutine r8vec_cross_product_3d ( v1, v2, v3 )

c*********************************************************************72
c
cc R8VEC_CROSS_PRODUCT_3D computes the cross product of two R8VEC's in 3D.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    The cross product in 3D can be regarded as the determinant of the
c    symbolic matrix:
c
c          |  i  j  k |
c      det | x1 y1 z1 |
c          | x2 y2 z2 |
c
c      = ( y1 * z2 - z1 * y2 ) * i
c      + ( z1 * x2 - x1 * z2 ) * j
c      + ( x1 * y2 - y1 * x2 ) * k
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision V1(3), V2(3), the two vectors.
c
c    Output, double precision V3(3), the cross product vector.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )

      double precision v1(dim_num)
      double precision v2(dim_num)
      double precision v3(dim_num)

      v3(1) = v1(2) * v2(3) - v1(3) * v2(2)
      v3(2) = v1(3) * v2(1) - v1(1) * v2(3)
      v3(3) = v1(1) * v2(2) - v1(2) * v2(1)

      return
      end
      function r8vec_dot_product ( n, v1, v2 )

c*********************************************************************72
c
cc R8VEC_DOT_PRODUCT finds the dot product of a pair of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    In FORTRAN90, the system routine DOT_PRODUCT should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), V2(N), the vectors.
c
c    Output, double precision R8VEC_DOT_PRODUCT, the dot product.
c
      implicit none

      integer n

      integer i
      double precision r8vec_dot_product
      double precision v1(n)
      double precision v2(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i) * v2(i)
      end do

      r8vec_dot_product = value

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
      double precision r8vec_max
      double precision value

      value = a(1)
      do i = 2, n
        value = max ( value, a(i) )
      end do

      r8vec_max = value

      return
      end
      function r8vec_norm ( n, a )

c*********************************************************************72
c
cc R8VEC_NORM returns the L2 norm of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    The vector L2 norm is defined as:
c
c      R8VEC_NORM = sqrt ( sum ( 1 <= I <= N ) A(I)^2 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in A.
c
c    Input, double precision A(N), the vector whose L2 norm is desired.
c
c    Output, double precision R8VEC_NORM, the L2 norm of A.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_norm
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + a(i) * a(i)
      end do
      value = sqrt ( value )

      r8vec_norm = value

      return
      end
      subroutine r8vec_mean ( n, a, mean )

c*********************************************************************72
c
cc R8VEC_MEAN returns the mean of an R8VEC.
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
c    29 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A(N), the vector whose mean is desired.
c
c    Output, double precision MEAN, the mean of the vector entries.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision mean

      mean = 0.0D+00
      do i = 1, n
        mean = mean + a(i)
      end do
      mean = mean / dble ( n )

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
      function r8vec_sum ( n, v1 )

c*********************************************************************72
c
cc R8VEC_SUM sums the entries of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    In FORTRAN90, the system routine SUM should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), the vector.
c
c    Output, double precision R8VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer i
      double precision r8vec_sum
      double precision v1(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i)
      end do

      r8vec_sum = value

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
      subroutine r8vec_variance ( n, a, variance )

c*********************************************************************72
c
cc R8VEC_VARIANCE returns the variance of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The variance of a vector X of length N is defined as
c
c      mean ( X(1:n) ) = sum ( X(1:n) ) / n
c
c      var ( X(1:n) ) = sum ( ( X(1:n) - mean )^2 ) / ( n - 1 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c    N should be at least 2.
c
c    Input, double precision A(N), the vector.
c
c    Output, double precision VARIANCE, the variance of the vector.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision mean
      double precision variance

      if ( n .lt. 2 ) then

        variance = 0.0D+00

      else

        mean = 0.0D+00
        do i = 1, n
          mean = mean + a(i)
        end do
        mean = mean / dble ( n )

        variance = 0.0D+00
        do i = 1, n
          variance = variance + ( a(i) - mean ) ** 2
        end do
        variance = variance / dble ( n - 1 )

      end if

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
      subroutine tet_mesh_neighbor_tets ( tet_order, tet_num, tet_node, 
     &  tet_neighbor )

c*********************************************************************72
c
cc TET_MESH_NEIGHBOR_TETS determines tetrahedron neighbors.
c
c  Discussion:
c
c    A tet mesh of a set of nodes can be completely described by
c    the coordinates of the nodes, and the list of nodes that make up
c    each tetrahedron.  In the most common case, four nodes are used.
c    There is also a 10 node case, where nodes are also placed on
c    the midsides of the tetrahedral edges.
c
c    This routine can handle 4 or 10-node tetrahedral meshes.  The
c    10-node case is handled simply by ignoring the six midside nodes,
c    which are presumed to be listed after the vertices.
c
c    The tetrahedron adjacency information records which tetrahedron
c    is adjacent to a given tetrahedron on a particular face.
c
c    This routine creates a data structure recording this information.
c
c    The primary amount of work occurs in sorting a list of 4 * TET_NUM
c    data items.
c
c    The neighbor tetrahedrons are indexed by the face they share with
c    the tetrahedron.
c
c    Each face of the tetrahedron is indexed by the node which is NOT
c    part of the face.  That is:
c
c    * Neighbor 1 shares face 1 defined by nodes 2, 3, 4.
c    * Neighbor 2 shares face 2 defined by nodes 1, 3, 4;
c    * Neighbor 3 shares face 3 defined by nodes 1, 2, 4;
c    * Neighbor 4 shares face 4 defined by nodes 1, 2, 3.
c
c    For instance, if the (transposed) TET_NODE array was:
c
c    Row       1      2      3      4
c    Col
c
c      1       4      3      5      1
c      2       4      2      5      1
c      3       4      7      3      5
c      4       4      7      8      5
c      5       4      6      2      5
c      6       4      6      8      5
c
c    then the (transposed) TET_NEIGHBOR array should be:
c
c    Row       1      2      3      4
c    Col
c
c      1      -1      2     -1      3
c      2      -1      1     -1      5
c      3      -1      1      4     -1
c      4      -1      6      3     -1
c      5      -1      2      6     -1
c      6      -1      4      5     -1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer TET_ORDER, the order of the tetrahedrons.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(TET_ORDER,TET_NUM), the
c    indices of the nodes.
c
c    Output, integer TET_NEIGHBOR(4,TET_NUM), the four
c    tetrahedrons that are direct neighbors of a given tetrahedron.  If
c    there is no neighbor sharing a given face, the index is set to -1.
c
      implicit none

      integer tet_num
      integer tet_order

      integer a
      integer b
      integer c
      integer face
      integer face1
      integer face2
      integer faces(5,4*tet_num)
      integer i
      integer j
      integer k
      integer l
      integer tet
      integer tet_neighbor(4,tet_num)
      integer tet_node(tet_order,tet_num)
      integer tet1
      integer tet2
c
c  Step 1.
c  From the list of nodes for tetrahedron T, of the form: (I,J,K,L)
c  construct the four face relations:
c
c    (J,K,L,1,T)
c    (I,K,L,2,T)
c    (I,J,L,3,T)
c    (I,J,K,4,T)
c
c  In order to make matching easier, we reorder each triple of nodes
c  into ascending order.
c
      do tet = 1, tet_num

        i = tet_node(1,tet)
        j = tet_node(2,tet)
        k = tet_node(3,tet)
        l = tet_node(4,tet)

        call i4i4i4_sort_a ( j, k, l, a, b, c )

        faces(1,4*(tet-1)+1) = a
        faces(2,4*(tet-1)+1) = b
        faces(3,4*(tet-1)+1) = c
        faces(4,4*(tet-1)+1) = 1
        faces(5,4*(tet-1)+1) = tet

        call i4i4i4_sort_a ( i, k, l, a, b, c )

        faces(1,4*(tet-1)+2) = a
        faces(2,4*(tet-1)+2) = b
        faces(3,4*(tet-1)+2) = c
        faces(4,4*(tet-1)+2) = 2
        faces(5,4*(tet-1)+2) = tet

        call i4i4i4_sort_a ( i, j, l, a, b, c )

        faces(1,4*(tet-1)+3) = a
        faces(2,4*(tet-1)+3) = b
        faces(3,4*(tet-1)+3) = c
        faces(4,4*(tet-1)+3) = 3
        faces(5,4*(tet-1)+3) = tet

        call i4i4i4_sort_a ( i, j, k, a, b, c )

        faces(1,4*(tet-1)+4) = a
        faces(2,4*(tet-1)+4) = b
        faces(3,4*(tet-1)+4) = c
        faces(4,4*(tet-1)+4) = 4
        faces(5,4*(tet-1)+4) = tet

      end do
c
c  Step 2. Perform an ascending dictionary sort on the neighbor relations.
c  We only intend to sort on rows 1:3; the routine we call here
c  sorts on rows 1 through 5 but that won't hurt us.
c
c  What we need is to find cases where two tetrahedrons share a face.
c  By sorting the columns of the FACES array, we will put shared faces
c  next to each other.
c
      call i4col_sort_a ( 5, 4*tet_num, faces )
c
c  Step 3. Neighboring tetrahedrons show up as consecutive columns with
c  identical first three entries.  Whenever you spot this happening,
c  make the appropriate entries in TET_NEIGHBOR.
c
      do j = 1, tet_num
        do i = 1, 4
          tet_neighbor(i,j) = -1
        end do
      end do

      face = 1

10    continue

        if ( 4 * tet_num <= face ) then
          go to 20
        end if

        if (
     &    faces(1,face) .eq. faces(1,face+1) .and.
     &    faces(2,face) .eq. faces(2,face+1) .and.
     &    faces(3,face) .eq. faces(3,face+1) ) then
          face1 = faces(4,face)
          tet1 = faces(5,face)
          face2 = faces(4,face+1)
          tet2 = faces(5,face+1)
          tet_neighbor(face1,tet1) = tet2
          tet_neighbor(face2,tet2) = tet1
          face = face + 2
        else
          face = face + 1
        end if

      go to 10

20    continue

      return
      end
      subroutine tet_mesh_node_order ( tet_order, tet_num, tet_node,
     &  node_num, node_order )

c*********************************************************************72
c
cc TET_MESH_NODE_ORDER: determine the order of nodes in a tet mesh.
c
c  Discussion:
c
c    The order of a node is the number of tetrahedrons that use that node
c    as a vertex.
c
c    Tetrahedrons of order 4 or 10 are allowed as input.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer TET_ORDER, the order of the mesh, either
c    4 or 10.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(TET_ORDER,TET_NUM), the indices
c    of the nodes.
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Output, integer NODE_ORDER(NODE_NUM), the order of each node.
c
      implicit none

      integer node_num
      integer tet_num
      integer tet_order

      integer i
      integer node
      integer node_order(node_num)
      integer tet
      integer tet_node(tet_order,tet_num)

      do i = 1, node_num
        node_order(i) = 0
      end do

      do tet = 1, tet_num
        do i = 1, tet_order
          node = tet_node(i,tet)
          if ( node .lt. 1 .or. node_num .lt. node ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'TET_MESH_NODE_ORDER - Fatal error!'
            write ( *, '(a)' ) '  Illegal entry in TET_NODE.'
            stop 1
          else
            node_order(node) = node_order(node) + 1
          end if
        end do
      end do

      return
      end
      subroutine tet_mesh_order4_adj_count ( node_num, tet_num, 
     &  tet_node, adj_num, adj_row )

c*********************************************************************72
c
cc TET_MESH_ORDER4_ADJ_COUNT counts the number of nodal adjacencies.
c
c  Discussion:
c
c    Assuming that the tet mesh is to be used in a finite element
c    computation, we declare that two distinct nodes are "adjacent" if and
c    only if they are both included in some tetrahedron.
c
c    It is the purpose of this routine to determine the number of
c    such adjacency relationships.
c
c    The initial count gets only the (I,J) relationships, for which
c    node I is strictly less than node J.  This value is doubled
c    to account for symmetry.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(4,TET_NUM), the indices of
c    the nodes.
c
c    Output, integer ADJ_NUM, the total number of adjacency
c    relationships,
c
c    Output, integer ADJ_ROW(NODE_NUM+1), the ADJ pointer array.
c
      implicit none

      integer tet_num
      integer node_num

      integer adj_num
      integer adj_row(node_num+1)
      integer i
      integer j
      integer j1
      integer j2
      integer k
      integer pair(2,6*tet_num)
      integer pair_num
      integer pair_unique_num
      integer tet_node(4,tet_num)
c
c  Each order 4 tetrahedron defines 6 adjacency pairs.
c
      do j2 = 1, tet_num
        j1 = j
        pair(1,j1) = tet_node(1,j2)
        pair(2,j1) = tet_node(2,j2)
      end do

      do j2 = 1, tet_num
        j1 = tet_num + j2
        pair(1,j1) = tet_node(1,j2)
        pair(2,j1) = tet_node(3,j2)
      end do

      do j2 = 1, tet_num
        j1 = 2*tet_num + j2
        pair(1,j1) = tet_node(1,j2)
        pair(2,j1) = tet_node(4,j2)
      end do

      do j2 = 1, tet_num
        j1 = 3*tet_num + j2
        pair(1,j1) = tet_node(2,j2)
        pair(2,j1) = tet_node(3,j2)
      end do

      do j2 = 1, tet_num
        j1 = 4*tet_num + j2
        pair(1,j1) = tet_node(2,j2)
        pair(2,j1) = tet_node(4,j2)
      end do

      do j2 = 1, tet_num
        j1 = 5*tet_num + j2
        pair(1,j1) = tet_node(3,j2)
        pair(2,j1) = tet_node(4,j2)
      end do

      pair_num = 6 * tet_num
c
c  Force the nodes of each pair to be listed in ascending order.
c
      call i4col_sort2_a ( 2, pair_num, pair )
c
c  Rearrange the columns in ascending order.
c
      call i4col_sort_a ( 2, pair_num, pair )
c
c  Get the number of unique columns.
c
      call i4col_sorted_unique_count ( 2, pair_num, pair, 
     &  pair_unique_num )
c
c  The number of adjacencies is TWICE this value, plus the number of nodes.
c
      adj_num = 2 * pair_unique_num
c
c  Now set up the ADJ_ROW counts.
c
      do i = 1, node_num
        adj_row(i) = 0
      end do

      do k = 1, pair_num

        if ( 1 .lt. k ) then
          if ( pair(1,k-1) .eq. pair(1,k) .and.
     &         pair(2,k-1) .eq. pair(2,k) ) then
            go to 10
          end if
        end if

        i = pair(1,k)
        j = pair(2,k)

        adj_row(i) = adj_row(i) + 1
        adj_row(j) = adj_row(j) + 1

10      continue

      end do
c
c  We used ADJ_ROW to count the number of entries in each row.
c  Convert it to pointers into the ADJ array.
c
      do i = node_num, 1, -1
        adj_row(i+1) = adj_row(i)
      end do

      adj_row(1) = 1
      do i = 2, node_num + 1
        adj_row(i) = adj_row(i-1) + adj_row(i)
      end do

      return
      end
      subroutine tet_mesh_order4_adj_set ( node_num, tet_num, tet_node, 
     &  adj_num, adj_row, adj )

c*********************************************************************72
c
cc TET_MESH_ORDER4_ADJ_SET sets the nodal adjacency matrix.
c
c  Discussion:
c
c    A compressed format is used for the nodal adjacency matrix.
c
c    It is assumed that we know ADJ_NUM, the number of adjacency entries
c    and the ADJ_ROW array, which keeps track of the list of slots
c    in ADJ where we can store adjacency information for each row.
c
c    We essentially repeat the work of TET_MESH_ORDER4_ADJ_COUNT, but
c    now we have a place to store the adjacency information.
c
c    A copy of the ADJ_ROW array is useful, as we can use it to keep track
c    of the next available entry in ADJ for adjacencies associated with
c    a given row.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(4,TET_NUM), the indices of
c    the nodes.
c
c    Input, integer ADJ_NUM, the total number of adjacency
c    relationships,
c
c    Input, integer ADJ_ROW(NODE_NUM+1), the ADJ pointer array.
c
c    Output, integer ADJ(ADJ_NUM), the adjacency information.
c
      implicit none

      integer adj_num
      integer tet_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num+1)
      integer adj_row_copy(node_num+1)
      integer i
      integer j
      integer j1
      integer j2
      integer k
      integer pair(2,6*tet_num)
      integer pair_num
      integer tet_node(4,tet_num)
c
c  Each order 4 tetrahedron defines 6 adjacency pairs.
c
      do j2 = 1, tet_num
        j1 = j2
        pair(1,j1) = tet_node(1,j2)
        pair(2,j1) = tet_node(2,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + tet_num
        pair(1,j1) = tet_node(1,j2)
        pair(2,j1) = tet_node(3,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + 2 * tet_num
        pair(1,j1) = tet_node(1,j2)
        pair(2,j1) = tet_node(4,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + 3 * tet_num
        pair(1,j1) = tet_node(2,j2)
        pair(2,j1) = tet_node(3,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + 4 * tet_num
        pair(1,j1) = tet_node(2,j2)
        pair(2,j1) = tet_node(4,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + 5 * tet_num
        pair(1,j1) = tet_node(3,j2)
        pair(2,j1) = tet_node(4,j2)
      end do

      pair_num = 6 * tet_num
c
c  Force the nodes of each pair to be listed in ascending order.
c
      call i4col_sort2_a ( 2, pair_num, pair )
c
c  Rearrange the columns in ascending order.
c
      call i4col_sort_a ( 2, pair_num, pair )
c
c  Mark all entries of ADJ so we will know later if we missed one.
c
      do i = 1, adj_num
        adj(i) = -1
      end do
c
c  Copy the ADJ_ROW array and use it to keep track of the next
c  free entry for each row.
c
      do i = 1, node_num
        adj_row_copy(i) = adj_row(i)
      end do
c
c  Now set up the ADJ_ROW counts.
c
      do k = 1, pair_num

        if ( 1 .lt. k ) then
          if ( pair(1,k-1) .eq. pair(1,k) .and.
     &         pair(2,k-1) .eq. pair(2,k) ) then
            go to 10
          end if
        end if

        i = pair(1,k)
        j = pair(2,k)

        adj(adj_row_copy(i)) = j
        adj_row_copy(i) = adj_row_copy(i) + 1
        adj(adj_row_copy(j)) = i
        adj_row_copy(j) = adj_row_copy(j) + 1

10      continue

      end do

      return
      end
      subroutine tet_mesh_order4_boundary_face_count ( tet_num, 
     &  tet_node, boundary_face_num )

c*********************************************************************72
c
cc TET_MESH_ORDER4_BOUNDARY_FACE_COUNT counts the number of boundary faces.
c
c  Discussion:
c
c    This routine is given a tet mesh, an abstract list of
c    quadruples of nodes.  It is assumed that the nodes forming each
c    face of each tetrahedron are listed in a counterclockwise order,
c    although the routine should work if the nodes are consistently
c    listed in a clockwise order as well.
c
c    It is assumed that each face of the tet mesh is either
c    * an INTERIOR face, which is listed twice, once with positive
c      orientation and once with negative orientation, or;
c    * a BOUNDARY face, which will occur only once.
c
c    This routine should work even if the region has holes.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(4,TET_NUM), the indices of
c    the nodes.
c
c    Output, integer BOUNDARY_FACE_NUM, the number of boundary
c    faces.
c
      implicit none

      integer tet_num

      integer boundary_face_num
      integer face(3,4*tet_num)
      integer face_num
      integer i
      integer interior_face_num
      integer j
      integer j1
      integer j2
      integer m
      integer tet_node(4,tet_num)
      integer unique_face_num

      m = 3
      face_num = 4 * tet_num
c
c  Set up the face array:
c  (Omit node 1)
c  (Omit node 2)
c  (Omit node 3)
c  (Omit node 4)
c
      do j = 1, tet_num
        do i = 1, 3
          face(i,j) = tet_node(i+1,j)
        end do
      end do

      do j2 = 1, tet_num
        j1 = j2 + tet_num
        face(1,j1) = tet_node(1,j2)
        face(2,j1) = tet_node(3,j2)
        face(3,j1) = tet_node(4,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + 2 * tet_num
        face(1,j1) = tet_node(1,j2)
        face(2,j1) = tet_node(2,j2)
        face(3,j1) = tet_node(4,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + 3 * tet_num
        face(1,j1) = tet_node(1,j2)
        face(2,j1) = tet_node(2,j2)
        face(3,j1) = tet_node(3,j2)
      end do
c
c  Force the nodes of each face to be listed in ascending order.
c
      call i4col_sort2_a ( m, face_num, face )
c
c  Ascending sort the columns.
c
      call i4col_sort_a ( m, face_num, face )
c
c  Get the number of unique columns.
c
      call i4col_sorted_unique_count ( m, face_num, face, 
     &  unique_face_num )
c
c  Determine the number of interior and boundary faces.
c
      interior_face_num = 4 * tet_num - unique_face_num

      boundary_face_num = 4 * tet_num - 2 * interior_face_num

      return
      end
      subroutine tet_mesh_order4_edge_count ( tet_num, tet_node, 
     &  edge_num )

c*********************************************************************72
c
cc TET_MESH_ORDER4_EDGE_COUNT counts the number of edges.
c
c  Discussion:
c
c    This routine is given a tet mesh, an abstract list of
c    quadruples of nodes.  Each tetrahedron defines 6 edges; however,
c    assuming that tetrahedrons are touching each other, most edges
c    will be used more than once.  This routine determines the actual
c    number of "geometric" edges associated with the tet mesh.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(4,TET_NUM), the indices of
c    the nodes.
c
c    Output, integer EDGE_NUM, the number of edges.
c
      implicit none

      integer tet_num

      integer edge(2,6*tet_num)
      integer edge_num
      integer edge_num_raw
      integer j1
      integer j2
      integer m
      integer tet_node(4,tet_num)

      m = 3
      edge_num_raw = 6 * tet_num
c
c  Set up the raw edge array:
c
      do j2 = 1, tet_num
        j1 = j2
        edge(1,j1) = tet_node(1,j2)
        edge(2,j1) = tet_node(2,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + tet_num
        edge(1,j1) = tet_node(1,j2)
        edge(2,j1) = tet_node(3,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + 2 * tet_num
        edge(1,j1) = tet_node(1,j2)
        edge(2,j1) = tet_node(4,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + 3 * tet_num
        edge(1,j1) = tet_node(2,j2)
        edge(2,j1) = tet_node(3,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + 4 * tet_num
        edge(1,j1) = tet_node(2,j2)
        edge(2,j1) = tet_node(4,j2)
      end do

      do j2 = 1, tet_num
        j1 = j2 + 5 * tet_num
        edge(1,j1) = tet_node(3,j2)
        edge(2,j1) = tet_node(4,j2)
      end do
c
c  Force the nodes of each face to be listed in ascending order.
c
      call i4col_sort2_a ( m, edge_num_raw, edge )
c
c  Ascending sort the columns.
c
      call i4col_sort_a ( m, edge_num_raw, edge )
c
c  Get the number of unique columns.
c
      call i4col_sorted_unique_count ( m, edge_num_raw, edge, edge_num )

      return
      end
      subroutine tet_mesh_order4_example_set ( node_num, tet_num,
     &  node_xyz, tet_node )

c*********************************************************************72
c
cc TET_MESH_ORDER4_EXAMPLE_SET sets an example linear tet mesh.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Output, double precision NODE_XYZ(3,NODE_NUM), the node coordinates.
c
c    Output, integer TET_NODE(4,TET_NUM), the nodes
c    forming each tet.
c
      implicit none

      integer node_num
      integer tet_num

      double precision node_xyz(3,node_num)
      double precision node_xyz_save(3,63)
      integer tet_node(4,tet_num)
      integer tet_node_save(4,144)

      save node_xyz_save
      save tet_node_save

      data node_xyz_save /
     & 0.0D+00,  0.0D+00,  0.0D+00, 
     & 0.0D+00,  0.0D+00,  0.5D+00, 
     & 0.0D+00,  0.0D+00,  1.0D+00, 
     & 0.0D+00,  0.5D+00,  0.0D+00, 
     & 0.0D+00,  0.5D+00,  0.5D+00, 
     & 0.0D+00,  0.5D+00,  1.0D+00, 
     & 0.0D+00,  1.0D+00,  0.0D+00, 
     & 0.0D+00,  1.0D+00,  0.5D+00, 
     & 0.0D+00,  1.0D+00,  1.0D+00, 
     & 0.5D+00,  0.0D+00,  0.0D+00, 
     & 0.5D+00,  0.0D+00,  0.5D+00, 
     & 0.5D+00,  0.0D+00,  1.0D+00, 
     & 0.5D+00,  0.5D+00,  0.0D+00, 
     & 0.5D+00,  0.5D+00,  0.5D+00, 
     & 0.5D+00,  0.5D+00,  1.0D+00, 
     & 0.5D+00,  1.0D+00,  0.0D+00, 
     & 0.5D+00,  1.0D+00,  0.5D+00, 
     & 0.5D+00,  1.0D+00,  1.0D+00, 
     & 1.0D+00,  0.0D+00,  0.0D+00, 
     & 1.0D+00,  0.0D+00,  0.5D+00, 
     & 1.0D+00,  0.0D+00,  1.0D+00, 
     & 1.0D+00,  0.5D+00,  0.0D+00, 
     & 1.0D+00,  0.5D+00,  0.5D+00, 
     & 1.0D+00,  0.5D+00,  1.0D+00, 
     & 1.0D+00,  1.0D+00,  0.0D+00, 
     & 1.0D+00,  1.0D+00,  0.5D+00, 
     & 1.0D+00,  1.0D+00,  1.0D+00, 
     & 1.5D+00,  0.0D+00,  0.0D+00, 
     & 1.5D+00,  0.0D+00,  0.5D+00, 
     & 1.5D+00,  0.0D+00,  1.0D+00, 
     & 1.5D+00,  0.5D+00,  0.0D+00, 
     & 1.5D+00,  0.5D+00,  0.5D+00, 
     & 1.5D+00,  0.5D+00,  1.0D+00, 
     & 1.5D+00,  1.0D+00,  0.0D+00, 
     & 1.5D+00,  1.0D+00,  0.5D+00, 
     & 1.5D+00,  1.0D+00,  1.0D+00, 
     & 2.0D+00,  0.0D+00,  0.0D+00, 
     & 2.0D+00,  0.0D+00,  0.5D+00, 
     & 2.0D+00,  0.0D+00,  1.0D+00, 
     & 2.0D+00,  0.5D+00,  0.0D+00, 
     & 2.0D+00,  0.5D+00,  0.5D+00, 
     & 2.0D+00,  0.5D+00,  1.0D+00, 
     & 2.0D+00,  1.0D+00,  0.0D+00, 
     & 2.0D+00,  1.0D+00,  0.5D+00, 
     & 2.0D+00,  1.0D+00,  1.0D+00, 
     & 2.5D+00,  0.0D+00,  0.0D+00, 
     & 2.5D+00,  0.0D+00,  0.5D+00, 
     & 2.5D+00,  0.0D+00,  1.0D+00, 
     & 2.5D+00,  0.5D+00,  0.0D+00, 
     & 2.5D+00,  0.5D+00,  0.5D+00, 
     & 2.5D+00,  0.5D+00,  1.0D+00, 
     & 2.5D+00,  1.0D+00,  0.0D+00, 
     & 2.5D+00,  1.0D+00,  0.5D+00, 
     & 2.5D+00,  1.0D+00,  1.0D+00, 
     & 3.0D+00,  0.0D+00,  0.0D+00, 
     & 3.0D+00,  0.0D+00,  0.5D+00, 
     & 3.0D+00,  0.0D+00,  1.0D+00, 
     & 3.0D+00,  0.5D+00,  0.0D+00, 
     & 3.0D+00,  0.5D+00,  0.5D+00, 
     & 3.0D+00,  0.5D+00,  1.0D+00, 
     & 3.0D+00,  1.0D+00,  0.0D+00, 
     & 3.0D+00,  1.0D+00,  0.5D+00, 
     & 3.0D+00,  1.0D+00,  1.0D+00 /

      data tet_node_save /
     &    1,   2,   4,  10, 
     &    2,   4,   5,  10, 
     &    2,   5,  10,  11, 
     &    2,   3,   5,  11, 
     &    4,   5,  10,  13, 
     &    3,   5,   6,  11, 
     &    5,  10,  11,  13, 
     &    4,   5,   7,  13, 
     &    5,   6,   8,  14, 
     &    5,   7,   8,  13, 
     &    6,   8,   9,  14, 
     &   11,  13,  14,  19, 
     &   12,  14,  15,  20, 
     &    3,   6,  11,  12, 
     &    5,   6,  11,  14, 
     &    6,   9,  14,  15, 
     &    6,  11,  12,  14, 
     &    6,  12,  14,  15, 
     &    7,   8,  13,  16, 
     &    5,   8,  13,  14, 
     &   10,  11,  13,  19, 
     &    8,   9,  14,  17, 
     &   11,  12,  14,  20, 
     &    5,  11,  13,  14, 
     &    8,  13,  14,  16, 
     &    9,  14,  15,  17, 
     &   13,  14,  16,  22, 
     &    8,  14,  16,  17, 
     &   14,  15,  17,  23, 
     &   14,  16,  17,  22, 
     &    9,  15,  17,  18, 
     &   15,  17,  18,  23, 
     &   14,  17,  22,  23, 
     &   13,  14,  19,  22, 
     &   11,  14,  19,  20, 
     &   14,  15,  20,  23, 
     &   15,  20,  21,  23, 
     &   21,  23,  24,  29, 
     &   20,  22,  23,  28, 
     &   14,  19,  20,  22, 
     &   15,  18,  23,  24, 
     &   12,  15,  20,  21, 
     &   15,  21,  23,  24, 
     &   16,  17,  22,  25, 
     &   19,  20,  22,  28, 
     &   17,  18,  23,  26, 
     &   20,  21,  23,  29, 
     &   14,  20,  22,  23, 
     &   17,  22,  23,  25, 
     &   18,  23,  24,  26, 
     &   22,  23,  25,  31, 
     &   17,  23,  25,  26, 
     &   23,  24,  26,  32, 
     &   23,  25,  26,  31, 
     &   18,  24,  26,  27, 
     &   24,  26,  27,  32, 
     &   23,  26,  31,  32, 
     &   22,  23,  28,  31, 
     &   20,  23,  28,  29, 
     &   23,  24,  29,  32, 
     &   24,  29,  30,  32, 
     &   30,  32,  33,  38, 
     &   29,  31,  32,  37, 
     &   23,  28,  29,  31, 
     &   24,  27,  32,  33, 
     &   21,  24,  29,  30, 
     &   24,  30,  32,  33, 
     &   25,  26,  31,  34, 
     &   28,  29,  31,  37, 
     &   26,  27,  32,  35, 
     &   29,  30,  32,  38, 
     &   23,  29,  31,  32, 
     &   26,  31,  32,  34, 
     &   27,  32,  33,  35, 
     &   31,  32,  34,  40, 
     &   26,  32,  34,  35, 
     &   32,  33,  35,  41, 
     &   32,  34,  35,  40, 
     &   27,  33,  35,  36, 
     &   33,  35,  36,  41, 
     &   32,  35,  40,  41, 
     &   31,  32,  37,  40, 
     &   29,  32,  37,  38, 
     &   32,  33,  38,  41, 
     &   33,  38,  39,  41, 
     &   39,  41,  42,  47, 
     &   38,  40,  41,  46, 
     &   32,  37,  38,  40, 
     &   33,  36,  41,  42, 
     &   30,  33,  38,  39, 
     &   33,  39,  41,  42, 
     &   34,  35,  40,  43, 
     &   37,  38,  40,  46, 
     &   35,  36,  41,  44, 
     &   38,  39,  41,  47, 
     &   32,  38,  40,  41, 
     &   35,  40,  41,  43, 
     &   36,  41,  42,  44, 
     &   40,  41,  43,  49, 
     &   35,  41,  43,  44, 
     &   41,  42,  44,  50, 
     &   41,  43,  44,  49, 
     &   36,  42,  44,  45, 
     &   42,  44,  45,  50, 
     &   41,  44,  49,  50, 
     &   40,  41,  46,  49, 
     &   38,  41,  46,  47, 
     &   41,  42,  47,  50, 
     &   42,  47,  48,  50, 
     &   48,  50,  51,  56, 
     &   47,  49,  50,  55, 
     &   41,  46,  47,  49, 
     &   42,  45,  50,  51, 
     &   39,  42,  47,  48, 
     &   42,  48,  50,  51, 
     &   43,  44,  49,  52, 
     &   46,  47,  49,  55, 
     &   44,  45,  50,  53, 
     &   47,  48,  50,  56, 
     &   41,  47,  49,  50, 
     &   44,  49,  50,  52, 
     &   45,  50,  51,  53, 
     &   49,  50,  52,  58, 
     &   44,  50,  52,  53, 
     &   50,  51,  53,  59, 
     &   50,  52,  53,  58, 
     &   45,  51,  53,  54, 
     &   51,  53,  54,  59, 
     &   50,  53,  58,  59, 
     &   49,  50,  55,  58, 
     &   47,  50,  55,  56, 
     &   50,  51,  56,  59, 
     &   51,  56,  57,  59, 
     &   50,  55,  56,  58, 
     &   51,  54,  59,  60, 
     &   48,  51,  56,  57, 
     &   51,  57,  59,  60, 
     &   52,  53,  58,  61, 
     &   53,  54,  59,  62, 
     &   50,  56,  58,  59, 
     &   53,  58,  59,  61, 
     &   54,  59,  60,  62, 
     &   53,  59,  61,  62, 
     &   54,  60,  62,  63  /

      call i4mat_copy ( 4, tet_num, tet_node_save, tet_node )
      call r8mat_copy ( 3, node_num, node_xyz_save, node_xyz )

      return
      end
      subroutine tet_mesh_order4_example_size ( node_num, tet_num )

c*********************************************************************72
c
cc TET_MESH_ORDER4_EXAMPLE_SIZE sizes an example linear tet mesh.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer NODE_NUM, the number of nodes.
c
c    Output, integer TET_NUM, the number of tetrahedrons.
c
      implicit none

      integer node_num
      integer tet_num

      node_num = 63
      tet_num = 144

      return
      end
      subroutine tet_mesh_order4_refine_compute ( node_num1, tet_num1, 
     &  node_xyz1, tet_node1, node_num2, tet_num2, edge_data, 
     &  node_xyz2, tet_node2 )

c*********************************************************************72
c
cc TET_MESH_ORDER4_REFINE_COMPUTE computes a refined order4 tet mesh.
c
c  Discussion:
c
c    A refined 4-node tet mesh can be derived from a given
c    4-node tet mesh by interpolating nodes at the midpoint of
c    every edge of the mesh.
c
c    The mesh is described indirectly, as the sum of individual
c    tetrahedrons.  A single physical edge may be a logical edge of
c    any number of tetrahedrons.  It is important, however, that a
c    new node be created exactly once for each edge, assigned an index,
c    and associated with every tetrahedron that shares this edge.
c
c    This routine handles that problem.
c
c    The primary amount of work occurs in sorting a list of 6 * TET_NUM
c    data items, one item for every edge of every tetrahedron.  Each
c    data item records, for a given tetrahedron edge, the global indices
c    of the two endpoints, the local indices of the two endpoints,
c    and the index of the tetrahedron.
c
c    Through careful sorting, it is possible to arrange this data in
c    a way that allows the proper generation of the interpolated nodes.
c
c    Let us add the new nodes and temporarily assign them local indices
c    5 through X, based on the following ordering:
c
c      1, 2, 3, 4, (1+2), (1+3), (1+4), (2+3), (2+4), (3+4).
c
c    Then let us assign these nodes to eight subtetrahedrons as follows:
c
c      1, 5, 6, 7
c      2, 5, 8, 9
c      3, 6, 8, 9
c      4, 7, 9, X
c      5, 6, 7, 9
c      5, 6, 8, 9
c      6, 7, 9, X
c      6, 8, 9, X
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
c  Reference:
c
c    Anwei Liu, Barry Joe,
c    Quality Local Refinement of Tetrahedral Meshes Based
c    on 8-Subtetrahedron Subdivision,
c    Mathematics of Computation,
c    Volume 65, Number 215, July 1996, pages 1183-1200.
c
c  Parameters:
c
c    Input, integer NODE_NUM1, the number of nodes in the input
c    mesh.
c
c    Input, integer TET_NUM1, the number of tetrahedrons in
c    the input mesh.
c
c    Input, double precision NODE_XYZ1(3,NODE_NUM1), the coordinates of
c    the nodes that make up the input mesh.
c
c    Input, integer TET_NODE1(4,TET_NUM1), the indices of
c    the nodes in the input mesh.
c
c    Input, integer NODE_NUM2, the number of nodes for the
c    refined mesh.
c
c    Input, integer TET_NUM2, the number of tetrahedrons in the
c    refined mesh.
c
c    Input, integer EDGE_DATA(5,6*TET_NUM), edge data.
c
c    Output, double precision NODE_XYZ2(3,NODE_NUM2), the coordinates of
c    the nodes that make up the output mesh.
c
c    Output, integer TET_NODE2(4,TET_NUM2), the indices of
c    the nodes in the output mesh.
c
      implicit none

      integer node_num1
      integer node_num2
      integer tet_num1
      integer tet_num2

      integer edge
      integer edge_data(5,6*tet_num1)
      integer i
      integer j
      integer n1
      integer n1_old
      integer n2
      integer n2_old
      integer node
      double precision node_xyz1(3,node_num1)
      double precision node_xyz2(3,node_num2)
      integer tet_node1(4,tet_num1)
      integer tet_node2(4,tet_num2)
      integer tet1
      integer tet2
      integer v
      integer v1
      integer v2
c
c  Generate the index and coordinates of the new midside nodes,
c  and update the tetradehron-node data.
c
      node_xyz2(1:3,1:node_num1) = node_xyz1(1:3,1:node_num1)

      tet_node2(1:4,1:tet_num2) = -1
c
c  The vertices of the input tetrahedron can be assigned now.
c
      do tet1 = 1, tet_num1
        tet_node2(1,(tet1-1)*8+1) = tet_node1(1,tet1)
        tet_node2(1,(tet1-1)*8+2) = tet_node1(2,tet1)
        tet_node2(1,(tet1-1)*8+3) = tet_node1(3,tet1)
        tet_node2(1,(tet1-1)*8+4) = tet_node1(4,tet1)
      end do

      node = node_num1

      n1_old = -1
      n2_old = -1

      do edge = 1, 6 * tet_num1
c
c  Read the data defining the edge.
c
        n1 = edge_data(1,edge)
        n2 = edge_data(2,edge)
c
c  If this edge is new, create the coordinates and index.
c
        if ( n1 .ne. n1_old .or. n2 .ne. n2_old ) then

          node = node + 1

          if ( node_num2 .lt. node ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 
     &        'TET_MESH_ORDER4_REFINE_COMPUTE - Fatal error!'
            write ( *, '(a)' ) '  Node index exceeds NODE_NUM2.'
            stop 1
          end if

          do i = 1, 3
            node_xyz2(i,node) =
     &        ( node_xyz2(i,n1) + node_xyz2(i,n2) ) / 2.0D+00
          end do

          n1_old = n1
          n2_old = n2

        end if
c
c  Assign the node to the tetrahedron.
c
        v1 = edge_data(3,edge)
        v2 = edge_data(4,edge)
        tet1 = edge_data(5,edge)
c
c  We know the two vertices that bracket this new node.
c  This tells us whether it is new node number 5, 6, 7, 8, 9 or 10.
c  This tells us which of the new subtetrahedrons it belongs to,
c  and what position it occupies.
c
        if ( v1 .eq. 1 .and. v2 .eq. 2 ) then

          tet_node2(2,(tet1-1)*8+1) = node
          tet_node2(2,(tet1-1)*8+2) = node
          tet_node2(1,(tet1-1)*8+5) = node
          tet_node2(1,(tet1-1)*8+6) = node

        else if ( v1 .eq. 1 .and. v2 .eq. 3 ) then

          tet_node2(3,(tet1-1)*8+1) = node
          tet_node2(2,(tet1-1)*8+3) = node
          tet_node2(2,(tet1-1)*8+5) = node
          tet_node2(2,(tet1-1)*8+6) = node
          tet_node2(1,(tet1-1)*8+7) = node
          tet_node2(1,(tet1-1)*8+8) = node

        else if ( v1 .eq. 1 .and. v2 .eq. 4 ) then

          tet_node2(4,(tet1-1)*8+1) = node
          tet_node2(2,(tet1-1)*8+4) = node
          tet_node2(3,(tet1-1)*8+5) = node
          tet_node2(2,(tet1-1)*8+7) = node

        else if ( v1 .eq. 2 .and. v2 .eq. 3 ) then

          tet_node2(3,(tet1-1)*8+2) = node
          tet_node2(3,(tet1-1)*8+3) = node
          tet_node2(3,(tet1-1)*8+6) = node
          tet_node2(2,(tet1-1)*8+8) = node

        else if ( v1 .eq. 2 .and. v2 .eq. 4 ) then

          tet_node2(4,(tet1-1)*8+2) = node
          tet_node2(4,(tet1-1)*8+3) = node
          tet_node2(3,(tet1-1)*8+4) = node
          tet_node2(4,(tet1-1)*8+5) = node
          tet_node2(4,(tet1-1)*8+6) = node
          tet_node2(3,(tet1-1)*8+7) = node
          tet_node2(3,(tet1-1)*8+8) = node

        else if ( v1 .eq. 3 .and. v2 .eq. 4 ) then

          tet_node2(4,(tet1-1)*8+4) = node
          tet_node2(4,(tet1-1)*8+7) = node
          tet_node2(4,(tet1-1)*8+8) = node

        end if

      end do

      return
      end
      subroutine tet_mesh_order4_refine_size ( node_num1, tet_num1, 
     &  tet_node1, node_num2, tet_num2, edge_data )

c*********************************************************************72
c
cc TET_MESH_ORDER4_REFINE_SIZE sizes a refined order 4 tet mesh.
c
c  Discussion:
c
c    A refined tet mesh can be derived from an existing one by interpolating
c    nodes at the midpoint of every edge of the mesh.
c
c    The mesh is described indirectly, as the sum of individual
c    tetrahedrons.  A single physical edge may be a logical edge of
c    any number of tetrahedrons.  It is important, however, that a
c    new node be created exactly once for each edge, assigned an index,
c    and associated with every tetrahedron that shares this edge.
c
c    This routine handles that problem.
c
c    The primary amount of work occurs in sorting a list of 6 * TET_NUM
c    data items, one item for every edge of every tetrahedron.  Each
c    data item records, for a given tetrahedron edge, the global indices
c    of the two endpoints, the local indices of the two endpoints,
c    and the index of the tetrahedron.
c
c    Through careful sorting, it is possible to arrange this data in
c    a way that allows the proper generation of the interpolated nodes.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM1, the number of nodes in the
c    original mesh.
c
c    Input, integer TET_NUM1, the number of tetrahedrons in the
c    original mesh.
c
c    Input, integer TET_NODE1(4,TET_NUM1), the indices of
c    the nodes that form the tetrahedrons in the input mesh.
c
c    Output, integer NODE_NUM2, the number of nodes in the
c    refined mesh.
c
c    Output, integer TET_NUM2, the number of tetrahedrons in the
c    refined mesh.
c
c    Output, integer EDGE_DATA(5,6*TET_NUM1), edge data.
c
      implicit none

      integer node_num1
      integer tet_num1

      integer a
      integer b
      integer edge
      integer edge_data(5,6*tet_num1)
      integer i
      integer j
      integer k
      integer l
      integer n1
      integer n1_old
      integer n2
      integer n2_old
      integer node_num2
      integer tet
      integer tet_node1(4,tet_num1)
      integer tet_num2
c
c  Step 1.
c  From the list of nodes for tetrahedron T, of the form: (I,J,K,L)
c  construct the six edge relations:
c
c    (I,J,1,2,T)
c    (I,K,1,3,T)
c    (I,L,1,4,T)
c    (J,K,2,3,T)
c    (J,L,2,4,T)
c    (K,L,3,4,T)
c
c  In order to make matching easier, we reorder each pair of nodes
c  into ascending order.
c
      do tet = 1, tet_num1

        i = tet_node1(1,tet)
        j = tet_node1(2,tet)
        k = tet_node1(3,tet)
        l = tet_node1(4,tet)

        call i4i4_sort_a ( i, j, a, b )

        edge_data(1,6*(tet-1)+1) = a
        edge_data(2,6*(tet-1)+1) = b
        edge_data(3,6*(tet-1)+1) = 1
        edge_data(4,6*(tet-1)+1) = 2
        edge_data(5,6*(tet-1)+1) = tet

        call i4i4_sort_a ( i, k, a, b )

        edge_data(1,6*(tet-1)+2) = a
        edge_data(2,6*(tet-1)+2) = b
        edge_data(3,6*(tet-1)+2) = 1
        edge_data(4,6*(tet-1)+2) = 3
        edge_data(5,6*(tet-1)+2) = tet

        call i4i4_sort_a ( i, l, a, b )

        edge_data(1,6*(tet-1)+3) = a
        edge_data(2,6*(tet-1)+3) = b
        edge_data(3,6*(tet-1)+3) = 1
        edge_data(4,6*(tet-1)+3) = 4
        edge_data(5,6*(tet-1)+3) = tet

        call i4i4_sort_a ( j, k, a, b )

        edge_data(1,6*(tet-1)+4) = a
        edge_data(2,6*(tet-1)+4) = b
        edge_data(3,6*(tet-1)+4) = 2
        edge_data(4,6*(tet-1)+4) = 3
        edge_data(5,6*(tet-1)+4) = tet

        call i4i4_sort_a ( j, l, a, b )

        edge_data(1,6*(tet-1)+5) = a
        edge_data(2,6*(tet-1)+5) = b
        edge_data(3,6*(tet-1)+5) = 2
        edge_data(4,6*(tet-1)+5) = 4
        edge_data(5,6*(tet-1)+5) = tet

        call i4i4_sort_a ( k, l, a, b )

        edge_data(1,6*(tet-1)+6) = a
        edge_data(2,6*(tet-1)+6) = b
        edge_data(3,6*(tet-1)+6) = 3
        edge_data(4,6*(tet-1)+6) = 4
        edge_data(5,6*(tet-1)+6) = tet

      end do
c
c  Step 2. Perform an ascending dictionary sort on the neighbor relations.
c  We only intend to sort on rows 1:2; the routine we call here
c  sorts on the full column but that won't hurt us.
c
c  What we need is to find all cases where tetrahedrons share an edge.
c  By sorting the columns of the EDGE_DATA array, we will put shared edges
c  next to each other.
c
      call i4col_sort_a ( 5, 6*tet_num1, edge_data )
c
c  Step 3. All the tetrahedrons which share an edge show up as consecutive
c  columns with identical first two entries.  Figure out how many new
c  nodes there are, and allocate space for their coordinates.
c
      node_num2 = node_num1

      n1_old = -1
      n2_old = -1

      do edge = 1, 6 * tet_num1
        n1 = edge_data(1,edge)
        n2 = edge_data(2,edge)
        if ( n1 .ne. n1_old .or. n2 .ne. n2_old ) then
          node_num2 = node_num2 + 1
          n1_old = n1
          n2_old = n2
        end if
      end do

      tet_num2 = 8 * tet_num1

      return
      end
      subroutine tet_mesh_order4_to_order10_compute ( tet_num, 
     &  tet_node1, node_num1, node_xyz1, edge_data, tet_node2, 
     &  node_num2, node_xyz2 )

c*********************************************************************72
c
cc TET_MESH_ORDER4_TO_ORDER10_COMPUTE: quadratic tet mesh from a linear one.
c
c  Discussion:
c
c    A quadratic (10 node) tet mesh can be derived from a linear
c    (4 node) tet mesh by interpolating nodes at the midpoint of
c    every edge of the mesh.
c
c    The mesh is described indirectly, as the sum of individual
c    tetrahedrons.  A single physical edge may be a logical edge of
c    any number of tetrahedrons.  It is important, however, that a
c    new node be created exactly once for each edge, assigned an index,
c    and associated with every tetrahedron that shares this edge.
c
c    This routine handles that problem.
c
c    The primary amount of work occurs in sorting a list of 6 * TET_NUM
c    data items, one item for every edge of every tetrahedron.  Each
c    data item records, for a given tetrahedron edge, the global indices
c    of the two endpoints, the local indices of the two endpoints,
c    and the index of the tetrahedron.
c
c    Through careful sorting, it is possible to arrange this data in
c    a way that allows the proper generation of the interpolated nodes.
c
c    The node ordering for the quadratic tetrahedron is somewhat
c    arbitrary.  In the current scheme, the vertices are listed
c    first, followed by the 6 midside nodes.  Each midside node
c    may be identified by the two vertices that bracket it.  Thus,
c    the node ordering may be suggested by:
c
c      1  2  3  4 (1+2) (1+3) (1+4) (2+3) (2+4) (3+4)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer TET_NUM, the number of tetrahedrons in the
c    linear mesh.
c
c    Input, integer TET_NODE1(4,TET_NUM), the indices of
c    the nodes in the linear mesh.
c
c    Input, integer NODE_NUM1, the number of nodes for the
c    linear mesh.
c
c    Input, double precision NODE_XYZ1(3,NODE_NUM1), the coordinates of
c    the nodes that make up the linear mesh.
c
c    Input, integer EDGE_DATA(5,6*TET_NUM), edge data.
c
c    Output, integer TET_NODE2(10,TET_NUM), the indices of
c    the nodes in the quadratic mesh.
c
c    Input, integer NODE_NUM2, the number of nodes for the
c    quadratic mesh.
c
c    Output, double precision NODE_XYZ2(3,NODE_NUM2), the coordinates of
c    the nodes that make up the quadratic mesh.
c
      implicit none

      integer node_num1
      integer node_num2
      integer tet_num

      integer edge
      integer edge_data(5,6*tet_num)
      integer i
      integer j
      integer n1
      integer n1_old
      integer n2
      integer n2_old
      integer node
      double precision node_xyz1(3,node_num1)
      double precision node_xyz2(3,node_num2)
      integer tet
      integer tet_node1(4,tet_num)
      integer tet_node2(10,tet_num)
      integer v
      integer v1
      integer v2
c
c  Generate the index and coordinates of the new midside nodes,
c  and update the tetradehron node data.
c
      do j = 1, node_num1
        do i = 1, 3
          node_xyz2(i,j) = node_xyz1(i,j)
        end do
      end do

      do j = 1, tet_num
        do i = 1, 4
          tet_node2(i,j) = tet_node1(i,j)
        end do
      end do

      node = node_num1

      n1_old = -1
      n2_old = -1

      do edge = 1, 6 * tet_num
c
c  Read the data defining the edge.
c
        n1 = edge_data(1,edge)
        n2 = edge_data(2,edge)
c
c  If this edge is new, create the coordinates and index.
c
        if ( n1 .ne. n1_old .or. n2 .ne. n2_old ) then

          node = node + 1

          if ( node_num2 .lt. node ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 
     &        'TET_MESH_ORDER4_TO_ORDER10_COMPUTE - Fatal error!'
            write ( *, '(a)' ) '  Node index exceeds NODE_NUM2.'
            stop 1
          end if

          do i = 1, 3
            node_xyz2(i,node) = 
     &        ( node_xyz2(i,n1) + node_xyz2(i,n2) ) / 2.0D+00
          end do

          n1_old = n1
          n2_old = n2

        end if
c
c  Assign the node to the tetrahedron.
c
        v1 = edge_data(3,edge)
        v2 = edge_data(4,edge)
c
c  Here is where the local ordering of the nodes is effected:
c
        if ( v1 .eq. 1 .and. v2 .eq. 2 ) then
          v = 5
        else if ( v1 .eq. 1 .and. v2 .eq. 3 ) then
          v = 6
        else if ( v1 .eq. 1 .and. v2 .eq. 4 ) then
          v = 7
        else if ( v1 .eq. 2 .and. v2 .eq. 3 ) then
          v = 8
        else if ( v1 .eq. 2 .and. v2 .eq. 4 ) then
          v = 9
        else if ( v1 .eq. 3 .and. v2 .eq. 4 ) then
          v = 10
        end if

        tet = edge_data(5,edge)

        tet_node2(v,tet) = node

      end do

      return
      end
      subroutine tet_mesh_order4_to_order10_size ( tet_num, tet_node1,  
     &  node_num1, edge_data, node_num2 )

c*********************************************************************72
c
cc TET_MESH_ORDER4_TO_ORDER10_SIZE sizes a quadratic tet mesh from a linear one.
c
c  Discussion:
c
c    A quadratic (10 node) tet mesh can be derived from a linear
c    (4 node) tet mesh by interpolating nodes at the midpoint of
c    every edge of the mesh.
c
c    The mesh is described indirectly, as the sum of individual
c    tetrahedrons.  A single physical edge may be a logical edge of
c    any number of tetrahedrons.  It is important, however, that a
c    new node be created exactly once for each edge, assigned an index,
c    and associated with every tetrahedron that shares this edge.
c
c    This routine handles that problem.
c
c    The primary amount of work occurs in sorting a list of 6 * TET_NUM
c    data items, one item for every edge of every tetrahedron.  Each
c    data item records, for a given tetrahedron edge, the global indices
c    of the two endpoints, the local indices of the two endpoints,
c    and the index of the tetrahedron.
c
c    Through careful sorting, it is possible to arrange this data in
c    a way that allows the proper generation of the interpolated nodes.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 October 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer TET_NUM, the number of tetrahedrons in the
c    linear mesh.
c
c    Input, integer TET_NODE1(4,TET_NUM), the indices of
c    the nodes in the linear mesh.
c
c    Input, integer NODE_NUM1, the number of nodes for the
c    linear mesh.
c
c    Output, integer EDGE_DATA(5,6*TET_NUM), edge data.
c
c    Output, integer NODE_NUM2, the number of nodes for the
c    quadratic mesh.
c
      implicit none

      integer node_num1
      integer tet_num

      integer a
      integer b
      integer edge
      integer edge_data(5,6*tet_num)
      integer i
      integer j
      integer k
      integer l
      integer n1
      integer n1_old
      integer n2
      integer n2_old
      integer node_num2
      integer tet
      integer tet_node1(4,tet_num)
c
c  Step 1.
c  From the list of nodes for tetrahedron T, of the form: (I,J,K,L)
c  construct the six edge relations:
c
c    (I,J,1,2,T)
c    (I,K,1,3,T)
c    (I,L,1,4,T)
c    (J,K,2,3,T)
c    (J,L,2,4,T)
c    (K,L,3,4,T)
c
c  In order to make matching easier, we reorder each pair of nodes
c  into ascending order.
c
      do tet = 1, tet_num

        i = tet_node1(1,tet)
        j = tet_node1(2,tet)
        k = tet_node1(3,tet)
        l = tet_node1(4,tet)

        call i4i4_sort_a ( i, j, a, b )

        edge_data(1,6*(tet-1)+1) = a
        edge_data(2,6*(tet-1)+1) = b
        edge_data(3,6*(tet-1)+1) = 1
        edge_data(4,6*(tet-1)+1) = 2
        edge_data(5,6*(tet-1)+1) = tet

        call i4i4_sort_a ( i, k, a, b )

        edge_data(1,6*(tet-1)+2) = a
        edge_data(2,6*(tet-1)+2) = b
        edge_data(3,6*(tet-1)+2) = 1
        edge_data(4,6*(tet-1)+2) = 3
        edge_data(5,6*(tet-1)+2) = tet

        call i4i4_sort_a ( i, l, a, b )

        edge_data(1,6*(tet-1)+3) = a
        edge_data(2,6*(tet-1)+3) = b
        edge_data(3,6*(tet-1)+3) = 1
        edge_data(4,6*(tet-1)+3) = 4
        edge_data(5,6*(tet-1)+3) = tet

        call i4i4_sort_a ( j, k, a, b )

        edge_data(1,6*(tet-1)+4) = a
        edge_data(2,6*(tet-1)+4) = b
        edge_data(3,6*(tet-1)+4) = 2
        edge_data(4,6*(tet-1)+4) = 3
        edge_data(5,6*(tet-1)+4) = tet

        call i4i4_sort_a ( j, l, a, b )

        edge_data(1,6*(tet-1)+5) = a
        edge_data(2,6*(tet-1)+5) = b
        edge_data(3,6*(tet-1)+5) = 2
        edge_data(4,6*(tet-1)+5) = 4
        edge_data(5,6*(tet-1)+5) = tet

        call i4i4_sort_a ( k, l, a, b )

        edge_data(1,6*(tet-1)+6) = a
        edge_data(2,6*(tet-1)+6) = b
        edge_data(3,6*(tet-1)+6) = 3
        edge_data(4,6*(tet-1)+6) = 4
        edge_data(5,6*(tet-1)+6) = tet

      end do
c
c  Step 2. Perform an ascending dictionary sort on the neighbor relations.
c  We only intend to sort on rows 1:2; the routine we call here
c  sorts on the full column but that won't hurt us.
c
c  What we need is to find all cases where tetrahedrons share an edge.
c  By sorting the columns of the EDGE_DATA array, we will put shared edges
c  next to each other.
c
      call i4col_sort_a ( 5, 6*tet_num, edge_data )
c
c  Step 3. All the tetrahedrons which share an edge show up as consecutive
c  columns with identical first two entries.  Figure out how many new
c  nodes there are, and allocate space for their coordinates.
c
      node_num2 = node_num1

      n1_old = -1
      n2_old = -1

      do edge = 1, 6 * tet_num
        n1 = edge_data(1,edge)
        n2 = edge_data(2,edge)
        if ( n1 .ne. n1_old .or. n2 .ne. n2_old ) then
          node_num2 = node_num2 + 1
          n1_old = n1
          n2_old = n2
        end if
      end do

      return
      end
      subroutine tet_mesh_order10_adj_count ( node_num, tet_num, 
     &  tet_node, adj_num, adj_row )

c*********************************************************************72
c
cc TET_MESH_ORDER10_ADJ_COUNT counts the number of nodal adjacencies.
c
c  Discussion:
c
c    Assuming that the tet mesh is to be used in a finite element
c    computation, we declare that two distinct nodes are "adjacent" if and
c    only if they are both included in some tetrahedron.
c
c    It is the purpose of this routine to determine the number of
c    such adjacency relationships.
c
c    The initial count gets only the (I,J) relationships, for which
c    node I is strictly less than node J.  This value is doubled
c    to account for symmetry.
c
c    Thanks to Iban Constenia Rozados for an important correction,
c    28 February 2013.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 February 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(10,TET_NUM), the indices of
c    the nodes.
c
c    Output, integer ADJ_NUM, the total number of adjacency
c    relationships,
c
c    Output, integer ADJ_ROW(NODE_NUM+1), the ADJ pointer array.
c
      implicit none

      integer tet_num
      integer node_num

      integer adj_num
      integer adj_row(node_num+1)
      integer i
      integer j
      integer k
      integer l
      integer pair(2,45*tet_num)
      integer pair_num
      integer pair_unique_num
      integer tet_node(10,tet_num)
c
c  Each order 10 tetrahedron defines 45 adjacency pairs.
c
      k = 0
      do i = 1, 9
        do j = i + 1, 10
          do l = 1, tet_num
            pair(1,k*tet_num+l) = tet_node(i,l)
            pair(2,k*tet_num+l) = tet_node(j,l)
          end do
          k = k + 1
        end do
      end do
c
c  Force the nodes of each pair to be listed in ascending order.
c
      pair_num = 45 * tet_num
      call i4col_sort2_a ( 2, pair_num, pair )
c
c  Rearrange the columns in ascending order.
c
      call i4col_sort_a ( 2, pair_num, pair )
c
c  Get the number of unique columns.
c
      call i4col_sorted_unique_count ( 2, pair_num, pair, 
     &  pair_unique_num )
c
c  The number of adjacencies is TWICE this value, plus the number of nodes.
c
      adj_num = 2 * pair_unique_num
c
c  Now set up the ADJ_ROW counts.
c
      do i = 1, node_num
        adj_row(i) = 0
      end do

      do k = 1, pair_num

        if ( 1 .lt. k ) then
          if ( pair(1,k-1) .eq. pair(1,k) .and.
     &         pair(2,k-1) .eq. pair(2,k) ) then
            go to 10
          end if
        end if

        i = pair(1,k)
        j = pair(2,k)

        adj_row(i) = adj_row(i) + 1
        adj_row(j) = adj_row(j) + 1

10      continue

      end do
c
c  We used ADJ_ROW to count the number of entries in each row.
c  Convert it to pointers into the ADJ array.
c
      do i = node_num, 1, -1
        adj_row(i+1) = adj_row(i)
      end do

      adj_row(1) = 1
      do i = 2, node_num+1
        adj_row(i) = adj_row(i-1) + adj_row(i)
      end do

      return
      end
      subroutine tet_mesh_order10_adj_set ( node_num, tet_num, tet_node,
     &   adj_num, adj_row, adj )

c*********************************************************************72
c
cc TET_MESH_ORDER10_ADJ_SET sets the nodal adjacency matrix.
c
c  Discussion:
c
c    A compressed format is used for the nodal adjacency matrix.
c
c    It is assumed that we know ADJ_NUM, the number of adjacency entries
c    and the ADJ_ROW array, which keeps track of the list of slots
c    in ADJ where we can store adjacency information for each row.
c
c    We essentially repeat the work of TET_MESH_ORDER10_ADJ_COUNT, but
c    now we have a place to store the adjacency information.
c
c    A copy of the ADJ_ROW array is useful, as we can use it to keep track
c    of the next available entry in ADJ for adjacencies associated with
c    a given row.
c
c    Thanks to Iban Constenia Rozados for an important correction,
c    28 February 2013.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 February 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(10,TET_NUM), the indices of
c    the nodes.
c
c    Input, integer ADJ_NUM, the total number of adjacency
c    relationships,
c
c    Input, integer ADJ_ROW(NODE_NUM+1), the ADJ pointer array.
c
c    Output, integer ADJ(ADJ_NUM), the adjacency information.
c
      implicit none

      integer adj_num
      integer tet_num
      integer node_num

      integer adj(adj_num)
      integer adj_row(node_num+1)
      integer adj_row_copy(node_num+1)
      integer i
      integer j
      integer k
      integer l
      integer pair(2,45*tet_num)
      integer pair_num
      integer tet_node(10,tet_num)
c
c  Each order 10 tetrahedron defines 45 adjacency pairs.
c
      k = 0
      do i = 1, 9
        do j = i + 1, 10
          do l = 1, tet_num
            pair(1,k*tet_num+l) = tet_node(i,l)
            pair(2,k*tet_num+l) = tet_node(j,l)
          end do
          k = k + 1
        end do
      end do
c
c  Force the nodes of each pair to be listed in ascending order.
c
      pair_num = 45 * tet_num
      call i4col_sort2_a ( 2, pair_num, pair )
c
c  Rearrange the columns in ascending order.
c
      call i4col_sort_a ( 2, pair_num, pair )
c
c  Mark all entries of ADJ so we will know later if we missed one.
c
      do i = 1, adj_num
        adj(i) = -1
      end do
c
c  Copy the ADJ_ROW array and use it to keep track of the next
c  free entry for each row.
c
      do i = 1, node_num
        adj_row_copy(i) = adj_row(i)
      end do
c
c  Now set up the ADJ_ROW counts.
c
      do k = 1, pair_num

        if ( 1 .lt. k ) then
          if ( pair(1,k-1) .eq. pair(1,k) .and.
     &         pair(2,k-1) .eq. pair(2,k) ) then
            go to 10
          end if
        end if

        i = pair(1,k)
        j = pair(2,k)

        adj(adj_row_copy(i)) = j
        adj_row_copy(i) = adj_row_copy(i) + 1
        adj(adj_row_copy(j)) = i
        adj_row_copy(j) = adj_row_copy(j) + 1

10      continue

      end do

      return
      end
      subroutine tet_mesh_order10_example_set ( node_num, tet_num,
     &  node_xyz, tet_node )

c*********************************************************************72
c
cc TET_MESH_ORDER10_EXAMPLE_SET sets an example quadratic tet mesh.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Output, double precision NODE_XYZ(3,NODE_NUM), the node coordinates.
c
c    Output, integer TET_NODE(10,TET_NUM), the nodes
c    forming each tet.
c
      implicit none

      integer node_num
      integer tet_num

      double precision node_xyz(3,node_num)
      double precision node_xyz_save(3,27)
      integer tet_node(10,tet_num)
      integer tet_node_save(10,6)

      save node_xyz_save
      save tet_node_save

      data node_xyz_save /
     &  0.0D+00,  0.0D+00,  0.0D+00,
     &  0.0D+00,  0.0D+00,  1.0D+00,
     &  0.0D+00,  1.0D+00,  0.0D+00,
     &  0.0D+00,  1.0D+00,  1.0D+00,
     &  1.0D+00,  0.0D+00,  0.0D+00,
     &  1.0D+00,  0.0D+00,  1.0D+00,
     &  1.0D+00,  1.0D+00,  0.0D+00, 
     &  1.0D+00,  1.0D+00,  1.0D+00,
     &  0.0D+00,  0.0D+00,  0.5D+00,   
     &  0.0D+00,  0.5D+00,  0.0D+00,
     &  0.0D+00,  0.5D+00,  0.5D+00,
     &  0.5D+00,  0.0D+00,  0.0D+00,
     &  0.0D+00,  0.5D+00,  1.0D+00,
     &  0.5D+00,  0.0D+00,  0.5D+00, 
     &  0.5D+00,  0.0D+00,  1.0D+00,
     &  0.0D+00,  1.0D+00,  0.5D+00,
     &  0.5D+00,  0.5D+00,  0.0D+00,
     &  0.5D+00,  1.0D+00,  0.0D+00,
     &  0.5D+00,  0.5D+00,  0.5D+00,
     &  0.5D+00,  0.5D+00,  1.0D+00,
     &  0.5D+00,  1.0D+00,  0.5D+00,
     &  0.5D+00,  1.0D+00,  1.0D+00,
     &  1.0D+00,  0.0D+00,  0.5D+00,
     &  1.0D+00,  0.5D+00,  0.0D+00,
     &  1.0D+00,  0.5D+00,  0.5D+00,
     &  1.0D+00,  0.5D+00,  1.0D+00,
     &  1.0D+00,  1.0D+00,  0.5D+00    /

      data tet_node_save /
     &  4,   3,   5,   1,  16,  19,  17,  11,  10,  12,
     &  4,   2,   5,   1,  13,  19,  14,  11,   9,  12,
     &  4,   7,   3,   5,  21,  16,  18,  19,  24,  17,   
     &  4,   7,   8,   5,  21,  22,  27,  19,  24,  25,
     &  4,   6,   2,   5,  20,  13,  15,  19,  23,  14,
     &  4,   6,   8,   5,  20,  22,  26,  19,  23,  25 /

      call i4mat_copy ( 10, tet_num, tet_node_save, tet_node )
      call r8mat_copy ( 3, node_num, node_xyz_save, node_xyz )

      return
      end
      subroutine tet_mesh_order10_example_size ( node_num, tet_num )

c*********************************************************************72
c
cc TET_MESH_ORDER10_EXAMPLE_SIZE sizes an example quadratic tet mesh.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer NODE_NUM, the number of nodes.
c
c    Output, integer TET_NUM, the number of tetrahedrons.
c
      implicit none

      integer node_num
      integer tet_num

      node_num = 27
      tet_num = 6

      return
      end
      subroutine tet_mesh_order10_to_order4_compute ( tet_num1, 
     &  tet_node1, tet_num2, tet_node2 )

c*********************************************************************72
c
cc TET_MESH_ORDER10_TO_ORDER4_COMPUTE linearizes a quadratic tet mesh.
c
c  Discussion:
c
c    A quadratic tet mesh is assumed to consist of 10-node
c    tetrahedrons.
c
c    This routine rearranges the information so as to define a 4-node
c    tet mesh.
c
c    The same nodes are used, but there are 8 times as many
c    tetrahedrons.
c
c    The node ordering for the quadratic tetrahedron is somewhat
c    arbitrary.  In the current scheme, the vertices are listed
c    first, followed by the 6 midside nodes.  Each midside node
c    may be identified by the two vertices that bracket it.  Thus,
c    the node ordering may be suggested by:
c
c      1  2  3  4 (1+2) (1+3) (1+4) (2+3) (2+4) (3+4)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Anwei Liu, Barry Joe,
c    Quality Local Refinement of Tetrahedral Meshes Based
c    on 8-Subtetrahedron Subdivision,
c    Mathematics of Computation,
c    Volume 65, Number 215, July 1996, pages 1183-1200.
c
c  Parameters:
c
c    Input, integer TET_NUM1, the number of tetrahedrons in
c    the quadratic tet mesh.
c
c    Input, integer TET_NODE1(10,TET_NUM1), the indices of
c    the nodes in the quadratic tet mesh.
c
c    Input, integer TET_NUM2, the number of tetrahedrons in
c    the linear tet mesh.  TET_NUM2 = 8 * TET_NUM1.
c
c    Output, integer TET_NODE2(4,TET_NUM2), the indices of
c    the nodes in the linear tet mesh.
c
      implicit none

      integer tet_num1
      integer tet_num2

      integer n1
      integer n2
      integer n3
      integer n4
      integer n5
      integer n6
      integer n7
      integer n8
      integer n9
      integer nx
      integer tet1
      integer tet2
      integer tet_node1(10,tet_num1)
      integer tet_node2(4,tet_num2)

      tet2 = 0

      do tet1 = 1, tet_num1

        n1 = tet_node1(1,tet1)
        n2 = tet_node1(2,tet1)
        n3 = tet_node1(3,tet1)
        n4 = tet_node1(4,tet1)
        n5 = tet_node1(5,tet1)
        n6 = tet_node1(6,tet1)
        n7 = tet_node1(7,tet1)
        n8 = tet_node1(8,tet1)
        n9 = tet_node1(9,tet1)
        nx = tet_node1(10,tet1)

        tet2 = tet2 + 1
        tet_node2(1,tet2) = n1
        tet_node2(2,tet2) = n5
        tet_node2(3,tet2) = n6
        tet_node2(4,tet2) = n7
        tet2 = tet2 + 1
        tet_node2(1,tet2) = n2
        tet_node2(2,tet2) = n5
        tet_node2(3,tet2) = n8
        tet_node2(4,tet2) = n9
        tet2 = tet2 + 1
        tet_node2(1,tet2) = n3
        tet_node2(2,tet2) = n6
        tet_node2(3,tet2) = n8
        tet_node2(4,tet2) = n9
        tet2 = tet2 + 1
        tet_node2(1,tet2) = n4
        tet_node2(2,tet2) = n7
        tet_node2(3,tet2) = n9
        tet_node2(4,tet2) = nx
        tet2 = tet2 + 1
        tet_node2(1,tet2) = n5
        tet_node2(2,tet2) = n6
        tet_node2(3,tet2) = n7
        tet_node2(4,tet2) = n9
        tet2 = tet2 + 1
        tet_node2(1,tet2) = n5
        tet_node2(2,tet2) = n6
        tet_node2(3,tet2) = n8
        tet_node2(4,tet2) = n9
        tet2 = tet2 + 1
        tet_node2(1,tet2) = n6
        tet_node2(2,tet2) = n7
        tet_node2(3,tet2) = n9
        tet_node2(4,tet2) = nx
        tet2 = tet2 + 1
        tet_node2(1,tet2) = n6
        tet_node2(2,tet2) = n8
        tet_node2(3,tet2) = n9
        tet_node2(4,tet2) = nx

      end do

      return
      end
      subroutine tet_mesh_order10_to_order4_size ( node_num1, tet_num1, 
     &  node_num2, tet_num2 )

c*********************************************************************72
c
cc TET_MESH_ORDER10_TO_ORDER4_SIZE sizes a linear tet mesh from a quadratic one.
c
c  Discussion:
c
c    A linear (4 node) tet mesh can be derived from a quadratic
c    (10 node) tet mesh using the same set of nodes, but reassigning
c    the nodes of each quadratic tet among 8 linear subtets.
c
c    This routine returns the number of nodes and tetrahedrons in the
c    linear mesh.
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
c  Reference:
c
c    Anwei Liu, Barry Joe,
c    Quality Local Refinement of Tetrahedral Meshes Based
c    on 8-Subtetrahedron Subdivision,
c    Mathematics of Computation,
c    Volume 65, Number 215, July 1996, pages 1183-1200.
c
c  Parameters:
c
c    Input, integer NODE_NUM1, the number of nodes in the
c    quadratic mesh.
c
c    Input, integer TET_NUM1, the number of tetrahedrons in the
c    quadratic mesh.
c
c    Output, integer NODE_NUM2, the number of nodes for the
c    linear mesh.
c
c    Output, integer TET_NUM2, the number of tetrahedrons in the
c    linear mesh.
c
      implicit none

      integer node_num1
      integer node_num2
      integer tet_num1
      integer tet_num2

      node_num2 = node_num1
      tet_num2 = 8 * tet_num1

      return
      end
      subroutine tet_mesh_quad ( node_num, node_xyz, tet_order,
     &  tet_num, tet_node, f, quad_num, quad_xyz, quad_w, quad_value,
     &  region_volume )

c*********************************************************************72
c
cc TET_MESH_QUAD approximates an integral over a tet mesh.
c
c  Discussion:
c
c    The routine will accept tetrahedral meshes of order higher than 4.
c    However, only the first four nodes (the vertices) of each
c    tetrahedron will be used.  This will still produce correct results
c    for higher order tet meshes, as long as the sides of each
c    tetrahedron are flat (linear).
c
c    We assume that the vertices of each tetrahedron are listed first
c    in the description of higher order tetrahedrons.
c
c    The approximation of the integral is made using a quadrature rule
c    defined on the unit tetrahedron, and supplied by the user.
c
c    The user also supplies the name of a subroutine, here called "F", which
c    evaluates the integrand at a set of points.  The form of F is:
c
c      subroutine f ( n, xyz_vec, f_vec )
c      integer n
c      double precision f_vec(n)
c      double precision xyz_vec(3,n)
c
c    and it returns in each entry F_VEC(1:N), the value of the integrand
c    at XYZ_VEC(1:3,1:N).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes in the tet mesh.
c
c    Input, double precision NODE_XYZ(3,NODE_NUM), the coordinates
c    of the nodes.
c
c    Input, integer TET_ORDER, the order of tetrahedrons in
c    the tet mesh.
c
c    Input, integer TET_NUM, the number of tetrahedrons in the
c    tet mesh.
c
c    Input, integer TET_NODE(TET_ORDER,TET_NUM), the indices
c    of the nodes.
c
c    Input, external F, the name of the subroutine that evaluates the integrand.
c
c    Input, integer QUAD_NUM, the order of the quadrature rule.
c
c    Input, double precision QUAD_XYZ(3,QUAD_NUM), the abscissas of the
c    quadrature rule, in the unit tetrahedron.
c
c    Input, double precision QUAD_W(QUAD_NUM), the weights of the
c    quadrature rule.
c
c    Output, double precision QUAD_VALUE, the estimate of the integral
c    of F(X,Y) over the region covered by the tet mesh.
c
c    Output, double precision REGION_VOLUME, the volume of the region.
c
      implicit none

      integer node_num
      integer quad_num
      integer tet_num
      integer tet_order

      external f
      integer i
      integer j
      double precision node_xyz(3,node_num)
      double precision quad_f(quad_num)
      double precision quad_value
      double precision quad_w(quad_num)
      double precision quad_xyz(3,quad_num)
      double precision quad2_xyz(3,quad_num)
      double precision r8vec_dot_product
      double precision region_volume
      integer tet
      double precision tet_volume
      integer tet_node(tet_order,tet_num)
      double precision tet_xyz(3,4)

      quad_value = 0.0D+00
      region_volume = 0.0D+00

      do tet = 1, tet_num

        do j = 1, 4
          do i = 1, 3
            tet_xyz(i,j) = node_xyz(i,tet_node(j,tet))
          end do
        end do

        call tetrahedron_volume ( tet_xyz, tet_volume )

        call tetrahedron_order4_reference_to_physical ( tet_xyz,
     &    quad_num, quad_xyz, quad2_xyz )

        call f ( quad_num, quad2_xyz, quad_f )

        quad_value = quad_value + tet_volume * 
     &    r8vec_dot_product ( quad_num, quad_w, quad_f )

        region_volume = region_volume + tet_volume

      end do

      return
      end
      subroutine tet_mesh_quality1 ( node_num, node_xyz, tet_order, 
     &  tet_num, tet_node, tet_quality )

c*********************************************************************72
c
cc TET_MESH_QUALITY1 returns the quality of each tet in a mesh.
c
c  Discussion:
c
c    The overall tet mesh quality measure is the minimum of the corresponding
c    tetrahedron quality measure, over all tetrahedrons in the tet mesh.
c
c    Although tetrahedrons of order 10 are allowed as input,
c    only the first 4 nodes (presumed to be the vertices) are used
c    in the calculation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XYZ(3,NODE_NUM), the nodes.
c
c    Input, integer TET_ORDER, the order of the mesh, either
c    4 or 10.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(TET_ORDER,TET_NUM), the
c    indices of the nodes.
c
c    Output, double precision TET_QUALITY(TET_NUM), the quality
c    measure for each tetrahedron.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer node_num
      integer tet_num
      integer tet_order

      integer i
      integer j
      double precision node_xyz(dim_num,node_num)
      integer tet
      integer tet_node(tet_order,tet_num)
      double precision tet_quality(tet_num)
      double precision tet_xyz(dim_num,4)

      do tet = 1, tet_num

        do j = 1, 4
          do i = 1, dim_num
            tet_xyz(i,j) = node_xyz(i,tet_node(j,tet))
          end do
        end do

        call tetrahedron_quality1 ( tet_xyz, tet_quality(tet) )

      end do

      return
      end
      subroutine tet_mesh_quality2 ( node_num, node_xyz, tet_order, 
     &  tet_num, tet_node, tet_quality )

c*********************************************************************72
c
cc TET_MESH_QUALITY2 returns the quality of each tet in a mesh.
c
c  Discussion:
c
c    The overall tet mesh quality measure is the minimum of the
c    corresponding tetrahedron quality measure, over all tetrahedrons in the
c    tet mesh.
c
c    Although tetrahedrons of order 10 are allowed as input,
c    only the first 4 nodes (presumed to be the vertices) are used
c    in the calculation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XYZ(3,NODE_NUM), the nodes.
c
c    Input, integer TET_ORDER, the order of the mesh, either
c    4 or 10.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(TET_ORDER,TET_NUM), the
c    indices of the nodes.
c
c    Output, double precision TET_QUALITY(TET_NUM), the quality
c    measure for each tetrahedron.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer node_num
      integer tet_num
      integer tet_order

      integer i
      integer j
      double precision node_xyz(dim_num,node_num)
      integer tet
      integer tet_node(tet_order,tet_num)
      double precision tet_quality(tet_num)
      double precision tet_xyz(dim_num,4)

      do tet = 1, tet_num

        do j = 1, 4
          do i = 1, dim_num
            tet_xyz(i,j) = node_xyz(i,tet_node(j,tet))
          end do
        end do

        call tetrahedron_quality2 ( tet_xyz, tet_quality(tet) )

      end do

      return
      end
      subroutine tet_mesh_quality3 ( node_num, node_xyz, tet_order, 
     &  tet_num, tet_node, tet_quality )

c*********************************************************************72
c
cc TET_MESH_QUALITY3 returns the quality of each tet in a mesh.
c
c  Discussion:
c
c    The overall tet mesh quality measure is the minimum of the
c    corresponding tetrahedron quality measure, over all tetrahedrons in the
c    tet mesh.
c
c    Although tetrahedrons of order 10 are allowed as input,
c    only the first 4 nodes (presumed to be the vertices) are used
c    in the calculation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XYZ(3,NODE_NUM), the nodes.
c
c    Input, integer TET_ORDER, the order of the mesh, either
c    4 or 10.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(TET_ORDER,TET_NUM),
c    the indices of the nodes.
c
c    Output, double precision TET_QUALITY(TET_NUM), the quality
c    measure for each tetrahedron.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer node_num
      integer tet_num
      integer tet_order

      integer i
      integer j
      double precision node_xyz(dim_num,node_num)
      integer tet
      integer tet_node(tet_order,tet_num)
      double precision tet_quality(tet_num)
      double precision tet_xyz(dim_num,4)

      do tet = 1, tet_num

        do j = 1, 4
          do i = 1, dim_num
            tet_xyz(i,j) = node_xyz(i,tet_node(j,tet))
          end do
        end do

        call tetrahedron_quality3 ( tet_xyz, tet_quality(tet) )

      end do

      return
      end
      subroutine tet_mesh_quality4 ( node_num, node_xyz, tet_order, 
     &  tet_num, tet_node, tet_quality )

c*********************************************************************72
c
cc TET_MESH_QUALITY4 returns the quality of each tet in a mesh.
c
c  Discussion:
c
c    The overall tet mesh quality measure is the minimum of the
c    corresponding tetrahedron quality measure, over all tetrahedrons in the
c    tet mesh.
c
c    Although tetrahedrons of order 10 are allowed as input,
c    only the first 4 nodes (presumed to be the vertices) are used
c    in the calculation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XYZ(3,NODE_NUM), the nodes.
c
c    Input, integer TET_ORDER, the order of the mesh, either
c    4 or 10.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(TET_ORDER,TET_NUM), the
c    indices of the nodes.
c
c    Output, double precision TET_QUALITY(TET_NUM), the quality
c    measure for each tetrahedron.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer node_num
      integer tet_num
      integer tet_order

      integer i
      integer j
      double precision node_xyz(dim_num,node_num)
      integer tet
      integer tet_node(tet_order,tet_num)
      double precision tet_quality(tet_num)
      double precision tet_xyz(dim_num,4)

      do tet = 1, tet_num

        do j = 1, 4
          do i = 1, dim_num
            tet_xyz(i,j) = node_xyz(i,tet_node(j,tet))
          end do
        end do

        call tetrahedron_quality4 ( tet_xyz, tet_quality(tet) )

      end do

      return
      end
      subroutine tet_mesh_quality5 ( node_num, node_xyz, tet_order, 
     &  tet_num, tet_node, tet_quality )

c*********************************************************************72
c
cc TET_MESH_QUALITY5 returns the quality of each tet in a mesh.
c
c  Discussion:
c
c    The overall tet mesh quality measure is the ratio of the minimum
c    tetrahedron volume to the maximum tetrahedron volume.
c
c    Although tetrahedrons of order 10 are allowed as input,
c    only the first 4 nodes (presumed to be the vertices) are used
c    in the calculation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 November 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XYZ(3,NODE_NUM), the nodes.
c
c    Input, integer TET_ORDER, the order of the mesh,
c    either 4 or 10.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(TET_ORDER,TET_NUM),
c    the indices of the nodes.
c
c    Output, double precision TET_QUALITY(TET_NUM), the quality
c    measure for each tetrahedron.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer node_num
      integer tet_num
      integer tet_order

      integer i
      integer j
      double precision node_xyz(dim_num,node_num)
      double precision r8vec_max
      integer tet
      integer tet_node(tet_order,tet_num)
      double precision tet_quality(tet_num)
      double precision tet_xyz(dim_num,4)
      double precision volume_max

      do tet = 1, tet_num

        do j = 1, 4
          do i = 1, dim_num
            tet_xyz(i,j) = node_xyz(i,tet_node(j,tet))
          end do
        end do

        call tetrahedron_volume ( tet_xyz, tet_quality(tet) )

      end do

      volume_max = r8vec_max ( tet_num, tet_quality )

      if ( 0.0D+00 .lt. volume_max ) then
        do i = 1, tet_num
          tet_quality(i) = tet_quality(i) / volume_max
        end do
      end if

      return
      end
      subroutine tet_mesh_search_delaunay ( node_num, node_xyz, 
     &  tet_order, tet_num, tet_node, tet_neighbor, p, tet_index, 
     &  face, step_num )

c*********************************************************************72
c
cc TET_MESH_SEARCH_DELAUNAY searches a Delaunay tet mesh for a point.
c
c  Discussion:
c
c    The algorithm "walks" from one tetrahedron to its neighboring tetrahedron,
c    and so on, until a tetrahedron is found containing point P, or P is found
c    to be outside the convex hull.
c
c    The algorithm computes the barycentric coordinates of the point with
c    respect to the current tetrahedron.  If all 4 quantities are positive,
c    the point is contained in the tetrahedron.  If the I-th coordinate is
c    negative, then P lies on the far side of edge I, which is opposite
c    from vertex I.  This gives a hint as to where to search next.
c
c    For a Delaunay tet mesh, the search is guaranteed to terminate.
c    For other meshes, a cycle may occur.
c
c    Note the surprising fact that, even for a Delaunay tet mesh of
c    a set of nodes, the nearest node to P need not be one of the
c    vertices of the tetrahedron containing P.
c
c    The code can be called for tet meshes of any order, but only
c    the first 4 nodes in each tetrahedron are considered.  Thus, if
c    higher order tetrahedrons are used, and the extra nodes are intended
c    to give the tetrahedron a polygonal shape, these will have no effect,
c    and the results obtained here might be misleading.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2009
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Barry Joe,
c    GEOMPACK - a software package for the generation of meshes
c    using geometric algorithms,
c    Advances in Engineering Software,
c    Volume 13, pages 325-331, 1991.
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XYZ(3,NODE_NUM), the coordinates of
c    the nodes.
c
c    Input, integer TET_ORDER, the order of the tetrahedrons.
c
c    Input, integer TET_NUM, the number of tetrahedrons.
c
c    Input, integer TET_NODE(TET_ORDER,TET_NUM),
c    the nodes that make up each tetrahedron.
c
c    Input, integer TET_NEIGHBOR(4,TET_NUM), the
c    tetrahedron neighbor list.
c
c    Input, double precision P(3), the coordinates of a point.
c
c    Output, integer TET_INDEX, the index of the tetrahedron
c    where the search ended.  If a cycle occurred, then TET_INDEX = -1.
c
c    Output, integer FACE, indicates the position of the point P in
c    face TET_INDEX:
c    0, the interior or boundary of the tetrahedron;
c    -1, outside the convex hull of the tet mesh, past face 1;
c    -2, outside the convex hull of the tet mesh, past face 2;
c    -3, outside the convex hull of the tet mesh, past face 3.
c    -4, outside the convex hull of the tet mesh, past face 4.
c
c    Output, integer STEP_NUM, the number of steps taken.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer node_num
      integer tet_num
      integer tet_order

      double precision alpha(dim_num+1)
      integer face
      integer i
      integer j
      double precision node_xyz(dim_num,node_num)
      double precision p(dim_num)
      integer step_num
      double precision t(3,4)
      integer tet_node(tet_order,tet_num)
      integer tet_index
      integer tet_index_save
      integer tet_neighbor(dim_num+1,tet_num)

      save tet_index_save

      data tet_index_save / -1 /
c
c  If possible, start with the previous successful value of TET_INDEX.
c
      if ( tet_index_save .lt. 1 .or. tet_num .lt. tet_index_save ) then
        tet_index = ( tet_num + 1 ) / 2
      else
        tet_index = tet_index_save
      end if

      step_num = -1
      face = 0

10    continue

        step_num = step_num + 1

        if ( tet_num .lt. step_num ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'TET_MESH_SEARCH_DELAUNAY - Fatal error!'
          write ( *, '(a)' ) '  The algorithm seems to be cycling.'
          stop 1
        end if

        do j = 1, 4
          do i = 1, 3
            t(i,j) = node_xyz(i,tet_node(j,tet_index))
          end do
        end do

        call tetrahedron_barycentric ( t, p, alpha )
c
c  If the barycentric coordinates are all positive, then the point
c  is inside the tetrahedron and we're done.
c
        if ( 0.0D+00 .le. alpha(1) .and.
     &       0.0D+00 .le. alpha(2) .and.
     &       0.0D+00 .le. alpha(3) .and.
     &       0.0D+00 .le. alpha(4) ) then
          go to 20
        end if
c
c  At least one barycentric coordinate is negative.
c
c  If there is a negative barycentric coordinate for which there exists an
c  opposing tetrahedron neighbor closer to the point, move to that tetrahedron.
c
        if ( alpha(1) .lt. 0.0D+00 .and. 
     &     0 .lt. tet_neighbor(1,tet_index) ) then
          tet_index = tet_neighbor(1,tet_index)
          go to 10
        else if ( alpha(2) .lt. 0.0D+00 .and.
     &    0 .lt. tet_neighbor(2,tet_index) ) then
          tet_index = tet_neighbor(2,tet_index)
          go to 10
        else if ( alpha(3) .lt. 0.0D+00 .and.
     &    0 .lt. tet_neighbor(3,tet_index) ) then
          tet_index = tet_neighbor(3,tet_index)
          go to 10
        else if ( alpha(4) .lt. 0.0D+00 .and.
     &    0 .lt. tet_neighbor(4,tet_index) ) then
          tet_index = tet_neighbor(4,tet_index)
          go to 10
        end if
c
c  All negative barycentric coordinates correspond to vertices opposite
c  faces on the convex hull.
c
c  Note the face and exit.
c
        if ( alpha(1) .lt. 0.0D+00 ) then
          face = -1
          go to 20
        else if ( alpha(2) .lt. 0.0D+00 ) then
          face = -2
          go to 20
        else if ( alpha(3) .lt. 0.0D+00 ) then
          face = -3
          go to 20
        else if ( alpha(4) .lt. 0.0D+00 ) then
          face = -4
          go to 20
        end if

      go to 10

20    continue

      tet_index_save = tet_index

      return
      end
      subroutine tet_mesh_search_naive ( node_num, node_xyz, 
     &  tet_order, tet_num, tet_node, p, tet_index, step_num )

c*********************************************************************72
c
cc TET_MESH_SEARCH_NAIVE naively searches a tet mesh.
c
c  Discussion:
c
c    The algorithm simply checks each tetrahedron to see if point P is
c    contained in it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 August 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer NODE_NUM, the number of nodes.
c
c    Input, double precision NODE_XYZ(3,NODE_NUM), the coordinates
c    of the nodes.
c
c    Input, integer TET_ORDER, the order of the tetrahedrons.
c
c    Input, integer TET_NUM, the number of tetrahedrons in
c    the mesh.
c
c    Input, integer TET_NODE(TET_ORDER,TET_NUM),
c    the nodes that make up each tetrahedron.
c
c    Input, double precision P(3), the coordinates of a point.
c
c    Output, integer TET_INDEX, the index of the tetrahedron
c    where the search ended, or -1 if no tetrahedron was found containing
c    the point.
c
c    Output, integer STEP_NUM, the number of tetrahedrons checked.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer node_num
      integer tet_num
      integer tet_order

      double precision alpha(4)
      integer i
      integer j
      double precision node_xyz(dim_num,node_num)
      double precision p(dim_num)
      integer step_num
      double precision t(3,4)
      integer tet
      integer tet_node(tet_order,tet_num)
      integer tet_index

      tet_index = -1
      step_num = 0

      do tet = 1, tet_num

        do j = 1, 4
          do i = 1, 3
            t(i,j) = node_xyz(i,tet_node(j,tet))
          end do
        end do

        call tetrahedron_barycentric ( t, p, alpha )

        if ( 
     &    0.0D+00 <= alpha(1) .and.
     &    0.0D+00 <= alpha(2) .and.
     &    0.0D+00 <= alpha(3) .and.
     &    0.0D+00 <= alpha(4) ) then
          tet_index = tet
          step_num = tet
          return
        end if

      end do

      return
      end
      subroutine tetrahedron_barycentric ( tetra, p, c )

c*********************************************************************72
c
cc TETRAHEDRON_BARYCENTRIC: barycentric coordinates of a point.
c
c  Discussion:
c
c    The barycentric coordinates of a point P with respect to
c    a tetrahedron are a set of four values C(1:4), each associated
c    with a vertex of the tetrahedron.  The values must sum to 1.
c    If all the values are between 0 and 1, the point is contained
c    within the tetrahedron.
c
c    The barycentric coordinate of point P related to vertex A can be
c    interpreted as the ratio of the volume of the tetrahedron with
c    vertex A replaced by vertex P to the volume of the original
c    tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4) the tetrahedron vertices.
c
c    Input, double precision P(3), the point to be checked.
c
c    Output, double precision C(4), the barycentric coordinates of P with
c    respect to the tetrahedron.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer rhs_num
      parameter ( rhs_num = 1 )

      double precision a(dim_num,dim_num+rhs_num)
      double precision c(dim_num+1)
      integer i
      integer info
      integer j 
      double precision p(dim_num)
      double precision tetra(dim_num,4)
c
c  Set up the linear system
c
c    ( X2-X1  X3-X1  X4-X1 ) C2    X - X1
c    ( Y2-Y1  Y3-Y1  Y4-Y1 ) C3  = Y - Y1
c    ( Z2-Z1  Z3-Z1  Z4-Z1 ) C4    Z - Z1
c
c  which is satisfied by the barycentric coordinates of P.
c
      do j = 1, 3
        do i = 1, dim_num
          a(i,j) = tetra(i,j+1)
        end do
      end do

      do i = 1, dim_num
        a(i,4) = p(i)
      end do

      do i = 1, dim_num
        a(i,1:4) = a(i,1:4) - tetra(i,1)
      end do
c
c  Solve the linear system.
c
      call r8mat_solve ( dim_num, rhs_num, a, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TETRAHEDRON_BARYCENTRIC - Fatal error!'
        write ( *, '(a)' ) '  The linear system is singular.'
        write ( *, '(a)' ) 
     &    '  The input data does not form a proper tetrahedron.'
        stop 1
      end if

      do i = 1, dim_num
        c(i+1) = a(i,4)
      end do

      c(1) = 1.0D+00 - ( c(2) + c(3) + c(4) )

      return
      end
      subroutine tetrahedron_circumsphere ( tetra, r, pc )

c*********************************************************************72
c
cc TETRAHEDRON_CIRCUMSPHERE computes the circumsphere of a tetrahedron.
c
c  Discussion:
c
c    The circumsphere, or circumscribed sphere, of a tetrahedron is the
c    sphere that passes through the four vertices.  The circumsphere is
c    not necessarily the smallest sphere that contains the tetrahedron.
c
c    Surprisingly, the diameter of the sphere can be found by solving
c    a 3 by 3 linear system.  This is because the vectors P2 - P1,
c    P3 - P1 and P4 - P1 are secants of the sphere, and each forms a
c    right triangle with the diameter through P1.  Hence, the dot product of
c    P2 - P1 with that diameter is equal to the square of the length
c    of P2 - P1, and similarly for P3 - P1 and P4 - P1.  This determines
c    the diameter vector originating at P1, and hence the radius and
c    center.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Adrian Bowyer, John Woodwark,
c    A Programmer's Geometry,
c    Butterworths, 1983.
c
c  Parameters:
c
c    Input, double precision TETRA(3,4) the tetrahedron vertices.
c
c    Output, double precision R, PC(3), the center of the
c    circumscribed sphere, and its radius.  If the linear system is
c    singular, then R = -1, PC(1:3) = 0.
c
      implicit none

      double precision a(3,4)
      integer i
      integer info
      integer j
      double precision pc(3)
      double precision r
      double precision tetra(3,4)
c
c  Set up the linear system.
c
      do j = 1, 3
        do i = 1, 4
          a(i,j) = tetra(j,i+1)
        end do
      end do

      do j = 1, 3
        do i = 1, 3
          a(i,j) = a(i,j) - tetra(j,1)
        end do
      end do

      do i = 1, 3
        a(i,4) = 0.0D+00
        do j = 1, 3
          a(i,4) = a(i,4) + a(i,j)**2
        end do
      end do
c
c  Solve the linear system.
c
      call r8mat_solve ( 3, 1, a, info )
c
c  If the system was singular, return a consolation prize.
c
      if ( info .ne. 0 ) then
        r = -1.0D+00
        do i = 1, 3
          pc(i) = 0.0D+00
        end do
        return
      end if
c
c  Compute the radius and center.
c
      r = 0.0D+00
      do i = 1, 3
        r = r + a(i,4)**2
      end do
      r = 0.5D+00 * sqrt ( r )

      do i = 1, 3
        pc(i) = tetra(i,1) + 0.5D+00 * a(i,4)
      end do

      return
      end
      subroutine tetrahedron_edge_length ( tetra, edge_length )

c*********************************************************************72
c
cc TETRAHEDRON_EDGE_LENGTH returns edge lengths of a tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the tetrahedron vertices.
c
c    Output, double precision EDGE_LENGTH(6), the length of the edges.
c
      implicit none

      double precision edge_length(6)
      integer i
      integer j1
      integer j2
      integer k
      double precision r8vec_norm
      double precision t(3)
      double precision tetra(3,4)

      k = 0
      do j1 = 1, 3
        do j2 = j1 + 1, 4
          k = k + 1
          do i = 1, 3
            t(i) = tetra(i,j2) - tetra(i,j1)
          end do
          edge_length(k) = r8vec_norm ( 3, t )
         end do
      end do

      return
      end
      subroutine tetrahedron_insphere ( tetra, r, pc )

c*********************************************************************72
c
cc TETRAHEDRON_INSPHERE finds the insphere of a tetrahedron.
c
c  Discussion:
c
c    The insphere of a tetrahedron is the inscribed sphere, which touches
c    each face of the tetrahedron at a single point.
c
c    The points of contact are the centroids of the triangular faces
c    of the tetrahedron.  Therefore, the point of contact for a face
c    can be computed as the average of the vertices of that face.
c
c    The sphere can then be determined as the unique sphere through
c    the four given centroids.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Philip Schneider, David Eberly,
c    Geometric Tools for Computer Graphics,
c    Elsevier, 2002,
c    ISBN: 1558605940,
c    LC: T385.G6974.
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the vertices of the tetrahedron.
c
c    Output, double precision R, PC(3), the radius and the center
c    of the sphere.
c
      implicit none

      double precision b(4,4)
      double precision r8mat_det_4d
      double precision r8vec_norm
      double precision gamma
      integer i
      integer j
      double precision l123
      double precision l124
      double precision l134
      double precision l234
      double precision n123(3)
      double precision n124(3)
      double precision n134(3)
      double precision n234(3)
      double precision pc(3)
      double precision r
      double precision tetra(3,4)
      double precision v21(3)
      double precision v31(3)
      double precision v41(3)
      double precision v32(3)
      double precision v42(3)
      double precision v43(3)

      call tetrahedron_edges ( tetra, v21, v31, v41, v32, v42, v43 )

      call r8vec_cross_product_3d ( v21, v31, n123 )
      call r8vec_cross_product_3d ( v41, v21, n124 )
      call r8vec_cross_product_3d ( v31, v41, n134 )
      call r8vec_cross_product_3d ( v42, v32, n234 )

      l123 = r8vec_norm ( 3, n123 )
      l124 = r8vec_norm ( 3, n124 )
      l134 = r8vec_norm ( 3, n134 )
      l234 = r8vec_norm ( 3, n234 )

      do i = 1, 3
        pc(i) = ( l234 * tetra(i,1)   
     &          + l134 * tetra(i,2)   
     &          + l124 * tetra(i,3)   
     &          + l123 * tetra(i,4) ) 
     &          / ( l234 + l134 + l124 + l123 )
      end do

      do j = 1, 4
        do i = 1, 3
          b(i,j) = tetra(i,j)
        end do
        b(4,j) = 1.0D+00
      end do

      gamma = abs ( r8mat_det_4d ( b ) )

      r = gamma / ( l234 + l134 + l124 + l123 )

      return
      end
      subroutine tetrahedron_order4_physical_to_reference ( tet_xyz, 
     &  n, phy, ref )

c*********************************************************************72
c
cc TETRAHEDRON_ORDER4_PHYSICAL_TO_REFERENCE: physical to reference points.
c
c  Discussion:
c
c    Given the vertices of an order 4 physical tetrahedron and a point
c    (X,Y,Z) in the physical tetrahedron, the routine computes the value
c    of the corresponding image point (R,S,T) in reference space.
c
c    This routine may be appropriate for an order 10 tetrahedron,
c    if the mapping between reference and physical space is linear.
c    This implies, in particular, that the edges of the image tetrahedron
c    are straight, the faces are flat, and the "midside" nodes in the
c    physical tetrahedron are halfway along the sides of the physical
c    tetrahedron.
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
c    Input, double precision TET_XYZ(3,4), the coordinates of the vertices.
c    The vertices are assumed to be the images of
c    (0,0,0), (1,0,0), (0,1,0) and (0,0,1) respectively.
c
c    Input, integer N, the number of points to transform.
c
c    Input, double precision PHY(3,N), the coordinates of physical points
c    to be transformed.
c
c    Output, double precision REF(3,N), the coordinates of the corresponding
c    points in the reference space.
c
      implicit none

      integer n

      double precision a(3,3)
      double precision det
      integer i
      integer j
      double precision phy(3,n)
      double precision ref(3,n)
      double precision tet_xyz(3,4)
c
c  Set up the matrix.
c
      do i = 1, 3
        a(i,1) = tet_xyz(i,2) - tet_xyz(i,1)
        a(i,2) = tet_xyz(i,3) - tet_xyz(i,1)
        a(i,3) = tet_xyz(i,4) - tet_xyz(i,1)
      end do
c
c  Compute the determinant.
c
      det =  a(1,1) * ( a(2,2) * a(3,3) - a(2,3) * a(3,2) )
     &     + a(1,2) * ( a(2,3) * a(3,1) - a(2,1) * a(3,3) )
     &     + a(1,3) * ( a(2,1) * a(3,2) - a(2,2) * a(3,1) )
c
c  If the determinant is zero, bail out.
c
      if ( det .eq. 0.0D+00 ) then
        do j = 1, n
          do i = 1, 3
            ref(1:3,1:n) = 0.0D+00
          end do
        end do
        return
      end if
c
c  Compute the solution.
c
      do j = 1, n

        ref(1,j) = ( 
     &      ( a(2,2) * a(3,3) - a(2,3) * a(3,2) ) 
     &    * ( phy(1,j) - tet_xyz(1,1) )
     &    - ( a(1,2) * a(3,3) - a(1,3) * a(3,2) ) 
     &    * ( phy(2,j) - tet_xyz(2,1) )  
     &    + ( a(1,2) * a(2,3) - a(1,3) * a(2,2) ) 
     &    * ( phy(3,j) - tet_xyz(3,1) )            
     &    ) / det

        ref(2,j) = ( 
     &    - ( a(2,1) * a(3,3) - a(2,3) * a(3,1) )            
     &    * ( phy(1,j) - tet_xyz(1,1) )
     &    + ( a(1,1) * a(3,3) - a(1,3) * a(3,1) )
     &    * ( phy(2,j) - tet_xyz( 2,1) )
     &    - ( a(1,1) * a(2,3) - a(1,3) * a(2,1) )
     &    * ( phy(3,j) - tet_xyz(3,1) )
     &    ) / det

        ref(3,j) = (
     &      ( a(2,1) * a(3,2) - a(2,2) * a(3,1) )            
     &    * ( phy(1,j) - tet_xyz(1,1) )
     &    - ( a(1,1) * a(3,2) - a(1,2) * a(3,1) )
     &    * ( phy(2,j) - tet_xyz( 2,1) )
     &    + ( a(1,1) * a(2,2) - a(1,2) * a(2,1) )                  
     &    * ( phy(3,j) - tet_xyz(3,1) )            
     &    ) / det

      end do

      return
      end
      subroutine tetrahedron_order4_reference_to_physical ( tet_xyz, 
     &  n, ref, phy )

c*********************************************************************72
c
cc TETRAHEDRON_ORDER4_REFERENCE_TO_PHYSICAL: T4 reference to physical points.
c
c  Discussion:
c
c    Given the vertices of an order 4 physical tetrahedron and a point
c    (R,S,T) in the reference tetrahedron, the routine computes the value
c    of the corresponding image point (X,Y,Z) in physical space.
c
c    This routine will also be correct for an order 10 tetrahedron,
c    if the mapping between reference and physical space
c    is linear.  This implies, in particular, that the sides of the
c    image tetrahedron are straight, the faces are flat, and
c    the "midside" nodes in the physical tetrahedron are
c    halfway along the edges of the physical tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TET_XYZ(3,4), the coordinates of the vertices.
c    The vertices are assumed to be the images of (0,0,0), (1,0,0),
c    (0,1,0) and (0,0,1) respectively.
c
c    Input, integer N, the number of points to transform.
c
c    Input, double precision REF(3,N), points in the reference tetrahedron.
c
c    Output, double precision PHY(3,N), corresponding points in the
c    physical tetrahedron.
c
      implicit none

      integer n

      integer i
      integer j
      double precision phy(3,n)
      double precision ref(3,n)
      double precision tet_xyz(3,4)

      do j = 1, n
        do i = 1, 3
          phy(i,j) =
     &      tet_xyz(i,1) * ( 1.0D+00 - ref(1,j) - ref(2,j) - ref(3,j) )
     &    + tet_xyz(i,2) *             ref(1,j)
     &    + tet_xyz(i,3) *                        ref(2,j)
     &    + tet_xyz(i,4) *                                   ref(3,j)
        end do
      end do

      return
      end
      subroutine tetrahedron_quality1 ( tetra, quality )

c*********************************************************************72
c
cc TETRAHEDRON_QUALITY1: "quality" of a tetrahedron.
c
c  Discussion:
c
c    The quality of a tetrahedron is 3 times the ratio of the radius of
c    the inscribed sphere divided by that of the circumscribed sphere.
c
c    An equilateral tetrahredron achieves the maximum possible quality of 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the tetrahedron vertices.
c
c    Output, double precision QUALITY, the quality of the tetrahedron.
c
      implicit none

      double precision pc(3)
      double precision quality
      double precision r_in
      double precision r_out
      double precision tetra(3,4)

      call tetrahedron_circumsphere ( tetra, r_out, pc )

      call tetrahedron_insphere ( tetra, r_in, pc )

      quality = 3.0D+00 * r_in / r_out

      return
      end
      subroutine tetrahedron_quality2 ( tetra, quality2 )

c*********************************************************************72
c
cc TETRAHEDRON_QUALITY2: "quality" of a tetrahedron.
c
c  Discussion:
c
c    The quality measure #2 of a tetrahedron is:
c
c      QUALITY2 = 2 * sqrt ( 6 ) * RIN / LMAX
c
c    where
c
c      RIN = radius of the inscribed sphere;
c      LMAX = length of longest side of the tetrahedron.
c
c    An equilateral tetrahredron achieves the maximum possible quality of 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 August 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Qiang Du, Desheng Wang,
c    The Optimal Centroidal Voronoi Tesselations and the Gersho's
c    Conjecture in the Three-Dimensional Space,
c    Computers and Mathematics with Applications,
c    Volume 49, 2005, pages 1355-1373.
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the tetrahedron vertices.
c
c    Output, double precision QUALITY2, the quality of the tetrahedron.
c
      implicit none

      double precision edge_length(6)
      double precision l_max
      double precision pc(3)
      double precision quality2
      double precision r_in
      double precision r8vec_max
      double precision tetra(3,4)

      call tetrahedron_edge_length ( tetra, edge_length )

      l_max = r8vec_max ( 6, edge_length )

      call tetrahedron_insphere ( tetra, r_in, pc )

      quality2 = 2.0D+00 * sqrt ( 6.0D+00 ) * r_in / l_max

      return
      end
      subroutine tetrahedron_quality3 ( tetra, quality3 )

c*********************************************************************72
c
cc TETRAHEDRON_QUALITY3 computes the mean ratio of a tetrahedron.
c
c  Discussion:
c
c    This routine computes QUALITY3, the eigenvalue or mean ratio of
c    a tetrahedron.
c
c      QUALITY3 = 12 * ( 3 * volume )^(2/3) / (sum of squares of edge lengths).
c
c    This value may be used as a shape quality measure for the tetrahedron.
c
c    For an equilateral tetrahedron, the value of this quality measure
c    will be 1.  For any other tetrahedron, the value will be between
c    0 and 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2005
c
c  Author:
c
c    Original FORTRAN77 version by Barry Joe.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Barry Joe,
c    GEOMPACK - a software package for the generation of meshes
c    using geometric algorithms,
c    Advances in Engineering Software,
c    Volume 13, pages 325-331, 1991.
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the vertices of the tetrahedron.
c
c    Output, double precision QUALITY3, the mean ratio of the tetrahedron.
c
      implicit none

      double precision ab(3)
      double precision ac(3)
      double precision ad(3)
      double precision bc(3)
      double precision bd(3)
      double precision cd(3)
      double precision denom
      double precision lab
      double precision lac
      double precision lad
      double precision lbc
      double precision lbd
      double precision lcd
      double precision quality3
      double precision r8vec_normsq
      double precision tetra(3,4)
      double precision volume
c
c  Compute the vectors representing the sides of the tetrahedron.
c
      call tetrahedron_edges ( tetra, ab, ac, ad, bc, bd, cd )
c
c  Compute the squares of the lengths of the sides.
c
      lab = r8vec_normsq ( 3, ab )
      lac = r8vec_normsq ( 3, ac )
      lad = r8vec_normsq ( 3, ad )
      lbc = r8vec_normsq ( 3, bc )
      lbd = r8vec_normsq ( 3, bd )
      lcd = r8vec_normsq ( 3, cd )
c
c  Compute the volume.
c
      volume = abs ( 
     &    ab(1) * ( ac(2) * ad(3) - ac(3) * ad(2) ) 
     &  + ab(2) * ( ac(3) * ad(1) - ac(1) * ad(3) ) 
     &  + ab(3) * ( ac(1) * ad(2) - ac(2) * ad(1) ) ) / 6.0D+00

      denom = lab + lac + lad + lbc + lbd + lcd

      if ( denom .eq. 0.0D+00 ) then
        quality3 = 0.0D+00
      else
        quality3 = 12.0D+00 
     &    * ( 3.0D+00 * volume )**( 2.0D+00 / 3.0D+00 ) / denom
      end if

      return
      end
      subroutine tetrahedron_quality4 ( tetra, quality4 )

c*********************************************************************72
c
cc TETRAHEDRON_QUALITY4 computes the minimum solid angle of a tetrahedron.
c
c  Discussion:
c
c    This routine computes a quality measure for a tetrahedron, based
c    on the sine of half the minimum of the four solid angles.
c
c    The quality measure for an equilateral tetrahedron should be 1,
c    since the solid angles of such a tetrahedron are each equal to pi.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2005
c
c  Author:
c
c    Original FORTRAN77 version by Barry Joe.
c    This version by John Burkardt.
c
c  Reference:
c
c    Barry Joe,
c    GEOMPACK - a software package for the generation of meshes
c    using geometric algorithms,
c    Advances in Engineering Software,
c    Volume 13, pages 325-331, 1991.
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the vertices of the tetrahedron.
c
c    Output, double precision QUALITY4, the value of the quality measure.
c
      implicit none

      double precision ab(3)
      double precision ac(3)
      double precision ad(3)
      double precision bc(3)
      double precision bd(3)
      double precision cd(3)
      double precision denom
      double precision l1
      double precision l2
      double precision l3
      double precision lab
      double precision lac
      double precision lad
      double precision lbc
      double precision lbd
      double precision lcd
      double precision quality4
      double precision r8vec_norm
      double precision tetra(3,4)
      double precision volume
c
c  Compute the vectors that represent the sides.
c
      call tetrahedron_edges ( tetra, ab, ac, ad, bc, bd, cd )
c
c  Compute the lengths of the sides.
c
      lab = r8vec_norm ( 3, ab )
      lac = r8vec_norm ( 3, ac )
      lad = r8vec_norm ( 3, ad )
      lbc = r8vec_norm ( 3, bc )
      lbd = r8vec_norm ( 3, bd )
      lcd = r8vec_norm ( 3, cd )
c
c  Compute the volume
c
      volume = abs ( 
     &    ab(1) * ( ac(2) * ad(3) - ac(3) * ad(2) ) 
     &  + ab(2) * ( ac(3) * ad(1) - ac(1) * ad(3) ) 
     &  + ab(3) * ( ac(1) * ad(2) - ac(2) * ad(1) ) ) / 6.0D+00

      quality4 = 1.0D+00

      l1 = lab + lac
      l2 = lab + lad
      l3 = lac + lad

      denom = ( l1 + lbc ) * ( l1 - lbc ) 
     &      * ( l2 + lbd ) * ( l2 - lbd ) 
     &      * ( l3 + lcd ) * ( l3 - lcd )

      if ( denom .le. 0.0D+00 ) then
        quality4 = 0.0D+00
      else
        quality4 = min ( quality4, 12.0D+00 * volume / sqrt ( denom ) )
      end if

      l1 = lab + lbc
      l2 = lab + lbd
      l3 = lbc + lbd

      denom = ( l1 + lac ) * ( l1 - lac ) 
     &      * ( l2 + lad ) * ( l2 - lad ) 
     &      * ( l3 + lcd ) * ( l3 - lcd )

      if ( denom .le. 0.0D+00 ) then
        quality4 = 0.0D+00
      else
        quality4 = min ( quality4, 12.0D+00 * volume / sqrt ( denom ) )
      end if

      l1 = lac + lbc
      l2 = lac + lcd
      l3 = lbc + lcd

      denom = ( l1 + lab ) * ( l1 - lab ) 
     &      * ( l2 + lad ) * ( l2 - lad ) 
     &      * ( l3 + lbd ) * ( l3 - lbd )

      if ( denom .le. 0.0D+00 ) then
        quality4 = 0.0D+00
      else
        quality4 = min ( quality4, 12.0D+00 * volume / sqrt ( denom ) )
      end if

      l1 = lad + lbd
      l2 = lad + lcd
      l3 = lbd + lcd

      denom = ( l1 + lab ) * ( l1 - lab ) 
     &      * ( l2 + lac ) * ( l2 - lac ) 
     &      * ( l3 + lbc ) * ( l3 - lbc )

      if ( denom .le. 0.0D+00 ) then
        quality4 = 0.0D+00
      else
        quality4 = min ( quality4, 12.0D+00 * volume / sqrt ( denom ) )
      end if

      quality4 = quality4 * 1.5D+00 * sqrt ( 6.0D+00 )

      return
      end
      subroutine tetrahedron_reference_sample ( n, seed, p )

c*********************************************************************72
c
cc TETRAHEDRON_REFERENCE_SAMPLE samples points in the reference tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points to sample.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision P(3,N), random points in the tetrahedron.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer n

      double precision alpha
      double precision beta
      double precision gamma
      integer i
      integer j
      double precision p(dim_num,n)
      double precision r
      double precision r8_uniform_01
      integer seed

      do j = 1, n

        r = r8_uniform_01 ( seed )
c
c  Interpret R as a percentage of the tetrahedron's volume.
c
c  Imagine a plane, parallel to face 1, so that the volume between
c  vertex 1 and the plane is R percent of the full tetrahedron volume.
c
c  The plane will intersect sides 12, 13, and 14 at a fraction
c  ALPHA = R^1/3 of the distance from vertex 1 to vertices 2, 3, and 4.
c
        alpha = r ** ( 1.0D+00 / 3.0D+00 )
c
c  Determine the coordinates of the points on sides 12, 13 and 14 intersected
c  by the plane, which form a triangle TR.
c
c  Now choose, uniformly at random, a point in this triangle.
c
        r = r8_uniform_01 ( seed )
c
c  Interpret R as a percentage of the triangle's area.
c
c  Imagine a line L, parallel to side 1, so that the area between
c  vertex 1 and line L is R percent of the full triangle's area.
c
c  The line L will intersect sides 2 and 3 at a fraction
c  ALPHA = SQRT ( R ) of the distance from vertex 1 to vertices 2 and 3.
c
        beta = sqrt ( r )
c
c  Determine the coordinates of the points on sides 2 and 3 intersected
c  by line L.
c
c
c  Now choose, uniformly at random, a point on the line L.
c
        gamma = r8_uniform_01 ( seed )

        p(1,j) = alpha * ( 1.0D+00 - beta ) *             gamma
        p(2,j) = alpha *             beta   * ( 1.0D+00 - gamma )
        p(3,j) = alpha *             beta   *             gamma

      end do

      return
      end
      subroutine tetrahedron_sample ( tet_xyz, n, seed, p )

c*********************************************************************72
c
cc TETRAHEDRON_SAMPLE returns random points in a tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TET_XYZ(3,4), the coordinates of the vertices.
c
c    Input, integer N, the  number of points to sample.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision P(3,N), random points in the tetrahedron.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer n

      double precision alpha
      double precision beta
      double precision gamma
      integer i
      integer j
      double precision p(dim_num,n)
      double precision p12(dim_num)
      double precision p13(dim_num)
      double precision r
      double precision r8_uniform_01
      integer seed
      double precision tet_xyz(dim_num,4)
      double precision tr(dim_num,3)

      do j = 1, n

        r = r8_uniform_01 ( seed )
c
c  Interpret R as a percentage of the tetrahedron's volume.
c
c  Imagine a plane, parallel to face 1, so that the volume between
c  vertex 1 and the plane is R percent of the full tetrahedron volume.
c
c  The plane will intersect sides 12, 13, and 14 at a fraction
c  ALPHA = R^1/3 of the distance from vertex 1 to vertices 2, 3, and 4.
c
        alpha = r ** ( 1.0D+00 / 3.0D+00 )
c
c  Determine the coordinates of the points on sides 12, 13 and 14 intersected
c  by the plane, which form a triangle TR.
c
        do i = 1, dim_num
          tr(i,1) = ( 1.0D+00 - alpha ) * tet_xyz(i,1)   
     &                        + alpha   * tet_xyz(i,2)
          tr(i,2) = ( 1.0D+00 - alpha ) * tet_xyz(i,1)   
     &                        + alpha   * tet_xyz(i,3)
          tr(i,3) = ( 1.0D+00 - alpha ) * tet_xyz(i,1)   
     &                        + alpha   * tet_xyz(i,4)
        end do
c
c  Now choose, uniformly at random, a point in this triangle.
c
        r = r8_uniform_01 ( seed )
c
c  Interpret R as a percentage of the triangle's area.
c
c  Imagine a line L, parallel to side 1, so that the area between
c  vertex 1 and line L is R percent of the full triangle's area.
c
c  The line L will intersect sides 2 and 3 at a fraction
c  ALPHA = SQRT ( R ) of the distance from vertex 1 to vertices 2 and 3.
c
        beta = sqrt ( r )
c
c  Determine the coordinates of the points on sides 2 and 3 intersected
c  by line L.
c
        do i = 1, dim_num
          p12(i) = ( 1.0D+00 - beta ) * tr(i,1)           
     &                       + beta   * tr(i,2)
          p13(i) = ( 1.0D+00 - beta ) * tr(i,1)           
     &                       + beta   * tr(i,3)
        end do
c
c  Now choose, uniformly at random, a point on the line L.
c
        gamma = r8_uniform_01 ( seed )

        do i = 1, dim_num
          p(i,j) = ( 1.0D+00 - gamma ) * p12(i)           
     &           +             gamma   * p13(i)
        end do


      end do

      return
      end
      subroutine tetrahedron_volume ( tetra, volume )

c*********************************************************************72
c
cc TETRAHEDRON_VOLUME computes the volume of a tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision TETRA(3,4), the vertices of the tetrahedron.
c
c    Output, double precision VOLUME, the volume of the tetrahedron.
c
      implicit none

      double precision a(4,4)
      integer i
      integer j
      double precision r8mat_det_4d
      double precision tetra(3,4)
      double precision volume

      do j = 1, 4
        do i = 1, 3
          a(i,j) = tetra(i,j)
        end do
        a(4,j) = 1.0D+00
      end do

      volume = abs ( r8mat_det_4d ( a ) ) / 6.0D+00

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
