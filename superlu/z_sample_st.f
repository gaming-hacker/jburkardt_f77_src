      program main

c*********************************************************************72
c
cc Z_SAMPLE_ST calls SUPERLU to solve a system.
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
      integer nst
      parameter ( nst = 12 )

      double complex acc(12)
      double complex ast(nst)
      double complex b(n)
      double complex b2(n)
      integer ccc(n+1)
      integer factors(8)
      integer i
      integer info
      integer iopt
      integer icc(12)
      integer ist(nst)
      integer jst(nst)
      integer ldb
      integer ncc
      integer nrhs

      save ast
      save ist
      save jst

      data ast /
     & 19.0, 12.0, 12.0, 
     & 21.0, 12.0, 12.0, 
     & 21.0, 16.0, 
     & 21.0,  5.0, 
     & 21.0, 18.0 /
      data ist /
     &  1, 2, 5, 
     &  2, 3, 5, 
     &  1, 3, 
     &  1, 4, 
     &  4, 5 /
      data jst /
     &  1, 1, 1, 
     &  2, 2, 2, 
     &  3, 3,
     &  4, 4,
     &  5, 5 /

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Z_SAMPLE_ST'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  ZGSSV factors and solves a linear system'
      write ( *, '(a)' ) '  using double precision complex arithmetic.'

      write ( *, '(a,i6)' ) '  Matrix order N = ', n
      write ( *, '(a,i6)' ) '  Matrix nonzeros NST = ', nst
c
c  Print the ST matrix.
c
      call st_print ( m, n, nst, ist, jst, ast, '  ST matrix:' )
c
c  Get the CC size.
c
      call st_to_cc_size ( nst, ist, jst, ncc )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of CC values = ', ncc
c
c  Create the CC indices.
c
      call st_to_cc_index ( nst, ist, jst, ncc, n, icc, ccc )
c
c  Create the CC values.
c
      call st_to_cc_values ( nst, ist, jst, ast, ncc, n, icc, ccc, acc )

      nrhs = 1
      ldb = n
      do i = 1, n
        b(i) = ( 10.0D+00, 1.0D+00 )
      end do
c
c  Factor the matrix.
c
      iopt = 1
      call c_fortran_zgssv ( iopt, n, ncc, nrhs, acc, icc,
     &  ccc, b, ldb, factors, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'Z_SAMPLE_ST - Fatal error!'
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
        write ( *, '(a)' ) 'Z_SAMPLE_ST - Fatal error!'
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
      write ( *, '(a)' ) 'Z_SAMPLE_ST:'
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
        b(i) = 0.0D+00
      end do

      do j = 1, n
        do k = ccc(j), ccc(j+1) - 1
          i = icc(k)
          b(i) = b(i) + acc(k) * x(j)
        end do
      end do

      return
      end
      subroutine i4vec_copy ( n, a1, a2 )

c*********************************************************************72
c
cc I4VEC_COPY copies an I4VEC.
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
c    02 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, integer A1(N), the vector to be copied.
c
c    Output, integer A2(N), a copy of A1.
c
      implicit none

      integer n

      integer a1(n)
      integer a2(n)
      integer i

      do i = 1, n
        a2(i) = a1(i)
      end do

      return
      end
      subroutine i4vec2_compare ( n, a1, a2, i, j, isgn )

c*********************************************************************72
c
cc I4VEC2_COMPARE compares pairs of integers stored in two vectors.
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
c    Input, integer N, the number of data items.
c
c    Input, integer A1(N), A2(N), contain the two components
c    of each item.
c
c    Input, integer I, J, the items to be compared.
c
c    Output, integer ISGN, the results of the comparison:
c    -1, item I .lt. item J,
c     0, item I = item J,
c    +1, item J .lt. item I.
c
      implicit none

      integer n

      integer a1(n)
      integer a2(n)
      integer i
      integer isgn
      integer j

      isgn = 0

           if ( a1(i) .lt. a1(j) ) then

        isgn = -1

      else if ( a1(i) .eq. a1(j) ) then

             if ( a2(i) .lt. a2(j) ) then
          isgn = -1
        else if ( a2(i) .lt. a2(j) ) then
          isgn = 0
        else if ( a2(j) .lt. a2(i) ) then
          isgn = +1
        end if

      else if ( a1(j) .lt. a1(i) ) then

        isgn = +1

      end if

      return
      end
      subroutine i4vec2_sort_a ( n, a1, a2 )

c*********************************************************************72
c
cc I4VEC2_SORT_A ascending sorts a vector of pairs of integers.
c
c  Discussion:
c
c    Each item to be sorted is a pair of integers (I,J), with the I
c    and J values stored in separate vectors A1 and A2.
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
c    Input, integer N, the number of items of data.
c
c    Input/output, integer A1(N), A2(N), the data to be sorted.
c
      implicit none

      integer n

      integer a1(n)
      integer a2(n)
      integer i
      integer indx
      integer isgn
      integer j
      integer temp

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

          temp  = a1(i)
          a1(i) = a1(j)
          a1(j) = temp

          temp  = a2(i)
          a2(i) = a2(j)
          a2(j) = temp
c
c  Compare the I and J objects.
c
        else if ( indx .lt. 0 ) then

          call i4vec2_compare ( n, a1, a2, i, j, isgn )

        else if ( indx .eq. 0 ) then

          go to 20

        end if

      go to 10

20    continue

      return
      end
      subroutine i4vec2_sorted_unique_count ( n, a1, a2, unique_num )

c*********************************************************************72
c
cc I4VEC2_SORTED_UNIQUE_COUNT counts unique elements in a sorted I4VEC2.
c
c  Discussion:
c
c    Item I is stored as the pair A1(I), A2(I).
c
c    The items must have been sorted, or at least it must be the
c    case that equal items are stored in adjacent vector locations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of items.
c
c    Input, integer A1(N), A2(N), the items.
c
c    Output, integer UNIQUE_NUM, the number of unique items.
c
      implicit none

      integer n

      integer a1(n)
      integer a2(n)
      integer i
      integer iu
      integer unique_num

      unique_num = 0

      if ( n .le. 0 ) then
        return
      end if

      iu = 1
      unique_num = 1

      do i = 2, n

        if ( a1(i) .ne. a1(iu) .or.
     &       a2(i) .ne. a2(iu) ) then
          iu = i
          unique_num = unique_num + 1
        end if

      end do

      return
      end
      subroutine i4vec2_sorted_uniquely ( n1, a1, b1, n2, a2, b2 )

c*********************************************************************72
c
cc I4VEC2_SORTED_UNIQUELY copies unique elements from a sorted I4VEC2.
c
c  Discussion:
c
c    An I4VEC2 is a pair of I4VEC's.
c
c    An I4VEC is a vector of I4's.
c
c    Entry K of an I4VEC2 is the pair of values located
c    at the K-th entries of the two I4VEC's.
c
c    Item I is stored as the pair A1(I), A2(I).
c
c    The items must have been sorted, or at least it must be the
c    case that equal items are stored in adjacent vector locations.
c
c    If the items were not sorted, then this routine will only
c    replace a string of equal values by a single representative.
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
c    Input, integer N1, the number of items.
c
c    Input, integer A1(N1), B1(N1), the array of items.
c
c    Input, integer N2, the number of unique items.
c
c    Output, integer A2(N2), B2(N2), the array of unique items.
c
      implicit none

      integer n1
      integer n2

      integer a1(n1)
      integer a2(n2)
      integer b1(n1)
      integer b2(n2)
      integer i1
      integer i2

      i1 = 1
      i2 = 1
      a2(i2) = a1(i1)
      b2(i2) = b1(i1)

      do i1 = 2, n1

        if ( a1(i1) .ne. a2(i2) .or. b1(i1) .ne. b2(i2) ) then

          i2 = i2 + 1

          a2(i2) = a1(i1)
          b2(i2) = b1(i1)

        end if

      end do

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
c    routine may be used to sort integers, double complexs, numbers, names,
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
c    Input, double complex AST(NST), the ST values.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer nst

      double complex ast(nst)
      integer ist(nst)
      integer jst(nst)
      integer k
      integer m
      integer n
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) '     #     I     J       Ar               Ai'
      write ( *, '(a)' ) 
     &  '  ----  ----  ----  --------------  --------------'
      write ( *, '(a)' ) ' '

      do k = 1, nst
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8,2x,g16.8)' ) 
     &    k, ist(k), jst(k), ast(k)
      end do

      return
      end
      subroutine st_to_cc_index ( nst, ist, jst, ncc, n, icc, ccc )

c*********************************************************************72
c
cc ST_TO_CC_INDEX creates CC indices from ST data.
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
c    Input, integer NST, the number of ST elements.
c
c    Input, integer IST(NST), JST(NST), the ST rows and columns.
c
c    Input, integer NCC, the number of CC elements.
c
c    Input, integer N, the number of columns in the matrix.
c
c    Output, integer ICC(NCC), the CC rows.
c
c    Output, integer CCC(N+1), the compressed CC columns.
c
      implicit none

      integer n
      integer ncc
      integer nst

      integer ccc(n+1)
      integer i
      integer icc(ncc)
      integer ist(nst)
      integer ist2(nst)
      integer j
      integer jhi
      integer jlo
      integer jcc(ncc)
      integer jst(nst)
      integer jst2(nst)
c
c  Make copies so the sorting doesn't confuse the user.
c
      call i4vec_copy ( nst, ist, ist2 )
      call i4vec_copy ( nst, jst, jst2 )
c
c  Sort the elements.
c
      call i4vec2_sort_a ( nst, jst2, ist2 )
c
c  Get the unique elements.
c
      call i4vec2_sorted_uniquely ( nst, jst2, ist2, ncc, jcc, icc )
c
c  Compress the column index.
c
      ccc(1) = 1
      jlo = 1
      do i = 1, ncc
        jhi = jcc(i)
        if ( jhi .ne. jlo ) then
          do j = jlo + 1, jhi
            ccc(j) = i
          end do
          jlo = jhi
        end if
      end do
      jhi = n + 1
      do j = jlo + 1, jhi
        ccc(j) = ncc + 1
      end do

      return
      end
      subroutine st_to_cc_size ( nst, ist, jst, ncc )

c*********************************************************************72
c
cc ST_TO_CC_SIZE sizes CC indexes based on ST data.
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
c    Input, integer NST, the number of ST elements.
c
c    Input, integer IST(NST), JST(NST), the ST rows and columns.
c
c    Output, integer NCC, the number of CC elements.
c
      implicit none

      integer nst

      integer ist(nst)
      integer ist2(nst)
      integer jst2(nst)
      integer jst(nst)
      integer ncc
c
c  Make copies so the sorting doesn't confuse the user.
c
      call i4vec_copy ( nst, ist, ist2 )
      call i4vec_copy ( nst, jst, jst2 )
c
c  Sort by column first, then row.
c
      call i4vec2_sort_a ( nst, jst2, ist2 )
c
c  Count the unique pairs.
c
      call i4vec2_sorted_unique_count ( nst, jst2, ist2, ncc )

      return
      end
      subroutine st_to_cc_values ( nst, ist, jst, ast, ncc, n, icc, 
     &  ccc, acc )

c*********************************************************************72
c
cc ST_TO_CC_VALUES creates CC values from ST data.
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
c    Input, integer NST, the number of ST elements.
c
c    Input, integer IST(NST), JST(NST), the ST rows and columns.
c
c    Input, double complex AST(NST), the ST values.
c
c    Input, integer NCC, the number of CC elements.
c
c    Input, integer N, the number of columns.
c
c    Input, integer ICC(NCC), the CC rows.
c
c    Input, integer CCC(N+1), the CC compressed columns.
c
c    Output, double complex ACC(NCC), the CC values.
c
      implicit none

      integer n
      integer ncc
      integer nst

      double complex ast(nst)
      double complex acc(ncc)
      integer ccc(n+1)
      integer chi
      integer clo
      logical fail
      integer i
      integer icc(ncc)
      integer ist(nst)
      integer j
      integer jst(nst)
      integer kcc
      integer kst

      do kcc = 1, ncc
        acc(kcc) = 0.0D+00
      end do

      do kst = 1, nst

        i = ist(kst)
        j = jst(kst)

        clo = ccc(j)
        chi = ccc(j+1)

        fail = .true.

        do kcc = clo, chi - 1
          if ( icc(kcc) .eq. i ) then
            acc(kcc) = acc(kcc) + ast(kst)
            fail = .false.
            exit
          end if
        end do

        if ( fail ) then
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'ST_TO_CC_VALUES - Fatal error!'
          write ( *, '(a)' ) '  ST entry cannot be located in CC array.'
          write ( *, '(a,i4)' ) '  ST index KST    = ', kst
          write ( *, '(a,i4)' ) '  ST row IST(KST) = ', ist(kst)
          write ( *, '(a,i4)' ) '  ST col JST(KST) = ', jst(kst)
          write ( *, '(a,g14.6)' ) '  ST val AST(KST) = ', ast(kst)
          stop 1
        end if

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
