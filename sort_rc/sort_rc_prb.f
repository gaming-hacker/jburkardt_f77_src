      program main

c*********************************************************************72
c
cc MAIN is the main program for SORT_RC_PRB.
c
c  Discussion:
c
c    SORT_RC_PRB tests the SORT_RC library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SORT_RC_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SORT_RC library.'

      call sort_rc_test ( )
      call sort_safe_rc_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SORT_RC_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine sort_rc_test ( )

c*********************************************************************72
c
cc SORT_RC_TEST tests SORT_RC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 October 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer a(n)
      integer i
      integer i4_hi
      integer i4_lo
      integer indx
      integer isgn
      integer j
      integer k
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SORT_RC_TEST'
      write ( *, '(a)' ) '  SORT_RC sorts objects externally.'
      write ( *, '(a)' ) '  This function relies on the use of'
      write ( *, '(a)' ) '  persistent data stored internally.'
c
c  Generate some data to sort.
c
      i4_lo = 1
      i4_hi = n
      seed = 123456789

      call i4vec_uniform_ab ( n, i4_lo, i4_hi, seed, a )

      call i4vec_print ( n, a, '  Unsorted array:' )
c
c  Sort the data.
c
      indx = 0

10    continue

        call sort_rc ( n, indx, i, j, isgn )

        if ( indx .lt. 0 ) then
          isgn = 1
          if ( a(i) .le. a(j) ) then
            isgn = -1
          end if
        else if ( 0 .lt. indx ) then
          k    = a(i)
          a(i) = a(j)
          a(j) = k
        else
          go to 20
        end if

      go to 10
c
c  Display the sorted data.
c
20    continue

      call i4vec_print ( n, a, '  Sorted array:' )

      return
      end
      subroutine sort_safe_rc_test ( )

c*********************************************************************72
c
cc SORT_SAFE_RC_TEST tests SORT_SAFE_RC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      integer a(n)
      integer i
      integer i_save
      integer i4_hi
      integer i4_lo
      integer indx
      integer isgn
      integer j
      integer j_save
      integer k
      integer k_save
      integer l_save
      integer n_save
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SORT_SAFE_RC_TEST'
      write ( *, '(a)' ) '  SORT_SAFE_RC sorts objects externally.'
      write ( *, '(a)' ) '  This version of the algorithm does not' 
      write ( *, '(a)' ) '  rely on internally saved or "persistent"'
      write ( *, '(a)' ) '  or "static" memory.'
c
c  Generate some data to sort.
c
      i4_lo = 1
      i4_hi = n
      seed = 123456789

      call i4vec_uniform_ab ( n, i4_lo, i4_hi, seed, a )

      call i4vec_print ( n, a, '  Unsorted array:' )
c
c  Sort the data.
c
      indx = 0

10    continue

        call sort_safe_rc ( n, indx, i, j, isgn, i_save, j_save, 
     &    k_save, l_save, n_save )

        if ( indx .lt. 0 ) then
          isgn = 1
          if ( a(i) .le. a(j) ) then
            isgn = -1
          end if
        else if ( 0 .lt. indx ) then
          k    = a(i)
          a(i) = a(j)
          a(j) = k
        else
          go to 20
        end if

      go to 10
c
c  Display the sorted data.
c
20    continue

      call i4vec_print ( n, a, '  Sorted array:' )

      return
      end
