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
      subroutine i4vec_uniform_ab ( n, a, b, seed, x )

c*********************************************************************72
c
cc I4VEC_UNIFORM_AB returns a scaled pseudorandom I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    The pseudorandom numbers should be uniformly distributed
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
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c
c    Input, integer A, B, the limits of the interval.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, integer X(N), a vector of numbers between A and B.
c
      implicit none

      integer n

      integer a
      integer b
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      real r
      integer seed
      integer value
      integer x(n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4VEC_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do i = 1, n

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
     &    +             r   * ( real ( max ( a, b ) ) + 0.5E+00 )
c
c  Use rounding to convert R to an integer between A and B.
c
        value = nint ( r )

        value = max ( value, min ( a, b ) )
        value = min ( value, max ( a, b ) )

        x(i) = value

      end do

      return
      end
      subroutine sort_rc ( n, indx, i, j, isgn )

c*********************************************************************72
c
cc SORT_RC externally sorts a list of items into ascending order.
c
c  Discussion:
c
c    The actual list of data is not passed to the routine.  Hence this
c    routine may be used to sort integers, reals, numbers, names,
c    dates, shoe sizes, and so on.  After each call, the routine asks
c    the user to compare or interchange two items, until a special
c    return value signals that the sorting is completed.
c
c    Note that this function uses internal persistent memory during the sort.
c
c  Example:
c
c    n = 100
c    indx = 0
c    i = 0
c    j = 0
c    isgn = 0
c
c    do
c
c      call sort_rc ( n, indx, i, j, isgn )
c
c      if ( indx .lt. 0 ) then
c
c        isgn = 1
c        if ( a(i) .le. a(j) ) then
c          isgn = -1
c        end if
c
c      else if ( 0 .lt. indx ) then
c
c        k    = a(i)
c        a(i) = a(j)
c        a(j) = k
c
c      else
c
c        exit
c
c      end if
c
c    end do
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 February 2004
c
c  Author:
c
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    FORTRAN90 version by John Burkardt.
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
c    The user must set INDX to 0 before the first call.
c    Thereafter, the user should not change the value of INDX until
c    the sorting is done.
c    On return, if INDX is
c    * greater than 0,
c      > interchange items I and J;
c      > call again.
c    * less than 0,
c      > compare items I and J;
c      > set ISGN = -1 if I .lt. J, ISGN = +1 if J .lt. I;
c      > call again.
c    * equal to 0, the sorting is done.
c
c    Output, integer I, J, the indices of two items.
c    On return with INDX positive, elements I and J should be interchanged.
c    On return with INDX negative, elements I and J should be compared, and
c    the result reported in ISGN on the next call.
c
c    Input, integer ISGN, results of comparison of elements
c    I and J. (Used only when the previous call returned INDX less than 0).
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
      integer k_save
      integer l_save
      integer n
      integer n_save

      save i_save
      save j_save
      save k_save
      save l_save
      save n_save

      data i_save / 0 /
      data j_save / 0 /
      data k_save / 0 /
      data l_save / 0 /
      data n_save / 0 /
c
c  INDX = 0: This is the first call.
c
      if ( indx .eq. 0 ) then

        i_save = 0
        j_save = 0
        k_save = n / 2
        l_save = n / 2
        n_save = n
c
c  INDX .lt. 0: The user is returning the results of a comparison.
c
      else if ( indx .lt. 0 ) then

        if ( indx .eq. -2 ) then

          if ( isgn .lt. 0 ) then
            i_save = i_save + 1
          end if

          j_save = l_save
          l_save = i_save
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

        if ( k_save .le. 1 ) then

          if ( n_save .eq. 1 ) then
            i_save = 0
            j_save = 0
            indx = 0
          else
            i_save = n_save
            n_save = n_save - 1
            j_save = 1
            indx = 1
          end if

          i = i_save
          j = j_save
          return

        end if

        k_save = k_save - 1
        l_save = k_save
c
c  0 .lt. INDX, the user was asked to make an interchange.
c
      else if ( indx .eq. 1 ) then

        l_save = k_save

      end if

10    continue

        i_save = 2 * l_save

        if ( i_save .eq. n_save ) then
          j_save = l_save
          l_save = i_save
          indx = -1
          i = i_save
          j = j_save
          return
        else if ( i_save .le. n_save ) then
          j_save = i_save + 1
          indx = -2
          i = i_save
          j = j_save
          return
        end if

        if ( k_save .le. 1 ) then
          go to 20
        end if

        k_save = k_save - 1
        l_save = k_save

      go to 10

20    continue

      if ( n_save .eq. 1 ) then
        i_save = 0
        j_save = 0
        indx = 0
        i = i_save
        j = j_save
      else
        i_save = n_save
        n_save = n_save - 1
        j_save = 1
        indx = 1
        i = i_save
        j = j_save
      end if

      return
      end
      subroutine sort_safe_rc ( n, indx, i, j, isgn, i_save, j_save,
     &  k_save, l_save, n_save )

c*********************************************************************72
c
cc SORT_SAFE_RC externally ascending sorts a list of items.
c
c  Discussion:
c
c    This is a version of SORT_RC which does not rely on
c    storing certain work variables internally to the function.  This makes
c    the function somewhat more awkward to call, but easier to program
c    in a variety of languages, and safe to use in a parallel programming
c    environment, or in cases where the sorting of several vectors is to
c    be carried out at more or less the same time.
c
c    The actual list of data is not passed to the routine.  Hence this
c    routine may be used to sort integers, reals, numbers, names,
c    dates, shoe sizes, and so on.  After each call, the routine asks
c    the user to compare or interchange two items, until a special
c    return value signals that the sorting is completed.
c
c  Example:
c
c    n = 100
c    indx = 0
c
c    do
c
c      call sort_safe_rc ( n, indx, i, j, isgn, i_save, j_save,
c        k_save, l_save, n_save )
c
c      if ( indx .lt. 0 ) then
c
c        isgn = 1
c        if ( a(i) .le. a(j) ) then
c          isgn = -1
c        end if
c
c      else if ( 0 .lt. indx ) then
c
c        k    = a(i)
c        a(i) = a(j)
c        a(j) = k
c
c      else
c
c        exit
c
c      end if
c
c    end do
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
c    John Burkardt.
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
c    The user must set INDX to 0 before the first call.
c    Thereafter, the user should not change the value of INDX until
c    the sorting is done.
c    On return, if INDX is
c    * greater than 0,
c      > interchange items I and J;
c      > call again.
c    * less than 0,
c      > compare items I and J;
c      > set ISGN = -1 if I .lt. J, ISGN = +1 if J .lt. I;
c      > call again.
c    * equal to 0, the sorting is done.
c
c    Output, integer I, J, the indices of two items.
c    On return with INDX positive, elements I and J should be interchanged.
c    On return with INDX negative, elements I and J should be compared, and
c    the result reported in ISGN on the next call.
c
c    Input, integer ISGN, results of comparison of elements
c    I and J. (Used only when the previous call returned INDX less than 0).
c    ISGN .le. 0 means I is less than or equal to J;
c    0 .le. ISGN means I is greater than or equal to J.
c
c    Input/output, integer I_SAVE, J_SAVE, K_SAVE, L_SAVE,
c    N_SAVE, workspace needed by the routine.  Before calling the function,
c    the user should declare variables to hold these values, but should
c    not change them, and need not ever examine them.
c
      implicit none

      integer i
      integer i_save
      integer indx
      integer isgn
      integer j
      integer j_save
      integer k_save
      integer l_save
      integer n
      integer n_save
c
c  INDX = 0: This is the first call.
c
      if ( indx .eq. 0 ) then

        i_save = 0
        j_save = 0
        k_save = n / 2
        l_save = n / 2
        n_save = n
c
c  INDX .lt. 0: The user is returning the results of a comparison.
c
      else if ( indx .lt. 0 ) then

        if ( indx .eq. -2 ) then

          if ( isgn .lt. 0 ) then
            i_save = i_save + 1
          end if

          j_save = l_save
          l_save = i_save
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

        if ( k_save .le. 1 ) then

          if ( n_save .eq. 1 ) then
            i_save = 0
            j_save = 0
            indx = 0
          else
            i_save = n_save
            n_save = n_save - 1
            j_save = 1
            indx = 1
          end if

          i = i_save
          j = j_save
          return

        end if

        k_save = k_save - 1
        l_save = k_save
c
c  0 .lt. INDX, the user was asked to make an interchange.
c
      else if ( indx .eq. 1 ) then

        l_save = k_save

      end if

10    continue

        i_save = 2 * l_save

        if ( i_save .eq. n_save ) then
          j_save = l_save
          l_save = i_save
          indx = -1
          i = i_save
          j = j_save
          return
        else if ( i_save .le. n_save ) then
          j_save = i_save + 1
          indx = -2
          i = i_save
          j = j_save
          return
        end if

        if ( k_save .le. 1 ) then
          go to 20
        end if

        k_save = k_save - 1
        l_save = k_save

      go to 10

20    continue

      if ( n_save .eq. 1 ) then
        i_save = 0
        j_save = 0
        indx = 0
        i = i_save
        j = j_save
      else
        i_save = n_save
        n_save = n_save - 1
        j_save = 1
        indx = 1
        i = i_save
        j = j_save
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
