      program main

c*********************************************************************72
c
cc TEST_NEAREST compares the performance of nearest neighbor routines.
c
c  Discussion:
c
c    We are given R, a set of NR points in M dimensions.
c
c    We are given S, a set of NS points in M dimensions.
c
c    For each S(I) in S, we seek the index J of the point R(J)
c    which is nearest to S(I) over all points in R.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 December 2012
c
c  Author:
c
c    John Burkardt
c
c  Local parameters:
c
c    Local, integer M, the spatial dimension.
c
c    Local, integer NR, the number of data points.
c
c    Local, integer NS, the number of sample points.
c
c    Local, double precision R(M,NR), the data points.
c
c    Local, double precision RT(NR,M), the transposed data points.
c
c    Local, double precision S(M,NS), the sample points. 
c
c    Local, double precision ST(NS,M), the transposed sample points. 
c
      implicit none

      integer m_test_num
      parameter ( m_test_num = 3 )
      integer n_test_num
      parameter ( n_test_num = 6 )

      integer m
      integer m_test
      integer m_test_data(m_test_num)
      integer nearest(1000000)
      integer nr
      integer nr_test_data(n_test_num)
      integer ns
      integer ns_test_data(n_test_num)
      double precision r(8000000)
      double precision s(8000000)
      integer seed
      double precision t1
      double precision t2
      integer test

      save m_test_data
      save nr_test_data
      save ns_test_data

      data m_test_data / 2, 4, 8 /
      data nr_test_data / 
     &  1000000, 100000, 10000,  1000,    100,      10 /
      data ns_test_data /
     &    10,    100,  1000, 10000, 100000, 1000000 /

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST_NEAREST:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Consider various nearest neighbor algorithms.'

      do m_test = 1, m_test_num

        m = m_test_data(m_test);

        do test = 1, n_test_num

          nr = nr_test_data(test)
          ns = ns_test_data(test)

          seed = 123456789
          call r8mat_uniform_01 ( m, ns, seed, s )
          call r8mat_uniform_01 ( m, nr, seed, r )

          write ( *, '(a)' ) ''
          write ( *, '(a,i8,a,i8,a,i8)' ) 
     &      '  M = ', m, ' NR = ', nr, '  NS = ', ns

          call cpu_time ( t1 )
          call find_closest1 ( m, nr, r, ns, s, nearest )
          call cpu_time ( t2 )
          write ( *, '(a,g14.6,a,i8,a,i8)' ) 
     &      '  #1 time: ', t2 - t1, '  size = ', ns, 
     &      '  i(1) = ', nearest(1)

        end do

      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST_NEAREST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine find_closest1 ( m, nr, r, ns, s, nearest )

c*********************************************************************72
c
cc FIND_CLOSEST1 finds the nearest R point to each S point.
c
c  Discussion:
c
c    We are given R, a set of NR points in M dimensions.
c
c    We are given S, a set of NS points in M dimensions.
c
c    For each S(I) in S, we seek the index J of the point R(J)
c    which is nearest to S(I) over all points in R.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 December 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer NR, the number of data points.
c
c    Input, double precision R(M,NR), the data points.
c
c    Input, integer NS, the number of sample points.
c
c    Input, double precision S(M,NS), the sample points.
c
c    Output, integer NEAREST(NS), the index of the nearest data point.
c
      implicit none

      integer m
      integer nr
      integer ns

      double precision dist_sq
      double precision dist_sq_min
      integer i
      integer jr
      integer js
      integer nearest(ns)
      double precision r(m,nr)
      double precision r8_huge
      double precision s(m,ns)

      do js = 1, ns

        dist_sq_min = r8_huge ( )
        nearest(js) = -1

        do jr = 1, nr

          dist_sq = 0.0D+00
          do i = 1, m
            dist_sq = dist_sq + ( r(i,jr) - s(i,js) ) ** 2
          end do

          if ( dist_sq .lt. dist_sq_min ) then
            dist_sq_min = dist_sq
            nearest(js) = jr
          end if

        end do

      end do

      return
      end
      function r8_huge ( )

c*********************************************************************72
c
cc R8_HUGE returns a "huge" R8.
c
c  Discussion:
c
c    The value returned by this function is NOT required to be the
c    maximum representable R8.  This value varies from machine to machine,
c    from compiler to compiler, and may cause problems when being printed.
c    We simply want a "very large" but non-infinite number.
c
c    FORTRAN90 provides a built-in routine HUGE ( X ) that
c    can return the maximum representable number of the same datatype
c    as X, if that is what is really desired.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_HUGE, a huge number.
c
      implicit none

      double precision r8_huge

      r8_huge = 1.0D+30

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
      integer j
      integer k
      integer seed
      double precision r(m,n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop
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
