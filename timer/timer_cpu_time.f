      program main

c*********************************************************************72
c
cc MAIN is the main program for TIMER_CPU_TIME.
c
c  Discussion:
c
c    TIMER_CPU_TIME uses CPU_TIME as the timer.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TIMER_CPU_TIME'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Attempt to use of the CPU_TIME timer.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  CPU_TIME is a FORTRAN 95 built in routine'
      write ( *, '(a)' ) '  which might be available to a F77 program.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    call cpu_time ( reading )'

      call test03 ( )
      call test04 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TIMER_CPU_TIME'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 times the unvectorized EXP routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_log_min
      parameter ( n_log_min = 12 )
      integer n_log_max
      parameter ( n_log_max = 22 )
      integer n_min
      parameter ( n_min = 2**n_log_min )
      integer n_max
      parameter ( n_max = 2**n_log_max )
      integer n_rep
      parameter ( n_rep = 5 )

      double precision delta(n_log_max,n_rep)
      integer func
      integer i
      integer i_rep
      integer n
      integer n_log
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      integer seed
      double precision time1
      double precision time2
      double precision x(n_max)
      double precision y(n_max)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Time the unvectorized loops:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    do i = 1, n'
      write ( *, '(a)' ) '      y(i) =        x(i)  '
      write ( *, '(a)' ) '      y(i) = PI *   x(i)  '
      write ( *, '(a)' ) '      y(i) = sqrt ( x(i) )'
      write ( *, '(a)' ) '      y(i) = exp  ( x(i) )'
      write ( *, '(a)' ) '    end do'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) 
     &  '  Data vectors will be of minimum size ', n_min
      write ( *, '(a,i12)' ) 
     &  '  Data vectors will be of maximum size ', n_max
      write ( *, '(a,i12)' ) 
     &  '  Number of repetitions of the operation: ', n_rep

      do func = 1, 4

        do i_rep = 1, n_rep

          do n_log = n_log_min, n_log_max

            n = 2**( n_log )

            call r8vec_uniform_01 ( n, seed, x )

            call cpu_time ( time1 )

            if ( func .eq. 1 ) then
              do i = 1, n
                y(i) = x(i)
              end do
            else if ( func .eq. 2 ) then
              do i = 1, n
                y(i) = pi * x(i)
              end do
            else if ( func .eq. 3 ) then
              do i = 1, n
                y(i) = sqrt ( x(i) )
              end do
            else if ( func .eq. 4 ) then
              do i = 1, n
                y(i) = exp ( x(i) )
              end do
            end if

            call cpu_time ( time2 )

            delta(n_log,i_rep) = time2 - time1

          end do

        end do

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Timing Results:'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '    Vector Size  Rep #1        Rep #2        '
     &    //  'Rep #3        Rep #4        Rep #5'
        write ( *, '(a)' ) ' '
        do n_log = n_log_min, n_log_max
          n = 2**( n_log )
          write ( *, '(i10,5f14.6)' ) n, delta(n_log,1:n_rep)
        end do

      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 times the 2D nearest neighbor problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_log_min
      parameter ( n_log_min = 10 )
      integer n_log_max
      parameter ( n_log_max = 20 )
      integer n_min
      parameter ( n_min = 2**n_log_min )
      integer n_max
      parameter ( n_max = 2**n_log_max )
      integer n_rep
      parameter ( n_rep = 5 )

      double precision delta(n_log_max,n_rep)
      double precision dist_i
      double precision dist_min
      integer i
      integer i_min
      integer i_rep
      integer n
      integer n_log
      double precision r8_huge
      integer seed
      double precision time1
      double precision time2
      double precision x(2,n_max)
      double precision y(2)

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  Time the 2D nearest neighbor problem.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Given X(2,N) and Y(2),'
      write ( *, '(a)' ) '    find X(2,*) closest to Y(2).'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    do i = 1, n'
      write ( *, '(a)' ) 
     &  '      if distance ( x(2,i), y ) < minimum so far'
      write ( *, '(a)' ) '        x_min = x(2,i)'
      write ( *, '(a)' ) '    end do'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) 
     &  '  Data vectors will be of minimum size ', n_min
      write ( *, '(a,i12)' ) 
     &  '  Data vectors will be of maximum size ', n_max
      write ( *, '(a,i12)' ) 
     &  '  Number of repetitions of the operation: ', n_rep

      call r8vec_uniform_01 ( 2 * n_max, seed, x )
      call r8vec_uniform_01 ( 2, seed, y )

      do i_rep = 1, n_rep

        do n_log = n_log_min, n_log_max

          n = 2**( n_log )

          call cpu_time ( time1 )

          dist_min = r8_huge ( )
          i_min = 0
          do i = 1, n
            dist_i = ( x(1,i) - y(1) )**2 + ( x(2,i) - y(2) )**2
            if ( dist_i .lt. dist_min ) then
              dist_min = dist_i
              i_min = i
            end if
          end do

          call cpu_time ( time2 )

          delta(n_log,i_rep) = time2 - time1

        end do

      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Timing Results:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '    Vector Size  Rep #1        Rep #2        Rep #3        ' 
     &  // 'Rep #4        Rep #5'
      write ( *, '(a)' ) ' '
      do n_log = n_log_min, n_log_max
        n = 2**( n_log )
        write ( *, '(i10,5f14.6)' ) n, delta(n_log,1:n_rep)
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
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

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
