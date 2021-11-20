      program main

c*****************************************************************************80
c
c  Purpose:
c
c    MAIN is the main program for UMFPACK_SIMPLE.
c
c  Discussion:
c
c    This program uses UMFPACK to solve the 5x5 linear system A*X=B:
c
c        2  3  0  0  0        1.0         8.0
c        3  0  4  0  6        2.0        45.0
c    A = 0 -1 -3  2  0    X = 3.0    B = -3.0
c        0  0  1  0  0        4.0         3.0
c        0  4  2  0  1        5.0        10.0
c
c    The matrix contains 12 nonzero values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 January 2014
c
c  Reference:
c
c    Timothy Davis,
c    UMFPACK User Guide,
c    Version 5.6.2, 25 April 2013
c    http://suitesparse.com
c
      implicit none
 
      integer n
      parameter ( n = 5 )
      integer nnz
      parameter ( nnz = 12 )

      integer ai(nnz)
      integer ap(n+1)
      double precision ax(nnz)
      double precision b(n)
      double precision control(20)
      integer filenum
      integer i
      double precision info(90)
      integer*8 numeric
      integer status
      integer*8 symbolic
      integer sys
      double precision x(n)

      save ai
      save ap
      save ax
      save b

      data ai /
     &  0, 1, 
     &  0, 2, 4, 
     &  1, 2, 3, 4, 
     &  2, 
     &  1, 4 /
      data ap /
     &  0, 2, 5, 9, 10, 12 /
      data ax /
     &  2.0,  3.0, 
     &  3.0, -1.0, 4.0, 
     &  4.0, -3.0, 1.0, 2.0, 
     &  2.0, 
     &  6.0, 1.0 /
      data b /
     &  8.0, 45.0, -3.0, 3.0, 19.0 /

      call timestamp ( );
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'UMFPACK_SIMPLE:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) 
     &  '  Use UMFPACK for the sparse linear system A*x=b.'
c
c  Set the default control parameters.
c
      call umf4def ( control )
c
c  Carry out the symbolic factorization.
c
      call umf4sym ( n, n, ap, ai, ax, symbolic, control, info )

      if ( info(1) .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'UMFPACK_SIMPLE - Fatal error!'
        write ( *, '(a,g14.6)' ) '  UMF4SYM returns INFO(1) = ', info(1)
        stop 1
      end if
c
c  Use the symbolic factorization to carry out the numeric factorization.
c
      call umf4num ( ap, ai, ax, symbolic, numeric, control, info )

      if ( info(1) .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'UMFPACK_SIMPLE - Fatal error!'
        write ( *, '(a,g14.6)' ) '  UMF4NUM returns INFO(1) = ', info(1)
        stop 1
      end if
c
c  Free the memory associated with the symbolic factorization.
c
      call umf4fsym ( symbolic )
c
c  Solve the linear system.
c
      sys = 0
      call umf4sol ( sys, x, b, numeric, control, info )

      if ( info(1) .lt. 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'UMFPACK_SIMPLE - Fatal error!'
        write ( *, '(a,g14.6)' ) '  UMF4SOL returns INFO(1) = ', info(1)
        stop 1
      end if
c
c  Free the memory associated with the numeric factorization.
c
      call umf4fnum ( numeric )
c
c  Print the solution.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Computed solution:'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,g14.6)' ) x(i)
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'UMFPACK_SIMPLE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
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
