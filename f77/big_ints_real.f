      program main

c*********************************************************************72
c
cc MAIN is the main program for BIG_INTS_REAL.
c
c  Discussion:
c
c    It's easy to forget that COMPUTER NUMBERS are not MATHEMATICAL NUMBERS.
c
c    There is a biggest real number.
c
c    There is a biggest integer.
c
c    There is a biggest integer that you can store as a real number.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BIG_INTS_REAL:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Storing big integers in real variables.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BIG_INTS_REAL:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*****************************************************************************80
c
cc TEST01 stores huge integers as reals.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i4
      integer i4r4i4
      integer i4r8i4
      integer * 8 i8
      integer * 8 i8r4i8
      integer * 8 i8r8i8
      real r4
      real r4i4
      real r4i8
      double precision r8
      double precision r8i4
      double precision r8i8

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Compute the largest possible integers.'
      write ( *, '(a)' ) '  Try to store them as real values.'
      write ( *, '(a)' ) '  Then copy them back.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  "Huge" integers and huge reals:'
      write ( *, '(a)' ) ''

      i4 = huge ( i4 )
      i8 = huge ( i8 )
      r4 = huge ( r4 )
      r8 = huge ( r8 )

      write ( *, '(a,i26)'   ) 
     &  '  i4 = huge ( integer ) = ', i4
      write ( *, '(a,i26)'   ) 
     &  '  i8 = huge ( integer * 8 ) = ', i8
      write ( *, '(a,g26.6)' ) 
     &  '  r4 = huge ( real ) =    ', r4
      write ( *, '(a,g26.6)' ) 
     &  '  r8 = huge ( double precision ) =    ', r8

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Convert huge integers to real values:'
      write ( *, '(a)' ) ''

      r4i4 = real ( i4 )
      r4i8 = real ( i8 )
      r8i4 = dble ( i4 )
      r8i8 = dble ( i8 )

      write ( *, '(a,g26.6)' ) 
     &  '  r4i4 = real ( i4 ) =     ', r4i4
      write ( *, '(a,g26.6)' ) 
     &  '  r4i8 = real ( i8 ) =     ', r4i8
      write ( *, '(a,g26.6)' ) 
     &  '  r8i4 = dble ( i4 ) =     ', r8i4
      write ( *, '(a,g26.6)' ) 
     &  '  r8i8 = dble ( i8 ) =     ', r8i8

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Convert real values of integers back to integers:'
      write ( *, '(a)' ) ''

      i4r4i4 = int ( r4i4 )
      i4r8i4 = int ( r8i4 )
      i8r4i8 = int8 ( r4i8 )
      i8r8i8 = int8 ( r8i8 )

      write ( *, '(a,i26)' )
     &  '  i4r4i4 = int ( r4i4 ) =  ', i4r4i4
      write ( *, '(a,i26)' )
     &  '  i4r8i4 = int ( r8i4 ) =  ', i4r8i4
      write ( *, '(a,i26)' )
     &  '  i8r4i8 = int8 ( r4i8 ) =  ', i8r4i8
      write ( *, '(a,i26)' )
     &  '  i8r8i8 = int8 ( r8i8 ) =  ', i8r8i8

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
c    12 June 2014
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
     &  d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, 
     &  trim ( ampm )

      return
      end

