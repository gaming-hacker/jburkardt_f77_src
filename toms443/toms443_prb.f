      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS443_PRB.
c
c  Discussion:
c
c    TOMS443_PRB tests the TOMS443 library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS443_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS443 library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS433_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests WEW_A
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision en
      integer n_data
      double precision w1
      double precision w2
      double precision wew_a
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Test WEW_A to evaluate'
      write ( *, '(a)' ) '  Lambert''s W function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          X          Exact            Computed          Error'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

      call lambert_w_values ( n_data, x, w1 )

      if ( n_data <= 0 ) then
        go to 20
      end if

      if ( x == 0.0D+00 ) then
        w2 = 0.0D+00
      else
        w2 = wew_a ( x, en )
      end if

      write ( *, '(2x,f12.4,2x,g16.8,2x,g16.8,2x,e10.2)' )
     &  x, w1, w2, abs ( w1 - w2 )

      go to 10

20    continue

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests WEW_B
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision en
      integer n_data
      double precision w1
      double precision w2
      double precision wew_b
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Test WEW_B to evaluate'
      write ( *, '(a)' ) '  Lambert''s W function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          X          Exact            Computed          Error'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

      call lambert_w_values ( n_data, x, w1 )

      if ( n_data .le. 0 ) then
        go to 20
      end if

      if ( x .eq. 0.0D+00 ) then
        w2 = 0.0D+00
      else
        w2 = wew_b ( x, en )
      end if

      write ( *, '(2x,f12.4,2x,g16.8,2x,g16.8,2x,e10.2)' )
     &  x, w1, w2, abs ( w1 - w2 )

      go to 10

20    continue

      return
      end

