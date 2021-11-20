      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS511_PRB.
c
c  Discussion:
c
c    TOMS511_PRB tests the TOMS511 library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 January 2016
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS511_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS511 library.'

      call ibess_test ( )
      call jbess_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS511_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine ibess_test ( )

c*********************************************************************72
c
cc IBESS_TEST tests IBESS_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 January 2016
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision alpha
      double precision fx
      integer kode
      integer n
      integer n_data
      double precision x
      double precision y(1)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'IBESS_TEST:'
      write ( *, '(a)' ) '  IBESS returns values of '
      write ( *, '(a)' ) '  the Bessel I function with NONINTEGER'
      write ( *, '(a)' ) '  order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      ALPHA             X                     FX' //
     &  '                        FX'
      write ( *, '(a)' ) 
     &  '                                              exact' //
     &  '                     computed'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_ix_values ( n_data, alpha, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        kode = 1
        n = 1
        call ibess ( kode, alpha, n, x, y )

        write ( *, '(2x,f12.6,2x,f24.16,2x,g24.16,2x,g24.16)' ) 
     &    alpha, x, fx, y(1)

      go to 10

20    continue

      return
      end
      subroutine jbess_test ( )

c*********************************************************************72
c
cc JBESS_TEST tests JBESS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision alpha
      double precision fx
      integer n
      integer n_data
      double precision x
      double precision y(1)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'JBESS_TEST:'
      write ( *, '(a)' ) '  JBESS returns values of '
      write ( *, '(a)' ) '  the Bessel J function with NONINTEGER'
      write ( *, '(a)' ) '  order.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '      ALPHA             X                     FX' //
     &  '                        FX'
      write ( *, '(a)' ) 
     &  '                                              exact' //
     &  '                     computed'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call bessel_jx_values ( n_data, alpha, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        n = 1
        call jbess ( alpha, n, x, y )

        write ( *, '(2x,f12.6,2x,f24.16,2x,g24.16,2x,g24.16)' ) 
     &    alpha, x, fx, y(1)

      go to 10

20    continue

      return
      end
