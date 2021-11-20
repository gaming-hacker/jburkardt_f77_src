      program main

c*********************************************************************72
c
cc MAIN is the main program for ASA109_PRB.
c
c  Discussion:
c
c    ASA109_PRB tests the ASA109 library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ASA109_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the ASA109 library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ASA109_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 demonstrates the use of XINBTA
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision alngam
      double precision b
      double precision beta_log
      double precision fx
      integer ifault
      integer n_data
      double precision x
      double precision x2
      double precision xinbta

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' )
     &  '  XINBTA inverts the incomplete Beta function.'
      write ( *, '(a)' ) '  Compare with tabulated values.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) '      A       B           FX      ',
     &  '    X                         X                       DIFF'
      write ( *, '(a,a)' ) '                                  ',
     &  '   (tabulated)               (XINBTA)'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call beta_inc_values ( n_data, a, b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        beta_log = lgamma ( a )
     &           + lgamma ( b )
     &           - lgamma ( a + b )

        x2 = xinbta ( a, b, beta_log, fx, ifault )

        write ( *,
     &    '(2x,f6.2,2x,f6.2,2x,f14.6,2x,g24.16,2x,g24.16,2x,g10.4)' )
     &    a, b, fx, x, x2, dabs ( x - x2 )

      go to 10

20    continue

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 demonstrates the use of BETA_INC_VALUES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision alngam
      double precision b
      double precision beta_log
      double precision betain
      double precision fx
      double precision fx2
      integer ifault
      integer n_data
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) '  BETA_INC_VALUES returns values of '
      write ( *, '(a)' ) '  the incomplete Beta function.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          A               B               X           CDF'
      write ( *, '(a)' ) ' '

      n_data = 0

10    continue

        call beta_inc_values ( n_data, a, b, x, fx )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        beta_log = lgamma ( a )
     &           + lgamma ( b )
     &           - lgamma ( a + b )

        fx2 = betain ( x, a, b, beta_log, ifault )

        write ( *, 
     &    '(2x,f12.8,2x,f12.8,2x,f14.6,2x,g24.16,2x,g24.16,2x,e10.4)' ) 
     &    a, b, x, fx, fx2, abs ( fx - fx2 )

      go to 10

20    continue

      return
      end
