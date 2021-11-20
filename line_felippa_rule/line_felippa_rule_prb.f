      program main

c*********************************************************************72
c
cc MAIN is the main program for LINE_FELIPPA_RULE_PRB.
c
c  Discussion:
c
c    LINE_FELIPPA_RULE_PRB tests the LINE_FELIPPA_RULE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer degree_max

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_FELIPPA_RULE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the LINE_FELIPPA_RULE library.'

      degree_max = 4
      call line_monomial_test ( degree_max )

      degree_max = 11
      call line_quad_test ( degree_max )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_FELIPPA_RULE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
