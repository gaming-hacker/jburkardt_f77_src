      program main

c*********************************************************************72
c
cc MAIN is the main program for CUBE_FELIPPA_RULE_PRB.
c
c  Discussion:
c
c    CUBE_FELIPPA_RULE_PRB tests the CUBE_FELIPPA_RULE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer degree_max

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CUBE_FELIPPA_RULE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CUBE_FELIPPA_RULE library.'

      degree_max = 4
      call cube_monomial_test ( degree_max )

      degree_max = 6
      call cube_quad_test ( degree_max )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CUBE_FELIPPA_RULE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end






