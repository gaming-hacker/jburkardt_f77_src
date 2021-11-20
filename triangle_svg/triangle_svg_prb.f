      program main

c*********************************************************************72
c
cc MAIN is the main program for TRIANGLE_SVG_PRB.
c
c  Discussion:
c
c    TRIANGLE_SVG_PRB tests the TRIANGLE_SVG library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE_SVG_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TRIANGLE_SVG library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE_SVG_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 calls TRIANGLE_SVG to plot a triangle and some points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer p_num
      parameter ( p_num = 13 )

      double precision a
      double precision b
      double precision c
      double precision d
      double precision e
      double precision f
      double precision g
      double precision h
      double precision p(2,p_num)
      character * ( 255 ) plot_filename
      double precision t(2,3)

      save p
      save t

      data p /
     &  0.333333333333333D+00, 0.333333333333333D+00,
     &  0.479308067841923D+00, 0.260345966079038D+00,
     &  0.260345966079038D+00, 0.479308067841923D+00,
     &  0.260345966079038D+00, 0.260345966079038D+00,
     &  0.869739794195568D+00, 0.065130102902216D+00,
     &  0.065130102902216D+00, 0.869739794195568D+00,
     &  0.065130102902216D+00, 0.065130102902216D+00,
     &  0.638444188569809D+00, 0.312865496004875D+00,
     &  0.638444188569809D+00, 0.048690315425316D+00,
     &  0.312865496004875D+00, 0.638444188569809D+00,
     &  0.312865496004875D+00, 0.048690315425316D+00,
     &  0.048690315425316D+00, 0.638444188569809D+00,
     &  0.048690315425316D+00, 0.312865496004875D+00 /
      data t /
     &  0.0, 0.0, 
     &  1.0, 0.0, 
     &  0.0, 1.0 /

      plot_filename = 'test01.svg'

      call triangle_svg ( plot_filename, t, p_num, p )

      return
      end
