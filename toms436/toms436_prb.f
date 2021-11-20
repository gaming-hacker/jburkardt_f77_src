      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS436_PRB.
c
c  Discussion:
c
c    TOMS436_PRB tests the TOMS436 library.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS436_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS436 library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS436_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests the rule with one factor equal to 1.
c
      implicit none

      double precision a
      double precision b
      double precision exact
      external fn00
      external gn00
      integer n
      double precision vint

      a = -4.0D+00
      b =  4.0D+00
      exact = 2.0D+00 * atan ( 4.0D+00 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Integral of F(X) * G(X) from -1 to 1,'
      write ( *, '(a)' ) '  with F(X) = 1, G(X) = 1/(1+x**2 )'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    VINT'
      write ( *, '(a)' ) ' '

      do n = 1, 10
        call ptrap ( a, b, n, fn00, gn00, vint )
        write ( *, '(2x,i6,2x,g14.6)' ) n, vint
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Exact:  ', exact

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests the rule with factors exp(-x) and cos(x).
c
      implicit none

      double precision a
      double precision b
      double precision exact
      external gn01
      external gn02
      integer n
      double precision vint

      a =  0.0D+00
      b =  3.141592653589793D+00

      exact = 0.5 * exp ( -b ) * ( sin ( b ) - cos ( b ) )
     &      - ( 0.5 * exp ( -a ) * ( sin ( a ) - cos ( a ) ) )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Integral of F(X) * G(X) from 0 to PI,'
      write ( *, '(a)' ) '  with F(X) = EXP(-X), G(X) = COS(X)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       N    VINT'
      write ( *, '(a)' ) ' '

      do n = 1, 10
        call ptrap ( a, b, n, gn01, gn02, vint )
        write ( *, '(2x,i6,2x,g14.6)' ) n, vint
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Exact:  ', exact

      return
      end
      function fn00 ( x )

c*********************************************************************72
c
cc FN00 evaluates the function 1.
c
      implicit none

      integer n

      double precision fn00
      double precision x

      fn00 = 1.0D+00

      return
      end
      function fn01 ( x )

c*********************************************************************72
c
cc FN01 evaluates the function X.
c
      implicit none

      integer n

      double precision fn01
      double precision x

      fn01 = x

      return
      end
      function gn00 ( x )

c*********************************************************************72
c
cc GN00 evaluates the function 1/(1+X**2).
c
      implicit none

      integer n

      double precision gn00
      double precision x

      gn00 = 1.0D+00 / ( 1.0D+00 + x * x )

      return
      end
      function gn01 ( x )

c*********************************************************************72
c
cc GN01 evaluates the function exp(-x).
c
      implicit none

      integer n

      double precision gn01
      double precision x

      gn01 = exp ( - x )

      return
      end
      function gn02 ( x )

c*********************************************************************72
c
cc GN02 evaluates the function cos(x).
c
      implicit none

      integer n

      double precision gn02
      double precision x

      gn02 = cos ( x )

      return
      end

