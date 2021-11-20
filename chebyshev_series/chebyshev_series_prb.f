      program main

c*********************************************************************72
c
cc MAIN is the main program for CHEBYSHEV_SERIES_PRB.
c
c  Discussion:
c
c    CHEBYSHEV_SERIES_PRB tests the CHEBYSHEV_SERIES library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CHEBYSHEV_SERIES_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CHEBYSHEV_SERIES libary.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CHEBYSHEV_SERIES_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 considers an even Chebyshev series for EXP(X).
c
c  Discussion:
c
c    Table 5 is from Clenshaw, and contains 18 terms of the Chebyshev
c    series for exp(x) over [-1,+1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
      implicit none

      integer nc
      parameter ( nc = 18 )

      integer i
      double precision s
      double precision s1
      double precision s2
      double precision s3
      double precision table5(nc)
      double precision x
      double precision y

      save table5

      data table5 /
     &  2.53213175550401667120D+00, 
     &  1.13031820798497005442D+00, 
     &  0.27149533953407656237D+00, 
     &  0.04433684984866380495D+00, 
     &  0.00547424044209373265D+00, 
     &  0.00054292631191394375D+00, 
     &  0.00004497732295429515D+00, 
     &  0.00000319843646240199D+00, 
     &  0.00000019921248066728D+00, 
     &  0.00000001103677172552D+00, 
     &  0.00000000055058960797D+00, 
     &  0.00000000002497956617D+00, 
     &  0.00000000000103915223D+00, 
     &  0.00000000000003991263D+00, 
     &  0.00000000000000142376D+00, 
     &  0.00000000000000004741D+00, 
     &  0.00000000000000000148D+00, 
     &  0.00000000000000000004D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) 
     &  '  ECHEBSER3 computes a Chebyshev series approximation'
      write ( *, '(a)' ) '  and the first three derivatives.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Errors of a Chebyshev series for exp(x)'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '    x        err(y)       err(y'')       err(y")      ' //
     &  'err(y"'')'
      write ( *, '(a)' ) ''

      do i = -10, 10
        x = dble ( i ) / 10.0D+00
        call echebser3 ( x, table5, nc, s, s1, s2, s3 )
        y = exp ( x )
        s = s - y
        s1 = s1 - y
        s2 = s2 - y
        s3 = s3 - y
        write ( *, '(f7.4,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    x, s, s1, s2, s3
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 considers an even Chebyshev series for COS(PI*X/2).
c
c  Discussion:
c
c    TABLE1 contains the even Chebyshev series coefficients for
c    cos(pi*x/2) over [-1,+1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
      implicit none

      integer nc
      parameter ( nc = 11 )

      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision s
      double precision s1
      double precision s2
      double precision table1(nc)
      double precision x
      double precision y
      double precision y1
      double precision y2

      save table1

      data table1 /
     &  +0.94400243153646953490D+00, 
     &  -0.49940325827040708740D+00, 
     &  +0.02799207961754761751D+00, 
     &  -0.00059669519654884650D+00, 
     &  +0.00000670439486991684D+00, 
     &  -0.00000004653229589732D+00, 
     &  +0.00000000021934576590D+00, 
     &  -0.00000000000074816487D+00, 
     &  +0.00000000000000193230D+00, 
     &  -0.00000000000000000391D+00, 
     &  +0.00000000000000000001D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  EVENCHEBSER2 computes an even Chebyshev series'
      write ( *, '(a)' ) '  and its first two derivatives.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Errors of an even Chebyshev series for cos(pi*x/2):'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '    x        err(y)       err(y'')       err(y")'
      write ( *, '(a)' ) ''

      do i = 0, 10
        x = dble ( i ) / 10.0D+00
        call evenchebser2 ( x, table1, nc, s, s1, s2 )
        y = cos ( 0.5D+00 * r8_pi * x );
        y1 = - 0.5D+00 * r8_pi * sin ( 0.5D+00 * r8_pi * x );
        y2 = - 0.25D+00 * r8_pi * r8_pi * cos ( 0.5D+00 * r8_pi * x );
        s = s - y
        s1 = s1 - y1
        s2 = s2 - y2
        write ( *, '(f7.4,2x,g14.6,2x,g14.6,2x,g14.6)' ) x, s, s1, s2
      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 considers an odd Chebyshev series for SINH(X).
c
c  Discussion:
c
c    TABLE5ODD contains the odd Chebyshev series coefficients for
c    sinh(x) over -1 <= x <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
      implicit none

      integer nc
      parameter ( nc = 9 )

      integer i
      double precision s
      double precision s1
      double precision s2
      double precision table5odd(nc)
      double precision x
      double precision y
      double precision y1

      save table5odd

      data table5odd /
     &  1.13031820798497005442D+00, 
     &  0.04433684984866380495D+00, 
     &  0.00054292631191394375D+00, 
     &  0.00000319843646240199D+00, 
     &  0.00000001103677172552D+00, 
     &  0.00000000002497956617D+00, 
     &  0.00000000000003991263D+00, 
     &  0.00000000000000004741D+00, 
     &  0.00000000000000000004D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) 
     &  '  ODDCHEBSER2 computes an odd Chebyshev series approximation.'
      write ( *, '(a)' ) '  and its first two derivatives.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Errors of odd Chebyshev series y(x) approximating sinh(x):'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '    x        err(y)       err(y'')       err(y")'
      write ( *, '(a)' ) ''

      do i = 0, 10
        x = dble ( i ) / 10.0D+00
        call oddchebser2 ( x, table5odd, nc, s, s1, s2 )
        y = sinh ( x )
        y1 = cosh ( x )
        s = s - y
        s1 = s1 - y1
        s2 = s2 - y
        write ( *, '(f7.4,2x,g14.6,2x,g14.6,2x,g14.6)' ) x, s, s1, s2
      end do

      return
      end

