      program main

c*********************************************************************72
c
cc MAIN is the main program for QWV_PRB.
c
c  Discussion:
c
c    QWV_PRB tests the QWV library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QWV_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the QWV library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QWV_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests QWV for a Newton-Cotes rule.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a
      double precision b
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta
      double precision w(n)
      double precision x(n)

      a =  0.0D+00
      b = +1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) 
     &  '  Use the Vandermonde procedure to compute the'
      write ( *, '(a)' ) 
     &  '  quadrature weights for a Newton-Cotes rule.'
      write ( *, '(a,i4)' ) '  Order N = ', n
      write ( *, '(a,f10.4,a,f10.4,a)' ) 
     &  '  Interval = [', a, ',', b, ']'
c
c  Set the points.
c
      call r8vec_even ( n, a, b, x )
      call r8vec_print ( n, x, '  Abscissas:' )
c
c  Compute the weights.
c
      call qwv ( n, a, b, x, w )

      call r8vec_print ( n, w, '  Weights:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests QWV for a Clenshaw-Curtis rule.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a
      double precision b
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta
      double precision w(n)
      double precision x(n)

      a =  -1.0D+00
      b = +1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Use the Vandermonde procedure to compute the'
      write ( *, '(a)' ) 
     &  '  quadrature weights for a Clenshaw-Curtis rule.'
      write ( *, '(a,i4)' ) '  Order N = ', n
      write ( *, '(a,f10.4,a,f10.4,a)' ) 
     &  '  Interval is [', a, ',', b, ']'
c
c  Set the points.
c
      do i = 1, n

        theta = dble ( n - i ) * pi 
     &        / dble ( n - 1 )

        x(i) = ( ( 1 - cos ( theta ) ) * a   
     &         + ( 1 + cos ( theta ) ) * b ) 
     &         /   2.0D+00

      end do

      call r8vec_print ( n, x, '  Abscissas:' )
c
c  Determine the corresponding weights.
c
      call qwv ( n, a, b, x, w )

      call r8vec_print ( n, w, '  Weights:' )

      return
      end
