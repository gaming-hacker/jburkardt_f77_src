      program main

c*********************************************************************72
c
cc POLYNOMIAL_PRB tests the POLYNOMIAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the POLYNOMIAL library.'

      call polynomial_add_test ( )
      call polynomial_axpy_test ( )
      call polynomial_compress_test ( )
      call polynomial_dif_test ( )
      call polynomial_mul_test ( )
      call polynomial_print_test ( )
      call polynomial_scale_test ( )
      call polynomial_sort_test ( )
      call polynomial_value_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine polynomial_add_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_ADD_TEST tests POLYNOMIAL_ADD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 November 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer o1
      parameter ( o1 = 6 )
      integer o2
      parameter ( o2 = 5 )
      integer o_max
      parameter ( o_max = o1 + o2 )

      double precision c(o_max)
      double precision c1(o1)
      double precision c2(o2)
      integer e(o_max)
      integer e1(o1)
      integer e2(o2)
      integer o
      character * ( 80 ) title
      character * ( 80 ) title1
      character * ( 80 ) title2

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_ADD_TEST'
      write ( *, '(a)' ) '  POLYNOMIAL_ADD adds two polynomials.'

      c1(1) = 7.0
      c1(2) = - 5.0
      c1(3) = 9.0
      c1(4) = 11.0
      c1(5) = 0.0
      c1(6) = - 13.0

      e1(1) = 1
      e1(2) = 2
      e1(3) = 4
      e1(4) = 5
      e1(5) = 12
      e1(6) = 33

      title1 = '  P1(X) ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o1, c1, e1, title1 )

      c2(1) = 2.0
      c2(2) = 3.0
      c2(3) = -8.0
      c2(4) = 4.0
      c2(5) = 9.0

      e2(1) = 1
      e2(2) = 3
      e2(3) = 4
      e2(4) = 30
      e2(5) = 33

      title2 = '  P2(X) ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o2, c2, e2, title2 )

      call polynomial_add ( o1, c1, e1, o2, c2, e2, o, c, e )
      title = '  P1(X) + P2(X) ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      return
      end
      subroutine polynomial_axpy_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_AXPY_TEST tests POLYNOMIAL_AXPY.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer o1
      parameter ( o1 = 6 )
      integer o2
      parameter ( o2 = 5 )
      integer o_max
      parameter ( o_max = o1 + o2 )

      double precision c(o_max)
      double precision c1(o1)
      double precision c2(o2)
      integer e(o_max)
      integer e1(o1)
      integer e2(o2)
      integer o
      double precision s
      character * ( 80 ) title
      character * ( 80 ) title1
      character * ( 80 ) title2

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_AXPY_TEST'
      write ( *, '(a)' ) '  POLYNOMIAL_AXPY adds a multiple of one'
      write ( *, '(a)' ) '  polynomial to another.'

      c1(1) = 7.0
      c1(2) = - 5.0
      c1(3) = 9.0
      c1(4) = 11.0
      c1(5) = 0.0
      c1(6) = - 13.0

      e1(1) = 1
      e1(2) = 2
      e1(3) = 4
      e1(4) = 5
      e1(5) = 12
      e1(6) = 33

      title1 = '  P1(X) ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o1, c1, e1, title1 )

      c2(1) = 2.0
      c2(2) = 3.0
      c2(3) = -8.0
      c2(4) = 4.0
      c2(5) = 9.0

      e2(1) = 1
      e2(2) = 3
      e2(3) = 4
      e2(4) = 30
      e2(5) = 33

      title2 = '  P2(X) ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o2, c2, e2, title2 )

      s = 10.0
      call polynomial_axpy ( s, o1, c1, e1, o2, c2, e2, o, c, e )
      write ( title, '(a,g14.6,a)' ) '  ', s, ' * P1(X) + P2(X) = '
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      return
      end
      subroutine polynomial_compress_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_COMPRESS_TEST tests POLYNOMIAL_COMPRESS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer o
      parameter ( o = 10 )

      double precision c(o)
      double precision c2(o)
      integer e(o)
      integer e2(o)
      integer o2
      character * ( 80 ) title

      save c
      save e

      data c /
     &  7.0, - 5.0, 5.0, 9.0, 11.0, 3.0, 6.0, 0.0, - 13.0, 1.0E-20 /
      data e /
     & 1, 2, 2, 4, 5, 5, 5, 12, 33, 35 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_COMPRESS_TEST'
      write ( *, '(a)' ) 
     &  '  POLYNOMIAL_COMPRESS compresses a polynomial.'

      write ( *, '(a)' ) ''
      title = '  Uncompressed P(X) ='
      call polynomial_print ( m, o, c, e, title )

      call polynomial_compress ( o, c, e, o2, c2, e2 )

      write ( *, '(a)' ) ''
      title = '  Compressed P(X) ='
      call polynomial_print ( m, o2, c2, e2, title )

      return
      end
      subroutine polynomial_dif_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_DIF_TEST tests POLYNOMIAL_DIF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer o1
      parameter ( o1 = 4 )
      integer o_max
      parameter ( o_max = o1 )

      double precision c(o_max)
      double precision c1(o1)
      integer dif(m)
      integer e(o_max)
      integer e1(o1)
      integer o
      character * ( 80 ) title
      character * ( 80 ) title1

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_DIF_TEST'
      write ( *, '(a)' ) 
     &  '  POLYNOMIAL_DIF computes derivatives of a polynomial.'

      c1(1) = 2.0
      c1(2) = 3.0
      c1(3) = 4.0
      c1(4) = 5.0
      e1(1) = 1
      e1(2) = 10
      e1(3) = 12
      e1(4) = 32
      title1 = '  P(X) ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o1, c1, e1, title1 )

      dif(1) = 2
      dif(2) = 1

      call polynomial_dif ( m, o1, c1, e1, dif, o, c, e )
      title = '  d3 P(X) dx1 dx1 dx2 ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      return
      end
      subroutine polynomial_mul_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_MUL_TEST tests POLYNOMIAL_MUL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 November 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer o1
      parameter ( o1 = 4 )
      integer o2
      parameter ( o2 = 2 )
      integer o_max
      parameter ( o_max = o1 * o2 )

      double precision c(o_max)
      double precision c1(o1)
      double precision c2(o2)
      integer e(o_max)
      integer e1(o1)
      integer e2(o2)
      integer o
      character * ( 80 ) title
      character * ( 80 ) title1
      character * ( 80 ) title2

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_MUL_TEST'
      write ( *, '(a)' ) '  POLYNOMIAL_MUL multiplies two polynomials.'

      c1(1) = 2.0
      c1(2) = 3.0
      c1(3) = 4.0
      c1(4) = 5.0
      e1(1) = 1
      e1(2) = 3
      e1(3) = 4
      e1(4) = 6
      title1 = '  P1(X) ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o1, c1, e1, title1 )

      c2(1) = 6.0
      c2(2) = 7.0
      e2(1) = 2
      e2(2) = 5
      title2 = '  P2(X) ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o2, c2, e2, title2 )

      call polynomial_mul ( m, o1, c1, e1, o2, c2, e2, o, c, e )
      title = '  P1(X) * P2(X) ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      return
      end
      subroutine polynomial_print_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_PRINT_TEST tests POLYNOMIAL_PRINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer o
      parameter ( o = 6 )

      double precision c(o)
      integer e(o)
      character * ( 80 ) title

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_PRINT_TEST'
      write ( *, '(a)' ) '  POLYNOMIAL_PRINT prints a polynomial.'

      c(1) = 7.0
      c(2) = - 5.0
      c(3) = 9.0
      c(4) = 11.0
      c(5) = 0.0
      c(6) = - 13.0

      e(1) = 1
      e(2) = 2
      e(3) = 4
      e(4) = 5
      e(5) = 12
      e(6) = 33

      title = '  P1(X) ='

      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      return
      end
      subroutine polynomial_scale_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_SCALE_TEST tests POLYNOMIAL_SCALE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 october 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer o
      parameter ( o = 6 )

      double precision c(o)
      integer e(o)
      double precision s
      character * ( 80 ) title

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_SCALE_TEST'
      write ( *, '(a)' ) '  POLYNOMIAL_SCALE scales a polynomial by S.'

      c(1) = 7.0
      c(2) = - 5.0
      c(3) = 9.0
      c(4) = 11.0
      c(5) = 0.0
      c(6) = - 13.0

      e(1) = 1
      e(2) = 2
      e(3) = 4
      e(4) = 5
      e(5) = 12
      e(6) = 33

      title = '  P1(X) ='

      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      s = - 0.5D+00
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Apply scale factor S = ', s
      call polynomial_scale ( s, m, o, c, e )

      write ( *, '(a)' ) ''
      title = '  S * P(X):'
      call polynomial_print ( m, o, c, e, title )

      return
      end
      subroutine polynomial_sort_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_SORT_TEST tests POLYNOMIAL_SORT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer o
      parameter ( o = 6 )

      double precision c(o)
      integer e(o)
      character * ( 80 ) title

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_SORT_TEST'
      write ( *, '(a)' ) 
     &  '  POLYNOMIAL_SORT sorts a polynomial by exponent index.'

      c(1) =    0.0
      c(2) =    9.0
      c(3) =  - 5.0
      c(4) =  - 13.0
      c(5) =     7.0
      c(6) =    11.0

      e(1) = 12
      e(2) =  4
      e(3) =  2
      e(4) = 33
      e(5) =  1
      e(6) =  5

      title = '  Unsorted polynomial'
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      call polynomial_sort ( o, c, e )

      title = '  Sorted polynomial'
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      return
      end
      subroutine polynomial_value_test ( )

c*********************************************************************72
c
cc POLYNOMIAL_VALUE_TEST tests POLYNOMIAL_VALUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer nx
      parameter ( nx = 2 )
      integer o
      parameter ( o = 6 )

      double precision c(o)
      integer e(o)
      integer j
      double precision p(nx)
      character * ( 80 ) title
      double precision x(m,nx)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'POLYNOMIAL_VALUE_TEST'
      write ( *, '(a)' ) '  POLYNOMIAL_VALUE evaluates a polynomial.'

      c(1) = 7.0
      c(2) =  - 5.0
      c(3) =  9.0
      c(4) =  11.0
      c(5) =  0.0
      c(6) =  - 13.0

      e(1) = 1
      e(2) = 2
      e(3) = 4
      e(4) = 5
      e(5) = 12
      e(6) = 33

      title = '  P(X) ='
      write ( *, '(a)' ) ''
      call polynomial_print ( m, o, c, e, title )

      x(1,1) = 1.0
      x(2,1) = 2.0
      x(3,1) = 3.0

      x(1,2) = -2.0
      x(2,2) = 4.0
      x(3,2) = 1.0

      call polynomial_value ( m, o, c, e, nx, x, p )
      do j = 1, nx
        write ( *, '(a,f10.4,a,f10.4,a,f10.4,a,g14.6)' ) 
     &    '  P(', x(1,j), ',', x(2,j), ',', x(3,j), ') = ', p(j)
      end do

      return
      end

