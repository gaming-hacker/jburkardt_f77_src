      program main

c*********************************************************************72
c
cc LINE_CVT_LLOYD_PRB tests the line_cvt_lloyd library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LINE_CVT_LLOYD_PRB'
      write ( *, '(a)' ) '  FORTRAN90 version'
      write ( *, '(a)' ) '  Test the LINE_CVT_LLOYD library.'

      call test01 ( )
      call test02 ( )
c
c  Repeat, using sorted initial points.
c
      call test03 ( )
      call test04 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LINE_CVT_LLOYD_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc LINE_CVT_LLOYD_TEST01 tests the unconstrained computation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 25 )

      double precision a
      double precision b
      double precision h
      character * ( 255 ) header
      integer it_num
      integer seed
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LINE_CVT_LLOYD_TEST01:'
      write ( *, '(a)' ) '  Test the unconstrained computation.'

      a = 0.0D+00
      b = 1.0D+00
      it_num = 200
      seed = 123456789
      call r8vec_uniform_ab ( n, a, b, seed, x )
      header = 'test01'

      write ( *, '(a)' ) ''
      write ( *, '(a,i3,a,f8.4,a,f8.4,a)' )
     &  '  Use ', n, ' points in the interval [', a, ',', b, ']'
      write ( *, '(a,i4)' ) 
     &  '  Number of iterations to take is ', it_num
      write ( *, '(a)' ) 
     &  '  Call this calculation "' // trim ( header ) // '"'
      h = ( b - a ) / dble ( n - 1 )
      write ( *, '(a,g14.6)' ) '  Expect a uniform spacing of ', h

      call r8vec_print ( n, x, '  Initial generators:' )

      call line_cvt_lloyd ( n, a, b, it_num, header, x )

      call r8vec_print ( n, x, '  Final generators:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc LINE_CVT_LLOYD_TEST02 tests the constrained computation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 25 )

      double precision a
      double precision b
      double precision h
      character * ( 255 ) header
      integer it_num
      integer seed
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LINE_CVT_LLOYD_TEST02:'
      write ( *, '(a)' ) '  Test the constrained computation.'

      a = 0.0D+00
      b = 1.0D+00
      it_num = 200
      seed = 123456789
      call r8vec_uniform_ab ( n, a, b, seed, x )
      header = 'test02'

      write ( *, '(a)' ) ''
      write ( *, '(a,i3,a,f8.4,a,f8.4,a)' )
     &  '  Use ', n, ' points in the interval [', a, ',', b, ']'
      write ( *, '(a,i4)' ) '  Number of iterations to take is ', it_num
      write ( *, '(a)' ) 
     &  '  Call this calculation "'// trim ( header ) // '"'
      h = ( b - a ) / dble ( n )
      write ( *, '(a,g14.6)' ) '  Expect a uniform spacing of ', h

      call r8vec_print ( n, x, '  Initial generators:' )

      call line_ccvt_lloyd ( n, a, b, it_num, header, x )

      call r8vec_print ( n, x, '  Final generators:' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc LINE_CVT_LLOYD_TEST03 tests the unconstrained computation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 25 )

      double precision a
      double precision b
      double precision h
      character * ( 255 ) header
      integer it_num
      integer seed
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LINE_CVT_LLOYD_TEST03:'
      write ( *, '(a)' ) '  Test the unconstrained computation.'
      write ( *, '(a)' ) '  SORT the random initial values before use.'

      a = 0.0D+00
      b = 1.0D+00
      it_num = 200
      seed = 123456789
      call r8vec_uniform_ab ( n, a, b, seed, x )
      call r8vec_sort_insert_a ( n, x )
      header = 'test03'

      write ( *, '(a)' ) ''
      write ( *, '(a,i3,a,f8.4,a,f8.4,a)' )
     &  '  Use ', n, ' points in the interval [', a, ',', b, ']'
      write ( *, '(a,i4)' ) '  Number of iterations to take is ', it_num
      write ( *, '(a)' ) 
     &  '  Call this calculation "'// trim ( header ) // '"'
      h = ( b - a ) / dble ( n - 1 )
      write ( *, '(a,g14.6)' ) '  Expect a uniform spacing of ', h

      call r8vec_print ( n, x, '  Initial generators:' )

      call line_cvt_lloyd ( n, a, b, it_num, header, x )

      call r8vec_print ( n, x, '  Final generators:' )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc LINE_CVT_LLOYD_TEST04 tests the constrained computation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 25 )

      double precision a
      double precision b
      double precision h
      character * ( 255 ) header
      integer it_num
      integer seed
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LINE_CVT_LLOYD_TEST04:'
      write ( *, '(a)' ) '  Test the constrained computation.'
      write ( *, '(a)' ) '  SORT the initial points before use.'

      a = 0.0D+00
      b = 1.0D+00
      it_num = 200
      seed = 123456789
      call r8vec_uniform_ab ( n, a, b, seed, x )
      call r8vec_sort_insert_a ( n, x )
      header = 'test04'

      write ( *, '(a)' ) ''
      write ( *, '(a,i3,a,f8.4,a,f8.4,a)' )
     &  '  Use ', n, ' points in the interval [', a, ',', b, ']'
      write ( *, '(a,i4)' ) '  Number of iterations to take is ', it_num
      write ( *, '(a)' ) 
     &  '  Call this calculation "'// trim ( header ) // '"'
      h = ( b - a ) / dble ( n )
      write ( *, '(a,g14.6)' ) '  Expect a uniform spacing of ', h

      call r8vec_print ( n, x, '  Initial generators:' )

      call line_ccvt_lloyd ( n, a, b, it_num, header, x )

      call r8vec_print ( n, x, '  Final generators:' )

      return
      end
