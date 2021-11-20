      program main

c*********************************************************************72
c
cc MAIN is the main program for TEST_INT_CIRCLE_PRB.
c
c  Discussion:
c
c    TEST_INT_CIRCLE_PRB tests the TEST_INT_CIRCLE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2013
c
c  Author:
c
c    John Burkardt
c
      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST_INT_CIRCLE_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TEST_INT_CIRCLE library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST_INT_CIRCLE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01: integral of X^E in the unit circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 1048576 )

      double precision circle_unit_area
      integer e
      double precision f(n_max)
      integer n
      integer n_log2
      double precision p01_exact
      double precision q
      double precision r8vec_sum
      integer seed
      double precision t
      double precision xy(2,n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Use a simple Monte Carlo approach to estimate the'
      write ( *, '(a)' ) 
     &  '  integral of X^E over the circle of radius 1'
      write ( *, '(a)' ) '  centered at the origin.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '           N   E       Exact Approximate   Error'

      do e = 2, 6, 2
        write ( *, '(a)' ) ''
        call p01_param_set ( e )
        n = 1
        do n_log2 = 0, 20
          seed = 123456789
          call circle_unit_sample ( n, seed, xy )
          call p01_f ( n, xy, f )
          q = circle_unit_area ( ) * r8vec_sum ( n, f ) / dble ( n )
          t = p01_exact ( )
          write ( *, '(2x,i10,2x,i2,2x,f10.4,2x,f10.4,2x,e10.4)' ) 
     &      n, e, t, q, abs ( t - q )
          n = n * 2
        end do
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02: integral of R^E in the unit circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 1048576 )

      double precision circle_unit_area
      integer e
      double precision f(n_max)
      integer n
      integer n_log2
      double precision p02_exact
      double precision q
      double precision r8vec_sum
      integer seed
      double precision t
      double precision xy(2,n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Use a simple Monte Carlo approach to estimate the'
      write ( *, '(a)' ) 
     &  '  integral of R^E over the circle of radius 1'
      write ( *, '(a)' ) '  centered at the origin.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '           N   E       Exact Approximate   Error'

      do e = 1, 5, 2
        write ( *, '(a)' ) ''
        call p02_param_set ( e )
        n = 1
        do n_log2 = 0, 20
          seed = 123456789
          call circle_unit_sample ( n, seed, xy )
          call p02_f ( n, xy, f )
          q = circle_unit_area ( ) * r8vec_sum ( n, f ) / dble ( n )
          t = p02_exact ( )
          write ( *, '(2x,i10,2x,i2,2x,f10.4,2x,f10.4,2x,e10.4)' ) 
     &      n, e, t, q, abs ( t - q )
          n = n * 2
        end do
      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03: integral of exp(x) in the unit circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 1048576 )

      double precision circle_unit_area
      integer e
      double precision f(n_max)
      integer n
      integer n_log2
      double precision p03_exact
      double precision q
      double precision r8vec_sum
      integer seed
      double precision t
      double precision xy(2,n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  Use a simple Monte Carlo approach to estimate the'
      write ( *, '(a)' ) 
     &  '  integral of exp(X) over the circle of radius 1'
      write ( *, '(a)' ) '  centered at the origin.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '           N       Exact Approximate   Error'
      write ( *, '(a)' ) ''

      n = 1
      do n_log2 = 0, 20
        seed = 123456789
        call circle_unit_sample ( n, seed, xy )
        call p03_f ( n, xy, f )
        q = circle_unit_area ( ) * r8vec_sum ( n, f ) / dble ( n )
        t = p03_exact ( )
        write ( *, '(2x,i10,2x,i2,2x,f10.4,2x,f10.4,2x,e10.4)' ) 
     &    n, e, t, q, abs ( t - q )
        n = n * 2
      end do

      return
      end
