      program main

c*********************************************************************72
c
cc MAIN is the main program for SPHERE_STEREOGRAPH_PRB.
c
c  Discussion:
c
c    SPHERE_STEREOGRAPH_PRB tests the SPHERE_STEREOGRAPH library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPHERE_STEREOGRAPH_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SPHERE_STEREOGRAPH library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SPHERE_STEREOGRAPH_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 checks that the two functions are inverses.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 100 )

      double precision dif
      integer j
      double precision p(m,n)
      double precision p1(m,n)
      double precision p2(m,n)
      double precision q(m,n)
      double precision q1(m,n)
      double precision q2(m,n)
      double precision r8mat_norm_fro_affine
      integer seed

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  SPHERE_STEREOGRAPH maps from sphere to plane.'
      write ( *, '(a)' ) 
     &  '  SPHERE_STEREOGRAPH_INVERSE is the inverse map.'
      write ( *, '(a)' ) 
     &  '  Check that these two functions are inverses.'
c
c  Check #1.
c
      seed = 123456789

      call uniform_on_sphere01_map ( m, n, seed, p1 )
      call sphere_stereograph ( m, n, p1, q )
      call sphere_stereograph_inverse ( m, n, q, p2 )

      dif = r8mat_norm_fro_affine ( m, n, p1, p2 )
      
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Map points from sphere to plane to sphere.'
      write ( *, '(a,i6,a,g14.6)' ) 
     &  '  Frobenius difference for ', n, ' points was ', dif
c
c  Check #2.
c
      call r8mat_uniform_01 ( m, n, seed, q1 )

       do j = 1, n
        q1(m,j) = 1.0D+00
      end do

      call sphere_stereograph_inverse ( m, n, q1, p )
      call sphere_stereograph ( m, n, p, q2 )

      dif = r8mat_norm_fro_affine ( m, n, q1, q2 )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Map points from plane to sphere to plane.'
      write ( *, '(a,i6,a,g14.6)' ) 
     &  '  Frobenius difference for ', n, ' points was ', dif

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 checks the generalized mapping.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 100 )

      double precision center(m)
      double precision dif
      double precision focus(m)
      integer i
      integer j
      double precision p1(m,n)
      double precision p2(m,n)
      double precision q1(m,n)
      double precision q2(m,n)
      double precision r8mat_norm_fro_affine
      integer seed

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  SPHERE_STEREOGRAPH standard mapping from sphere to plane.'
      write ( *, '(a)' ) '  SPHERE_STEREOGRAPH2 generalized mapping:'
      write ( *, '(a)' ) '  (focus and center are arbitrary)'
      write ( *, '(a)' ) '  Check that these two functions can agree.'

      do i = 1, m - 1
        focus(i) = 0.0D+00
      end do
      focus(m) = -1.0D+00

      do i = 1, m
        center(i) = 0.0D+00
      end do
c
c  Check #1.
c
      seed = 123456789
      call uniform_on_sphere01_map ( m, n, seed, p1 )

      call sphere_stereograph ( m, n, p1, q1 )

      call sphere_stereograph2 ( m, n, p1, focus, center, q2 )

      dif = r8mat_norm_fro_affine ( m, n, q1, q2 )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Map points from sphere to plane.'
      write ( *, '(a,i6,a,g14.6)' ) 
     &  '  Frobenius difference for ', n, ' points was ', dif
c
c  Check #2.
c
      call r8mat_uniform_01 ( m, n, seed, q1 )
      do j = 1, n
        q1(m,j) = 1.0D+00
      end do

      call sphere_stereograph_inverse ( m, n, q1, p1 )

      call sphere_stereograph2_inverse ( m, n, q1, focus, center, p2 )

      dif = r8mat_norm_fro_affine ( m, n, p1, p2 )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Map points from plane to sphere.'
      write ( *, '(a,i6,a,g14.6)' ) 
     &  '  Frobenius difference for ', n, ' points was ', dif

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 checks that the two functions are inverses.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 100 )

      double precision alpha(n)
      double precision beta(n)
      double precision center(m)
      double precision dif
      double precision focus(m)
      integer i
      integer j
      double precision normal(m)
      double precision p(m,n)
      double precision p1(m,n)
      double precision p2(m,n)
      double precision pq(m)
      double precision pr(m)
      double precision q(m,n)
      double precision q1(m,n)
      double precision q2(m,n)
      double precision r
      double precision r8mat_norm_fro_affine
      double precision r8vec_norm_affine
      integer seed
      double precision tang(m)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  SPHERE_STEREOGRAPH2 maps from sphere to plane.'
      write ( *, '(a)' ) 
     &  '  SPHERE_STEREOGRAPH2_INVERSE is the inverse map.'
      write ( *, '(a)' ) 
     &  '  Check that these two functions are inverses.'

      seed = 123456789

      call r8vec_uniform_01 ( m, seed, focus )
      call r8vec_uniform_01 ( m, seed, center )
      r = r8vec_norm_affine ( m, focus, center )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Using radius = ', r
      call r8vec_transpose_print ( m, center, '  Center:' )
      call r8vec_transpose_print ( m, focus, '  Focus:' )
c
c  Check #1.
c
      call uniform_on_sphere01_map ( m, n, seed, p1 )

      do j = 1, n
        p1(1:m,j) = center(1:m) + r * p1(1:m,j)
      end do

      call sphere_stereograph2 ( m, n, p1, focus, center, q )

      call sphere_stereograph2_inverse ( m, n, q, focus, center, p2 )

      dif = r8mat_norm_fro_affine ( m, n, p1, p2 )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Map points from sphere to plane to sphere.'
      write ( *, '(a,i6,a,g14.6)' ) 
     &  '  Frobenius difference for ', n, ' points was ', dif
c
c  Check #2.
c  We have to work hard to get random points on the plane, since
c  all we know to begin with is the point of tangency and the normal.
c
      do i = 1, m
        tang(i) = 2.0D+00 * center(i) - focus(i)
      end do

      do i = 1, m
        normal(i) = center(i) - focus(i)
      end do

      call plane_normal_basis_3d ( tang, normal, pr, pq )

      call r8vec_uniform_01 ( n, seed, alpha )
      call r8vec_uniform_01 ( n, seed, beta )

      do j = 1, n
        do i = 1, m
          q1(i,j) = tang(i) + pr(i) * alpha(j) + pq(i) * beta(j)
        end do
      end do
      call sphere_stereograph2_inverse ( m, n, q1, focus, center, p )
      call sphere_stereograph2 ( m, n, p, focus, center, q2 )

      dif = r8mat_norm_fro_affine ( m, n, q1, q2 )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Map points from plane to sphere to plane.'
      write ( *, '(a,i6,a,g14.6)' ) 
     &  '  Frobenius difference for ', n, ' points was ', dif

      return
      end
