      program main

c*********************************************************************72
c
cc MAIN is the main program for BVLS_PRB.
c
c  Discussion:
c
c    BVLS_PRB tests the BVLS library.
c
c    This program demonstrates the use of BVLS for solving least squares
c    problems with bounds on the variables.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 June 2014
c
c  Author:
c
c    Original FORTRAN90 version by Charles Lawson, Richard Hanson.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Charles Lawson, Richard Hanson,
c    Solving Least Squares Problems,
c    SIAM, 1995,
c    ISBN: 0898713560,
c    LC: QA275.L38.
c
      implicit none

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVLS_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BVLS library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVLS_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 runs test case 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2014
c
c  Author:
c
c    Original FORTRAN90 version by Charles Lawson, Richard Hanson.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Charles Lawson, Richard Hanson,
c    Solving Least Squares Problems,
c    SIAM, 1995,
c    ISBN: 0898713560,
c    LC: QA275.L38.
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer n
      parameter ( n = 2 )
      integer jstep
      parameter ( jstep = 5 )

      double precision a(m,n)
      double precision a2(m,n)
      double precision b(m)
      double precision b2(m)
      double precision bnd(2,n)
      integer i
      integer ierr
      integer index(n)
      integer j
      integer j1
      integer j2
      integer nsetp
      double precision rnorm
      integer seed
      double precision w(n)
      double precision x(n)

      save bnd

      data ((bnd(i,j),i=1,2),j=1,2)/
     &  1.0D+00, 2.0D+00,
     &  3.0D+00, 4.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i5,a,i5,a,g17.5)')     '  M =', m,',   N =', n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Bounds:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        write ( *, '(2x,5g14.6)' ) bnd(1,j1:j2)
        write ( *, '(2x,5g14.6)' ) bnd(2,j1:j2)
      end do

      seed = 123456789
      call r8vec_uniform_01 ( m, seed, b )
      call r8mat_uniform_01 ( m, n, seed, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Matrix A:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        do i = 1,m
          write ( *, '(2x,5g14.6)' ) a(i,j1:j2)
        end do
      end do

      call r8vec_copy ( m, b, b2 )
      call r8mat_copy ( m, n, a, a2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  RHS B:'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,5g14.6)' ) b(1:m)

      call bvls ( m, n, a2, b2, bnd, x, rnorm, nsetp, w, index, ierr )

      call bvls_report ( m, n, a, b, bnd, x, rnorm, nsetp, w, index, 
     &  ierr )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 runs test case 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2014
c
c  Author:
c
c    Original FORTRAN90 version by Charles Lawson, Richard Hanson.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Charles Lawson, Richard Hanson,
c    Solving Least Squares Problems,
c    SIAM, 1995,
c    ISBN: 0898713560,
c    LC: QA275.L38.
c
      implicit none

      integer m
      parameter ( m = 2 )
      integer n
      parameter ( n = 4 )
      integer jstep 
      parameter ( jstep = 5 )

      double precision a(m,n)
      double precision a2(m,n)
      double precision b(m)
      double precision b2(m)
      double precision bnd(2,n)
      integer i
      integer ierr
      integer index(n)
      integer j
      integer j1
      integer j2
      integer nsetp
      double precision rnorm
      integer seed
      double precision w(n)
      double precision x(n)

      save bnd

      data ((bnd(i,j),i=1,2),j=1,4)/
     &  0.0D+00, 10.0D+00,
     &  0.0D+00, 10.0D+00,
     &  0.0D+00, 10.0D+00,
     &  0.0D+00, 10.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i5,a,i5,a,g17.5)')     '  M =', m,',   N =', n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Bounds:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        write ( *, '(2x,5g14.6)' ) bnd(1,j1:j2)
        write ( *, '(2x,5g14.6)' ) bnd(2,j1:j2)
      end do

      seed = 123456789
      call r8vec_uniform_01 ( m, seed, b )
      call r8mat_uniform_01 ( m, n, seed, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Matrix A:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        do i = 1,m
          write ( *, '(2x,5g14.6)' ) a(i,j1:j2)
        end do
      end do

      call r8vec_copy ( m, b, b2 )
      call r8mat_copy ( m, n, a, a2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  RHS B:'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,5g14.6)' ) b(1:m)

      call bvls ( m, n, a2, b2, bnd, x, rnorm, nsetp, w, index, ierr )

      call bvls_report ( m, n, a, b, bnd, x, rnorm, nsetp, w, index, 
     &  ierr )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 runs test case 3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2014
c
c  Author:
c
c    Original FORTRAN90 version by Charles Lawson, Richard Hanson.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Charles Lawson, Richard Hanson,
c    Solving Least Squares Problems,
c    SIAM, 1995,
c    ISBN: 0898713560,
c    LC: QA275.L38.
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 2 )
      integer jstep
      parameter ( jstep = 5 )

      double precision a(m,n)
      double precision a2(m,n)
      double precision b(m)
      double precision b2(m)
      double precision bnd(2,n)
      integer i
      integer ierr
      integer index(n)
      integer j
      integer j1
      integer j2
      integer nsetp
      double precision rnorm
      integer seed
      double precision w(n)
      double precision x(n)

      save bnd

      data ((bnd(i,j),i=1,2),j=1,2)/
     &    0.0D+00,  100.0D+00,
     & -100.0D+00,  100.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i5,a,i5,a,g17.5)')     '  M =', m,',   N =', n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Bounds:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        write ( *, '(2x,5g14.6)' ) bnd(1,j1:j2)
        write ( *, '(2x,5g14.6)' ) bnd(2,j1:j2)
      end do

      seed = 123456789
      call r8vec_uniform_01 ( m, seed, b )
      call r8mat_uniform_01 ( m, n, seed, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Matrix A:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        do i = 1,m
          write ( *, '(2x,5g14.6)' ) a(i,j1:j2)
        end do
      end do

      call r8vec_copy ( m, b, b2 )
      call r8mat_copy ( m, n, a, a2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  RHS B:'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,5g14.6)' ) b(1:m)

      call bvls ( m, n, a2, b2, bnd, x, rnorm, nsetp, w, index, ierr )

      call bvls_report ( m, n, a, b, bnd, x, rnorm, nsetp, w, index, 
     &  ierr )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 runs test case 4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2014
c
c  Author:
c
c    Original FORTRAN90 version by Charles Lawson, Richard Hanson.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Charles Lawson, Richard Hanson,
c    Solving Least Squares Problems,
c    SIAM, 1995,
c    ISBN: 0898713560,
c    LC: QA275.L38.
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 10 )
      integer jstep
      parameter ( jstep = 5 )

      double precision a(m,n)
      double precision a2(m,n)
      double precision b(m)
      double precision b2(m)
      double precision bnd(2,n)
      integer i
      integer ierr
      integer index(n)
      integer j
      integer j1
      integer j2
      integer nsetp
      double precision r8_huge
      parameter ( r8_huge = 1.0D+30 )
      double precision rnorm
      integer seed
      double precision unbnd
      double precision w(n)
      double precision x(n)

      save bnd
      save unbnd

      data ((bnd(i,j),i=1,2),j=1,10)/
     &   0.0D+00,     0.0D+00,
     &  -0.3994D+00, -0.3994D+00,
     &  -1.0D+00,     1.0D+00,
     &  -0.3D+00,    -0.2D+00,
     &  21.0D+00,    22.0D+00,
     &  -4.0D+00,    -3.0D+00,
     &  45.0D+00,    46.0D+00,
     & 100.0D+00,   101.0D+00,
     &   1.0D+06,     1.0D+06,
     &  -1.0D+00,     1.0D+00 /

      data unbnd / 1.0D+06 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'

      do j = 1, n
        if ( bnd(1,j) .eq. unbnd ) then
          bnd(1,j) = - r8_huge
        end if
        if ( bnd(2,j) .eq. unbnd ) then
          bnd(2,j) = + r8_huge
        end if
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,i5,a,i5,a,g17.5)')     '  M =', m,',   N =', n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Bounds:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        write ( *, '(2x,5g14.6)' ) bnd(1,j1:j2)
        write ( *, '(2x,5g14.6)' ) bnd(2,j1:j2)
      end do

      seed = 123456789
      call r8vec_uniform_01 ( m, seed, b )
      call r8mat_uniform_01 ( m, n, seed, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Matrix A:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        do i = 1,m
          write ( *, '(2x,5g14.6)' ) a(i,j1:j2)
        end do
      end do

      call r8vec_copy ( m, b, b2 )
      call r8mat_copy ( m, n, a, a2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  RHS B:'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,5g14.6)' ) b(1:m)

      call bvls ( m, n, a2, b2, bnd, x, rnorm, nsetp, w, index, ierr )

      call bvls_report ( m, n, a, b, bnd, x, rnorm, nsetp, w, index, 
     &  ierr )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 runs test case 5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2014
c
c  Author:
c
c    Original FORTRAN90 version by Charles Lawson, Richard Hanson.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Charles Lawson, Richard Hanson,
c    Solving Least Squares Problems,
c    SIAM, 1995,
c    ISBN: 0898713560,
c    LC: QA275.L38.
c
      implicit none

      integer m
      parameter ( m = 10 )
      integer n
      parameter ( n = 5 )
      integer jstep
      parameter ( jstep = 5 )

      double precision a(m,n)
      double precision a2(m,n)
      double precision b(m)
      double precision b2(m)
      double precision bnd(2,n)
      integer i
      integer ierr
      integer index(n)
      integer j
      integer j1
      integer j2
      integer nsetp
      double precision rnorm
      integer seed
      double precision w(n)
      double precision x(n)

      save bnd

      data ((bnd(i,j),i=1,2),j=1,5)/
     &  0.0D+00,   1.0D+00,
     & -1.0D+00,   0.0D+00,
     &  0.0D+00,   1.0D+00,
     &  0.3D+00,   0.4D+00,
     &  0.048D+00, 0.049D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i5,a,i5,a,g17.5)')     '  M =', m,',   N =', n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Bounds:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        write ( *, '(2x,5g14.6)' ) bnd(1,j1:j2)
        write ( *, '(2x,5g14.6)' ) bnd(2,j1:j2)
      end do

      seed = 123456789
      call r8vec_uniform_01 ( m, seed, b )
      call r8mat_uniform_01 ( m, n, seed, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Matrix A:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        do i = 1,m
          write ( *, '(2x,5g14.6)' ) a(i,j1:j2)
        end do
      end do

      call r8vec_copy ( m, b, b2 )
      call r8mat_copy ( m, n, a, a2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  RHS B:'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,5g14.6)' ) b(1:m)

      call bvls ( m, n, a2, b2, bnd, x, rnorm, nsetp, w, index, ierr )

      call bvls_report ( m, n, a, b, bnd, x, rnorm, nsetp, w, index, 
     &  ierr )

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 runs test case 6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 June 2014
c
c  Author:
c
c    Original FORTRAN90 version by Charles Lawson, Richard Hanson.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Charles Lawson, Richard Hanson,
c    Solving Least Squares Problems,
c    SIAM, 1995,
c    ISBN: 0898713560,
c    LC: QA275.L38.
c
      implicit none

      integer m
      parameter ( m = 6 )
      integer n
      parameter ( n = 4 )
      integer jstep
      parameter ( jstep = 5 )

      double precision a(m,n)
      double precision a2(m,n)
      double precision b(m)
      double precision b2(m)
      double precision bnd(2,n)
      integer i
      integer ierr
      integer index(n)
      integer j
      integer j1
      integer j2
      integer nsetp
      double precision r8_huge
      parameter ( r8_huge = 1.0D+30 )
      double precision rnorm
      integer seed
      double precision unbnd
      double precision w(n)
      double precision x(n)

      save bnd
      save unbnd

      data ((bnd(i,j),i=1,2),j=1,4)/
     & -100.0D+00, 100.0D+00,
     &  999.0D+00, 999.0D+00,
     &  999.0D+00, 999.0D+00,
     &  999.0D+00, 999.0D+00 /

      data unbnd / 999.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'

      do j = 1, n
        if ( bnd(1,j) .eq. unbnd ) then
          bnd(1,j) = - r8_huge
        end if
        if ( bnd(2,j) .eq. unbnd ) then
          bnd(2,j) = + r8_huge
        end if
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,i5,a,i5,a,g17.5)')     '  M =', m,',   N =', n

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Bounds:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        write ( *, '(2x,5g14.6)' ) bnd(1,j1:j2)
        write ( *, '(2x,5g14.6)' ) bnd(2,j1:j2)
      end do

      seed = 123456789
      call r8vec_uniform_01 ( m, seed, b )
      call r8mat_uniform_01 ( m, n, seed, a )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Matrix A:'

      do j1 = 1, n, jstep
        j2 = min ( j1 - 1 + jstep, n )
        write ( *, '(a)' ) ' '
        do i = 1,m
          write ( *, '(2x,5g14.6)' ) a(i,j1:j2)
        end do
      end do

      call r8vec_copy ( m, b, b2 )
      call r8mat_copy ( m, n, a, a2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  RHS B:'
      write ( *, '(a)' ) ' '
      write ( *, '(2x,5g14.6)' ) b(1:m)

      call bvls ( m, n, a2, b2, bnd, x, rnorm, nsetp, w, index, ierr )

      call bvls_report ( m, n, a, b, bnd, x, rnorm, nsetp, w, index, 
     &  ierr )

      return
      end

