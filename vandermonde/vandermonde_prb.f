      program main

c*********************************************************************72
c
cc MAIN is the main program for VANDERMONDE_PRB.
c
c  Discussion:
c
c    VANDERMONDE_TEST tests the VANDERMONDE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 2014
c
c  Author:
c
c    John Burkardt
c
      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'VANDERMONDE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the VANDERMONDE library.'

      call bivand1_test ( )
      call bivand2_test ( )
      call dvand_test ( )
      call dvandprg_test ( )
      call pvand_test ( )
      call pvandprg_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'VANDERMONDE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine bivand1_test ( )

c*********************************************************************72
c
cc BIVAND1_TEST tests BIVAND1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 3 )

      integer n2
      parameter ( n2 = ( n * ( n + 1 ) ) / 2 )

      double precision a(n2,n2)
      double precision alpha(n)
      double precision beta(n)

      save alpha
      save beta 

      data alpha /
     &  1.0, 2.0, 3.0 /
      data beta /
     &  10.0, 20.0, 30.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BIVAND1_TEST:'
      write ( *, '(a)' ) 
     &  '  Compute a bidimensional Vandermonde matrix'
      write ( *, '(a)' ) '  associated with polynomials'
      write ( *, '(a)' ) '  of total degree < N.'

      call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )
      call r8vec_print ( n, beta, '  Vandermonde vector BETA:' )

      call bivand1 ( n, alpha, beta, a )

      call r8mat_print ( n2, n2, a, 
     &  '  Bidimensional Vandermonde matrix:' )

      return
      end
      subroutine bivand2_test ( )

c*********************************************************************72
c
cc BIVAND2_TEST tests BIVAND2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 3 )

      integer n2
      parameter ( n2 = n * n )

      double precision a(n2,n2)
      double precision alpha(n)
      double precision beta(n)

      save alpha
      save beta 

      data alpha /
     &  1.0, 2.0, 3.0 /
      data beta /
     &  10.0, 20.0, 30.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BIVAND2_TEST:'
      write ( *, '(a)' ) 
     &  '  Compute a bidimensional Vandermonde matrix'
      write ( *, '(a)' ) '  associated with polynomials'
      write ( *, '(a)' ) '  of maximum degree < N.'

      call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )
      call r8vec_print ( n, beta, '  Vandermonde vector BETA:' )

      call bivand2 ( n, alpha, beta, a )

      call r8mat_print ( n2, n2, a, 
     &  '  Bidimensional Vandermonde matrix:' )

      return
      end
      subroutine dvand_test ( )

c*********************************************************************72
c
cc DVAND_TEST tests DVAND.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision alpha(n)
      double precision alpha1(n)
      double precision b(n)
      integer seed
      integer test
      double precision x(n)
      double precision x1(n)

      save alpha1
      save x1

      data alpha1 /
     &  0.0, 1.0, 2.0, 3.0, 4.0 /
      data x1 /
     &  5.0, 3.0, 4.0, 1.0, 2.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'DVAND_TEST:'
      write ( *, '(a)' ) '  Solve a Vandermonde linear system A''*x=b'

      do test = 1, 2

        if ( test .eq. 1 ) then
          call r8vec_copy ( n, alpha1, alpha )
        else if ( test .eq. 2 ) then
          seed = 123456789
          call r8vec_uniform_01 ( n, seed, alpha )
        end if

        call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )

        call vand1 ( n, alpha, a )

        call r8vec_copy ( n, x1, x )
        call r8mat_mtv ( n, n, a, x, b )
        call r8vec_print ( n, b, '  Right hand side B:' )

        call dvand ( n, alpha, b, x )
        call r8vec_print ( n, x, '  Solution X:' )

      end do

      return
      end
      subroutine dvandprg_test ( )

c*********************************************************************72
c
cc DVANDPRG_TEST tests DVANDPRG.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision alpha(n)
      double precision alpha1(n)
      double precision b(n)
      double precision c(n)
      integer i
      double precision m(n)
      integer nsub
      integer seed
      integer test
      double precision x(n)
      double precision x1(n)

      save alpha1
      save x1

      data alpha1 /
     &  0.0, 1.0, 2.0, 3.0, 4.0 /
      data x1 /
     &  5.0, 3.0, 4.0, 1.0, 2.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'DVANDPRG_TEST:'
      write ( *, '(a)' ) '  Solve a Vandermonde linear system A''*x=b'
      write ( *, '(a)' ) '  progressively.'
      write ( *, '(a)' ) '  First we use ALPHA = 0, 1, 2, 3, 4.'
      write ( *, '(a)' ) '  Then we choose ALPHA at random.'

      do test = 1, 2

        if ( test .eq. 1 ) then
          do i = 1, n
            alpha(i) = alpha1(i)
          end do
        else if ( test == 2 ) then
          seed = 123456789
          call r8vec_uniform_01 ( n, seed, alpha )
        end if

        call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )

        call vand1 ( n, alpha, a )

        do i = 1, n
          x(i) = x1(i)
        end do
        call r8mat_mtv ( n, n, a, x, b )
        call r8vec_print ( n, b, '  Right hand side B:' )

        do i = 1, n
          x(i) = 0.0D+00
        end do

        do nsub = 1, n
          call dvandprg ( nsub, alpha, b, x, c, m )
          call r8vec_print ( nsub, x, '  Solution X:' )
        end do

      end do

      return
      end
      subroutine pvand_test ( )

c*********************************************************************72
c
cc PVAND_TEST tests PVAND.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision alpha(n)
      double precision alpha1(n)
      double precision b(n)
      integer seed
      integer test
      double precision x(n)
      double precision, dimension ( n ) :: x1(n)

      save alpha1
      save x1

      data alpha1 /
     &  0.0, 1.0, 2.0, 3.0, 4.0 /
      data x1 /
     &  5.0, 3.0, 4.0, 1.0, 2.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'PVAND_TEST:'
      write ( *, '(a)' ) '  Solve a Vandermonde linear system A*x=b'

      do test = 1, 2

        if ( test .eq. 1 ) then
          call r8vec_copy ( n, alpha1, alpha )
        else if ( test .eq. 2 ) then
          seed = 123456789
          call r8vec_uniform_01 ( n, seed, alpha )
        end if

        call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )

        call vand1 ( n, alpha, a )

        call r8vec_copy ( n, x1, x )
        call r8mat_mv ( n, n, a, x, b )
        call r8vec_print ( n, b, '  Right hand side B:' )

        call pvand ( n, alpha, b, x )
        call r8vec_print ( n, x, '  Solution X:' )

      end do

      return
      end
      subroutine pvandprg_test ( )

c*********************************************************************72
c
cc PVANDPRG_TEST tests PVANDPRG.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision alpha(n)
      double precision alpha1(n)
      double precision b(n)
      double precision c(n)
      integer i
      double precision m(n)
      integer nsub
      integer seed
      integer test
      double precision x(n)
      double precision x1(n)

      save alpha1
      save x1

      data alpha1 /
     &  0.0, 1.0, 2.0, 3.0, 4.0 /
      data x1 /
     &  5.0, 3.0, 4.0, 1.0, 2.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'PVANDPRG_TEST:'
      write ( *, '(a)' ) '  Solve a Vandermonde linear system A*x=b'
      write ( *, '(a)' ) '  progressively.'
      write ( *, '(a)' ) '  First we use ALPHA = 0, 1, 2, 3, 4.'
      write ( *, '(a)' ) '  Then we choose ALPHA at random.'

      do test = 1, 2

        if ( test .eq. 1 ) then
          do i = 1, n
            alpha(i) = alpha1(i)
          end do
        else if ( test .eq. 2 ) then
          seed = 123456789
          call r8vec_uniform_01 ( n, seed, alpha )
        end if

        call r8vec_print ( n, alpha, '  Vandermonde vector ALPHA:' )

        call vand1 ( n, alpha, a )

        do i = 1, n
          x(i) = x1(i)
        end do
        call r8mat_mv ( n, n, a, x, b )
        call r8vec_print ( n, b, '  Right hand side B:' )

        do i = 1, n
          x(i) = 0.0D+00
        end do

        do nsub = 1, n
          call pvandprg ( nsub, alpha, b, x, c, m )
          call r8vec_print ( nsub, x, '  Solution X:' )
        end do

      end do

      return
      end

