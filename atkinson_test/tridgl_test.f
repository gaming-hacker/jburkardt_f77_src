      program tridgl_test

c*********************************************************************72
c
cc tridgl_test tests tridgl.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 June 2019
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 100 )

      real a(n)
      real b(n)
      real c(n)
      real f(n)
      integer i
      integer ier
      integer iflag

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'tridgl_test'
      write ( *, '(a)' ) '  tridgl solves a tridiagonal linear system.'
      write ( *, '(a,i8)' ) '  The matrix size is N = ', n
      write ( *, '(a)' ) ' '
c
c  Set up the linear system, by storing the values of the
c  subdiagonal, diagonal, and superdiagonal in C, D, and E,
c  and the right hand side in B.
c
      a(1) = 0.0E+00
      do i = 2, n
        a(i) = -1.0E+00
      end do

      do i = 1, n
        b(i) = 2.0E+00
      end do

      do i = 1, n - 1
        c(i) = -1.0E+00
      end do
      c(n) = 0.0E+00

      do i = 1, n - 1
        f(i) = 0.0E+00
      end do
      f(n) = real ( n + 1 )
c
c  Factor and solve the system in one step.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Factor the matrix and solve the system.'

      iflag = 0
      call tridgl ( a, b, c, f, n, iflag, ier )

      if ( ier .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Error: ier = ', ier
        return
      end if
c
c  Print the results.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The first and last 5 entries of solution:'
      write ( *, '(a)' ) '  (Should be (1,2,3,4,5,...,n-1,n))'
      write ( *, '(a)' ) ' '

      do i = 1, n
        if ( i .le. 5 .or. n-5 .lt. i ) then
          write ( *, '(2x,i8,2x,g14.6)' ) i, f(i)
        end if
        if ( i .eq. 5 ) then
          write ( *, '(a)' ) '  ......  ..............'
        end if
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'tridgl_test:'
      write ( *, '(a)' ) '  Normal end of execution.'

      stop 0
      end
