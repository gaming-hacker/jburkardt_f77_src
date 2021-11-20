      program main

c*********************************************************************72
c
cc MAIN is the main program for asa082_test.
c
c  Discussion:
c
c    asa082_test tests asa082.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2020
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'asa082_test'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  asa082 computes the determinant of '
      write ( *, '(a)' ) '  an orthogonal matrix.'

      call detq_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'asa082_test'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine detq_test ( )

c*********************************************************************72
c
cc detq_test tests detq.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2020
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nmax
      parameter ( nmax = 10 )

      double precision a(nmax*nmax)
      double precision d1
      double precision d2
      integer ifault
      integer n

      do n = 5, nmax
        call helmert_matrix ( n, a )
        write ( *, '(a)' ) ''
        write ( *, '(a,i4)' ) '  Helmert matrix of order ', n
        if ( .false. ) then
          call r8mat_print ( n, n, a, '  Helmert matrix:' )
        end if
        call helmert_determinant ( n, d1 )
        write ( *, '(a,g14.6)' ) '  determinant =      ', d1
        call detq ( a, n, d2, ifault )
        if ( ifault == 1 ) then
          write ( *, '(a)' ) '  DETQ failed for this case.'
        else
          write ( *, '(a,g14.6)' ) '  DETQ determinant = ', d2
        end if
      end do

      return
      end
      subroutine helmert_matrix ( n, a )

c*********************************************************************72
c
cc helmert_matrix returns the HELMERT matrix.
c
c  Formula:
c
c    If I = 1 then
c      A(I,J) = 1 / sqrt ( N )
c    else if J < I then
c      A(I,J) = 1 / sqrt ( I * ( I - 1 ) )
c    else if J = I then
c      A(I,J) = - sqrt (I-1) / sqrt ( I )
c    else
c      A(I,J) = 0
c
c  Discussion:
c
c    The matrix given above by Helmert is the classic example of
c    a family of matrices which are now called Helmertian or
c    Helmert matrices.
c
c    A matrix is a (standard) Helmert matrix if it is orthogonal,
c    and the elements which are above the diagonal and below the
c    first row are zero.
c
c    If the elements of the first row are all strictly positive,
c    then the matrix is a strictly Helmertian matrix.
c
c    It is possible to require in addition that all elements below
c    the diagonal be strictly positive, but in the reference, this
c    condition is discarded as being cumbersome and not useful.
c
c    A Helmert matrix can be regarded as a change of basis matrix
c    between a pair of orthonormal coordinate bases.  The first row
c    gives the coordinates of the first new basis vector in the old
c    basis.  Each later row describes combinations of (an increasingly
c    extensive set of) old basis vectors that constitute the new
c    basis vectors.
c
c    Helmert matrices have important applications in statistics.
c
c  Example:
c
c    N = 5
c
c    0.4472    0.4472    0.4472    0.4472    0.4472
c    0.7071   -0.7071         0         0         0
c    0.4082    0.4082   -0.8165         0         0
c    0.2887    0.2887    0.2887   -0.8660         0
c    0.2236    0.2236    0.2236    0.2236   -0.8944
c
c  Properties:
c
c    A is generally not symmetric: A' /= A.
c
c    A is orthogonal: A' * A = A * A' = I.
c
c    Because A is orthogonal, it is normal: A' * A = A * A'.
c
c    A is not symmetric: A' /= A.
c
c    det ( A ) = (-1)^(N+1)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    HO Lancaster,
c    The Helmert Matrices,
c    American Mathematical Monthly,
c    Volume 72, 1965, pages 4-12.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      integer i
      integer j
c
c  A begins with the first row, diagonal, and lower triangle set to 1.
c
      do i = 1, n
        do j = 1, n

          if ( i .eq. 1 ) then
            a(i,j) = 1.0D+00 / sqrt ( dble ( n ) )
          else if ( j .lt. i ) then
            a(i,j) = 1.0D+00 / sqrt ( dble ( i * ( i - 1 ) ) )
          else if ( i .eq. j ) then
            a(i,j) = - sqrt ( dble ( i - 1 ) ) 
     &               / sqrt ( dble ( i ) )
          else
            a(i,j) = 0.0D+00
          end if

        end do
      end do

      return
      end
      subroutine helmert_determinant ( n, determ )

c*********************************************************************72
c
cc HELMERT_DETERMINANT returns the determinant of the HELMERT matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Output, double precision DETERM, the determinant.
c
      implicit none

      double precision determ
      integer n

      if ( mod ( n, 2 ) .eq. 0 ) then
        determ = -1.0D+00
      else
        determ = +1.0D+00
      end if

      return
      end
