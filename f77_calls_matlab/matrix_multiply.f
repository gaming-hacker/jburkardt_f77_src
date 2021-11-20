      program main

c*********************************************************************72
c
cc MAIN is the main program for the matrix multiplication example.
c
c  Discussion:
c
c    This program is part of a demonstration of how a FORTRAN77
c    program can interact with MATLAB.
c
c    In this example, the FORTRAN77 program generates matrices A
c    and B, writes them to a file, asks MATLAB to multiply them,
c    and reads the result back from another file.
c
c    The interaction with MATLAB is done using the (nonstandard)
c    SYSTEM call, which is available with certain FORTRAN compilers,
c    including GNU G95 and IBM XLF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision a(n,n)
      character * ( 255 ) a_file_name
      double precision alpha
      double precision b(n,n)
      character * ( 255 ) b_file_name
      double precision beta
      double precision c(n,n)
      character * ( 255 ) c_file_name
      character * ( 255 ) command
      double precision error_frobenius
      integer result

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MATRIX_MULTIPLY:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  This program:'
      write ( *, '(a)' ) '  * generates matrices A and B;'
      write ( *, '(a)' ) '  * writes them to files;'
      write ( *, '(a)' ) '  * calls MATLAB (via the SYSTEM routine) to'
      write ( *, '(a)' ) '    ** read A and B from files;'
      write ( *, '(a)' ) '    ** compute C = A * B;'
      write ( *, '(a)' ) '    ** write C to a file;'
      write ( *, '(a)' ) '  * reads C from the file;'
      write ( *, '(a)' ) '  * reports the success of the computation.'
c
c  Matrix generation.
c
      alpha = 2.0D+00
      beta = 3.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Matrix order N = ', n
      write ( *, '(a,g14.6)' ) '  Matrix parameter ALPHA = ', alpha
      write ( *, '(a,g14.6)' ) '  Matrix parameter BETA =  ', beta

      call combin ( alpha, beta, n, a )
      call combin_inverse ( alpha, beta, n, b )
c
c  Write the matrices to files.
c
      a_file_name = 'a.txt'
      call r8mat_write ( a_file_name, n, n, a )

      b_file_name = 'b.txt'
      call r8mat_write ( b_file_name, n, n, b )
c
c  Call MATLAB to multiply the matrices.
c
      command = '/usr/local/bin/matlab -nosplash -nodisplay ' // 
     &  '< matrix_multiply.m > matrix_multiply_matlab_output.txt'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Issued call to MATLAB.'

      call system ( command, result )

      if ( result /= 0 ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MATRIX_MULTIPLY:'
        write ( *, '(a)' ) '  Abnormal end of execution.'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Failure of command issued through SYSTEM.'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The command was:'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  "' // trim ( command ) // '".'
        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Error return from command = ', result
        write ( *, '(a)' ) ' '
        call timestamp ( )
        stop

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  MATLAB sesion terminated normally.'
c
c  Read the result matrix from the file.
c
      c_file_name = 'c.txt'

      call r8mat_read ( c_file_name, n, n, c )
c
c  Determine ||A*B-I|| which should be zero.
c
      call r8mat_is_identity ( n, c, error_frobenius )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 
     &  '  Frobenius norm of A * B - I is ', error_frobenius
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MATRIX_MULTIPLY:'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine combin ( alpha, beta, n, a )

c*********************************************************************72
c
cc COMBIN returns the COMBIN matrix.
c
c  Discussion:
c
c    This matrix is known as the combinatorial matrix.
c
c  Formula:
c
c    If ( I = J ) then
c      A(I,J) = ALPHA + BETA
c    else
c      A(I,J) = BETA
c
c  Example:
c
c    N = 5, ALPHA = 2, BETA = 3
c
c    5 3 3 3 3
c    3 5 3 3 3
c    3 3 5 3 3
c    3 3 3 5 3
c    3 3 3 3 5
c
c  Properties:
c
c    A is symmetric: A' = A.
c
c    Because A is symmetric, it is normal.
c
c    Because A is normal, it is diagonalizable.
c
c    A is persymmetric: A(I,J) = A(N+1-J,N+1-I).
c
c    A is a circulant matrix: each row is shifted once to get the next row.
c
c    det ( A ) = ALPHA^(N-1) * ( ALPHA + N * BETA ).
c
c    A has constant row sums.
c
c    Because A has constant row sums,
c    it has an eigenvalue with this value,
c    and a (right) eigenvector of ( 1, 1, 1, ..., 1 ).
c
c    A has constant column sums.
c
c    Because A has constant column sums,
c    it has an eigenvalue with this value,
c    and a (left) eigenvector of ( 1, 1, 1, ..., 1 ).
c
c    LAMBDA(1:N-1) = ALPHA,
c    LAMBDA(N) = ALPHA + N * BETA.
c
c    The eigenvector associated with LAMBDA(N) is (1,1,1,...,1)/sqrt(N).
c
c    The other N-1 eigenvectors are simply any (orthonormal) basis
c    for the space perpendicular to (1,1,1,...,1).
c
c    A is nonsingular if ALPHA /= 0 and ALPHA + N * BETA /= 0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Robert Gregory, David Karney,
c    A Collection of Matrices for Testing Computational Algorithms,
c    Wiley, 1969,
c    ISBN: 0882756494,
c    LC: QA263.68
c
c    Donald Knuth,
c    The Art of Computer Programming,
c    Volume 1, Fundamental Algorithms, Second Edition,
c    Addison-Wesley, Reading, Massachusetts, 1973, page 36.
c
c  Parameters:
c
c    Input, double precision ALPHA, BETA, scalars that define A.
c
c    Input, integer N, the order of the matrix.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision alpha
      double precision beta
      integer i
      integer j

      do j = 1, n
        do i = 1, n
          a(i,j) = beta
        end do
      end do

      do i = 1, n
        a(i,i) = a(i,i) + alpha
      end do

      return
      end
      subroutine combin_inverse ( alpha, beta, n, a )

c*********************************************************************72
c
cc COMBIN_INVERSE returns the inverse of the COMBIN matrix.
c
c  Formula:
c
c    if ( I = J )
c      A(I,J) = (ALPHA+(N-1)*BETA) / (ALPHA*(ALPHA+N*BETA))
c    else
c      A(I,J) =             - BETA / (ALPHA*(ALPHA+N*BETA))
c
c  Example:
c
c    N = 5, ALPHA = 2, BETA = 3
c
c           14 -3 -3 -3 -3
c           -3 14 -3 -3 -3
c   1/34 *  -3 -3 14 -3 -3
c           -3 -3 -3 14 -3
c           -3 -3 -3 -3 14
c
c  Properties:
c
c    A is symmetric: A' = A.
c
c    Because A is symmetric, it is normal.
c
c    Because A is normal, it is diagonalizable.
c
c    A is persymmetric: A(I,J) = A(N+1-J,N+1-I).
c
c    A is a circulant matrix: each row is shifted once to get the next row.
c
c    A is Toeplitz: constant along diagonals.
c
c    det ( A ) = 1 / (ALPHA^(N-1) * (ALPHA+N*BETA)).
c
c    A is well defined if ALPHA /= 0D+00 and ALPHA+N*BETA /= 0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Donald Knuth,
c    The Art of Computer Programming,
c    Volume 1, Fundamental Algorithms, Second Edition,
c    Addison-Wesley, Reading, Massachusetts, 1973, page 36.
c
c  Parameters:
c
c    Input, double precision ALPHA, BETA, scalars that define the matrix.
c
c    Input, integer N, the order of the matrix.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision alpha
      double precision beta
      double precision bot
      integer i
      integer j

      if ( alpha .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'COMBIN_INVERSE - Fatal error!'
        write ( *, '(a)' ) '  The entries of the matrix are undefined'
        write ( *, '(a)' ) '  because ALPHA = 0.'
        stop
      else if ( alpha + n * beta .eq. 0.0D+00 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'COMBIN_INVERSE - Fatal error!'
        write ( *, '(a)' ) '  The entries of the matrix are undefined'
        write ( *, '(a)' ) '  because ALPHA+N*BETA is zero.'
        stop
      end if

      bot = alpha * ( alpha + dble ( n ) * beta )

      do j = 1, n
        do i = 1, n

          if ( i .eq. j ) then
            a(i,j) = ( alpha + dble ( n - 1 ) * beta ) / bot
          else
            a(i,j) = - beta / bot
          end if

        end do
      end do

      return
      end
      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      subroutine r8mat_is_identity ( n, a, error_frobenius )

c*********************************************************************72
c
cc R8MAT_IS_IDENTITY determines if an R8MAT is the identity.
c
c  Discussion:
c
c    An R8MAT is a matrix of double precision values.
c
c    The routine returns the Frobenius norm of A - I.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(N,N), the matrix.
c
c    Output, rdouble precision ERROR_FROBENIUS, the Frobenius norm
c    of the difference matrix A - I, which would be exactly zero
c    if A were the identity matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision error_frobenius
      integer i
      integer j
      double precision value

      error_frobenius = 0.0D+00

      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            error_frobenius = error_frobenius + ( a(i,j) - 1.0D+00 )**2
          else
            error_frobenius = error_frobenius + a(i,j)**2
          end if
        end do 
      end do

      error_frobenius = sqrt ( error_frobenius )

      return
      end
      subroutine r8mat_read ( file_name, m, n, a )

c*********************************************************************72
c
cc R8MAT_READ reads an R8MAT from a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILE_NAME, the name of the file.
c
c    Input, integer M, N, the order of the matrix.
c
c    Output, double precision A(M,N), the matrix.
c
      implicit none

      integer ( kind = 4 ) m
      integer ( kind = 4 ) n

      real ( kind = 8 ) a(m,n)
      character ( len = * ) file_name
      integer ( kind = 4 ) file_unit
      integer ( kind = 4 ) i

      call get_unit ( file_unit )

      open ( unit = file_unit, file = file_name, status = 'old' )

      do i = 1, m
        read ( file_unit, * ) a(i,1:n)
      end do

      close ( unit = file_unit )

      return
      end
      subroutine r8mat_write ( output_filename, m, n, table )

c*********************************************************************72
c
cc R8MAT_WRITE writes a R8MAT file.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 October 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision TABLE(M,N), the data.
c
      implicit none

      integer m
      integer n

      integer i
      character * ( * ) output_filename
      integer output_unit
      character * ( 30 ) string
      double precision table(m,n)
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename,
     &  status = 'replace' )
c
c  Create the format string.
c
      if ( 0 .lt. m .and. 0 .lt. n ) then

        write ( string, '(a1,i8,a1,i8,a1,i8,a1)' )
     &    '(', n, 'g', 24, '.', 16, ')'
c
c  Write the data.
c
        do i = 1, m
          write ( output_unit, string ) table(i,1:n)
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ', 
     &  'May      ', 'June     ', 'July     ', 'August   ', 
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *, 
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) 
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
