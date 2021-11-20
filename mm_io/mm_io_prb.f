      program main

c*********************************************************************72
c
cc MAIN is the main program for MM_IO_PRB.
c
c  Discussion:
c
c    MM_IO_PRB tests the MM_IO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MM_IO_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the MM_IO library.'

      call test01 ( 'matrix_05_05_crg.txt' )
      call test02 ( 'matrix_05_05_crg.txt' )
      call test03 ( )
      call test04 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MM_IO_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( input_file )

c*********************************************************************72
c
cc TEST01 tests MM_HEADER_READ
c
c  Discussion:
c
c    The size information can be handled either by
c    MM_SIZE_READ_FILE (by backspacing once the comments have been read)
c    or by MM_SIZE_READ_STRING (by passing the most recently read line,
c    which is NOT a comment).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      character * ( 80 ) comment
      integer nnz
      character * ( 7 ) field
      character * ( 14 ) id
      character * ( * ) input_file
      integer input_unit
      integer ios
      integer ncol
      integer nrow
      character * ( 10 ) rep
      character * ( 19 ) symm
      character * ( 6 ) type

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) '  MM_HEADER_READ reads the header line of'
      write ( *, '(a)' ) '  a Matrix Market file.'
      write ( *, '(a)' ) '  MM_SIZE_READ_FILE or MM_SIZE_READ_STRING'
      write ( *, '(a)' ) 
     &  '  reads the size line of a Matrix Market file.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Reading "' // trim ( input_file ) // '".'

      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_file, status = 'old', 
     &  iostat = ios )

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST01 - Fatal error!'
        write ( *, '(a)' ) '  Could not open the input file.'
        stop 1
      end if

      call mm_header_read ( input_unit, id, type, rep, field, symm )

      call mm_header_check ( id, type, rep, field, symm )

      call mm_header_print ( input_file, id, type, rep, field, symm )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Comment lines:'
      write ( *, '(a)' ) ' '

10    continue

        call mm_comment_read ( input_unit, comment )

        if ( comment(1:1) .ne. '%' ) then

          go to 20

        end if

        call mm_comment_print ( comment )

      go to 10

20    continue

      if ( .true. ) then
        call mm_size_read_string ( comment, rep, symm, nrow, ncol, nnz )
      else
        call mm_size_read_file ( input_unit, rep, symm, nrow, ncol, 
     &    nnz )
      end if

      call mm_size_print ( input_file, rep, symm, nrow, ncol, nnz )

      close ( unit = input_unit )

      return
      end
      subroutine test02 ( input_file )

c*********************************************************************72
c
cc TEST02 tests MM_FILE_READ
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnzmax
      parameter ( nnzmax = 100 )

      complex cval(nnzmax)
      double precision dval(nnzmax)
      integer nnz
      character * ( 7 ) field
      character * ( 14 ) id
      integer ihi
      integer ilo
      integer indx(nnzmax)
      character * ( * ) input_file
      integer input_unit
      integer ios
      integer ival(nnzmax)
      integer jndx(nnzmax)
      integer ncol
      integer nrow
      character * ( 10 ) rep
      real rval(nnzmax)
      character * ( 19 ) symm
      character * ( 6 ) type

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) '  MM_FILE_READ reads a Matrix Market file.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Reading "' // trim ( input_file ) // '".'

      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_file, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST02 - Fatal errorc'
        write ( *, '(a)' ) '  Could not open the input file.'
        stop
      end if

      call mm_file_read ( input_unit, id, type, rep, field, symm, nrow, 
     &  ncol, nnz, nnzmax, indx, jndx, ival, rval, dval, cval )

      close ( unit = input_unit )

      call mm_header_print ( input_file, id, type, rep, field, symm )

      call mm_size_print ( input_file, rep, symm, nrow, ncol, nnz )

      ilo = 1
      ihi = 5

      call mm_values_print_some ( rep, field, nnz, indx, jndx, ival, 
     &  rval, dval, cval, ilo, ihi )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests MM_FILE_WRITE
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nnz
      parameter ( nnz = 10 )

      complex cval(nnz)
      double precision dval(nnz)
      character * ( 7 ) field
      character * ( 14 ) id
      integer indx(nnz)
      integer ios
      integer ival(nnz)
      integer jndx(nnz)
      integer ncol
      integer nrow
      character * ( 80 ) output_file
      integer output_unit
      character * ( 10 ) rep
      real rval(nnz) 
      character * ( 19 ) symm
      character * ( 6 ) type

      save indx
      save jndx
      save rval

      data indx /
     &  1, 1, 2, 2, 3, 3, 4, 5, 5, 5 /
      data jndx /
     &  1, 5, 3, 4, 2, 5, 1, 2, 4, 5 /
      data rval /
     &  11.0E+00, 15.0E+00, 23.0E+00, 24.0E+00, 32.0E+00, 
     &  35.0E+00, 41.0E+00, 52.0E+00, 54.0E+00, 55.0E+00 /

      field = 'real'
      id = '%%MatrixMarket'
      ncol = 5
      nrow = 5
      output_file = 'matrix_05_05_crg.txt'
      rep = 'coordinate'
      symm = 'general'
      type = 'matrix'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) '  MM_FILE_WRITE writes the header and data of'
      write ( *, '(a)' ) '  a Matrix Market file.'

      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_file, status = 'replace', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST03 - Fatal errorc'
        write ( *, '(a)' ) '  Could not open the output file.'
        stop
      end if

      call mm_file_write ( output_unit, id, type, rep, field, symm, 
     &  nrow, ncol, nnz, indx, jndx, ival, rval, dval, cval )

      close ( unit = output_unit )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests MM_FILE_WRITE
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nmax
      parameter ( nmax = 3000 )

      complex cval(1)
      double precision dval(nmax,nmax)
      character * ( 7 ) field
      character * ( 14 ) id
      integer indx(1)
      integer ios
      integer ival(1)
      integer jndx(1)
      integer n
      integer ncol
      integer nnz
      integer nrow
      integer nx
      integer ny
      character * ( 80 ) output_file
      integer output_unit
      character * ( 10 ) rep
      real rval(1)
      character * ( 19 ) symm
      character * ( 6 ) type

      field = 'double'
      id = '%%MatrixMarket'
      nx = 3
      ny = 2
      output_file = 'wathen_29_29_adg.txt'
      rep = 'array'
      symm = 'general'
      type = 'matrix'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04:'
      write ( *, '(a)' ) '  MM_FILE_WRITE writes the header and data of'
      write ( *, '(a)' ) '  a Matrix Market file.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  In this example, we generate the Wathen matrix'
      write ( *, '(a)' ) 
     &  '  of order 29 x 29, and store it (inefficiently)'
      write ( *, '(a)' ) '  as an array.'
c
c  Generate the WATHEN matrix.
c
      call wathen_size ( nx, ny, n )

      call wathen ( nx, ny, n, dval )
c
c  Write the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_file, status = 'replace',
     &  iostat = ios )

      if ( ios /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST04 - Fatal error!'
        write ( *, '(a)' ) '  Could not open the output file.'
        stop 1
      end if

      nrow = n
      ncol = n
      nnz = nrow * ncol

      call mm_file_write ( output_unit, id, type, rep, field, symm, 
     &  nrow, ncol, nnz, indx, jndx, ival, rval, dval, cval )

      close ( unit = output_unit )

      return
      end
      subroutine wathen ( nx, ny, n, a )

c*********************************************************************72
c
cc WATHEN returns a finite element matrix.
c
c  Discussion:
c
c    A is the consistent mass matrix for a regular NX by NY grid
c    of 8 node serendipity elements.  Here is an illustration
c    for NX = 3, NX = 2:
c
c     23-24-25-26-27-28-29
c      |     |     |     |
c     19    20    21    22
c      |     |     |     |
c     12-13-14-15-16-17-18
c      |     |     |     |
c      8     9    10    11
c      |     |     |     |
c      1--2--3--4--5--6--7
c
c    For this example, the total number of nodes is, as expected,
c
c      N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
c
c  Properties:
c
c    A is symmetric positive definite for any positive values of the
c    density RHO(NX,NY), which is here given the value 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 March 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    A J Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size of A.
c
c    Input, integer N, the order of the matrix, as determined
c    by NX and NY.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n
      integer nx
      integer ny

      double precision a(n,n)
      double precision em(8,8)
      integer i
      integer j
      integer kcol
      integer krow
      integer node(8)
      double precision rho

      save em

      data em /
     &   6.0, -6.0,  2.0, -8.0,  3.0, -8.0,  2.0, -6.0, 
     &  -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, 
     &   2.0, -6.0,  6.0, -6.0,  2.0, -8.0,  3.0, -8.0, 
     &  -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, 
     &   3.0, -8.0,  2.0, -6.0,  6.0, -6.0,  2.0, -8.0, 
     &  -8.0, 16.0, -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, 
     &   2.0, -8.0,  3.0, -8.0,  2.0, -6.0,  6.0, -6.0, 
     &  -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, -6.0, 32.0 /

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      do j = 1, ny

        do i = 1, nx
c
c  For the element (I,J), determine the indices of the 8 nodes.
c
          node(1) = 3 * j * nx + 2 * j + 2 * i + 1
          node(2) = node(1) - 1
          node(3) = node(1) - 2

          node(4) = ( 3 * j - 1 ) * nx + 2 * j + i - 1
          node(8) = node(4) + 1

          node(5) = ( 3 * j - 3 ) * nx + 2 * j + 2 * i - 3
          node(6) = node(5) + 1
          node(7) = node(5) + 2
c
c  The density RHO can also be set to a random positive value.
c
          rho = 1.0D+00

          do krow = 1, 8
            do kcol = 1, 8

              if ( node(krow) .lt. 1 .or. n .lt. node(krow) ) then
                write ( *, '(a)' ) ' '
                write ( *, '(a)' ) 'WATHEN - Fatal error!'
                write ( *, '(a)' ) '  Index NODE(KROW) out of bounds.'
                write ( *, '(a)' ) '  I = ', i
                write ( *, '(a)' ) '  J = ', j
                write ( *, '(a)' ) '  KROW = ', krow
                write ( *, '(a)' ) '  NODE(KROW) = ', node(krow)
                return
              else if ( node(kcol) .lt. 1 .or. n .lt. node(kcol) ) then
                write ( *, '(a)' ) ' '
                write ( *, '(a)' ) 'WATHEN - Fatal error!'
                write ( *, '(a)' ) '  Index NODE(KCOL) out of bounds.'
                write ( *, '(a)' ) '  I = ', i
                write ( *, '(a)' ) '  J = ', j
                write ( *, '(a)' ) '  KCOL = ', kcol
                write ( *, '(a)' ) '  NODE(KCOL) = ', node(kcol)
                return
              end if

              a(node(krow),node(kcol)) = a(node(krow),node(kcol)) 
     &          + 20.0D+00 * rho * em(krow,kcol) / 9.0D+00

            end do
          end do

        end do
      end do

      return
      end
      subroutine wathen_size ( nx, ny, n )

c*********************************************************************72
c
cc WATHEN_SIZE returns the size of a finite element matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    A J Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size of A.
c
c    Output, integer N, the order of the matrix, as determined
c    by NX and NY.
c
      implicit none

      integer n
      integer nx
      integer ny

      n = 3 * nx * ny + 2 * nx + 2 * ny + 1

      return
      end
