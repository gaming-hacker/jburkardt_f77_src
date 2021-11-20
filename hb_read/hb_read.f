      subroutine c4_hb_data_read ( input, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt, 
     &  colptr, rowind, values, rhsval )

c*********************************************************************72
c
cc C4_HB_DATA_READ reads the data for a C4 Harwell-Boeing file.
c
c  Discussion:
c
c    It is assumed that C4_HB_HEADER_READ has already been called, and that
c    the file is still open.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, character * ( 3 ) RHSTYP, HB stuff.
c
c    Input, integer NRHS, the number of right hand sides.
c
c    Input, integer NRHSIX, HB stuff.
c
c    Input, integer VALCRD, the number of input lines of values.
c
c    Input, integer RHSCRD, the number of input lines of
c    right hand side data.
c
c    Input, character * ( 16 ) PTRFMT, the format to read column pointer data.
c
c    Input, character * ( 16 ) INDFMT, the format to read row index data.
c
c    Input, character * ( 20 ) VALFMT, the format to read matrix value data.
c
c    Input, character * ( 20 ) RHSFMT, the format to read right hand side data.
c
c    Output, complex VALUES(NNZERO), the nonzero values.
c
c    Output, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Output, integer COLPTR(NCOL+1), compressed column indices.
c
c    Output, complex RHSVAL(NROW*NRHS), HB stuff.
c
      implicit none

      integer ncol
      integer nnzero
      integer nrhs
      integer nrow

      integer colptr(ncol+1)
      integer i
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer neltvl
      integer nrhsix
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      complex rhsval(nrow*nrhs)
      integer rowind(nnzero)
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt
      complex values(nnzero)
c
c  Read matrix structure.
c
      read ( input, ptrfmt ) colptr(1:ncol+1)
      read ( input, indfmt ) rowind(1:nnzero)
c
c  Read matrix values.
c
      if ( 0 .lt. valcrd ) then
        read ( input, valfmt ) values(1:nnzero)
      end if
c
c  Read right hand side values.
c
      if ( 0 .lt. rhscrd ) then

        if ( rhstyp(1:1) .eq. 'F' .or. rhstyp(1:1) == 'f' ) then
          read ( input, rhsfmt ) rhsval(1:nrow*nrhs)
        else
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'C4_HB_DATA_READ - Fatal error!'
          write ( *, '(a)' ) '  Unsupported RHS input type.'
          stop 1
        end if

      end if

      return
      end
      subroutine c4_hb_header_print ( filename, nrow, ncol, nnzero, 
     &  rhstyp, nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, 
     &  rhsfmt )

c*********************************************************************72
c
cc C4_HB_HEADER_PRINT prints the header for a C4 Harwell-Boeing file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILENAME, the name of the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, character * ( 3 ) RHSTYP, HB stuff.
c
c    Input, integer NRHS, the number of right hand sides.
c
c    Input, integer NRHSIX, HB stuff.
c
c    Input, integer VALCRD, number of lines of VALUE data.
c
c    Input, integer RHSCRD, number of lines of RHS data.
c
c    Input, character * ( 16 ) PTRFMT, the format for reading column
c    pointers.
c
c    Input, character * ( 16 ) INDFMT, the format for reading row indices.
c
c    Input, character * ( 20 ) VALFMT, the format for reading VALUE data.
c
c    Input, character * ( 20 ) RHSFMT, the format for reading RHS data.
c
      implicit none

      character * ( * ) filename
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Harwell Boeing data read from "' // trim ( filename ) // '"'
      write ( *, '(a)' ) ''
      write ( *, '(a,i8)' ) 
     &  '  Matrix rows                NROW =   ', nrow
      write ( *, '(a,i8)' ) 
     &  '  Matrix columns             NCOL =   ', ncol
      write ( *, '(a,i8)' ) 
     &  '  Matrix nonzeros            NNZERO = ', nnzero
      write ( *, '(a,i8)' ) 
     &  '  Right hand sides           NRHS =   ', nrhs
      write ( *, '(a,i8)' ) 
     &  '                             NRHSIX = ', nrhsix
      write ( *, '(a,i8)' ) 
     &  '  Number of VALUE lines      VALCRD = ', valcrd
      write ( *, '(a,i8)' ) 
     &  '  Number of RHS lines        RHSCRD = ', rhscrd
      write ( *, '(a)'    ) 
     &  '  RHS descriptor             RHSTYP = "' // rhstyp // '"'
      write ( *, '(a)'    ) 
     &  '  Format for column pointers PTRFMT = "' // 
     &  trim ( ptrfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for row indices     INDFMT = "' // 
     &  trim ( indfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for VALUE data      VALFMT = "' // 
     &  trim ( valfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for RHS data        RHSFMT = "' // 
     &  trim ( rhsfmt ) // '"'

      return
      end
      subroutine c4_hb_header_read ( input, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )

c*********************************************************************72
c
cc C4_HB_HEADER_READ reads the header for a C4 Harwell-Boeing file.
c
c  Discussion:
c
c    This function only works for a particular kind of HB file.
c
c    It is assumed that the matrix is complex.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Output, integer NROW, the number of rows.
c
c    Output, integer NCOL, the number of columns.
c
c    Output, integer NNZERO, the number of nonzeros.
c
c    Output, character * ( 3 ) RHSTYP, HB stuff.
c
c    Output, integer NRHS, HB stuff
c
c    Output, integer NRHSIX, HB stuff.
c
c    Output, integer VALCRD, the number of input lines of values.
c
c    Output, integer RHSCRD, the number of input lines of
c    right hand side data.
c
c    Output, character * ( 16 ) PTRFMT, the format to read column pointer data.
c
c    Output, character * ( 16 ) INDFMT, the format to read row index data.
c
c    Output, character * ( 20 ) VALFMT, the format to read matrix value data.
c
c    Output, character * ( 20 ) RHSFMT, the format to read right hand side data.
c
      implicit none

      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt

      read ( input, '(a72, a8 )' ) title, key

      read ( input, '(5i14)' ) totcrd, ptrcrd, indcrd, valcrd, rhscrd

      read ( input, '(a3, 11x, 4i14)' ) 
     &  mxtype, nrow, ncol, nnzero, neltvl

      read ( input, '(a16, a16, a20, a20)' ) 
     &  ptrfmt, indfmt, valfmt, rhsfmt

      if ( 0 .lt. rhscrd ) then
        read ( input, '(a3,11x,i14,i14)' ) rhstyp, nrhs, nrhsix
      else
        rhstyp = '***'
        nrhs = 0
        nrhsix = 0
      end if

      return
      end
      subroutine c4_hb_quick_print ( filename, nrow, ncol, nnzero, 
     &  values, rowind, colptr )

c*********************************************************************72
c
cc C4_HB_QUICK_PRINT prints a sparse matrix in C4 Harwell-Boeing format.
c
c  Discussion:
c
c    This function is designed to print the information that was read by
c    the simplified Harwell-Boeing reader C4_HB_QUICK_READ.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILENAME, the name of the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, complex VALUES(NNZERO), the nonzero values.
c
c    Input, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Input, integer COLPTR(NCOL+1), compressed column indices.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      character * ( * ) filename
      integer nrow
      integer rowind(nnzero)
      complex values(nnzero)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Harwell Boeing data read from "' // trim ( filename ) // '"'
      write ( *, '(a)' ) ''
      write ( *, '(a,i8)' ) 
     &  '  Matrix rows                NROW =   ', nrow
      write ( *, '(a,i8)' ) 
     &  '  Matrix columns             NCOL =   ', ncol
      write ( *, '(a,i8)' ) 
     &  '  Matrix nonzeros            NNZERO = ', nnzero

      call c4_hb_structure_print ( ncol, nnzero, colptr, rowind )

      call c4_hb_values_print ( ncol, colptr, nnzero, values )

      return
      end
      subroutine c4_hb_quick_read ( input, nrow, ncol, nnzero, values, 
     &  rowind, colptr )

c*********************************************************************72
c
cc C4_HB_QUICK_READ reads a sparse matrix in C4 Harwell-Boeing format.
c
c  Discussion:
c
c    The Harwell-Boeing file format for sparse matrix storage is complicated,
c    and has many options.
c
c    This routine can only handle a particular simple case of the format,
c    in which the file contains a "CUA" matrix, complex, unsymmetric, assembled.
c
c    While the file may contain additional information, this routine cannot
c    retrieve it.
c
c    The user must allocate at least enough space for VALUES, ROWIND, and 
c    COLPTR before calling this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Output, integer NROW, the number of rows.
c
c    Output, integer NCOL, the number of columns.
c
c    Output, integer NNZERO, the number of nonzeros.
c
c    Output, complex VALUES(NNZERO), the nonzero values.
c
c    Output, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Output, integer COLPTR(NCOL+1), compressed column indices.
c
      implicit none

      integer colptr(*)
      integer i
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      integer rowind(*)
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt
      complex values(*)
c
c  Read header.
c
      read ( input, 
     &  '(a72, a8 / 5i14 / a3, 11x, 4i14 / a16, a16, a20, a20 )' ) 
     &  title, key, 
     &  totcrd, ptrcrd, indcrd, valcrd, rhscrd, 
     &  mxtype, nrow, ncol, nnzero, neltvl, 
     &  ptrfmt, indfmt, valfmt, rhsfmt
c
c  Read matrix structure.
c
      read ( input, ptrfmt ) colptr(1:ncol+1)
      read ( input, indfmt ) rowind(1:nnzero)
c
c  Read matrix values.
c
      if ( 0 .lt. valcrd ) then
        read ( input, valfmt ) values(1:nnzero)
      end if

      return
      end
      subroutine c4_hb_structure_print ( ncol, nnzero, colptr, rowind )

c*********************************************************************72
c
cc C4_HB_STRUCTURE_PRINT prints the structure of a C4 Harwell-Boeing matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Iain Duff, Roger Grimes, John Lewis,
c    User's Guide for the Harwell-Boeing Sparse Matrix Collection,
c    Technical Report TR/PA/92/86, CERFACS,
c    October 1992.
c
c  Parameters:
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeroes.
c
c    Input, integer COLPTR(NCOL+1), COLPTR(I) points to the
c    location of the first entry of column I in the sparse matrix structure.
c
c    Input, integer ROWIND(NNZERO), the row index of each item.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      integer j
      integer khi
      integer klo
      integer rowind(nnzero)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Column   Begin     End   ' // 
     &  '------------------------------------------------------------'
      write ( *, '(a)' ) ' '

      do j = 1, ncol

        if ( 5 .lt. j .and. j .lt. ncol ) then
          go to 10
        end if

        if ( j .eq. ncol .and. 6 .lt. ncol ) then
          write ( *, '(a)' ) '  (Skipping intermediate columns...)'
        end if

        if ( colptr(j+1)-1 .lt. colptr(j) ) then

          write ( *, '(i8,3x,a)' ) j, 'EMPTY'

        else

          do klo = colptr(j), colptr(j+1)-1, 5
            khi = min ( klo + 4, colptr(j+1)-1 )
            if ( klo .eq. colptr(j) ) then
              write ( *, '(3i8,3x,5i6)' ) 
     &          j, colptr(j), colptr(j+1)-1, rowind(klo:khi)
            else
              write ( *, '(27x,5i6)' ) rowind(klo:khi)
            end if
          end do

        end if

10      continue

      end do

      write ( *, '(a)' ) 
     &  '                           ' // 
     &  '------------------------------------------------------------'

      return
      end
      subroutine c4_hb_values_print ( ncol, colptr, nnzero, values )

c*********************************************************************72
c
cc C4_HB_VALUES_PRINT prints the values of a C4 Harwell-Boeing matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Iain Duff, Roger Grimes, John Lewis,
c    User's Guide for the Harwell-Boeing Sparse Matrix Collection,
c    Technical Report TR/PA/92/86, CERFACS,
c    October 1992.
c
c  Parameters:
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer COLPTR(NCOL+1), COLPTR(I) points to the
c    location of the first entry of column I in the sparse matrix structure.
c
c    Input, integer NNZERO, the number of nonzeroes.
c
c    Input, complex VALUES(NNZERO), the nonzero values of
c    the matrix.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      integer j
      integer k
      integer khi
      integer klo
      complex values(nnzero)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Column   Begin     End   ' // 
     &  '--------------------------------------------------------'
      write ( *, '(a)' ) ' '

      do j = 1, ncol

        if ( 5 .lt. j .and. j .lt. ncol ) then
          go to 10
        end if

        if ( j .eq. ncol .and. 6 .lt. ncol ) then
          write ( *, '(a)' ) '  (Skipping intermediate columns...)'
        end if

        if ( colptr(j+1)-1 .lt. colptr(j) ) then

          write ( *, '(i8)' ) j, '   EMPTY'

        else

          do klo = colptr(j), colptr(j+1)-1, 2
            khi = min ( klo + 1, colptr(j+1)-1 )
            if ( klo .eq. colptr(j) ) then
              write ( *, '(2x,i6,2x,i6,2x,i6,3x)', advance = 'no' ) 
     &          j, colptr(j), colptr(j+1)-1
            else
              write ( *, '(27x)', advance = 'no' )
            end if
            do k = klo, khi
              write ( *, 
     &          '(''('',g12.4,'','',g12.4,'')'')', advance = 'no' ) 
     &          values(k)
            end do
          write ( *, '(a)' ) ''
          end do


        end if

10      continue

      end do

      write ( *, '(a)' ) 
     &  '                           ' // 
     &  '--------------------------------------------------------'

      return
      end
      subroutine c4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, 
     &  title )

c*********************************************************************72
c
cc C4MAT_PRINT_SOME prints some of a C4MAT.
c
c  Discussion:
c
c    A C4MAT is a matrix of C4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns 
c    in the matrix.
c
c    Input, complex A(M,N), the matrix.
c
c    Input, integer ILO, JLO, IHI, JHI, the first row and
c    column, and the last row and column to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 4 )
      integer m
      integer n

      complex a(m,n)
      character * ( 20 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title
      complex zero

      zero = cmplx ( 0.0E+00, 0.0E+00 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)' 
        return
      end if
c
c  Print the columns of the matrix, in strips of INCX.
c
      do j2lo = jlo, min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i10,10x)' ) j
        end do

        write ( *, '(a,4a20)' ) '  Col: ', ( ctemp(j2), j2 = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) '  ---'
c
c  Determine the range of the rows in this strip.
c
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi
c
c  Print out (up to) INCX entries in row I, that lie in the current strip.
c
          do j2 = 1, inc

            j = j2lo - 1 + j2

            if ( aimag ( a(i,j) ) .eq. 0.0E+00 ) then
              write ( ctemp(j2), '(g10.3,10x)' ) real ( a(i,j) )
            else
              write ( ctemp(j2), '(2g10.3)' ) a(i,j)
            end if

          end do

          write ( *, '(i5,a1,4a20)' ) i, ':', ( ctemp(j2), j2 = 1, inc )

        end do

      end do

      return
      end
      subroutine c8_hb_read ( nrow, ncol, nnzero, values, rowind, 
     &  colptr )

c*********************************************************************72
c
cc C8_HB_READ reads a sparse matrix in C8 Harwell-Boeing format.
c
c  Discussion:
c
c    The Harwell-Boeing file format for sparse matrix storage is complicated,
c    and has many options.
c
c    This routine can only handle a particular simple case of the format,
c    in which the file contains a "CUA" matrix, complex, unsymmetric, assembled.
c
c    While the file may contain additional information, this routine cannot
c    retrieve it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer NROW, the number of rows.
c
c    Output, integer NCOL, the number of columns.
c
c    Output, integer NNZERO, the number of nonzeros.
c
c    Output, double complex VALUES(NNZERO), the nonzero values.
c
c    Output, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Output, integer COLPTR(NCOL+1), compressed column indices.
c
      implicit none

      integer colptr(*)
      integer i
      integer indcrd
      character * ( 16 ) indfmt
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      integer rowind(*)
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt
      double complex values(*)
c
c  Read header.
c
      read ( *, 
     &  '(a72, a8 / 5i14 / a3, 11x, 4i14 / a16, a16, a20, a20 )' ) 
     &  title, key, 
     &  totcrd, ptrcrd, indcrd, valcrd, rhscrd, 
     &  mxtype, nrow, ncol, nnzero, neltvl, 
     &  ptrfmt, indfmt, valfmt, rhsfmt
c
c  Read matrix structure.
c
      read ( *, ptrfmt ) colptr(1:ncol+1)
      read ( *, indfmt ) rowind(1:nnzero)
c
c  Read matrix values.
c
      if ( 0 .lt. valcrd ) then
        read ( *, valfmt ) values(1:nnzero)
      end if

      return
      end
      subroutine c8_hb_data_read ( input, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt, 
     &  colptr, rowind, values, rhsval )

c*********************************************************************72
c
cc C8_HB_DATA_READ reads the data for a C8 Harwell-Boeing file.
c
c  Discussion:
c
c    It is assumed that C8_HB_HEADER_READ has already been called, and that
c    the file is still open.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, character * ( 3 ) RHSTYP, HB stuff.
c
c    Input, integer NRHS, the number of right hand sides.
c
c    Input, integer NRHSIX, HB stuff.
c
c    Input, integer VALCRD, the number of input lines of values.
c
c    Input, integer RHSCRD, the number of input lines of
c    right hand side data.
c
c    Input, character * ( 16 ) PTRFMT, the format to read column pointer data.
c
c    Input, character * ( 16 ) INDFMT, the format to read row index data.
c
c    Input, character * ( 20 ) VALFMT, the format to read matrix value data.
c
c    Input, character * ( 20 ) RHSFMT, the format to read right hand side data.
c    Output, double complex VALUES(NNZERO), the nonzero values.
c
c    Output, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Output, integer COLPTR(NCOL+1), compressed column indices.
c
c    Output, double complex RHSVAL(NROW*NRHS), HB stuff.
c
      implicit none

      integer ncol
      integer nnzero
      integer nrhs
      integer nrow

      integer colptr(ncol+1)
      integer i
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer neltvl
      integer nrhsix
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      double complex rhsval(nrow*nrhs)
      integer rowind(nnzero)
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt
      double complex values(nnzero)
c
c  Read matrix structure.
c
      read ( input, ptrfmt ) colptr(1:ncol+1)
      read ( input, indfmt ) rowind(1:nnzero)
c
c  Read matrix values.
c
      if ( 0 .lt. valcrd ) then
        read ( input, valfmt ) values(1:nnzero)
      end if
c
c  Read right hand side values.
c
      if ( 0 .lt. rhscrd ) then

        if ( rhstyp(1:1) .eq. 'F' .or. rhstyp(1:1) == 'f' ) then
          read ( input, rhsfmt ) rhsval(1:nrow*nrhs)
        else
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'C8_HB_DATA_READ - Fatal error!'
          write ( *, '(a)' ) '  Unsupported RHS input type.'
          stop 1
        end if

      end if

      return
      end
      subroutine c8_hb_header_print ( filename, nrow, ncol, nnzero, 
     &  rhstyp, nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, 
     &  rhsfmt )

c*********************************************************************72
c
cc C8_HB_HEADER_PRINT prints the header for a C8 Harwell-Boeing file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILENAME, the name of the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, character * ( 3 ) RHSTYP, HB stuff.
c
c    Input, integer NRHS, the number of right hand sides.
c
c    Input, integer NRHSIX, HB stuff.
c
c    Input, integer VALCRD, number of lines of VALUE data.
c
c    Input, integer RHSCRD, number of lines of RHS data.
c
c    Input, character * ( 16 ) PTRFMT, the format for reading column
c    pointers.
c
c    Input, character * ( 16 ) INDFMT, the format for reading row indices.
c
c    Input, character * ( 20 ) VALFMT, the format for reading VALUE data.
c
c    Input, character * ( 20 ) RHSFMT, the format for reading RHS data.
c
      implicit none

      character * ( * ) filename
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Harwell Boeing data read from "' // trim ( filename ) // '"'
      write ( *, '(a)' ) ''
      write ( *, '(a,i8)' ) 
     &  '  Matrix rows                NROW =   ', nrow
      write ( *, '(a,i8)' ) 
     &  '  Matrix columns             NCOL =   ', ncol
      write ( *, '(a,i8)' ) 
     &  '  Matrix nonzeros            NNZERO = ', nnzero
      write ( *, '(a,i8)' ) 
     &  '  Right hand sides           NRHS =   ', nrhs
      write ( *, '(a,i8)' ) 
     &  '                             NRHSIX = ', nrhsix
      write ( *, '(a,i8)' ) 
     &  '  Number of VALUE lines      VALCRD = ', valcrd
      write ( *, '(a,i8)' ) 
     &  '  Number of RHS lines        RHSCRD = ', rhscrd
      write ( *, '(a)'    ) 
     &  '  RHS descriptor             RHSTYP = "' // rhstyp // '"'
      write ( *, '(a)'    ) 
     &  '  Format for column pointers PTRFMT = "' // 
     &  trim ( ptrfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for row indices     INDFMT = "' // 
     &  trim ( indfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for VALUE data      VALFMT = "' // 
     &  trim ( valfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for RHS data        RHSFMT = "' // 
     &  trim ( rhsfmt ) // '"'

      return
      end
      subroutine c8_hb_header_read ( input, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )

c*********************************************************************72
c
cc C8_HB_HEADER_READ reads the header for a C8 Harwell-Boeing file.
c
c  Discussion:
c
c    This function only works for a particular kind of HB file.
c
c    It is assumed that the matrix is complex.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Output, integer NROW, the number of rows.
c
c    Output, integer NCOL, the number of columns.
c
c    Output, integer NNZERO, the number of nonzeros.
c
c    Output, character * ( 3 ) RHSTYP, HB stuff.
c
c    Output, integer NRHS, HB stuff
c
c    Output, integer NRHSIX, HB stuff.
c
c    Output, integer VALCRD, the number of input lines of values.
c
c    Output, integer RHSCRD, the number of input lines of
c    right hand side data.
c
c    Output, character * ( 16 ) PTRFMT, the format to read column pointer data.
c
c    Output, character * ( 16 ) INDFMT, the format to read row index data.
c
c    Output, character * ( 20 ) VALFMT, the format to read matrix value data.
c
c    Output, character * ( 20 ) RHSFMT, the format to read right hand side data.
c
      implicit none

      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt

      read ( input, '(a72, a8 )' ) title, key

      read ( input, '(5i14)' ) totcrd, ptrcrd, indcrd, valcrd, rhscrd

      read ( input, '(a3, 11x, 4i14)' ) 
     &  mxtype, nrow, ncol, nnzero, neltvl

      read ( input, '(a16, a16, a20, a20)' ) 
     &  ptrfmt, indfmt, valfmt, rhsfmt

      if ( 0 .lt. rhscrd ) then
        read ( input, '(a3,11x,i14,i14)' ) rhstyp, nrhs, nrhsix
      else
        rhstyp = '***'
        nrhs = 0
        nrhsix = 0
      end if

      return
      end
      subroutine c8_hb_quick_print ( filename, nrow, ncol, nnzero, 
     &  values, rowind, colptr )

c*********************************************************************72
c
cc C8_HB_QUICK_PRINT prints a sparse matrix in C8 Harwell-Boeing format.
c
c  Discussion:
c
c    This function is designed to print the information that was read by
c    the simplified Harwell-Boeing reader C8_HB_QUICK_READ.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILENAME, the name of the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, double complex VALUES(NNZERO), the nonzero values.
c
c    Input, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Input, integer COLPTR(NCOL+1), compressed column indices.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      character * ( * ) filename
      integer nrow
      integer rowind(nnzero)
      double complex values(nnzero)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Harwell Boeing data read from "' // trim ( filename ) // '"'
      write ( *, '(a)' ) ''
      write ( *, '(a,i8)' ) 
     &  '  Matrix rows                NROW =   ', nrow
      write ( *, '(a,i8)' ) 
     &  '  Matrix columns             NCOL =   ', ncol
      write ( *, '(a,i8)' ) 
     &  '  Matrix nonzeros            NNZERO = ', nnzero

      call c8_hb_structure_print ( ncol, nnzero, colptr, rowind )

      call c8_hb_values_print ( ncol, colptr, nnzero, values )

      return
      end
      subroutine c8_hb_quick_read ( input, nrow, ncol, nnzero, values, 
     &  rowind, colptr )

c*********************************************************************72
c
cc C8_HB_QUICK_READ reads a sparse matrix in C8 Harwell-Boeing format.
c
c  Discussion:
c
c    The Harwell-Boeing file format for sparse matrix storage is complicated,
c    and has many options.
c
c    This routine can only handle a particular simple case of the format,
c    in which the file contains a "CUA" matrix, complex, unsymmetric, assembled.
c
c    While the file may contain additional information, this routine cannot
c    retrieve it.
c
c    The user must allocate at least enough space for VALUES, ROWIND, and 
c    COLPTR before calling this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Output, integer NROW, the number of rows.
c
c    Output, integer NCOL, the number of columns.
c
c    Output, integer NNZERO, the number of nonzeros.
c
c    Output, double complex VALUES(NNZERO), the nonzero values.
c
c    Output, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Output, integer COLPTR(NCOL+1), compressed column indices.
c
      implicit none

      integer colptr(*)
      integer i
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      integer rowind(*)
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt
      double complex values(*)
c
c  Read header.
c
      read ( input, 
     &  '(a72, a8 / 5i14 / a3, 11x, 4i14 / a16, a16, a20, a20 )' ) 
     &  title, key, 
     &  totcrd, ptrcrd, indcrd, valcrd, rhscrd, 
     &  mxtype, nrow, ncol, nnzero, neltvl, 
     &  ptrfmt, indfmt, valfmt, rhsfmt
c
c  Read matrix structure.
c
      read ( input, ptrfmt ) colptr(1:ncol+1)
      read ( input, indfmt ) rowind(1:nnzero)
c
c  Read matrix values.
c
      if ( 0 .lt. valcrd ) then
        read ( input, valfmt ) values(1:nnzero)
      end if

      return
      end
      subroutine c8_hb_structure_print ( ncol, nnzero, colptr, rowind )

c*********************************************************************72
c
cc C8_HB_STRUCTURE_PRINT prints the structure of a C8 Harwell-Boeing matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Iain Duff, Roger Grimes, John Lewis,
c    User's Guide for the Harwell-Boeing Sparse Matrix Collection,
c    Technical Report TR/PA/92/86, CERFACS,
c    October 1992.
c
c  Parameters:
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeroes.
c
c    Input, integer COLPTR(NCOL+1), COLPTR(I) points to the
c    location of the first entry of column I in the sparse matrix structure.
c
c    Input, integer ROWIND(NNZERO), the row index of each item.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      integer j
      integer khi
      integer klo
      integer rowind(nnzero)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Column   Begin     End   ' // 
     &  '------------------------------------------------------------'
      write ( *, '(a)' ) ' '
      do j = 1, ncol

        if ( 5 .lt. j .and. j .lt. ncol ) then
          go to 10
        end if

        if ( j .eq. ncol .and. 6 .lt. ncol ) then
          write ( *, '(a)' ) '  (Skipping intermediate columns...)'
        end if

        if ( colptr(j+1)-1 .lt. colptr(j) ) then

          write ( *, '(i8,3x,a)' ) j, 'EMPTY'

        else

          do klo = colptr(j), colptr(j+1)-1, 5
            khi = min ( klo + 4, colptr(j+1)-1 )
            if ( klo .eq. colptr(j) ) then
              write ( *, '(3i8,3x,5i6)' ) 
     &          j, colptr(j), colptr(j+1)-1, rowind(klo:khi)
            else
              write ( *, '(27x,5i6)' ) rowind(klo:khi)
            end if
          end do

        end if

10      continue

      end do

      write ( *, '(a)' ) 
     &  '                           ' // 
     &  '------------------------------------------------------------'

      return
      end
      subroutine c8_hb_values_print ( ncol, colptr, nnzero, values )

c*********************************************************************72
c
cc C8_HB_VALUES_PRINT prints the values of a C8 Harwell-Boeing matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Iain Duff, Roger Grimes, John Lewis,
c    User's Guide for the Harwell-Boeing Sparse Matrix Collection,
c    Technical Report TR/PA/92/86, CERFACS,
c    October 1992.
c
c  Parameters:
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer COLPTR(NCOL+1), COLPTR(I) points to the
c    location of the first entry of column I in the sparse matrix structure.
c
c    Input, integer NNZERO, the number of nonzeroes.
c
c    Input, double complex VALUES(NNZERO), the nonzero values of
c    the matrix.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      integer j
      integer k
      integer khi
      integer klo
      double complex values(nnzero)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Column   Begin     End   ' // 
     &  '--------------------------------------------------------'
      write ( *, '(a)' ) ' '

      do j = 1, ncol

        if ( 5 .lt. j .and. j .lt. ncol ) then
          go to 10
        end if

        if ( j .eq. ncol .and. 6 .lt. ncol ) then
          write ( *, '(a)' ) '  (Skipping intermediate columns...)'
        end if

        if ( colptr(j+1)-1 .lt. colptr(j) ) then

          write ( *, '(i8)' ) j, '   EMPTY'

        else

          do klo = colptr(j), colptr(j+1)-1, 2
            khi = min ( klo + 1, colptr(j+1)-1 )
            if ( klo .eq. colptr(j) ) then
              write ( *, '(2x,i6,2x,i6,2x,i6,3x)', advance = 'no' ) 
     &          j, colptr(j), colptr(j+1)-1
            else
              write ( *, '(27x)', advance = 'no' )
            end if
            do k = klo, khi
              write ( *, 
     &          '(''('',g12.4,'','',g12.4,'')'')', advance = 'no' ) 
     &          values(k)
            end do
          write ( *, '(a)' ) ''
          end do

        end if

10      continue

      end do

      write ( *, '(a)' ) 
     &  '                           ' // 
     &  '--------------------------------------------------------'

      return
      end
      subroutine c8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, 
     &  title )

c*********************************************************************72
c
cc C8MAT_PRINT_SOME prints some of a C8MAT.
c
c  Discussion:
c
c    A C8MAT is a matrix of C8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns 
c    in the matrix.
c
c    Input, double complex A(M,N), the matrix.
c
c    Input, integer ILO, JLO, IHI, JHI, the first row and
c    column, and the last row and column to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 4 )
      integer m
      integer n

      double complex a(m,n)
      character * ( 20 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title
      double complex zero

      zero = dcmplx ( 0.0D+00, 0.0D+00 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)' 
        return
      end if
c
c  Print the columns of the matrix, in strips of INCX.
c
      do j2lo = jlo, min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i10,10x)' ) j
        end do

        write ( *, '(a,4a20)' ) '  Col: ', ( ctemp(j2), j2 = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) '  ---'
c
c  Determine the range of the rows in this strip.
c
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi
c
c  Print out (up to) INCX entries in row I, that lie in the current strip.
c
          do j2 = 1, inc

            j = j2lo - 1 + j2

            if ( dimag ( a(i,j) ) .eq. 0.0D+00 ) then
              write ( ctemp(j2), '(g10.3,10x)' ) dreal ( a(i,j) )
            else
              write ( ctemp(j2), '(2g10.3)' ) a(i,j)
            end if

          end do

          write ( *, '(i5,a,4a20)' ) i, ':', ( ctemp(j2), j2 = 1, inc )

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
      subroutine r4_hb_data_read ( input, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt, 
     &  colptr, rowind, values, rhsval )

c*********************************************************************72
c
cc R4_HB_DATA_READ reads the data for an R4 Harwell-Boeing file.
c
c  Discussion:
c
c    It is assumed that R4_HB_HEADER_READ has already been called, and that
c    the file is still open.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, character * ( 3 ) RHSTYP, HB stuff.
c
c    Input, integer NRHS, the number of right hand sides.
c
c    Input, integer NRHSIX, HB stuff.
c
c    Input, integer VALCRD, the number of input lines of values.
c
c    Input, integer RHSCRD, the number of input lines of
c    right hand side data.
c
c    Input, character * ( 16 ) PTRFMT, the format to read column pointer data.
c
c    Input, character * ( 16 ) INDFMT, the format to read row index data.
c
c    Input, character * ( 20 ) VALFMT, the format to read matrix value data.
c
c    Input, character * ( 20 ) RHSFMT, the format to read right hand side data.
c    Output, real VALUES(NNZERO), the nonzero values.
c
c    Output, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Output, integer COLPTR(NCOL+1), compressed column indices.
c
c    Output, real RHSVAL(NROW*NRHS), HB stuff.
c
      implicit none

      integer ncol
      integer nnzero
      integer nrhs
      integer nrow

      integer colptr(ncol+1)
      integer i
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer neltvl
      integer nrhsix
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      real rhsval(nrow*nrhs)
      integer rowind(nnzero)
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt
      real values(nnzero)
c
c  Read matrix structure.
c
      read ( input, ptrfmt ) colptr(1:ncol+1)
      read ( input, indfmt ) rowind(1:nnzero)
c
c  Read matrix values.
c
      if ( 0 .lt. valcrd ) then
        read ( input, valfmt ) values(1:nnzero)
      end if
c
c  Read right hand side values.
c
      if ( 0 .lt. rhscrd ) then

        if ( rhstyp(1:1) .eq. 'F' .or. rhstyp(1:1) == 'f' ) then
          read ( input, rhsfmt ) rhsval(1:nrow*nrhs)
        else
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'R4_HB_DATA_READ - Fatal error!'
          write ( *, '(a)' ) '  Unsupported RHS input type.'
          stop 1
        end if

      end if

      return
      end
      subroutine r4_hb_header_print ( filename, nrow, ncol, nnzero, 
     &  rhstyp, nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, 
     &  rhsfmt )

c*********************************************************************72
c
cc R4_HB_HEADER_PRINT prints the header for an R4 Harwell-Boeing file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILENAME, the name of the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, character * ( 3 ) RHSTYP, HB stuff.
c
c    Input, integer NRHS, the number of right hand sides.
c
c    Input, integer NRHSIX, HB stuff.
c
c    Input, integer VALCRD, number of lines of VALUE data.
c
c    Input, integer RHSCRD, number of lines of RHS data.
c
c    Input, character * ( 16 ) PTRFMT, the format for reading column
c    pointers.
c
c    Input, character * ( 16 ) INDFMT, the format for reading row indices.
c
c    Input, character * ( 20 ) VALFMT, the format for reading VALUE data.
c
c    Input, character * ( 20 ) RHSFMT, the format for reading RHS data.
c
      implicit none

      character * ( * ) filename
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Harwell Boeing data read from "' // trim ( filename ) // '"'
      write ( *, '(a)' ) ''
      write ( *, '(a,i8)' ) 
     &  '  Matrix rows                NROW =   ', nrow
      write ( *, '(a,i8)' ) 
     &  '  Matrix columns             NCOL =   ', ncol
      write ( *, '(a,i8)' ) 
     &  '  Matrix nonzeros            NNZERO = ', nnzero
      write ( *, '(a,i8)' ) 
     &  '  Right hand sides           NRHS =   ', nrhs
      write ( *, '(a,i8)' ) 
     &  '                             NRHSIX = ', nrhsix
      write ( *, '(a,i8)' ) 
     &  '  Number of VALUE lines      VALCRD = ', valcrd
      write ( *, '(a,i8)' ) 
     &  '  Number of RHS lines        RHSCRD = ', rhscrd
      write ( *, '(a)'    ) 
     &  '  RHS descriptor             RHSTYP = "' // rhstyp // '"'
      write ( *, '(a)'    ) 
     &  '  Format for column pointers PTRFMT = "' // 
     &  trim ( ptrfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for row indices     INDFMT = "' // 
     &  trim ( indfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for VALUE data      VALFMT = "' // 
     &  trim ( valfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for RHS data        RHSFMT = "' // 
     &  trim ( rhsfmt ) // '"'

      return
      end
      subroutine r4_hb_header_read ( input, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )

c*********************************************************************72
c
cc R4_HB_HEADER_READ reads the header for an R4 Harwell-Boeing file.
c
c  Discussion:
c
c    This function only works for a particular kind of HB file.
c
c    It is assumed that the matrix is real.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Output, integer NROW, the number of rows.
c
c    Output, integer NCOL, the number of columns.
c
c    Output, integer NNZERO, the number of nonzeros.
c
c    Output, character * ( 3 ) RHSTYP, HB stuff.
c
c    Output, integer NRHS, HB stuff
c
c    Output, integer NRHSIX, HB stuff.
c
c    Output, integer VALCRD, the number of input lines of values.
c
c    Output, integer RHSCRD, the number of input lines of
c    right hand side data.
c
c    Output, character * ( 16 ) PTRFMT, the format to read column pointer data.
c
c    Output, character * ( 16 ) INDFMT, the format to read row index data.
c
c    Output, character * ( 20 ) VALFMT, the format to read matrix value data.
c
c    Output, character * ( 20 ) RHSFMT, the format to read right hand side data.
c
      implicit none

      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt

      read ( input, '(a72, a8 )' ) title, key

      read ( input, '(5i14)' ) totcrd, ptrcrd, indcrd, valcrd, rhscrd

      read ( input, '(a3, 11x, 4i14)' ) 
     &  mxtype, nrow, ncol, nnzero, neltvl

      read ( input, '(a16, a16, a20, a20)' ) 
     &  ptrfmt, indfmt, valfmt, rhsfmt

      if ( 0 .lt. rhscrd ) then
        read ( input, '(a3,11x,i14,i14)' ) rhstyp, nrhs, nrhsix
      else
        rhstyp = '***'
        nrhs = 0
        nrhsix = 0
      end if

      return
      end
      subroutine r4_hb_quick_print ( filename, nrow, ncol, nnzero, 
     &  values, rowind, colptr )

c*********************************************************************72
c
cc R4_HB_QUICK_PRINT prints a sparse matrix in R4 Harwell-Boeing format.
c
c  Discussion:
c
c    This function is designed to print the information that was read by
c    the simplified Harwell-Boeing reader R4_HB_QUICK_READ.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILENAME, the name of the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, real VALUES(NNZERO), the nonzero values.
c
c    Input, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Input, integer COLPTR(NCOL+1), compressed column indices.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      character * ( * ) filename
      integer nrow
      integer rowind(nnzero)
      real values(nnzero)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Harwell Boeing data read from "' // trim ( filename ) // '"'
      write ( *, '(a)' ) ''
      write ( *, '(a,i8)' ) 
     &  '  Matrix rows                NROW =   ', nrow
      write ( *, '(a,i8)' ) 
     &  '  Matrix columns             NCOL =   ', ncol
      write ( *, '(a,i8)' ) 
     &  '  Matrix nonzeros            NNZERO = ', nnzero

      call r4_hb_structure_print ( ncol, nnzero, colptr, rowind )

      call r4_hb_values_print ( ncol, colptr, nnzero, values )

      return
      end
      subroutine r4_hb_quick_read ( input, nrow, ncol, nnzero, values, 
     &  rowind, colptr )

c*********************************************************************72
c
cc R4_HB_QUICK_READ reads a sparse matrix in R4 Harwell-Boeing format.
c
c  Discussion:
c
c    The Harwell-Boeing file format for sparse matrix storage is complicated,
c    and has many options.
c
c    This routine can only handle a particular simple case of the format,
c    in which the file contains a "RUA" matrix, real, unsymmetric, assembled.
c
c    While the file may contain additional information, this routine cannot
c    retrieve it.
c
c    The user must allocate at least enough space for VALUES, ROWIND, and 
c    COLPTR before calling this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Output, integer NROW, the number of rows.
c
c    Output, integer NCOL, the number of columns.
c
c    Output, integer NNZERO, the number of nonzeros.
c
c    Output, real VALUES(NNZERO), the nonzero values.
c
c    Output, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Output, integer COLPTR(NCOL+1), compressed column indices.
c
      implicit none

      integer colptr(*)
      integer i
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      integer rowind(*)
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt
      real values(*)
c
c  Read header.
c
      read ( input, 
     &  '(a72, a8 / 5i14 / a3, 11x, 4i14 / a16, a16, a20, a20 )' ) 
     &  title, key, 
     &  totcrd, ptrcrd, indcrd, valcrd, rhscrd, 
     &  mxtype, nrow, ncol, nnzero, neltvl, 
     &  ptrfmt, indfmt, valfmt, rhsfmt
c
c  Read matrix structure.
c
      read ( input, ptrfmt ) colptr(1:ncol+1)
      read ( input, indfmt ) rowind(1:nnzero)
c
c  Read matrix values.
c
      if ( 0 .lt. valcrd ) then
        read ( input, valfmt ) values(1:nnzero)
      end if

      return
      end
      subroutine r4_hb_structure_print ( ncol, nnzero, colptr, rowind )

c*********************************************************************72
c
cc R4_HB_STRUCTURE_PRINT prints the structure of an R4 Harwell-Boeing matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Iain Duff, Roger Grimes, John Lewis,
c    User's Guide for the Harwell-Boeing Sparse Matrix Collection,
c    Technical Report TR/PA/92/86, CERFACS,
c    October 1992.
c
c  Parameters:
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeroes.
c
c    Input, integer COLPTR(NCOL+1), COLPTR(I) points to the
c    location of the first entry of column I in the sparse matrix structure.
c
c    Input, integer ROWIND(NNZERO), the row index of each item.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      integer j
      integer khi
      integer klo
      integer rowind(nnzero)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Column   Begin     End   ' // 
     &  '------------------------------------------------------------'
      write ( *, '(a)' ) ' '
      do j = 1, ncol

        if ( 5 .lt. j .and. j .lt. ncol ) then
          go to 10
        end if

        if ( j .eq. ncol .and. 6 .lt. ncol ) then
          write ( *, '(a)' ) '  (Skipping intermediate columns...)'
        end if

        if ( colptr(j+1)-1 .lt. colptr(j) ) then

          write ( *, '(i8,3x,a)' ) j, 'EMPTY'

        else

          do klo = colptr(j), colptr(j+1)-1, 5
            khi = min ( klo + 4, colptr(j+1)-1 )
            if ( klo .eq. colptr(j) ) then
              write ( *, '(3i8,3x,5i6)' ) 
     &          j, colptr(j), colptr(j+1)-1, rowind(klo:khi)
            else
              write ( *, '(27x,5i6)' ) rowind(klo:khi)
            end if
          end do

        end if

10      continue

      end do

      write ( *, '(a)' ) 
     &  '                           ' // 
     &  '------------------------------------------------------------'

      return
      end
      subroutine r4_hb_values_print ( ncol, colptr, nnzero, values )

c*********************************************************************72
c
cc R4_HB_VALUES_PRINT prints the values of an R4 Harwell-Boeing matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Iain Duff, Roger Grimes, John Lewis,
c    User's Guide for the Harwell-Boeing Sparse Matrix Collection,
c    Technical Report TR/PA/92/86, CERFACS,
c    October 1992.
c
c  Parameters:
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer COLPTR(NCOL+1), COLPTR(I) points to the
c    location of the first entry of column I in the sparse matrix structure.
c
c    Input, integer NNZERO, the number of nonzeroes.
c
c    Input, real VALUES(NNZERO), the nonzero values of
c    the matrix.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      integer j
      integer khi
      integer klo
      real values(nnzero)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Column   Begin     End   ' // 
     &  '--------------------------------------------------------'
      write ( *, '(a)' ) ' '

      do j = 1, ncol

        if ( 5 .lt. j .and. j .lt. ncol ) then
          go to 10
        end if

        if ( j .eq. ncol .and. 6 .lt. ncol ) then
          write ( *, '(a)' ) '  (Skipping intermediate columns...)'
        end if

        if ( colptr(j+1)-1 .lt. colptr(j) ) then

          write ( *, '(i8)' ) j, '   EMPTY'

        else

          do klo = colptr(j), colptr(j+1)-1, 5
            khi = min ( klo + 4, colptr(j+1)-1 )
            if ( klo .eq. colptr(j) ) then
              write ( *, '(2x,i6,2x,i6,2x,i6,3x,5g12.4)' ) 
     &          j, colptr(j), colptr(j+1)-1, values(klo:khi)
            else
              write ( *, '(27x,5g12.4)' ) values(klo:khi)
            end if
          end do

        end if

10      continue

      end do

      write ( *, '(a)' ) 
     &  '                           ' // 
     &  '--------------------------------------------------------'

      return
      end
      subroutine r4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R4MAT_PRINT_SOME prints some of an R4MAT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, real A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      real a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(g14.6)' ) a(i,j)

          end do

          write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

      end do

      return
      end
      subroutine r8_hb_data_read ( input, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt, 
     &  colptr, rowind, values, rhsval )

c*********************************************************************72
c
cc R8_HB_DATA_READ reads the data for an R8 Harwell-Boeing file.
c
c  Discussion:
c
c    It is assumed that R8_HB_HEADER_READ has already been called, and that
c    the file is still open.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, character * ( 3 ) RHSTYP, HB stuff.
c
c    Input, integer NRHS, the number of right hand sides.
c
c    Input, integer NRHSIX, HB stuff.
c
c    Input, integer VALCRD, the number of input lines of values.
c
c    Input, integer RHSCRD, the number of input lines of
c    right hand side data.
c
c    Input, character * ( 16 ) PTRFMT, the format to read column pointer data.
c
c    Input, character * ( 16 ) INDFMT, the format to read row index data.
c
c    Input, character * ( 20 ) VALFMT, the format to read matrix value data.
c
c    Input, character * ( 20 ) RHSFMT, the format to read right hand side data.
c
c    Output, double precision VALUES(NNZERO), the nonzero values.
c
c    Output, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Output, integer COLPTR(NCOL+1), compressed column indices.
c
c    Output, double precision RHSVAL(NROW*NRHS), HB stuff.
c
      implicit none

      integer ncol
      integer nnzero
      integer nrhs
      integer nrow

      integer colptr(ncol+1)
      integer i
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer neltvl
      integer nrhsix
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      double precision rhsval(nrow*nrhs)
      integer rowind(nnzero)
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt
      double precision values(nnzero)
c
c  Read matrix structure.
c
      read ( input, ptrfmt ) colptr(1:ncol+1)
      read ( input, indfmt ) rowind(1:nnzero)
c
c  Read matrix values.
c
      if ( 0 .lt. valcrd ) then
        read ( input, valfmt ) values(1:nnzero)
      end if
c
c  Read right hand side values.
c
      if ( 0 .lt. rhscrd ) then

        if ( rhstyp(1:1) .eq. 'F' .or. rhstyp(1:1) == 'f' ) then
          read ( input, rhsfmt ) rhsval(1:nrow*nrhs)
        else
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'R8_HB_DATA_READ - Fatal error!'
          write ( *, '(a)' ) '  Unsupported RHS input type.'
          stop 1
        end if

      end if

      return
      end
      subroutine r8_hb_header_print ( filename, nrow, ncol, nnzero, 
     &  rhstyp, nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, 
     &  rhsfmt )

c*********************************************************************72
c
cc R8_HB_HEADER_PRINT prints the header for an R8 Harwell-Boeing file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILENAME, the name of the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, character * ( 3 ) RHSTYP, HB stuff.
c
c    Input, integer NRHS, the number of right hand sides.
c
c    Input, integer NRHSIX, HB stuff.
c
c    Input, integer VALCRD, number of lines of VALUE data.
c
c    Input, integer RHSCRD, number of lines of RHS data.
c
c    Input, character * ( 16 ) PTRFMT, the format for reading column
c    pointers.
c
c    Input, character * ( 16 ) INDFMT, the format for reading row indices.
c
c    Input, character * ( 20 ) VALFMT, the format for reading VALUE data.
c
c    Input, character * ( 20 ) RHSFMT, the format for reading RHS data.
c
      implicit none

      character * ( * ) filename
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Harwell Boeing data read from "' // trim ( filename ) // '"'
      write ( *, '(a)' ) ''
      write ( *, '(a,i8)' ) 
     &  '  Matrix rows                NROW =   ', nrow
      write ( *, '(a,i8)' ) 
     &  '  Matrix columns             NCOL =   ', ncol
      write ( *, '(a,i8)' ) 
     &  '  Matrix nonzeros            NNZERO = ', nnzero
      write ( *, '(a,i8)' ) 
     &  '  Right hand sides           NRHS =   ', nrhs
      write ( *, '(a,i8)' ) 
     &  '                             NRHSIX = ', nrhsix
      write ( *, '(a,i8)' ) 
     &  '  Number of VALUE lines      VALCRD = ', valcrd
      write ( *, '(a,i8)' ) 
     &  '  Number of RHS lines        RHSCRD = ', rhscrd
      write ( *, '(a)'    ) 
     &  '  RHS descriptor             RHSTYP = "' // rhstyp // '"'
      write ( *, '(a)'    ) 
     &  '  Format for column pointers PTRFMT = "' // 
     &  trim ( ptrfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for row indices     INDFMT = "' // 
     &  trim ( indfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for VALUE data      VALFMT = "' // 
     &  trim ( valfmt ) // '"'
      write ( *, '(a)'    ) 
     &  '  Format for RHS data        RHSFMT = "' // 
     &  trim ( rhsfmt ) // '"'

      return
      end
      subroutine r8_hb_header_read ( input, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )

c*********************************************************************72
c
cc R8_HB_HEADER_READ reads the header for an R8 Harwell-Boeing file.
c
c  Discussion:
c
c    This function only works for a particular kind of HB file.
c
c    It is assumed that the matrix is real.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Output, integer NROW, the number of rows.
c
c    Output, integer NCOL, the number of columns.
c
c    Output, integer NNZERO, the number of nonzeros.
c
c    Output, character * ( 3 ) RHSTYP, HB stuff.
c
c    Output, integer NRHS, the number of right hand sides.
c
c    Output, integer NRHSIX, HB stuff.
c
c    Output, integer VALCRD, the number of input lines of values.
c
c    Output, integer RHSCRD, the number of input lines of
c    right hand side data.
c
c    Output, character * ( 16 ) PTRFMT, the format to read column pointer data.
c
c    Output, character * ( 16 ) INDFMT, the format to read row index data.
c
c    Output, character * ( 20 ) VALFMT, the format to read matrix value data.
c
c    Output, character * ( 20 ) RHSFMT, the format to read right hand side data.
c
      implicit none

      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt

      read ( input, '(a72, a8 )' ) title, key

      read ( input, '(5i14)' ) totcrd, ptrcrd, indcrd, valcrd, rhscrd

      read ( input, '(a3, 11x, 4i14)' ) 
     &  mxtype, nrow, ncol, nnzero, neltvl

      read ( input, '(a16, a16, a20, a20)' ) 
     &  ptrfmt, indfmt, valfmt, rhsfmt

      if ( 0 .lt. rhscrd ) then
        read ( input, '(a3,11x,i14,i14)' ) rhstyp, nrhs, nrhsix
      else
        rhstyp = '***'
        nrhs = 0
        nrhsix = 0
      end if

      return
      end
      subroutine r8_hb_quick_print ( filename, nrow, ncol, nnzero, 
     &  values, rowind, colptr )

c*********************************************************************72
c
cc R8_HB_QUICK_PRINT prints a sparse matrix in R8 Harwell-Boeing format.
c
c  Discussion:
c
c    This function is designed to print the information that was read by
c    the simplified Harwell-Boeing reader R8_HB_QUICK_READ.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FILENAME, the name of the file.
c
c    Input, integer NROW, the number of rows.
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeros.
c
c    Input, double precision VALUES(NNZERO), the nonzero values.
c
c    Input, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Input, integer COLPTR(NCOL+1), compressed column indices.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      character * ( * ) filename
      integer nrow
      integer rowind(nnzero)
      double precision values(nnzero)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Harwell Boeing data read from "' // trim ( filename ) // '"'
      write ( *, '(a)' ) ''
      write ( *, '(a,i8)' ) 
     &  '  Matrix rows                NROW =   ', nrow
      write ( *, '(a,i8)' ) 
     &  '  Matrix columns             NCOL =   ', ncol
      write ( *, '(a,i8)' ) 
     &  '  Matrix nonzeros            NNZERO = ', nnzero

      call r8_hb_structure_print ( ncol, nnzero, colptr, rowind )

      call r8_hb_values_print ( ncol, colptr, nnzero, values )

      return
      end
      subroutine r8_hb_quick_read ( input, nrow, ncol, nnzero, values, 
     &  rowind, colptr )

c*********************************************************************72
c
cc R8_HB_QUICK_READ reads a sparse matrix in R8 Harwell-Boeing format.
c
c  Discussion:
c
c    The Harwell-Boeing file format for sparse matrix storage is complicated,
c    and has many options.
c
c    This routine can only handle a particular simple case of the format,
c    in which the file contains a "RUA" matrix, real, unsymmetric, assembled.
c
c    While the file may contain additional information, this routine cannot
c    retrieve it.
c
c    The user must allocate at least enough space for VALUES, ROWIND, and 
c    COLPTR before calling this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer INPUT, the input unit associated with the file.
c
c    Output, integer NROW, the number of rows.
c
c    Output, integer NCOL, the number of columns.
c
c    Output, integer NNZERO, the number of nonzeros.
c
c    Output, double precision VALUES(NNZERO), the nonzero values.
c
c    Output, integer ROWIND(NNZERO), the row indices of nonzeros.
c
c    Output, integer COLPTR(NCOL+1), compressed column indices.
c
      implicit none

      integer colptr(*)
      integer i
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      integer rowind(*)
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt
      double precision values(*)
c
c  Read header.
c
      read ( input, 
     &  '(a72, a8 / 5i14 / a3, 11x, 4i14 / a16, a16, a20, a20 )' ) 
     &  title, key, 
     &  totcrd, ptrcrd, indcrd, valcrd, rhscrd, 
     &  mxtype, nrow, ncol, nnzero, neltvl, 
     &  ptrfmt, indfmt, valfmt, rhsfmt
c
c  Read matrix structure.
c
      read ( input, ptrfmt ) colptr(1:ncol+1)
      read ( input, indfmt ) rowind(1:nnzero)
c
c  Read matrix values.
c
      if ( 0 .lt. valcrd ) then
        read ( input, valfmt ) values(1:nnzero)
      end if

      return
      end
      subroutine r8_hb_structure_print ( ncol, nnzero, colptr, rowind )

c*********************************************************************72
c
cc R8_HB_STRUCTURE_PRINT prints the structure of an R8 Harwell-Boeing matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Iain Duff, Roger Grimes, John Lewis,
c    User's Guide for the Harwell-Boeing Sparse Matrix Collection,
c    Technical Report TR/PA/92/86, CERFACS,
c    October 1992.
c
c  Parameters:
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer NNZERO, the number of nonzeroes.
c
c    Input, integer COLPTR(NCOL+1), COLPTR(I) points to the
c    location of the first entry of column I in the sparse matrix structure.
c
c    Input, integer ROWIND(NNZERO), the row index of each item.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      integer j
      integer khi
      integer klo
      integer rowind(nnzero)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Column   Begin     End   ' // 
     &  '------------------------------------------------------------'
      write ( *, '(a)' ) ' '
      do j = 1, ncol

        if ( 5 .lt. j .and. j .lt. ncol ) then
          go to 10
        end if

        if ( j .eq. ncol .and. 6 .lt. ncol ) then
          write ( *, '(a)' ) '  (Skipping intermediate columns...)'
        end if

        if ( colptr(j+1)-1 .lt. colptr(j) ) then

          write ( *, '(i8,3x,a)' ) j, 'EMPTY'

        else

          do klo = colptr(j), colptr(j+1)-1, 5
            khi = min ( klo + 4, colptr(j+1)-1 )
            if ( klo .eq. colptr(j) ) then
              write ( *, '(3i8,3x,5i6)' ) 
     &          j, colptr(j), colptr(j+1)-1, rowind(klo:khi)
            else
              write ( *, '(27x,5i6)' ) rowind(klo:khi)
            end if
          end do

        end if

10      continue

      end do

      write ( *, '(a)' ) 
     &  '                           ' // 
     &  '------------------------------------------------------------'

      return
      end
      subroutine r8_hb_values_print ( ncol, colptr, nnzero, values )

c*********************************************************************72
c
cc R8_HB_VALUES_PRINT prints the values of an R8 Harwell-Boeing matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Iain Duff, Roger Grimes, John Lewis,
c    User's Guide for the Harwell-Boeing Sparse Matrix Collection,
c    Technical Report TR/PA/92/86, CERFACS,
c    October 1992.
c
c  Parameters:
c
c    Input, integer NCOL, the number of columns.
c
c    Input, integer COLPTR(NCOL+1), COLPTR(I) points to the
c    location of the first entry of column I in the sparse matrix structure.
c
c    Input, integer NNZERO, the number of nonzeroes.
c
c    Input, double precision VALUES(NNZERO), the nonzero values of
c    the matrix.
c
      implicit none

      integer ncol
      integer nnzero

      integer colptr(ncol+1)
      integer j
      integer khi
      integer klo
      double precision values(nnzero)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Column   Begin     End   ' // 
     &  '--------------------------------------------------------'
      write ( *, '(a)' ) ' '

      do j = 1, ncol

        if ( 5 .lt. j .and. j .lt. ncol ) then
          go to 10
        end if

        if ( j .eq. ncol .and. 6 .lt. ncol ) then
          write ( *, '(a)' ) '  (Skipping intermediate columns...)'
        end if

        if ( colptr(j+1)-1 .lt. colptr(j) ) then

          write ( *, '(i8)' ) j, '   EMPTY'

        else

          do klo = colptr(j), colptr(j+1)-1, 5
            khi = min ( klo + 4, colptr(j+1)-1 )
            if ( klo .eq. colptr(j) ) then
              write ( *, '(2x,i6,2x,i6,2x,i6,3x,5g12.4)' ) 
     &          j, colptr(j), colptr(j+1)-1, values(klo:khi)
            else
              write ( *, '(27x,5g12.4)' ) values(klo:khi)
            end if
          end do

        end if

10      continue

      end do

      write ( *, '(a)' ) 
     &  '                           ' // 
     &  '--------------------------------------------------------'

      return
      end
      subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R8MAT_PRINT_SOME prints some of an R8MAT.
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
c    25 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(g14.6)' ) a(i,j)

          end do

          write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

      end do

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
