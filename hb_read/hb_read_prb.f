      program main

c*********************************************************************72
c
cc MAIN is the main program for HB_READ_PRB.
c
c  Discussion:
c
c    HB_READ_PRB tests the HB_READ library.
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
      implicit none

      character * ( 255 ) filename

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HB_READ_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the HB_READ library.'
c
c  Quick read real single precision matrices.
c
      filename = 'g05_rua.txt'
      call test01 ( filename )
c
c  Quick read real double precision matrices.
c
      filename = 'g05_rua.txt'
      call test02 ( filename )
c
c  Quick read complex single precision matrices.
c
      filename = 'cg05_cua.txt'
      call test03 ( filename )
c
c  Quick read complex single precision matrices.
c
      filename = 'cg05_cua.txt'
      call test04 ( filename )
c
c  Full read real single precision matrices.
c
      filename = 'g05_rua.txt'
      call test05 ( filename )

      filename = 'g10_rua.txt'
      call test05 ( filename )

      filename = 'g20_rua.txt'
      call test05 ( filename )

      filename = 'cavity_small_rua.txt'
      call test05 ( filename )

      filename = 'cavity_big_rua.txt'
      call test05 ( filename )
c
c  Full read real double precision matrices.
c
      filename = 'g05_rua.txt'
      call test06 ( filename )
c
c  Full read complex single precision matrices.
c
      filename = 'cg20_cua.txt'
      call test07 ( filename )
c
c  Full read complex double precision matrices.
c
      filename = 'cg20_cua.txt'
      call test08 ( filename )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'HB_READ_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( filename )

c*********************************************************************72
c
cc TEST01 tests R4_HB_QUICK_READ.
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
c    Input, character * ( * ) FILENAME, the name of the HB file to read.
c
      implicit none

      integer n_max
      parameter ( n_max = 30 )
      integer nnzero_max
      parameter ( nnzero_max = 200 )

      integer colptr(n_max+1)
      character * ( * ) filename
      integer input
      integer ios
      integer ncol
      integer nnzero
      integer nrow
      integer rowind(nnzero_max)
      real values(nnzero_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  R4_HB_QUICK_READ reads a sparse matrix '
      write ( *, '(a)' ) '  from a Harwell-Boeing file,'
      write ( *, '(a)' ) '  using real single precision arithmetic.'
      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) 
     &  '  We assume NNZERO is no greater than ', nnzero_max
      write ( *, '(a,i6)' ) 
     &  '  We assume N is no greater than ', n_max
c
c  #1: Open the file.
c
      call get_unit ( input )
     
      open ( unit = input, file = filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TEST01 - Warning!'
        write ( *, '(a)' ) '  Could not open file "' // 
     &    trim ( filename ) // '".'
        return
      end if
c
c  #2: Read the information.
c
      call r4_hb_quick_read ( input, nrow, ncol, nnzero, values, 
     &  rowind, colptr )
c
c  #3: Print (some of) the information.
c
      call r4_hb_quick_print ( filename, nrow, ncol, nnzero, values, 
     &  rowind, colptr )
c
c  #4: Close the file.
c
      close ( unit = input )

      return
      end
      subroutine test02 ( filename )

c*********************************************************************72
c
cc TEST02 tests R8_HB_QUICK_READ.
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
c    Input, character * ( * ) FILENAME, the name of the HB file to read.
c
      implicit none

      integer n_max
      parameter ( n_max = 30 )
      integer nnzero_max
      parameter ( nnzero_max = 200 )

      integer colptr(n_max+1)
      character * ( * ) filename
      integer input
      integer ios
      integer ncol
      integer nnzero
      integer nrow
      integer rowind(nnzero_max)
      real values(nnzero_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  R8_HB_QUICK_READ reads a sparse matrix '
      write ( *, '(a)' ) '  from a Harwell-Boeing file,'
      write ( *, '(a)' ) '  using real double precision arithmetic.'
      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) 
     &  '  We assume NNZERO is no greater than ', nnzero_max
      write ( *, '(a,i6)' ) 
     &  '  We assume N is no greater than ', n_max
c
c  #1: Open the file.
c
      call get_unit ( input )
     
      open ( unit = input, file = filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TEST02 - Warning!'
        write ( *, '(a)' ) '  Could not open file "' // 
     &    trim ( filename ) // '".'
        return
      end if
c
c  #2: Read the information.
c
      call r4_hb_quick_read ( input, nrow, ncol, nnzero, values, 
     &  rowind, colptr )
c
c  #3: Print (some of) the information.
c
      call r4_hb_quick_print ( filename, nrow, ncol, nnzero, values, 
     &  rowind, colptr )
c
c  #4: Close the file.
c
      close ( unit = input )

      return
      end
      subroutine test03 ( filename )

c*********************************************************************72
c
cc TEST03 tests C4_HB_QUICK_READ.
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
c    Input, character * ( * ) FILENAME, the name of the HB file to read.
c
      implicit none

      integer n_max
      parameter ( n_max = 30 )
      integer nnzero_max
      parameter ( nnzero_max = 200 )

      integer colptr(n_max+1)
      character * ( * ) filename
      integer input
      integer ios
      integer ncol
      integer nnzero
      integer nrow
      integer rowind(nnzero_max)
      complex values(nnzero_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  C4_HB_QUICK_READ reads a sparse matrix '
      write ( *, '(a)' ) '  from a Harwell-Boeing file,'
      write ( *, '(a)' ) '  using complex single precision arithmetic.'
      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) 
     &  '  We assume NNZERO is no greater than ', nnzero_max
      write ( *, '(a,i6)' ) 
     &  '  We assume N is no greater than ', n_max
c
c  #1: Open the file.
c
      call get_unit ( input )
     
      open ( unit = input, file = filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TEST03 - Warning!'
        write ( *, '(a)' ) '  Could not open file "' // 
     &    trim ( filename ) // '".'
        return
      end if
c
c  #2: Read the information.
c
      call c4_hb_quick_read ( input, nrow, ncol, nnzero, values, 
     &  rowind, colptr )
c
c  #3: Print (some of) the information.
c
      call c4_hb_quick_print ( filename, nrow, ncol, nnzero, values, 
     &  rowind, colptr )
c
c  #4: Close the file.
c
      close ( unit = input )

      return
      end
      subroutine test04 ( filename )

c*********************************************************************72
c
cc TEST04 tests C8_HB_QUICK_READ.
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
c    Input, character * ( * ) FILENAME, the name of the HB file to read.
c
      implicit none

      integer n_max
      parameter ( n_max = 30 )
      integer nnzero_max
      parameter ( nnzero_max = 200 )

      integer colptr(n_max+1)
      character * ( * ) filename
      integer input
      integer ios
      integer ncol
      integer nnzero
      integer nrow
      integer rowind(nnzero_max)
      double complex values(nnzero_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  C8_HB_QUICK_READ reads a sparse matrix'
      write ( *, '(a)' ) '  from a Harwell-Boeing file,'
      write ( *, '(a)' ) '  using complex single precision arithmetic.'
      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) 
     &  '  We assume NNZERO is no greater than ', nnzero_max
      write ( *, '(a,i6)' ) 
     &  '  We assume N is no greater than ', n_max
c
c  #1: Open the file.
c
      call get_unit ( input )
     
      open ( unit = input, file = filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TEST04 - Warning!'
        write ( *, '(a)' ) '  Could not open file "' // 
     &    trim ( filename ) // '".'
        return
      end if
c
c  #2: Read the information.
c
      call c8_hb_quick_read ( input, nrow, ncol, nnzero, values, 
     &  rowind, colptr )
c
c  #3: Print (some of) the information.
c
      call c8_hb_quick_print ( filename, nrow, ncol, nnzero, values, 
     &  rowind, colptr )
c
c  #4: Close the file.
c
      close ( unit = input )

      return
      end
      subroutine test05 ( filename )

c*********************************************************************72
c
cc TEST05 tests R4_HB_HEADER_READ and R4_HB_DATA_READ.
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
c  Parameters:
c
c    Input, character * ( * ) FILENAME, the name of the HB file to read.
c
      implicit none

      character * ( * ) filename
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      integer ios
      integer ncol
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      integer valcrd
      character * ( 20 ) valfmt

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  R4_HB_HEADER_READ and R4_HB_DATA_READ'
      write ( *, '(a)' ) 
     &  '  read a sparse matrix from a Harwell-Boeing file,'
      write ( *, '(a)' ) '  using real single precision arithmetic.'
c
c  #1: Open the file.
c
      call get_unit ( input )
     
      open ( unit = input, file = filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TEST05 - Warning!'
        write ( *, '(a)' ) '  Could not open file "' // 
     &    trim ( filename ) // '".'
        return
      end if
c
c  #2: Read the header.
c
      call r4_hb_header_read ( input, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )
c
c  #3: Print the header.
c
      call r4_hb_header_print ( filename, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )
c
c  #4: Call a subroutine so we can allocate space for arrays.
c
      call test05_sub ( indfmt, input, ncol, nnzero, nrhs, nrhsix, 
     &  nrow, ptrfmt, rhscrd, rhsfmt, rhstyp, valcrd, valfmt )
c
c  #9: Close the file.
c
      close ( unit = input )

      return
      end
      subroutine test05_sub ( indfmt, input, ncol, nnzero, nrhs, nrhsix, 
     &  nrow, ptrfmt, rhscrd, rhsfmt, rhstyp, valcrd, valfmt )

c*********************************************************************72
c
cc TEST05_SUB is called by TEST05 to set up arrays.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer ncol
      integer nnzero
      integer nrhs
      integer nrow

      integer colptr(ncol+1)
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      integer ios
      integer nrhsix
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      real rhsval(nrow,nrhs)
      integer rowind(nnzero)
      integer valcrd
      character * ( 20 ) valfmt
      real values(nnzero)
c
c  #5: Read the structure and data.
c
      call r4_hb_data_read ( input, nrow, ncol, nnzero, rhstyp, nrhs, 
     &  nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt, colptr, 
     &  rowind, values, rhsval )
c
c  #6: Print (some of) the structure.
c
      call r4_hb_structure_print ( ncol, nnzero, colptr, rowind )
c
c  #7: Print (some of) the data.
c
      call r4_hb_values_print ( ncol, colptr, nnzero, values )
c
c  #8: Print (some of) the right hand sides.
c
      if ( 0 .lt. nrhs ) then
        call r4mat_print_some ( nrow, nrhs, rhsval, 1, 1, 10, 5, 
     &    '  10x5 portion of right hand sides:' )
      end if

      return
      end
      subroutine test06 ( filename )

c*********************************************************************72
c
cc TEST06 tests R8_HB_HEADER_READ and R8_HB_DATA_READ.
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
c    Input, character * ( * ) FILENAME, the name of the HB file to read.
c
      implicit none

      character * ( * ) filename
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      integer ios
      integer ncol
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      integer valcrd
      character * ( 20 ) valfmt

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  R8_HB_HEADER_READ and R8_HB_DATA_READ'
      write ( *, '(a)' ) 
     &  '  read a sparse matrix from a Harwell-Boeing file'
      write ( *, '(a)' ) '  using real double precision arithmetic.'
c
c  #1: Open the file.
c
      call get_unit ( input )
     
      open ( unit = input, file = filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TEST06 - Warning!'
        write ( *, '(a)' ) '  Could not open file "' // 
     &    trim ( filename ) // '".'
        return
      end if
c
c  #2: Read the header.
c
      call r8_hb_header_read ( input, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )
c
c  #3: Print the header.
c
      call r8_hb_header_print ( filename, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )
c
c  #4: Call a subroutine so we can allocate space for arrays.
c
      call test06_sub ( indfmt, input, ncol, nnzero, nrhs, nrhsix, 
     &  nrow, ptrfmt, rhscrd, rhsfmt, rhstyp, valcrd, valfmt )
c
c  #9: Close the file.
c
      close ( unit = input )

      return
      end
      subroutine test06_sub ( indfmt, input, ncol, nnzero, nrhs, nrhsix, 
     &  nrow, ptrfmt, rhscrd, rhsfmt, rhstyp, valcrd, valfmt )

c*********************************************************************72
c
cc TEST06_SUB is called by TEST06 to set up arrays.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer ncol
      integer nnzero
      integer nrhs
      integer nrow

      integer colptr(ncol+1)
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      integer ios
      integer nrhsix
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      double precision rhsval(nrow,nrhs)
      integer rowind(nnzero)
      integer valcrd
      character * ( 20 ) valfmt
      double precision values(nnzero)
c
c  #5: Read the structure and data.
c
      call r8_hb_data_read ( input, nrow, ncol, nnzero, rhstyp, nrhs, 
     &  nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt, colptr, 
     &  rowind, values, rhsval )
c
c  #6: Print (some of) the structure.
c
      call r8_hb_structure_print ( ncol, nnzero, colptr, rowind )
c
c  #7: Print (some of) the data.
c
      call r8_hb_values_print ( ncol, colptr, nnzero, values )
c
c  #8: Print (some of) the right hand sides.
c
      if ( 0 .lt. nrhs ) then
        call r8mat_print_some ( nrow, nrhs, rhsval, 1, 1, 10, 5, 
     &    '  10x5 portion of right hand sides:' )
      end if

      return
      end
      subroutine test07 ( filename )

c*********************************************************************72
c
cc TEST07 tests C4_HB_HEADER_READ and C4_HB_DATA_READ.
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
c    Input, character * ( * ) FILENAME, the name of the HB file to read.
c
      implicit none

      character * ( * ) filename
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      integer ios
      integer ncol
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      integer valcrd
      character * ( 20 ) valfmt

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  C4_HB_HEADER_READ and C4_HB_DATA_READ'
      write ( *, '(a)' ) 
     &  '  read a sparse matrix from a Harwell-Boeing file,'
      write ( *, '(a)' ) '  using complex single precision arithmetic.'
c
c  #1: Open the file.
c
      call get_unit ( input )
     
      open ( unit = input, file = filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TEST07 - Warning!'
        write ( *, '(a)' ) '  Could not open file "' // 
     &    trim ( filename ) // '".'
        return
      end if
c
c  #2: Read the header.
c
      call c4_hb_header_read ( input, nrow, ncol, nnzero, rhstyp, nrhs, 
     &  nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )
c
c  #3: Print the header.
c
      call c4_hb_header_print ( filename, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )
c
c  #4: Call a subroutine so we can allocate space for arrays.
c
      call test07_sub ( indfmt, input, ncol, nnzero, nrhs, nrhsix, 
     &  nrow, ptrfmt, rhscrd, rhsfmt, rhstyp, valcrd, valfmt )
c
c  #9: Close the file.
c
      close ( unit = input )

      return
      end
      subroutine test07_sub ( indfmt, input, ncol, nnzero, nrhs, nrhsix, 
     &  nrow, ptrfmt, rhscrd, rhsfmt, rhstyp, valcrd, valfmt )

c*********************************************************************72
c
cc TEST07_SUB is called by TEST07 to set up arrays.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer ncol
      integer nnzero
      integer nrhs
      integer nrow

      integer colptr(ncol+1)
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      integer ios
      integer nrhsix
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      complex rhsval(nrow,nrhs)
      integer rowind(nnzero)
      integer valcrd
      character * ( 20 ) valfmt
      complex values(nnzero)
c
c  #5: Read the structure and data.
c
      call c4_hb_data_read ( input, nrow, ncol, nnzero, rhstyp, nrhs, 
     &  nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt, colptr, 
     &  rowind, values, rhsval )
c
c  #6: Print (some of) the structure.
c
      call c4_hb_structure_print ( ncol, nnzero, colptr, rowind )
c
c  #7: Print (some of) the data.
c
      call c4_hb_values_print ( ncol, colptr, nnzero, values )
c
c  #8: Print (some of) the right hand sides.
c
      if ( 0 .lt. nrhs ) then
        call c4mat_print_some ( nrow, nrhs, rhsval, 1, 1, 10, 5, 
     &    '  10x5 portion of right hand sides:' )
      end if

      return
      end
      subroutine test08 ( filename )

c*********************************************************************72
c
cc TEST08 tests C8_HB_HEADER_READ and C8_HB_DATA_READ.
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
c    Input, character * ( * ) FILENAME, the name of the HB file to read.
c
      implicit none

      character * ( * ) filename
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      integer ios
      integer ncol
      integer nnzero
      integer nrhs
      integer nrhsix
      integer nrow
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      integer valcrd
      character * ( 20 ) valfmt

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  C8_HB_HEADER_READ and C8_HB_DATA_READ'
      write ( *, '(a)' ) 
     &  '  read a sparse matrix from a Harwell-Boeing file,'
      write ( *, '(a)' ) '  using complex double precision arithmetic.'
c
c  #1: Open the file.
c
      call get_unit ( input )
     
      open ( unit = input, file = filename, status = 'old', 
     &  iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TEST08 - Warning!'
        write ( *, '(a)' ) '  Could not open file "' // 
     &    trim ( filename ) // '".'
        return
      end if
c
c  #2: Read the header.
c
      call c8_hb_header_read ( input, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )
c
c  #3: Print the header.
c
      call c8_hb_header_print ( filename, nrow, ncol, nnzero, rhstyp, 
     &  nrhs, nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt )
c
c  #4: Call a subroutine so we can allocate space for arrays.
c
      call test08_sub ( indfmt, input, ncol, nnzero, nrhs, nrhsix, 
     &  nrow, ptrfmt, rhscrd, rhsfmt, rhstyp, valcrd, valfmt )
c
c  #9: Close the file.
c
      close ( unit = input )

      return
      end
      subroutine test08_sub ( indfmt, input, ncol, nnzero, nrhs, nrhsix, 
     &  nrow, ptrfmt, rhscrd, rhsfmt, rhstyp, valcrd, valfmt )

c*********************************************************************72
c
cc TEST08_SUB is called by TEST08 to set up arrays.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer ncol
      integer nnzero
      integer nrhs
      integer nrow

      integer colptr(ncol+1)
      integer indcrd
      character * ( 16 ) indfmt
      integer input
      integer ios
      integer nrhsix
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      character * ( 3 ) rhstyp
      double complex rhsval(nrow,nrhs)
      integer rowind(nnzero)
      integer valcrd
      character * ( 20 ) valfmt
      double complex values(nnzero)
c
c  #5: Read the structure and data.
c
      call c8_hb_data_read ( input, nrow, ncol, nnzero, rhstyp, nrhs, 
     &  nrhsix, valcrd, rhscrd, ptrfmt, indfmt, valfmt, rhsfmt, colptr, 
     &  rowind, values, rhsval )
c
c  #6: Print (some of) the structure.
c
      call c8_hb_structure_print ( ncol, nnzero, colptr, rowind )
c
c  #7: Print (some of) the data.
c
      call c8_hb_values_print ( ncol, colptr, nnzero, values )
c
c  #8: Print (some of) the right hand sides.
c
      if ( 0 .lt. nrhs ) then
        call c8mat_print_some ( nrow, nrhs, rhsval, 1, 1, 10, 5, 
     &    '  10x5 portion of right hand sides:' )
      end if

      return
      end