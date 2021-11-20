      program main

c*********************************************************************72
c
cc MAIN is the main program for DLAP_IO_PRB.
c
c  Discussion:
c
c    DLAP_IO_PRB tests the DLAP_IO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DLAP_IO_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the DLAP_IO library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DLAP_IO_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests DLAP_FILE_WRITE.
c
c  Discussion:
c
c    The matrix is:
c
c      11  12   0   0  15
c      21  22   0   0   0
c       0   0  33   0  35
c       0   0   0  44   0
c      51   0  53   0  55
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer nelt
      parameter ( nelt = 11 )

      double precision a(nelt)
      integer ia(nelt)
      integer ios
      integer irhs
      integer isoln
      integer isym
      character * ( 80 ) output_file_name
      integer output_file_unit
      integer ja(nelt)
      double precision rhs(n)
      double precision soln(n)
      character * ( 80 ) title

      save a
      save ia
      save ja
      save rhs
      save soln

      data a /
     &  51.0D+00, 12.0D+00, 11.0D+00, 33.0D+00, 15.0D+00, 
     &  53.0D+00, 55.0D+00, 22.0D+00, 35.0D+00, 44.0D+00, 
     &  21.0D+00 /
      data ia /
     &   5,  1,  1,  3,  1,  5,  5,  2,  3,  4,  2 /
      data ja /
     &   1,  2,  1,  3,  5,  3,  5,  2,  5,  4,  1 /
      data rhs /
     &  110.0D+00, 65.0D+00, 274.0D+00, 176.0D+00, 485.0D+00 /
      data soln /
     &  1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /

      irhs = 1
      isoln = 1
      isym = 0
      output_file_name = 'a5by5.dlap'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  DLAP_FILE_WRITE writes a matrix in DLAP'
      write ( *, '(a)' ) '  Triad format to a DLAP sparse matrix file.'

      title = '  The DLAP data to be written to the file.'
      call dlap_file_print ( n, nelt, isym, irhs, isoln, ia, ja, a, rhs, 
     &  soln, title )

      call get_unit ( output_file_unit )

      open ( unit = output_file_unit, file = output_file_name, 
     &  status = 'replace', iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST01 - Fatal error!'
        write ( *, '(a)' ) '  Could not open the file "' 
     &    // trim ( output_file_name ) // '".'
        return
      end if

      call dlap_file_write ( n, nelt, isym, irhs, isoln, ia, ja, a, rhs, 
     &  soln, output_file_unit )

      close ( unit = output_file_unit )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Wrote the matrix data to "' 
     &  // trim ( output_file_name ) // '".'

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests DLAP_FILE_READ.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )
      integer nelt_max
      parameter ( nelt_max = 11 )

      double precision a(nelt_max)
      integer ia(nelt_max)
      character * ( 80 ) input_file_name
      integer input_file_unit
      integer ios
      integer irhs
      integer isoln
      integer isym
      integer ja(nelt_max)
      integer n
      integer nelt
      double precision rhs(n_max)
      double precision soln(n_max)

      input_file_name = 'a5by5.dlap'
      isym = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  DLAP_FILE_READ reads a matrix from a DLAP '
      write ( *, '(a)' ) '  sparse matrix file into DLAP Triad format.'

      call get_unit ( input_file_unit )

      open ( unit = input_file_unit, file = input_file_name, 
     &  status = 'old', iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST02 - Fatal error!'
        write ( *, '(a)' ) '  Could not open the file "' 
     &    // trim ( input_file_name ) // '".'
        return
      end if

      call dlap_file_read ( n_max, nelt_max, n, nelt, isym, irhs, isoln, 
     &  ia, ja, a, rhs, soln, input_file_unit )

      close ( unit = input_file_unit )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Read the matrix data from "' 
     &  // trim ( input_file_name ) // '".'

      call dlap_file_print ( n, nelt, isym, irhs, isoln, ia, ja, a, rhs, 
     &  soln, '  The DLAP data read from the file.' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests DLAP_FILE_WRITE.
c
c  Discussion:
c
c    The symmetric matrix is:
c
c      11   0  31   0  51
c       0  22   0   0   0
c      31   0  33  43   0
c       0   0  43  44  54
c      51   0   0  54  55
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer nelt
      parameter ( nelt = 9 )

      double precision a(nelt)
      integer ia(nelt)
      integer ios
      integer irhs
      integer isoln
      integer isym
      character * ( 80 ) output_file_name
      integer output_file_unit
      integer ja(nelt)
      double precision rhs(n)
      double precision soln(n)

      save a
      save ia
      save ja
      save rhs
      save soln

      data a /
     &  11.0D+00, 31.0D+00, 51.0D+00, 22.0D+00, 33.0D+00, 
     &  43.0D+00, 44.0D+00, 54.0D+00, 55.0D+00 /
      data ia /
     &  1, 3, 5, 2, 3, 4, 4, 5, 5 /
      data ja /
     &   1, 1, 1, 2, 3, 3, 4, 4, 5 /
      data rhs /
     &  359.0D+00, 44.0D+00, 302.0D+00, 575.0D+00, 542.0D+00 /
      data soln /
     &  1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /

      irhs = 1
      isoln = 1
      isym = 1
      output_file_name = 'a5by5_sym.dlap'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  DLAP_FILE_WRITE writes a matrix in DLAP Triad format'
      write ( *, '(a)' ) '  to a DLAP sparse matrix file.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In this example, symmetric storage is used.'

      call dlap_file_print ( n, nelt, isym, irhs, isoln, ia, ja, a, rhs, 
     &  soln, '  The DLAP data to be written to the file.' )

      call get_unit ( output_file_unit )

      open ( unit = output_file_unit, file = output_file_name, 
     &  status = 'replace', iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST03 - Fatal error!'
        write ( *, '(a)' ) '  Could not open the file "' 
     &    // trim ( output_file_name ) // '".'
        return
      end if

      call dlap_file_write ( n, nelt, isym, irhs, isoln, ia, ja, a, rhs, 
     &  soln, output_file_unit )

      close ( unit = output_file_unit )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Wrote the matrix data to "' 
     &  // trim ( output_file_name ) // '".'

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests DLAP_FILE_READ.
c
c  Discussion:
c
c    In this example, a matrix in symmetric storage will be read.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )
      integer nelt_max
      parameter ( nelt_max = 11 )

      double precision a(nelt_max)
      integer ia(nelt_max)
      character * ( 80 ) input_file_name
      integer input_file_unit
      integer ios
      integer irhs
      integer isoln
      integer isym
      integer ja(nelt_max)
      integer n
      integer nelt
      double precision rhs(n_max)
      double precision soln(n_max)

      input_file_name = 'a5by5_sym.dlap'
      isym = 0

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  DLAP_FILE_READ reads a matrix from a DLAP'
      write ( *, '(a)' ) '  sparse matrix file into DLAP Triad format.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In this example, symmetric storage is used.'

      call get_unit ( input_file_unit )

      open ( unit = input_file_unit, file = input_file_name, 
     &  status = 'old', iostat = ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST04 - Fatal error!'
        write ( *, '(a)' ) '  Could not open the file "' 
     &    // trim ( input_file_name ) // '".'
        return
      end if

      call dlap_file_read ( n_max, nelt_max, n, nelt, isym, irhs, isoln, 
     &  ia, ja, a, rhs, soln, input_file_unit )

      close ( unit = input_file_unit )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Read the matrix data from "' 
     &  // trim ( input_file_name ) // '".'

      call dlap_file_print ( n, nelt, isym, irhs, isoln, ia, ja, a, rhs, 
     &  soln, '  The DLAP data read from the file.' )

      return
      end

