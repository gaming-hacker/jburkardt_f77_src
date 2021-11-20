      program main

c*********************************************************************72
c
cc MAIN is the main program for FILE_NAME_SEQUENCE_PRB.
c
c  Discussion:
c
c    FILE_NAME_SEQUENCE_PRB tests the FILE_NAME_SEQUENCE library.
c
c    There are situations such as animations or parallel processing in which
c    it is necessary to generate a sequence of file names which include
c    an embedded index that increases.  A simple example might be
c
c      "fred0.txt", "fred1.txt", "fred2.txt"
c
c    A side issue arises when the number of files is large enough that the
c    number of digits in the index will vary.  Thus, if we are going to have
c    15 files, do we want to number them as
c
c      "fred00.txt" through "fred14.txt"
c
c    which means, for one thing, that they will alphabetize properly, or
c    will we be satisfied with
c
c      "fred0.txt" through "fred14.txt" ?
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 July 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      character * ( 80 ) filename

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FILE_NAME_SEQUENCE_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the FILE_NAME_SEQUENCE library.'

      filename = 'frodo_01345_lives.txt'
      call test04 ( filename, 10 )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FILE_NAME_SEQUENCE_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      return
      end
      subroutine test04 ( filename, filename_num )

c*********************************************************************72
c
cc  TEST04 uses FILENAME_INC.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 September 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      character * ( * ) filename
      integer filename_num
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04:'
      write ( *, '(a)' ) 
     &  '  FILENAME(I+1) = FILENAME_INC ( FILENAME(I) )'
      write ( *, '(a)' ) 
     &  '  First FILENAME = "' // trim ( filename ) // '".'
      write ( *, '(a,i4)' ) '  Number of filenames = ', filename_num
      write ( *, '(a)' ) '  Numbers may include leading zeros.'
      write ( *, '(a)' ) ' '

      do i = 1, filename_num
        write ( *, '(2x,i4,2x,a)' )  
     &    i, '"' // trim ( filename ) // '".'
        call filename_inc ( filename )
      end do

      return
      end
