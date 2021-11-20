      program main

c*********************************************************************72
c
cc MAIN is the main program for ASA314_PRB.
c
c  Discussion:
c
c    ASA314_PRB tests the ASA314 library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Roger Payne,
c    Inversion of matrices with contents subject to modulo arithmetic,
c    Applied Statistics,
c    Volume 46, Number 2, 1997, pages 295-298.
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ASA314_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the ASA314 library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ASA314_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests INVMOD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Roger Payne,
c    Inversion of matrices with contents subject to modulo arithmetic,
c    Applied Statistics,
c    Volume 46, Number 2, 1997, pages 295-298.
c
      implicit none

      integer nrow
      parameter ( nrow = 3)

      integer cmod(nrow)
      integer i
      integer ifault
      integer imat(nrow,nrow)
      integer iwk(nrow,2)
      integer jmat(nrow,nrow)
      integer mat(nrow,nrow)
      integer rmod(nrow)

      save jmat
      save mat

      data jmat /
     &  1, 0, 0, 2, 1, 0, 1, 0, 1 /

      data mat /
     &  1, 0, 0, 1, 1, 0, 2, 0, 1 /

      do i = 1, nrow
        cmod(i) = 3
        rmod(i) = 3
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  INVMOD computes the inverse of a matrix'
      write ( *, '(a)' ) 
     &  '  whose elements are subject to modulo arithmetic.'

      call i4mat_print ( nrow, nrow, mat, 
     &  '  The matrix to be inverted:' )

      call invmod ( mat, imat, rmod, cmod, iwk, nrow, ifault )

      call i4mat_print ( nrow, nrow, imat, '  The computed inverse:' )

      call i4mat_print ( nrow, nrow, jmat, '  The correct inverse:' )

      return
      end

