c  SETPRB.F  18 January 1994
c
      program main

c*********************************************************************72
c
cc MAIN is the main program for SETMAT_PRB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 November 2008
c
c  Author:
c
c    John Burkardt
c
      call timestamp ( )
      write(*,*)' '
      write(*,*)'SETMAT_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write(*,*)'  Tests for SETMAT'
      write(*,*)' '
 
      call test01
      call test02
      call test03
      call test04
      call test05
      call test06
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SETMAT_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01

c*********************************************************************72
c
cc TEST01 tests symmetric triangular storage.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 November 2008
c
c  Author:
c
c    John Burkardt
c
      integer lda
      parameter (lda=10)

      integer nmax
      parameter (nmax=200)

      real a(lda,lda)
      real entry
      integer i
      integer ia(lda,3)
      integer istore(5)
      integer iwork(nmax)
      integer j
      integer ncol
      integer nrow
      integer nscale

      write(*,*)' '
      write(*,*)'TEST01'
      write(*,*)'  Demonstration of symmetric triangle storage.'
      write(*,*)' '
c
c  Zero out matrix
c
      istore(1)=0
      istore(2)=0
      istore(3)=0
      istore(4)=0
      istore(5)=0
      nrow=lda
      ncol=lda
      call zermat(a,ia,istore,lda,ncol,nrow)
c
c  Set entries of matrix
c
      istore(1)=6
      nrow=5
      ncol=5
 
      do i=1,nrow
        do j=1,i
          entry=10*i+j
          call setmat(a,ia,istore,lda,ncol,nrow,entry,i,j)
        enddo
      enddo
c
c  Print matrix, analyze it, and show its structure.
c
      call primat(a,ia,istore,lda,ncol,nrow)

      nscale=4
      call anamat(a,ia,istore,lda,ncol,nrow,nscale)

      call shomat(ia,istore,iwork,lda,ncol,nmax,nrow)

      return
      end
      subroutine test02

c*********************************************************************72
c
cc TEST02 tests upper triangle storage.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 November 2008
c
c  Author:
c
c    John Burkardt
c
      integer lda
      parameter (lda=10)

      real a(lda,lda)
      real entry
      integer i
      integer ia(lda,3)
      integer istore(5)
      integer j
      integer ncol
      integer nrow
      integer nscale

      write(*,*)' '
      write(*,*)'TEST02'
      write(*,*)'  Demonstration of upper triangle storage.'
      write(*,*)' '
c
c  Zero out matrix
c
      istore(1)=0
      istore(2)=0
      istore(3)=0
      istore(4)=0
      istore(5)=0
      nrow=lda
      ncol=lda
      call zermat(a,ia,istore,lda,ncol,nrow)
c
c  Set entries of matrix
c
      istore(1)=7
      nrow=5
      ncol=5
 
      do i=1,nrow
        do j=i,ncol
          entry=10*i+j
          call setmat(a,ia,istore,lda,ncol,nrow,entry,i,j)
        enddo
      enddo
c
c  Print and analyze matrix
c
      call primat(a,ia,istore,lda,ncol,nrow)

      nscale=4
      call anamat(a,ia,istore,lda,ncol,nrow,nscale)

      return
      end
      subroutine test03

c*********************************************************************72
c
cc TEST03 tests PRIMAT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 November 2008
c
c  Author:
c
c    John Burkardt
c
      integer lda
      parameter (lda=10)

      integer nmax
      parameter (nmax=200)

      real a(lda,lda)
      real entry
      integer i
      integer ia(lda,3)
      integer istore(5)
      integer iwork(nmax)
      integer j
      integer jhi
      integer jlo
      integer ml
      integer mu
      integer ncol
      integer nrow
      integer nscale

      write(*,*)' '
      write(*,*)'TEST03'
      write(*,*)'  Use SETMAT to store a band matrix.'
      write(*,*)'  Use PRIMAT to print it out.'

      istore(1)=2
      ml=1
      istore(2)=ml
      mu=2
      istore(3)=mu
      nrow=8
      ncol=8
 
      do i=1,nrow
        jlo=max(1,i-ml)
        jhi=min(i+mu,ncol)
        do j=jlo,jhi
          entry=j+2-i
          call setmat(a,ia,istore,lda,ncol,nrow,entry,i,j)
        enddo
      enddo
 
      call primat(a,ia,istore,lda,ncol,nrow)
      nscale=3
      call anamat(a,ia,istore,lda,ncol,nrow,nscale)
c
c  Now add 10 to diagonal
c
      do i=1,nrow
        call addmat(a,ia,istore,lda,ncol,nrow,10.0,i,i)
      enddo
 
      write(*,*)' '
      write(*,*)'ADDMAT adds 10 to the diagonal.'
      write(*,*)' '

      call primat(a,ia,istore,lda,ncol,nrow)
      call anamat(a,ia,istore,lda,ncol,nrow,nscale)
c
c  Now display storage scheme
c
      call shomat(ia,istore,iwork,lda,ncol,nmax,nrow)
      return
      end
      subroutine test04

c*********************************************************************72
c
cc TEST04 tests NORMAT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 November 2008
c
c  Author:
c
c    John Burkardt
c
      integer lda
      parameter (lda=10)

      integer nmax
      parameter (nmax=200)

      real a(lda,lda)
      integer ia(lda,3)
      integer istore(5)
      integer iwork(nmax)
      integer ncol
      integer nrow
      integer nscale
      real rhs(lda)

      write(*,*)' '
      write(*,*)'TEST04'
      write(*,*)'  Now we switch to a border-banded matrix.'
      write(*,*)'  ZERMAT zeroes out old information.'
      write(*,*)'  SETMAT stores new values.'
      write(*,*)'  PRISYS prints the matrix and right hand side.'
      write(*,*)'  NORMAT normalizes the matrix and a right hand side.'

      ncol=5
      nrow=5
      istore(1)=4
      istore(2)=1
      istore(3)=1
      istore(4)=4
      istore(5)=1
      rhs(1)=1.0
      rhs(2)=2.0
      rhs(3)=3.0
      rhs(4)=4.0
      rhs(5)=5.0
      call zermat(a,ia,istore,lda,ncol,nrow)

      call setmat(a,ia,istore,lda,ncol,nrow,-16.0,1,1)
      call setmat(a,ia,istore,lda,ncol,nrow,8.0,1,2)
      call setmat(a,ia,istore,lda,ncol,nrow,10.0,1,5)
      call setmat(a,ia,istore,lda,ncol,nrow,2.0,2,2)
      call setmat(a,ia,istore,lda,ncol,nrow,9.0,2,5)
      call setmat(a,ia,istore,lda,ncol,nrow,100.0,3,2)
      call setmat(a,ia,istore,lda,ncol,nrow,99.0,3,3)
      call setmat(a,ia,istore,lda,ncol,nrow,98.0,3,4)
      call setmat(a,ia,istore,lda,ncol,nrow,8.0,3,5)
      call setmat(a,ia,istore,lda,ncol,nrow,17.0,4,3)
      call setmat(a,ia,istore,lda,ncol,nrow,16.0,4,5)
      call setmat(a,ia,istore,lda,ncol,nrow,1.0,5,1)
      call setmat(a,ia,istore,lda,ncol,nrow,3.0,5,3)
      call setmat(a,ia,istore,lda,ncol,nrow,14.0,5,4)
      call setmat(a,ia,istore,lda,ncol,nrow,15.0,5,5)

      call prisys(a,ia,istore,lda,ncol,nrow,rhs)
      nscale=5
      call anamat(a,ia,istore,lda,ncol,nrow,nscale)
      write(*,*)' '
      write(*,*)'NORMAT row-normalizes the matrix and right hand side.'

      call normat(a,ia,istore,lda,ncol,nrow,rhs)
      call prisys(a,ia,istore,lda,ncol,nrow,rhs)
      call anamat(a,ia,istore,lda,ncol,nrow,nscale)
c
c  Now display the storage scheme.
c
      call shomat(ia,istore,iwork,lda,ncol,nmax,nrow)

      return
      end
      subroutine test05

c*********************************************************************72
c
cc TEST05 tests AXMUL on a LINPACK band matrix
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 November 2008
c
c  Author:
c
c    John Burkardt
c
      integer lda
      parameter (lda=10)

      real a(lda,lda)
      integer i
      integer ia(lda,3)
      integer istore(5)
      integer j
      integer ncol
      integer nrow
      integer nscale
      real x(10)
      real y(10)

      write(*,*)' '
      write(*,*)'TEST05'
      write(*,*)'  Demonstration of matrix-vector multiplication.'
      write(*,*)' '
c
c  Zero out matrix
c
      istore(1)=0
      istore(2)=0
      istore(3)=0
      istore(4)=0
      istore(5)=0
      nrow=lda
      ncol=lda
      call zermat(a,ia,istore,lda,ncol,nrow)
c
c  Set entries of matrix
c
      istore(1)=2
      istore(2)=1
      istore(3)=1
      nrow=5
      ncol=5
 
      do j=1,ncol
        x(j)=j
      enddo
 
      do i=1,nrow

        if(i-1.ge.1)
     &    call setmat(a,ia,istore,lda,ncol,nrow,-1.0,i,i-1)

        call setmat(a,ia,istore,lda,ncol,nrow,2.0,i,i)

        if(i+1.le.ncol)
     &    call setmat(a,ia,istore,lda,ncol,nrow,-1.0,i,i+1)

      enddo
c
c  Print matrix, analyze it, and show its structure.
c
      call primat(a,ia,istore,lda,ncol,nrow)
      nscale=4
      call anamat(a,ia,istore,lda,ncol,nrow,nscale)
c
c  Carry out the multiplication.
c
      call axmul(a,ia,istore,lda,ncol,nrow,x,y)
      write(*,*)' '
      write(*,*)'       X             A*X'
      write(*,*)' '
      do i=1,nrow
        write(*,'(2g14.6)')x(i),y(i)
      enddo
 
      return
      end
      subroutine test06

c*********************************************************************72
c
cc TEST06 tests DIFMAT on a compressed storage matrix
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 November 2008
c
c  Author:
c
c    John Burkardt
c
      integer lda
      parameter (lda=10)

      integer nmax
      parameter (nmax=200)

      real a(lda,lda)
      real fxback(lda)
      real fxplus(lda)
      integer i
      integer ia(lda,3)
      integer idif
      integer ierror
      integer istore(5)
      integer iwork(nmax)
      integer j
      integer ncol
      integer nonzer
      integer nrow
      integer nscale
      real x(lda)
      real xdelt(lda)

      external fxcomp

      write(*,*)' '
      write(*,*)'TEST06'
      write(*,*)'  Now use compressed storage mode.'
      write(*,*)'  DIFMAT computes the jacobian'
      write(*,*)'  of a function, which involves at most 3 variables.'

      istore(1)=5
      nonzer=3
      istore(2)=nonzer
      nrow=5
      ncol=5
c
c  Zero out the matrix
c
      call zermat(a,ia,istore,lda,ncol,nrow)
 
      do i=1,nrow
        ia(i,1)=0
        if(i-1.gt.0)ia(i,1)=i-1
        ia(i,2)=i
        ia(i,3)=0
        if(i+3.le.nrow)ia(i,3)=i+3
      enddo
 
      write(*,*)' '
      write(*,*)'Nonzero column indices:'
      write(*,*)' '
      do i=1,nrow
        write(*,'(3i6)')(ia(i,j),j=1,3)
      enddo
      write(*,*)' '
      idif=0
 
      do i=1,nrow
        x(i)=i
      enddo
 
      call difmat(a,ia,fxback,fxcomp,fxplus,idif,ierror,istore,
     &  lda,nrow,x,xdelt)
 
      call primat(a,ia,istore,lda,ncol,nrow)
      nscale=4
      call anamat(a,ia,istore,lda,ncol,nrow,nscale)
c
c  Now display storage scheme
c
      call shomat(ia,istore,iwork,lda,ncol,nmax,nrow)
      return
      end
      subroutine fxcomp(neqn,x,fx,ierror)

c*********************************************************************72
c
cc FXCOMP
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 November 2008
c
c  Author:
c
c    John Burkardt
c
      integer neqn

      real fx(neqn)
      integer i
      integer ierror
      real x(neqn)

      ierror=0
 
      do i=1,neqn
        fx(i)=0.0
        if(i.gt.1)fx(i)=fx(i)-x(i-1)
        fx(i)=fx(i)+2.0*x(i)
        if(i.le.neqn-3)fx(i)=fx(i)-x(i+3)
      enddo
 
      return
      end
 
 
