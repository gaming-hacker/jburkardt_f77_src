c  SETMAT.F  14 June 1996
c
      subroutine addmat ( a, ia, istore, lda, ncol, nrow, entry, i, j )

c*********************************************************************72
c
cc ADDMAT adds the value ENTRY to the matrix A in position (I,J).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, real A(*), the NROW by NCOL matrix.
c
c    Input, integer IA(*,*), contains information about
c    nonzero entries, if A is a sparse matrix.
c
c    Input, integer ISTORE(5), contains information defining
c    the matrix.
c
c    Input, integer LDA, the leading dimension for the storage of A.
c
c    Input, integer NCOL, the number of columns in the matrix.
c
c    Input, integer NROW, the number of rows in the matrix.
c
c    Input, real ENTRY, the value to be added to the matrix
c    entry in row I, column J.
c
c    Input, integer I, J, the row and column of the entry
c    to which the value is to be added.
c
      implicit none

      real a(*)
      real entry
      integer i
      integer ia(*)
      integer indx
      integer istore(5)
      integer j
      integer lda
      integer ncol
      integer nrow

      if ( entry .eq. 0.0 ) then
        return
      end if
c
c  Check the row and column indices.
c
      call chekij ( i, j, ncol, nrow )
c
c  Get the index into the matrix.
c
      call legtwo ( i, ia, indx, istore, j, lda )
c
c  If the index is not positive, (I,J) is illegal.
c
      if ( indx .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ADDMAT - Fatal error!'
        write ( *, '(a)' ) '  Illegal array index value for (I,j).'
        stop
      end if
c
c  Add the entry.
c
      a(indx) = a(indx) + entry

      return
      end
      subroutine anamat(a,ia,istore,lda,ncol,nrow,nscale)

c*********************************************************************72
c
cc ANAMAT symbolically analyzes a given matrix.
c
c  Discussion:
c
c    The routine prints out a symbolic picture of the elements, with 
c    a '.' for zero elements, and then a '1', '2', ... etc up to an 
c    NSCALE-th character for the largest elements.
c
c    a is the matrix to be analyzed.  it has a declared leading
c    dimension of NROW, with an actual used dimension of n.
c    NSCALE intervals are to be used.  the least nonzero magnitude,
c    amin, and the greatest, amax, are used to construct the
c    NSCALE intervals.
c
c    At the moment, only the full storage mode is supported.
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A(*), the NROW by NCOL matrix.
c
      real a(*)
      real abssum
      real ahi
      real aij
      real aji
      real alo
      real amax
      real amin
      real atemp
      real dsum
      real dsummn
      real entry
      integer i
      integer ia(*)
      integer ianti
      integer idiag
      integer ihess
      integer ii
      integer ilo
      integer istore(5)
      integer isym
      integer iup
      integer izero
      integer j
      integer jhi
      integer jinc
      integer jj
      integer jjj
      integer jlo
      integer k
      integer ksave
      integer lda
      integer ml
      integer mu
      integer ncol
      integer nonzer
      integer nrow
      integer nscale
      character output*80

      intrinsic char
c
c  Find maximum and minimum nonzero magnitude
c
      amax=0.0
      amin=0.0
 
      do i=1,nrow
        do j=1,ncol
 
          call getmat(a,ia,istore,lda,ncol,nrow,entry,i,j)
          atemp=abs(entry)
          if(atemp.gt.0.0.and.amin.eq.0.0)amin=atemp
          if(atemp.gt.0.0.and.atemp.lt.amin)amin=atemp
          if(atemp.gt.amax)amax=atemp
 
        end do
      end do
 
      jinc=70
      write(*,*)' '
      write(*,*)'ANAMAT analyzes matrix structure.'
      write(*,*)'using ',nscale,' intervals.'
      write(*,*)'AMIN=',amin,' AMAX=',amax
      write(*,*)' '
c
c  Create the symbolic picture of the matrix in OUTPUT
c
      do jlo=1,ncol,jinc
 
        jhi=min(jlo+jinc-1,ncol)

        write(*,*)'Columns ',jlo,' to ',jhi
        write(*,*)' '
 
        do i=1,nrow
 
          ii=i
          output=' '
 
          do j=jlo,jhi
 
            jj=j
            jjj=j+1-jlo
            call getmat(a,ia,istore,lda,ncol,nrow,entry,ii,jj)
            output(jjj:jjj)='.'
 
            if(amax.ne.0.0.and.entry.ne.0.0)then
 
              atemp=abs(entry)
              ksave=1
 
              do k=2,nscale
                alo=((k-1)*amax+(nscale+1-k)*amin)/nscale
                ahi=(k*amax+(nscale-k)*amin)/nscale
                if(alo.le.atemp.and.atemp.le.ahi)ksave=k
              end do
 
              output(jjj:jjj)=char(48+ksave)
 
            endif
 
          end do
 
          write(*,'(a80)')output
 
        end do
      end do

      write(*,*)' '
c
c  First set switches
c
      abssum=0.0
      dsummn=1.0
      ianti=1
      idiag=1
      ihess=1
      ilo=1
      isym=1
      iup=1
      izero=1
      ml=0
      mu=0
      nonzer=0
 
      do i=1,nrow
 
        ii=i
        dsum=0.0
 
        do j=1,ncol
 
          jj=j
          call getmat(a,ia,istore,lda,ncol,nrow,aij,ii,jj)
          abssum=abssum+abs(aij)
 
          if(aij.ne.0.0)then
            nonzer=nonzer+1
            izero=0
            if(i.ne.j)idiag=0
            if(i.lt.j)ilo=0
            if(i.gt.j)iup=0
            if(i.gt.j+1)ihess=0
            if(i-j.gt.ml)ml=i-j
            if(j-i.gt.mu)mu=j-i
          endif
 
          if(i.eq.j)then
            dsum=dsum+abs(aij)
          else
            dsum=dsum-abs(aij)
          endif
 
          call getmat(a,ia,istore,lda,ncol,nrow,aji,jj,ii)
          if(aij.ne.aji)isym=0
          if(aij.ne.-aji)ianti=0
 
        end do
 
        if(dsum.lt.dsummn)dsummn=dsum
 
      end do
 
      if(istore(1).eq.0)then
        write(*,*)'Full dense storage is used.'
      elseif(istore(1).eq.1)then
        write(*,*)'Full banded storage used.'
      elseif(istore(1).eq.2)then
        write(*,*)'LINPACK banded storage used.'
      elseif(istore(1).eq.3)then
        write(*,*)'LSODI banded storage used.'
      elseif(istore(1).eq.4)then
        write(*,*)'Border banded storage used.'
      elseif(istore(1).eq.5)then
        write(*,*)'Compressed storage used.'
      elseif(istore(1).eq.6)then
        write(*,*)'Symmetric triangle storage.'
      elseif(istore(1).eq.7)then
        write(*,*)'Upper triangle storage.'
      else
        write(*,*)'Unknown storage method!'
      endif
 
      write(*,*)'Sum of absolute values of entries=',abssum
      write(*,*)'Matrix has ',nonzer,' nonzero entries.'
 
      if(izero.eq.1)then
        write(*,*)'The matrix is null (all zero).'
        return
      endif
 
      if(idiag.eq.1)then
        write(*,*)'The matrix is diagonal.'
        return
      endif
 
      if(iup.eq.1)then
        write(*,*)'The matrix is upper triangular.'
      else
        write(*,*)'The matrix is not upper triangular.'
      endif
 
      if(ilo.eq.1)then
        write(*,*)'The matrix is lower triangular.'
      else
        write(*,*)'The matrix is not lower triangular.'
      endif
 
      if(ihess.eq.1)then
        write(*,*)'The matrix is upper Hessenberg.'
      else
        write(*,*)'The matrix is not upper Hessenberg.'
      endif
 
      if(isym.eq.1)then
        write(*,*)'The matrix is symmetric.'
      else
        write(*,*)'The matrix is not symmetric.'
      endif
 
      if(ianti.eq.1)then
        write(*,*)'The matrix is antisymmetric.'
      else
        write(*,*)'The matrix is not antisymmetric.'
      endif
 
      if(dsummn.gt.0.0)then
        write(*,*)'The matrix is strictly diagonally dominant.'
      elseif(dsummn.eq.0.0)then
        write(*,*)'The matrix is weakly diagonally dominant.'
      else
        write(*,*)'The matrix is not diagonally dominant.'
      endif
 
      if(mu.eq.ncol-1.and.ml.eq.nrow-1)then
        write(*,*)'The matrix is not banded.'
      else
        write(*,*)'The matrix is banded.'
        write(*,*)'Upper bandwidth MU=',mu
        write(*,*)'Lower bandwidth ML=',ml
      endif
 
      return
      end
      subroutine axmul(a,ia,istore,lda,ncol,nrow,x,y)

c*********************************************************************72
c
cc AXMUL computes y=A*x.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A(*), the NROW by NCOL matrix.
c
      integer ncol
      integer nrow

      real a(*)
      integer i
      integer ia(*)
      integer indx
      integer istore(5)
      integer j
      integer lda
      real x(ncol)
      real y(nrow)
c
c  For now, do it the clumsy way
c
      do i=1,nrow
 
        y(i)=0.0
 
	do j = 1, ncol
 
          call legtwo ( i, ia, indx, istore, j, lda )
 
	  if ( indx .gt. 0 ) then
	    y(i)=y(i)+a(indx)*x(j)
	  end if
 
        end do
      end do
 
      return
      end
      function chrrel ( rval )

c*********************************************************************72
c
cc CHRREL converts a real number to a right-justified string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, REAL RVAL, a real number.
c
c    Output (through function value), CHARACTER*14 CHRREL,
c    a right-justified character variable containing the representation of
c    RVAL, using a G14.6 format.
c
      character chrrel*14
      character chrtmp*14
      real rval
c
c  We can't seem to write directly into CHRREL because of compiler
c  quibbles.
c
      write(chrtmp,'(g14.6)')rval
      chrrel=chrtmp
 
      return
      end
      subroutine difmat ( a, ia, fxback, fxname, fxplus, idif, ierror,
     &  istore, lda, neqn, x, xdelt )

c*********************************************************************72
c
cc DIFMAT computes the Jacobian of a function.
c
c  Discussion:
c
c    The routine uses backward, centered, or forward differences.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, real A(*), an NROW by NCOL matrix, into which the
c    results of the computation are to be stored.  the
c    format used to store the results is determined by
c    the value of ISTORE.
c 
c    ia, dimension it to (1,1) unless storage mode 5 is used.
c    in that case only, dimension it (NROW,NCOL) and set
c    it to record the nonzeroes in each row.  see remarks
c    at beginning of file.
c 
c    fxback, an array of dimension NCOL, used for the computation.
c    on return, if idif.eq.1 or -1, fx contains f(x).
c 
c    fxname, an external quantity, the name of a user written
c    subroutine which, for any input vector x, evaluates
c    the NCOL functions f(x).  fxname has the form
c 
c      SUBROUTINE fxname(NCOL,x,fx,IERROR)
c      dimension x(NCOL),fx(neqn)
c 
c      IERROR is an error switch which is not set by DIFMAT
c      but which may be set by fxname to a nonzero value.
c      on return from fxname with a nonzero value of IERROR,
c      DIFMAT will abort the computation and return to the calling
c      program.
c
c    fxplus, an array of dimension NCOL, used for the computation.
c
c    idif, backward, centered, or forward difference switch.
c    idif=-1, backward differences are used.
c    idif= 0, centered differences are used.
c    idif=+1, forward differences are used.
c 
c    IERROR, a flag which is not set by DIFMAT, but by
c    fxname.  if fxname is called and
c    returns a nonzero value of IERROR, DIFMAT
c    aborts the computation and returns.
c 
c    ISTORE, vector of dimension 5 containing information about
c    the storage scheme used for the matrix.
c
c    NCOL, the number of variables.
c 
c    NROW, the formal first dimension of a.
c 
c    x an array of dimension NCOL, the point at which
c    the jacobian is to be evaluated.
c
c    XDELT, an array of dimension NCOL, used as
c    workspace by the program.
c
      external fxname

      integer neqn

      real a(*)
      real dmax
      real entry
      real epmach
      real fxback(neqn)
      real fxplus(neqn)
      real h
      integer i
      integer ia(*)
      integer iband
      integer icopy
      integer idif
      integer ierror
      integer istore(5)
      integer j
      integer jcopy
      integer kcall
      integer lda
      integer mcalls
      integer ml
      integer mu
      real rlsqrt
      real temp
      real x(neqn)
      real xdelt(neqn)
      real xnorm

      iband=istore(1)
      ml=istore(2)
      mu=istore(3)
      epmach=1.0
10    continue
      epmach=0.5*epmach
      call addit(1.0,epmach,temp)
      if(temp.gt.1.0)go to 10
      epmach=2.0*epmach
      rlsqrt=sqrt(epmach)
c
c  Except for centered differences, get FX at X
c
      if(idif.eq.1)then
        call fxname(neqn,x,fxback,ierror)
        if(ierror.ne.0)go to 130
      elseif(idif.eq.-1)then
        call fxname(neqn,x,fxplus,ierror)
        if(ierror.ne.0)go to 130
      endif
c
c  Compute how many calls to the function routine are required
c
      mcalls=neqn
      if(iband.eq.1.or.iband.eq.2.or.iband.eq.3)mcalls=ml+1+mu
c
c  Computation of approximate Jacobian
c
      do kcall=1,mcalls
c
c  For centered or forward differences, increment unrelated X'S
c
        if(idif.eq.0.or.idif.eq.1)then
 
          do j=1,neqn
            xdelt(j)=x(j)
          end do
 
          do j=kcall,neqn,mcalls
            xnorm=abs(x(j))
            h=epmach
            if(xnorm.ge.rlsqrt)h=rlsqrt
            if(xnorm.ge.1.0)h=rlsqrt*xnorm
            xdelt(j)=xdelt(j)+h
          end do
 
          call fxname(neqn,xdelt,fxplus,ierror)
          if(ierror.ne.0)go to 130
 
        endif
c
c  For backward or centered differences, decrement unrelated X'S
c
        if(idif.eq.-1.or.idif.eq.0)then
 
          do j=1,neqn
            xdelt(j)=x(j)
          end do
 
          do j=kcall,neqn,mcalls
            xnorm=abs(x(j))
            h=epmach
            if(xnorm.ge.rlsqrt)h=rlsqrt
            if(xnorm.ge.1.0)h=rlsqrt*xnorm
            xdelt(j)=xdelt(j)-h
          end do
 
          call fxname(neqn,xdelt,fxback,ierror)
          if(ierror.ne.0)go to 130
          endif
c
c  Take difference and check norm
c
        dmax=0.0
 
        do i=1,neqn
 
          fxplus(i)=fxplus(i)-fxback(i)
 
          if(abs(fxplus(i)).ge.dmax)then
            dmax=abs(fxplus(i))
          endif
 
        end do
 
        if(dmax.eq.0.0)then
          write(*,1020)kcall
          write(*,1030)
        endif
c
c  Compute Jacobian columns
c
        do j=kcall,neqn,mcalls
 
          jcopy=j
          if(idif.eq.-1)h=x(j)-xdelt(j)
          if(idif.eq.0)h=2.0*(x(j)-xdelt(j))
          if(idif.eq.1)h=xdelt(j)-x(j)
 
          do i=1,neqn
            icopy=i
            entry=fxplus(i)/h
            call setmat(a,ia,istore,lda,neqn,neqn,entry,icopy,jcopy)
          end do
 
        end do
 
      end do
 
      return
c
c  Nonzero IERROR from FXNAME
c
130   continue
      write(*,*)ierror
      write(*,*)'DIFMAT - aborted computation.'
      return
 
1000  format(' DIFMAT - Received nonzero value of IERROR=',i6)
1020  format(' DIFMAT - On step ',i6,' of jacobian generation')
1030  format(' DIFMAT - I computed a zero column')
      end
      subroutine addit(a,b,c)

c*********************************************************************72
c
cc ADDIT adds two real numbers.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      real a
      real b
      real c

      c=a+b
 
      return
      end
      subroutine getmat(a,ia,istore,lda,ncol,nrow,entry,i,j)

c*********************************************************************72
c
cc GETMAT returns ENTRY = A(I,J).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A(*), the NROW by NCOL matrix.
c
      real a(*)
      real entry
      integer i
      integer ia(*)
      integer indx
      integer istore(5)
      integer j
      integer lda
      integer ncol
      integer nrow
c
c  Check the row and column indices.
c
      call chekij ( i, j, ncol, nrow ) 
c
c  Get index into matrix
c
      call legtwo ( i, ia, indx, istore, j, lda )
 
      if ( indx .gt. 0 ) then
        entry = a(indx)
      else
        entry = 0.0
      end if
 
      return
      end
      subroutine chekij(i,j,ncol,nrow)

c*********************************************************************72
c
cc CHEKIJ checks the matrix indices I and J.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c   Input, integer I, J, the matrix indices to check.
c
c   Input, integer NCOL, the number of matrix columns.
c
c   Input, integer NROW, the number of matrix rows.
c
      integer i
      integer j
      integer ncol
      integer nrow

      if(i.lt.1)then
        write(*,*)'CHEKIJ - Fatal error!'
        write(*,*)'The matrix row index I=',i
        write(*,*)'which is less than 1.'
        stop
      elseif(i.gt.nrow)then
        write(*,*)'CHEKIJ - Fatal error!'
        write(*,*)'The matrix row index I=',i
        write(*,*)'which is greater than NROW=',nrow
        stop
      elseif(j.lt.1)then
        write(*,*)'CHEKIJ - Fatal error!'
        write(*,*)'The matrix column index J=',j
        write(*,*)'which is less than 1.'
        stop
      elseif(i.gt.nrow)then
        write(*,*)'CHEKIJ - Fatal error!'
        write(*,*)'The matrix column index J=',j
        write(*,*)'which is greater than NCOL=',ncol
        stop
      endif

      return
      end
      subroutine legtwo(i,ia,indx,istore,j,lda)

c*********************************************************************72
c
cc LEGTWO determines the vector index corresponding to an array index.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      integer lda

      integer i
      integer ia(lda,1)
      integer indx
      integer istore(5)
      integer j
      integer k
      integer ml
      integer mu
      integer n1
      integer n2
      integer nonzer
c
c  Dense matrix, full storage.
c
      if(istore(1).eq.0)then

        indx=lda*(j-1)+i
c
c  Banded, full
c
      elseif(istore(1).eq.1)then

        ml=istore(2)
        mu=istore(3)

        if((j-i).gt.mu.or.(i-j).gt.ml)then
          indx=-1
        else
          indx=lda*(j-1)+i
        endif
c
c  Banded, LINPACK
c
      elseif(istore(1).eq.2)then
 
        ml=istore(2)
        mu=istore(3)

        if((j-i).gt.mu.or.(i-j).gt.ml)then
          indx=-1
        else
          indx=(2*ml+mu+1)*(j-1)+i-j+ml+mu+1
        endif
c
c  Banded, LSODI
c
      elseif(istore(1).eq.3)then
 
        ml=istore(2)
        mu=istore(3)

        if((j-i).gt.mu.or.(i-j).gt.ml)then
          indx=-1
        else
          indx=lda*(j-1)+i-j+mu+1
        endif
c
c  Border banded
c
      elseif(istore(1).eq.4)then
 
        ml=istore(2)
        mu=istore(3)
        n1=istore(4)
        n2=istore(5)
 
        if(i.le.n1.and.j.le.n1)then
 
          if((j-i).gt.mu.or.(i-j).gt.ml)then
            indx=-1
          else
            indx=(2*ml+mu+1)*(j-1)+(i-j+ml+mu+1)
          endif
 
        elseif(i.le.n1.and.j.gt.n1)then
          indx=(2*ml+mu+1)*n1+(j-n1-1)*n1+i
        elseif(i.gt.n1.and.j.le.n1)then
          indx=(2*ml+mu+1)*n1+n2*n1+(j-1)*n2+(i-n1)
        else
          indx=(2*ml+mu+1)*n1+2*n1*n2+(j-n1-1)*n2+(i-n1)
        endif
c
c  Compressed storage
c
      elseif(istore(1).eq.5)then
 
        nonzer=istore(2)
 
        do k=1,nonzer
 
          if(ia(i,k).eq.j)then
            indx=(i-1)*nonzer+k
            go to 10
          endif
 
        end do
 
        indx=-1

10      continue
c
c  Symmetric triangle
c
      elseif(istore(1).eq.6)then
 
        if(i.ge.j)then
          indx=j+i*(i-1)/2
        else
          indx=i+j*(j-1)/2
        endif
c
c  Upper triangle
c
      elseif(istore(1).eq.7)then
 
        if(i.le.j)then
          indx=i+j*(j-1)/2
        else
          indx=-1
        endif
c
c  Unrecognized format
c
      else

        write(*,*)'LEGTWO - Fatal error!'
        write(*,*)'The matrix format ISTORE(1)=',istore(1)
        write(*,*)'was not recognized!'
        stop

      endif
 
      return
      end
      subroutine normat(a,ia,istore,lda,ncol,nrow,rhs)

c*********************************************************************72
c
cc NORMAT normalizes a matrix.
c
c  Discussion:
c
c    This routine normalizes a matrix and the right hand side by dividing
c    each row by the maximum row entry.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A(*), the NROW by NCOL matrix.
c
      integer lda
      integer ncol
      integer nrow

      real a(*)
      real aij
      real aimax
      integer i
      integer ia(*)
      integer iband
      integer irow
      integer istore(5)
      integer j
      integer jcol
      integer jhi
      integer jlo
      integer ml
      integer mu
      real rhs(nrow)

      iband=istore(1)
      ml=istore(2)
      mu=istore(3)
 
      do i=1,nrow
 
        irow=i
        aimax=0.0
        jlo=1
        if(iband.ge.1.and.iband.le.3)jlo=max(jlo,irow-ml)
        jhi=ncol
        if(iband.ge.1.and.iband.le.3)jhi=min(jhi,irow+mu)
 
        do j=jlo,jhi
          jcol=j
          call getmat(a,ia,istore,lda,ncol,nrow,aij,irow,jcol)
          aij=abs(aij)
          if(aimax.lt.aij)aimax=aij
        end do
 
        if(aimax.ne.0.0)then
 
          do j=jlo,jhi
            jcol=j
            call getmat(a,ia,istore,lda,ncol,nrow,aij,irow,jcol)
            aij=aij/aimax
            call setmat(a,ia,istore,lda,ncol,nrow,aij,irow,jcol)
          end do
 
          rhs(irow)=rhs(irow)/aimax
 
        endif
 
      end do
 
      return
      end
      subroutine prbmat(afl,ihi,ilo,jhi,jlo,ldafl,neqnfl,nlband)

c*********************************************************************72
c
cc PRBMAT prints a block of entries from a band matrix.
c
c  Discussion:
c
c    The routine prints all nonzero entries of rows ILO to IHI, columns JLO to 
c    JHI of a square band matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision AFL(LDAFL,MAXNFL).
c    If Newton iteration is being carried out, then AFL contains the 
c    Jacobian matrix for the full system.  If Picard iteration is
c    being carried out, AFL contains the Picard matrix.
c    AFL is stored in LINPACK general band storage mode, with
c    dimension 3*NBANDL+1 by NEQNFL.
c
c    Input, integer IHI, ILO, JHI, JLO.
c    PRMAT is to print all nonzero entries in rows ILO through IHI,
c    and columns JLO through JHI, of the matrix AFL.
c
c    Input, integer LDAFL.
c    LDAFL is the first dimension of the matrix AFL as declared in
c    the main program.  LDAFL must be at least 3*NLBAND+1.
c
c    Input, integer NEQNFL.
c    NEQNFL is the number of equations (and coefficients) in the full 
c    finite element system.
c
c    Input, integer NLBAND, the lower bandwidth of the matrix AFL.  
c    The zero structure of AFL is assumed to be symmetric, and so 
c    NLBAND is also the upper bandwidth of AFL.  
c 
      implicit double precision (a-h,o-z)
      integer incx
      parameter (incx=5)

      integer ldafl
      integer neqnfl

      double precision afl(ldafl,neqnfl)
      character*14 ctemp(incx)
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
      integer nlband

      do j2lo=jlo,jhi,incx
 
        j2hi=j2lo+incx-1
        j2hi=min(j2hi,neqnfl)
        j2hi=min(j2hi,jhi)
 
        inc=j2hi+1-j2lo
 
        write(*,*)' '
        write(*,*)'Columns ',j2lo,' to ',j2hi
        write(*,*)'  Row'
        write(*,*)' '

        i2lo=ilo
        i2lo=max(ilo,1)
        i2lo=max(j2lo-nlband,i2lo)

        i2hi=ihi
        i2hi=min(ihi,neqnfl)
        i2hi=min(j2hi+nlband,i2hi)
 
        do i=i2lo,i2hi
 
          do j2=1,inc
 
            j=j2lo-1+j2
 
            if(i-j.le.nlband.and.j-i.le.nlband)then
              write(ctemp(j2),'(g14.6)')afl(i-j+2*nlband+1,j)
              if(afl(i-j+2*nlband+1,j).eq.0.0)ctemp(j2)='    0.0'
            else
              ctemp(j2)='              '
            endif
 
          end do
 
          write(*,'(i5,1x,5a14)')i,(ctemp(j2),j2=1,inc)
 
        end do
 
      end do
 
      write(*,*)' '
 
      return
      end
      subroutine prdmat(a,ihi,ilo,jhi,jlo,lda,ncol,nrow)

c*********************************************************************72
c
cc PRDMAT prints out a portion of a dense matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A(LDA,NCOL), the NROW by NCOL matrix.
c
c    Input, INTEGER IHI, ILO.
c    ILO is the first and IHI the last row to print.
c
c    Input, INTEGER JHI, JLO.
c    JLO is the first, and JHI the last column to print.
c
c    Input, INTEGER LDA.
c    LDA is the leading dimension of A.
c
c    Input, INTEGER NCOL, NROW.
c    NROW is the number of rows, and NCOL the number of columns
c    in the matrix A.
c
      integer incx
      parameter (incx=5)

      real a(lda,ncol)
      character chrrel*14
      character ctemp(incx)*14
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
      integer lda
      integer ncol
      integer nrow

      write(*,*)' '

      do j2lo=jlo,jhi,incx
 
        j2hi=j2lo+incx-1
        j2hi=min(j2hi,ncol)
        j2hi=min(j2hi,jhi)
 
        inc=j2hi+1-j2lo
 
        write(*,*)' '
        write(*,*)'Columns ',j2lo,' to ',j2hi
        write(*,*)'  Row'
        write(*,*)' '
 

        i2lo=max(ilo,1)
        i2hi=min(ihi,nrow)
 
        do i=i2lo,i2hi
 
          do j2=1,inc
 
            j=j2lo-1+j2
            
            ctemp(j2)=chrrel(a(i,j))
            if(a(i,j).eq.0.0)ctemp(j2)='    0.0'
 
          end do
 
          write(*,'(i5,1x,5a14)')i,(ctemp(j),j=1,inc)
 
        end do
 
      end do
 
      write(*,*)' '
 
      return
      end
      subroutine primat(a,ia,istore,lda,ncol,nrow)

c*********************************************************************72
c
cc PRIMAT prints a matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A(*), the NROW by NCOL matrix.
c
      real a(*)
      character chrrel*14
      character ctemp(5)*14
      real entry
      integer i
      integer ia(*)
      integer iband
      integer icopy
      integer ihi
      integer ilo
      integer inc
      integer incx
      integer indx
      integer istore(5)
      integer j
      integer jcopy
      integer jhi
      integer jlo
      integer lda
      integer ml
      integer mu
      integer ncol
      integer nrow

      iband=istore(1)
      ml=istore(2)
      mu=istore(3)
      incx=5
      write(*,*)' '

      do jlo=1,ncol,incx
 
        jhi=min(jlo+incx-1,ncol)
        inc=min(incx,ncol+1-jlo)
        write(*,*)' '
        write(*,*)'Columns ',jlo,' to ',jhi
        write(*,*)'  Row'
        write(*,*)' '
        ilo=1
        ihi=nrow

        if(iband.ge.1.and.iband.le.3)then
          ilo=max(jlo-mu,1)
          ihi=min(jhi+ml,nrow)
        endif
 
        do i=ilo,ihi
 
          icopy=i
 
          do j=1,inc
 
            jcopy=jlo-1+j
            call legtwo(icopy,ia,indx,istore,jcopy,lda)
 
            if(indx.le.0)then
              ctemp(j)=' '
            else
              entry=a(indx)
              ctemp(j)=chrrel(entry)
              if(entry.eq.0.0)ctemp(j)='    0.0'
            endif
 
          end do
 
          if(inc.lt.incx)then
 
            do j=inc+1,5
              ctemp(j)=' '
            end do
 
          endif
 
          write(*,'(i5,1x,5a14)')i,(ctemp(j),j=1,5)
 
        end do
 
      end do
 
      write(*,*)' '
 
      return
      end
      subroutine prisys(a,ia,istore,lda,ncol,nrow,rhs)

c*********************************************************************72
c
cc PRISYS prints a matrix, and a right hand side vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A(*), the NROW by NCOL matrix.
c
      integer nrow

      real a(*)
      character chrrel*14
      character ctemp(5)*14
      real entry
      integer i
      integer ia(*)
      integer iband
      integer icopy
      integer ihi
      integer ilo
      integer inc
      integer incx
      integer indx
      integer istore(5)
      integer j
      integer jcopy
      integer jhi
      integer jlo
      integer lda
      integer ml
      integer mu
      integer ncol
      real rhs(nrow)

      iband=istore(1)
      ml=istore(2)
      mu=istore(3)
      incx=4
      write(*,*)' '

      do jlo=1,ncol,incx
 
        jhi=min(jlo+incx-1,ncol)
        inc=min(incx,ncol+1-jlo)
        write(*,*)' '
        write(*,*)'Columns ',jlo,' to ',jhi,' and RHS'
        write(*,*)'  Row'
        write(*,*)' '
        ilo=1
        ihi=nrow
 
        if(iband.ge.1.and.iband.le.3)then
          ilo=max(jlo-mu,1)
          ihi=min(jhi+ml,nrow)
        endif
 
        do i=ilo,ihi
 
          icopy=i
 
          do j=1,inc
 
            jcopy=jlo-1+j
 
            call legtwo(icopy,ia,indx,istore,jcopy,lda)
 
            if(indx.le.0)then
              ctemp(j)=' '
            else
              entry=a(indx)
              ctemp(j)=chrrel(entry)
              if(entry.eq.0.0)ctemp(j)='    0.0'
            endif
 
          end do
 
          if(inc.lt.incx)then
 
            do j=inc+1,5
              ctemp(j)=' '
            end do
 
          endif
 
          ctemp(5)=chrrel(rhs(i))
          write(*,'(i5,1x,5a14)')i,(ctemp(j),j=1,5)
 
        end do
 
      end do
 
      write(*,*)' '
 
      return
      end
      subroutine setmat ( a, ia, istore, lda, ncol, nrow, entry, i, j )

c*********************************************************************72
c
cc SETMAT sets A(I,J)=ENTRY
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A(*), the NROW by NCOL matrix.
c
      real a(*)
      real entry
      integer i
      integer ia(*)
      integer indx
      integer istore(5)
      integer j
      integer lda
      integer ncol
      integer nrow
c
c  Check the row and column indices.
c
      call chekij(i,j,ncol,nrow)
c
c  Get index into matrix
c
      call legtwo(i,ia,indx,istore,j,lda)
      if(indx.gt.0)a(indx)=entry
 
      return
      end
      subroutine shomat ( ia, istore, iwork, lda, ncol, nmax, nrow )

c*********************************************************************72
c
cc SHOMAT shows the storage structure of a matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      integer nmax

      integer i
      integer ia(*)
      integer ii
      integer indx
      integer istore(5)
      integer iwork(nmax)
      integer j
      integer jj
      integer lda
      integer ncol
      integer nhi
      integer nrow

      do i=1,nmax
        iwork(i)=0
      end do

      nhi=0
 
      do i=1,nrow
        do j=1,ncol
 
          call legtwo(i,ia,indx,istore,j,lda)
 
          if(indx.gt.0.and.indx.le.(nmax/2))then
            if(indx.gt.nhi)nhi=indx
            ii=2*indx-1
            jj=2*indx
 
            if(iwork(ii).ne.0)then
 
              if(istore(1).ne.6.or.iwork(ii).ne.j.or.iwork(jj).ne.i)then
                write(*,*)'SHOMAT - storage conflict!'
                write(*,*)'(I1,J1)=',i,j
                write(*,*)'(I2,J2)=',iwork(ii),iwork(jj)
              endif
 
            else
              iwork(ii)=i
              iwork(jj)=j
            endif
 
          endif
 
        end do
      end do
 
      write(*,*)' '
      write(*,*)'SHOMAT displays the storage of a matrix in memory.'
      write(*,*)' '
      write(*,*)'    Memory    Row I    Column J'
      write(*,*)' '
 
      do i=1,nhi
        write(*,'(1x,3i10)')i,iwork(2*i-1),iwork(2*i)
      end do
 
      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
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
      subroutine zermat ( a, ia, istore, lda, ncol, nrow )

c*********************************************************************72
c
cc ZERMAT zeroes out a matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real A(*), the NROW by NCOL matrix.
c
      real a(*)
      real entry
      integer i
      integer ia(*)
      integer istore(5)
      integer j
      integer lda
      integer ncol
      integer nrow

      entry=0.0
 
      do i=1,nrow
        do j=1,ncol
          call setmat(a,ia,istore,lda,ncol,nrow,entry,i,j)
        end do
      end do
 
      return
      end
