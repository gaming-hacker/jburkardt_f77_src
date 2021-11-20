c-----------------------------------------------------------------------
      subroutine addblk(nrowa, ncola, a, ja, ia, ipos, jpos, job,
     & nrowb, ncolb, b, jb, ib, nrowc, ncolc, c, jc, ic, nzmx, ierr)

c      implicit none

      integer nrowa, nrowb, nrowc, ncola, ncolb, ncolc, ipos, jpos
      integer nzmx, ierr, job
      integer ja(1:*), ia(1:*), jb(1:*), ib(1:*), jc(1:*), ic(1:*)
      real*8 a(1:*), b(1:*), c(1:*)
c-----------------------------------------------------------------------
c     This subroutine adds a matrix B into a submatrix of A whose 
c     (1,1) element is located in the starting position (ipos, jpos). 
c     The resulting matrix is allowed to be larger than A (and B), 
c     and the resulting dimensions nrowc, ncolc will be redefined 
c     accordingly upon return.  
c     The input matrices are assumed to be sorted, i.e. in each row
c     the column indices appear in ascending order in the CSR format.
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrowa    = number of rows in A.
c bcola    = number of columns in A.
c a,ja,ia  = Matrix A in compressed sparse row format with entries sorted
c nrowb    = number of rows in B.
c ncolb    = number of columns in B.
c b,jb,ib  = Matrix B in compressed sparse row format with entries sorted
c
c nzmax	   = integer. The  length of the arrays c and jc. addblk will 
c            stop if the number of nonzero elements in the matrix C
c            exceeds nzmax. See ierr.
c 
c on return:
c----------
c nrowc    = number of rows in C.
c ncolc    = number of columns in C.
c c,jc,ic  = resulting matrix C in compressed sparse row sparse format
c            with entries sorted ascendly in each row. 
c	    
c ierr	   = integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that addblk stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c Notes: 
c-------
c     this will not work if any of the two input matrices is not sorted
c-----------------------------------------------------------------------
      logical values
      integer i,j1,j2,ka,kb,kc,kamax,kbmax
      values = (job .ne. 0) 
      ierr = 0
      nrowc = max(nrowa, nrowb+ipos-1)
      ncolc = max(ncola, ncolb+jpos-1)
      kc = 1
      kbmax = 0
      ic(1) = kc
c
      do 10 i=1, nrowc
         if (i.le.nrowa) then
            ka = ia(i)
            kamax = ia(i+1)-1
         else
            ka = ia(nrowa+1)
         end if
         if ((i.ge.ipos).and.((i-ipos).le.nrowb)) then
            kb = ib(i-ipos+1)
            kbmax = ib(i-ipos+2)-1 
         else
            kb = ib(nrowb+1)
         end if
c
c     a do-while type loop -- goes through all the elements in a row.
c
 20      continue 
         if (ka .le. kamax) then
            j1 = ja(ka)
         else
            j1 = ncolc+1
         endif
         if (kb .le. kbmax) then 
            j2 = jb(kb) + jpos - 1
         else 
            j2 = ncolc+1
         endif
c
c     if there are more elements to be added.
c
         if ((ka .le. kamax .or. kb .le. kbmax) .and.
     &        (j1 .le. ncolc .or. j2 .le. ncolc)) then
c
c     three cases
c
            if (j1 .eq. j2) then 
               if (values) c(kc) = a(ka)+b(kb)
               jc(kc) = j1
               ka = ka+1
               kb = kb+1
               kc = kc+1
            else if (j1 .lt. j2) then
               jc(kc) = j1
               if (values) c(kc) = a(ka)
               ka = ka+1
               kc = kc+1
            else if (j1 .gt. j2) then
               jc(kc) = j2
               if (values) c(kc) = b(kb)
               kb = kb+1
               kc = kc+1
            endif
            if (kc .gt. nzmx) goto 999
            goto 20
         end if
         ic(i+1) = kc
 10   continue
      return
 999  ierr = i 
      return
c---------end-of-addblk------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine add_lk(new,nod,idom,ndom,lkend,levst,link,nodes,marker) 
      implicit none
      integer new,nod,idom,ndom,lkend,levst(*),link(*),nodes(*),
     *     marker(*) 
c----------------------------------------------------------------------- 
c     inserts new element to linked list from the tail.
c----------------------------------------------------------------------- 
c     adds one entry (new) to linked list and ipdates everything.
c     new  = node to be added
c     nod  = current number of marked nodes
c     idom = domain to which new is to be added
c     ndom = total number of domains
c     lkend= location of end of structure (link and nodes)
c     levst= pointer array for link, nodes
c     link = link array 
c     nodes= nodes array -- 
c     marker = marker array == if marker(k) =0 then node k is not
c              assigned yet. 
c----------------------------------------------------------------------- 
c      locals
c     
      integer ktop  
      lkend = lkend + 1
      nodes(lkend) = new
      nod = nod+1 
      marker(new) = idom 
      ktop = levst(idom) 
      link(lkend) = ktop 
      link(idom) = link(idom)-1 
      levst(idom) = lkend 
      return
c-----------------------------------------------------------------------
c-------end-of-add_lk--------------------------------------------------- 
      end 
c-----------------------------------------------------------------------
      subroutine add_lvst(istart,iend,nlev,riord,ja,ia,mask,maskval) 
      integer nlev, nod, riord(*), ja(*), ia(*), mask(*) 
c-------------------------------------------------------------
c     adds one level set to the previous sets.. 
c     span all nodes of previous mask
c-------------------------------------------------------------
      nod = iend
      do 25 ir = istart+1,iend 
         i = riord(ir)		
         do 24 k=ia(i),ia(i+1)-1
            j = ja(k)
            if (mask(j) .eq. maskval) then
               nod = nod+1 
               mask(j) = 0
               riord(nod) = j
            endif 
 24      continue
 25   continue
      istart = iend 
      iend   = nod 
      return
      end 
c-----------------------------------------------------------------------
      subroutine amask (nrow,ncol,a,ja,ia,jmask,imask,
     *                  c,jc,ic,iw,nzmax,ierr)
c---------------------------------------------------------------------
      real*8 a(*),c(*) 
      integer ia(nrow+1),ja(*),jc(*),ic(nrow+1),jmask(*),imask(nrow+1) 
      logical iw(ncol)
c-----------------------------------------------------------------------
c This subroutine builds a sparse matrix from an input matrix by 
c extracting only elements in positions defined by the mask jmask, imask
c-----------------------------------------------------------------------
c On entry:
c---------
c nrow  = integer. row dimension of input matrix 
c ncol	= integer. Column dimension of input matrix.
c
c a,
c ja,
c ia	= matrix in Compressed Sparse Row format
c
c jmask,
c imask = matrix defining mask (pattern only) stored in compressed
c         sparse row format.
c
c nzmax = length of arrays c and jc. see ierr.
c 
c On return:
c-----------
c
c a, ja, ia and jmask, imask are unchanged.
c
c c
c jc, 
c ic	= the output matrix in Compressed Sparse Row format.
c 
c ierr  = integer. serving as error message.c
c         ierr = 1  means normal return
c         ierr .gt. 1 means that amask stopped when processing
c         row number ierr, because there was not enough space in
c         c, jc according to the value of nzmax.
c
c work arrays:
c------------- 
c iw	= logical work array of length ncol.
c
c note: 
c------ the  algorithm is in place: c, jc, ic can be the same as 
c a, ja, ia in which cas the code will overwrite the matrix c
c on a, ja, ia
c
c-----------------------------------------------------------------------
      ierr = 0
      len = 0
      do 1 j=1, ncol
         iw(j) = .false.
 1    continue
c     unpack the mask for row ii in iw
      do 100 ii=1, nrow
c     save pointer in order to be able to do things in place
         do 2 k=imask(ii), imask(ii+1)-1
            iw(jmask(k)) = .true.
 2       continue
c     add umasked elemnts of row ii
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         ic(ii) = len+1
         do 200 k=k1,k2 
            j = ja(k)
            if (iw(j)) then
               len = len+1
               if (len .gt. nzmax) then
                  ierr = ii
                  return
               endif
               jc(len) = j
               c(len) = a(k)
            endif
 200     continue	      
c     
         do 3 k=imask(ii), imask(ii+1)-1
            iw(jmask(k)) = .false.
 3       continue
 100  continue	  
      ic(nrow+1)=len+1
c
      return
c-----end-of-amask -----------------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine amubdg (nrow,ncol,ncolb,ja,ia,jb,ib,ndegr,nnz,iw) 
      integer ja(*),jb(*),ia(nrow+1),ib(ncol+1),ndegr(nrow),iw(ncolb) 
c-----------------------------------------------------------------------
c gets the number of nonzero elements in each row of A*B and the total 
c number of nonzero elements in A*B. 
c-----------------------------------------------------------------------
c on entry:
c -------- 
c
c nrow  = integer.  row dimension of matrix A
c ncol  = integer.  column dimension of matrix A = row dimension of 
c                   matrix B.
c ncolb = integer. the colum dimension of the matrix B.
c
c ja, ia= row structure of input matrix A: ja = column indices of
c         the nonzero elements of A stored by rows.
c         ia = pointer to beginning of each row  in ja.
c 
c jb, ib= row structure of input matrix B: jb = column indices of
c         the nonzero elements of A stored by rows.
c         ib = pointer to beginning of each row  in jb.
c 
c on return:
c ---------
c ndegr	= integer array of length nrow containing the degrees (i.e., 
c         the number of nonzeros in  each row of the matrix A * B 
c				
c nnz   = total number of nonzero elements found in A * B
c
c work arrays:
c-------------
c iw	= integer work array of length ncolb. 
c-----------------------------------------------------------------------
      do 1 k=1, ncolb 
         iw(k) = 0 
 1    continue
      
      do 2 k=1, nrow
         ndegr(k) = 0 
 2    continue
c     
c     method used: Transp(A) * A = sum [over i=1, nrow]  a(i)^T a(i)
c     where a(i) = i-th row of  A. We must be careful not to add  the
c     elements already accounted for.
c     
c     
      do 7 ii=1,nrow 
c     
c     for each row of A
c     
         ldg = 0 
c     
c    end-of-linked list
c     
         last = -1 
         do 6 j = ia(ii),ia(ii+1)-1 
c     
c     row number to be added:
c     
            jr = ja(j) 
            do 5 k=ib(jr),ib(jr+1)-1
               jc = jb(k) 
               if (iw(jc) .eq. 0) then 
c     
c     add one element to the linked list 
c     
                  ldg = ldg + 1
                  iw(jc) = last 
                  last = jc
               endif
 5          continue
 6       continue
         ndegr(ii) = ldg
c     
c     reset iw to zero
c     
         do 61 k=1,ldg 
            j = iw(last) 
            iw(last) = 0
            last = j
 61      continue
c-----------------------------------------------------------------------
 7    continue
c     
      nnz = 0
      do 8 ii=1, nrow 
         nnz = nnz+ndegr(ii) 
 8    continue
c     
      return
c---------------end-of-amubdg ------------------------------------------
c-----------------------------------------------------------------------
      end
       subroutine amub (nrow,ncol,job,a,ja,ia,b,jb,ib,
     *                  c,jc,ic,nzmax,iw,ierr) 
      real*8 a(*), b(*), c(*) 
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(*),ic(*),iw(ncol)
c-----------------------------------------------------------------------
c performs the matrix by matrix product C = A B 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow  = integer. The row dimension of A = row dimension of C
c ncol  = integer. The column dimension of B = column dimension of C
c job   = integer. Job indicator. When job = 0, only the structure
c                  (i.e. the arrays jc, ic) is computed and the
c                  real values are ignored.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c b, 
c jb, 
c ib    =  Matrix B in compressed sparse row format.
c
c nzmax = integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic    = resulting matrix C in compressed sparse row sparse format.
c           
c ierr  = integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c work arrays:
c------------
c iw    = integer work array of length equal to the number of
c         columns in A.
c Note: 
c-------
c   The row dimension of B is not needed. However there is no checking 
c   on the condition that ncol(A) = nrow(B). 
c
c----------------------------------------------------------------------- 
      real*8 scal 
      logical values
      values = (job .ne. 0) 
      len = 0
      ic(1) = 1 
      ierr = 0
c     initialize array iw.
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
c
      do 500 ii=1, nrow 
c     row i 
         do 200 ka=ia(ii), ia(ii+1)-1 
	    if (values) scal = a(ka)
	    jj   = ja(ka)
	    do 100 kb=ib(jj),ib(jj+1)-1
               jcol = jb(kb)
               jpos = iw(jcol)
               if (jpos .eq. 0) then
                  len = len+1
                  if (len .gt. nzmax) then
                     ierr = ii
                     return
                  endif
                  jc(len) = jcol
                  iw(jcol)= len
                  if (values) c(len)  = scal*b(kb)
               else
                  if (values) c(jpos) = c(jpos) + scal*b(kb)
               endif
 100	    continue
 200     continue
         do 201 k=ic(ii), len
	    iw(jc(k)) = 0
 201     continue
         ic(ii+1) = len+1
 500  continue
      return
c-------------end-of-amub-----------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine amudia (nrow,job, a, ja, ia, diag, b, jb, ib)
      real*8 a(*), b(*), diag(nrow) 
      integer ja(*),jb(*), ia(nrow+1),ib(nrow+1) 
c-----------------------------------------------------------------------
c performs the matrix by matrix product B = A * Diag  (in place) 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c job   = integer. job indicator. Job=0 means get array b only
c         job = 1 means get b, and the integer arrays ib, jb.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c diag = diagonal matrix stored as a vector dig(1:n)
c
c on return:
c----------
c
c b, 
c jb, 
c ib	= resulting matrix B in compressed sparse row sparse format.
c	    
c Notes:
c-------
c 1)        The column dimension of A is not needed. 
c 2)        algorithm in place (B can take the place of A).
c-----------------------------------------------------------------
      do 1 ii=1,nrow
c     
c     scale each element 
c     
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         do 2 k=k1, k2
            b(k) = a(k)*diag(ja(k)) 
 2       continue
 1    continue
c     
      if (job .eq. 0) return
c     
      do 3 ii=1, nrow+1
         ib(ii) = ia(ii)
 3    continue
      do 31 k=ia(1), ia(nrow+1) -1 
         jb(k) = ja(k)
 31   continue
      return
c-----------------------------------------------------------------------
c-----------end-of-amudiag----------------------------------------------
      end 
c-----------------------------------------------------------------------
      subroutine amuxd (n,x,y,diag,ndiag,idiag,ioff) 
      integer n, ndiag, idiag, ioff(idiag) 
      real*8 x(n), y(n), diag(ndiag,idiag)
c-----------------------------------------------------------------------
c        A times a vector in Diagonal storage format (DIA) 
c----------------------------------------------------------------------- 
c multiplies a matrix by a vector when the original matrix is stored 
c in the diagonal storage format.
c-----------------------------------------------------------------------
c
c on entry:
c----------
c n     = row dimension of A
c x     = real array of length equal to the column dimension of
c         the A matrix.
c ndiag  = integer. The first dimension of array adiag as declared in
c         the calling program.
c idiag  = integer. The number of diagonals in the matrix.
c diag   = real array containing the diagonals stored of A.
c idiag  = number of diagonals in matrix.
c diag   = real array of size (ndiag x idiag) containing the diagonals
c          
c ioff   = integer array of length idiag, containing the offsets of the
c   	   diagonals of the matrix:
c          diag(i,k) contains the element a(i,i+ioff(k)) of the matrix.
c
c on return:
c-----------
c y     = real array of length n, containing the product y=A*x
c
c-----------------------------------------------------------------------
c local variables 
c
      integer j, k, io, i1, i2 
c-----------------------------------------------------------------------
      do 1 j=1, n
         y(j) = 0.0d0
 1    continue	
      do 10 j=1, idiag
         io = ioff(j)
         i1 = max0(1,1-io)
         i2 = min0(n,n-io)
         do 9 k=i1, i2	
            y(k) = y(k)+diag(k,j)*x(k+io)
 9       continue
 10   continue
c 
      return
c----------end-of-amuxd-------------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine amuxe (n,x,y,na,ncol,a,ja) 
      real*8 x(n), y(n), a(na,*)  
      integer  n, na, ncol, ja(na,*)
c-----------------------------------------------------------------------
c        A times a vector in Ellpack Itpack format (ELL)               
c----------------------------------------------------------------------- 
c multiplies a matrix by a vector when the original matrix is stored 
c in the ellpack-itpack sparse format.
c-----------------------------------------------------------------------
c
c on entry:
c----------
c n     = row dimension of A
c x     = real array of length equal to the column dimension of
c         the A matrix.
c na    = integer. The first dimension of arrays a and ja
c         as declared by the calling program.
c ncol  = integer. The number of active columns in array a.
c         (i.e., the number of generalized diagonals in matrix.)
c a, ja = the real and integer arrays of the itpack format
c         (a(i,k),k=1,ncol contains the elements of row i in matrix
c          ja(i,k),k=1,ncol contains their column numbers) 
c
c on return:
c-----------
c y     = real array of length n, containing the product y=y=A*x
c
c-----------------------------------------------------------------------
c local variables
c
      integer i, j 
c-----------------------------------------------------------------------
      do 1 i=1, n
         y(i) = 0.0 
 1    continue
      do 10 j=1,ncol
         do 25 i = 1,n
            y(i) = y(i)+a(i,j)*x(ja(i,j))
 25      continue
 10   continue
c
      return
c--------end-of-amuxe--------------------------------------------------- 
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c          BASIC MATRIX-VECTOR OPERATIONS - MATVEC MODULE              c
c         Matrix-vector Mulitiplications and Triang. Solves            c
c----------------------------------------------------------------------c
c contents: (as of Nov 18, 1991)                                       c
c----------                                                            c
c 1) Matrix-vector products:                                           c
c---------------------------                                           c
c amux  : A times a vector. Compressed Sparse Row (CSR) format.        c
c amuxms: A times a vector. Modified Compress Sparse Row format.       c
c atmux : Transp(A) times a vector. CSR format.                        c
c atmuxr: Transp(A) times a vector. CSR format. A rectangular.         c
c amuxe : A times a vector. Ellpack/Itpack (ELL) format.               c
c amuxd : A times a vector. Diagonal (DIA) format.                     c
c amuxj : A times a vector. Jagged Diagonal (JAD) format.              c
c vbrmv : Sparse matrix-full vector product, in VBR format             c
c                                                                      c
c 2) Triangular system solutions:                                      c
c-------------------------------                                       c
c lsol  : Unit Lower Triang. solve. Compressed Sparse Row (CSR) format.c
c ldsol : Lower Triang. solve.  Modified Sparse Row (MSR) format.      c
c lsolc : Unit Lower Triang. solve. Comp. Sparse Column (CSC) format.  c
c ldsolc: Lower Triang. solve. Modified Sparse Column (MSC) format.    c
c ldsoll: Lower Triang. solve with level scheduling. MSR format.       c
c usol  : Unit Upper Triang. solve. Compressed Sparse Row (CSR) format.c
c udsol : Upper Triang. solve.  Modified Sparse Row (MSR) format.      c
c usolc : Unit Upper Triang. solve. Comp. Sparse Column (CSC) format.  c
c udsolc: Upper Triang. solve.  Modified Sparse Column (MSC) format.   c
c----------------------------------------------------------------------c
c 1)     M A T R I X    B Y    V E C T O R     P R O D U C T S         c
c----------------------------------------------------------------------c
      subroutine amux (n, x, y, a,ja,ia) 
      real*8  x(*), y(*), a(*) 
      integer n, ja(*), ia(*)
c-----------------------------------------------------------------------
c         A times a vector
c----------------------------------------------------------------------- 
c multiplies a matrix by a vector using the dot product form
c Matrix A is stored in compressed sparse row storage.
c
c on entry:
c----------
c n     = row dimension of A
c x     = real array of length equal to the column dimension of
c         the A matrix.
c a, ja,
c    ia = input matrix in compressed sparse row format.
c
c on return:
c-----------
c y     = real array of length n, containing the product y=Ax
c
c-----------------------------------------------------------------------
c local variables
c
      real*8 t
      integer i, k
c-----------------------------------------------------------------------
      do 100 i = 1,n
c
c     compute the inner product of row i with vector x
c 
         t = 0.0d0
         do 99 k=ia(i), ia(i+1)-1 
            t = t + a(k)*x(ja(k))
 99      continue
c
c     store result in y(i) 
c
         y(i) = t
 100  continue
c
      return
c---------end-of-amux---------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine amuxj (n, x, y, jdiag, a, ja, ia)
      integer n, jdiag, ja(*), ia(*)
      real*8 x(n), y(n), a(*)  
c-----------------------------------------------------------------------
c        A times a vector in Jagged-Diagonal storage format (JAD) 
c----------------------------------------------------------------------- 
c multiplies a matrix by a vector when the original matrix is stored 
c in the jagged diagonal storage format.
c-----------------------------------------------------------------------
c
c on entry:
c----------
c n      = row dimension of A
c x      = real array of length equal to the column dimension of
c         the A matrix.
c jdiag  = integer. The number of jadded-diagonals in the data-structure.
c a      = real array containing the jadded diagonals of A stored
c          in succession (in decreasing lengths) 
c j      = integer array containing the colum indices of the 
c          corresponding elements in a.
c ia     = integer array containing the lengths of the  jagged diagonals
c
c on return:
c-----------
c y      = real array of length n, containing the product y=A*x
c
c Note:
c------- 
c Permutation related to the JAD format is not performed.
c this can be done by:
c     call permvec (n,y,y,iperm) 
c after the call to amuxj, where iperm is the permutation produced
c by csrjad.
c-----------------------------------------------------------------------
c local variables 
c
      integer i, ii, k1, len, j 
c-----------------------------------------------------------------------
      do 1 i=1, n
         y(i) = 0.0d0
 1    continue
      do 70 ii=1, jdiag
         k1 = ia(ii)-1
         len = ia(ii+1)-k1-1
         do 60 j=1,len
            y(j)= y(j)+a(k1+j)*x(ja(k1+j)) 
 60      continue
 70   continue
c
      return
c----------end-of-amuxj------------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine amuxms (n, x, y, a,ja)
      real*8  x(*), y(*), a(*)
      integer n, ja(*)
c-----------------------------------------------------------------------
c         A times a vector in MSR format
c-----------------------------------------------------------------------
c multiplies a matrix by a vector using the dot product form
c Matrix A is stored in Modified Sparse Row storage.
c
c on entry:
c----------
c n     = row dimension of A
c x     = real array of length equal to the column dimension of
c         the A matrix.
c a, ja,= input matrix in modified compressed sparse row format.
c
c on return:
c-----------
c y     = real array of length n, containing the product y=Ax
c
c-----------------------------------------------------------------------
c local variables
c
      integer i, k
c-----------------------------------------------------------------------
        do 10 i=1, n
        y(i) = a(i)*x(i)
 10     continue
      do 100 i = 1,n
c
c     compute the inner product of row i with vector x
c
         do 99 k=ja(i), ja(i+1)-1
            y(i) = y(i) + a(k) *x(ja(k))
 99      continue
 100  continue
c
      return
c---------end-of-amuxm--------------------------------------------------
c-----------------------------------------------------------------------
      end
c **********************************************************************
      subroutine ANCCNX(n, mccnex, iccnex, mark, ncount)
C-----------------------------------------------------------------------
c     
c     We put in ICCNEX the vertices marked in the component MCCNEX.
C
C-----------------------------------------------------------------------
c     include "NSIMPLIC"
      dimension mark(n)
C-----------------------------------------------------------------------
C     Laura C. Dutto - email: dutto@cerca.umontreal.ca - December 1993
C-----------------------------------------------------------------------
      ncount = 0
      do i = 1, n
         if( mark(i) .eq. mccnex) then
              mark(i) = iccnex
              ncount  = ncount + 1
         endif
      enddo
c
      return
      end
c----------------------------------------------------------------------
      subroutine ansym(n,sym,a,ja,ia,ao,jao,iao,imatch,
     *     av,fas,fan)
c---------------------------------------------------------------------
c     this routine computes the Frobenius norm of the symmetric and
c     non-symmetric parts of A, computes number of matching elements
c     in symmetry and relative symmetry match. 
c---------------------------------------------------------------------
c on entry:
c----------
c n   = integer column dimension of matrix
c a   = real array containing the nonzero elements of the matrix
c       the elements are stored by columns in order
c       (i.e. column i comes before column i+1, but the elements
c       within each column can be disordered).
c ja  = integer array containing the row indices of elements in a
c ia  = integer array containing of length n+1 containing the
c       pointers to the beginning of the columns in arrays a and ja.
c       It is assumed that ia(*) = 1 and ia(n+1) = nzz+1.
c sym = logical variable indicating whether or not the matrix is
c       symmetric.
c on return
c----------
c fas   = Frobenius norm of symmetric part
c fan   = Frobenius norm of non-symmetric part
c imatch = number of matching elements in symmetry
c av     = relative symmetry match (symmetry = 1)
c ao,jao,iao = transpose of A just as a, ja, ia contains 
c              information of A.
c-----------------------------------------------------------------------
      implicit real*8 (a-h, o-z)
      real*8 a(*),ao(*),fas,fan,av, Fnorm, st
      integer n, ja(*), ia(n+1), jao(*), iao(n+1),imatch
      logical sym
c-----------------------------------------------------------------------
      nnz    = ia(n+1)-ia(1)
      call csrcsc(n,1,1,a,ja,ia,ao,jao,iao)
      if (sym) goto 7
      st     = 0.0d0
      fas    = 0.0d0
      fan    = 0.0d0
      imatch = 0
      do 6 i=1,n
         k1 = ia(i)
         k2 = iao(i)
         k1max = ia(i+1) - 1
         k2max = iao(i+1) - 1
c     
 5       if (k1 .gt. k1max .or. k2 .gt. k2max) goto 6
c     
         j1 = ja(k1)
         j2 = jao(k2)
         if (j1 .ne. j2 ) goto 51
         fas = fas + (a(k1)+ao(k2))**2
         fan = fan + (a(k1)-ao(k2))**2
         st  = st + a(k1)**2
         imatch = imatch + 1
 51      k1 = k1+1
         k2 = k2+1
         if (j1 .lt. j2)  k2 = k2 - 1
         if (j1 .gt. j2)  k1 = k1 - 1
         goto 5
 6    continue
      fas = 0.25D0 * fas
      fan = 0.25D0 * fan
 7    call frobnorm(n,sym,ao,jao,iao,Fnorm)
      if (sym) then
         imatch = nnz
         fas = Fnorm
         fan = 0.0d0
      else
         if (imatch.eq.nnz) then
            st = 0.0D0
         else
            st = 0.5D0 * (Fnorm**2 - st)
            if (st.lt.0.0D0) st = 0.0D0
         endif
         fas = sqrt(fas + st)
         fan = sqrt(fan + st)
      endif
      av = real(imatch)/real(nnz)
      return
      end
c-----------------------------------------------------------------------
      subroutine aplb1(nrow,ncol,job,a,ja,ia,b,jb,ib,c,jc,ic,nzmax,ierr)
      real*8 a(*), b(*), c(*) 
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1),ic(nrow+1)
c-----------------------------------------------------------------------
c performs the matrix sum  C = A+B for matrices in sorted CSR format.
c the difference with aplb  is that the resulting matrix is such that
c the elements of each row are sorted with increasing column indices in
c each row, provided the original matrices are sorted in the same way. 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c job   = integer. Job indicator. When job = 0, only the structure
c                  (i.e. the arrays jc, ic) is computed and the
c                  real values are ignored.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format with entries sorted
c 
c b, 
c jb, 
c ib	=  Matrix B in compressed sparse row format with entries sorted
c        ascendly in each row   
c
c nzmax	= integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic	= resulting matrix C in compressed sparse row sparse format
c         with entries sorted ascendly in each row. 
c	    
c ierr	= integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c Notes: 
c-------
c     this will not work if any of the two input matrices is not sorted
c-----------------------------------------------------------------------
      logical values
      values = (job .ne. 0) 
      ierr = 0
      kc = 1
      ic(1) = kc 
c
      do 6 i=1, nrow
         ka = ia(i)
         kb = ib(i)
         kamax = ia(i+1)-1
         kbmax = ib(i+1)-1 
 5       continue 
         if (ka .le. kamax) then
            j1 = ja(ka)
         else
            j1 = ncol+1
         endif
         if (kb .le. kbmax) then 
            j2 = jb(kb)         
         else 
            j2 = ncol+1
         endif
c
c     three cases
c     
         if (kc .gt. nzmax) goto 999
         if (j1 .eq. j2) then 
            if (values) c(kc) = a(ka)+b(kb)
            jc(kc) = j1
            ka = ka+1
            kb = kb+1
            kc = kc+1
         else if (j1 .lt. j2) then
            jc(kc) = j1
            if (values) c(kc) = a(ka)
            ka = ka+1
            kc = kc+1
         else if (j1 .gt. j2) then
            jc(kc) = j2
            if (values) c(kc) = b(kb)
            kb = kb+1
            kc = kc+1
         endif
         if (ka .le. kamax .or. kb .le. kbmax) goto 5
         ic(i+1) = kc
 6    continue
      return
 999  ierr = i 
      return
c------------end-of-aplb1----------------------------------------------- 
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine aplbdg (nrow,ncol,ja,ia,jb,ib,ndegr,nnz,iw) 
      integer ja(*),jb(*),ia(nrow+1),ib(nrow+1),iw(ncol),ndegr(nrow) 
c-----------------------------------------------------------------------
c gets the number of nonzero elements in each row of A+B and the total 
c number of nonzero elements in A+B. 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c b, 
c jb, 
c ib	=  Matrix B in compressed sparse row format.
c
c on return:
c----------
c ndegr	= integer array of length nrow containing the degrees (i.e., 
c         the number of nonzeros in  each row of the matrix A + B.
c				
c nnz   = total number of nonzero elements found in A * B
c
c work arrays:
c------------
c iw	= integer work array of length equal to ncol. 
c
c-----------------------------------------------------------------------
      do 1 k=1, ncol 
         iw(k) = 0 
 1    continue
c
      do 2 k=1, nrow
         ndegr(k) = 0 
 2    continue
c
      do 7 ii=1,nrow 
         ldg = 0 
c     
c    end-of-linked list
c     
         last = -1 
c     
c     row of A
c     
         do 5 j = ia(ii),ia(ii+1)-1 
            jr = ja(j) 
c     
c     add element to the linked list 
c     
            ldg = ldg + 1
            iw(jr) = last 
            last = jr
 5       continue
c     
c     row of B
c     
         do 6 j=ib(ii),ib(ii+1)-1
            jc = jb(j)
            if (iw(jc) .eq. 0) then 
c     
c     add one element to the linked list 
c     
               ldg = ldg + 1
               iw(jc) = last 
               last = jc
            endif
 6       continue
c     done with row ii. 
         ndegr(ii) = ldg
c     
c     reset iw to zero
c     
         do 61 k=1,ldg 
            j = iw(last) 
            iw(last) = 0
            last = j
 61      continue
c-----------------------------------------------------------------------
 7    continue
c     
      nnz = 0
      do 8 ii=1, nrow 
         nnz = nnz+ndegr(ii) 
 8    continue
      return
c----------------end-of-aplbdg -----------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine aplb (nrow,ncol,job,a,ja,ia,b,jb,ib,
     *     c,jc,ic,nzmax,iw,ierr)
      real*8 a(*), b(*), c(*) 
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1),ic(nrow+1),
     *     iw(ncol)
c-----------------------------------------------------------------------
c performs the matrix sum  C = A+B. 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c job   = integer. Job indicator. When job = 0, only the structure
c                  (i.e. the arrays jc, ic) is computed and the
c                  real values are ignored.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c b, 
c jb, 
c ib	=  Matrix B in compressed sparse row format.
c
c nzmax	= integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic	= resulting matrix C in compressed sparse row sparse format.
c	    
c ierr	= integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c work arrays:
c------------
c iw	= integer work array of length equal to the number of
c         columns in A.
c
c-----------------------------------------------------------------------
      logical values
      values = (job .ne. 0) 
      ierr = 0
      len = 0
      ic(1) = 1 
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
c     
      do 500 ii=1, nrow
c     row i 
         do 200 ka=ia(ii), ia(ii+1)-1 
            len = len+1
            jcol    = ja(ka)
            if (len .gt. nzmax) goto 999
            jc(len) = jcol 
            if (values) c(len)  = a(ka) 
            iw(jcol)= len
 200     continue
c     
         do 300 kb=ib(ii),ib(ii+1)-1
            jcol = jb(kb)
            jpos = iw(jcol)
            if (jpos .eq. 0) then
               len = len+1
               if (len .gt. nzmax) goto 999
               jc(len) = jcol
               if (values) c(len)  = b(kb)
               iw(jcol)= len
            else
               if (values) c(jpos) = c(jpos) + b(kb)
            endif
 300     continue
         do 301 k=ic(ii), len
	    iw(jc(k)) = 0
 301     continue
         ic(ii+1) = len+1
 500  continue
      return
 999  ierr = ii
      return
c------------end of aplb ----------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine apldia (nrow, job, a, ja, ia, diag, b, jb, ib, iw) 
      real*8 a(*), b(*), diag(nrow) 
      integer ja(*),jb(*), ia(nrow+1),ib(nrow+1), iw(*)
c-----------------------------------------------------------------------
c Adds a diagonal matrix to a general sparse matrix:  B = A + Diag 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c job   = integer. job indicator. Job=0 means get array b only
c         (i.e. assume that a has already been copied into array b,
c         or that algorithm is used in place. ) For all practical 
c         purposes enter job=0 for an in-place call and job=1 otherwise
c 
c         Note: in case there are missing diagonal elements in A, 
c         then the option job =0 will be ignored, since the algorithm 
c         must modify the data structure (i.e. jb, ib) in this 
c         situation.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c     
c diag = diagonal matrix stored as a vector dig(1:n)
c
c on return:
c----------
c
c b, 
c jb, 
c ib	= resulting matrix B in compressed sparse row sparse format.
c
c
c iw    = integer work array of length n. On return iw will
c         contain  the positions of the diagonal entries in the 
c         output matrix. (i.e., a(iw(k)), ja(iw(k)), k=1,...n,
c         are the values/column indices of the diagonal elements 
c         of the output matrix. ). 
c
c Notes:
c-------
c 1)        The column dimension of A is not needed. 
c 2)        algorithm in place (b, jb, ib, can be the same as
c           a, ja, ia, on entry). See comments for parameter job.
c
c coded by Y. Saad. Latest version July, 19, 1990
c-----------------------------------------------------------------
      logical test
c
c     copy integer arrays into b's data structure if required
c
      if (job .ne. 0) then 
         nnz = ia(nrow+1)-1
         do 2  k=1, nnz
            jb(k) = ja(k)
            b(k)  = a(k) 
 2       continue
         do 3 k=1, nrow+1
            ib(k) = ia(k)
 3       continue
      endif 
c
c     get positions of diagonal elements in data structure.
c     
      call diapos (nrow,ja,ia,iw)
c     
c     count number of holes in diagonal and add diag(*) elements to
c     valid diagonal entries.
c     
      icount = 0
      do 1 j=1, nrow
         if (iw(j) .eq. 0) then
            icount = icount+1
         else
            b(iw(j)) = a(iw(j)) + diag(j) 
         endif
 1    continue
c     
c     if no diagonal elements to insert return
c     
      if (icount .eq. 0) return
c     
c     shift the nonzero elements if needed, to allow for created 
c     diagonal elements. 
c     
      ko = ib(nrow+1)+icount
c     
c     copy rows backward
c     
      do 5 ii=nrow, 1, -1 
c     
c     go through  row ii
c     
         k1 = ib(ii)
         k2 = ib(ii+1)-1 
         ib(ii+1) = ko
         test = (iw(ii) .eq. 0) 
         do 4 k = k2,k1,-1 
            j = jb(k)
            if (test .and. (j .lt. ii)) then 
               test = .false. 
               ko = ko - 1
               b(ko) = diag(ii) 
               jb(ko) = ii
               iw(ii) = ko
            endif
            ko = ko-1
            b(ko) = a(k) 
            jb(ko) = j
 4       continue
c     diagonal element has not been added yet.
         if (test) then
            ko = ko-1
            b(ko) =  diag(ii) 
            jb(ko) = ii
            iw(ii) = ko
         endif
 5    continue
      ib(1) = ko 
      return
c-----------------------------------------------------------------------
c------------end-of-apldiag---------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine aplsb1 (nrow,ncol,a,ja,ia,s,b,jb,ib,c,jc,ic,
     *     nzmax,ierr)
      real*8 a(*), b(*), c(*), s
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1),ic(nrow+1)
c-----------------------------------------------------------------------
c performs the operation C = A+s B for matrices in sorted CSR format.
c the difference with aplsb is that the resulting matrix is such that
c the elements of each row are sorted with increasing column indices in
c each row, provided the original matrices are sorted in the same way. 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format with entries sorted
c
c s	= real. scalar factor for B.
c 
c b, 
c jb, 
c ib	=  Matrix B in compressed sparse row format with entries sorted
c        ascendly in each row   
c
c nzmax	= integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic	= resulting matrix C in compressed sparse row sparse format
c         with entries sorted ascendly in each row. 
c	    
c ierr	= integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c Notes: 
c-------
c     this will not work if any of the two input matrices is not sorted
c-----------------------------------------------------------------------
      ierr = 0
      kc = 1
      ic(1) = kc 
c
c     the following loop does a merge of two sparse rows + adds  them.
c 
      do 6 i=1, nrow
         ka = ia(i)
         kb = ib(i)
         kamax = ia(i+1)-1
         kbmax = ib(i+1)-1 
 5       continue 
c
c     this is a while  -- do loop -- 
c 
         if (ka .le. kamax .or. kb .le. kbmax) then 
c     
            if (ka .le. kamax) then
               j1 = ja(ka)
            else
c     take j1 large enough  that always j2 .lt. j1
               j1 = ncol+1
            endif
            if (kb .le. kbmax) then 
               j2 = jb(kb)         
            else 
c     similarly take j2 large enough  that always j1 .lt. j2 
               j2 = ncol+1
            endif
c     
c     three cases
c     
            if (j1 .eq. j2) then 
               c(kc) = a(ka)+s*b(kb)
               jc(kc) = j1
               ka = ka+1
               kb = kb+1
               kc = kc+1
            else if (j1 .lt. j2) then
               jc(kc) = j1
               c(kc) = a(ka)
               ka = ka+1
               kc = kc+1
            else if (j1 .gt. j2) then
               jc(kc) = j2
               c(kc) = s*b(kb)
               kb = kb+1
               kc = kc+1
            endif
            if (kc .gt. nzmax) goto 999
            goto 5
c
c     end while loop
c
         endif
         ic(i+1) = kc
 6    continue
      return
 999  ierr = i 
      return
c------------end-of-aplsb1 --------------------------------------------- 
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine aplsb (nrow,ncol,a,ja,ia,s,b,jb,ib,c,jc,ic,
     *     nzmax,ierr)
      real*8 a(*), b(*), c(*), s
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1),ic(nrow+1)
c-----------------------------------------------------------------------
c performs the operation C = A+s B for matrices in sorted CSR format.
c the difference with aplsb is that the resulting matrix is such that
c the elements of each row are sorted with increasing column indices in
c each row, provided the original matrices are sorted in the same way. 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format with entries sorted
c
c s	= real. scalar factor for B.
c 
c b, 
c jb, 
c ib	=  Matrix B in compressed sparse row format with entries sorted
c        ascendly in each row   
c
c nzmax	= integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic	= resulting matrix C in compressed sparse row sparse format
c         with entries sorted ascendly in each row. 
c	    
c ierr	= integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c Notes: 
c-------
c     this will not work if any of the two input matrices is not sorted
c-----------------------------------------------------------------------
      ierr = 0
      kc = 1
      ic(1) = kc 
c
c     the following loop does a merge of two sparse rows + adds  them.
c 
      do 6 i=1, nrow
         ka = ia(i)
         kb = ib(i)
         kamax = ia(i+1)-1
         kbmax = ib(i+1)-1 
 5       continue 
c
c     this is a while  -- do loop -- 
c 
         if (ka .le. kamax .or. kb .le. kbmax) then 
c     
            if (ka .le. kamax) then
               j1 = ja(ka)
            else
c     take j1 large enough  that always j2 .lt. j1
               j1 = ncol+1
            endif
            if (kb .le. kbmax) then 
               j2 = jb(kb)         
            else 
c     similarly take j2 large enough  that always j1 .lt. j2 
               j2 = ncol+1
            endif
c     
c     three cases
c     
            if (kc .gt. nzmax) goto 999
            if (j1 .eq. j2) then 
               c(kc) = a(ka)+s*b(kb)
               jc(kc) = j1
               ka = ka+1
               kb = kb+1
               kc = kc+1
            else if (j1 .lt. j2) then
               jc(kc) = j1
               c(kc) = a(ka)
               ka = ka+1
               kc = kc+1
            else if (j1 .gt. j2) then
               jc(kc) = j2
               c(kc) = s*b(kb)
               kb = kb+1
               kc = kc+1
            endif
            goto 5
c
c     end while loop
c
         endif
         ic(i+1) = kc
 6    continue
      return
 999  ierr = i 
      return
c------------end-of-aplsb --------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine aplsbt(nrow,ncol,a,ja,ia,s,b,jb,ib,
     *     c,jc,ic,nzmax,iw,ierr)
      real*8 a(*), b(*), c(*), s
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(ncol+1),ic(*),iw(*)
c-----------------------------------------------------------------------
c performs the matrix sum  C = A + transp(B).
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A and transp(B)
c ncol  = integer. The column dimension of A. Also the row 
c                  dimension of B. 
c
c a,
c ja,
c ia    = Matrix A in compressed sparse row format.
c
c s	= real. scalar factor for B.
c
c 
c b, 
c jb, 
c ib	=  Matrix B in compressed sparse row format.
c
c nzmax	= integer. The  length of the arrays c, jc, and ic.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic	= resulting matrix C in compressed sparse row format.
c	    
c ierr	= integer. serving as error message. 
c         ierr = 0 means normal return.
c         ierr = -1 means that nzmax was .lt. either the number of
c         nonzero elements of A or the number of nonzero elements in B.
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c work arrays:
c------------
c iw	= integer work array of length at least max(nrow,ncol) 
c
c Notes:
c------- It is important to note that here all of three arrays c, ic, 
c        and jc are assumed to be of length nnz(c). This is because 
c        the matrix is internally converted in coordinate format.
c        
c-----------------------------------------------------------------------
      ierr = 0
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
c     
      nnza = ia(nrow+1)-1
      nnzb = ib(ncol+1)-1
      len = nnzb
      if (nzmax .lt. nnzb .or. nzmax .lt. nnza) then
         ierr = -1
         return
      endif
c     
c     transpose matrix b into c
c
      ljob = 1
      ipos = 1
      call csrcsc (ncol,ljob,ipos,b,jb,ib,c,jc,ic) 
      do 2 k=1,len
 2       c(k) = c(k)*s
c     
c     main loop. add rows from ii = 1 to nrow.
c     
         do 500 ii=1, nrow
c     iw is used as a system to recognize whether there
c     was a nonzero element in c. 
            do 200 k = ic(ii),ic(ii+1)-1
               iw(jc(k)) = k
 200        continue
c     
            do 300 ka = ia(ii), ia(ii+1)-1 
               jcol = ja(ka)
               jpos = iw(jcol)
           if (jpos .eq. 0) then
c     
c     if fill-in append in coordinate format to matrix.
c     
              len = len+1
              if (len .gt. nzmax) goto 999
              jc(len) = jcol              
              ic(len) = ii
              c(len)  = a(ka)
           else
c     else do addition.
              c(jpos) = c(jpos) + a(ka)
           endif
 300    continue
        do 301 k=ic(ii), ic(ii+1)-1
           iw(jc(k)) = 0
 301    continue
 500  continue
c     
c     convert first part of matrix (without fill-ins) into coo format
c     
      ljob = 3
      do 501 i=1, nrow+1
         iw(i) = ic(i) 
 501  continue
      call csrcoo (nrow,ljob,nnzb,c,jc,iw,nnzb,c,ic,jc,ierr)
c
c     convert the whole thing back to csr format. 
c 
      ljob = 1
      call coicsr (nrow,len,ljob,c,jc,ic,iw)
      return
 999  ierr = ii
      return
c--------end-of-aplsbt--------------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine aplsca (nrow, a, ja, ia, scal,iw) 
      real*8 a(*), scal
      integer ja(*), ia(nrow+1),iw(*)
c-----------------------------------------------------------------------
c Adds a scalar to the diagonal entries of a sparse matrix A :=A + s I 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c a,
c ja,
c ia    = Matrix A in compressed sparse row format.
c 
c scal  = real. scalar to add to the diagonal entries. 
c
c on return:
c----------
c
c a, 
c ja, 
c ia	= matrix A with diagonal elements shifted (or created).
c	    
c iw    = integer work array of length n. On return iw will
c         contain  the positions of the diagonal entries in the 
c         output matrix. (i.e., a(iw(k)), ja(iw(k)), k=1,...n,
c         are the values/column indices of the diagonal elements 
c         of the output matrix. ). 
c
c Notes:
c-------
c     The column dimension of A is not needed. 
c     important: the matrix a may be expanded slightly to allow for
c     additions of nonzero elements to previously nonexisting diagonals.
c     The is no checking as to whether there is enough space appended
c     to the arrays a and ja. if not sure allow for n additional 
c     elemnts. 
c     coded by Y. Saad. Latest version July, 19, 1990
c-----------------------------------------------------------------------
      logical test
c
      call diapos (nrow,ja,ia,iw)
      icount = 0
      do 1 j=1, nrow
         if (iw(j) .eq. 0) then
            icount = icount+1
         else
            a(iw(j)) = a(iw(j)) + scal 
         endif
 1    continue
c
c     if no diagonal elements to insert in data structure return.
c
      if (icount .eq. 0) return
c
c shift the nonzero elements if needed, to allow for created 
c diagonal elements. 
c
      ko = ia(nrow+1)+icount
c
c     copy rows backward
c
      do 5 ii=nrow, 1, -1 
c     
c     go through  row ii
c     
         k1 = ia(ii)
         k2 = ia(ii+1)-1 
         ia(ii+1) = ko
         test = (iw(ii) .eq. 0) 
         do 4 k = k2,k1,-1 
            j = ja(k)
            if (test .and. (j .lt. ii)) then 
               test = .false. 
               ko = ko - 1
               a(ko) = scal 
               ja(ko) = ii
               iw(ii) = ko
            endif
            ko = ko-1
            a(ko) = a(k) 
            ja(ko) = j
 4       continue
c     diagonal element has not been added yet.
         if (test) then
            ko = ko-1
            a(ko) = scal 
            ja(ko) = ii
            iw(ii) = ko
         endif
 5    continue
      ia(1) = ko 
      return
c-----------------------------------------------------------------------
c----------end-of-aplsca------------------------------------------------ 
      end
c-----------------------------------------------------------------------
      subroutine apmbt (nrow,ncol,job,a,ja,ia,b,jb,ib,
     *     c,jc,ic,nzmax,iw,ierr)
      real*8 a(*), b(*), c(*) 
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(ncol+1),ic(*),iw(*) 
c-----------------------------------------------------------------------
c performs the matrix sum  C = A + transp(B) or C = A - transp(B) 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A and transp(B)
c ncol  = integer. The column dimension of A. Also the row 
c                  dimension of B. 
c
c job	= integer. if job = -1, apmbt will compute C= A - transp(B)
c         (structure + values) 
c         if (job .eq. 1)  it will compute C=A+transp(A) 
c         (structure+ values) 
c         if (job .eq. 0) it will compute the structure of
c         C= A+/-transp(B) only (ignoring all real values).
c         any other value of job will be treated as  job=1
c a,
c ja,
c ia    = Matrix A in compressed sparse row format.
c
c b, 
c jb, 
c ib	=  Matrix B in compressed sparse row format.
c
c nzmax	= integer. The  length of the arrays c, jc, and ic.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic	= resulting matrix C in compressed sparse row format.
c	    
c ierr	= integer. serving as error message. 
c         ierr = 0 means normal return.
c         ierr = -1 means that nzmax was .lt. either the number of
c         nonzero elements of A or the number of nonzero elements in B.
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c work arrays:
c------------
c iw	= integer work array of length at least max(ncol,nrow) 
c
c Notes:
c------- It is important to note that here all of three arrays c, ic, 
c        and jc are assumed to be of length nnz(c). This is because 
c        the matrix is internally converted in coordinate format.
c        
c-----------------------------------------------------------------------
      logical values
      values = (job .ne. 0) 
c
      ierr = 0
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
c     
      nnza = ia(nrow+1)-1
      nnzb = ib(ncol+1)-1
      len = nnzb
      if (nzmax .lt. nnzb .or. nzmax .lt. nnza) then
         ierr = -1
         return
      endif
c     
c trasnpose matrix b into c
c
      ljob = 0
      if (values) ljob = 1
      ipos = 1
      call csrcsc (ncol,ljob,ipos,b,jb,ib,c,jc,ic) 
c----------------------------------------------------------------------- 
      if (job .eq. -1) then
         do 2 k=1,len
	    c(k) = -c(k)
 2       continue
      endif
c
c--------------- main loop --------------------------------------------
c
      do 500 ii=1, nrow
         do 200 k = ic(ii),ic(ii+1)-1
            iw(jc(k)) = k
 200     continue
c-----------------------------------------------------------------------     
         do 300 ka = ia(ii), ia(ii+1)-1 
            jcol = ja(ka)
            jpos = iw(jcol)
            if (jpos .eq. 0) then
c
c     if fill-in append in coordinate format to matrix.
c 
               len = len+1
               if (len .gt. nzmax) goto 999
               jc(len) = jcol
               
               ic(len) = ii
               if (values) c(len)  = a(ka)
            else
c     else do addition.
               if (values) c(jpos) = c(jpos) + a(ka)
            endif
 300     continue
         do 301 k=ic(ii), ic(ii+1)-1
	    iw(jc(k)) = 0
 301     continue
 500  continue
c     
c     convert first part of matrix (without fill-ins) into coo format
c     
      ljob = 2
      if (values) ljob = 3
      do 501 i=1, nrow+1
         iw(i) = ic(i) 
 501  continue
      call csrcoo (nrow,ljob,nnzb,c,jc,iw,nnzb,c,ic,jc,ierr)
c
c     convert the whole thing back to csr format. 
c 
      ljob = 0
      if (values) ljob = 1
      call coicsr (nrow,len,ljob,c,jc,ic,iw)
      return
 999  ierr = ii
      return
c--------end-of-apmbt---------------------------------------------------
c-----------------------------------------------------------------------
      end
c----- end of genfeu_lstif ---------------------------------------------
c-----------------------------------------------------------------------
      subroutine assmb1 (u,nu,a,ja,ia,fu,f,nx,nelx,ijk,nodcode,
     *     node,iwk,jwk)
c--------------------------------------------------------------
c u	 = unassembled matrix u(na,node,node)
c nu	 = 1-st dimension of u
c a,ja,ia= assembled matrix on output
c fu	 = unassembled right hand side
c f      = right hand side (global load vector) assembled
c nx     = number of nodes at input
c nelx	 = number of elements at input
c ijk	 = connectivity matrix: for node k, ijk(*,k) point to the
c          nodes of element k.
c node	 = total number of nodal points in each element
c
c nodcode= boundary information list for each node with the
c	   following meaning:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point (corner points
c
c x,y   = real*8 arrays containing the $x$ and $y$ coordinates 
c	  resp. of the nodes.
c         K11, K22, and K12 at that element.
c iwk,jwk = two integer work arrays.
c ierr	= error message integer . 
c	  ierr = 0 --> normal return
c	  ierr = 1 --> negative area encountered (due to bad 
c	           numbering of nodes of an element- see
c		   message printed in unit iout). not used..
c iout	= output unit (not used here).
c--------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      real*8 u(nu,node,node),a(*),fu(node,*),f(*)
      integer ja(*),ia(*),ijk(node,*),iwk(*),jwk(*),nodcode(*)
c     max number of nonzeros per row allowed  = 200
c--------------------------------------------------------------
c     initialize
c--------------------------------------------------------------
      do 100 i=1,nx 
         f(i) = 0.0d0 
 100  continue
c     
c     initialize  pointer arrays.
c     
      do 5 k=1,nx+1
         ia(k) = 1
         jwk(k) = 0
 5    continue
      do 6 k=1,nelx
         do 59 j=1,node
            knod = ijk(j,k)
            ia(knod) = ia(knod) + 1
 59      continue
 6    continue
c---------------------------------------------------
      do 7 k=1, nx
         if (nodcode(k) .ge.1 ) ia(k)=ia(k)+1
 7    continue
c     
      ksav = ia(1)
      ia(1) = 1
      do 101 j=2, nx+1
         ksavn = ia(j)
         ia(j) = ia(j-1) +  ksav
         iwk(j-1) = ia(j-1)-1
         ksav = ksavn
 101  continue
c-----------------
c     main loop
c-----------------
      do 102 nel=1, nelx
c     
c     get nodal points
c     
         do 120 ka=1, node
            ii = ijk(ka,nel)
            f(ii) = f(ii) + fu(ka,nel)
c     
c     unpack row into jwk1
c     
            irowst = ia(ii)
            ilast  = iwk(ii) 
            do 109 k=irowst,ilast 
               jwk(ja(k)) = k
 109        continue
c     
            do 108 kb = 1,node
c     
c     column number = jj
c     
               jj = ijk(kb,nel)
               k = jwk(jj)
               if (k .eq. 0) then
                  ilast = ilast+1
                  jwk(jj) = ilast
                  ja(ilast) = jj
                  a(ilast) = u(nel,ka,kb) 
               else 
                  a(k) = a(k) + u(nel,ka,kb)
               endif
 108        continue
c     refresh jwk
            do 119 k=irowst,ilast 
               jwk(ja(k)) = 0
 119        continue
            iwk(ii) = ilast 
 120     continue
c     
 102  continue
      return
c---------end-of-assmb1---------------------------------------------- 
      end
c-----------------------------------------------------------------------
	subroutine assmbo2 (nx, nelx, node, ijk, nodcode, x, y, a, ja,
     *                   ia, f, iwk, jwk, ierr, xyk, funb, func, fung)
c----------------------------------------------------------------------- 
c nx     = number of nodes at input
c
c nelx	 = number of elements at input
c
c node	 = total number of nodal points in each element
c
c ijk	 = connectivity matrix: for node k, ijk(*,k) point to the
c          nodes of element k.
c
c nodcode= boundary information list for each node with the
c	   following meaning:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point (corner points
c
c x,y   = real arrays containing the $x$ and $y$ coordinates 
c	  resp. of the nodes.
c
c a,ja,ia= assembled matrix on output
c
c f      = right hand side (global load vector) 
c
c iwk,jwk = two integer work arrays.
c
c ierr	= error message integer . 
c	  ierr = 0 --> normal return
c	  ierr = 1 --> negative area encountered (due to bad 
c	           numbering of nodes of an element)
c
c xyk	= subroutine defining the material properties at each 
c         element. Form: 
c 	call xyk(nel,xyke,x,y,ijk,node) with on return
c         xyke =  material constant matrices. 
c         for each element nel, xyke(1,nel),xyke(2,nel) 
c         and xyke(3,nel) represent the constants
c         K11, K22, and K12 at that element.
c--------------------------------------------------------------
c
c moulitsa@cs.umn.edu : This routine yields the same results
c   as assmbo. It differs in that it constructs the ia array
c   by creating a list with the adjacent nodes for each node
c
c--------------------------------------------------------------
      implicit real*8  (a-h,o-z)
      dimension a(*),ijk(node,1),x(1),y(1),f(1),ske(3,3),fe(3),
     *      xe(3),ye(3),iwk(1),jwk(1), kwk(500)
      integer ia(1), ja(*), nodcode(1)
      external xyk, funb, func, fung

c--------------------------------------------------------------
c   initialize
c--------------------------------------------------------------
      do i=1,nx 
        f(i) = 0.0 
        iwk(i) = 0
        kwk(i) = 0
      end do

c iwk : how many elements a node belongs to
      do k=1,nelx
        do j=1,node
          knod = ijk(j,k)
          iwk(knod) = iwk(knod) + 1
        end do
      end do
c
c iwk : prepare for csr like format
      ksav=iwk(1)
      iwk(1)=1
      do j=2, nx+1
        ksavn = iwk(j)
        iwk(j) = iwk(j-1) +  ksav
        ksav = ksavn
      end do
c
c jwk : list of elements a node belongs to
      k=1
      do i=1,nelx
        do j=1,node
          knod = ijk(j,i)
          k=iwk(knod)
          jwk(k)=i
          iwk(knod)=iwk(knod)+1
        end do
      end do

c iwk : transform iwk back to what it was
      do i=nx+1,2,-1
        iwk(i)=iwk(i-1)
      end do
      iwk(1)=1

c kwk : mark edges that a node is associated with
      nedges=1
      ia(1)=1
      do i=1,nx
        kwk(i)=i
        do j=iwk(i), iwk(i+1)-1
          do k=1, node
            knod = ijk(k,jwk(j))
            if ( kwk(knod) .NE. i) then
              kwk(knod) = i
              nedges=nedges+1
            end if
          end do
        end do
        ia(i+1)=nedges
      end do
      do i=2,nx+1
        ia(i)=ia(i)+i-1
        iwk(i-1)=ia(i-1)-1
        jwk(i)=0
      end do
      jwk(1)=0
            
c-----------------
c main loop
c-----------------
      do nel=1, nelx
c
c get coordinates of nodal points
c
	do i=1, node
	  j = ijk(i,nel)
          xe(i) = x(j)
          ye(i) = y(j)
        end do
c
c compute determinant
c
c 	det=xe(2)*(ye(3)-ye(1))+xe(3)*(ye(1)-ye(2))+xe(1)*(ye(2)-ye(3))
c
c set material properties
c 
c	call xyk(nel,xyke,x,y,ijk,node)
c
c construct element stiffness matrix
c
        ierr = 0
c
c	call evalg(nel, fe, xe, ye, fung, ierr)
c	call estif3(nel,ske,fe,det,xe,ye,xyke,ierr)
        call lstif3(ske, fe, xe, ye, xyk, funb, func, fung)
        if (ierr .ne. 0) return
c
c assemble: add element stiffness matrix to global matrix
c 
        do ka=1, node
          ii = ijk(ka,nel)
	  f(ii) = f(ii) + fe(ka)
c
c unpack row into jwk1
c
	  irowst = ia(ii)
	  ilast  = iwk(ii) 
          do k=irowst,ilast 
	    jwk(ja(k)) = k
          end do
c
	  do kb = 1,node
c
c column number = jj
c
            jj = ijk(kb,nel)
	    k = jwk(jj)
	    if (k .eq. 0) then
	      ilast = ilast+1
	      jwk(jj) = ilast
	      ja(ilast) = jj
	      a(ilast) = ske(ka,kb)
	    else 
	      a(k) = a(k) + ske(ka,kb)
	    endif
          end do
c refresh jwk
          do k=irowst,ilast 
	    jwk(ja(k)) = 0
          end do
          iwk(ii) = ilast 
        end do
c
      end do

      return
      end
c----------------------------------------------------------------------- 
	subroutine assmbo (nx, nelx, node, ijk, nodcode, x, y, a, ja, 
     *                  ia, f, iwk, jwk, ierr, xyk, funb, func, fung)
c----------------------------------------------------------------------- 
c nx     = number of nodes at input
c
c nelx	 = number of elements at input
c
c node	 = total number of nodal points in each element
c
c ijk	 = connectivity matrix: for node k, ijk(*,k) point to the
c          nodes of element k.
c
c nodcode= boundary information list for each node with the
c	   following meaning:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point (corner points
c
c x,y   = real arrays containing the $x$ and $y$ coordinates 
c	  resp. of the nodes.
c
c a,ja,ia= assembled matrix on output
c
c f      = right hand side (global load vector) 
c
c iwk,jwk = two integer work arrays.
c
c ierr	= error message integer . 
c	  ierr = 0 --> normal return
c	  ierr = 1 --> negative area encountered (due to bad 
c	           numbering of nodes of an element)
c
c xyk	= subroutine defining the material properties at each 
c         element. Form: 
c 	call xyk(nel,xyke,x,y,ijk,node) with on return
c         xyke =  material constant matrices. 
c         for each element nel, xyke(1,nel),xyke(2,nel) 
c         and xyke(3,nel) represent the constants
c         K11, K22, and K12 at that element.
c--------------------------------------------------------------
c moulitsa@cs.umn.edu : It has been modified so as to handle
c      more types of domains/meshes i.e.  |\ /|
c                                         | X |
c                                         |/ \|
c--------------------------------------------------------------
      implicit real*8  (a-h,o-z)
      dimension a(*),ijk(node,1),x(1),y(1),f(1),ske(3,3),fe(3),
     *      xe(3),ye(3),iwk(1),jwk(1)
      integer ia(1), ja(*), nodcode(1)
      external xyk, funb, func, fung

c--------------------------------------------------------------
c   initialize
c--------------------------------------------------------------
      do i=1,nx 
        f(i) = 0.0 
      end do
c initialize  pointer arrays.
      do k=1,nx+1
        ia(k) = 1
        jwk(k) = 0
      end do
      do k=1,nelx
        do j=1,node
          knod = ijk(j,k)
          ia(knod) = ia(knod) + 2
        end do
      end do
c---------------------------------------------------
      do k=1, nx
	if (nodcode(k) .ge.1 ) ia(k)=ia(k)+1
      end do
c
      ksav = ia(1)
      ia(1) = 1
      do j=2, nx+1
        ksavn = ia(j)
        ia(j) = ia(j-1) +  ksav
        iwk(j-1) = ia(j-1)-1
        ksav = ksavn
      end do

c-----------------
c main loop
c-----------------
      do nel=1, nelx
c
c get coordinates of nodal points
c
	do i=1, node
	  j = ijk(i,nel)
          xe(i) = x(j)
          ye(i) = y(j)
        end do
c
c compute determinant
c
c 	det=xe(2)*(ye(3)-ye(1))+xe(3)*(ye(1)-ye(2))+xe(1)*(ye(2)-ye(3))
c
c set material properties
c 
c	call xyk(nel,xyke,x,y,ijk,node)
c
c construct element stiffness matrix
c
        ierr = 0
c
c	call evalg(nel, fe, xe, ye, fung, ierr)
c	call estif3(nel,ske,fe,det,xe,ye,xyke,ierr)
        call lstif3(ske, fe, xe, ye, xyk, funb, func, fung)
        if (ierr .ne. 0) return
c
c assemble: add element stiffness matrix to global matrix
c 
        do ka=1, node
          ii = ijk(ka,nel)
	  f(ii) = f(ii) + fe(ka)
c
c unpack row into jwk1
c
	  irowst = ia(ii)
	  ilast  = iwk(ii) 
          do k=irowst,ilast 
	    jwk(ja(k)) = k
          end do
c
	  do kb = 1,node
c
c column number = jj
c
            jj = ijk(kb,nel)
	    k = jwk(jj)
	    if (k .eq. 0) then
	      ilast = ilast+1
	      jwk(jj) = ilast
	      ja(ilast) = jj
	      a(ilast) = ske(ka,kb)
	    else 
	      a(k) = a(k) + ske(ka,kb)
	    endif
          end do
c refresh jwk
          do k=irowst,ilast 
	    jwk(ja(k)) = 0
          end do
          iwk(ii) = ilast 
        end do
c
      end do

c squeeze away the zero entries
c added so as to handle more type of domains/meshes
      do i=1, nx
        ista=ia(i)
        isto=ia(i+1)-1
        do j=ista, isto
          if (ja(j) .EQ. 0) then
            iwk(i)=j-ista
            go to 200
          end if
        end do
 200    continue
      end do

      do i=2, nx
        ksav=ia(i)
        ia(i)=ia(i-1)+iwk(i-1)
        ksavn=ia(i)
        do j=0, iwk(i)-1
          ja(ksavn+j)=ja(ksav+j)
          a(ksavn+j) = a(ksav+j)
        end do
      end do
      ia(nx+1)=ia(nx)+iwk(nx)

      return
      end
c-----------------------------------------------------------------------
      subroutine atmux (n, x, y, a, ja, ia)
      real*8 x(*), y(*), a(*) 
      integer n, ia(*), ja(*)
c-----------------------------------------------------------------------
c         transp( A ) times a vector
c----------------------------------------------------------------------- 
c multiplies the transpose of a matrix by a vector when the original
c matrix is stored in compressed sparse row storage. Can also be
c viewed as the product of a matrix by a vector when the original
c matrix is stored in the compressed sparse column format.
c-----------------------------------------------------------------------
c
c on entry:
c----------
c n     = row dimension of A
c x     = real array of length equal to the column dimension of
c         the A matrix.
c a, ja,
c    ia = input matrix in compressed sparse row format.
c
c on return:
c-----------
c y     = real array of length n, containing the product y=transp(A)*x
c
c-----------------------------------------------------------------------
c     local variables 
c
      integer i, k 
c-----------------------------------------------------------------------
c
c     zero out output vector
c 
      do 1 i=1,n
         y(i) = 0.0
 1    continue
c
c loop over the rows
c
      do 100 i = 1,n
         do 99 k=ia(i), ia(i+1)-1 
            y(ja(k)) = y(ja(k)) + x(i)*a(k)
 99      continue
 100  continue
c
      return
c-------------end-of-atmux---------------------------------------------- 
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine atmuxr (m, n, x, y, a, ja, ia)
      real*8 x(*), y(*), a(*) 
      integer m, n, ia(*), ja(*)
c-----------------------------------------------------------------------
c         transp( A ) times a vector, A can be rectangular
c----------------------------------------------------------------------- 
c See also atmux.  The essential difference is how the solution vector
c is initially zeroed.  If using this to multiply rectangular CSC 
c matrices by a vector, m number of rows, n is number of columns.
c-----------------------------------------------------------------------
c
c on entry:
c----------
c m     = column dimension of A
c n     = row dimension of A
c x     = real array of length equal to the column dimension of
c         the A matrix.
c a, ja,
c    ia = input matrix in compressed sparse row format.
c
c on return:
c-----------
c y     = real array of length n, containing the product y=transp(A)*x
c
c-----------------------------------------------------------------------
c     local variables 
c
      integer i, k 
c-----------------------------------------------------------------------
c
c     zero out output vector
c 
      do 1 i=1,m
         y(i) = 0.0
 1    continue
c
c loop over the rows
c
      do 100 i = 1,n
         do 99 k=ia(i), ia(i+1)-1 
            y(ja(k)) = y(ja(k)) + x(i)*a(k)
 99      continue
 100  continue
c
      return
c-------------end-of-atmuxr--------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine avnz_col(n,ja,ia,iao, ndiag, av, st)
      implicit real*8 (a-h, o-z)
      real*8 av, st
      integer n,  ja(*), ia(n+1), iao(n+1)
c---------------------------------------------------------------------
c     this routine computes average number of nonzero elements/column and
c     standard deviation for this average
c---------------------------------------------------------------------
c
c On entry :
c-----------
c n     = integer. column dimension of matrix
c ja    = integer array containing the row indices of elements in a
c ia    = integer array containing of length n+1 containing the
c         pointers to the beginning of the columns in arrays a and ja.
c         It is assumed that ia(*) = 1 and ia(n+1) = nzz+1.
c ndiag = number of the most important diagonals
c On return
c----------
c av    = average number of nonzero elements/column
c st    = standard deviation for this average
c Notes
c---------
c standard deviation will not be correct for symmetric storage. 
c----------------------------------------------------------------------
c     standard deviatioan for the average
      st = 0.0d0
c     average and standard deviation
c     
      av = real(ia(n+1)-1)/real(n)
c     
c     will be corrected later.
c     
      do 3 i=1,n
         j0 = ia(i)
         j1 = ia(i+1) - 1
c     indiag = nonzero diagonal element indicator
         do 31 k=j0, j1
            j=ja(k)
 31      continue
         lenc = j1+1-j0
         st = st + (real(lenc) - av)**2
 3    continue
c     
      st = sqrt( st / real(n) )
      return
      end
c-----------------------------------------------------------------------
      subroutine bandpart(n,ja,ia,dist,nper,band)
      implicit real*8 (a-h, o-z)
      integer n,ja(*), ia(n+1),dist(*)
      integer nper,band
c-------------------------------------------------------------------------
c this routine computes the bandwidth of the banded matrix, which contains
c 'nper' percent of the original matrix.
c-------------------------------------------------------------------------
c On entry :
c-----------
c n     = integer. column dimension of matrix
c ja    = integer array containing the row indices of elements in a
c ia    = integer array containing of length n+1 containing the
c         pointers to the beginning of the columns in arrays a and ja.
c         It is assumed that ia(*) = 1 and ia(n+1) = nzz+1.
c dist  = integer array containing the numbers of elements in the 
c         matrix with different distance of row indices and column 
c         indices.
c nper  = percentage of matrix  within the bandwidth
c on return
c----------
c band  = the width of the bandwidth
c----------------------------------------------------------------------
      nnz  = ia(n+1)-ia(1)
      iacc  = dist(n)
      band  = 0
      j     = 0
 10   j     = j+1
      iacc  = iacc + dist(n+j) +dist(n-j) 
      if (iacc*100 .le. nnz*nper) then 
         band = band +1 
         goto 10         
      endif 
      return 
      end 
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c                  INFORMATION ROUTINES. INFO MODULE                   c
c----------------------------------------------------------------------c
c bandwidth :  Computes  ml     = lower_bandwidth(A)                   c
c                        mu     = upper_bandwidth(A)                   c
c                        iband  = max_bandwidth(A)                     c
c                        bndav  = average_bandwidth(A)                 c
c nonz      :  Computes  nzmaxc = max_column_length(A)                 c
c                        nzminc = min_column_length(A)                 c
c                        nzmaxr = max_row_length(A)                    c
c                        nzminr = min_row_length(A)                    c
c                        nzcol  = zero_column_number(A)                c
c                        nzrow  = zero_row_number(A)                   c
c diag_domi :  Computes  ddomc  = diag_domi_column_percentage(A)       c
c                        ddomr  = diag_domi_row_percentage(A)          c
c frobnorm  :  Computes  Fnorm  = Frobenius_norm(A)                    c
c ansym     :  Computes  fas    = sym_part_Frobenius_norm(A)           c
c                        fan    = nonsym_part_Frobenius_norm(A)        c
c                        imatch = matching_elements_number(A)          c
c                        av     = relative_sym_match(A)                c
c distaij   :  Computes  dist   = average_dist_of_a(i,j)(A)            c
c                        std    = standard_deviation(A)                c
c skyline   :  Computes  nsky   = nonzero_number_in_skyline(A)         c
c distdiag  :  Computes  dist   = element_number_in_eachdiag(A)        c
c bandpart  :  Computes  band   = bandwidth_width(A)                   c
c n_imp_diag:  Computes  ndiag  = important_diag_number(A)             c
c nonz_lud  :  Computes  nlower = nonzero_number_of_lower_part(A)      c
c                        nupper = nonzero_number_of_upper_part(A)      c
c                        ndiag  = nonzero_number_of_maindiag(A)        c
c avnz_col  :  Computes  av     = average_nonzero_number_in_column(A)  c
c                        st     = standard_deviation(A)                c
c vbrinfo   :  Print info on matrix in variable block row format       c
c----------------------------------------------------------------------c
      subroutine bandwidth(n,ja, ia, ml, mu, iband, bndav)
      implicit none 
      integer n, ml, mu, iband
      integer ja(*), ia(n+1)
      real*8 bndav
c-----------------------------------------------------------------------
c this routine computes the lower, upper, maximum, and average 
c bandwidths.     revised -- July 12, 2001  -- bug fix -- YS. 
c-----------------------------------------------------------------------
c On Entry:
c----------
c n     = integer. column dimension of matrix
c a     = real array containing the nonzero elements of the matrix
c         the elements are stored by columns in order
c         (i.e. column i comes before column i+1, but the elements
c         within each column can be disordered).
c ja    = integer array containing the row indices of elements in a
c ia    = integer array containing of length n+1 containing the
c         pointers to the beginning of the columns in arrays a and ja.
c         It is assumed that ia(*) = 1 and ia(n+1) = nzz+1.
c
c on return
c----------
c ml    = lower bandwidth as defined by
c        ml = max(i-j | all  a(i,j).ne. 0)
c mu    = upper bandwidth as defined by
c        mu = max ( j-i | all  a(i,j).ne. 0 )
c iband =  maximum bandwidth as defined by
c         iband = Max (  Max [ j | a(i,j) .ne. 0 ] - 
c                        Min [ j | a(i,j) .ne. 0 ] )
c bndav = Average Bandwidth          
c-----------------------------------------------------------------------
c     locals
      integer max 
      integer j0, j1, jminc, jmaxc, i
c-----------------------------------------------------------------------
      ml = -n
      mu = -n
      bndav = 0.0d0
      iband = 0 
      do 10 i=1,n
         j0 = ia(i)
         j1 = ia(i+1) - 1
         jminc = ja(j0)
         jmaxc = ja(j1)
         ml = max(ml,i-jminc)
         mu = max(mu,jmaxc-i)
         iband = max(iband,jmaxc-jminc+1)
         bndav = bndav+real( jmaxc-jminc+1)
 10   continue
      bndav = bndav/real(n)
      return
c-----end-of-bandwidth--------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----end-of-cgnr
c-----------------------------------------------------------------------
      subroutine bcg(n,rhs,sol,ipar,fpar,w)
      implicit none
      integer n, ipar(16)
      real*8  fpar(16), rhs(n), sol(n), w(n,*)
c-----------------------------------------------------------------------
c     BCG: Bi Conjugate Gradient method. Programmed with reverse
c     communication, see the header for detailed specifications
c     of the protocol.
c
c     in this routine, before successful return, the fpar's are
c     fpar(3) == initial residual norm
c     fpar(4) == target residual norm
c     fpar(5) == current residual norm
c     fpar(7) == current rho (rhok = <r, s>)
c     fpar(8) == previous rho (rhokm1)
c
c     w(:,1) -- r, the residual
c     w(:,2) -- s, the dual of the 'r'
c     w(:,3) -- p, the projection direction
c     w(:,4) -- q, the dual of the 'p'
c     w(:,5) -- v, a scratch vector to store A*p, or A*q.
c     w(:,6) -- a scratch vector to store intermediate results
c     w(:,7) -- changes in the solution
c-----------------------------------------------------------------------
c     external routines used
c
      real*8 distdot
      logical stopbis,brkdn
      external distdot, stopbis, brkdn
c
      real*8 one
      parameter(one=1.0D0)
c
c     local variables
c
      integer i
      real*8 alpha
      logical rp, lp
      save
c
c     status of the program
c
      if (ipar(1).le.0) ipar(10) = 0
      goto (10, 20, 40, 50, 60, 70, 80, 90, 100, 110), ipar(10)
c
c     initialization, initial residual
c
      call bisinit(ipar,fpar,7*n,1,lp,rp,w)
      if (ipar(1).lt.0) return
c
c     compute initial residual, request a matvecc
c
      ipar(1) = 1
      ipar(8) = 3*n+1
      ipar(9) = ipar(8) + n
      do i = 1, n
         w(i,4) = sol(i)
      enddo
      ipar(10) = 1
      return
 10   ipar(7) = ipar(7) + 1
      ipar(13) = ipar(13) + 1
      do i = 1, n
         w(i,1) = rhs(i) - w(i,5)
      enddo
      fpar(11) = fpar(11) + n
      if (lp) then
         ipar(1) = 3
         ipar(8) = 1
         ipar(9) = n+1
         ipar(10) = 2
         return
      endif
c
 20   if (lp) then
         do i = 1, n
            w(i,1) = w(i,2)
            w(i,3) = w(i,2)
            w(i,4) = w(i,2)
         enddo
      else
         do i = 1, n
            w(i,2) = w(i,1)
            w(i,3) = w(i,1)
            w(i,4) = w(i,1)
         enddo
      endif
c
      fpar(7) = distdot(n,w,1,w,1)
      fpar(11) = fpar(11) + 2 * n
      fpar(3) = sqrt(fpar(7))
      fpar(5) = fpar(3)
      fpar(8) = one
      if (abs(ipar(3)).eq.2) then
         fpar(4) = fpar(1) * sqrt(distdot(n,rhs,1,rhs,1)) + fpar(2)
         fpar(11) = fpar(11) + 2 * n
      else if (ipar(3).ne.999) then
         fpar(4) = fpar(1) * fpar(3) + fpar(2)
      endif
      if (ipar(3).ge.0.and.fpar(5).le.fpar(4)) then
         fpar(6) = fpar(5)
         goto 900
      endif
c
c     end of initialization, begin iteration, v = A p
c
 30   if (rp) then
         ipar(1) = 5
         ipar(8) = n + n + 1
         if (lp) then
            ipar(9) = 4*n + 1
         else
            ipar(9) = 5*n + 1
         endif
         ipar(10) = 3
         return
      endif
c
 40   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = n + n + 1
      endif
      if (lp) then
         ipar(9) = 5*n + 1
      else
         ipar(9) = 4*n + 1
      endif
      ipar(10) = 4
      return
c
 50   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = 4*n + 1
         ipar(10) = 5
         return
      endif
c
 60   ipar(7) = ipar(7) + 1
      alpha = distdot(n,w(1,4),1,w(1,5),1)
      fpar(11) = fpar(11) + 2 * n
      if (brkdn(alpha,ipar)) goto 900
      alpha = fpar(7) / alpha
      do i = 1, n
         w(i,7) = w(i,7) + alpha * w(i,3)
         w(i,1) = w(i,1) - alpha * w(i,5)
      enddo
      fpar(11) = fpar(11) + 4 * n
      if (ipar(3).eq.999) then
         ipar(1) = 10
         ipar(8) = 6*n + 1
         ipar(9) = 5*n + 1
         ipar(10) = 6
         return
      endif
 70   if (ipar(3).eq.999) then
         if (ipar(11).eq.1) goto 900
      else if (stopbis(n,ipar,1,fpar,w,w(1,3),alpha)) then
         goto 900
      endif
c
c     A^t * x
c
      if (lp) then
         ipar(1) = 4
         ipar(8) = 3*n + 1
         if (rp) then
            ipar(9) = 4*n + 1
         else
            ipar(9) = 5*n + 1
         endif
         ipar(10) = 7
         return
      endif
c
 80   ipar(1) = 2
      if (lp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = 3*n + 1
      endif
      if (rp) then
         ipar(9) = 5*n + 1
      else
         ipar(9) = 4*n + 1
      endif
      ipar(10) = 8
      return
c
 90   if (rp) then
         ipar(1) = 6
         ipar(8) = ipar(9)
         ipar(9) = 4*n + 1
         ipar(10) = 9
         return
      endif
c
 100  ipar(7) = ipar(7) + 1
      do i = 1, n
         w(i,2) = w(i,2) - alpha * w(i,5)
      enddo
      fpar(8) = fpar(7)
      fpar(7) = distdot(n,w,1,w(1,2),1)
      fpar(11) = fpar(11) + 4 * n
      if (brkdn(fpar(7), ipar)) return
      alpha = fpar(7) / fpar(8)
      do i = 1, n
         w(i,3) = w(i,1) + alpha * w(i,3)
         w(i,4) = w(i,2) + alpha * w(i,4)
      enddo
      fpar(11) = fpar(11) + 4 * n
c
c     end of the iterations
c
      goto 30
c
c     some clean up job to do
c
 900  if (rp) then
         if (ipar(1).lt.0) ipar(12) = ipar(1)
         ipar(1) = 5
         ipar(8) = 6*n + 1
         ipar(9) = ipar(8) - n
         ipar(10) = 10
         return
      endif
 110  if (rp) then
         call tidycg(n,ipar,fpar,sol,w(1,6))
      else
         call tidycg(n,ipar,fpar,sol,w(1,7))
      endif
      return
c-----end-of-bcg
      end
c-----------------------------------------------------------------------
      subroutine bcgstab(n, rhs, sol, ipar, fpar, w)
      implicit none
      integer n, ipar(16)
      real*8 rhs(n), sol(n), fpar(16), w(n,8)
c-----------------------------------------------------------------------
c     BCGSTAB --- Bi Conjugate Gradient stabilized (BCGSTAB)
c     This is an improved BCG routine. (1) no matrix transpose is
c     involved. (2) the convergence is smoother.
c
c
c     Algorithm:
c     Initialization - r = b - A x, r0 = r, p = r, rho = (r0, r),
c     Iterate -
c     (1) v = A p
c     (2) alpha = rho / (r0, v)
c     (3) s = r - alpha v
c     (4) t = A s
c     (5) omega = (t, s) / (t, t)
c     (6) x = x + alpha * p + omega * s
c     (7) r = s - omega * t
c     convergence test goes here
c     (8) beta = rho, rho = (r0, r), beta = rho * alpha / (beta * omega)
c         p = r + beta * (p - omega * v)
c
c     in this routine, before successful return, the fpar's are
c     fpar(3) == initial (preconditionied-)residual norm
c     fpar(4) == target (preconditionied-)residual norm
c     fpar(5) == current (preconditionied-)residual norm
c     fpar(6) == current residual norm or error
c     fpar(7) == current rho (rhok = <r, r0>)
c     fpar(8) == alpha
c     fpar(9) == omega
c
c     Usage of the work space W
c     w(:, 1) = r0, the initial residual vector
c     w(:, 2) = r, current residual vector
c     w(:, 3) = s
c     w(:, 4) = t
c     w(:, 5) = v
c     w(:, 6) = p
c     w(:, 7) = tmp, used in preconditioning, etc.
c     w(:, 8) = delta x, the correction to the answer is accumulated
c               here, so that the right-preconditioning may be applied
c               at the end
c-----------------------------------------------------------------------
c     external routines used
c
      real*8 distdot
      logical stopbis, brkdn
      external distdot, stopbis, brkdn
c
      real*8 one
      parameter(one=1.0D0)
c
c     local variables
c
      integer i
      real*8 alpha,beta,rho,omega
      logical lp, rp
      save lp, rp
c
c     where to go
c
      if (ipar(1).gt.0) then
         goto (10, 20, 40, 50, 60, 70, 80, 90, 100, 110) ipar(10)
      else if (ipar(1).lt.0) then
         goto 900
      endif
c
c     call the initialization routine
c
      call bisinit(ipar,fpar,8*n,1,lp,rp,w)
      if (ipar(1).lt.0) return
c
c     perform a matvec to compute the initial residual
c
      ipar(1) = 1
      ipar(8) = 1
      ipar(9) = 1 + n
      do i = 1, n
         w(i,1) = sol(i)
      enddo
      ipar(10) = 1
      return
 10   ipar(7) = ipar(7) + 1
      ipar(13) = ipar(13) + 1
      do i = 1, n
         w(i,1) = rhs(i) - w(i,2)
      enddo
      fpar(11) = fpar(11) + n
      if (lp) then
         ipar(1) = 3
         ipar(10) = 2
         return
      endif
c
 20   if (lp) then
         do i = 1, n
            w(i,1) = w(i,2)
            w(i,6) = w(i,2)
         enddo
      else
         do i = 1, n
            w(i,2) = w(i,1)
            w(i,6) = w(i,1)
         enddo
      endif
c
      fpar(7) = distdot(n,w,1,w,1)
      fpar(11) = fpar(11) + 2 * n
      fpar(5) = sqrt(fpar(7))
      fpar(3) = fpar(5)
      if (abs(ipar(3)).eq.2) then
         fpar(4) = fpar(1) * sqrt(distdot(n,rhs,1,rhs,1)) + fpar(2)
         fpar(11) = fpar(11) + 2 * n
      else if (ipar(3).ne.999) then
         fpar(4) = fpar(1) * fpar(3) + fpar(2)
      endif
      if (ipar(3).ge.0) fpar(6) = fpar(5)
      if (ipar(3).ge.0 .and. fpar(5).le.fpar(4) .and.
     +     ipar(3).ne.999) then
         goto 900
      endif
c
c     beginning of the iterations
c
c     Step (1), v = A p
 30   if (rp) then
         ipar(1) = 5
         ipar(8) = 5*n+1
         if (lp) then
            ipar(9) = 4*n + 1
         else
            ipar(9) = 6*n + 1
         endif
         ipar(10) = 3
         return
      endif
c
 40   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = 5*n+1
      endif
      if (lp) then
         ipar(9) = 6*n + 1
      else
         ipar(9) = 4*n + 1
      endif
      ipar(10) = 4
      return
 50   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = 4*n + 1
         ipar(10) = 5
         return
      endif
c
 60   ipar(7) = ipar(7) + 1
c
c     step (2)
      alpha = distdot(n,w(1,1),1,w(1,5),1)
      fpar(11) = fpar(11) + 2 * n
      if (brkdn(alpha, ipar)) goto 900
      alpha = fpar(7) / alpha
      fpar(8) = alpha
c
c     step (3)
      do i = 1, n
         w(i,3) = w(i,2) - alpha * w(i,5)
      enddo
      fpar(11) = fpar(11) + 2 * n
c
c     Step (4): the second matvec -- t = A s
c
      if (rp) then
         ipar(1) = 5
         ipar(8) = n+n+1
         if (lp) then
            ipar(9) = ipar(8)+n
         else
            ipar(9) = 6*n + 1
         endif
         ipar(10) = 6
         return
      endif
c
 70   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = n+n+1
      endif
      if (lp) then
         ipar(9) = 6*n + 1
      else
         ipar(9) = 3*n + 1
      endif
      ipar(10) = 7
      return
 80   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = 3*n + 1
         ipar(10) = 8
         return
      endif
 90   ipar(7) = ipar(7) + 1
c
c     step (5)
      omega = distdot(n,w(1,4),1,w(1,4),1)
      fpar(11) = fpar(11) + n + n
      if (brkdn(omega,ipar)) goto 900
      omega = distdot(n,w(1,4),1,w(1,3),1) / omega
      fpar(11) = fpar(11) + n + n
      if (brkdn(omega,ipar)) goto 900
      fpar(9) = omega
      alpha = fpar(8)
c
c     step (6) and (7)
      do i = 1, n
         w(i,7) = alpha * w(i,6) + omega * w(i,3)
         w(i,8) = w(i,8) + w(i,7)
         w(i,2) = w(i,3) - omega * w(i,4)
      enddo
      fpar(11) = fpar(11) + 6 * n + 1
c
c     convergence test
      if (ipar(3).eq.999) then
         ipar(1) = 10
         ipar(8) = 7*n + 1
         ipar(9) = 6*n + 1
         ipar(10) = 9
         return
      endif
      if (stopbis(n,ipar,2,fpar,w(1,2),w(1,7),one))  goto 900
 100  if (ipar(3).eq.999.and.ipar(11).eq.1) goto 900
c
c     step (8): computing new p and rho
      rho = fpar(7)
      fpar(7) = distdot(n,w(1,2),1,w(1,1),1)
      omega = fpar(9)
      beta = fpar(7) * fpar(8) / (fpar(9) * rho)
      do i = 1, n
         w(i,6) = w(i,2) + beta * (w(i,6) - omega * w(i,5))
      enddo
      fpar(11) = fpar(11) + 6 * n + 3
      if (brkdn(fpar(7),ipar)) goto 900
c
c     end of an iteration
c
      goto 30
c
c     some clean up job to do
c
 900  if (rp) then
         if (ipar(1).lt.0) ipar(12) = ipar(1)
         ipar(1) = 5
         ipar(8) = 7*n + 1
         ipar(9) = ipar(8) - n
         ipar(10) = 10
         return
      endif
 110  if (rp) then
         call tidycg(n,ipar,fpar,sol,w(1,7))
      else
         call tidycg(n,ipar,fpar,sol,w(1,8))
      endif
c
      return
c-----end-of-bcgstab
      end
      

      function betfun(side, x, y, z)
      real*8 betfun, x, y, z
      character*2 side
      betfun = 1.0
      return
      end
c----------------------------------------------------------------------- 
      subroutine BFS(n,ja,ia,nfirst,iperm,mask,maskval,riord,levels,
     *     nlev)
      implicit none 
      integer n,ja(*),ia(*),nfirst,iperm(n),mask(n),riord(*),levels(*),
     *     nlev,maskval 
c-----------------------------------------------------------------------
c finds the level-structure (breadth-first-search or CMK) ordering for a
c given sparse matrix. Uses add_lvst. Allows an set of nodes to be 
c the initial level (instead of just one node). 
c-------------------------parameters------------------------------------
c on entry:
c---------
c     n      = number of nodes in the graph 
c     ja, ia = pattern of matrix in CSR format (the ja,ia arrays of csr data
c     structure)
c     nfirst = number of nodes in the first level that is input in riord
c     iperm  = integer array indicating in which order to  traverse the graph
c     in order to generate all connected components. 
c     if iperm(1) .eq. 0 on entry then BFS will traverse the nodes
c     in the  order 1,2,...,n.
c     
c     riord  = (also an ouput argument). On entry riord contains the labels  
c     of the nfirst nodes that constitute the first level.      
c     
c     mask   = array used to indicate whether or not a node should be 
c     condidered in the graph. see maskval.
c     mask is also used as a marker of  visited nodes. 
c     
c     maskval= consider node i only when:  mask(i) .eq. maskval 
c     maskval must be .gt. 0. 
c     thus, to consider all nodes, take mask(1:n) = 1. 
c     maskval=1 (for example) 
c     
c     on return
c     ---------
c     mask   = on return mask is restored to its initial state. 
c     riord  = `reverse permutation array'. Contains the labels of the nodes
c     constituting all the levels found, from the first level to
c     the last. 
c     levels = pointer array for the level structure. If lev is a level
c     number, and k1=levels(lev),k2=levels(lev+1)-1, then
c     all the nodes of level number lev are:
c     riord(k1),riord(k1+1),...,riord(k2) 
c     nlev   = number of levels found
c-----------------------------------------------------------------------
c     
      integer j, ii, nod, istart, iend 
      logical permut
      permut = (iperm(1) .ne. 0) 
c     
c     start pointer structure to levels 
c     
      nlev   = 0 
c     
c     previous end
c     
      istart = 0 
      ii = 0
c     
c     current end 
c     
      iend = nfirst
c     
c     intialize masks to zero -- except nodes of first level -- 
c     
      do 12 j=1, nfirst 
         mask(riord(j)) = 0 
 12   continue
c-----------------------------------------------------------------------
 13   continue 
c     
 1    nlev = nlev+1
      levels(nlev) = istart + 1
      call add_lvst (istart,iend,nlev,riord,ja,ia,mask,maskval) 
      if (istart .lt. iend) goto 1
 2    ii = ii+1 
      if (ii .le. n) then
         nod = ii         
         if (permut) nod = iperm(nod)          
         if (mask(nod) .eq. maskval) then
c     
c     start a new level
c
            istart = iend
            iend = iend+1 
            riord(iend) = nod
            mask(nod) = 0
            goto 1
         else 
            goto 2
         endif
      endif
c----------------------------------------------------------------------- 
 3    levels(nlev+1) = iend+1 
      do j=1, iend
         mask(riord(j)) = maskval 
      enddo
c----------------------------------------------------------------------- 
      return
      end
c-----end-of-brkdn
c-----------------------------------------------------------------------
      subroutine bisinit(ipar,fpar,wksize,dsc,lp,rp,wk)
      implicit none
      integer i,ipar(16),wksize,dsc
      logical lp,rp
      real*8  fpar(16),wk(*)
c-----------------------------------------------------------------------
c     some common initializations for the iterative solvers
c-----------------------------------------------------------------------
      real*8 zero, one
      parameter(zero=0.0D0, one=1.0D0)
c
c     ipar(1) = -2 inidcate that there are not enough space in the work
c     array
c
      if (ipar(4).lt.wksize) then
         ipar(1) = -2
         ipar(4) = wksize
         return
      endif
c
      if (ipar(2).gt.2) then
         lp = .true.
         rp = .true.
      else if (ipar(2).eq.2) then
         lp = .false.
         rp = .true.
      else if (ipar(2).eq.1) then
         lp = .true.
         rp = .false.
      else
         lp = .false.
         rp = .false.
      endif
      if (ipar(3).eq.0) ipar(3) = dsc
c     .. clear the ipar elements used
      ipar(7) = 0
      ipar(8) = 0
      ipar(9) = 0
      ipar(10) = 0
      ipar(11) = 0
      ipar(12) = 0
      ipar(13) = 0
c
c     fpar(1) must be between (0, 1), fpar(2) must be positive,
c     fpar(1) and fpar(2) can NOT both be zero
c     Normally return ipar(1) = -4 to indicate any of above error
c
      if (fpar(1).lt.zero .or. fpar(1).ge.one .or. fpar(2).lt.zero .or.
     &     (fpar(1).eq.zero .and. fpar(2).eq.zero)) then
         if (ipar(1).eq.0) then
            ipar(1) = -4
            return
         else
            fpar(1) = 1.0D-6
            fpar(2) = 1.0D-16
         endif
      endif
c     .. clear the fpar elements
      do i = 3, 10
         fpar(i) = zero
      enddo
      if (fpar(11).lt.zero) fpar(11) = zero
c     .. clear the used portion of the work array to zero
      do i = 1, wksize
         wk(i) = zero
      enddo
c
      return
c-----end-of-bisinit
      end
c **********************************************************************
      subroutine BLCCN1(n, nbloc, nblcmx, nsbloc, lpw, kpw, ia, ja,
     *                  mark, mxccex, lccnex, mxptbl, iout, ier)
C-----------------------------------------------------------------------
c     
c     This routine determines if the matrix given by the structure
c     IA et JA is irreductible. If not, it orders the unknowns such
c     that all the consecutive unknowns in KPW between NSBLOC(i-1)+1
c     and NSBLOC(i) belong to the ith component of the matrix.
c
c     On entry:
c     --------
c     n      = row and column dimension of the matrix
c     nblcmx = The size of NSBLOC is nblcmx + 1 (in fact, it starts at 0).
c     ia     = integer array of length N+1 corresponding to the
c              pointer to the beginning of each row in JA (compressed
c              sparse row storage).
c     ja     = integer array of length NNZERO (= IA(N+1)-IA(1)) corresponding
c              to the column indices of nonzero elements of the matrix,
c              stored rowwise.
c     mxccex = maximum number of connected components allowed by block.
c     mxptbl = maximum number of points (or unknowns) in each connected
c              component (mxptbl .le. n).
c     iout   = impression parameter. If 0 < iout < 100, we print
c              comments and error messages on unit IOUT.
c
c     Input/Output:
c     ------------
c     nbloc  = number of connected components of the matrix. If the
c              matrix is not irreductible, nbloc > 1. We allow
c              nbloc > 1 on entry; in this case we calculate the
c              number of connected components in each previous one.
c     nsbloc = integer array of length NBLOC + 1 containing the pointers
c              to the first node of each component on the new ordering.
c              Normally, on entry you put: NBLOC = 1, NSBLOC(0) = 0,
c              NSBLOC(NBLOC) = N.
c
c     On return:
c     ----------
c     lpw    = integer array of length N corresponding to the
c              permutation vector (the row i goes to lpw(i)).
c     ier    = integer. Error message. Normal return ier = 0.
c
c     Work space:
c     ----------
c     kpw    = integer vector of length MXPTBL*NBLOC necessary for parallel
c              computation.
c     mark   = integer vector of length N
c     lccnex = integer vector of length (MXCCEX+1)*NBLOC necessary for parallel
c              computation.
c
C-----------------------------------------------------------------------
C     Laura C. Dutto - e-mail: dutto@cerca.umontreal.ca
c                      Juillet 1992. Update: March 1994
C-----------------------------------------------------------------------
      dimension lpw(n), kpw(mxptbl*nbloc), ia(n+1), ja(*),
     *          lccnex((mxccex+1)*nbloc), nsbloc(0:nbloc), mark(n)
      logical impr
      character chsubr*6
C-----------------------------------------------------------------------
      ier  = 0
      impr = iout.gt.0.and.iout.le.99
      isor = 0
c
      chsubr = 'CCONEX'
      newblc = 0
C$DOACROSS if(nbloc.gt.1), LOCAL(ibloc, ik0, ik1, ins0, ntb0,
C$&   nccnex, ilccnx, info, kpibl), REDUCTION(ier, newblc)
      do 100 ibloc = 1,nbloc
         ik0 = nsbloc(ibloc - 1)
         ik1 = nsbloc(ibloc)
         ntb0 = ia(ik0+1)
         if(ia(ik1+1) - ntb0 .le. 1) go to 100
         ntb0 = ntb0 - 1
         ins0 = ik1 - ik0
c........We need more memory place for KPW1 because of parallel computation
         kpibl = (ibloc-1) * mxptbl
         call numini( ins0, kpw(kpibl+1))
         nccnex = mxccex
         ilccnx = (mxccex+1) * (ibloc-1) + 1
c.......................................................................
c
c        Call to the main routine: CCONEX
c
c.......................................................................
         call cconex(ins0, ik0, nccnex, lccnex(ilccnx), kpw(kpibl+1), 
     *               ia(ik0+1), ja(ntb0+1), mark(ik0+1), isor, info)
         ier = ier + info
         if(info .ne. 0 .or. nccnex .lt. 1) go to 100
c
c........We add the new connected components on NEWBLC
         newblc = newblc + nccnex
c........We define LPW different from the identity only if there are more 
c........than one connected component in this block
         if(nccnex .eq. 1) then
              call numini(ins0, lpw(ik0+1))
         else
              call invlpw(ins0, kpw(kpibl+1), lpw(ik0+1))
         endif
         call iplusa(ins0, ik0, 1, lpw(ik0+1))
 100  continue
c
      if(ier .ne. 0) go to 218
      if(newblc .eq. nbloc) go to 120
      if(newblc .gt. nblcmx) go to 230
c
c.....We modify the number of blocks to indicate the number of connected 
c.....components in the matrix.
      newblc = 0
      nsfin = 0
CDIR$ NEXT SCALAR
      do ibloc = 1, nbloc
         ilccnx = (mxccex+1) * (ibloc-1) + 1
         nccnex = lccnex(ilccnx)
         if(nccnex .gt. 1 .and. impr) write(iout,420) ibloc,nccnex
         lcc0 = 0
CDIR$ NEXT SCALAR
         do icc = 1,nccnex
            newblc = newblc + 1
            nsb = lccnex(ilccnx+icc)
c...........Be careful! In LCCNEX we have the cumulated number of vertices 
            nsbloc(newblc) = nsfin + nsb
            if(nccnex .gt. 1 .and. impr) write(iout,425) icc,nsb-lcc0
            lcc0 = nsb
         enddo
         nsfin = nsfin + nsb
      enddo
      nbloc = newblc
c
 120  return
c
 218  if(impr) write(iout,318) chsubr,ier
      go to 120
 230  if(impr) write(iout,330) newblc,nblcmx
      if(ier.eq.0) ier = -1
      go to 120
c
 318  format(' ***BLCCN1*** ERROR IN ',a6,'. IER = ',i8)
 330  format(' ***BLCCN1*** THE MEMORY SPACE ALLOWED FOR NSBLOC IS',
     *       ' NOT ENOUGH.'/13X,' NUMBER (NECESSARY) OF CONNECTED',
     *       ' COMPONENTS = ',I5/13X,' MAXIMAL NUMBER OF BLOCKS',14x,
     *       '= ',i5)
 420  FORMAT(' *** The block ',i3,' has ',i3,' strongly connected',
     *       ' components. The number of vertices by component is:')
 425  format(5x,'Component No.',i3,' - Number of vertices = ',i6)
      end

c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c          REORDERING ROUTINES -- STRONGLY CONNECTED COMPONENTS        c 
c----------------------------------------------------------------------c
c     Contributed by:
C     Laura C. Dutto - email: dutto@cerca.umontreal.ca
c                      July 1992 - Update: March 1994
C-----------------------------------------------------------------------
c     CONTENTS:
c     --------
c     blccnx : Driver routine to reduce the structure of a  matrix 
c              to its strongly connected components.
c     cconex : Main routine to compute the strongly connected components
c              of a (block diagonal) matrix.
c     anccnx : We put in ICCNEX the vertices marked in the component MCCNEX.
c     newcnx : We put in ICCNEX the vertices marked in the component
c              MCCNEX. We modify also the vector KPW.
c     blccn1 : Parallel computation of the connected components of a
c              matrix. The parallel loop is performed only if the matrix
c              has a block diagonal structure.
c     ccnicopy:We copy an integer vector into anothoer.
c     compos : We calculate the composition between two permutation
c              vectors.
c     invlpw : We calculate the inverse of a permutation vector.
c     numini : We initialize a vector to the identity.
c     tbzero : We initialize to ZERO an integer vector.
c     iplusa : Given two integers IALPHA and IBETA, for an integer vector 
c              IA we calculate IA(i) = ialpha + ibeta * ia(i)
C
c----------------------------------------------------------------------c
      subroutine BLCCNX(n, nbloc, nblcmx, nsbloc, job, lpw, amat, ja,
     *                  ia, iout, ier, izs, nw)
C-----------------------------------------------------------------------
c     
c     This routine determines if the matrix given by the structure
c     IA et JA is irreductible. If not, it orders the unknowns such
c     that all the consecutive unknowns in KPW between NSBLOC(i-1)+1
c     and NSBLOC(i) belong to the ith component of the matrix.
c     The numerical values of the matrix are in AMAT. They are modified
c     only if JOB = 1 and if we have more than one connected component.
c
c     On entry:
c     --------
c     n      = row and column dimension of the matrix
c     nblcmx = maximum number of connected components allowed. The size
c              of NSBLOC is nblcmx + 1 (in fact, it starts at 0).
c     job    = integer indicating the work to be done:
c              job = 1  if the permutation LPW is modified, we
c                       permute not only the structure of the matrix
c                       but also its numerical values.
c              job.ne.1 if the permutation LPW is modified, we permute 
c                       the structure of the matrix ignoring real values.
c     iout   = impression parameter. If 0 < iout < 100, we print
c              comments and error messages on unit IOUT.
c     nw     = length of the work vector IZS.
c
c     Input / output:
c     --------------
c     nbloc  = number of connected components of the matrix. If the
c              matrix is not irreductible, nbloc > 1. We allow
c              nbloc > 1 on entry; in this case we calculate the
c              number of connected components in each previous one.
c     nsbloc = integer array of length NBLOC + 1 containing the pointers
c              to the first node of each component on the old (input)
c              and on the new (output) ordering.
c     lpw    = integer array of length N corresponding to the
c              permutation of the unknowns. We allow LPW to be a vector 
c              different from the identity on input.
c     amat   = real*8 values of the matrix given by the structure IA, JA.
c     ja     = integer array of length NNZERO (= IA(N+1)-IA(1)) corresponding
c              to the column indices of nonzero elements of the matrix, stored
c              rowwise. It is modified only if the matrix has more
c              than one connected component.
c     ia     = integer array of length N+1 corresponding to the
c              pointer to the beginning of each row in JA (compressed
c              sparse row storage). It is modified only if
c              the matrix has more than one connected component.
c
c     On return:
c     ----------
c     ier    = integer. Error message. Normal return ier = 0.
c
c     Work space:
c     ----------
c     izs    = integer vector of length NW
c
C-----------------------------------------------------------------------
C     Laura C. Dutto - email: dutto@cerca.umontreal.ca
c                      July 1992 - Update: March 1994
C-----------------------------------------------------------------------
      integer izs(nw), lpw(n), nsbloc(0:nblcmx), ia(n+1), ja(*)
      real*8 amat(*)
      logical impr
      character*6 chsubr
C-----------------------------------------------------------------------
      ier    = 0
      impr   = iout.gt.0.and.iout.le.99
      ntb    = ia(n+1) - 1
      mxccex = max(nblcmx,20)
c.....The matrix AMAT is a real*8 vector
      ireal  = 2
c
c.....MXPTBL: maximal number of vertices by block
      mxptbl = 0
      do ibloc = 1, nbloc
         mxptbl = max( mxptbl, nsbloc(ibloc) - nsbloc(ibloc-1))
      enddo
c
      long1 = nbloc * mxptbl
      long2 = nbloc * (mxccex+1)
c.....Dynamic allocation of memory
      iend   = 1
      iiend  = iend
      ilpw   = iiend 
      ikpw   = ilpw   + n
      ilccnx = ikpw   + long1
      imark  = ilccnx + long2
      iend   = imark  + n
      if(iend .gt. nw) go to 220
c
      nbloc0 = nbloc
      chsubr = 'BLCCN1'
c.....We determine if the matrix has more than NBLOC0 connected components.
      call BLCCN1(n, nbloc, nblcmx, nsbloc, izs(ilpw), izs(ikpw), ia,
     *            ja, izs(imark), mxccex, izs(ilccnx), mxptbl, iout,
     *            ier)
      if(ier.ne.0) go to 210
c
      if(nbloc .gt. nbloc0) then
c..........The matrix has more than NBLOC0 conneted components. So, we
c..........modify the vectors IA and JA to take account of the new permutation.
           nfree = iend - ikpw
           call tbzero(izs(ikpw), nfree)
           iiat  = ikpw
           ijat  = iiat + n + 1
           iamat = ijat + ntb
           iend  = iamat
           if(job .eq. 1) iend = iamat + ireal * ntb
           if(iend .gt. nw) go to 220
c
c..........We copy IA and JA on IAT and JAT respectively
           call ccnicopy(n+1, ia, izs(iiat))
           call ccnicopy(ntb, ja, izs(ijat))
           if(job .eq. 1) call dcopy(ntb, amat, 1, izs(iamat), 1)
           call dperm(n, izs(iamat), izs(ijat), izs(iiat), amat,
     *                ja, ia, izs(ilpw), izs(ilpw), job)
           ipos = 1
c..........We sort columns inside JA.
           call csrcsc(n, job, ipos, amat, ja, ia, izs(iamat),
     *                 izs(ijat), izs(iiat))
           call csrcsc(n, job, ipos, izs(iamat), izs(ijat), izs(iiat),
     *                 amat, ja, ia)
      endif
c.....We modify the ordering of unknowns in LPW
      call compos(n, lpw, izs(ilpw))
c
 120  nfree = iend - iiend
      call tbzero(izs(iiend), nfree)
      iend = iiend
      return
c
 210  IF(IMPR) WRITE(IOUT,310) chsubr,ier
      go to 120
 220  IF(IMPR) WRITE(IOUT,320) nw, iend
      if(ier.eq.0) ier = -1
      go to 120
c
 310  FORMAT(' ***BLCCNX*** ERROR IN ',a6,'. IER = ',i8)
 320  FORMAT(' ***BLCCNX*** THERE IS NOT ENOUGH MEMORY IN THE WORK',
     1       ' VECTOR.'/13X,' ALLOWED MEMORY = ',I10,'  - NEEDED',
     2       ' MEMORY = ',I10)
      end 
c-----------------------------------------------------------------------
      subroutine blkchk (nrow,ja,ia,nblk,imsg)
c----------------------------------------------------------------------- 
c This routine checks whether the input matrix is a block
c matrix with block size of nblk. A block matrix is one which is
c comprised of small square dense blocks. If there are zero
c elements within the square blocks and the data structure
c takes them into account then blkchk may fail to find the
c correct block size. 
c----------------------------------------------------------------------- 
c on entry
c---------
c nrow	= integer equal to the row dimension of the matrix.  
c ja    = integer array containing the column indices of the entries 
c         nonzero entries of the matrix stored by row.
c ia    = integer array of length nrow + 1 containing the pointers 
c         beginning of each row in array ja.
c
c nblk  = integer containing the value of nblk to be checked. 
c         
c on return
c---------- 
c
c imsg  = integer containing a message  with the following meaning.
c          imsg = 0 means that the output value of nblk is a correct
c                   block size. nblk .lt. 0 means nblk not correct 
c                   block size.
c          imsg = -1 : nblk does not divide nrow 
c          imsg = -2 : a starting element in a row is at wrong position
c             (j .ne. mult*nblk +1 )
c          imsg = -3 : nblk does divide a row length - 
c          imsg = -4 : an element is isolated outside a block or
c             two rows in same group have different lengths
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
      integer ia(nrow+1),ja(*) 
c---------------------------------------------------------------------- 
c first part of code will find candidate block sizes.
c this is not guaranteed to work . so a check is done at the end
c the criterion used here is a simple one:
c scan rows and determine groups of rows that have the same length
c and such that the first column number and the last column number 
c are identical. 
c---------------------------------------------------------------------- 
      imsg = 0
      if (nblk .le. 1) return
      nr = nrow/nblk
      if (nr*nblk .ne. nrow) goto 101
c--   main loop --------------------------------------------------------- 
      irow = 1
      do 20 ii=1, nr
c     i1= starting position for group of nblk rows in original matrix
         i1 = ia(irow)
         j2 = i1
c     lena = length of each row in that group  in the original matrix
         lena = ia(irow+1)-i1
c     len = length of each block-row in that group in the output matrix
         len = lena/nblk
         if (len* nblk .ne. lena) goto 103
c     
c     for each row
c     
         do 6 i = 1, nblk
            irow = irow + 1
            if (ia(irow)-ia(irow-1) .ne. lena ) goto 104
c     
c     for each block
c     
            do 7 k=0, len-1
               jstart = ja(i1+nblk*k)-1
               if ( (jstart/nblk)*nblk .ne. jstart) goto 102
c     
c     for each column
c     
               do 5 j=1, nblk
                  if (jstart+j .ne. ja(j2) )  goto 104
                  j2 = j2+1 
 5             continue
 7          continue
 6       continue
 20   continue
c     went through all loops successfully:
      return
 101  imsg = -1
      return
 102  imsg = -2
      return
 103  imsg = -3
      return
 104  imsg = -4
c----------------end of chkblk ----------------------------------------- 
c-----------------------------------------------------------------------
      return
      end
c----------------------------------------------------------------------- 
      subroutine blkfnd (nrow,ja,ia,nblk)
c-----------------------------------------------------------------------
c This routine attemptps to determine whether or not  the input
c matrix has a block structure and finds the blocks size
c if it does. A block matrix is one which is
c comprised of small square dense blocks. If there are zero
c elements within the square blocks and the original data structure
c takes these zeros into account then blkchk may fail to find the
c correct block size. 
c----------------------------------------------------------------------- 
c on entry
c---------
c nrow	= integer equal to the row dimension of the matrix.  
c ja    = integer array containing the column indices of the entries 
c         nonzero entries of the matrix stored by row.
c ia    = integer array of length nrow + 1 containing the pointers 
c         beginning of each row in array ja.
c		
c nblk  = integer containing the assumed value of nblk if job = 0
c         
c on return
c---------- 
c nblk  = integer containing the value found for nblk when job = 1.
c         if imsg .ne. 0 this value is meaningless however.
c
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
      integer ia(nrow+1),ja(*) 
c----------------------------------------------------------------------- 
c first part of code will find candidate block sizes.
c criterion used here is a simple one: scan rows and  determine groups 
c of rows that have the same length and such that the first column 
c number and the last column number are identical. 
c----------------------------------------------------------------------- 
      minlen = ia(2)-ia(1)
      irow   = 1
      do 1 i=2,nrow
         len = ia(i+1)-ia(i)
         if (len .lt. minlen) then
            minlen = len 
            irow = i
         endif
 1    continue
c     
c     ---- candidates are all dividers of minlen
c
      nblk = 1
      if (minlen .le. 1) return
c     
      do 99 iblk = minlen, 1, -1
         if (mod(minlen,iblk) .ne. 0) goto 99
         len = ia(2) - ia(1)
         len0 = len
         jfirst = ja(1) 
         jlast = ja(ia(2)-1)
         do 10 jrow = irow+1,irow+nblk-1
            i1 = ia(jrow)
            i2 = ia(jrow+1)-1
            len = i2+1-i1
            jf = ja(i1)
            jl = ja(i2) 
            if (len .ne. len0 .or. jf .ne. jfirst .or. 
     *           jl .ne. jlast) goto 99
 10      continue
c     
c     check for this candidate ----
c     
         call blkchk (nrow,ja,ia,iblk,imsg)	       
         if (imsg .eq. 0) then 
c     
c     block size found
c     
            nblk = iblk
            return
         endif 
 99   continue	       
c--------end-of-blkfnd ------------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine bndcsr (n,abd,nabd,lowd,ml,mu,a,ja,ia,len,ierr)
      real*8 a(*),abd(nabd,*), t
      integer ia(n+1),ja(*)
c----------------------------------------------------------------------- 
c Banded (Linpack ) format   to    Compressed Sparse Row  format.
c----------------------------------------------------------------------- 
c on entry:
c----------
c n	= integer,the actual row dimension of the matrix.
c
c nabd  = first dimension of array abd.
c
c abd   = real array containing the values of the matrix stored in
c         banded form. The j-th column of abd contains the elements
c         of the j-th column of  the original matrix,comprised in the
c         band ( i in (j-ml,j+mu) ) with the lowest diagonal located
c         in row lowd (see below). 
c    
c lowd  = integer. this should be set to the row number in abd where
c         the lowest diagonal (leftmost) of A is located. 
c         lowd should be s.t.  ( 1  .le.  lowd  .le. nabd).
c         The subroutines dgbco, ... of linpack use lowd=2*ml+mu+1.
c
c ml	= integer. equal to the bandwidth of the strict lower part of A
c mu	= integer. equal to the bandwidth of the strict upper part of A
c         thus the total bandwidth of A is ml+mu+1.
c         if ml+mu+1 is found to be larger than nabd then an error 
c         message is set. see ierr.
c
c len   = integer. length of arrays a and ja. bndcsr will stop if the
c         length of the arrays a and ja is insufficient to store the 
c         matrix. see ierr.
c
c on return:
c-----------
c a,
c ja,
c ia    = input matrix stored in compressed sparse row format.
c
c lowd  = if on entry lowd was zero then lowd is reset to the default
c         value ml+mu+l. 
c
c ierr  = integer. used for error message output. 
c         ierr .eq. 0 :means normal return
c         ierr .eq. -1 : means invalid value for lowd. 
c	  ierr .gt. 0 : means that there was not enough storage in a and ja
c         for storing the ourput matrix. The process ran out of space 
c         (as indicated by len) while trying to fill row number ierr. 
c         This should give an idea of much more storage might be required. 
c         Moreover, the first irow-1 rows are correctly filled. 
c
c notes:  the values in abd found to be equal to zero
c -----   (actual test: if (abd(...) .eq. 0.0d0) are removed.
c         The resulting may not be identical to a csr matrix
c         originally transformed to a bnd format.
c          
c----------------------------------------------------------------------- 
      ierr = 0
c-----------
      if (lowd .gt. nabd .or. lowd .le. 0) then 
         ierr = -1
         return
      endif
c-----------
      ko = 1
      ia(1) = 1
      do 30 irow=1,n
c-----------------------------------------------------------------------
         i = lowd 
          do  20 j=irow-ml,irow+mu
             if (j .le. 0 ) goto 19
             if (j .gt. n) goto 21
             t = abd(i,j) 
             if (t .eq. 0.0d0) goto 19
             if (ko .gt. len) then 
               ierr = irow 
               return
            endif
            a(ko) = t
            ja(ko) = j
            ko = ko+1
 19         i = i-1
 20      continue
c     end for row irow
 21      ia(irow+1) = ko
 30   continue
      return
c------------- end of bndcsr ------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c----- end of hsourc --------------------------------------------------- 
c-----------------------------------------------------------------------
      subroutine bound (nx,nelx,ijk,nodcode,node,nint,iperm,
     *             x,y,wk,iwk)
c-----------------------------------------------------------------------
c this routine counts the number of boundary points and 
c reorders the points in such a way that the boundary nodes
c are last.
c 
c nx, nelx, ijk, nodcode, node: see other subroutines
c iperm = permutation array from old orderin to new ordering,
c iwk   = reverse permutation array or return.
c wk	= real work array
c On return
c x, y, nodecode, are permuted
c ijk  is updated according to new oerdering.
c nint = number of interior points.
c 
c-----------------------------------------------------------------------
      implicit real*8  (a-h,o-z)
      dimension ijk(node,1),x(1),y(1),wk(1),iwk(1),iperm(1),
     *     nodcode(1)

c     put all boundary points at the end, backwards
      nint = 1
      nbound = nx
      do 1 j=1, nx
         if (nodcode(j) .eq. 0) then
            iperm(nint) = j
            nint = nint+1
         else
            iperm(nbound) = j
            nbound = nbound-1
         endif
 1    continue
c-------------------------------------------------------------------
      nint = nint-1
c     
c permute x's
c   
      do 2 k=1, nx
         wk(k) = x(k)
 2    continue
      do 3 k=1,nx
         x(k) = wk(iperm(k))
 3    continue
c 
c     permute the y's
c
      do 4 k=1, nx
         wk(k) = y(k)
 4    continue
      do 5 k=1, nx
         y(k) = wk(iperm(k))
 5    continue
c 
c     permute the boundary information
c
      do 6 k=1, nx
         iwk(k) = nodcode(k)
 6    continue 
      do 7 k=1,nx
         nodcode(k) = iwk(iperm(k))
 7    continue
c
c     get reverse permutation
c
      do 8 k=1, nx
         iwk(iperm(k)) = k
 8    continue
c
c     update the elements connectivity matrix
c
      do 10 nel = 1, nelx
         do 9 j=1, node
            knod = ijk(j,nel)
            ijk(j,nel) = iwk(knod) 
 9       continue
 10   continue
      return
      end
c-----end-of-tidycg
c-----------------------------------------------------------------------
      logical function brkdn(alpha, ipar)
      implicit none
      integer ipar(16)
      real*8 alpha, beta, zero, one
      parameter (zero=0.0D0, one=1.0D0)
c-----------------------------------------------------------------------
c     test whether alpha is zero or an abnormal number, if yes,
c     this routine will return .true.
c
c     If alpha == 0, ipar(1) = -3,
c     if alpha is an abnormal number, ipar(1) = -9.
c-----------------------------------------------------------------------
      brkdn = .false.
      if (alpha.gt.zero) then
         beta = one / alpha
         if (.not. beta.gt.zero) then
            brkdn = .true.
            ipar(1) = -9
         endif
      else if (alpha.lt.zero) then
         beta = one / alpha
         if (.not. beta.lt.zero) then
            brkdn = .true.
            ipar(1) = -9
         endif
      else if (alpha.eq.zero) then
         brkdn = .true.
         ipar(1) = -3
      else
         brkdn = .true.
         ipar(1) = -9
      endif
      return
      end
c-----------------------------------------------------------------------
      subroutine bsrcsr (job, n, m, na, a, ja, ia, ao, jao, iao)
      implicit none
      integer job, n, m, na, ia(*), ja(*), jao(*), iao(n+1)
      real*8 a(na,*), ao(*)
c-----------------------------------------------------------------------
c             Block Sparse Row  to Compressed Sparse Row.
c----------------------------------------------------------------------- 
c NOTE: ** meanings of parameters may have changed wrt earlier versions
c FORMAT DEFINITION HAS CHANGED WRT TO EARLIER VERSIONS... 
c-----------------------------------------------------------------------
c
c converts a  matrix stored in block-reduced   a, ja, ia  format to the
c general  sparse row a,  ja, ia  format.  A matrix   that has  a block
c structure is a matrix whose entries are blocks  of the same size m
c (e.g.  3 x 3).   Then it is often preferred  to work with the reduced
c graph of the matrix. Instead of storing one element at a time one can
c store a whole block at a time.  In this storage scheme  an entry is a
c square array holding the m**2 elements of a block.
c 
c-----------------------------------------------------------------------
c on entry:
c----------
c job   = if job.eq.0 on entry, values are not copied (pattern only)
c
c n	= the block row dimension of the matrix.
c
c m     = the dimension of each block. Thus, the actual row dimension 
c         of A is n x m.  
c
c na	= first dimension of array a as declared in calling program.
c         This should be .ge. m**2.
c
c a	= real array containing the real entries of the matrix. Recall
c         that each entry is in fact an m x m block. These entries 
c         are stored column-wise in locations a(1:m*m,k) for each k-th
c         entry. See details below.
c 
c ja	= integer array of length n. ja(k) contains the column index 
c         of the leading element, i.e., the element (1,1) of the block
c         that is held in the column a(*,k) of the value array. 
c
c ia    = integer array of length n+1. ia(i) points to the beginning
c         of block row number i in the arrays a and ja. 
c 
c on return:
c-----------
c ao, jao, 
c iao   = matrix stored in compressed sparse row format. The number of
c         rows in the new matrix is n x m. 
c 
c Notes: THIS CODE IS NOT IN PLACE.
c 
c-----------------------------------------------------------------------
c BSR FORMAT.
c---------- 
c Each row of A contains the m x m block matrix unpacked column-
c wise (this allows the user to declare the array a as a(m,m,*) on entry 
c if desired). The block rows are stored in sequence just as for the
c compressed sparse row format. 
c
c-----------------------------------------------------------------------
c     example  with m = 2:
c                                                       1  2 3   
c    +-------|--------|--------+                       +-------+
c    | 1   2 |  0   0 |  3   4 |     Block             | x 0 x | 1
c    | 5   6 |  0   0 |  7   8 |     Representation:   | 0 x x | 2 
c    +-------+--------+--------+                       | x 0 0 | 3 
c    | 0   0 |  9  10 | 11  12 |                       +-------+ 
c    | 0   0 | 13  14 | 15  16 |  
c    +-------+--------+--------+   
c    | 17 18 |  0   0 |  0   0 |
c    | 22 23 |  0   0 |  0   0 |
c    +-------+--------+--------+
c
c    For this matrix:     n    = 3
c                         m    = 2
c                         nnz  = 5 
c-----------------------------------------------------------------------
c Data structure in Block Sparse Row format:      
c-------------------------------------------
c Array A:
c------------------------- 
c     1   3   9   11   17   <<--each m x m block is stored column-wise 
c     5   7   13  15   22       in a  column of the array A.
c     2   4   10  12   18      
c     6   8   14  16   23
c------------------------- 
c JA  1   3   2    3    1   <<-- column indices for each block. Note that
c-------------------------       these indices are wrt block matrix.
c IA  1   3   5    6        <<-- pointers to beginning of each block row 
c-------------------------       in arrays A and JA. 
c-----------------------------------------------------------------------
c locals 
c 
      integer i, i1, i2, ij, ii, irow, j, jstart, k, krow, no
      logical val
c     
      val = (job.ne.0)
      no = n * m 
      irow = 1	
      krow = 1
      iao(irow) = 1      
c-----------------------------------------------------------------------
      do 2 ii=1, n
c
c     recall: n is the block-row dimension
c
         i1 = ia(ii)
         i2 = ia(ii+1)-1
c
c     create m rows for each block row -- i.e., each k. 
c     
         do 23 i=1,m
            do 21 k=i1, i2
               jstart = m*(ja(k)-1)
               do 22  j=1,m
                  ij = (j-1)*m + i
                  if (val) ao(krow) = a(ij,k) 
                  jao(krow) = jstart+j
                  krow = krow+1
 22            continue	    
 21         continue
            irow = irow+1 
            iao(irow) = krow
 23      continue
 2    continue
      return
c-------------end-of-bsrcsr -------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine bsten (nx,ny,nz,kx,ky,kz,nfree,stencil,h)
c-----------------------------------------------------------------------
c     This subroutine calcultes the correct block-stencil values for
c     centered difference discretization of the elliptic operator
c     (block version of stencil)
c
c L u = delx( a delx u ) + dely ( b dely u) + delz ( c delz u ) +
c       d delx ( u ) + e dely (u) + f delz( u ) + g u
c
c   For 2-D problems the discretization formula that is used is:
c
c h**2 * Lu == a(i+1/2,j)*{u(i+1,j) - u(i,j)} +
c	       a(i-1/2,j)*{u(i-1,j) - u(i,j)} +
c              b(i,j+1/2)*{u(i,j+1) - u(i,j)} +
c              b(i,j-1/2)*{u(i,j-1) - u(i,j)} +
c              (h/2)*d(i,j)*{u(i+1,j) - u(i-1,j)} +
c              (h/2)*e(i,j)*{u(i,j+1) - u(i,j-1)} +
c              (h/2)*e(i,j)*{u(i,j+1) - u(i,j-1)} +
c              (h**2)*g(i,j)*u(i,j)
c-----------------------------------------------------------------------
c     some constants
c
      real*8  zero,half
      parameter(zero=0.0D0,half=0.5D0)
c
c     local variables
c
      integer i,k,kx,ky,kz,nfree,nfree2,nx,ny,nz
      real*8 stencil(7,*), cntr(225), coeff(225),h,h2,hhalf,x,y,z
c------------
      if (nfree .gt. 15) then
         print *, ' ERROR ** nfree too large '
         stop
      endif
c
      nfree2 = nfree*nfree
      do 200 k=1, nfree2
         cntr(k) = zero
         do 199 i=1,7
            stencil(i,k) = zero
 199     continue
 200  continue
c------------
      hhalf = h*half
      h2 = h*h
      x = h*dble(kx)
      y = h*dble(ky)
      z = h*dble(kz)
c differentiation wrt x:
      call afunbl(nfree,x+hhalf,y,z,coeff)
      do 1 k=1, nfree2
      stencil(3,k) = stencil(3,k) + coeff(k)
      cntr(k) = cntr(k) + coeff(k)
 1    continue
c
      call afunbl(nfree,x-hhalf,y,z,coeff)
      do 2 k=1, nfree2
         stencil(2,k) = stencil(2,k) + coeff(k)
         cntr(k) = cntr(k) + coeff(k)
 2    continue
c
      call dfunbl(nfree,x,y,z,coeff)
      do 3 k=1, nfree2
         stencil(3,k) = stencil(3,k) + coeff(k)*hhalf
         stencil(2,k) = stencil(2,k) - coeff(k)*hhalf
 3    continue
      if (ny .le. 1) goto 99
c
c differentiation wrt y:
c
      call bfunbl(nfree,x,y+hhalf,z,coeff)
      do 4 k=1,nfree2
         stencil(5,k) = stencil(5,k) + coeff(k)
         cntr(k) = cntr(k) + coeff(k)
 4    continue
c
      call bfunbl(nfree,x,y-hhalf,z,coeff)
      do 5 k=1, nfree2
         stencil(4,k) = stencil(4,k) + coeff(k)
         cntr(k) = cntr(k) + coeff(k)
 5    continue
c
      call efunbl(nfree,x,y,z,coeff)
      do 6 k=1, nfree2
         stencil(5,k) = stencil(5,k) + coeff(k)*hhalf
         stencil(4,k) = stencil(4,k) - coeff(k)*hhalf
 6    continue
      if (nz .le. 1) goto 99
c
c differentiation wrt z:
c
      call cfunbl(nfree,x,y,z+hhalf,coeff)
      do 7 k=1, nfree2
         stencil(7,k) = stencil(7,k) + coeff(k)
         cntr(k) = cntr(k) + coeff(k)
 7    continue
c
      call cfunbl(nfree,x,y,z-hhalf,coeff)
      do 8 k=1, nfree2
         stencil(6,k) = stencil(6,k) + coeff(k)
         cntr(k) = cntr(k) + coeff(k)
 8    continue
c
      call ffunbl(nfree,x,y,z,coeff)
      do 9 k=1, nfree2
         stencil(7,k) = stencil(7,k) + coeff(k)*hhalf
         stencil(6,k) = stencil(6,k) - coeff(k)*hhalf
 9    continue
c
c discretization of  product by g:
c
 99   call gfunbl(nfree,x,y,z,coeff)
      do 10 k=1, nfree2
         stencil(1,k) = h2*coeff(k) - cntr(k)
 10   continue
c
      return
c------------end of bsten-----------------------------------------------
c-----------------------------------------------------------------------
      end
C***********************************************************************
      SUBROUTINE CCNICOPY(N,IX,IY)
C.......................................................................
C     We copy the vector IX on the vector IY
C.......................................................................
      DIMENSION IX(n),IY(n)
C.......................................................................
      IF(N.LE.0) RETURN
C$DOACROSS if(n .gt. 250), local(i)
      DO 10 I = 1,N
         IY(I) = IX(I)
   10 CONTINUE
C
      RETURN
      END
c **********************************************************************
      subroutine CCONEX(n, icol0, mxccnx, lccnex, kpw, ia, ja, mark,
     *                  iout, ier)
C-----------------------------------------------------------------------
c
c     This routine determines if the matrix given by the structure
c     IA and JA is irreductible. If not, it orders the unknowns such
c     that all the consecutive unknowns in KPW between LCCNEX(i-1)+1
c     and LCCNEX(i) belong to the ith component of the matrix.
c     The structure of the matrix could be nonsymmetric.
c     The diagonal vertices (if any) will belong to the last connected
c     component (convention).
c
c     On entry:
c     --------
c     n      = row and column dimension of the matrix
c     icol0  = the columns of the matrix are between ICOL0+1 and ICOL0+N
c     iout   = impression parameter. If 0 < IOUT < 100, we print
c              comments and error messages on unit IOUT.
c     ia     = integer array of length N+1 corresponding to the
c              pointer to the beginning of each row in JA (compressed
c              sparse row storage).
c     ja     = integer array of length NNZERO (= IA(N+1)-IA(1))
c              corresponding to the column indices of nonzero elements
c              of the matrix, stored rowwise.
c
c     Input/Output:
c     ------------
c     mxccnx = maximum number of connected components allowed on input,
c              and number of connected components of the matrix, on output.
c
c     On return:
c     ----------
c     lccnex = integer array of length MXCCNX + 1 containing the pointers
c              to the first node of each component, in the vector KPW.
c     kpw    = integer array of length N corresponding to the
c              inverse of permutation vector.
c     ier    = integer. Error message. Normal return ier = 0.
c
c     Work space:
c     ----------
c     mark   = integer vector of length N
c
C-----------------------------------------------------------------------
C     Laura C. Dutto - email: dutto@cerca.umontreal.ca
c                      July 1992 - Update: March 1994
C-----------------------------------------------------------------------
      dimension ia(n+1), lccnex(0:mxccnx), kpw(n), ja(*), mark(n)
      logical impr
C-----------------------------------------------------------------------
      ier    = 0
      ipos = ia(1) - 1 
      impr   = iout.gt.0.and.iout.le.99
c
      nccnex = 0
c.....We initialize MARK to zero. At the end of the algorithm, it would
c.....indicate the number of connected component associated with the vertex.
c.....The number (-1) indicates that the row associated with this vertex
c.....is a diagonal row. This value could be modified because we accept
c.....a non symmetric matrix. All the diagonal vertices will be put in
c.....the same connected component.
      call tbzero(mark, n)
c
 5    do i = 1,n
         if(mark(i) .eq. 0) then
              ideb = i
              go to 15
         endif
      enddo
      go to 35
c
 15   if( ia(ideb+1) - ia(ideb) .eq. 1) then
c..........The row is a diagonal row.
           mark(ideb) = -1
           go to 5
      endif
      iccnex = nccnex + 1
      if(iccnex .gt. mxccnx) go to 220
      index  = 0
      newind = 0
      jref   = 0
      mark(ideb) = iccnex
      index  = index + 1
      kpw(index) = ideb
c
 20   jref = jref + 1
      ideb = kpw(jref)

      do 30 ir = ia(ideb)-ipos, ia(ideb+1)-ipos-1
         j = ja(ir) - icol0
         mccnex = mark(j)
         if(mccnex .le. 0) then
              index = index + 1
              kpw(index) = j
              mark(j) = iccnex
         else if( mccnex .eq. iccnex) then
              go to 30
         else if( mccnex .gt. iccnex) then
c.............We realize that the connected component MCCNX is,
c.............in fact, included in this one. We modify MARK and KPW.
              call NEWCNX(n, mccnex, iccnex, index, kpw, mark)
              if(mccnex .eq. nccnex) nccnex = nccnex - 1
         else
c.............We realize that the previously marked vertices belong,
c.............in fact, to the connected component ICCNX. We modify MARK.
              call ANCCNX(n, iccnex, mccnex, mark, nwindx)
              iccnex = mccnex
              newind = newind + nwindx
         endif
 30   continue
      if(jref .lt. index) go to 20
c
c.....We have finished with this connected component.
      index = index + newind
      if(iccnex .eq. nccnex+1) nccnex = nccnex + 1
      go to 5
c.......................................................................
c
c     We have partitioned the graph in its connected components!
c
c.......................................................................
 35   continue
c
c.....All the vertices have been already marked. Before modifying KPW
c.....(if necessary), we put the diagonal vertex (if any) in the last
c.....connected component.
      call tbzero(lccnex(1), nccnex)
c
      idiag = 0
      do i = 1, n
         iccnex = mark(i)
         if(iccnex .eq. -1) then
              idiag = idiag + 1
              if(idiag .eq. 1) then
                   nccnex = nccnex + 1
                   if(nccnex .gt. mxccnx) go to 220
                   if(impr) write(iout,340)
              endif
              mark(i) = nccnex
         else
              lccnex(iccnex) = lccnex(iccnex) + 1
         endif
      enddo
      if(idiag .ge. 1) lccnex(nccnex) = idiag
c
      if(nccnex .eq. 1) then
         lccnex(nccnex) = n
         go to 40
      endif
c
      iccnex = 1
 8    if(iccnex .gt. nccnex) go to 12
      if(lccnex(iccnex) .le. 0) then
           do i = 1, n
              if(mark(i) .ge. iccnex) mark(i) = mark(i) - 1
           enddo
           nccnex = nccnex - 1
           do mccnex = iccnex, nccnex
              lccnex(mccnex) = lccnex(mccnex + 1)
           enddo
      else
           iccnex = iccnex + 1
      endif
      go to 8
c
 12   index = 0
      do iccnex = 1, nccnex
         noeicc = lccnex(iccnex)
         lccnex(iccnex) = index
         index  = index + noeicc
      enddo
      if(index .ne. n) go to 210
c
c.....We define correctly KPW
      do i = 1,n
         iccnex = mark(i)
         index = lccnex(iccnex) + 1
         kpw(index) = i
         lccnex(iccnex) = index
      enddo
c
 40   mxccnx = nccnex
      lccnex(0) = nccnex
      if(nccnex .eq. 1) call numini(n, kpw)
      return
c
 210  if(impr) write(iout,310) index,n
      go to 235
 220  if(impr) write(iout,320) nccnex, mxccnx
      go to 235
 235  ier = -1
      return
c
 310  format(' ***CCONEX*** ERROR TRYING TO DETERMINE THE NUMBER',
     *       ' OF CONNECTED COMPONENTS.'/13X,' NUMBER OF MARKED',
     *       ' VERTICES =',i7,3x,'TOTAL NUMBER OF VERTICES =',I7)
 320  format(' ***CCONEX*** THE ALLOWED NUMBER OF CONNECTED COMPONENTS',
     *       ' IS NOT ENOUGH.'/13X,' NECESSARY NUMBER = ',I4,
     *       5x,' ALLOWED NUMBER = ',I4)
 323  format(' ***CCONEX*** ERROR IN ',A6,'. IER = ',I8)
 340  format(/' ***CCONEX*** THE LAST CONNECTED COMPONENT WILL',
     *       ' HAVE THE DIAGONAL VERTICES.')
      end
c
      subroutine  ccopy(n,cx,incx,cy,incy)
c
c     copies a vector, x, to a vector, y.
c     jack dongarra, linpack, 3/11/78.
c
      complex cx(1),cy(1)
      integer i,incx,incy,ix,iy,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        cy(iy) = cx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
   20 do 30 i = 1,n
        cy(i) = cx(i)
   30 continue
      return
      end
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c         Basic Iterative Solvers with Reverse Communication           c
c----------------------------------------------------------------------c
c     This file currently has several basic iterative linear system    c
c     solvers. They are:                                               c
c     CG       -- Conjugate Gradient Method                            c
c     CGNR     -- Conjugate Gradient Method (Normal Residual equation) c
c     BCG      -- Bi-Conjugate Gradient Method                         c
c     DBCG     -- BCG with partial pivoting                            c
c     BCGSTAB  -- BCG stabilized                                       c
c     TFQMR    -- Transpose-Free Quasi-Minimum Residual method         c
c     FOM      -- Full Orthogonalization Method                        c
c     GMRES    -- Generalized Minimum RESidual method                  c
c     FGMRES   -- Flexible version of Generalized Minimum              c
c                 RESidual method                                      c
c     DQGMRES  -- Direct versions of Quasi Generalize Minimum          c
c                 Residual method                                      c
c----------------------------------------------------------------------c
c     They all have the following calling sequence:
c      subroutine solver(n, rhs, sol, ipar, fpar, w)
c      integer n, ipar(16)
c      real*8 rhs(n), sol(n), fpar(16), w(*)
c     Where
c     (1) 'n' is the size of the linear system,
c     (2) 'rhs' is the right-hand side of the linear system,
c     (3) 'sol' is the solution to the linear system,
c     (4) 'ipar' is an integer parameter array for the reverse
c     communication protocol,
c     (5) 'fpar' is an floating-point parameter array storing
c     information to and from the iterative solvers.
c     (6) 'w' is the work space (size is specified in ipar)
c
c     They are preconditioned iterative solvers with reverse
c     communication. The preconditioners can be applied from either
c     from left or right or both (specified by ipar(2), see below).
c
c     Author: Kesheng John Wu (kewu@mail.cs.umn.edu) 1993
c
c     NOTES:
c
c     (1) Work space required by each of the iterative solver
c     routines is as follows:
c       CG      == 5 * n
c       CGNR    == 5 * n
c       BCG     == 7 * n
c       DBCG    == 11 * n
c       BCGSTAB == 8 * n
c       TFQMR   == 11 * n
c       FOM     == (n+3)*(m+2) + (m+1)*m/2 (m = ipar(5), default m=15)
c       GMRES   == (n+3)*(m+2) + (m+1)*m/2 (m = ipar(5), default m=15)
c       FGMRES  == 2*n*(m+1) + (m+1)*m/2 + 3*m + 2 (m = ipar(5),
c                  default m=15)
c       DQGMRES == n + lb * (2*n+4) (lb=ipar(5)+1, default lb = 16)
c
c     (2) ALL iterative solvers require a user-supplied DOT-product
c     routine named DISTDOT. The prototype of DISTDOT is
c
c     real*8 function distdot(n,x,ix,y,iy)
c     integer n, ix, iy
c     real*8 x(1+(n-1)*ix), y(1+(n-1)*iy)
c
c     This interface of DISTDOT is exactly the same as that of
c     DDOT (or SDOT if real == real*8) from BLAS-1. It should have
c     same functionality as DDOT on a single processor machine. On a
c     parallel/distributed environment, each processor can perform
c     DDOT on the data it has, then perform a summation on all the
c     partial results.
c
c     (3) To use this set of routines under SPMD/MIMD program paradigm,
c     several things are to be noted: (a) 'n' should be the number of
c     vector elements of 'rhs' that is present on the local processor.
c     (b) if RHS(i) is on processor j, it is expected that SOL(i)
c     will be on the same processor, i.e. the vectors are distributed
c     to each processor in the same way. (c) the preconditioning and
c     stopping criteria specifications have to be the same on all
c     processor involved, ipar and fpar have to be the same on each
c     processor. (d) DISTDOT should be replaced by a distributed
c     dot-product function.
c
c     ..................................................................
c     Reverse Communication Protocols
c
c     When a reverse-communication routine returns, it could be either
c     that the routine has terminated or it simply requires the caller
c     to perform one matrix-vector multiplication. The possible matrices
c     that involve in the matrix-vector multiplications are:
c     A       (the matrix of the linear system),
c     A^T     (A transposed),
c     Ml^{-1} (inverse of the left preconditioner),
c     Ml^{-T} (inverse of the left preconditioner transposed),
c     Mr^{-1} (inverse of the right preconditioner),
c     Mr^{-T} (inverse of the right preconditioner transposed).
c     For all the matrix vector multiplication, v = A u. The input and
c     output vectors are supposed to be part of the work space 'w', and
c     the starting positions of them are stored in ipar(8:9), see below.
c
c     The array 'ipar' is used to store the information about the solver.
c     Here is the list of what each element represents:
c
c     ipar(1) -- status of the call/return.
c     A call to the solver with ipar(1) == 0 will initialize the
c     iterative solver. On return from the iterative solver, ipar(1)
c     carries the status flag which indicates the condition of the
c     return. The status information is divided into two categories,
c     (1) a positive value indicates the solver requires a matrix-vector
c     multiplication,
c     (2) a non-positive value indicates termination of the solver.
c     Here is the current definition:
c       1 == request a matvec with A,
c       2 == request a matvec with A^T,
c       3 == request a left preconditioner solve (Ml^{-1}),
c       4 == request a left preconditioner transposed solve (Ml^{-T}),
c       5 == request a right preconditioner solve (Mr^{-1}),
c       6 == request a right preconditioner transposed solve (Mr^{-T}),
c      10 == request the caller to perform stopping test,
c       0 == normal termination of the solver, satisfied the stopping
c            criteria,
c      -1 == termination because iteration number is greater than the
c            preset limit,
c      -2 == return due to insufficient work space,
c      -3 == return due to anticipated break-down / divide by zero,
c            in the case where Arnoldi procedure is used, additional
c            error code can be found in ipar(12), where ipar(12) is
c            the error code of orthogonalization procedure MGSRO:
c               -1: zero input vector
c               -2: input vector contains abnormal numbers
c               -3: input vector is a linear combination of others
c               -4: trianguler system in GMRES/FOM/etc. has nul rank
c      -4 == the values of fpar(1) and fpar(2) are both <= 0, the valid
c            ranges are 0 <= fpar(1) < 1, 0 <= fpar(2), and they can
c            not be zero at the same time
c      -9 == while trying to detect a break-down, an abnormal number is
c            detected.
c     -10 == return due to some non-numerical reasons, e.g. invalid
c            floating-point numbers etc.
c
c     ipar(2) -- status of the preconditioning:
c       0 == no preconditioning
c       1 == left preconditioning only
c       2 == right preconditioning only
c       3 == both left and right preconditioning
c
c     ipar(3) -- stopping criteria (details of this will be
c     discussed later).
c
c     ipar(4) -- number of elements in the array 'w'. if this is less
c     than the desired size, it will be over-written with the minimum
c     requirement. In which case the status flag ipar(1) = -2.
c
c     ipar(5) -- size of the Krylov subspace (used by GMRES and its
c     variants), e.g. GMRES(ipar(5)), FGMRES(ipar(5)),
c     DQGMRES(ipar(5)).
c
c     ipar(6) -- maximum number of matrix-vector multiplies, if not a
c     positive number the iterative solver will run till convergence
c     test is satisfied.
c
c     ipar(7) -- current number of matrix-vector multiplies. It is
c     incremented after each matrix-vector multiplication. If there
c     is preconditioning, the counter is incremented after the
c     preconditioning associated with each matrix-vector multiplication.
c
c     ipar(8) -- pointer to the input vector to the requested matrix-
c     vector multiplication.
c
c     ipar(9) -- pointer to the output vector of the requested matrix-
c     vector multiplication.
c
c     To perform v = A * u, it is assumed that u is w(ipar(8):ipar(8)+n-1)
c     and v is stored as w(ipar(9):ipar(9)+n-1).
c
c     ipar(10) -- the return address (used to determine where to go to
c     inside the iterative solvers after the caller has performed the
c     requested services).
c
c     ipar(11) -- the result of the external convergence test
c     On final return from the iterative solvers, this value
c     will be reflected by ipar(1) = 0 (details discussed later)
c
c     ipar(12) -- error code of MGSRO, it is
c                  1 if the input vector to MGSRO is linear combination
c                    of others,
c                  0 if MGSRO was successful,
c                 -1 if the input vector to MGSRO is zero,
c                 -2 if the input vector contains invalid number.
c
c     ipar(13) -- number of initializations. During each initilization
c                 residual norm is computed directly from M_l(b - A x).
c
c     ipar(14) to ipar(16) are NOT defined, they are NOT USED by
c     any iterative solver at this time.
c
c     Information about the error and tolerance are stored in the array
c     FPAR. So are some internal variables that need to be saved from
c     one iteration to the next one. Since the internal variables are
c     not the same for each routine, we only define the common ones.
c
c     The first two are input parameters:
c     fpar(1) -- the relative tolerance,
c     fpar(2) -- the absolute tolerance (details discussed later),
c
c     When the iterative solver terminates,
c     fpar(3) -- initial residual/error norm,
c     fpar(4) -- target residual/error norm,
c     fpar(5) -- current residual norm (if available),
c     fpar(6) -- current residual/error norm,
c     fpar(7) -- convergence rate,
c
c     fpar(8:10) are used by some of the iterative solvers to save some
c     internal information.
c
c     fpar(11) -- number of floating-point operations. The iterative
c     solvers will add the number of FLOPS they used to this variable,
c     but they do NOT initialize it, nor add the number of FLOPS due to
c     matrix-vector multiplications (since matvec is outside of the
c     iterative solvers). To insure the correct FLOPS count, the
c     caller should set fpar(11) = 0 before invoking the iterative
c     solvers and account for the number of FLOPS from matrix-vector
c     multiplications and preconditioners.
c
c     fpar(12:16) are not used in current implementation.
c
c     Whether the content of fpar(3), fpar(4) and fpar(6) are residual
c     norms or error norms depends on ipar(3). If the requested
c     convergence test is based on the residual norm, they will be
c     residual norms. If the caller want to test convergence based the
c     error norms (estimated by the norm of the modifications applied
c     to the approximate solution), they will be error norms.
c     Convergence rate is defined by (Fortran 77 statement)
c     fpar(7) = log10(fpar(3) / fpar(6)) / (ipar(7)-ipar(13))
c     If fpar(7) = 0.5, it means that approximately every 2 (= 1/0.5)
c     steps the residual/error norm decrease by a factor of 10.
c
c     ..................................................................
c     Stopping criteria,
c
c     An iterative solver may be terminated due to (1) satisfying
c     convergence test; (2) exceeding iteration limit; (3) insufficient
c     work space; (4) break-down. Checking of the work space is
c     only done in the initialization stage, i.e. when it is called with
c     ipar(1) == 0. A complete convergence test is done after each
c     update of the solutions. Other conditions are monitored
c     continuously.
c
c     With regard to the number of iteration, when ipar(6) is positive,
c     the current iteration number will be checked against it. If
c     current iteration number is greater the ipar(6) than the solver
c     will return with status -1. If ipar(6) is not positive, the
c     iteration will continue until convergence test is satisfied.
c
c     Two things may be used in the convergence tests, one is the
c     residual 2-norm, the other one is 2-norm of the change in the
c     approximate solution. The residual and the change in approximate
c     solution are from the preconditioned system (if preconditioning
c     is applied). The DQGMRES and TFQMR use two estimates for the
c     residual norms. The estimates are not accurate, but they are
c     acceptable in most of the cases. Generally speaking, the error
c     of the TFQMR's estimate is less accurate.
c
c     The convergence test type is indicated by ipar(3). There are four
c     type convergence tests: (1) tests based on the residual norm;
c     (2) tests based on change in approximate solution; (3) caller
c     does not care, the solver choose one from above two on its own;
c     (4) caller will perform the test, the solver should simply continue.
c     Here is the complete definition:
c      -2 == || dx(i) || <= rtol * || rhs || + atol
c      -1 == || dx(i) || <= rtol * || dx(1) || + atol
c       0 == solver will choose test 1 (next)
c       1 == || residual || <= rtol * || initial residual || + atol
c       2 == || residual || <= rtol * || rhs || + atol
c     999 == caller will perform the test
c     where dx(i) denote the change in the solution at the ith update.
c     ||.|| denotes 2-norm. rtol = fpar(1) and atol = fpar(2).
c
c     If the caller is to perform the convergence test, the outcome
c     should be stored in ipar(11).
c     ipar(11) = 0 -- failed the convergence test, iterative solver
c     should continue
c     ipar(11) = 1 -- satisfied convergence test, iterative solver
c     should perform the clean up job and stop.
c
c     Upon return with ipar(1) = 10,
c     ipar(8)  points to the starting position of the change in
c              solution Sx, where the actual solution of the step is
c              x_j = x_0 + M_r^{-1} Sx.
c              Exception: ipar(8) < 0, Sx = 0. It is mostly used by
c              GMRES and variants to indicate (1) Sx was not necessary,
c              (2) intermediate result of Sx is not computed.
c     ipar(9)  points to the starting position of a work vector that
c              can be used by the caller.
c
c     NOTE: the caller should allow the iterative solver to perform
c     clean up job after the external convergence test is satisfied,
c     since some of the iterative solvers do not directly
c     update the 'sol' array. A typical clean-up stage includes
c     performing the final update of the approximate solution and
c     computing the convergence information (e.g. values of fpar(3:7)).
c
c     NOTE: fpar(4) and fpar(6) are not set by the accelerators (the
c     routines implemented here) if ipar(3) = 999.
c
c     ..................................................................
c     Usage:
c
c     To start solving a linear system, the user needs to specify
c     first 6 elements of the ipar, and first 2 elements of fpar.
c     The user may optionally set fpar(11) = 0 if one wants to count
c     the number of floating-point operations. (Note: the iterative
c     solvers will only add the floating-point operations inside
c     themselves, the caller will have to add the FLOPS from the
c     matrix-vector multiplication routines and the preconditioning
c     routines in order to account for all the arithmetic operations.)
c
c     Here is an example:
c     ipar(1) = 0	! always 0 to start an iterative solver
c     ipar(2) = 2	! right preconditioning
c     ipar(3) = 1	! use convergence test scheme 1
c     ipar(4) = 10000	! the 'w' has 10,000 elements
c     ipar(5) = 10	! use *GMRES(10) (e.g. FGMRES(10))
c     ipar(6) = 100	! use at most 100 matvec's
c     fpar(1) = 1.0E-6	! relative tolerance 1.0E-6
c     fpar(2) = 1.0E-10 ! absolute tolerance 1.0E-10
c     fpar(11) = 0.0	! clearing the FLOPS counter
c
c     After the above specifications, one can start to call an iterative
c     solver, say BCG. Here is a piece of pseudo-code showing how it can
c     be done,
c
c 10   call bcg(n,rhs,sol,ipar,fpar,w)
c      if (ipar(1).eq.1) then
c         call amux(n,w(ipar(8)),w(ipar(9)),a,ja,ia)
c         goto 10
c      else if (ipar(1).eq.2) then
c         call atmux(n,w(ipar(8)),w(ipar(9)),a,ja,ia)
c         goto 10
c      else if (ipar(1).eq.3) then
c         left preconditioner solver
c         goto 10
c      else if (ipar(1).eq.4) then
c         left preconditioner transposed solve
c         goto 10
c      else if (ipar(1).eq.5) then
c         right preconditioner solve
c         goto 10
c      else if (ipar(1).eq.6) then
c         right preconditioner transposed solve
c         goto 10
c      else if (ipar(1).eq.10) then
c         call my own stopping test routine
c         goto 10
c      else if (ipar(1).gt.0) then
c         ipar(1) is an unspecified code
c      else
c         the iterative solver terminated with code = ipar(1)
c      endif
c
c     This segment of pseudo-code assumes the matrix is in CSR format,
c     AMUX and ATMUX are two routines from the SPARSKIT MATVEC module.
c     They perform matrix-vector multiplications for CSR matrices,
c     where w(ipar(8)) is the first element of the input vectors to the
c     two routines, and w(ipar(9)) is the first element of the output
c     vectors from them. For simplicity, we did not show the name of
c     the routine that performs the preconditioning operations or the
c     convergence tests.
c-----------------------------------------------------------------------
      subroutine cg(n, rhs, sol, ipar, fpar, w)
      implicit none
      integer n, ipar(16)
      real*8 rhs(n), sol(n), fpar(16), w(n,*)
c-----------------------------------------------------------------------
c     This is a implementation of the Conjugate Gradient (CG) method
c     for solving linear system.
c
c     NOTE: This is not the PCG algorithm. It is a regular CG algorithm.
c     To be consistent with the other solvers, the preconditioners are
c     applied by performing Ml^{-1} A Mr^{-1} P in place of A P in the
c     CG algorithm.  PCG uses the preconditioner differently.
c
c     fpar(7) is used here internally to store <r, r>.
c     w(:,1) -- residual vector
c     w(:,2) -- P, the conjugate direction
c     w(:,3) -- A P, matrix multiply the conjugate direction
c     w(:,4) -- temporary storage for results of preconditioning
c     w(:,5) -- change in the solution (sol) is stored here until
c               termination of this solver
c-----------------------------------------------------------------------
c     external functions used
c
      real*8 distdot
      logical stopbis, brkdn
      external distdot, stopbis, brkdn, bisinit
c
c     local variables
c
      integer i
      real*8 alpha
      logical lp,rp
      save
c
c     check the status of the call
c
      if (ipar(1).le.0) ipar(10) = 0
      goto (10, 20, 40, 50, 60, 70, 80), ipar(10)
c
c     initialization
c
      call bisinit(ipar,fpar,5*n,1,lp,rp,w)
      if (ipar(1).lt.0) return
c
c     request for matrix vector multiplication A*x in the initialization
c
      ipar(1) = 1
      ipar(8) = n+1
      ipar(9) = ipar(8) + n
      ipar(10) = 1
      do i = 1, n
         w(i,2) = sol(i)
      enddo
      return
 10   ipar(7) = ipar(7) + 1
      ipar(13) = 1
      do i = 1, n
         w(i,2) = rhs(i) - w(i,3)
      enddo
      fpar(11) = fpar(11) + n
c
c     if left preconditioned
c
      if (lp) then
         ipar(1) = 3
         ipar(9) = 1
         ipar(10) = 2
         return
      endif
c
 20   if (lp) then
         do i = 1, n
            w(i,2) = w(i,1)
         enddo
      else
         do i = 1, n
            w(i,1) = w(i,2)
         enddo
      endif
c
      fpar(7) = distdot(n,w,1,w,1)
      fpar(11) = fpar(11) + 2 * n
      fpar(3) = sqrt(fpar(7))
      fpar(5) = fpar(3)
      if (abs(ipar(3)).eq.2) then
         fpar(4) = fpar(1) * sqrt(distdot(n,rhs,1,rhs,1)) + fpar(2)
         fpar(11) = fpar(11) + 2 * n
      else if (ipar(3).ne.999) then
         fpar(4) = fpar(1) * fpar(3) + fpar(2)
      endif
c
c     before iteration can continue, we need to compute A * p, which
c     includes the preconditioning operations
c
 30   if (rp) then
         ipar(1) = 5
         ipar(8) = n + 1
         if (lp) then
            ipar(9) = ipar(8) + n
         else
            ipar(9) = 3*n + 1
         endif
         ipar(10) = 3
         return
      endif
c
 40   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = n + 1
      endif
      if (lp) then
         ipar(9) = 3*n+1
      else
         ipar(9) = n+n+1
      endif
      ipar(10) = 4
      return
c
 50   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = n+n+1
         ipar(10) = 5
         return
      endif
c
c     continuing with the iterations
c
 60   ipar(7) = ipar(7) + 1
      alpha = distdot(n,w(1,2),1,w(1,3),1)
      fpar(11) = fpar(11) + 2*n
      if (brkdn(alpha,ipar)) goto 900
      alpha = fpar(7) / alpha
      do i = 1, n
         w(i,5) = w(i,5) + alpha * w(i,2)
         w(i,1) = w(i,1) - alpha * w(i,3)
      enddo
      fpar(11) = fpar(11) + 4*n
c
c     are we ready to terminate ?
c
      if (ipar(3).eq.999) then
         ipar(1) = 10
         ipar(8) = 4*n + 1
         ipar(9) = 3*n + 1
         ipar(10) = 6
         return
      endif
 70   if (ipar(3).eq.999) then
         if (ipar(11).eq.1) goto 900
      else if (stopbis(n,ipar,1,fpar,w,w(1,2),alpha)) then
         goto 900
      endif
c
c     continue the iterations
c
      alpha = fpar(5)*fpar(5) / fpar(7)
      fpar(7) = fpar(5)*fpar(5)
      do i = 1, n
         w(i,2) = w(i,1) + alpha * w(i,2)
      enddo
      fpar(11) = fpar(11) + 2*n
      goto 30
c
c     clean up -- necessary to accommodate the right-preconditioning
c
 900  if (rp) then
         if (ipar(1).lt.0) ipar(12) = ipar(1)
         ipar(1) = 5
         ipar(8) = 4*n + 1
         ipar(9) = ipar(8) - n
         ipar(10) = 7
         return
      endif
 80   if (rp) then
         call tidycg(n,ipar,fpar,sol,w(1,4))
      else
         call tidycg(n,ipar,fpar,sol,w(1,5))
      endif
c
      return
      end
c-----end-of-cg
c-----------------------------------------------------------------------
      subroutine cgnr(n,rhs,sol,ipar,fpar,wk)
      implicit none
      integer n, ipar(16)
      real*8 rhs(n),sol(n),fpar(16),wk(n,*)
c-----------------------------------------------------------------------
c     CGNR -- Using CG algorithm solving A x = b by solving
c     Normal Residual equation: A^T A x = A^T b
c     As long as the matrix is not singular, A^T A is symmetric
c     positive definite, therefore CG (CGNR) will converge.
c
c     Usage of the work space:
c     wk(:,1) == residual vector R
c     wk(:,2) == the conjugate direction vector P
c     wk(:,3) == a scratch vector holds A P, or A^T R
c     wk(:,4) == a scratch vector holds intermediate results of the
c                preconditioning
c     wk(:,5) == a place to hold the modification to SOL
c
c     size of the work space WK is required = 5*n
c-----------------------------------------------------------------------
c     external functions used
c
      real*8 distdot
      logical stopbis, brkdn
      external distdot, stopbis, brkdn, bisinit
c
c     local variables
c
      integer i
      real*8 alpha, zz, zzm1
      logical lp, rp
      save
c
c     check the status of the call
c
      if (ipar(1).le.0) ipar(10) = 0
      goto (10, 20, 40, 50, 60, 70, 80, 90, 100, 110), ipar(10)
c
c     initialization
c
      call bisinit(ipar,fpar,5*n,1,lp,rp,wk)
      if (ipar(1).lt.0) return
c
c     request for matrix vector multiplication A*x in the initialization
c
      ipar(1) = 1
      ipar(8) = 1
      ipar(9) = 1 + n
      ipar(10) = 1
      do i = 1, n
         wk(i,1) = sol(i)
      enddo
      return
 10   ipar(7) = ipar(7) + 1
      ipar(13) = ipar(13) + 1
      do i = 1, n
         wk(i,1) = rhs(i) - wk(i,2)
      enddo
      fpar(11) = fpar(11) + n
c
c     if left preconditioned, precondition the initial residual
c
      if (lp) then
         ipar(1) = 3
         ipar(10) = 2
         return
      endif
c
 20   if (lp) then
         do i = 1, n
            wk(i,1) = wk(i,2)
         enddo
      endif
c
      zz = distdot(n,wk,1,wk,1)
      fpar(11) = fpar(11) + 2 * n
      fpar(3) = sqrt(zz)
      fpar(5) = fpar(3)
      if (abs(ipar(3)).eq.2) then
         fpar(4) = fpar(1) * sqrt(distdot(n,rhs,1,rhs,1)) + fpar(2)
         fpar(11) = fpar(11) + 2 * n
      else if (ipar(3).ne.999) then
         fpar(4) = fpar(1) * fpar(3) + fpar(2)
      endif
c
c     normal iteration begins here, first half of the iteration
c     computes the conjugate direction
c
 30   continue
c
c     request the caller to perform a A^T r --> wk(:,3)
c
      if (lp) then
         ipar(1) = 4
         ipar(8) = 1
         if (rp) then
            ipar(9) = n + n + 1
         else
            ipar(9) = 3*n + 1
         endif
         ipar(10) = 3
         return
      endif
c
 40   ipar(1) = 2
      if (lp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = 1
      endif
      if (rp) then
         ipar(9) = 3*n + 1
      else
         ipar(9) = n + n + 1
      endif
      ipar(10) = 4
      return
c
 50   if (rp) then
         ipar(1) = 6
         ipar(8) = ipar(9)
         ipar(9) = n + n + 1
         ipar(10) = 5
         return
      endif
c
 60   ipar(7) = ipar(7) + 1
      zzm1 = zz
      zz = distdot(n,wk(1,3),1,wk(1,3),1)
      fpar(11) = fpar(11) + 2 * n
      if (brkdn(zz,ipar)) goto 900
      if (ipar(7).gt.3) then
         alpha = zz / zzm1
         do i = 1, n
            wk(i,2) = wk(i,3) + alpha * wk(i,2)
         enddo
         fpar(11) = fpar(11) + 2 * n
      else
         do i = 1, n
            wk(i,2) = wk(i,3)
         enddo
      endif
c
c     before iteration can continue, we need to compute A * p
c
      if (rp) then
         ipar(1) = 5
         ipar(8) = n + 1
         if (lp) then
            ipar(9) = ipar(8) + n
         else
            ipar(9) = 3*n + 1
         endif
         ipar(10) = 6
         return
      endif
c
 70   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = n + 1
      endif
      if (lp) then
        ipar(9) = 3*n+1
      else
         ipar(9) = n+n+1
      endif
      ipar(10) = 7
      return
c
 80   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = n+n+1
         ipar(10) = 8
         return
      endif
c
c     update the solution -- accumulate the changes in w(:,5)
c
 90   ipar(7) = ipar(7) + 1
      alpha = distdot(n,wk(1,3),1,wk(1,3),1)
      fpar(11) = fpar(11) + 2 * n
      if (brkdn(alpha,ipar)) goto 900
      alpha = zz / alpha
      do i = 1, n
         wk(i,5) = wk(i,5) + alpha * wk(i,2)
         wk(i,1) = wk(i,1) - alpha * wk(i,3)
      enddo
      fpar(11) = fpar(11) + 4 * n
c
c     are we ready to terminate ?
c
      if (ipar(3).eq.999) then
         ipar(1) = 10
         ipar(8) = 4*n + 1
         ipar(9) = 3*n + 1
         ipar(10) = 9
         return
      endif
 100  if (ipar(3).eq.999) then
         if (ipar(11).eq.1) goto 900
      else if (stopbis(n,ipar,1,fpar,wk,wk(1,2),alpha)) then
         goto 900
      endif
c
c     continue the iterations
c
      goto 30
c
c     clean up -- necessary to accommodate the right-preconditioning
c
 900  if (rp) then
         if (ipar(1).lt.0) ipar(12) = ipar(1)
         ipar(1) = 5
         ipar(8) = 4*n + 1
         ipar(9) = ipar(8) - n
         ipar(10) = 10
         return
      endif
 110  if (rp) then
         call tidycg(n,ipar,fpar,sol,wk(1,4))
      else
         call tidycg(n,ipar,fpar,sol,wk(1,5))
      endif
      return
      end
c
      subroutine checkref(nx,nelx,ijk,node,nodcode,
     *     nbound,  nxnew,nelxnew) 
c-------------------------------------------------------------	
c returns the expected the new number of nodes and 
c elemnts of refall is applied to current grid once.
c
c nx	= number of nodes at input
c nelx	= number of elements at input
c ijk	= connectivity matrix: for node k, ijk(*,k) point to the
c         nodes of element k.
c nbound  = number of boundary points on entry - enter zero if
c	     unknown
c
c nodcode= boundary information list for each node with the
c	   following meaning:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point.
c
c nxnew  = new number of nodes if refall were to be applied
c nelxnew = same for nelx.
c--------------------------------------------------------------
       integer ijk(node,1),nodcode(nx)
c
	nelxnew = nelx*4
c
c count the number of boundary nodes
c
	if (nbound .ne. 0) goto 2
	do 1 j=1, nx
	if (nodcode(j) .ge. 1) nbound = nbound+1
 1	continue
c number of edges=[3*(number of elmts) + number of bound nodes ]/ 2
 2	continue
	nxnew = nx + (3*nelx+nbound)/2
	nbound = 2*nbound
	return
	end				
c----------------------------------------------------------------------- 
	subroutine chkelmt (nx, x, y, nelx, ijk, node)
	implicit real*8 (a-h,o-z)
	dimension ijk(node,1),x(1),y(1)
c----------------------------------------------------------------------- 
c this subsourine checks the labeling within each elment and reorders
c the nodes in they ar not correctly ordered.
c----------------------------------------------------------------------- 
	do 1 nel =1, nelx
 	det = x(ijk(2,nel))*(y(ijk(3,nel))-y(ijk(1,nel)))+
     *        x(ijk(3,nel))*(y(ijk(1,nel))-y(ijk(2,nel)))+
     *        x(ijk(1,nel))*(y(ijk(2,nel))-y(ijk(3,nel)))
c
c if determinant negative exchange last two nodes of elements.
c
	if (det .lt. 0.0d0) then
	    j = ijk(2,nel)
	    ijk(2,nel) = ijk(3,nel)
	    ijk(3,nel) = j
	endif
 1	continue
c
	return			
	end			 
c-----------------------------------------------------------------------
      subroutine cleanel (nelx,ijk,node,nodcode,nodexc) 
c      implicit none
      integer nelx,node,nodexc,ijk(node,nelx),nodcode(*)
c-----------------------------------------------------------------------
c     this routine remove certain types of elements from the mesh
c     An element whose nodes are all labelled by the same label 
c     nodexc are removed. nelx is changed accordingly on return.
c-----------------------------------------------------------------------
      logical exclude
      integer nel, i,k 
      nel = 1
 1    continue
      exclude = .true. 
      do i=1,node
         k = ijk(i,nel) 
         exclude = (exclude .and. nodcode(k).eq. nodexc) 
      enddo
c     
      if (exclude) then
         do i=1,node
            ijk(i,nel) = ijk(i,nelx) 
         enddo
         nelx = nelx - 1
      else
         nel = nel+1
      endif
      if (nel .le. nelx) goto 1
      return
c-----------------------------------------------------------------------
c-----end-of-cleanel---------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine cleannods (nx,x,y,nelx,ijk,node,nodcode,iperm) 
c      implicit none 
      integer nx,nelx,node,ijk(node,nelx),nodcode(*),iperm(nx) 
      real*8 x(nx),y(nx)
c-----------------------------------------------------------------------
c     this routine removes the nodes that do not belong to any element
c     (spurious points) and relabels the ijk array accordingly.
c-----------------------------------------------------------------------
      integer nel,i,k,j,indx
c
      do j=1, nx
         iperm(j) = 0
      enddo
c     
      do nel = 1, nelx
         do i=1,node
            k = ijk(i,nel) 
            iperm(k) = nel 
         enddo
      enddo
c
      indx = 0 
      do j =1, nx
         if (iperm(j) .ne. 0) then
            indx = indx+1
            iperm(indx) = j
            x(indx) = x(j)
            y(indx) = y(j) 
            nodcode(indx) = nodcode(j) 
         endif
      enddo
c     
c     update nx
c     
      nx = indx
c     
c     old number to new numbers
c     
      do j =1, nx 
         iperm(nx+iperm(j)) = j 
      enddo 
c     
c     
c     change all node numbers in ijk
c     
      do nel = 1, nelx
         do i=1,node
            k = ijk(i,nel) 
            k = iperm(nx+k) 
            ijk(i,nel) = k
         enddo
      enddo
      return
c-----------------------------------------------------------------------
c-----end-of-cleannod---------------------------------------------------
      end


c-----------------------------------------------------------------------
      subroutine clncsr(job,value2,nrow,a,ja,ia,indu,iwk)
c     .. Scalar Arguments ..
      integer job, nrow, value2
c     ..
c     .. Array Arguments ..
      integer ia(nrow+1),indu(nrow),iwk(nrow+1),ja(*)
      real*8  a(*)
c     ..
c
c     This routine performs two tasks to clean up a CSR matrix
c     -- remove duplicate/zero entries,
c     -- perform a partial ordering, new order lower triangular part,
c        main diagonal, upper triangular part.
c
c     On entry:
c
c     job   = options
c         0 -- nothing is done
c         1 -- eliminate duplicate entries, zero entries.
c         2 -- eliminate duplicate entries and perform partial ordering.
c         3 -- eliminate duplicate entries, sort the entries in the
c              increasing order of clumn indices.
c
c     value2  -- 0 the matrix is pattern only (a is not touched)
c                1 matrix has values too.
c     nrow    -- row dimension of the matrix
c     a,ja,ia -- input matrix in CSR format
c
c     On return:
c     a,ja,ia -- cleaned matrix
c     indu    -- pointers to the beginning of the upper triangular
c                portion if job > 1
c
c     Work space:
c     iwk     -- integer work space of size nrow+1
c
c     .. Local Scalars ..
      integer i,j,k,ko,ipos,kfirst,klast
      real*8  tmp
c     ..
c
      if (job.le.0) return
c
c     .. eliminate duplicate entries --
c     array INDU is used as marker for existing indices, it is also the
c     location of the entry.
c     IWK is used to stored the old IA array.
c     matrix is copied to squeeze out the space taken by the duplicated
c     entries.
c
      do 90 i = 1, nrow
         indu(i) = 0
         iwk(i) = ia(i)
 90   continue
      iwk(nrow+1) = ia(nrow+1)
      k = 1
      do 120 i = 1, nrow
         ia(i) = k
         ipos = iwk(i)
         klast = iwk(i+1)
 100     if (ipos.lt.klast) then
            j = ja(ipos)
            if (indu(j).eq.0) then
c     .. new entry ..
               if (value2.ne.0) then
                  if (a(ipos) .ne. 0.0D0) then
                     indu(j) = k
                     ja(k) = ja(ipos)
                     a(k) = a(ipos)
                     k = k + 1
                  endif
               else
                  indu(j) = k
                  ja(k) = ja(ipos)
                  k = k + 1
               endif
            else if (value2.ne.0) then
c     .. duplicate entry ..
               a(indu(j)) = a(indu(j)) + a(ipos)
            endif
            ipos = ipos + 1
            go to 100
         endif
c     .. remove marks before working on the next row ..
         do 110 ipos = ia(i), k - 1
            indu(ja(ipos)) = 0
 110     continue
 120  continue
      ia(nrow+1) = k
      if (job.le.1) return
c
c     .. partial ordering ..
c     split the matrix into strict upper/lower triangular
c     parts, INDU points to the the beginning of the upper part.
c
      do 140 i = 1, nrow
         klast = ia(i+1) - 1
         kfirst = ia(i)
 130     if (klast.gt.kfirst) then
            if (ja(klast).lt.i .and. ja(kfirst).ge.i) then
c     .. swap klast with kfirst ..
               j = ja(klast)
               ja(klast) = ja(kfirst)
               ja(kfirst) = j
               if (value2.ne.0) then
                  tmp = a(klast)
                  a(klast) = a(kfirst)
                  a(kfirst) = tmp
               endif
            endif
            if (ja(klast).ge.i)
     &         klast = klast - 1
            if (ja(kfirst).lt.i)
     &         kfirst = kfirst + 1
            go to 130
         endif
c
         if (ja(klast).lt.i) then
            indu(i) = klast + 1
         else
            indu(i) = klast
         endif
 140  continue
      if (job.le.2) return
c
c     .. order the entries according to column indices
c     burble-sort is used
c
      do 190 i = 1, nrow
         do 160 ipos = ia(i), indu(i)-1
            do 150 j = indu(i)-1, ipos+1, -1
               k = j - 1
               if (ja(k).gt.ja(j)) then
                  ko = ja(k)
                  ja(k) = ja(j)
                  ja(j) = ko
                  if (value2.ne.0) then
                     tmp = a(k)
                     a(k) = a(j)
                     a(j) = tmp
                  endif
               endif
 150        continue
 160     continue
         do 180 ipos = indu(i), ia(i+1)-1
            do 170 j = ia(i+1)-1, ipos+1, -1
               k = j - 1
               if (ja(k).gt.ja(j)) then
                  ko = ja(k)
                  ja(k) = ja(j)
                  ja(j) = ko
                  if (value2.ne.0) then
                     tmp = a(k)
                     a(k) = a(j)
                     a(j) = tmp
                  endif
               endif
 170        continue
 180     continue
 190  continue
      return
c---- end of clncsr ----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine clos2bdr (nx,xnew,ynew,x,y,x1,x2,y1,y2,excl,nodcode) 
      implicit none 
      integer nx,nodcode(nx)
      real*8 x(nx),y(nx),xnew,ynew,x1,x2,y1,y2,excl 
c----------------------------------------------------------------------- 
c     takes care of case where a point generated is too close to the
c     boundary  -- in this case projects the previous point to the
c     rectangle boundary == that makes some exclusion criterion
c     violated... does a simple job.
c----------------------------------------------------------------------- 
      if (xnew .ge. x2-excl) then
         x(nx) = x2
         y(nx) = y(nx-1) 
         nodcode(nx) = 1 
      endif
      if (xnew .le. x1+excl) then
         x(nx) = x1 
         y(nx) = y(nx-1) 
         nodcode(nx) = 1 
      endif
      if (ynew .ge. y2-excl) then
         y(nx) = y2
         x(nx) = x(nx-1) 
         nodcode(nx) = 1 
      endif
      if (ynew .le. y1+excl) then
         y(nx) = y1 
         x(nx) = x(nx-1) 
         nodcode(nx) = 1 
      endif
c     
      return
      end
c-----end of fdaddbc----------------------------------------------------
c-----------------------------------------------------------------------
      subroutine clrow(i, a, ja, ia)
      integer i, ja(*), ia(*), k
      real *8 a(*)
c-----------------------------------------------------------------------
c     clear the row i to all zero, but still keep the structure of the
c     CSR matrix
c-----------------------------------------------------------------------
      do 10 k = ia(i), ia(i+1)-1
         a(k) = 0.0D0
 10   continue
c
      return
c-----end of clrow------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine cnrms   (nrow, nrm, a, ja, ia, diag) 
      real*8 a(*), diag(nrow) 
      integer ja(*), ia(nrow+1) 
c-----------------------------------------------------------------------
c gets the norms of each column of A. (choice of three norms)
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c on return:
c----------
c
c diag = real vector of length nrow containing the norms
c NOTE: this is designed for square matrices. For the case 
c nrow .ne. ncol -- diag must be of size max(nrow,ncol) even
c though only the ncol first entries will be filled.. 
c [report E. Canot 10/20/05 ] 
c-----------------------------------------------------------------
      do 10 k=1, nrow 
         diag(k) = 0.0d0
 10   continue
      do 1 ii=1,nrow
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         do 2 k=k1, k2
            j = ja(k) 
c     update the norm of each column
            if (nrm .eq. 0) then
               diag(j) = max(diag(j),abs(a(k) ) ) 
            elseif (nrm .eq. 1) then
               diag(j) = diag(j) + abs(a(k) ) 
            else
               diag(j) = diag(j)+a(k)**2
            endif 
 2       continue
 1    continue
      if (nrm .ne. 2) return
      do 3 k=1, nrow
         diag(k) = sqrt(diag(k))
 3    continue
      return
c-----------------------------------------------------------------------
c------------end-of-cnrms-----------------------------------------------
      end 
c----------------------------------------------------------------------- 
      subroutine coicsr (n,nnz,job,a,ja,ia,iwk)
      integer ia(nnz),ja(nnz),iwk(n+1) 
      real*8 a(*)
c------------------------------------------------------------------------
c IN-PLACE coo-csr conversion routine.
c------------------------------------------------------------------------
c this subroutine converts a matrix stored in coordinate format into 
c the csr format. The conversion is done in place in that the arrays 
c a,ja,ia of the result are overwritten onto the original arrays.
c------------------------------------------------------------------------
c on entry:
c--------- 
c n	= integer. row dimension of A.
c nnz	= integer. number of nonzero elements in A.
c job   = integer. Job indicator. when job=1, the real values in a are
c         filled. Otherwise a is not touched and the structure of the
c         array only (i.e. ja, ia)  is obtained.
c a	= real array of size nnz (number of nonzero elements in A)
c         containing the nonzero elements 
c ja	= integer array of length nnz containing the column positions
c 	  of the corresponding elements in a.
c ia	= integer array of length nnz containing the row positions
c 	  of the corresponding elements in a.
c iwk	= integer work array of length n+1 
c on return:
c----------
c a
c ja 
c ia	= contains the compressed sparse row data structure for the 
c         resulting matrix.
c Note: 
c-------
c         the entries of the output matrix are not sorted (the column
c         indices in each are not in increasing order) use coocsr
c         if you want them sorted.
c----------------------------------------------------------------------c
c  Coded by Y. Saad, Sep. 26 1989                                      c
c----------------------------------------------------------------------c
      real*8 t,tnext
      logical values
c----------------------------------------------------------------------- 
      values = (job .eq. 1) 
c find pointer array for resulting matrix. 
      do 35 i=1,n+1
         iwk(i) = 0
 35   continue
      do 4 k=1,nnz
         i = ia(k)
         iwk(i+1) = iwk(i+1)+1
 4    continue 
c------------------------------------------------------------------------
      iwk(1) = 1 
      do 44 i=2,n
         iwk(i) = iwk(i-1) + iwk(i)
 44   continue 
c
c     loop for a cycle in chasing process. 
c
      init = 1
      k = 0
 5    if (values) t = a(init)
      i = ia(init)
      j = ja(init)
      ia(init) = -1
c------------------------------------------------------------------------
 6    k = k+1 		
c     current row number is i.  determine  where to go. 
      ipos = iwk(i)
c     save the chased element. 
      if (values) tnext = a(ipos)
      inext = ia(ipos)
      jnext = ja(ipos)
c     then occupy its location.
      if (values) a(ipos)  = t
      ja(ipos) = j
c     update pointer information for next element to come in row i. 
      iwk(i) = ipos+1
c     determine  next element to be chased,
      if (ia(ipos) .lt. 0) goto 65
      t = tnext
      i = inext
      j = jnext 
      ia(ipos) = -1
      if (k .lt. nnz) goto 6
      goto 70
 65   init = init+1
      if (init .gt. nnz) goto 70
      if (ia(init) .lt. 0) goto 65
c     restart chasing --	
      goto 5
 70   do 80 i=1,n 
         ia(i+1) = iwk(i)
 80   continue
      ia(1) = 1
      return
c----------------- end of coicsr ----------------------------------------
c------------------------------------------------------------------------
      end
c***********************************************************************
      SUBROUTINE COMPOS(n, lpw0, lpw1)
C-----------------------------------------------------------------------
c
c     We take account of the original order of unknowns. We put the
c     final result on LPW0.
c
C-----------------------------------------------------------------------
      DIMENSION lpw0(n), lpw1(n)
C-----------------------------------------------------------------------
c     Laura C. Dutto - Mars 1994
C-----------------------------------------------------------------------
C$DOACROSS if(n .gt. 250), local(i0)
      do i0 = 1, n
         lpw0(i0) = lpw1(lpw0(i0))
      enddo
c
      return
      end
c----------------------------------------------------------------------- 
      subroutine coocsr(nrow,nnz,a,ir,jc,ao,jao,iao)
c----------------------------------------------------------------------- 
      real*8 a(*),ao(*),x
      integer ir(*),jc(*),jao(*),iao(*)
c-----------------------------------------------------------------------
c  Coordinate     to   Compressed Sparse Row 
c----------------------------------------------------------------------- 
c converts a matrix that is stored in coordinate format
c  a, ir, jc into a row general sparse ao, jao, iao format.
c
c on entry:
c--------- 
c nrow	= dimension of the matrix 
c nnz	= number of nonzero elements in matrix
c a,
c ir, 
c jc    = matrix in coordinate format. a(k), ir(k), jc(k) store the nnz
c         nonzero elements of the matrix with a(k) = actual real value of
c 	  the elements, ir(k) = its row number and jc(k) = its column 
c	  number. The order of the elements is arbitrary. 
c
c on return:
c----------- 
c ir 	is destroyed
c
c ao, jao, iao = matrix in general sparse matrix format with ao 
c 	continung the real values, jao containing the column indices, 
c	and iao being the pointer to the beginning of the row, 
c	in arrays ao, jao.
c
c Notes:
c------ This routine is NOT in place.  See coicsr
c
c------------------------------------------------------------------------
      do 1 k=1,nrow+1
         iao(k) = 0
 1    continue
c determine row-lengths.
      do 2 k=1, nnz
         iao(ir(k)) = iao(ir(k))+1
 2    continue
c starting position of each row..
      k = 1
      do 3 j=1,nrow+1
         k0 = iao(j)
         iao(j) = k
         k = k+k0
 3    continue
c go through the structure  once more. Fill in output matrix.
      do 4 k=1, nnz
         i = ir(k)
         j = jc(k)
         x = a(k)
         iad = iao(i)
         ao(iad) =  x
         jao(iad) = j
         iao(i) = iad+1
 4    continue
c shift back iao
      do 5 j=nrow,1,-1
         iao(j+1) = iao(j)
 5    continue
      iao(1) = 1
      return
c------------- end of coocsr ------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c-------end-of-dcsort---------------------------------------------------
c-----------------------------------------------------------------------
      subroutine cooell(job,n,nnz,a,ja,ia,ao,jao,lda,ncmax,nc,ierr)
      implicit none
      integer job,n,nnz,lda,ncmax,nc,ierr
      integer ja(nnz),ia(nnz),jao(lda,ncmax)
      real*8  a(nnz),ao(lda,ncmax)
c-----------------------------------------------------------------------
c     COOrdinate format to ELLpack format
c-----------------------------------------------------------------------
c     On entry:
c     job     -- 0 if only pattern is to be processed(AO is not touched)
c     n       -- number of rows in the matrix
c     a,ja,ia -- input matix in COO format
c     lda     -- leading dimension of array AO and JAO
c     ncmax   -- size of the second dimension of array AO and JAO
c
c     On exit:
c     ao,jao  -- the matrix in ELL format
c     nc      -- maximum number of nonzeros per row
c     ierr    -- 0 if convertion succeeded
c                -1 if LDA < N
c                nc if NC > ncmax
c
c     NOTE: the last column of JAO is used as work space!!
c-----------------------------------------------------------------------
      integer i,j,k,ip
      real*8  zero
      logical copyval
      parameter (zero=0.0D0)
c     .. first executable statement ..
      copyval = (job.ne.0)
      if (lda .lt. n) then
         ierr = -1
         return
      endif
c     .. use the last column of JAO as workspace
c     .. initialize the work space
      do i = 1, n
         jao(i,ncmax) = 0
      enddo
      nc = 0
c     .. go through ia and ja to find out number nonzero per row
      do k = 1, nnz
         i = ia(k)
         jao(i,ncmax) = jao(i,ncmax) + 1
      enddo
c     .. maximum number of nonzero per row
      nc = 0
      do i = 1, n
         if (nc.lt.jao(i,ncmax)) nc = jao(i,ncmax)
         jao(i,ncmax) = 0
      enddo
c     .. if nc > ncmax retrun now
      if (nc.gt.ncmax) then
         ierr = nc
         return
      endif
c     .. go through ia and ja to copy the matrix to AO and JAO
      do k = 1, nnz
         i = ia(k)
         j = ja(k)
         jao(i,ncmax) = jao(i,ncmax) + 1
         ip = jao(i,ncmax)
         if (ip.gt.nc) nc = ip
         if (copyval) ao(i,ip) = a(k)
         jao(i,ip) = j
      enddo
c     .. fill the unspecified elements of AO and JAO with zero diagonals
      do i = 1, n
         do j = ia(i+1)-ia(i)+1, nc
            jao(i,j)=i
            if(copyval) ao(i,j) = zero
         enddo
      enddo
      ierr = 0
c
      return
      end
c----------------------------------------------------------------------- 
      subroutine copmat (nrow,a,ja,ia,ao,jao,iao,ipos,job) 
      real*8 a(*),ao(*) 
      integer nrow, ia(*),ja(*),jao(*),iao(*), ipos, job 
c----------------------------------------------------------------------
c copies the matrix a, ja, ia, into the matrix ao, jao, iao. 
c----------------------------------------------------------------------
c on entry:
c---------
c nrow	= row dimension of the matrix 
c a,
c ja,
c ia    = input matrix in compressed sparse row format. 
c ipos  = integer. indicates the position in the array ao, jao
c         where the first element should be copied. Thus 
c         iao(1) = ipos on return. 
c job   = job indicator. if (job .ne. 1) the values are not copies 
c         (i.e., pattern only is copied in the form of arrays ja, ia).
c
c on return:
c----------
c ao,
c jao,
c iao   = output matrix containing the same data as a, ja, ia.
c-----------------------------------------------------------------------
c           Y. Saad, March 1990. 
c-----------------------------------------------------------------------
c local variables
      integer kst, i, k 
c
      kst    = ipos -ia(1) 
      do 100 i = 1, nrow+1
         iao(i) = ia(i) + kst
 100  continue
c     
      do 200 k=ia(1), ia(nrow+1)-1
         jao(kst+k)= ja(k)
 200  continue
c
      if (job .ne. 1) return
      do 201 k=ia(1), ia(nrow+1)-1
         ao(kst+k) = a(k)
 201  continue
c
      return
c--------end-of-copmat -------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine coscal(nrow,job,nrm,a,ja,ia,diag,b,jb,ib,ierr) 
c----------------------------------------------------------------------- 
      real*8 a(*),b(*),diag(nrow) 
      integer nrow,job,ja(*),jb(*),ia(nrow+1),ib(nrow+1),ierr 
c-----------------------------------------------------------------------
c scales the columns of A such that their norms are one on return
c result matrix written on b, or overwritten on A.
c 3 choices of norms: 1-norm, 2-norm, max-norm. in place.
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c job   = integer. job indicator. Job=0 means get array b only
c         job = 1 means get b, and the integer arrays ib, jb.
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c on return:
c----------
c
c diag = diagonal matrix stored as a vector containing the matrix
c        by which the columns have been scaled, i.e., on return 
c        we have B = A * Diag
c
c b, 
c jb, 
c ib	= resulting matrix B in compressed sparse row sparse format.
c
c ierr  = error message. ierr=0     : Normal return 
c                        ierr=i > 0 : Column number i is a zero row.
c Notes:
c-------
c 1)     The column dimension of A is not needed. 
c 2)     algorithm in place (B can take the place of A).
c-----------------------------------------------------------------
      call cnrms (nrow,nrm,a,ja,ia,diag)
      ierr = 0
      do 1 j=1, nrow
         if (diag(j) .eq. 0.0) then
            ierr = j 
            return
         else
            diag(j) = 1.0d0/diag(j)
         endif
 1    continue
      call amudia (nrow,job,a,ja,ia,diag,b,jb,ib)
      return
c--------end-of-coscal-------------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine cperm (nrow,a,ja,ia,ao,jao,iao,perm,job) 
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(*), job
      real*8 a(*), ao(*) 
c-----------------------------------------------------------------------
c this subroutine permutes the columns of a matrix a, ja, ia.
c the result is written in the output matrix  ao, jao, iao.
c cperm computes B = A P, where  P is a permutation matrix
c that maps column j into column perm(j), i.e., on return 
c      a(i,j) becomes a(i,perm(j)) in new matrix 
c Y. Saad, May 2, 1990 / modified Jan. 28, 1991. 
c-----------------------------------------------------------------------
c on entry:
c----------
c nrow 	= row dimension of the matrix
c
c a, ja, ia = input matrix in csr format. 
c
c perm	= integer array of length ncol (number of columns of A
c         containing the permutation array  the columns: 
c         a(i,j) in the original matrix becomes a(i,perm(j))
c         in the output matrix.
c
c job	= integer indicating the work to be done:
c 		job = 1	permute a, ja, ia into ao, jao, iao 
c                       (including the copying of real values ao and
c                       the array iao).
c 		job .ne. 1 :  ignore real values ao and ignore iao.
c
c------------
c on return: 
c------------ 
c ao, jao, iao = input matrix in a, ja, ia format (array ao not needed)
c
c Notes:
c------- 
c 1. if job=1 then ao, iao are not used.
c 2. This routine is in place: ja, jao can be the same. 
c 3. If the matrix is initially sorted (by increasing column number) 
c    then ao,jao,iao  may not be on return. 
c 
c----------------------------------------------------------------------c
c local parameters:
      integer k, i, nnz
c
      nnz = ia(nrow+1)-1
      do 100 k=1,nnz
         jao(k) = perm(ja(k)) 
 100  continue
c
c     done with ja array. return if no need to touch values.
c
      if (job .ne. 1) return
c
c else get new pointers -- and copy values too.
c 
      do 1 i=1, nrow+1
         iao(i) = ia(i)
 1    continue
c
      do 2 k=1, nnz
         ao(k) = a(k)
 2    continue
c
      return
c---------end-of-cperm-------------------------------------------------- 
c-----------------------------------------------------------------------
      end
      subroutine  cscal(n,ca,cx,incx)
c
c     scales a vector by a constant.
c     jack dongarra, linpack,  3/11/78.
c
      complex ca,cx(1)
      integer i,incx,n,nincx
c
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        cx(i) = ca*cx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
   20 do 30 i = 1,n
        cx(i) = ca*cx(i)
   30 continue
      return
      end
c-----------------------------------------------------------------------
c---------------------------end-of-vbrcsr-------------------------------
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      subroutine csorted(n, ia, ja, sorted)
c-----------------------------------------------------------------------
      integer n, ia(n+1), ja(*)
      logical sorted
c-----------------------------------------------------------------------
c     Checks if matrix in CSR format is sorted by columns.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     n       = number of rows in matrix
c     ia, ja  = sparsity structure of matrix in CSR format
c
c     On return:
c---------------
c     sorted  = indicates if matrix is sorted by columns
c
c-----------------------------------------------------------------------
c-----local variables
      integer i,j
c---------------------------------
      do i = 1, n
         do j = ia(i)+1, ia(i+1)-1
            if (ja(j-1) .ge. ja(j)) then
               sorted = .false.
               return
            endif
         enddo
      enddo
      sorted = .true.
      return
      end
c-----------------------------------------------------------------------
      subroutine csort (n,a,ja,ia,iwork,values) 
      logical values
      integer n, ja(*), ia(n+1), iwork(*) 
      real*8 a(*) 
c-----------------------------------------------------------------------
c This routine sorts the elements of  a matrix (stored in Compressed
c Sparse Row Format) in increasing order of their column indices within 
c each row. It uses a form of bucket sort with a cost of O(nnz) where
c nnz = number of nonzero elements. 
c requires an integer work array of length 2*nnz.  
c-----------------------------------------------------------------------
c on entry:
c--------- 
c n     = the row dimension of the matrix
c a     = the matrix A in compressed sparse row format.
c ja    = the array of column indices of the elements in array a.
c ia    = the array of pointers to the rows. 
c iwork = integer work array of length max ( m+1, 2*nnz ) 
c         where m   = column dimension of the matrix
c               nnz = (ia(n+1)-ia(1))
c values= logical indicating whether or not the real values a(*) must 
c         also be permuted. if (.not. values) then the array a is not
c         touched by csort and can be a dummy array. 
c 
c on return:
c----------
c the matrix stored in the structure a, ja, ia is permuted in such a
c way that the column indices are in increasing order within each row.
c iwork(1:nnz) contains the permutation used  to rearrange the elements.
c----------------------------------------------------------------------- 
c Y. Saad - Feb. 1, 1991.
c L. M. Baker - Oct. 15, 2009.  Correct computation of column pointers
c                               for rectangular matrices 
c-----------------------------------------------------------------------
c local variables
      integer i, k, j, m, ifirst, nnz, next  
c
c count the number of elements in each column
c
      m = 0
      do 11 i=1, n
         do 1 k=ia(i), ia(i+1)-1 
            m = max( m, ja(k) )
 1       continue 
 11   continue
      do 2 j=1,m
         iwork(j+1) = 0
 2    continue
      do 33 i=1, n
         do 3 k=ia(i), ia(i+1)-1 
            j = ja(k)
            iwork(j+1) = iwork(j+1)+1
 3       continue 
 33   continue
c
c compute pointers from lengths. 
c
      iwork(1) = 1
      do 4 i=1,m
         iwork(i+1) = iwork(i) + iwork(i+1)
 4    continue
c 
c get the positions of the nonzero elements in order of columns.
c
      ifirst = ia(1) 
      nnz = ia(n+1)-ifirst
      do 5 i=1,n
         do 51 k=ia(i),ia(i+1)-1 
            j = ja(k) 
            next = iwork(j)
            iwork(nnz+next) = k
            iwork(j) = next+1
 51      continue
 5    continue
c
c convert to coordinate format
c 
      do 6 i=1, n
         do 61 k=ia(i), ia(i+1)-1 
            iwork(k) = i
 61      continue
 6    continue
c
c loop to find permutation: for each element find the correct 
c position in (sorted) arrays a, ja. Record this in iwork. 
c 
      do 7 k=1, nnz
         ko = iwork(nnz+k) 
         irow = iwork(ko)
         next = ia(irow)
c
c the current element should go in next position in row. iwork
c records this position. 
c 
         iwork(ko) = next
         ia(irow)  = next+1
 7       continue
c
c perform an in-place permutation of the  arrays.
c 
         call ivperm (nnz, ja(ifirst), iwork) 
         if (values) call dvperm (nnz, a(ifirst), iwork) 
c
c reshift the pointers of the original matrix back.
c 
      do 8 i=n,1,-1
         ia(i+1) = ia(i)
 8    continue
      ia(1) = ifirst 
c
      return 
c---------------end-of-csort-------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrbnd (n,a,ja,ia,job,abd,nabd,lowd,ml,mu,ierr)
      real*8 a(*),abd(nabd,n)
      integer ia(n+1),ja(*)
c----------------------------------------------------------------------- 
c   Compressed Sparse Row  to  Banded (Linpack ) format.
c----------------------------------------------------------------------- 
c this subroutine converts a general sparse matrix stored in
c compressed sparse row format into the banded format. for the
c banded format,the Linpack conventions are assumed (see below).
c----------------------------------------------------------------------- 
c on entry:
c----------
c n	= integer,the actual row dimension of the matrix.
c
c a,
c ja,
c ia    = input matrix stored in compressed sparse row format.
c
c job	= integer. if job=1 then the values of the lower bandwith ml 
c         and the upper bandwidth mu are determined internally. 
c         otherwise it is assumed that the values of ml and mu 
c         are the correct bandwidths on input. See ml and mu below.
c
c nabd  = integer. first dimension of array abd.
c
c lowd  = integer. this should be set to the row number in abd where
c         the lowest diagonal (leftmost) of A is located. 
c         lowd should be  ( 1  .le.  lowd  .le. nabd).
c         if it is not known in advance what lowd should be
c         enter lowd = 0 and the default value lowd = ml+mu+1
c         will be chosen. Alternative: call routine getbwd from unary
c         first to detrermione ml and mu then define lowd accordingly.
c         (Note: the banded solvers in linpack use lowd=2*ml+mu+1. )
c
c ml	= integer. equal to the bandwidth of the strict lower part of A
c mu	= integer. equal to the bandwidth of the strict upper part of A
c         thus the total bandwidth of A is ml+mu+1.
c         if ml+mu+1 is found to be larger than lowd then an error 
c         flag is raised (unless lowd = 0). see ierr.
c
c note:   ml and mu are assumed to have	 the correct bandwidth values
c         as defined above if job is set to zero on entry.
c
c on return:
c-----------
c
c abd   = real array of dimension abd(nabd,n).
c         on return contains the values of the matrix stored in
c         banded form. The j-th column of abd contains the elements
c         of the j-th column of  the original matrix comprised in the
c         band ( i in (j-ml,j+mu) ) with the lowest diagonal at
c         the bottom row (row lowd). See details below for this format.
c
c ml	= integer. equal to the bandwidth of the strict lower part of A
c mu	= integer. equal to the bandwidth of the strict upper part of A
c         if job=1 on entry then these two values are internally computed.
c
c lowd  = integer. row number in abd where the lowest diagonal 
c         (leftmost) of A is located on return. In case lowd = 0
c         on return, then it is defined to ml+mu+1 on return and the
c         lowd will contain this value on return. `
c
c ierr  = integer. used for error messages. On return:
c         ierr .eq. 0  :means normal return
c         ierr .eq. -1 : means invalid value for lowd. (either .lt. 0
c         or larger than nabd).
c         ierr .eq. -2 : means that lowd is not large enough and as 
c         result the matrix cannot be stored in array abd. 
c         lowd should be at least ml+mu+1, where ml and mu are as
c         provided on output.
c
c----------------------------------------------------------------------* 
c Additional details on banded format.  (this closely follows the      *
c format used in linpack. may be useful for converting a matrix into   *
c this storage format in order to use the linpack  banded solvers).    * 
c----------------------------------------------------------------------*
c             ---  band storage format  for matrix abd ---             * 
c uses ml+mu+1 rows of abd(nabd,*) to store the diagonals of           *
c a in rows of abd starting from the lowest (sub)-diagonal  which  is  *
c stored in row number lowd of abd. the minimum number of rows needed  *
c in abd is ml+mu+1, i.e., the minimum value for lowd is ml+mu+1. the  *
c j-th  column  of  abd contains the elements of the j-th column of a, *
c from bottom to top: the element a(j+ml,j) is stored in  position     *
c abd(lowd,j), then a(j+ml-1,j) in position abd(lowd-1,j) and so on.   *
c Generally, the element a(j+k,j) of original matrix a is stored in    *
c position abd(lowd+k-ml,j), for k=ml,ml-1,..,0,-1, -mu.               *
c The first dimension nabd of abd must be .ge. lowd                    *
c                                                                      *
c     example [from linpack ]:   if the original matrix is             *
c                                                                      *
c              11 12 13  0  0  0                                       *
c              21 22 23 24  0  0                                       *
c               0 32 33 34 35  0     original banded matrix            *
c               0  0 43 44 45 46                                       *
c               0  0  0 54 55 56                                       *
c               0  0  0  0 65 66                                       *
c                                                                      *
c then  n = 6, ml = 1, mu = 2. lowd should be .ge. 4 (=ml+mu+1)  and   *
c if lowd = 5 for example, abd  should be:                             *
c                                                                      *
c untouched --> x  x  x  x  x  x                                       *
c               *  * 13 24 35 46                                       *
c               * 12 23 34 45 56    resulting abd matrix in banded     *
c              11 22 33 44 55 66    format                             *
c  row lowd--> 21 32 43 54 65  *                                       *
c                                                                      *
c * = not used                                                         *
c                                                                      
*
c----------------------------------------------------------------------*
c first determine ml and mu.
c----------------------------------------------------------------------- 
      ierr = 0
c-----------
      if (job .eq. 1) call getbwd(n,a,ja,ia,ml,mu)
      m = ml+mu+1
      if (lowd .eq. 0) lowd = m
      if (m .gt. lowd)  ierr = -2
      if (lowd .gt. nabd .or. lowd .lt. 0) ierr = -1
      if (ierr .lt. 0) return
c------------
      do 15  i=1,m
         ii = lowd -i+1
         do 10 j=1,n
	    abd(ii,j) = 0.0d0
 10      continue
 15   continue
c---------------------------------------------------------------------	   
      mdiag = lowd-ml
      do 30 i=1,n
         do 20 k=ia(i),ia(i+1)-1
            j = ja(k)
            abd(i-j+mdiag,j) = a(k) 
 20      continue
 30   continue
      return
c------------- end of csrbnd ------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine csrbsr (job,nrow,m,na,a,ja,ia,ao,jao,iao,iw,ierr)
      implicit none
      integer job,ierr,nrow,m,na,ia(nrow+1),ja(*),jao(na),iao(*),iw(*)
      real*8 a(*),ao(na,*)
c-----------------------------------------------------------------------
c     Compressed Sparse Row  to    Block Sparse Row
c-----------------------------------------------------------------------
c
c This  subroutine converts a matrix stored  in a general compressed a,
c ja, ia format into a a block  sparse row format a(m,m,*),ja(*),ia(*).
c See routine  bsrcsr  for more  details on  data   structure for block
c matrices. 
c
c NOTES: 1) the initial matrix does not have to have a block structure. 
c zero padding is done for general sparse matrices. 
c        2) For most practical purposes, na should be the same as m*m.
c 
c-----------------------------------------------------------------------
c 
c In what follows nr=1+(nrow-1)/m = block-row dimension of output matrix 
c 
c on entry:
c----------
c 
c job   =  job indicator.
c          job =  0 -> only the pattern of output matrix is generated 
c          job >  0 -> both pattern and values are generated. 
c          job = -1 -> iao(1) will return the number of nonzero blocks,
c            in the output matrix. In this case jao(1:nr) is used as 
c            workspace, ao is untouched, iao is untouched except iao(1)
c
c nrow	= integer, the actual row dimension of the matrix.
c 
c m     = integer equal to the dimension of each block. m should be > 0. 
c 
c na	= first dimension of array ao as declared in calling program.
c         na should be .ge. m*m. 
c
c a, ja, 
c    ia = input matrix stored in compressed sparse row format.
c
c on return:
c-----------
c 
c ao    = real  array containing the  values of the matrix. For details
c         on the format  see below. Each  row of  a contains the  m x m
c         block matrix  unpacked column-wise (this  allows the  user to
c         declare the  array a as ao(m,m,*) on  entry if desired).  The
c         block rows are stored in sequence  just as for the compressed
c         sparse row format. The block  dimension  of the output matrix
c         is  nr = 1 + (nrow-1) / m.
c         
c jao   = integer array. containing the block-column indices of the 
c         block-matrix. Each jao(k) is an integer between 1 and nr
c         containing the block column index of the block ao(*,k).  
c
c iao   = integer array of length nr+1. iao(i) points to the beginning
c         of block row number i in the arrays ao and jao. When job=-1
c         iao(1) contains the number of nonzero blocks of the output
c         matrix and the rest of iao is unused. This is useful for
c         determining the lengths of ao and jao. 
c
c ierr  = integer, error code. 
c              0 -- normal termination
c              1 -- m is equal to zero 
c              2 -- NA too small to hold the blocks (should be .ge. m**2)
c
c Work arrays:
c------------- 
c iw    = integer work array of dimension  nr = 1 + (nrow-1) / m
c
c NOTES: 
c-------
c     1) this code is not in place.
c     2) see routine bsrcsr for details on data sctructure for block 
c        sparse row format.
c     
c-----------------------------------------------------------------------
c     nr is the block-dimension of the output matrix.
c     
      integer nr, m2, io, ko, ii, len, k, jpos, j, i, ij, jr, irow   
      logical vals  
c----- 
      ierr = 0 
      if (m*m .gt. na) ierr = 2 
      if (m .eq. 0) ierr = 1 
      if (ierr .ne. 0) return
c----------------------------------------------------------------------- 
      vals = (job .gt. 0) 
      nr = 1 + (nrow-1) / m
      m2 = m*m 
      ko = 1 
      io = 1 
      iao(io) = 1 
      len = 0 
c     
c     iw determines structure of block-row (nonzero indicator) 
c 
         do j=1, nr
            iw(j) = 0
         enddo
c     
c     big loop -- leap by m rows each time.
c     
      do ii=1, nrow, m
         irow = 0
c
c     go through next m rows -- make sure not to go beyond nrow. 
c
         do while (ii+irow .le. nrow .and. irow .le. m-1) 
            do k=ia(ii+irow),ia(ii+irow+1)-1
c     
c     block column index = (scalar column index -1) / m + 1 
c
               j = ja(k)-1 
               jr = j/m + 1                
               j = j - (jr-1)*m 
               jpos = iw(jr) 
               if (jpos .eq. 0) then
c
c     create a new block
c     
                  iw(jr) = ko 
                  jao(ko) = jr 
                  if (vals) then
c     
c     initialize new block to zero -- then copy nonzero element
c     
                     do i=1, m2
                        ao(i,ko) = 0.0d0
                     enddo
                     ij = j*m + irow + 1 
                     ao(ij,ko) = a(k) 
                  endif
                  ko = ko+1
               else
c
c     copy column index and nonzero element 
c     
                  jao(jpos) = jr 
                  ij = j*m + irow + 1 
                  if (vals) ao(ij,jpos) = a(k) 
               endif 
            enddo  
            irow = irow+1
         enddo
c     
c     refresh iw
c                      
         do j = iao(io),ko-1 
            iw(jao(j)) = 0
         enddo
         if (job .eq. -1) then
            len = len + ko-1
            ko = 1
         else
            io = io+1 
            iao(io) = ko
         endif
      enddo
      if (job .eq. -1) iao(1) = len
c
      return
c--------------end-of-csrbsr-------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine csrcoo (nrow,job,nzmax,a,ja,ia,nnz,ao,ir,jc,ierr)
c-----------------------------------------------------------------------
      real*8 a(*),ao(*) 
      integer ir(*),jc(*),ja(*),ia(nrow+1) 
c----------------------------------------------------------------------- 
c  Compressed Sparse Row      to      Coordinate 
c----------------------------------------------------------------------- 
c converts a matrix that is stored in coordinate format
c  a, ir, jc into a row general sparse ao, jao, iao format.
c
c on entry: 
c---------
c nrow	= dimension of the matrix.
c job   = integer serving as a job indicator. 
c         if job = 1 fill in only the array ir, ignore jc, and ao.
c         if job = 2 fill in ir, and jc but not ao 
c         if job = 3 fill in everything.
c         The reason why these options are provided is that on return 
c         ao and jc are the same as a, ja. So when job = 3, a and ja are
c         simply copied into ao, jc.  When job=2, only jc and ir are
c         returned. With job=1 only the array ir is returned. Moreover,
c         the algorithm is in place:
c	     call csrcoo (nrow,1,nzmax,a,ja,ia,nnz,a,ia,ja,ierr) 
c         will write the output matrix in coordinate format on a, ja,ia.
c
c a,
c ja,
c ia    = matrix in compressed sparse row format.
c nzmax = length of space available in ao, ir, jc.
c         the code will stop immediatly if the number of
c         nonzero elements found in input matrix exceeds nzmax.
c 
c on return:
c----------- 
c ao, ir, jc = matrix in coordinate format.
c
c nnz        = number of nonzero elements in matrix.
c ierr       = integer error indicator.
c         ierr .eq. 0 means normal retur
c         ierr .eq. 1 means that the the code stopped 
c         because there was no space in ao, ir, jc 
c         (according to the value of  nzmax).
c 
c NOTES: 1)This routine is PARTIALLY in place: csrcoo can be called with 
c         ao being the same array as as a, and jc the same array as ja. 
c         but ir CANNOT be the same as ia. 
c         2) note the order in the output arrays, 
c------------------------------------------------------------------------
      ierr = 0
      nnz = ia(nrow+1)-1
      if (nnz .gt. nzmax) then
         ierr = 1
         return
      endif
c------------------------------------------------------------------------
      goto (3,2,1) job
 1    do 10 k=1,nnz
         ao(k) = a(k)
 10   continue
 2    do 11 k=1,nnz
         jc(k) = ja(k)
 11   continue
c
c     copy backward to allow for in-place processing. 
c
 3    do 13 i=nrow,1,-1
         k1 = ia(i+1)-1
         k2 = ia(i)
         do 12 k=k1,k2,-1
            ir(k) = i
 12      continue
 13   continue
      return
c------------- end-of-csrcoo ------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine csrcsc2 (n,n2,job,ipos,a,ja,ia,ao,jao,iao)
      integer ia(n+1),iao(n2+1),ja(*),jao(*)
      real*8  a(*),ao(*)
c-----------------------------------------------------------------------
c Compressed Sparse Row     to      Compressed Sparse Column
c
c (transposition operation)   Not in place. 
c----------------------------------------------------------------------- 
c Rectangular version.  n is number of rows of CSR matrix,
c                       n2 (input) is number of columns of CSC matrix.
c----------------------------------------------------------------------- 
c -- not in place --
c this subroutine transposes a matrix stored in a, ja, ia format.
c ---------------
c on entry:
c----------
c n	= number of rows of CSR matrix.
c n2    = number of columns of CSC matrix.
c job	= integer to indicate whether to fill the values (job.eq.1) of the
c         matrix ao or only the pattern., i.e.,ia, and ja (job .ne.1)
c
c ipos  = starting position in ao, jao of the transposed matrix.
c         the iao array takes this into account (thus iao(1) is set to ipos.)
c         Note: this may be useful if one needs to append the data structure
c         of the transpose to that of A. In this case use for example
c                call csrcsc2 (n,n,1,ia(n+1),a,ja,ia,a,ja,ia(n+2)) 
c	  for any other normal usage, enter ipos=1.
c a	= real array of length nnz (nnz=number of nonzero elements in input 
c         matrix) containing the nonzero elements.
c ja	= integer array of length nnz containing the column positions
c 	  of the corresponding elements in a.
c ia	= integer of size n+1. ia(k) contains the position in a, ja of
c	  the beginning of the k-th row.
c
c on return:
c ---------- 
c output arguments:
c ao	= real array of size nzz containing the "a" part of the transpose
c jao	= integer array of size nnz containing the column indices.
c iao	= integer array of size n+1 containing the "ia" index array of
c	  the transpose. 
c
c----------------------------------------------------------------------- 
c----------------- compute lengths of rows of transp(A) ----------------
      do 1 i=1,n2+1
         iao(i) = 0
 1    continue
      do 3 i=1, n
         do 2 k=ia(i), ia(i+1)-1 
            j = ja(k)+1
            iao(j) = iao(j)+1
 2       continue 
 3    continue
c---------- compute pointers from lengths ------------------------------
      iao(1) = ipos 
      do 4 i=1,n2
         iao(i+1) = iao(i) + iao(i+1)
 4    continue
c--------------- now do the actual copying ----------------------------- 
      do 6 i=1,n
         do 62 k=ia(i),ia(i+1)-1 
            j = ja(k) 
            next = iao(j)
            if (job .eq. 1)  ao(next) = a(k)
            jao(next) = i
            iao(j) = next+1
 62      continue
 6    continue
c-------------------------- reshift iao and leave ---------------------- 
      do 7 i=n2,1,-1
         iao(i+1) = iao(i)
 7    continue
      iao(1) = ipos
c--------------- end of csrcsc2 ---------------------------------------- 
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine csrcsc (n,job,ipos,a,ja,ia,ao,jao,iao)
      integer ia(n+1),iao(n+1),ja(*),jao(*)
      real*8  a(*),ao(*)
c-----------------------------------------------------------------------
c Compressed Sparse Row     to      Compressed Sparse Column
c
c (transposition operation)   Not in place. 
c----------------------------------------------------------------------- 
c -- not in place --
c this subroutine transposes a matrix stored in a, ja, ia format.
c ---------------
c on entry:
c----------
c n	= dimension of A.
c job	= integer to indicate whether to fill the values (job.eq.1) of the
c         matrix ao or only the pattern., i.e.,ia, and ja (job .ne.1)
c
c ipos  = starting position in ao, jao of the transposed matrix.
c         the iao array takes this into account (thus iao(1) is set to ipos.)
c         Note: this may be useful if one needs to append the data structure
c         of the transpose to that of A. In this case use for example
c                call csrcsc (n,1,ia(n+1),a,ja,ia,a,ja,ia(n+2)) 
c	  for any other normal usage, enter ipos=1.
c a	= real array of length nnz (nnz=number of nonzero elements in input 
c         matrix) containing the nonzero elements.
c ja	= integer array of length nnz containing the column positions
c 	  of the corresponding elements in a.
c ia	= integer of size n+1. ia(k) contains the position in a, ja of
c	  the beginning of the k-th row.
c
c on return:
c ---------- 
c output arguments:
c ao	= real array of size nzz containing the "a" part of the transpose
c jao	= integer array of size nnz containing the column indices.
c iao	= integer array of size n+1 containing the "ia" index array of
c	  the transpose. 
c
c----------------------------------------------------------------------- 
      call csrcsc2 (n,n,job,ipos,a,ja,ia,ao,jao,iao)
      end
c-----------------------------------------------------------------------
      subroutine csrdia (n,idiag,job,a,ja,ia,ndiag,
     *                   diag,ioff,ao,jao,iao,ind)
      real*8 diag(ndiag,idiag), a(*), ao(*)
      integer ia(*), ind(*), ja(*), jao(*), iao(*), ioff(*)
c----------------------------------------------------------------------- 
c Compressed sparse row     to    diagonal format
c----------------------------------------------------------------------- 
c this subroutine extracts  idiag diagonals  from the  input matrix a,
c a, ia, and puts the rest of  the matrix  in the  output matrix ao,
c jao, iao.  The diagonals to be extracted depend  on the  value of job
c (see below for details.)  In  the first  case, the  diagonals to be
c extracted are simply identified by  their offsets  provided in ioff
c by the caller.  In the second case, the  code internally determines
c the idiag most significant diagonals, i.e., those  diagonals of the
c matrix which  have  the  largest  number  of  nonzero elements, and
c extracts them.
c----------------------------------------------------------------------- 
c on entry:
c---------- 
c n	= dimension of the matrix a.
c idiag = integer equal to the number of diagonals to be extracted. 
c         Note: on return idiag may be modified.
c a, ja, 			
c    ia = matrix stored in a, ja, ia, format
c job	= integer. serves as a job indicator.  Job is better thought 
c         of as a two-digit number job=xy. If the first (x) digit
c         is one on entry then the diagonals to be extracted are 
c         internally determined. In this case csrdia exctracts the
c         idiag most important diagonals, i.e. those having the largest
c         number on nonzero elements. If the first digit is zero
c         then csrdia assumes that ioff(*) contains the offsets 
c         of the diagonals to be extracted. there is no verification 
c         that ioff(*) contains valid entries.
c         The second (y) digit of job determines whether or not
c         the remainder of the matrix is to be written on ao,jao,iao.
c         If it is zero  then ao, jao, iao is not filled, i.e., 
c         the diagonals are found  and put in array diag and the rest is
c         is discarded. if it is one, ao, jao, iao contains matrix
c         of the remaining elements.
c         Thus:
c         job= 0 means do not select diagonals internally (pick those
c                defined by ioff) and do not fill ao,jao,iao
c         job= 1 means do not select diagonals internally 
c                      and fill ao,jao,iao
c         job=10 means  select diagonals internally 
c                      and do not fill ao,jao,iao
c         job=11 means select diagonals internally 
c                      and fill ao,jao,iao
c 
c ndiag = integer equal to the first dimension of array diag.
c
c on return:
c----------- 
c
c idiag = number of diagonals found. This may be smaller than its value 
c         on entry. 
c diag  = real array of size (ndiag x idiag) containing the diagonals
c         of A on return
c          
c ioff  = integer array of length idiag, containing the offsets of the
c   	  diagonals to be extracted.
c ao, jao
c  iao  = remainder of the matrix in a, ja, ia format.
c work arrays:
c------------ 
c ind   = integer array of length 2*n-1 used as integer work space.
c         needed only when job.ge.10 i.e., in case the diagonals are to
c         be selected internally.
c
c Notes:
c-------
c    1) The algorithm is in place: ao, jao, iao can be overwritten on 
c       a, ja, ia if desired 
c    2) When the code is required to select the diagonals (job .ge. 10) 
c       the selection of the diagonals is done from left to right 
c       as a result if several diagonals have the same weight (number 
c       of nonzero elemnts) the leftmost one is selected first.
c-----------------------------------------------------------------------
      job1 = job/10
      job2 = job-job1*10
      if (job1 .eq. 0) goto 50
      n2 = n+n-1
      call infdia(n,ja,ia,ind,idum)
c----------- determine diagonals to  accept.---------------------------- 
c----------------------------------------------------------------------- 
      ii = 0
 4    ii=ii+1
      jmax = 0
      do 41 k=1, n2
         j = ind(k)
         if (j .le. jmax) goto 41
         i = k
         jmax = j
 41   continue
      if (jmax .le. 0) then
         ii = ii-1
         goto 42
      endif
      ioff(ii) = i-n
      ind(i) = - jmax
      if (ii .lt.  idiag) goto 4
 42   idiag = ii
c---------------- initialize diago to zero ----------------------------- 
 50   continue
      do 55 j=1,idiag
         do 54 i=1,n
            diag(i,j) = 0.0d0
 54      continue
 55   continue
c----------------------------------------------------------------------- 
      ko = 1
c----------------------------------------------------------------------- 
c extract diagonals and accumulate remaining matrix.
c----------------------------------------------------------------------- 
      do 6 i=1, n
         do 51 k=ia(i),ia(i+1)-1 
            j = ja(k)
            do 52 l=1,idiag
               if (j-i .ne. ioff(l)) goto 52
               diag(i,l) = a(k)
               goto 51
 52         continue
c--------------- append element not in any diagonal to ao,jao,iao ----- 
            if (job2 .eq. 0) goto 51
            ao(ko) = a(k)
            jao(ko) = j
            ko = ko+1
 51      continue
         if (job2 .ne. 0 ) ind(i+1) = ko
 6    continue
      if (job2 .eq. 0) return
c     finish with iao
      iao(1) = 1
      do 7 i=2,n+1
         iao(i) = ind(i)
 7    continue
      return
c----------- end of csrdia ---------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c                    FORMAT CONVERSION MODULE                          c
c----------------------------------------------------------------------c
c contents:                                                            c
c----------                                                            c
c csrdns  : converts a row-stored sparse matrix into the dense format. c
c dnscsr  : converts a dense matrix to a sparse storage format.        c
c coocsr  : converts coordinate to  to csr format                      c
c coicsr  : in-place conversion of coordinate to csr format            c
c csrcoo  : converts compressed sparse row to coordinate.              c
c csrssr  : converts compressed sparse row to symmetric sparse row     c
c ssrcsr  : converts symmetric sparse row to compressed sparse row     c
c csrell  : converts compressed sparse row to ellpack format           c
c ellcsr  : converts ellpack format to compressed sparse row format    c
c csrmsr  : converts compressed sparse row format to modified sparse   c
c           row format                                                 c
c msrcsr  : converts modified sparse row format to compressed sparse   c
c           row format.                                                c
c csrcsc  : converts compressed sparse row format to compressed sparse c
c           column format (transposition)                              c
c csrcsc2 : rectangular version of csrcsc                              c
c csrlnk  : converts compressed sparse row to linked list format       c
c lnkcsr  : converts linked list format to compressed sparse row fmt   c
c csrdia  : converts a compressed sparse row format into a diagonal    c
c           format.                                                    c
c diacsr  : converts a diagonal format into a compressed sparse row    c
c           format.                                                    c
c bsrcsr  : converts a block-row sparse format into a compressed       c
c           sparse row format.                                         c
c csrbsr  : converts a compressed sparse row format into a block-row   c
c           sparse format.                                             c
c csrbnd  : converts a compressed sparse row format into a banded      c
c           format (linpack style).                                    c
c bndcsr  : converts a banded format (linpack style) into a compressed c
c           sparse row storage.                                        c
c csrssk  : converts the compressed sparse row format to the symmetric c
c           skyline format                                             c
c sskssr  : converts symmetric skyline format to symmetric  sparse row c
c           format.                                                    c
c csrjad  : converts the csr format into the jagged diagonal format    c
c jadcsr  : converts the jagged-diagonal format into the csr format    c
c csruss  : Compressed Sparse Row to Unsymmetric Sparse Skyline        c
c           format                                                     c
c usscsr  : Unsymmetric Sparse Skyline format to Compressed Sparse Row c
c csrsss  : Compressed Sparse Row to Symmetric Sparse Skyline format   c
c ssscsr  : Symmetric Sparse Skyline format to Compressed Sparse Row   c
c csrvbr  : Converts compressed sparse row to var block row format     c
c vbrcsr  : Converts var block row to compressed sparse row format     c
c csorted : Checks if matrix in CSR format is sorted by columns        c
c--------- miscalleneous additions not involving the csr format--------c
c cooell  : converts coordinate to Ellpack/Itpack format               c
c dcsort  : sorting routine used by crsjad                             c
c----------------------------------------------------------------------c
      subroutine csrdns(nrow,ncol,a,ja,ia,dns,ndns,ierr) 
      real*8 dns(ndns,*),a(*)
      integer ja(*),ia(*)
c-----------------------------------------------------------------------
c Compressed Sparse Row    to    Dense 
c-----------------------------------------------------------------------
c
c converts a row-stored sparse matrix into a densely stored one
c
c On entry:
c---------- 
c
c nrow	= row-dimension of a
c ncol	= column dimension of a
c a, 
c ja, 
c ia    = input matrix in compressed sparse row format. 
c         (a=value array, ja=column array, ia=pointer array)
c dns   = array where to store dense matrix
c ndns	= first dimension of array dns 
c
c on return: 
c----------- 
c dns   = the sparse matrix a, ja, ia has been stored in dns(ndns,*)
c 
c ierr  = integer error indicator. 
c         ierr .eq. 0  means normal return
c         ierr .eq. i  means that the code has stopped when processing
c         row number i, because it found a column number .gt. ncol.
c 
c----------------------------------------------------------------------- 
      ierr = 0
      do 1 i=1, nrow
         do 2 j=1,ncol
	    dns(i,j) = 0.0d0
 2       continue
 1    continue
c     
      do 4 i=1,nrow
         do 3 k=ia(i),ia(i+1)-1
            j = ja(k) 
	    if (j .gt. ncol) then
               ierr = i
               return
	    endif
	    dns(i,j) = a(k)
 3       continue	   
 4    continue
      return
c---- end of csrdns ----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrell (nrow,a,ja,ia,maxcol,coef,jcoef,ncoef,
     *                   ndiag,ierr)
      integer ia(nrow+1), ja(*), jcoef(ncoef,1)  
      real*8 a(*), coef(ncoef,1)
c----------------------------------------------------------------------- 
c Compressed Sparse Row	    to    Ellpack - Itpack format 
c----------------------------------------------------------------------- 
c this subroutine converts  matrix stored in the general a, ja, ia 
c format into the coef, jcoef itpack format.
c
c----------------------------------------------------------------------- 
c on entry:
c---------- 
c nrow 	  = row dimension of the matrix A.
c
c a, 
c ia, 
c ja      = input matrix in compressed sparse row format. 
c
c ncoef  = first dimension of arrays coef, and jcoef.
c 
c maxcol = integer equal to the number of columns available in coef.
c
c on return: 
c----------
c coef	= real array containing the values of the matrix A in 
c         itpack-ellpack format.
c jcoef = integer array containing the column indices of coef(i,j) 
c         in A.
c ndiag = number of active 'diagonals' found. 
c
c ierr 	= error message. 0 = correct return. If ierr .ne. 0 on
c	  return this means that the number of diagonals found
c         (ndiag) exceeds maxcol.
c
c----------------------------------------------------------------------- 
c first determine the length of each row of lower-part-of(A)
      ierr = 0
      ndiag = 0
      do 3 i=1, nrow
         k = ia(i+1)-ia(i)
         ndiag = max0(ndiag,k) 
 3    continue
c----- check whether sufficient columns are available. ----------------- 
      if (ndiag .gt. maxcol) then
         ierr = 1 
         return
      endif
c
c fill coef with zero elements and jcoef with row numbers.------------ 
c
      do 4 j=1,ndiag 
         do 41 i=1,nrow
            coef(i,j) = 0.0d0
            jcoef(i,j) = i
 41      continue
 4    continue
c     
c------- copy elements row by row.-------------------------------------- 
c     
      do 6 i=1, nrow
         k1 = ia(i)
         k2 = ia(i+1)-1
         do 5 k=k1,k2
            coef(i,k-k1+1) = a(k)
            jcoef(i,k-k1+1) = ja(k)
 5       continue
 6    continue
      return
c--- end of csrell------------------------------------------------------ 
c----------------------------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine csrjad (nrow, a, ja, ia, idiag, iperm, ao, jao, iao) 
      integer ja(*), jao(*), ia(nrow+1), iperm(nrow), iao(nrow) 
      real*8 a(*), ao(*)
c-----------------------------------------------------------------------
c    Compressed Sparse Row  to   JAgged Diagonal storage. 
c----------------------------------------------------------------------- 
c this subroutine converts  matrix stored in the compressed sparse
c row format to the jagged diagonal format. The data structure
c for the JAD (Jagged Diagonal storage) is as follows. The rows of 
c the matrix are (implicitly) permuted so that their lengths are in
c decreasing order. The real entries ao(*) and their column indices 
c jao(*) are stored in succession. The number of such diagonals is idiag.
c the lengths of each of these diagonals is stored in iao(*).
c For more details see [E. Anderson and Y. Saad,
c ``Solving sparse triangular systems on parallel computers'' in
c Inter. J. of High Speed Computing, Vol 1, pp. 73-96 (1989).]
c or  [Y. Saad, ``Krylov Subspace Methods on Supercomputers''
c SIAM J. on  Stat. Scient. Comput., volume 10, pp. 1200-1232 (1989).]
c----------------------------------------------------------------------- 
c on entry:
c---------- 
c nrow 	  = row dimension of the matrix A.
c
c a, 
c ia, 
c ja      = input matrix in compressed sparse row format. 
c
c on return: 
c----------
c 
c idiag = integer. The number of jagged diagonals in the matrix.
c
c iperm = integer array of length nrow containing the permutation
c         of the rows that leads to a decreasing order of the
c         number of nonzero elements.
c
c ao    = real array containing the values of the matrix A in 
c         jagged diagonal storage. The j-diagonals are stored
c         in ao in sequence. 
c
c jao   = integer array containing the column indices of the 
c         entries in ao.
c
c iao   = integer array containing pointers to the beginning 
c         of each j-diagonal in ao, jao. iao is also used as 
c         a work array and it should be of length n at least.
c
c----------------------------------------------------------------------- 
c     ---- define initial iperm and get lengths of each row
c     ---- jao is used a work vector to store tehse lengths
c     
      idiag = 0
      ilo = nrow 
      do 10 j=1, nrow
         iperm(j) = j 
         len = ia(j+1) - ia(j)
         ilo = min(ilo,len) 
         idiag = max(idiag,len) 
         jao(j) = len
 10   continue 
c     
c     call sorter to get permutation. use iao as work array.
c    
      call dcsort (jao, nrow, iao, iperm, ilo, idiag) 
c     
c     define output data structure. first lengths of j-diagonals
c     
      do 20 j=1, nrow
         iao(j) = 0
 20   continue
      do 40 k=1, nrow
         len = jao(iperm(k)) 
         do 30 i=1,len
            iao(i) = iao(i)+1
 30      continue
 40   continue
c     
c     get the output matrix itself
c     
      k1 = 1
      k0 = k1
      do 60 jj=1, idiag
         len = iao(jj)
         do 50 k=1,len
            i = ia(iperm(k))+jj-1
            ao(k1) = a(i)
            jao(k1) = ja(i) 
            k1 = k1+1
 50      continue
         iao(jj) = k0
         k0 = k1
 60   continue
      iao(idiag+1) = k1
      return
c----------end-of-csrjad------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
c------------------------end-of-csrkvstr--------------------------------
      subroutine csrkvstc(n, ia, ja, nc, kvstc, iwk)
c-----------------------------------------------------------------------
      integer n, ia(n+1), ja(*), nc, kvstc(*), iwk(*)
c-----------------------------------------------------------------------
c     Finds block column partitioning of matrix in CSR format.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     n       = number of matrix scalar rows
c     ia,ja   = input matrix sparsity structure in CSR format
c
c     On return:
c---------------
c     nc      = number of block columns
c     kvstc   = first column number for each block column
c
c     Work space:
c----------------
c     iwk(*) of size equal to the number of scalar columns plus one.
c        Assumed initialized to 0, and left initialized on return.
c
c     Notes:
c-----------
c     Assumes that the matrix is sorted by columns.
c
c-----------------------------------------------------------------------
c     local variables
      integer i, j, k, ncol
c
c-----------------------------------------------------------------------
c-----use ncol to find maximum scalar column number
      ncol = 0
c-----mark the beginning position of the blocks in iwk
      do i = 1, n
         if (ia(i) .lt. ia(i+1)) then
            j = ja(ia(i))
            iwk(j) = 1
            do k = ia(i)+1, ia(i+1)-1
               j = ja(k)
               if (ja(k-1).ne.j-1) then
                  iwk(j) = 1
                  iwk(ja(k-1)+1) = 1
               endif
            enddo
            iwk(j+1) = 1
            ncol = max0(ncol, j)
         endif
      enddo
c---------------------------------
      nc = 1
      kvstc(1) = 1
      do i = 2, ncol+1
         if (iwk(i).ne.0) then
            nc = nc + 1
            kvstc(nc) = i
            iwk(i) = 0
         endif
      enddo
      nc = nc - 1
c---------------------------------
      return
      end
c-----------------------------------------------------------------------
      subroutine csrkvstr(n, ia, ja, nr, kvstr)
c-----------------------------------------------------------------------
      integer n, ia(n+1), ja(*), nr, kvstr(*)
c-----------------------------------------------------------------------
c     Finds block row partitioning of matrix in CSR format.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     n       = number of matrix scalar rows
c     ia,ja   = input matrix sparsity structure in CSR format
c
c     On return:
c---------------
c     nr      = number of block rows
c     kvstr   = first row number for each block row
c
c     Notes:
c-----------
c     Assumes that the matrix is sorted by columns.
c     This routine does not need any workspace.
c
c-----------------------------------------------------------------------
c     local variables
      integer i, j, jdiff
c-----------------------------------------------------------------------
      nr = 1
      kvstr(1) = 1
c---------------------------------
      do i = 2, n
         jdiff = ia(i+1)-ia(i)
         if (jdiff .eq. ia(i)-ia(i-1)) then
            do j = ia(i), ia(i+1)-1
               if (ja(j) .ne. ja(j-jdiff)) then
                  nr = nr + 1
                  kvstr(nr) = i
                  goto 299
               endif
            enddo
 299        continue
         else
 300        nr = nr + 1
            kvstr(nr) = i
         endif
      enddo
      kvstr(nr+1) = n+1
c---------------------------------
      return
      end
c-----------------------------------------------------------------------
      subroutine csrlnk (n,a,ja,ia,link) 
      real*8 a(*) 
      integer n, ja(*), ia(n+1), link(*)
c----------------------------------------------------------------------- 
c      Compressed Sparse Row         to    Linked storage format. 
c----------------------------------------------------------------------- 
c this subroutine translates a matrix stored in compressed sparse
c row into one with a linked list storage format. Only the link
c array needs to be obtained since the arrays a, ja, and ia may
c be unchanged and  carry the same meaning for the output matrix.
c in  other words a, ja, ia, link   is the output linked list data
c structure with a, ja, unchanged from input, and ia possibly 
c altered (in case therea re null rows in matrix). Details on
c the output array link are given below.
c----------------------------------------------------------------------- 
c Coded by Y. Saad, Feb 21, 1991.
c----------------------------------------------------------------------- 
c
c on entry:
c----------
c n	= integer equal to the dimension of A.	
c         
c a	= real array of size nna containing the nonzero elements
c ja	= integer array of size	nnz containing the column positions
c 	  of the corresponding elements in a.
c ia	= integer of size n+1 containing the pointers to the beginning 
c         of each row. ia(k) contains the position in a, ja of the 
c         beginning of the k-th row.
c
c on return:
c---------- 
c a, ja, are not changed.
c ia    may be changed if there are null rows.
c 
c a     = nonzero elements.
c ja    = column positions. 
c ia    = ia(i) points to the first element of row i in linked structure.
c link	= integer array of size containing the linked list information.
c         link(k) points to the next element of the row after element 
c         a(k), ja(k). if link(k) = 0, then there is no next element,
c         i.e., a(k), jcol(k) is the last element of the current row.
c
c  Thus row number i can be accessed as follows:
c     next = ia(i) 
c     while(next .ne. 0) do 
c          value = a(next)      ! value a(i,j) 
c          jcol  = ja(next)     ! column index j
c          next  = link(next)   ! address of next element in row
c     endwhile
c notes:
c ------ ia may be altered on return.
c----------------------------------------------------------------------- 
c local variables
      integer i, k
c
c loop through all rows
c
      do 100 i =1, n
         istart = ia(i) 
         iend = ia(i+1)-1
         if (iend .gt. istart) then
            do 99  k=istart, iend-1 
               link(k) = k+1
 99         continue
            link(iend) = 0
         else
            ia(i) = 0
         endif
 100  continue
c     
      return
c-------------end-of-csrlnk --------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrmsr (n,a,ja,ia,ao,jao,wk,iwk)
      real*8 a(*),ao(*),wk(n)
      integer ia(n+1),ja(*),jao(*),iwk(n+1)
c----------------------------------------------------------------------- 
c Compressed Sparse Row   to      Modified - Sparse Row 
c                                 Sparse row with separate main diagonal
c----------------------------------------------------------------------- 
c converts a general sparse matrix a, ja, ia into 
c a compressed matrix using a separated diagonal (referred to as
c the bell-labs format as it is used by bell labs semi conductor
c group. We refer to it here as the modified sparse row format.
c Note: this has been coded in such a way that one can overwrite
c the output matrix onto the input matrix if desired by a call of
c the form 
c
c     call csrmsr (n, a, ja, ia, a, ja, wk,iwk)
c
c In case ao, jao, are different from a, ja, then one can
c use ao, jao as the work arrays in the calling sequence:
c
c     call csrmsr (n, a, ja, ia, ao, jao, ao,jao)
c
c----------------------------------------------------------------------- 
c
c on entry :
c---------
c a, ja, ia = matrix in csr format. note that the 
c	     algorithm is in place: ao, jao can be the same
c            as a, ja, in which case it will be overwritten on it
c            upon return.
c	 
c on return :
c-----------
c
c ao, jao  = sparse matrix in modified sparse row storage format:
c	   +  ao(1:n) contains the diagonal of the matrix. 
c	   +  ao(n+2:nnz) contains the nondiagonal elements of the
c             matrix, stored rowwise.
c	   +  jao(n+2:nnz) : their column indices
c	   +  jao(1:n+1) contains the pointer array for the nondiagonal
c             elements in ao(n+1:nnz) and jao(n+2:nnz).
c             i.e., for i .le. n+1 jao(i) points to beginning of row i 
c	      in arrays ao, jao.
c	       here nnz = number of nonzero elements+1 
c work arrays:
c------------
c wk	= real work array of length n
c iwk   = integer work array of length n+1
c
c notes: 
c------- 
c        Algorithm is in place.  i.e. both:
c
c          call csrmsr (n, a, ja, ia, ao, jao, ao,jao)
c          (in which  ao, jao, are different from a, ja)
c           and
c          call csrmsr (n, a, ja, ia, a, ja, wk,iwk) 
c          (in which  wk, jwk, are different from a, ja)
c        are OK.
c--------
c coded by Y. Saad Sep. 1989. Rechecked Feb 27, 1990.
c-----------------------------------------------------------------------
      icount = 0
c
c store away diagonal elements and count nonzero diagonal elements.
c
      do 1 i=1,n
         wk(i) = 0.0d0
         iwk(i+1) = ia(i+1)-ia(i)
         do 2 k=ia(i),ia(i+1)-1
            if (ja(k) .eq. i) then
               wk(i) = a(k)
               icount = icount + 1 
               iwk(i+1) = iwk(i+1)-1
            endif
 2       continue
 1    continue
c     
c compute total length
c     
      iptr = n + ia(n+1) - icount
c     
c     copy backwards (to avoid collisions)
c     
      do 500 ii=n,1,-1
         do 100 k=ia(ii+1)-1,ia(ii),-1
            j = ja(k)
            if (j .ne. ii) then
               ao(iptr) = a(k)
               jao(iptr) = j 
               iptr = iptr-1
            endif
 100     continue
 500  continue
c
c compute pointer values and copy wk(*)
c
      jao(1) = n+2
      do 600 i=1,n
         ao(i) = wk(i) 
         jao(i+1) = jao(i)+iwk(i+1)
 600  continue
      return	
c------------ end of subroutine csrmsr ---------------------------------
c----------------------------------------------------------------------- 
      end
c
      subroutine  csrot (n,cx,incx,cy,incy,c,s)
c
c     applies a plane rotation, where the cos and sin (c and s) are real
c     and the vectors cx and cy are complex.
c     jack dongarra, linpack, 3/11/78.
c
      complex cx(1),cy(1),ctemp
      real c,s
      integer i,incx,incy,ix,iy,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        ctemp = c*cx(ix) + s*cy(iy)
        cy(iy) = c*cy(iy) - s*cx(ix)
        cx(ix) = ctemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
   20 do 30 i = 1,n
        ctemp = c*cx(i) + s*cy(i)
        cy(i) = c*cy(i) - s*cx(i)
        cx(i) = ctemp
   30 continue
      return
      end
c----------------------------------------------------------------------- 
      subroutine csrssk (n,imod,a,ja,ia,asky,isky,nzmax,ierr)
      real*8 a(*),asky(nzmax) 
      integer n, imod, nzmax, ierr, ia(n+1), isky(n+1), ja(*)
c----------------------------------------------------------------------- 
c      Compressed Sparse Row         to     Symmetric Skyline Format 
c  or  Symmetric Sparse Row        
c----------------------------------------------------------------------- 
c this subroutine translates a compressed sparse row or a symmetric
c sparse row format into a symmetric skyline format.
c the input matrix can be in either compressed sparse row or the 
c symmetric sparse row format. The output matrix is in a symmetric
c skyline format: a real array containing the (active portions) of the
c rows in  sequence and a pointer to the beginning of each row.
c
c This module is NOT  in place.
c----------------------------------------------------------------------- 
c Coded by Y. Saad, Oct 5, 1989. Revised Feb. 18, 1991.
c----------------------------------------------------------------------- 
c
c on entry:
c----------
c n	= integer equal to the dimension of A.	
c imod  = integer indicating the variant of skyline format wanted:
c         imod = 0 means the pointer isky points to the `zeroth' 
c         element of the row, i.e., to the position of the diagonal
c         element of previous row (for i=1, isky(1)= 0)
c         imod = 1 means that itpr points to the beginning of the row. 
c         imod = 2 means that isky points to the end of the row (diagonal
c                  element) 
c         
c a	= real array of size nna containing the nonzero elements
c ja	= integer array of size	nnz containing the column positions
c 	  of the corresponding elements in a.
c ia	= integer of size n+1. ia(k) contains the position in a, ja of
c	  the beginning of the k-th row.
c nzmax = integer. must be set to the number of available locations
c         in the output array asky. 
c
c on return:
c---------- 
c
c asky    = real array containing the values of the matrix stored in skyline
c         format. asky contains the sequence of active rows from 
c         i=1, to n, an active row being the row of elemnts of 
c         the matrix contained between the leftmost nonzero element 
c         and the diagonal element. 
c isky	= integer array of size n+1 containing the pointer array to 
c         each row. The meaning of isky depends on the input value of
c         imod (see above). 
c ierr  =  integer.  Error message. If the length of the 
c         output array asky exceeds nzmax. ierr returns the minimum value
c         needed for nzmax. otherwise ierr=0 (normal return).
c 
c Notes:
c         1) This module is NOT  in place.
c         2) even when imod = 2, length of  isky is  n+1, not n.
c
c----------------------------------------------------------------------- 
c first determine individial bandwidths and pointers.
c----------------------------------------------------------------------- 
      ierr = 0
      isky(1) = 0
      do 3 i=1,n
         ml = 0
         do 31 k=ia(i),ia(i+1)-1 
            ml = max(ml,i-ja(k)+1) 
 31      continue
         isky(i+1) = isky(i)+ml
 3    continue
c
c     test if there is enough space  asky to do the copying.  
c
      nnz = isky(n+1) 
      if (nnz .gt. nzmax) then
         ierr = nnz
         return
      endif
c    
c   fill asky with zeros.
c     
      do 1 k=1, nnz 
         asky(k) = 0.0d0
 1    continue
c     
c     copy nonzero elements.
c     
      do 4 i=1,n
         kend = isky(i+1) 
         do 41 k=ia(i),ia(i+1)-1 
            j = ja(k)
            if (j .le. i) asky(kend+j-i) = a(k)
 41      continue
 4    continue
c 
c modify pointer according to imod if necessary.
c
      if (imod .eq. 0) return
      if (imod .eq. 1) then 
         do 50 k=1, n+1
            isky(k) = isky(k)+1
 50      continue
      endif
      if (imod .eq. 2) then
         do 60 k=1, n
            isky(k) = isky(k+1) 
 60      continue
      endif
c
      return
c------------- end of csrssk ------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------- 
      subroutine csrssr (nrow,a,ja,ia,nzmax,ao,jao,iao,ierr)
      real*8 a(*), ao(*), t
      integer ia(*), ja(*), iao(*), jao(*)
c-----------------------------------------------------------------------
c Compressed Sparse Row     to     Symmetric Sparse Row
c----------------------------------------------------------------------- 
c this subroutine extracts the lower triangular part of a matrix.
c It can used as a means for converting a symmetric matrix for 
c which all the entries are stored in sparse format into one
c in which only the lower part is stored. The routine is in place in 
c that the output matrix ao, jao, iao can be overwritten on 
c the  input matrix  a, ja, ia if desired. Csrssr has been coded to
c put the diagonal elements of the matrix in the last position in
c each row (i.e. in position  ao(ia(i+1)-1   of ao and jao) 
c----------------------------------------------------------------------- 
c On entry
c-----------
c nrow  = dimension of the matrix a.
c a, ja, 
c    ia = matrix stored in compressed row sparse format
c
c nzmax = length of arrays ao,  and jao. 
c
c On return:
c----------- 
c ao, jao, 
c     iao = lower part of input matrix (a,ja,ia) stored in compressed sparse 
c          row format format.
c  
c ierr   = integer error indicator. 
c          ierr .eq. 0  means normal return
c          ierr .eq. i  means that the code has stopped when processing
c          row number i, because there is not enough space in ao, jao
c          (according to the value of nzmax) 
c
c----------------------------------------------------------------------- 
      ierr = 0
      ko = 0
c-----------------------------------------------------------------------
      do  7 i=1, nrow
         kold = ko
         kdiag = 0
         do 71 k = ia(i), ia(i+1) -1
            if (ja(k)  .gt. i) goto 71
            ko = ko+1
            if (ko .gt. nzmax) then
               ierr = i
               return
            endif
            ao(ko) = a(k)
            jao(ko) = ja(k)
            if (ja(k)  .eq. i) kdiag = ko
 71      continue
         if (kdiag .eq. 0 .or. kdiag .eq. ko) goto 72
c     
c     exchange
c     
         t = ao(kdiag)
         ao(kdiag) = ao(ko)
         ao(ko) = t
c     
         k = jao(kdiag)
         jao(kdiag) = jao(ko)
         jao(ko) = k
 72      iao(i) = kold+1
 7    continue
c     redefine iao(n+1)
      iao(nrow+1) = ko+1
      return
c--------- end of csrssr ----------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------- 
      subroutine csrsss (nrow,a,ja,ia,sorted,diag,al,jal,ial,au)
      real*8 a(*),al(*),diag(*),au(*) 
      integer ja(*),ia(nrow+1),jal(*),ial(nrow+1)
      logical sorted 
c-----------------------------------------------------------------------
c Compressed Sparse Row     to     Symmetric Sparse Skyline   format 
c----------------------------------------------------------------------- 
c this subroutine converts a matrix stored in csr format into the 
c Symmetric sparse skyline   format. This latter format assumes that 
c that the matrix has a symmetric pattern. It consists of the following 
c * the diagonal of A stored separately in diag(*);
c * The strict lower part of A is stored  in csr format in al,jal,ial 
c * The values only of strict upper part as stored in csc format in au. 
c----------------------------------------------------------------------- 
c On entry
c-----------
c nrow  = dimension of the matrix a.
c a     = real array containing the nonzero values of the matrix 
c         stored rowwise.
c ja    = column indices of the values in array a
c ia    = integer array of length n+1 containing the pointers to
c         beginning of each row in arrays a, ja.
c sorted= a logical indicating whether or not the elements in a,ja,ia
c         are sorted. 
c 
c On return
c --------- 
c diag  = array containing the diagonal entries of A
c al,jal,ial = matrix in csr format storing the strict lower 
c              trangular part of A.
c au    = values of the strict upper trangular part of A, column wise.
c----------------------------------------------------------------------- 
c 
c     extract lower part and diagonal.
c
      kl = 1
      ial(1) = kl
      do  7 i=1, nrow
c
c scan all elements in a row
c 
         do 71 k = ia(i), ia(i+1)-1
            jak = ja(k) 
            if (jak  .eq. i) then
               diag(i) = a(k) 
            elseif (jak .lt. i) then
               al(kl) = a(k)
               jal(kl) = jak
               kl = kl+1
            endif
 71      continue
         ial(i+1) = kl 
 7    continue
c
c sort if not sorted
c 
      if (.not. sorted) then
c%%%%%---- incompatible arg list! 
         call csort (nrow, al, jal, ial, au, .true.) 
      endif
c
c copy u
c 
      do  8 i=1, nrow
c
c scan all elements in a row
c 
         do 81 k = ia(i), ia(i+1)-1
            jak = ja(k) 
            if (jak  .gt. i) then
               ku = ial(jak) 
               au(ku) = a(k)
               ial(jak) = ku+1
            endif
 81      continue
 8    continue
c   
c readjust ial
c
      do 9 i=nrow,1,-1
         ial(i+1) = ial(i)
 9    continue
      ial(1) = 1
c--------------- end-of-csrsss ----------------------------------------- 
c-----------------------------------------------------------------------
      end 
c----------------------------------------------------------------------- 
      subroutine csruss (nrow,a,ja,ia,diag,al,jal,ial,au,jau,iau) 
      real*8 a(*),al(*),diag(*),au(*) 
      integer nrow,ja(*),ia(nrow+1),jal(*),ial(nrow+1),jau(*),
     *     iau(nrow+1)
c-----------------------------------------------------------------------
c Compressed Sparse Row     to     Unsymmetric Sparse Skyline format
c----------------------------------------------------------------------- 
c this subroutine converts a matrix stored in csr format into a nonsym. 
c sparse skyline format. This latter format does not assume
c that the matrix has a symmetric pattern and consists of the following 
c * the diagonal of A stored separately in diag(*);
c * The strict lower part of A is stored  in CSR format in al,jal,ial 
c * The strict upper part is stored in CSC format in au,jau,iau.
c----------------------------------------------------------------------- 
c On entry
c---------
c nrow  = dimension of the matrix a.
c a     = real array containing the nonzero values of the matrix 
c         stored rowwise.
c ja    = column indices of the values in array a
c ia    = integer array of length n+1 containing the pointers to
c         beginning of each row in arrays a, ja.
c 
c On return
c----------
c diag  = array containing the diagonal entries of A
c al,jal,ial = matrix in CSR format storing the strict lower 
c              trangular part of A.
c au,jau,iau = matrix in CSC format storing the strict upper
c              triangular part of A. 
c----------------------------------------------------------------------- 
      integer i, j, k, kl, ku 
c
c determine U's data structure first
c 
      do 1 i=1,nrow+1
         iau(i) = 0
 1    continue
      do 3 i=1, nrow
         do 2 k=ia(i), ia(i+1)-1 
            j = ja(k)
            if (j .gt. i) iau(j+1) = iau(j+1)+1
 2       continue 
 3    continue
c
c     compute pointers from lengths
c
      iau(1) = 1
      do 4 i=1,nrow
         iau(i+1) = iau(i)+iau(i+1)
         ial(i+1) = ial(i)+ial(i+1)
 4    continue
c
c     now do the extractions. scan all rows.
c
      kl = 1
      ial(1) = kl
      do  7 i=1, nrow
c
c     scan all elements in a row
c 
         do 71 k = ia(i), ia(i+1)-1
            j = ja(k) 
c
c     if in upper part, store in row j (of transp(U) )
c     
            if (j  .gt. i) then
               ku = iau(j) 
               au(ku) = a(k)
               jau(ku) = i
               iau(j) = ku+1
            elseif (j  .eq. i) then
               diag(i) = a(k) 
            elseif (j .lt. i) then
               al(kl) = a(k)
               jal(kl) = j
               kl = kl+1
            endif
 71      continue
         ial(i+1) = kl 
 7    continue
c
c readjust iau
c
      do 8 i=nrow,1,-1
         iau(i+1) = iau(i)
 8    continue
      iau(1) = 1
c--------------- end-of-csruss ----------------------------------------- 
c-----------------------------------------------------------------------
      end 
c-----------------------------------------------------------------------
      subroutine csrvbr(n,ia,ja,a,nr,nc,kvstr,kvstc,ib,jb,kb,
     &     b, job, iwk, nkmax, nzmax, ierr )
c-----------------------------------------------------------------------
      integer n, ia(n+1), ja(*), nr, nc, ib(*), jb(nkmax-1), kb(nkmax)
      integer kvstr(*), kvstc(*), job, iwk(*), nkmax, nzmax, ierr
      real*8  a(*), b(nzmax)
c-----------------------------------------------------------------------
c     Converts compressed sparse row to variable block row format.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     n       = number of matrix rows
c     ia,ja,a = input matrix in CSR format
c
c     job     = job indicator.
c               If job=0, kvstr and kvstc are used as supplied.
c               If job=1, kvstr and kvstc are determined by the code.
c               If job=2, a conformal row/col partitioning is found and
c               returned in both kvstr and kvstc.  In the latter two cases,
c               an optimized algorithm can be used to perform the
c               conversion because all blocks are full.
c
c     nkmax   = size of supplied jb and kb arrays
c     nzmax   = size of supplied b array
c
c     If job=0 then the following are input:
c     nr,nc   = matrix block row and block column dimension
c     kvstr   = first row number for each block row
c     kvstc   = first column number for each block column.
c               (kvstr and kvstc may be the same array)
c
c     On return:
c---------------
c
c     ib,jb,kb,b = output matrix in VBR format
c
c     ierr    = error message
c               ierr = 0 means normal return
c               ierr = 1 out of space in jb and/or kb arrays
c               ierr = 2 out of space in b array
c               ierr = 3 nonsquare matrix used with job=2
c
c     If job=1,2 then the following are output:
c     nr,nc   = matrix block row and block column dimension
c     kvstr   = first row number for each block row
c     kvstc   = first column number for each block column
c               If job=2, then kvstr and kvstc contain the same info.
c
c     Work space:
c----------------
c     iwk(1:ncol) = inverse kvstc array.  If job=1,2 then we also need:
c     iwk(ncol+1:ncol+nr) = used to help determine sparsity of each block row.
c     The workspace is not assumed to be initialized to zero, nor is it
c     left that way.
c
c     Algorithms:
c----------------
c     There are two conversion codes in this routine.  The first assumes
c     that all blocks are full (there is a nonzero in the CSR data
c     structure for each entry in the block), and is used if the routine
c     determines the block partitioning itself.  The second code makes
c     no assumptions about the block partitioning, and is used if the
c     caller provides the partitioning.  The second code is much less
c     efficient than the first code.
c
c     In the first code, the CSR data structure is traversed sequentially
c     and entries are placed into the VBR data structure with stride
c     equal to the row dimension of the block row.  The columns of the
c     CSR data structure are sorted first if necessary.
c
c     In the second code, the block sparsity pattern is first determined.
c     This is done by traversing the CSR data structure and using an
c     implied linked list to determine which blocks are nonzero.  Then
c     the VBR data structure is filled by mapping each individual entry
c     in the CSR data structure into the VBR data structure.  The columns
c     of the CSR data structure are sorted first if necessary.
c
c-----------------------------------------------------------------------
c     Local variables:
c---------------------
      integer ncol, nb, neqr, numc, a0, b0, b1, k0, i, ii, j, jj, jnew
      logical sorted
c
c     ncol = number of scalar columns in matrix
c     nb = number of blocks in conformal row/col partitioning
c     neqr = number of rows in block row
c     numc = number of nonzero columns in row
c     a0 = index for entries in CSR a array
c     b0 = index for entries in VBR b array
c     b1 = temp
c     k0 = index for entries in VBR kb array
c     i  = loop index for block rows
c     ii = loop index for scalar rows in block row
c     j  = loop index for block columns
c     jj = loop index for scalar columns in block column
c     jnew = block column number
c     sorted = used to indicate if matrix already sorted by columns
c
c-----------------------------------------------------------------------
      ierr = 0
c-----sort matrix by column indices
      call csorted(n, ia, ja, sorted)
      if (.not. sorted) then
         call csort (n, a, ja, ia, b, .true.)
      endif
      if (job .eq. 1 .or. job .eq. 2) then
c--------need to zero workspace; first find ncol
         ncol = 0
         do i = 2, n
            ncol = max0(ncol, ja(ia(i)-1))
         enddo
         do i = 1, ncol
            iwk(i) = 0
         enddo
         call csrkvstr(n, ia, ja, nr, kvstr)
         call csrkvstc(n, ia, ja, nc, kvstc, iwk)
      endif
c-----check if want conformal partitioning
      if (job .eq. 2) then
         if (kvstr(nr+1) .ne. kvstc(nc+1)) then
            ierr = 3
            return
         endif
c        use iwk temporarily
         call kvstmerge(nr, kvstr, nc, kvstc, nb, iwk)
         nr = nb
         nc = nb
         do i = 1, nb+1
            kvstr(i) = iwk(i)
            kvstc(i) = iwk(i)
         enddo
      endif
c-----------------------------------------------------------------------
c     inverse kvst (scalar col number) = block col number
c     stored in iwk(1:n)
c-----------------------------------------------------------------------
      do i = 1, nc
         do j = kvstc(i), kvstc(i+1)-1
            iwk(j) = i
         enddo
      enddo
      ncol = kvstc(nc+1)-1
c-----jump to conversion routine
      if (job .eq. 0) goto 400
c-----------------------------------------------------------------------
c     Fast conversion for computed block partitioning
c-----------------------------------------------------------------------
      a0 = 1
      b0 = 1
      k0 = 1
      kb(1) = 1
c-----loop on block rows
      do i = 1, nr
         neqr = kvstr(i+1) - kvstr(i)
         numc = ia(kvstr(i)+1) - ia(kvstr(i))
         ib(i) = k0
c--------loop on first row in block row to determine block sparsity
         j = 0
         do jj = ia(kvstr(i)), ia(kvstr(i)+1)-1
            jnew = iwk(ja(jj))
            if (jnew .ne. j) then
c--------------check there is enough space in kb and jb arrays
               if (k0+1 .gt. nkmax) then
                  ierr = 1
                  write (*,*) 'csrvbr: no space in kb for block row ', i
                  return
               endif
c--------------set entries for this block
               j = jnew
               b0 = b0 + neqr * (kvstc(j+1) - kvstc(j))
               kb(k0+1) = b0
               jb(k0) = j
               k0 = k0 + 1
            endif
         enddo
c--------loop on scalar rows in block row
         do ii = 0, neqr-1
            b1 = kb(ib(i))+ii
c-----------loop on elements in a scalar row
            do jj = 1, numc
c--------------check there is enough space in b array
               if (b1 .gt. nzmax) then
                  ierr = 2
                  write (*,*) 'csrvbr: no space in b for block row ', i
                  return
               endif
               b(b1) = a(a0)
               b1 = b1 + neqr
               a0 = a0 + 1
            enddo
         enddo
      enddo
      ib(nr+1) = k0
      return
c-----------------------------------------------------------------------
c     Conversion for user supplied block partitioning
c-----------------------------------------------------------------------
 400  continue
c-----initialize workspace for sparsity indicator
      do i = ncol+1, ncol+nc
         iwk(i) = 0
      enddo
      k0 = 1
      kb(1) = 1
c-----find sparsity of block rows
      do i = 1, nr
         neqr = kvstr(i+1) - kvstr(i)
         numc = ia(kvstr(i)+1) - ia(kvstr(i))
         ib(i) = k0
c--------loop on all the elements in the block row to determine block sparsity
         do jj = ia(kvstr(i)), ia(kvstr(i+1))-1
            iwk(iwk(ja(jj))+ncol) = 1
         enddo
c--------use sparsity to set jb and kb arrays
         do j = 1, nc
            if (iwk(j+ncol) .ne. 0) then
c--------------check there is enough space in kb and jb arrays
               if (k0+1 .gt. nkmax) then
                  ierr = 1
                  write (*,*) 'csrvbr: no space in kb for block row ', i
                  return
               endif
               kb(k0+1) = kb(k0) + neqr * (kvstc(j+1) - kvstc(j))
               jb(k0) = j
               k0 = k0 + 1
               iwk(j+ncol) = 0
            endif
         enddo
      enddo
      ib(nr+1) = k0
c-----Fill b with entries from a by traversing VBR data structure.
      a0 = 1
c-----loop on block rows
      do i = 1, nr
         neqr = kvstr(i+1) - kvstr(i)
c--------loop on scalar rows in block row
         do ii = 0, neqr-1
            b0 = kb(ib(i)) + ii
c-----------loop on block columns
            do j = ib(i), ib(i+1)-1
c--------------loop on scalar columns within block column
               do jj = kvstc(jb(j)), kvstc(jb(j)+1)-1
c-----------------check there is enough space in b array
                  if (b0 .gt. nzmax) then
                     ierr = 2
                     write (*,*)'csrvbr: no space in b for blk row',i
                     return
                  endif
                  if (a0 .ge. ia(kvstr(i)+ii+1)) then
                     b(b0) = 0.d0
                  else
                     if (jj .eq. ja(a0)) then
                        b(b0) = a(a0)
                        a0 = a0 + 1
                     else
                        b(b0) = 0.d0
                     endif
                  endif
                  b0 = b0 + neqr
c--------------endloop on scalar columns
               enddo
c-----------endloop on block columns
            enddo
 2020       continue
         enddo
      enddo
      return
      end
      subroutine  csscal(n,sa,cx,incx)
c
c     scales a complex vector by a real constant.
c     jack dongarra, linpack, 3/11/78.
c
      complex cx(1)
      real sa
      integer i,incx,n,nincx
c
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        cx(i) = cmplx(sa*real(cx(i)),sa*aimag(cx(i)))
   10 continue
      return
c
c        code for increment equal to 1
c
   20 do 30 i = 1,n
        cx(i) = cmplx(sa*real(cx(i)),sa*aimag(cx(i)))
   30 continue
      return
      end
      subroutine  cswap (n,cx,incx,cy,incy)
c
c     interchanges two vectors.
c     jack dongarra, linpack, 3/11/78.
c
      complex cx(1),cy(1),ctemp
      integer i,incx,incy,ix,iy,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        ctemp = cx(ix)
        cx(ix) = cy(iy)
        cy(iy) = ctemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
   20 do 30 i = 1,n
        ctemp = cx(i)
        cx(i) = cy(i)
        cy(i) = ctemp
   30 continue
      return
      end
c
      double precision function dasum(n,dx,incx)
c
c     takes the sum of the absolute values.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dtemp
      integer i,incx,m,mp1,n,nincx
c
      dasum = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dtemp = dtemp + dabs(dx(i))
   10 continue
      dasum = dtemp
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,6)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dabs(dx(i))
   30 continue
      if( n .lt. 6 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,6
        dtemp = dtemp + dabs(dx(i)) + dabs(dx(i + 1)) + dabs(dx(i + 2))
     *  + dabs(dx(i + 3)) + dabs(dx(i + 4)) + dabs(dx(i + 5))
   50 continue
   60 dasum = dtemp
      return
      end
c----------end-of-hes---------------------------------------------------
c-----------------------------------------------------------------------
      subroutine daxpy(n,t,x,indx,y,indy)
      integer n, indx, indy
      real*8 x(n), y(n), t
c-------------------------------------------------------------------
c does the following operation
c y <--- y + t * x ,   (replace by the blas routine daxpy )
c indx and indy are supposed to be one here
c-------------------------------------------------------------------
       integer k

       do 1 k=1,n
          y(k) = y(k) + x(k)*t
1      continue
       return
       end
c-----end-of-fgmres
c-----------------------------------------------------------------------
      subroutine dbcg (n,rhs,sol,ipar,fpar,w)
      implicit none
      integer n,ipar(16)
      real*8 rhs(n), sol(n), fpar(16), w(n,*)
c-----------------------------------------------------------------------
c Quasi GMRES method for solving a linear
c system of equations a * sol = y.  double precision version.
c this version is without restarting and without preconditioning.
c parameters :
c -----------
c n     = dimension of the problem
c
c y     = w(:,1) a temporary storage used for various operations
c z     = w(:,2) a work vector of length n.
c v     = w(:,3:4) size n x 2
c w     = w(:,5:6) size n x 2
c p     = w(:,7:9) work array of dimension n x 3
c del x = w(:,10)  accumulation of the changes in solution
c tmp   = w(:,11)  a temporary vector used to hold intermediate result of
c                  preconditioning, etc.
c
c sol   = the solution of the problem . at input sol must contain an
c         initial guess to the solution.
c    ***  note:   y is destroyed on return.
c
c-----------------------------------------------------------------------
c subroutines and functions called:
c 1) matrix vector multiplication and preconditioning through reverse
c     communication
c
c 2) implu, uppdir, distdot (blas)
c-----------------------------------------------------------------------
c aug. 1983  version.    author youcef saad. yale university computer
c science dept. some  changes made july 3, 1986.
c references: siam j. sci. stat. comp., vol. 5, pp. 203-228 (1984)
c-----------------------------------------------------------------------
c     local variables
c
      real*8 one,zero
      parameter(one=1.0D0,zero=0.0D0)
c
      real*8 t,sqrt,distdot,ss,res,beta,ss1,delta,x,zeta,umm
      integer k,j,i,i2,ip2,ju,lb,lbm1,np,indp
      logical lp,rp,full, perm(3)
      real*8 ypiv(3),u(3),usav(3)
      external tidycg
      save
c
c     where to go
c
      if (ipar(1).le.0) ipar(10) = 0
      goto (110, 120, 130, 140, 150, 160, 170, 180, 190, 200) ipar(10)
c
c     initialization, parameter checking, clear the work arrays
c
      call bisinit(ipar,fpar,11*n,1,lp,rp,w)
      if (ipar(1).lt.0) return
      perm(1) = .false.
      perm(2) = .false.
      perm(3) = .false.
      usav(1) = zero
      usav(2) = zero
      usav(3) = zero
      ypiv(1) = zero
      ypiv(2) = zero
      ypiv(3) = zero
c-----------------------------------------------------------------------
c     initialize constants for outer loop :
c-----------------------------------------------------------------------
      lb = 3
      lbm1 = 2
c
c     get initial residual vector and norm
c
      ipar(1) = 1
      ipar(8) = 1
      ipar(9) = 1 + n
      do i = 1, n
         w(i,1) = sol(i)
      enddo
      ipar(10) = 1
      return
 110  ipar(7) = ipar(7) + 1
      ipar(13) = ipar(13) + 1
      if (lp) then
         do i = 1, n
            w(i,1) = rhs(i) - w(i,2)
         enddo
         ipar(1) = 3
         ipar(8) = 1
         ipar(9) = n+n+1
         ipar(10) = 2
         return
      else
         do i = 1, n
            w(i,3) = rhs(i) - w(i,2)
         enddo
      endif
      fpar(11) = fpar(11) + n
c
 120  fpar(3) = sqrt(distdot(n,w(1,3),1,w(1,3),1))
      fpar(11) = fpar(11) + n + n
      fpar(5) = fpar(3)
      fpar(7) = fpar(3)
      zeta = fpar(3)
      if (abs(ipar(3)).eq.2) then
         fpar(4) = fpar(1) * sqrt(distdot(n,rhs,1,rhs,1)) + fpar(2)
         fpar(11) = fpar(11) + 2*n
      else if (ipar(3).ne.999) then
         fpar(4) = fpar(1) * zeta + fpar(2)
      endif
      if (ipar(3).ge.0.and.fpar(5).le.fpar(4)) then
         fpar(6) = fpar(5)
         goto 900
      endif
c
c     normalize first arnoldi vector
c
      t = one/zeta
      do 22 k=1,n
         w(k,3) = w(k,3)*t
         w(k,5) = w(k,3)
 22   continue
      fpar(11) = fpar(11) + n
c
c     initialize constants for main loop
c
      beta = zero
      delta = zero
      i2 = 1
      indp = 0
      i = 0
c
c     main loop: i = index of the loop.
c
c-----------------------------------------------------------------------
 30   i = i + 1
c
      if (rp) then
         ipar(1) = 5
         ipar(8) = (1+i2)*n+1
         if (lp) then
            ipar(9) = 1
         else
            ipar(9) = 10*n + 1
         endif
         ipar(10) = 3
         return
      endif
c
 130  ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = (1+i2)*n + 1
      endif
      if (lp) then
         ipar(9) = 10*n + 1
      else
         ipar(9) = 1
      endif
      ipar(10) = 4
      return
c
 140  if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = 1
         ipar(10) = 5
         return
      endif
c
c     A^t * x
c
 150  ipar(7) = ipar(7) + 1
      if (lp) then
         ipar(1) = 4
         ipar(8) = (3+i2)*n + 1
         if (rp) then
            ipar(9) = n + 1
         else
            ipar(9) = 10*n + 1
         endif
         ipar(10) = 6
         return
      endif
c
 160  ipar(1) = 2
      if (lp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = (3+i2)*n + 1
      endif
      if (rp) then
         ipar(9) = 10*n + 1
      else
         ipar(9) = n + 1
      endif
      ipar(10) = 7
      return
c
 170  if (rp) then
         ipar(1) = 6
         ipar(8) = ipar(9)
         ipar(9) = n + 1
         ipar(10) = 8
         return
      endif
c-----------------------------------------------------------------------
c     orthogonalize current v against previous v's and
c     determine relevant part of i-th column of u(.,.) the
c     upper triangular matrix --
c-----------------------------------------------------------------------
 180  ipar(7) = ipar(7) + 1
      u(1) = zero
      ju = 1
      k = i2
      if (i .le. lbm1) ju = 0
      if (i .lt. lb) k = 0
 31   if (k .eq. lbm1) k=0
      k=k+1
c
      if (k .ne. i2) then
         ss  = delta
         ss1 = beta
         ju = ju + 1
         u(ju) = ss
      else
         ss = distdot(n,w(1,1),1,w(1,4+k),1)
         fpar(11) = fpar(11) + 2*n
         ss1= ss
         ju = ju + 1
         u(ju) = ss
      endif
c
      do 32  j=1,n
         w(j,1) = w(j,1) - ss*w(j,k+2)
         w(j,2) = w(j,2) - ss1*w(j,k+4)
 32   continue
      fpar(11) = fpar(11) + 4*n
c
      if (k .ne. i2) goto 31
c
c     end of Mod. Gram. Schmidt loop
c
      t = distdot(n,w(1,2),1,w(1,1),1)
c
      beta   = sqrt(abs(t))
      delta  = t/beta
c
      ss = one/beta
      ss1 = one/ delta
c
c     normalize and insert new vectors
c
      ip2 = i2
      if (i2 .eq. lbm1) i2=0
      i2=i2+1
c
      do 315 j=1,n
         w(j,i2+2)=w(j,1)*ss
         w(j,i2+4)=w(j,2)*ss1
 315  continue
      fpar(11) = fpar(11) + 4*n
c-----------------------------------------------------------------------
c     end of orthogonalization.
c     now compute the coefficients u(k) of the last
c     column of the  l . u  factorization of h .
c-----------------------------------------------------------------------
      np = min0(i,lb)
      full = (i .ge. lb)
      call implu(np, umm, beta, ypiv, u, perm, full)
c-----------------------------------------------------------------------
c     update conjugate directions and solution
c-----------------------------------------------------------------------
      do 33 k=1,n
         w(k,1) = w(k,ip2+2)
 33   continue
      call uppdir(n, w(1,7), np, lb, indp, w, u, usav, fpar(11))
c-----------------------------------------------------------------------
      if (i .eq. 1) goto 34
      j = np - 1
      if (full) j = j-1
      if (.not.perm(j)) zeta = -zeta*ypiv(j)
 34   x = zeta/u(np)
      if (perm(np))goto 36
      do 35 k=1,n
         w(k,10) = w(k,10) + x*w(k,1)
 35   continue
      fpar(11) = fpar(11) + 2 * n
c-----------------------------------------------------------------------
 36   if (ipar(3).eq.999) then
         ipar(1) = 10
         ipar(8) = 9*n + 1
         ipar(9) = 10*n + 1
         ipar(10) = 9
         return
      endif
      res = abs(beta*zeta/umm)
      fpar(5) = res * sqrt(distdot(n, w(1,i2+2), 1, w(1,i2+2), 1))
      fpar(11) = fpar(11) + 2 * n
      if (ipar(3).lt.0) then
         fpar(6) = x * sqrt(distdot(n,w,1,w,1))
         fpar(11) = fpar(11) + 2 * n
         if (ipar(7).le.3) then
            fpar(3) = fpar(6)
            if (ipar(3).eq.-1) then
               fpar(4) = fpar(1) * sqrt(fpar(3)) + fpar(2)
            endif
         endif
      else
         fpar(6) = fpar(5)
      endif
c---- convergence test -----------------------------------------------
 190  if (ipar(3).eq.999.and.ipar(11).eq.0) then
         goto 30
      else if (fpar(6).gt.fpar(4) .and. (ipar(6).gt.ipar(7) .or.
     +        ipar(6).le.0)) then
         goto 30
      endif
c-----------------------------------------------------------------------
c     here the fact that the last step is different is accounted for.
c-----------------------------------------------------------------------
      if (.not. perm(np)) goto 900
      x = zeta/umm
      do 40 k = 1,n
         w(k,10) = w(k,10) + x*w(k,1)
 40   continue
      fpar(11) = fpar(11) + 2 * n
c
c     right preconditioning and clean-up jobs
c
 900  if (rp) then
         if (ipar(1).lt.0) ipar(12) = ipar(1)
         ipar(1) = 5
         ipar(8) = 9*n + 1
         ipar(9) = ipar(8) + n
         ipar(10) = 10
         return
      endif
 200  if (rp) then
         call tidycg(n,ipar,fpar,sol,w(1,11))
      else
         call tidycg(n,ipar,fpar,sol,w(1,10))
      endif
      return
      end
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c               REORDERING ROUTINES -- LEVEL SET BASED ROUTINES        c
c----------------------------------------------------------------------c
c dblstr   : doubled stripe partitioner 
c rdis     : recursive dissection partitioner
c dse2way  : distributed site expansion usuing sites from dblstr 
c dse      : distributed site expansion usuing sites from rdis
c------------- utility routines ----------------------------------------- 
c BFS      : Breadth-First search traversal algorithm 
c add_lvst : routine to add a level -- used by BFS 
c stripes  : finds the level set structure
c stripes0 : finds a trivial one-way partitioning from level-sets 
c perphn   : finds a pseudo-peripheral node and performs a BFS from it.
c mapper4  : routine used by dse and dse2way to do center expansion
c get_domns: routine to find subdomaine from linked lists found by 
c            mapper4. 
c add_lk   : routine to add entry to linked list -- used by mapper4. 
c find_ctr : routine to locate an approximate center of a subgraph. 
c rversp   : routine to reverse a given permutation (e.g., for RCMK)
c maskdeg  : integer function to compute the `masked' of a node
c-----------------------------------------------------------------------
      subroutine dblstr(n,ja,ia,ip1,ip2,nfirst,riord,ndom,map,mapptr,
     *     mask,levels,iwk) 
      implicit none
      integer ndom,ja(*),ia(*),ip1,ip2,nfirst,riord(*),map(*),mapptr(*),
     *     mask(*),levels(*),iwk(*),nextdom
c-----------------------------------------------------------------------
c     this routine does a two-way partitioning of a graph using 
c     level sets recursively. First a coarse set is found by a
c     simple cuthill-mc Kee type algorithm. Them each of the large
c     domains is further partitioned into subsets using the same 
c     technique. The ip1 and ip2 parameters indicate the desired number 
c     number of partitions 'in each direction'. So the total number of
c     partitions on return ought to be equal (or close) to ip1*ip2 
c----------------------parameters----------------------------------------
c on entry: 
c---------
c n      = row dimension of matrix == number of vertices in graph
c ja, ia = pattern of matrix in CSR format (the ja,ia arrays of csr data
c          structure)
c ip1    = integer indicating the number of large partitions ('number of
c          paritions in first direction') 
c ip2    = integer indicating the number of smaller partitions, per 
c          large partition, ('number of partitions in second direction') 
c nfirst = number of nodes in the first level that is input in riord 
c riord  = (also an ouput argument). on entry riord contains the labels  
c          of the nfirst nodes that constitute the first level.   
c on return:
c-----------
c ndom   = total number of partitions found 
c map    = list of nodes listed partition by partition from partition 1
c          to paritition ndom.
c mapptr = pointer array for map. All nodes from position 
c          k1=mapptr(idom),to position k2=mapptr(idom+1)-1 in map belong
c          to partition idom.
c work arrays:
c-------------
c mask   = array of length n, used to hold the partition number of each 
c          node for the first (large) partitioning. 
c          mask is also used as a marker of  visited nodes. 
c levels = integer array of length .le. n used to hold the pointer 
c          arrays for the various level structures obtained from BFS. 
c 
c-----------------------------------------------------------------------
      integer n, j,idom,kdom,jdom,maskval,k,nlev,init,ndp1,numnod
      maskval = 1 
      do j=1, n
         mask(j) = maskval 
      enddo
      iwk(1) = 0 
      call BFS(n,ja,ia,nfirst,iwk,mask,maskval,riord,levels,nlev)      
c
c     init = riord(1) 
c     call perphn (ja,ia,mask,maskval,init,nlev,riord,levels) 
      call stripes (nlev,riord,levels,ip1,map,mapptr,ndom)
c-----------------------------------------------------------------------
      if (ip2 .eq. 1) return      
      ndp1 = ndom+1
c     
c     pack info into array iwk 
c 
      do j = 1, ndom+1
         iwk(j) = ndp1+mapptr(j)  
      enddo
      do j=1, mapptr(ndom+1)-1
         iwk(ndp1+j) = map(j) 
      enddo
      do idom=1, ndom 
         j = iwk(idom) 
         numnod = iwk(idom+1) - iwk(idom) 
         init = iwk(j) 
         do k=j, iwk(idom+1)-1 
         enddo
      enddo

      do idom=1, ndom 
         do k=mapptr(idom),mapptr(idom+1)-1 
            mask(map(k)) = idom
         enddo
      enddo
      nextdom = 1 
c
c     jdom = counter for total number of (small) subdomains 
c 
      jdom = 1
      mapptr(jdom) = 1 
c----------------------------------------------------------------------- 
      do idom =1, ndom
         maskval = idom
         nfirst = 1
         numnod = iwk(idom+1) - iwk(idom) 
         j = iwk(idom) 
         init = iwk(j) 
         nextdom = mapptr(jdom) 
c  note:    old version uses iperm array 
         call perphn(numnod,ja,ia,init,mask,maskval,
     *        nlev,riord,levels)
c          
         call stripes (nlev,riord,levels,ip2,map(nextdom),
     *        mapptr(jdom),kdom)
c          
         mapptr(jdom) = nextdom
         do j = jdom,jdom+kdom-1
            mapptr(j+1) = nextdom + mapptr(j+1)-1
         enddo
         jdom = jdom + kdom
      enddo
c
      ndom = jdom - 1
      return
      end 
      SUBROUTINE DCN(AR,IA,JA,N,NE,IC,NN,IERR)
C-----------------------------------------------------------------------
C
C   PURPOSE
C   -------
C   The subroutine generates sparse (square) matrices of the type
C   D(N,C).  This type of matrix has the following characteristics:
C   1's in the diagonal, three bands at the distance C above the
C   diagonal (and reappearing cyclicly under it), and a 10 x 10
C   triangle of elements in the upper right-hand corner.
C   Different software libraries require different storage schemes.
C   This subroutine generates the matrix in the storage  by
C   indices mode.
C
C
C   Note: If A is the sparse matrix of type D(N,C), then
C
C       min|A(i,j)| = 1,     max|A(i,j)| = max(1000,N + 1)
C
C
C
C   CONTRIBUTOR: Ernest E. Rothman
C                Cornell Theory Center/Cornell National Supercomputer
C                Facility.
C                e-mail address: BITNET:   eer@cornellf
C                                INTERNET: eer@cornellf.tn.cornell.edu
C
C
C   REFERENCE
C   ---------
C   1) Zlatev, Zahari; Schaumburg, Kjeld; Wasniewski, Jerzy;
C      "A Testing Scheme for Subroutines Solving Large Linear Problems",
C       Computers and Chemistry, Vol. 5, No. 2-3, pp. 91-100, 1981.
C   2) Osterby, Ole and Zletev, Zahari;
C      "Direct Methods for Sparse Matrices";
C       Springer-Verlag 1983.
C
C
C
C   INPUT PARAMETERS
C   ----------------
C   N    - Integer. The size of the square matrix.
C          N > 13 must be specified.
C
C   NN   - Integer. The dimension of integer arrays IA and JA and 
C          real array AR. Must be at least NE.
C
C   IC   - Integer. The sparsity pattern can be changed by means of this
C          parameter.  0 < IC < N-12  must be specified.
C
C
C   OUTPUT PARAMETERS
C   -----------------
C   NE   - Integer. The number of nonzero elements in the sparse matrix
C          of the type D(N,C). NE = 4*N + 55.
C
C   AR(NN) - Real array. (Double precision)
C            Stored entries of a sparse matrix to be generated by this
C            subroutine.
C            NN is greater then or equal to, NE, the number of
C            nonzeros including a mandatory diagonal entry for
C            each row. Entries are stored by indices.
C
C   IA(NN) - Integer array.
C            Pointers to specify rows for the stored nonzero entries
C            in AR.
C
C   JA(NN) - Integer array.
C            Pointers to specify columns for the stored nonzero entries
C            in AR.
C
C   IERR   - Error parameter is returned as zero on successful
C             execution of the subroutine.
C             Error diagnostics are given by means of positive values
C             of this parameter as follows:
C             IERR = 1    -  N       is out of range.
C             IERR = 2    -  IC      is out of range.
C             IERR = 3    -  NN      is out of range.
C
C----------------------------------------------------------------------
C
      real*8 ar(nn)
      integer ia(nn), ja(nn), ierr
      ierr = 0
c
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Check the input parameters:
c
      if(n.le.13)then
         ierr = 1
         return
      endif
      if(ic .le. 0 .or. ic .ge. n-12)then
         ierr = 2
         return
      endif
      ne = 4*n+55 
      if(nn.lt.ne)then
         ierr = 3
         return
      endif
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c Begin to generate the nonzero elements as well as the row and column
c pointers:
c
      do 20 i=1,n
        ar(i) = 1.0d0
        ia(i) = i
        ja(i) = i
20    continue
      ilast = n
      do 30 i=1,n-ic
        it = ilast + i
        ar(it) = 1.0 + dfloat(i)
        ia(it) = i
        ja(it) = i+ic
30    continue
      ilast = ilast + n-ic
      do 40 i=1,n-ic-1
        it = ilast + i
        ar(it) = -dfloat(i)
        ia(it) = i
        ja(it) = i+ic+1
40    continue
      ilast = ilast + n-ic-1
      do 50 i=1,n-ic-2
        it = ilast + i
        ar(it) = 16.0d0
        ia(it) = i
        ja(it) = i+ic+2
50    continue
      ilast = ilast + n-ic-2
      icount = 0
      do 70 j=1,10
        do 60 i=1,11-j
         icount = icount + 1
         it = ilast + icount
         ar(it) = 100.0d0 * dfloat(j)
         ia(it) = i
         ja(it) = n-11+i+j
60    continue
70    continue
      icount = 0
      ilast = 55 + ilast
      do 80 i=n-ic+1,n
        icount = icount + 1
        it = ilast + icount
        ar(it) = 1.0d0 + dfloat(i)
        ia(it) = i
        ja(it) = i-n+ic
80    continue
      ilast = ilast + ic
      icount = 0
      do 90 i=n-ic,n
        icount = icount + 1
        it = ilast + icount
        ar(it) = -dfloat(i)
        ia(it) = i
        ja(it) = i-n+ic+1
90    continue
      ilast = ilast + ic + 1
      icount = 0
      do 100 i=n-ic-1,n
        icount = icount + 1
        it = ilast + icount
        ar(it) = 16.0d0
        ia(it) = i
        ja(it) = i-n+ic+2
100   continue
c     ilast = ilast + ic + 2
c     if(ilast.ne.4*n+55) then
c     write(*,*)' ilast equal to ', ilast
c     write(*,*)' ILAST, the number of nonzeros, should = ', 4*n + 55
c     stop
c     end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
      subroutine  dcopy(n,dx,incx,dy,incy)
c
c     copies a vector, x, to a vector, y.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dy(1)
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        dy(i) = dx(i)
        dy(i + 1) = dx(i + 1)
        dy(i + 2) = dx(i + 2)
        dy(i + 3) = dx(i + 3)
        dy(i + 4) = dx(i + 4)
        dy(i + 5) = dx(i + 5)
        dy(i + 6) = dx(i + 6)
   50 continue
      return
      end
      subroutine dcsort(ival, n, icnt, index, ilo, ihi)
c-----------------------------------------------------------------------
c     Specifications for arguments:
c     ----------------------------
      integer n, ilo, ihi, ival(n), icnt(ilo:ihi), index(n)
c-----------------------------------------------------------------------
c    This routine computes a permutation which, when applied to the
c    input vector ival, sorts the integers in ival in descending
c    order.  The permutation is represented by the vector index.  The
c    permuted ival can be interpreted as follows:
c      ival(index(i-1)) .ge. ival(index(i)) .ge. ival(index(i+1))
c
c    A specialized sort, the distribution counting sort, is used 
c    which takes advantage of the knowledge that
c        1)  The values are in the (small) range [ ilo, ihi ]
c        2)  Values are likely to be repeated often
c
c    contributed to SPARSKIT by Mike Heroux. (Cray Research) 
c    --------------------------------------- 
c----------------------------------------------------------------------- 
c Usage:
c------ 
c     call dcsort( ival, n, icnt, index, ilo, ihi )
c
c Arguments:
c----------- 
c    ival  integer array (input)
c          On entry, ia is an n dimensional array that contains
c          the values to be sorted.  ival is unchanged on exit.
c
c    n     integer (input)
c          On entry, n is the number of elements in ival and index.
c
c    icnt  integer (work)
c          On entry, is an integer work vector of length 
c          (ihi - ilo + 1).
c
c    index integer array (output)
c          On exit, index is an n-length integer vector containing
c          the permutation which sorts the vector ival.
c
c    ilo   integer (input)
c          On entry, ilo is .le. to the minimum value in ival.
c
c    ihi   integer (input)
c          On entry, ihi is .ge. to the maximum value in ival.
c
c Remarks:
c--------- 
c         The permutation is NOT applied to the vector ival.
c
c----------------------------------------------------------------
c
c Local variables:
c    Other integer values are temporary indices.
c
c Author: 
c-------- 
c    Michael Heroux
c    Sandra Carney
c       Mathematical Software Research Group
c       Cray Research, Inc.
c
c References:
c    Knuth, Donald E., "The Art of Computer Programming, Volume 3:
c    Sorting and Searching," Addison-Wesley, Reading, Massachusetts,
c    1973, pp. 78-79.
c
c Revision history:
c    05/09/90: Original implementation.  A variation of the 
c              Distribution Counting Sort recommended by
c              Sandra Carney. (Mike Heroux)
c
c-----------------------------------------------------------------
c     ----------------------------------
c     Specifications for local variables
c     ----------------------------------
      integer i, j, ivalj
c
c     --------------------------
c     First executable statement
c     --------------------------
      do 10 i = ilo, ihi
        icnt(i) = 0
 10   continue
c
      do 20 i = 1, n
        icnt(ival(i)) = icnt(ival(i)) + 1
 20   continue
c
      do 30 i = ihi-1,ilo,-1
        icnt(i) = icnt(i) + icnt(i+1)
 30   continue
c
      do 40 j = n, 1, -1
        ivalj = ival(j)
        index(icnt(ivalj)) = j
        icnt(ivalj) = icnt(ivalj) - 1
 40   continue
      return
      end
c----------end-of-daxpy-------------------------------------------------
c----------------------------------------------------------------------- 
       function ddot(n,x,ix,y,iy)
       integer n, ix, iy
       real*8 ddot, x(n), y(n)
c-------------------------------------------------------------------
c computes the inner product t=(x,y) -- replace by blas routine ddot
c-------------------------------------------------------------------
       integer j
       real*8 t
        
       t = 0.0d0
       do 10 j=1,n
          t = t + x(j)*y(j)
10     continue
       ddot=t
       return
       end
c----------------------------------------------------------------------- 
      subroutine diacsr (n,job,idiag,diag,ndiag,ioff,a,ja,ia)
      real*8 diag(ndiag,idiag), a(*), t
      integer ia(*), ja(*), ioff(*)
c----------------------------------------------------------------------- 
c    diagonal format     to     compressed sparse row     
c----------------------------------------------------------------------- 
c this subroutine extract the idiag most important diagonals from the 
c input matrix a, ja, ia, i.e, those diagonals of the matrix which have
c the largest number of nonzero elements. If requested (see job),
c the rest of the matrix is put in a the output matrix ao, jao, iao
c----------------------------------------------------------------------- 
c on entry:
c---------- 
c n	= integer. dimension of the matrix a.
c job	= integer. job indicator with the following meaning.
c         if (job .eq. 0) then check for each entry in diag
c         whether this entry is zero. If it is then do not include
c         in the output matrix. Note that the test is a test for
c         an exact arithmetic zero. Be sure that the zeros are
c         actual zeros in double precision otherwise this would not
c         work.
c         
c idiag = integer equal to the number of diagonals to be extracted. 
c         Note: on return idiag may be modified.
c
c diag  = real array of size (ndiag x idiag) containing the diagonals
c         of A on return. 
c 
c ndiag = integer equal to the first dimension of array diag.
c
c ioff  = integer array of length idiag, containing the offsets of the
c   	  diagonals to be extracted.
c
c on return:
c----------- 
c a, 
c ja, 			
c ia    = matrix stored in a, ja, ia, format
c
c Note:
c ----- the arrays a and ja should be of length n*idiag.
c
c----------------------------------------------------------------------- 
      ia(1) = 1
      ko = 1
      do 80 i=1, n
         do 70 jj = 1, idiag
            j = i+ioff(jj) 
            if (j .lt. 1 .or. j .gt. n) goto 70
            t = diag(i,jj) 
            if (job .eq. 0 .and. t .eq. 0.0d0) goto 70
            a(ko) = t
            ja(ko) = j
            ko = ko+1
 70      continue
         ia(i+1) = ko
 80   continue
      return
c----------- end of diacsr ---------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine diagblk(n, nrow, ncol, a, ja, ia)
      implicit none
      integer n, nrow, ncol, ia(1:*), ja(1:*)
      real*8  a(1:*)
c-----------------------------------------------------------------------
c     generates the diagonal block for the given problem.
c-----------------------------------------------------------------------
      integer i, k
      nrow = 1 + (n-1)/2
      ncol = nrow
      k = 1
      ia(1) = 1
      ja(1) = 1
      if (mod(n, 2) .eq. 1) then
         a(1) = 3
      else
         a(1) = 5
      end if
      k = k + 1
      if (ncol.gt.1) then
         ja(2) = 2
         a(2) = -1.0
         k = k + 1
      end if
      
      do 10 i = 2, nrow
         ia(i) = k
         ja(k) = i-1
         a(k) = -1.0
         k = k + 1
         ja(k) = i
         a(k) = 6.0
         k = k + 1
         if (i.lt.nrow) then
            ja(k) = i + 1
            a(k) = -1.0
            k = k+1
         end if
 10   continue
      ia(nrow+1) = k
      return
c---------end-of-diagblk------------------------------------------------ 
      end
c-----------------------------------------------------------------------
      subroutine diag_domi(n,sym,valued,a, ja,ia,ao,jao, iao, 
     *     ddomc, ddomr)
      implicit none 
      real*8 a(*), ao(*), ddomc, ddomr
      integer n, ja(*), ia(n+1), jao(*), iao(n+1)
      logical sym, valued
c-----------------------------------------------------------------
c     this routine computes the percentage of weakly diagonally 
c     dominant rows/columns
c-----------------------------------------------------------------
c     on entry:
c     ---------
c     n     = integer column dimension of matrix
c a     = real array containing the nonzero elements of the matrix
c     the elements are stored by columns in order
c     (i.e. column i comes before column i+1, but the elements
c     within each column can be disordered).
c     ja    = integer array containing the row indices of elements in a
c ia    = integer array containing of length n+1 containing the
c     pointers to the beginning of the columns in arrays a and ja.
c     It is assumed that ia(*) = 1 and ia(n+1) = nzz+1.
c     ao    = real array containing the nonzero elements of the matrix
c     the elements are stored by rows in order
c     (i.e. row i comes before row i+1, but the elements
c     within each row can be disordered).
c ao,jao, iao, 
c     structure for transpose of a 
c     sym   = logical variable indicating whether or not the matrix is
c     symmetric.
c     valued= logical equal to .true. if values are provided and .false.
c         if only the pattern of the matrix is provided. (in that
c     case a(*) and ao(*) are dummy arrays.
c     
c     ON RETURN
c----------
c     ddomc = percentage of weakly diagonally dominant columns
c     ddomr = percentage of weakly diagonally dominant rows
c-------------------------------------------------------------------
c     locals
      integer i, j0, j1, k, j 
      real*8 aii, dsumr, dsumc 
c     number of diagonally dominant columns
c     real arithmetic used to avoid problems.. YS. 03/27/01 
      ddomc = 0.0  
c     number of diagonally dominant rows
      ddomr = 0.0 
      do 10 i = 1, n
         j0 = ia(i)
         j1 = ia(i+1) - 1
         if (valued) then
            aii = 0.0d0
            dsumc = 0.0d0
            do 20 k=j0,j1
               j = ja(k) 
               if (j .eq. i) then
                  aii = abs(a(k))
               else
                  dsumc = dsumc + abs(a(k))
               endif
 20         continue
            dsumr = 0.0d0
            if (.not. sym) then
               do 30 k=iao(i), iao(i+1)-1
                  if (jao(k) .ne. i) dsumr = dsumr+abs(ao(k))
 30            continue 
            else
               dsumr = dsumc
            endif
            if (dsumc .le. aii) ddomc = ddomc + 1.0
            if (dsumr .le. aii) ddomr = ddomr + 1.0
         endif
 10   continue
      ddomr = ddomr / real(n)
      ddomc = ddomc / real(n)
      return
c-----------------------------------------------------------------------
c--------end-of-diag_moni-----------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine diamua (nrow,job, a, ja, ia, diag, b, jb, ib)
      real*8 a(*), b(*), diag(nrow), scal
      integer ja(*),jb(*), ia(nrow+1),ib(nrow+1) 
c-----------------------------------------------------------------------
c performs the matrix by matrix product B = Diag * A  (in place) 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c job   = integer. job indicator. Job=0 means get array b only
c         job = 1 means get b, and the integer arrays ib, jb.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c diag = diagonal matrix stored as a vector dig(1:n)
c
c on return:
c----------
c
c b, 
c jb, 
c ib	= resulting matrix B in compressed sparse row sparse format.
c	    
c Notes:
c-------
c 1)        The column dimension of A is not needed. 
c 2)        algorithm in place (B can take the place of A).
c           in this case use job=0.
c-----------------------------------------------------------------
      do 1 ii=1,nrow
c     
c     normalize each row 
c     
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         scal = diag(ii) 
         do 2 k=k1, k2
            b(k) = a(k)*scal
 2       continue
 1    continue
c     
      if (job .eq. 0) return
c     
      do 3 ii=1, nrow+1
         ib(ii) = ia(ii)
 3    continue
      do 31 k=ia(1), ia(nrow+1) -1 
         jb(k) = ja(k)
 31   continue
      return
c----------end-of-diamua------------------------------------------------
c-----------------------------------------------------------------------
      end 
c----------------------------------------------------------------------- 
      subroutine diapos  (n,ja,ia,idiag) 
      integer ia(n+1), ja(*), idiag(n) 
c-----------------------------------------------------------------------
c this subroutine returns the positions of the diagonal elements of a
c sparse matrix a, ja, ia, in the array idiag.
c-----------------------------------------------------------------------
c on entry:
c---------- 
c
c n	= integer. row dimension of the matrix a.
c a,ja,
c    ia = matrix stored compressed sparse row format. a array skipped.
c
c on return:
c-----------
c idiag  = integer array of length n. The i-th entry of idiag 
c          points to the diagonal element a(i,i) in the arrays
c          a, ja. (i.e., a(idiag(i)) = element A(i,i) of matrix A)
c          if no diagonal element is found the entry is set to 0.
c----------------------------------------------------------------------c
c           Y. Saad, March, 1990
c----------------------------------------------------------------------c
      do 1 i=1, n 
         idiag(i) = 0
 1    continue
c     
c     sweep through data structure. 
c     
      do  6 i=1,n
         do 51 k= ia(i),ia(i+1) -1
            if (ja(k) .eq. i) idiag(i) = k
 51      continue
 6    continue
c----------- -end-of-diapos---------------------------------------------
c-----------------------------------------------------------------------
      return
      end
c-----------------------------------------------------------------------
c-----------------------end-of-vbrinfo----------------------------------
       subroutine dinfo1(n,iout,a,ja,ia,valued,
     *		          title,key,type,ao,jao,iao)
	implicit real*8 (a-h,o-z)
        real*8 a(*),ao(*)
        integer ja(*),ia(n+1),jao(*),iao(n+1),nzdiag 
        character title*72,key*8,type*3 
        logical valued 
c----------------------------------------------------------------------c
c  SPARSKIT:  ELEMENTARY INFORMATION ROUTINE.                          c
c----------------------------------------------------------------------c
c info1 obtains a number of statistics on a sparse matrix and writes   c
c it into the output unit iout. The matrix is assumed                  c
c to be stored in the compressed sparse COLUMN format sparse a, ja, ia c
c----------------------------------------------------------------------c
c Modified Nov 1, 1989. 1) Assumes A is stored in column               
c format. 2) Takes symmetry into account, i.e., handles Harwell-Boeing
c            matrices correctly. 
c          ***  (Because of the recent modification the words row and 
c            column may be mixed-up at occasions... to be checked...
c
c bug-fix July 25: 'upper' 'lower' mixed up in formats 108-107.
c
c On entry :
c-----------
c n	= integer. column dimension of matrix	
c iout  = integer. unit number where the information it to be output.	
c a	= real array containing the nonzero elements of the matrix
c	  the elements are stored by columns in order 
c	  (i.e. column i comes before column i+1, but the elements
c         within each column can be disordered).
c ja	= integer array containing the row indices of elements in a
c ia	= integer array containing of length n+1 containing the 
c         pointers to the beginning of the columns in arrays a and ja.
c	  It is assumed that ia(*) = 1 and ia(n+1) = nzz+1.
c 
c valued= logical equal to .true. if values are provided and .false.
c         if only the pattern of the matrix is provided. (in that
c         case a(*) and ao(*) are dummy arrays.
c
c title = a 72-character title describing the matrix
c         NOTE: The first character in title is ignored (it is often
c         a one).
c
c key   = an 8-character key for the matrix
c type  = a 3-character string to describe the type of the matrix.
c         see harwell/Boeing documentation for more details on the 
c         above three parameters.
c 
c on return
c---------- 
c 1) elementary statistics on the matrix is written on output unit 
c    iout. See below for detailed explanation of typical output. 
c 2) the entries of a, ja, ia are sorted.
c
c---------- 
c 
c ao	= real*8 array of length nnz used as work array.
c jao	= integer work array of length max(2*n+1,nnz) 
c iao   = integer work array of length n+1
c
c Note  : title, key, type are the same paramaters as those
c         used for Harwell-Bowing matrices.
c 
c-----------------------------------------------------------------------
c Output description:
c--------------------
c *** The following info needs to be updated.
c
c + A header containing the Title, key, type of the matrix and, if values
c   are not provided a message to that effect.
c    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c    * SYMMETRIC STRUCTURE MEDIEVAL RUSSIAN TOWNS                       
c    *                    Key = RUSSIANT , Type = SSA                   
c    * No values provided - Information of pattern only                 
c    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c +  dimension n, number of nonzero elements nnz, average number of
c    nonzero elements per column, standard deviation for this average.
c +  if the matrix is upper or lower triangular a message to that effect
c    is printed. Also the number of nonzeros in the strict upper 
c    (lower) parts and the main diagonal are printed.
c +  weight of longest column. This is the largest number of nonzero 
c    elements in a column encountered. Similarly for weight of 
c    largest/smallest row.
c +  lower dandwidth as defined by
c          ml = max ( i-j, / all  a(i,j).ne. 0 ) 
c +  upper bandwidth as defined by
c          mu = max ( j-i, / all  a(i,j).ne. 0 ) 
c    NOTE that ml or mu can be negative. ml .lt. 0 would mean
c    that A is confined to the strict upper part above the diagonal 
c    number -ml. Similarly for mu.
c
c +  maximun bandwidth as defined by
c    Max (  Max [ j ; a(i,j) .ne. 0 ] - Min [ j ; a(i,j) .ne. 0 ] )
c     i  
c +  average bandwidth = average over all columns of the widths each column.
c
c +  If there are zero columns /or rows a message is printed
c    giving the number of such columns/rows.
c    
c +  matching elements in A and transp(A) :this counts the number of
c    positions (i,j) such that if a(i,j) .ne. 0 then a(j,i) .ne. 0.
c    if this number is equal to nnz then the matrix is symmetric.
c +  Relative symmetry match : this is the ratio of the previous integer
c    over nnz. If this ratio is equal to one then the matrix has a
c    symmetric structure. 
c
c +  average distance of a given element from the diagonal, standard dev.
c    the distance of a(i,j) is defined as iabs(j-i). 
c
c +  Frobenius norm of A
c    Frobenius norm of 0.5*(A + transp(A))
c    Frobenius norm of 0.5*(A - transp(A))
c    these numbers provide information on the degree of symmetry
c    of the matrix. If the norm of the nonsymmetric part is
c    zero then the matrix is symmetric.
c
c + 90% of matrix is in the band of width k, means that
c   by moving away and in a symmetric manner from the main
c   diagonal you would have to include exactly k diagonals 
c   (k is always odd), in order to include 90% of the nonzero 
c   elements of A.  The same thing is then for 80%.
c 
c + The total number of nonvoid diagonals, i.e., among the
c   2n-1 diagonals of the matrix which have at least one nonxero
c   element.
c
c +  Most important diagonals. The code selects a number of k
c    (k .le. 10) diagonals that are the most important ones, i.e.
c    that have the largest number of nonzero elements. Any diagonal
c    that has fewer than 1% of the nonzero elements of A is dropped.
c    the numbers printed are the offsets with respect to the 
c    main diagonal, going from left tp right. 
c    Thus 0 means the main diagonal -1 means the subdiagonal, and 
c    +10 means the 10th upper diagonal.
c +  The accumulated percentages in the next line represent the 
c    percentage of the nonzero elements represented by the diagonals
c    up the current one put together. 
c    Thus:
c    *  The 10 most important diagonals are (offsets)    :             *
c    *     0     1     2    24    21     4    23    22    20    19     *
c    *  The accumulated percentages they represent are   :             *
c    *  40.4  68.1  77.7  80.9  84.0  86.2  87.2  88.3  89.4  90.4     *
c    *-----------------------------------------------------------------*
c    shows the offsets of the most important  diagonals and
c    40.4 represent ratio of the number of nonzero elements in the
c    diagonal zero (main diagonal) over the total number of nonzero
c    elements. the second number indicates that the diagonal 0 and the 
c    diagonal 1 together hold 68.1% of the matrix, etc..
c
c +  Block structure:
c    if the matrix has a block structure then the block size is found
c    and printed. Otherwise the info1 will say that the matrix
c    does not have a block structure. Note that block struture has
c    a very specific meaning here. the matrix has a block structure
c    if it consists of square blocks that are dense. even if there
c    are zero elements in the blocks  they should be represented 
c    otherwise it would be possible to determine the block size.
c
c-----------------------------------------------------------------------
	real*8 dcount(20),amx
	integer ioff(20)
	character*61 tmpst
	logical sym
c-----------------------------------------------------------------------
	data ipar1 /1/
	write (iout,99) 
        write (iout,97) title(2:72), key, type
 97     format(2x,' * ',a71,' *'/,
     *         2x,' *',20x,'Key = ',a8,' , Type = ',a3,25x,' *')
	if (.not. valued) write (iout,98)
 98    format(2x,' * No values provided - Information on pattern only',
     *   23x,' *')
c---------------------------------------------------------------------
	nnz = ia(n+1)-ia(1)
	sym = ((type(2:2) .eq. 'S') .or. (type(2:2) .eq. 'Z')
     *    .or. (type(2:2) .eq. 's') .or. (type(2:2) .eq. 'z')) 
c
        write (iout, 99)
        write(iout, 100) n, nnz
	job = 0
	if (valued) job = 1
	ipos = 1
        call csrcsc(n, job, ipos, a, ja, ia, ao, jao, iao)
        call csrcsc(n, job, ipos, ao, jao, iao, a, ja, ia)
c-------------------------------------------------------------------
c computing max bandwith, max number of nonzero elements per column
c min nonzero elements per column/row, row/column diagonal dominance 
c occurences, average distance of an element from diagonal, number of 
c elemnts in lower and upper parts, ...
c------------------------------------------------------------------
c    jao will be modified later, so we call skyline here
          call skyline(n,sym,ja,ia,jao,iao,nsky)
          call nonz_lud(n,ja,ia,nlower, nupper, ndiag) 
          call avnz_col(n,ja,ia,iao, ndiag, av, st)
c------ write out info ----------------------------------------------
	  if (sym)  nupper = nlower 
          write(iout, 101) av, st
          if (nlower .eq. 0 ) write(iout, 105)
 1        if (nupper .eq. 0) write(iout, 106)
          write(iout, 107) nlower
          write(iout, 108) nupper
          write(iout, 109) ndiag
c
          call nonz(n,sym, ja, ia, iao, nzmaxc, nzminc,
     *                  nzmaxr, nzminr, nzcol, nzrow)
          write(iout, 1020) nzmaxc, nzminc
c
	  if (.not. sym) write(iout, 1021) nzmaxr, nzminr
c
	  if (nzcol .ne. 0) write(iout,116) nzcol
	  if (nzrow .ne. 0) write(iout,115) nzrow
c     
          call diag_domi(n,sym,valued,a, ja,ia,ao, jao, iao,
     *                           ddomc, ddomr)
c----------------------------------------------------------------------- 
c symmetry and near symmetry - Frobenius  norms
c-----------------------------------------------------------------------
          call frobnorm(n,sym,a,ja,ia,Fnorm)   
          call ansym(n,sym,a,ja,ia,ao,jao,iao,imatch,av,fas,fan)
          call distaij(n,nnz,sym,ja,ia,dist, std)
          amx = 0.0d0
          do 40 k=1, nnz
            amx = max(amx, abs(a(k)) )
 40       continue
          write (iout,103) imatch, av, dist, std
          write(iout,96) 
          if (valued) then
             write(iout,104) Fnorm, fas, fan, amx, ddomr, ddomc
             write (iout,96)
          endif
c-----------------------------------------------------------------------
c--------------------bandedness- main diagonals ----------------------- -
c-----------------------------------------------------------------------
	n2 = n+n-1
	do 8 i=1, n2
             jao(i) = 0
 8        continue
          do 9 i=1, n
             k1 = ia(i)
             k2 = ia(i+1) -1
             do 91 k=k1, k2
                j = ja(k)
                jao(n+i-j) = jao(n+i-j) +1
 91          continue 
 9        continue
c
          call bandwidth(n,ja, ia, ml, mu, iband, bndav)
c
c     write bandwidth information .
c     
          write(iout,117)  ml, mu, iband, bndav
c     
          write(iout,1175) nsky
c     
c         call percentage_matrix(n,nnz,ja,ia,jao,90,jb2)
c         call percentage_matrix(n,nnz,ja,ia,jao,80,jb1)
          nrow = n
          ncol = n
          call distdiag(nrow,ncol,ja,ia,jao)
          call bandpart(n,ja,ia,jao,90,jb2)
          call bandpart(n,ja,ia,jao,80,jb1)
          write (iout,112) 2*jb2+1, 2*jb1+1
c-----------------------------------------------------------------
          nzdiag = 0
          n2 = n+n-1
          do 42 i=1, n2
             if (jao(i) .ne. 0) nzdiag=nzdiag+1
 42       continue
          call n_imp_diag(n,nnz,jao,ipar1, ndiag,ioff,dcount)
          write (iout,118) nzdiag
          write (tmpst,'(10i6)') (ioff(j),j=1,ndiag)
          write (iout,110) ndiag,tmpst
          write (tmpst,'(10f6.1)')(dcount(j), j=1,ndiag)
          write (iout,111) tmpst
          write (iout, 96)
c     jump to next page -- optional //
c     write (iout,'(1h1)') 
c-----------------------------------------------------------------------
c     determine block size if matrix is a block matrix..
c-----------------------------------------------------------------------
          call blkfnd(n, ja, ia, nblk)
          if (nblk .le. 1) then 
             write(iout,113)  
          else 
             write(iout,114) nblk
          endif 
          write (iout,96)
c     
c---------- done. Next define all the formats -------------------------- 
c
 99    format (2x,38(2h *))
 96    format (6x,' *',65(1h-),'*')
c-----------------------------------------------------------------------
 100   format(
     * 6x,' *  Dimension N                                      = ',
     * i10,'  *'/
     * 6x,' *  Number of nonzero elements                       = ',
     * i10,'  *')
 101   format(
     * 6x,' *  Average number of nonzero elements/Column        = ',
     * f10.4,'  *'/
     * 6x,' *  Standard deviation for above average             = ',
     * f10.4,'  *')
c-----------------------------------------------------------------------
 1020       format(
     * 6x,' *  Weight of longest column                         = ',
     * i10,'  *'/
     * 6x,' *  Weight of shortest column                        = ',
     * i10,'  *')
 1021       format(
     * 6x,' *  Weight of longest row                            = ',
     * i10,'  *'/
     * 6x,' *  Weight of shortest row                           = ',
     * i10,'  *')
 117        format(
     * 6x,' *  Lower bandwidth  (max: i-j, a(i,j) .ne. 0)       = ',
     * i10,'  *'/
     * 6x,' *  Upper bandwidth  (max: j-i, a(i,j) .ne. 0)       = ',
     * i10,'  *'/
     * 6x,' *  Maximum Bandwidth                                = ',
     * i10,'  *'/
     * 6x,' *  Average Bandwidth                                = ',
     * e10.3,'  *')
 1175       format(
     * 6x,' *  Number of nonzeros in skyline storage            = ',
     * i10,'  *')
 103   format(
     * 6x,' *  Matching elements in symmetry                    = ',
     * i10,'  *'/
     * 6x,' *  Relative Symmetry Match (symmetry=1)             = ',
     * f10.4,'  *'/
     * 6x,' *  Average distance of a(i,j)  from diag.           = ',
     * e10.3,'  *'/
     * 6x,' *  Standard deviation for above average             = ',
     * e10.3,'  *') 
 104   format(
     * 6x,' *  Frobenius norm of A                              = ',
     * e10.3,'  *'/
     * 6x,' *  Frobenius norm of symmetric part                 = ',
     * e10.3,'  *'/
     * 6x,' *  Frobenius norm of nonsymmetric part              = ',
     * e10.3,'  *'/
     * 6x,' *  Maximum element in A                             = ',
     * e10.3,'  *'/
     * 6x,' *  Percentage of weakly diagonally dominant rows    = ',
     * e10.3,'  *'/
     * 6x,' *  Percentage of weakly diagonally dominant columns = ',
     * e10.3,'  *')
 105        format(
     * 6x,' *  The matrix is lower triangular ...       ',21x,' *')
 106        format(
     * 6x,' *  The matrix is upper triangular ...       ',21x,' *')
 107        format(
     * 6x,' *  Nonzero elements in strict lower part            = ',
     * i10,'  *')
 108       format(
     * 6x,' *  Nonzero elements in strict upper part            = ',
     * i10,'  *')
 109       format(
     * 6x,' *  Nonzero elements in main diagonal                = ',
     * i10,'  *')
 110   format(6x,' *  The ', i2, ' most important',
     *	   ' diagonals are (offsets)    : ',10x,'  *',/,  
     * 6x,' *',a61,3x,' *')
 111   format(6x,' *  The accumulated percentages they represent are ',
     * '  : ', 10x,'  *',/,
     * 6x,' *',a61,3x,' *')
c 111	format( 
c     * 6x,' *  They constitute the following % of A             = ',
c     * f8.1,' %  *') 
 112	format( 
     * 6x,' *  90% of matrix is in the band of width            = ',
     * i10,'  *',/,
     * 6x,' *  80% of matrix is in the band of width            = ',
     * i10,'  *')
 113 	format( 
     * 6x,' *  The matrix does not have a block structure ',19x,
     *    ' *')
 114 	format( 
     * 6x,' *  Block structure found with block size            = ',
     * i10,'  *')
 115 	format( 
     * 6x,' *  There are zero rows. Number of such rows         = ', 
     * i10,'  *')
 116 	format( 
     * 6x,' *  There are zero columns. Number of such columns   = ', 
     * i10,'  *')
 118 	format( 
     * 6x,' *  The total number of nonvoid diagonals is         = ', 
     * i10,'  *')
c-------------------------- end of dinfo --------------------------
        return
        end
c----------------------------------------------------------------------- 
	subroutine diric (nx,nint,a,ja,ia, f)
c--------------------------------------------------------------
c this routine takes into account the boundary conditions
c and removes the unnecessary boundary points.
c--------------------------------------------------------------
	implicit real*8  (a-h,o-z)
	dimension a(*),ia(*),ja(*),f(*)
c call extract from UNARY
	call submat (nx,1,1,nint,1,nint,a,ja,ia,nr,nc,a,ja,ia)
        write (*,*) 'nr=',nr,'nc=',nc
	return
c----------- end of diric ------------------------------------- 
	end
c------end-of-ansym-----------------------------------------------------
c-----------------------------------------------------------------------
      subroutine distaij(n,nnz,sym,ja,ia,dist, std)
      implicit real*8 (a-h, o-z)
      real*8  dist, std
      integer ja(*), ia(n+1)
c-----------------------------------------------------------------------
c     this routine computes the average distance of a(i,j) from diag and
c     standard deviation  for this average.
c-----------------------------------------------------------------------
c On entry :
c-----------
c n     = integer. column dimension of matrix
c nnz   = number of nonzero elements of matrix
c ja    = integer array containing the row indices of elements in a
c ia    = integer array containing of length n+1 containing the
c         pointers to the beginning of the columns in arrays a and ja.
c         It is assumed that ia(*) = 1 and ia(n+1) = nzz+1.
c sym   = logical variable indicating whether or not the matrix is
c         symmetric.
c on return
c----------
c dist  = average distance of a(i,j) from diag.
c std   = standard deviation for above average.
c-----------------------------------------------------------------------
c
c distance of an element from diagonal.
c
      dist   = 0.0
      std = 0.0
      do 3 i=1,n
         j0 = ia(i)
         j1 = ia(i+1) - 1
         do 31 k=j0, j1
            j=ja(k)
            dist = dist + real(iabs(j-i) )
 31      continue
 3    continue
      dist = dist/real(nnz)
      do 6 i = 1, n 
         do 61 k=ia(i), ia(i+1) - 1
            std=std+(dist-real(iabs(ja(k)-i)))**2
 61      continue
 6    continue
      std = sqrt(std/ real(nnz))
      return
      end
c-----------------------------------------------------------------------
      subroutine distdiag(nrow,ncol,ja,ia,dist)
      implicit real*8 (a-h, o-z)
      integer nrow,ncol,ja(*), ia(nrow+1),dist(*)
c----------------------------------------------------------------------
c this routine computes the numbers of elements in each diagonal. 
c----------------------------------------------------------------------
c On entry :
c-----------
c nrow  = integer. row dimension of matrix
c ncol  = integer. column dimension of matrix
c ja    = integer array containing the row indices of elements in a
c ia    = integer array containing of length n+1 containing the
c         pointers to the beginning of the columns in arrays a and ja.
c         It is assumed that ia(*) = 1 and ia(n+1) = nzz+1.
c on return
c----------
c dist  = integer array containing the numbers of elements in each of 
c         the nrow+ncol-1 diagonals of A. dist(k) contains the 
c         number of elements in diagonal '-nrow+k'.  k ranges from 
c         1 to (nrow+ncol-1).
c----------------------------------------------------------------------
      nnz  = ia(nrow+1)-ia(1)
      n2   = nrow+ncol-1
      do 8 i=1, n2
         dist(i) = 0
 8    continue
      do 9 i=1, nrow
         k1 = ia(i)
         k2 = ia(i+1) -1
         do 91 k=k1, k2
            j = ja(k)
            dist(nrow+j-i) = dist(nrow+j-i) +1
 91      continue
 9    continue
      return
      end
c-----end-of-runrc
c-----------------------------------------------------------------------
      function distdot(n,x,ix,y,iy)
      integer n, ix, iy
      real*8 distdot, x(*), y(*), ddot
      external ddot
      distdot = ddot(n,x,ix,y,iy)
      return
      end
c----------------------------------------------------------------------- 
      SUBROUTINE DLAUNY(X,Y,NODES,ELMNTS,NEMAX,NELMNT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c 
C code written by P.K. Sweby
c simple delauney triangulation routine (non optimal)
c
C     ******************************************************************
C     *                                                                *
C     * Performs a Delaunay triangularisation of a region given a set  *
C     * of mesh points.                                                *
C     *   X,Y    :- 1D arrays holding coordinates of mesh points.      *
C     *             dimensioned AT LEAST NODES+3.                      *
C     *   NODES  :- number of mesh points.                             *
C     *   ELMNTS :- INTEGER array, dimensioned NEMAX x 3, which on exit*
C     *             contains the index of global nodes associated with *
C     *             each element.                                      *
C     *   NELMNT :- on exit contains the number of elements in the     *
C     *             triangularisation.                                 *
C     *                                                                *
C     *                                   P.K.Sweby                    *
C     *                                                                *
C     ******************************************************************
C
      INTEGER ELMNTS
      DIMENSION X(NODES),Y(NODES),ELMNTS(NEMAX,3)
C
      PI=4.0*ATAN(1.0)
C
C     Calculate artificial nodes NODES+i i=1,2,3,4 and construct first
C     two (artificial) elements.
C
      XMIN=X(1)
      XMAX=X(1)
      YMIN=Y(1)
      YMAX=Y(1)
      DO 10 I=2,NODES
      XMIN=MIN(XMIN,X(I))
      XMAX=MAX(XMAX,X(I))
      YMIN=MIN(YMIN,Y(I))
      YMAX=MAX(YMAX,Y(I))
 10   CONTINUE
      DX=XMAX-XMIN
      DY=YMAX-YMIN
      XL=XMIN-4.0*DX
      XR=XMAX+4.0*DX
      YL=YMIN-4.0*DY
      YR=YMAX+4.0*DY
      X(NODES+1)=XL
      Y(NODES+1)=YL
      X(NODES+2)=XL
      Y(NODES+2)=YR
      X(NODES+3)=XR
      Y(NODES+3)=YR
      X(NODES+4)=XR
      Y(NODES+4)=YL
      ELMNTS(1,1)=NODES+1
      ELMNTS(1,2)=NODES+2
      ELMNTS(1,3)=NODES+3
      ELMNTS(2,1)=NODES+3
      ELMNTS(2,2)=NODES+4
      ELMNTS(2,3)=NODES+1
      NELMNT=2
      DO 90 IN=1,NODES
C
C     Add one mesh point at a time and remesh locally if necessary
C
      NDEL=0
      NEWEL=0
      DO 40 IE=1,NELMNT
C
C     Is point IN insided circumcircle of element IE ?
C
      I1=ELMNTS(IE,1)
      I2=ELMNTS(IE,2)
      I3=ELMNTS(IE,3)
      X2=X(I2)-X(I1)
      X3=X(I3)-X(I1)
      Y2=Y(I2)-Y(I1)
      Y3=Y(I3)-Y(I1)
      Z=(X2*(X2-X3)+Y2*(Y2-Y3))/(Y2*X3-Y3*X2)
      CX=0.5*(X3-Z*Y3)
      CY=0.5*(Y3+Z*X3)
      R2=CX**2+CY**2
      RN2=((X(IN)-X(I1)-CX)**2+(Y(IN)-Y(I1)-CY)**2)
      IF(RN2.GT.R2)GOTO 40
C
C     Yes it is inside,create new elements and mark old for deletion.
C
      DO 30 J=1,3
      DO 20 K=1,3
      ELMNTS(NELMNT+NEWEL+J,K)=ELMNTS(IE,K)
 20   CONTINUE
      ELMNTS(NELMNT+NEWEL+J,J)=IN
 30   CONTINUE
      NEWEL=NEWEL+3
      ELMNTS(IE,1)=0
      NDEL=NDEL+1
C
 40   CONTINUE
C
C     If IN was inside circumcircle of more than 1 element then will
C     have created 2 identical new elements: delete them both.
C
      IF(NDEL.GT.1)THEN
          DO 60 IE=NELMNT+1,NELMNT+NEWEL-1
          DO 60 JE=IE+1,NELMNT+NEWEL
          MATCH=0
          DO 50 K=1,3
          DO 50 L=1,3
          IF(ELMNTS(IE,K).EQ.ELMNTS(JE,L))MATCH=MATCH+1
 50       CONTINUE
          IF(MATCH.EQ.3)THEN
              ELMNTS(IE,1)=0
              ELMNTS(JE,1)=0
              NDEL=NDEL+2
          ENDIF
 60       CONTINUE
      ENDIF
C
C     Delete any elements
C
      NN=NELMNT+NEWEL
      IE=1
 70   CONTINUE
      IF(ELMNTS(IE,1).EQ.0)THEN
          DO 80 J=IE,NN-1
          DO 80 K=1,3
          ELMNTS(J,K)=ELMNTS(J+1,K)
 80       CONTINUE
          NN=NN-1
          IE=IE-1
      ENDIF
      IE=IE+1
      IF(IE.LE.NN)GOTO 70
      NELMNT=NN
 90   CONTINUE
C
C     Finally remove elements containing artificial nodes
C
      IE=1
 100  CONTINUE
      NART=0
      DO 110 L=1,3
      IF(ELMNTS(IE,L).GT.NODES)NART=NART+1
 110  CONTINUE
      IF(NART.GT.0)THEN
          DO 120 J=IE,NN-1
          DO 120 K=1,3
          ELMNTS(J,K)=ELMNTS(J+1,K)
 120      CONTINUE
          NELMNT=NELMNT-1
          IE=IE-1
      ENDIF
      IE=IE+1
      IF(IE.LE.NELMNT)GOTO 100
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine dmperm (nrow,a,ja,ao,jao,perm,job)
      integer nrow,ja(*),jao(*),perm(nrow),job
      real*8 a(*),ao(*) 
c-----------------------------------------------------------------------
c This routine performs a symmetric permutation of the rows and 
c columns of a matrix stored in MSR format. i.e., it computes 
c B = P A transp(P), where P, is  a permutation matrix. 
c P maps row i into row perm(i) and column j into column perm(j): 
c      a(i,j)    becomes   a(perm(i),perm(j)) in new matrix
c (i.e.  ao(perm(i),perm(j)) = a(i,j) ) 
c calls dperm. 
c-----------------------------------------------------------------------
c Y. Saad, Nov 15, 1991. 
c-----------------------------------------------------------------------
c on entry: 
c---------- 
c n 	= dimension of the matrix
c a, ja = input matrix in MSR format. 
c perm 	= integer array of length n containing the permutation arrays
c	  for the rows: perm(i) is the destination of row i in the
c         permuted matrix -- also the destination of column i in case
c         permutation is symmetric (job .le. 2) 
c
c job	= integer indicating the work to be done:
c 		job = 1	permute a, ja, ia into ao, jao, iao 
c 		job = 2 permute matrix ignoring real values.
c		
c on return: 
c-----------
c ao, jao = output matrix in MSR. 
c
c in case job .eq. 2 a and ao are never referred to and can be dummy 
c arguments. 
c
c Notes:
c------- 
c  1) algorithm is NOT in place 
c  2) column indices may not be sorted on return even  though they may be 
c     on entry.
c----------------------------------------------------------------------c
c     local variables
c     
      integer n1, n2
      n1 = nrow+1
      n2 = n1+1
c      
      call dperm (nrow,a,ja,ja,ao(n2),jao(n2),jao,perm,perm,job) 
c     
      jao(1) = n2
      do 101 j=1, nrow 
         ao(perm(j)) = a(j) 
         jao(j+1) = jao(j+1)+n1
 101  continue
c
c done
c     
      return
c-----------------------------------------------------------------------
c--------end-of-dmperm--------------------------------------------------
      end
      double precision function dnrm2 ( n, dx, incx)
      integer          next
      double precision   dx(1), cutlo, cuthi, hitest, sum, xmax,zero,one
      data   zero, one /0.0d0, 1.0d0/
c
c     euclidean norm of the n-vector stored in dx() with storage
c     increment incx .
c     if    n .le. 0 return with result = 0.
c     if n .ge. 1 then incx must be .ge. 1
c
c           c.l.lawson, 1978 jan 08
c
c     four phase method     using two built-in constants that are
c     hopefully applicable to all machines.
c         cutlo = maximum of  dsqrt(u/eps)  over all known machines.
c         cuthi = minimum of  dsqrt(v)      over all known machines.
c     where
c         eps = smallest no. such that eps + 1. .gt. 1.
c         u   = smallest positive no.   (underflow limit)
c         v   = largest  no.            (overflow  limit)
c
c     brief outline of algorithm..
c
c     phase 1    scans zero components.
c     move to phase 2 when a component is nonzero and .le. cutlo
c     move to phase 3 when a component is .gt. cutlo
c     move to phase 4 when a component is .ge. cuthi/m
c     where m = n for x() real and m = 2*n for complex.
c
c     values for cutlo and cuthi..
c     from the environmental parameters listed in the imsl converter
c     document the limiting values are as follows..
c     cutlo, s.p.   u/eps = 2**(-102) for  honeywell.  close seconds are
c                   univac and dec at 2**(-103)
c                   thus cutlo = 2**(-51) = 4.44089e-16
c     cuthi, s.p.   v = 2**127 for univac, honeywell, and dec.
c                   thus cuthi = 2**(63.5) = 1.30438e19
c     cutlo, d.p.   u/eps = 2**(-67) for honeywell and dec.
c                   thus cutlo = 2**(-33.5) = 8.23181d-11
c     cuthi, d.p.   same as s.p.  cuthi = 1.30438d19
c     data cutlo, cuthi / 8.232d-11,  1.304d19 /
c     data cutlo, cuthi / 4.441e-16,  1.304e19 /
      data cutlo, cuthi / 8.232d-11,  1.304d19 /
c
      if(n .gt. 0) go to 10
         dnrm2  = zero
         go to 300
c
   10 assign 30 to next
      sum = zero
      nn = n * incx
c                                                 begin main loop
      i = 1
   20    go to next,(30, 50, 70, 110)
   30 if( dabs(dx(i)) .gt. cutlo) go to 85
      assign 50 to next
      xmax = zero
c
c                        phase 1.  sum is zero
c
   50 if( dx(i) .eq. zero) go to 200
      if( dabs(dx(i)) .gt. cutlo) go to 85
c
c                                prepare for phase 2.
      assign 70 to next
      go to 105
c
c                                prepare for phase 4.
c
  100 i = j
      assign 110 to next
      sum = (sum / dx(i)) / dx(i)
  105 xmax = dabs(dx(i))
      go to 115
c
c                   phase 2.  sum is small.
c                             scale to avoid destructive underflow.
c
   70 if( dabs(dx(i)) .gt. cutlo ) go to 75
c
c                     common code for phases 2 and 4.
c                     in phase 4 sum is large.  scale to avoid overflow.
c
  110 if( dabs(dx(i)) .le. xmax ) go to 115
         sum = one + sum * (xmax / dx(i))**2
         xmax = dabs(dx(i))
         go to 200
c
  115 sum = sum + (dx(i)/xmax)**2
      go to 200
c
c
c                  prepare for phase 3.
c
   75 sum = (sum * xmax) * xmax
c
c
c     for real or d.p. set hitest = cuthi/n
c     for complex      set hitest = cuthi/(2*n)
c
   85 hitest = cuthi/float( n )
c
c                   phase 3.  sum is mid-range.  no scaling.
c
      do 95 j =i,nn,incx
      if(dabs(dx(j)) .ge. hitest) go to 100
   95    sum = sum + dx(j)**2
      dnrm2 = dsqrt( sum )
      go to 300
c
  200 continue
      i = i + incx
      if ( i .le. nn ) go to 20
c
c              end of main loop.
c
c              compute square root and adjust for scaling.
c
      dnrm2 = xmax * dsqrt(sum)
  300 continue
      return
      end
c----------------------------------------------------------------------- 
      subroutine dnscsr(nrow,ncol,nzmax,dns,ndns,a,ja,ia,ierr)
      real*8 dns(ndns,*),a(*)
      integer ia(*),ja(*)
c-----------------------------------------------------------------------
c Dense		to    Compressed Row Sparse 
c----------------------------------------------------------------------- 
c
c converts a densely stored matrix into a row orientied
c compactly sparse matrix. ( reverse of csrdns )
c Note: this routine does not check whether an element 
c is small. It considers that a(i,j) is zero if it is exactly
c equal to zero: see test below.
c-----------------------------------------------------------------------
c on entry:
c---------
c
c nrow	= row-dimension of a
c ncol	= column dimension of a
c nzmax = maximum number of nonzero elements allowed. This
c         should be set to be the lengths of the arrays a and ja.
c dns   = input nrow x ncol (dense) matrix.
c ndns	= first dimension of dns. 
c
c on return:
c---------- 
c 
c a, ja, ia = value, column, pointer  arrays for output matrix 
c
c ierr	= integer error indicator: 
c         ierr .eq. 0 means normal retur
c         ierr .eq. i means that the the code stopped while
c         processing row number i, because there was no space left in
c         a, and ja (as defined by parameter nzmax).
c----------------------------------------------------------------------- 
      ierr = 0
      next = 1
      ia(1) = 1
      do 4 i=1,nrow
         do 3 j=1, ncol 
            if (dns(i,j) .eq. 0.0d0) goto 3
            if (next .gt. nzmax) then
               ierr = i
               return
            endif
            ja(next) = j
            a(next) = dns(i,j)
            next = next+1
 3       continue	   
         ia(i+1) = next
 4    continue
      return
c---- end of dnscsr ---------------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine dperm1 (i1,i2,a,ja,ia,b,jb,ib,perm,ipos,job)
      integer i1,i2,job,ja(*),ia(*),jb(*),ib(*),perm(*)
      real*8 a(*),b(*)
c----------------------------------------------------------------------- 
c     general submatrix extraction routine.
c----------------------------------------------------------------------- 
c     extracts rows perm(i1), perm(i1+1), ..., perm(i2) (in this order) 
c     from a matrix (doing nothing in the column indices.) The resulting 
c     submatrix is constructed in b, jb, ib. A pointer ipos to the
c     beginning of arrays b,jb,is also allowed (i.e., nonzero elements
c     are accumulated starting in position ipos of b, jb). 
c-----------------------------------------------------------------------
c Y. Saad,Sep. 21 1989 / recoded Jan. 28 1991 / modified for PSPARSLIB 
c Sept. 1997.. 
c-----------------------------------------------------------------------
c on entry: 
c---------- 
c n 	= dimension of the matrix
c a,ja,
c   ia  = input matrix in CSR format
c perm 	= integer array of length n containing the indices of the rows
c         to be extracted. 
c
c job   = job indicator. if (job .ne.1) values are not copied (i.e.,
c         only pattern is copied).
c
c on return: 
c-----------
c b,ja,
c ib   = matrix in csr format. b(ipos:ipos+nnz-1),jb(ipos:ipos+nnz-1) 
c     contain the value and column indices respectively of the nnz
c     nonzero elements of the permuted matrix. thus ib(1)=ipos.
c
c Notes:
c------- 
c  algorithm is NOT in place 
c----------------------------------------------------------------------- 
c local variables
c
      integer ko,irow,k 
      logical values
c-----------------------------------------------------------------------
      values = (job .eq. 1) 
      ko = ipos 
      ib(1) = ko
      do 900 i=i1,i2
         irow = perm(i) 
         do 800 k=ia(irow),ia(irow+1)-1
            if (values) b(ko) = a(k)
            jb(ko) = ja(k)
            ko=ko+1
 800     continue
         ib(i-i1+2) = ko
 900  continue
      return
c--------end-of-dperm1--------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine dperm2 (i1,i2,a,ja,ia,b,jb,ib,cperm,rperm,istart,
     *        ipos,job)
      integer i1,i2,job,istart,ja(*),ia(*),jb(*),ib(*),cperm(*),rperm(*) 
      real*8 a(*),b(*)
c----------------------------------------------------------------------- 
c     general submatrix permutation/ extraction routine.
c----------------------------------------------------------------------- 
c     extracts rows rperm(i1), rperm(i1+1), ..., rperm(i2) and does an
c     associated column permutation (using array cperm). The resulting
c     submatrix is constructed in b, jb, ib. For added flexibility, the
c     extracted elements are put in sequence starting from row 'istart' 
c     of B. In addition a pointer ipos to the beginning of arrays b,jb,
c     is also allowed (i.e., nonzero elements are accumulated starting in
c     position ipos of b, jb). In most applications istart and ipos are 
c     equal to one. However, the generality adds substantial flexiblity.
c     EXPLE: (1) to permute msr to msr (excluding diagonals) 
c     call dperm2 (1,n,a,ja,ja,b,jb,jb,rperm,rperm,1,n+2) 
c            (2) To extract rows 1 to 10: define rperm and cperm to be
c     identity permutations (rperm(i)=i, i=1,n) and then
c            call dperm2 (1,10,a,ja,ia,b,jb,ib,rperm,rperm,1,1) 
c            (3) to achieve a symmetric permutation as defined by perm: 
c            call dperm2 (1,10,a,ja,ia,b,jb,ib,perm,perm,1,1) 
c            (4) to get a symmetric permutation of A and append the
c            resulting data structure to A's data structure (useful!) 
c            call dperm2 (1,10,a,ja,ia,a,ja,ia(n+1),perm,perm,1,ia(n+1))
c-----------------------------------------------------------------------
c Y. Saad,Sep. 21 1989 / recoded Jan. 28 1991. 
c-----------------------------------------------------------------------
c on entry: 
c---------- 
c n 	= dimension of the matrix
c i1,i2 = extract rows rperm(i1) to rperm(i2) of A, with i1<i2.
c
c a,ja,
c   ia  = input matrix in CSR format
c cperm = integer array of length n containing the permutation arrays
c	  for the columns: cperm(i) is the destination of column j, 
c         i.e., any column index ja(k) is transformed into cperm(ja(k)) 
c
c rperm	=  permutation array for the rows. rperm(i) = origin (in A) of
c          row i in B. This is the reverse permutation relative to the
c          ones used in routines cperm, dperm,.... 
c          rows rperm(i1), rperm(i1)+1, ... rperm(i2) are 
c          extracted from A and stacked into B, starting in row istart
c          of B. 
c istart= starting row for B where extracted matrix is to be added.
c         this is also only a pointer of the be beginning address for
c         ib , on return. 
c ipos  = beginning position in arrays b and jb where to start copying 
c         elements. Thus, ib(istart) = ipos. 
c
c job   = job indicator. if (job .ne.1) values are not copied (i.e.,
c         only pattern is copied).
c
c on return: 
c-----------
c b,ja,
c ib   = matrix in csr format. positions 1,2,...,istart-1 of ib 
c     are not touched. b(ipos:ipos+nnz-1),jb(ipos:ipos+nnz-1) 
c     contain the value and column indices respectively of the nnz
c     nonzero elements of the permuted matrix. thus ib(istart)=ipos.
c
c Notes:
c------- 
c  1) algorithm is NOT in place 
c  2) column indices may not be sorted on return even  though they 
c     may be on entry.
c----------------------------------------------------------------------- 
c local variables
c
      integer ko,irow,k 
      logical values
c-----------------------------------------------------------------------
      values = (job .eq. 1) 
      ko = ipos 
      ib(istart) = ko
      do 900 i=i1,i2
         irow = rperm(i) 
         do 800 k=ia(irow),ia(irow+1)-1
            if (values) b(ko) = a(k)
            jb(ko) = cperm(ja(k))
            ko=ko+1
 800     continue
         ib(istart+i-i1+1) = ko
 900  continue
      return
c--------end-of-dperm2--------------------------------------------------
c-----------------------------------------------------------------------
      end 
c----------------------------------------------------------------------- 
      subroutine dperm (nrow,a,ja,ia,ao,jao,iao,perm,qperm,job)
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(nrow),
     +        qperm(*),job
      real*8 a(*),ao(*) 
c-----------------------------------------------------------------------
c This routine permutes the rows and columns of a matrix stored in CSR
c format. i.e., it computes P A Q, where P, Q are permutation matrices. 
c P maps row i into row perm(i) and Q maps column j into column qperm(j): 
c      a(i,j)    becomes   a(perm(i),qperm(j)) in new matrix
c In the particular case where Q is the transpose of P (symmetric 
c permutation of A) then qperm is not needed. 
c note that qperm should be of length ncol (number of columns) but this
c is not checked. 
c-----------------------------------------------------------------------
c Y. Saad, Sep. 21 1989 / recoded Jan. 28 1991. 
c-----------------------------------------------------------------------
c on entry: 
c---------- 
c n 	= dimension of the matrix
c a, ja, 
c    ia = input matrix in a, ja, ia format
c perm 	= integer array of length n containing the permutation arrays
c	  for the rows: perm(i) is the destination of row i in the
c         permuted matrix -- also the destination of column i in case
c         permutation is symmetric (job .le. 2) 
c
c qperm	= same thing for the columns. This should be provided only
c         if job=3 or job=4, i.e., only in the case of a nonsymmetric
c	  permutation of rows and columns. Otherwise qperm is a dummy
c
c job	= integer indicating the work to be done:
c * job = 1,2 permutation is symmetric  Ao :== P * A * transp(P)
c 		job = 1	permute a, ja, ia into ao, jao, iao 
c 		job = 2 permute matrix ignoring real values.
c * job = 3,4 permutation is non-symmetric  Ao :== P * A * Q 
c 		job = 3	permute a, ja, ia into ao, jao, iao 
c 		job = 4 permute matrix ignoring real values.
c		
c on return: 
c-----------
c ao, jao, iao = input matrix in a, ja, ia format
c
c in case job .eq. 2 or job .eq. 4, a and ao are never referred to 
c and can be dummy arguments. 
c Notes:
c------- 
c  1) algorithm is in place 
c  2) column indices may not be sorted on return even  though they may be 
c     on entry.
c----------------------------------------------------------------------c
c local variables 
      integer locjob, mod
c
c     locjob indicates whether or not real values must be copied. 
c     
      locjob = mod(job,2) 
c
c permute rows first 
c 
      call rperm (nrow,a,ja,ia,ao,jao,iao,perm,locjob)
c
c then permute columns
c
      locjob = 0
c
      if (job .le. 2) then
         call cperm (nrow,ao,jao,iao,ao,jao,iao,perm,locjob) 
      else 
         call cperm (nrow,ao,jao,iao,ao,jao,iao,qperm,locjob) 
      endif 
c     
      return
c-------end-of-dperm----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----end-of-gmres
c-----------------------------------------------------------------------
      subroutine dqgmres(n, rhs, sol, ipar, fpar, w)
      implicit none
      integer n, ipar(16)
      real*8 rhs(n), sol(n), fpar(16), w(*)
c-----------------------------------------------------------------------
c     DQGMRES -- Flexible Direct version of Quasi-General Minimum
c     Residual method. The right preconditioning can be varied from
c     step to step.
c
c     Work space used = n + lb * (2*n+4)
c     where lb = ipar(5) + 1 (default 16 if ipar(5) <= 1)
c-----------------------------------------------------------------------
c     local variables
c
      real*8 one,zero,deps
      parameter(one=1.0D0,zero=0.0D0)
      parameter(deps=1.0D-33)
c
      integer i,ii,j,jp1,j0,k,ptrw,ptrv,iv,iw,ic,is,ihm,ihd,lb,ptr
      real*8 alpha,beta,psi,c,s,distdot
      logical lp,rp,full
      external distdot,bisinit
      save
c
c     where to go
c
      if (ipar(1).le.0) ipar(10) = 0
      goto (10, 20, 40, 50, 60, 70) ipar(10)
c
c     locations of the work arrays. The arrangement is as follows:
c     w(1:n) -- temporary storage for the results of the preconditioning
c     w(iv+1:iw) -- the V's
c     w(iw+1:ic) -- the W's
c     w(ic+1:is) -- the COSINEs of the Givens rotations
c     w(is+1:ihm) -- the SINEs of the Givens rotations
c     w(ihm+1:ihd) -- the last column of the Hessenberg matrix
c     w(ihd+1:i) -- the inverse of the diagonals of the Hessenberg matrix
c
      if (ipar(5).le.1) then
         lb = 16
      else
         lb = ipar(5) + 1
      endif
      iv = n
      iw = iv + lb * n
      ic = iw + lb * n
      is = ic + lb
      ihm = is + lb
      ihd = ihm + lb
      i = ihd + lb
c
c     parameter check, initializations
c
      full = .false.
      call bisinit(ipar,fpar,i,1,lp,rp,w)
      if (ipar(1).lt.0) return
      ipar(1) = 1
      if (lp) then
         do ii = 1, n
            w(iv+ii) = sol(ii)
         enddo
         ipar(8) = iv+1
         ipar(9) = 1
      else
         do ii = 1, n
            w(ii) = sol(ii)
         enddo
         ipar(8) = 1
         ipar(9) = iv+1
      endif
      ipar(10) = 1
      return
c
 10   ipar(7) = ipar(7) + 1
      ipar(13) = ipar(13) + 1
      if (lp) then
         do i = 1, n
            w(i) = rhs(i) - w(i)
         enddo
         ipar(1) = 3
         ipar(8) = 1
         ipar(9) = iv+1
         ipar(10) = 2
         return
      else
         do i = 1, n
            w(iv+i) = rhs(i) - w(iv+i)
         enddo
      endif
      fpar(11) = fpar(11) + n
c
 20   alpha = sqrt(distdot(n, w(iv+1), 1, w(iv+1), 1))
      fpar(11) = fpar(11) + (n + n)
      if (abs(ipar(3)).eq.2) then
         fpar(4) = fpar(1) * sqrt(distdot(n,rhs,1,rhs,1)) + fpar(2)
         fpar(11) = fpar(11) + 2*n
      else if (ipar(3).ne.999) then
         fpar(4) = fpar(1) * alpha + fpar(2)
      endif
      fpar(3) = alpha
      fpar(5) = alpha
      psi = alpha
      if (alpha.le.fpar(4)) then
         ipar(1) = 0
         fpar(6) = alpha
         goto 80
      endif
      alpha = one / alpha
      do i = 1, n
         w(iv+i) = w(iv+i) * alpha
      enddo
      fpar(11) = fpar(11) + n
      j = 0
c
c     iterations start here
c
 30   j = j + 1
      if (j.gt.lb) j = j - lb
      jp1 = j + 1
      if (jp1.gt.lb) jp1 = jp1 - lb
      ptrv = iv + (j-1)*n + 1
      ptrw = iv + (jp1-1)*n + 1
      if (.not.full) then
         if (j.gt.jp1) full = .true.
      endif
      if (full) then
         j0 = jp1+1
         if (j0.gt.lb) j0 = j0 - lb
      else
         j0 = 1
      endif
c
c     request the caller to perform matrix-vector multiplication and
c     preconditioning
c
      if (rp) then
         ipar(1) = 5
         ipar(8) = ptrv
         ipar(9) = ptrv + iw - iv
         ipar(10) = 3
         return
      else
         do i = 0, n-1
            w(ptrv+iw-iv+i) = w(ptrv+i)
         enddo
      endif
c
 40   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = ptrv
      endif
      if (lp) then
         ipar(9) = 1
      else
         ipar(9) = ptrw
      endif
      ipar(10) = 4
      return
c
 50   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = ptrw
         ipar(10) = 5
         return
      endif
c
c     compute the last column of the Hessenberg matrix
c     modified Gram-schmidt procedure, orthogonalize against (lb-1)
c     previous vectors
c
 60   continue
      call mgsro(full,n,n,lb,jp1,fpar(11),w(iv+1),w(ihm+1),
     $     ipar(12))
      if (ipar(12).lt.0) then
         ipar(1) = -3
         goto 80
      endif
      beta = w(ihm+jp1)
c
c     incomplete factorization (QR factorization through Givens rotations)
c     (1) apply previous rotations [(lb-1) of them]
c     (2) generate a new rotation
c
      if (full) then
         w(ihm+jp1) = w(ihm+j0) * w(is+jp1)
         w(ihm+j0) = w(ihm+j0) * w(ic+jp1)
      endif
      i = j0
      do while (i.ne.j)
         k = i+1
         if (k.gt.lb) k = k - lb
         c = w(ic+i)
         s = w(is+i)
         alpha = w(ihm+i)
         w(ihm+i) = c * alpha + s * w(ihm+k)
         w(ihm+k) = c * w(ihm+k) - s * alpha
         i = k
      enddo
      call givens(w(ihm+j), beta, c, s)
      if (full) then
         fpar(11) = fpar(11) + 6 * lb
      else
         fpar(11) = fpar(11) + 6 * j
      endif
c
c     detect whether diagonal element of this column is zero
c
      if (abs(w(ihm+j)).lt.deps) then
         ipar(1) = -3
         goto 80
      endif
      w(ihd+j) = one / w(ihm+j)
      w(ic+j) = c
      w(is+j) = s
c
c     update the W's (the conjugate directions) -- essentially this is one
c     step of triangular solve.
c
      ptrw = iw+(j-1)*n + 1
      if (full) then
         do i = j+1, lb
            alpha = -w(ihm+i)*w(ihd+i)
            ptr = iw+(i-1)*n+1
            do ii = 0, n-1
               w(ptrw+ii) = w(ptrw+ii) + alpha * w(ptr+ii)
            enddo
         enddo
      endif
      do i = 1, j-1
         alpha = -w(ihm+i)*w(ihd+i)
         ptr = iw+(i-1)*n+1
         do ii = 0, n-1
            w(ptrw+ii) = w(ptrw+ii) + alpha * w(ptr+ii)
         enddo
      enddo
c
c     update the solution to the linear system
c
      alpha = psi * c * w(ihd+j)
      psi = - s * psi
      do i = 1, n
         sol(i) = sol(i) + alpha * w(ptrw-1+i)
      enddo
      if (full) then
         fpar(11) = fpar(11) + lb * (n+n)
      else
         fpar(11) = fpar(11) + j * (n+n)
      endif
c
c     determine whether to continue,
c     compute the desired error/residual norm
c
      ipar(7) = ipar(7) + 1
      fpar(5) = abs(psi)
      if (ipar(3).eq.999) then
         ipar(1) = 10
         ipar(8) = -1
         ipar(9) = 1
         ipar(10) = 6
         return
      endif
      if (ipar(3).lt.0) then
         alpha = abs(alpha)
         if (ipar(7).eq.2 .and. ipar(3).eq.-1) then
            fpar(3) = alpha*sqrt(distdot(n, w(ptrw), 1, w(ptrw), 1))
            fpar(4) = fpar(1) * fpar(3) + fpar(2)
            fpar(6) = fpar(3)
         else
            fpar(6) = alpha*sqrt(distdot(n, w(ptrw), 1, w(ptrw), 1))
         endif
         fpar(11) = fpar(11) + 2 * n
      else
         fpar(6) = fpar(5)
      endif
      if (ipar(1).ge.0 .and. fpar(6).gt.fpar(4) .and. (ipar(6).le.0
     +     .or. ipar(7).lt.ipar(6))) goto 30
 70   if (ipar(3).eq.999 .and. ipar(11).eq.0) goto 30
c
c     clean up the iterative solver
c
 80   fpar(7) = zero
      if (fpar(3).ne.zero .and. fpar(6).ne.zero .and.
     +     ipar(7).gt.ipar(13))
     +     fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7)-ipar(13))
      if (ipar(1).gt.0) then
         if (ipar(3).eq.999 .and. ipar(11).ne.0) then
            ipar(1) = 0
         else if (fpar(6).le.fpar(4)) then
            ipar(1) = 0
         else if (ipar(6).gt.0 .and. ipar(7).ge.ipar(6)) then
            ipar(1) = -1
         else
            ipar(1) = -10
         endif
      endif
      return
      end
c
      subroutine  drot (n,dx,incx,dy,incy,c,s)
c
c     applies a plane rotation.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dy(1),dtemp,c,s
      integer i,incx,incy,ix,iy,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = c*dx(ix) + s*dy(iy)
        dy(iy) = c*dy(iy) - s*dx(ix)
        dx(ix) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
   20 do 30 i = 1,n
        dtemp = c*dx(i) + s*dy(i)
        dy(i) = c*dy(i) - s*dx(i)
        dx(i) = dtemp
   30 continue
      return
      end
c
      subroutine drotg(da,db,c,s)
c
c     construct givens plane rotation.
c     jack dongarra, linpack, 3/11/78.
c
      double precision da,db,c,s,roe,scale,r,z
c
      roe = db
      if( dabs(da) .gt. dabs(db) ) roe = da
      scale = dabs(da) + dabs(db)
      if( scale .ne. 0.0d0 ) go to 10
         c = 1.0d0
         s = 0.0d0
         r = 0.0d0
         go to 20
   10 r = scale*dsqrt((da/scale)**2 + (db/scale)**2)
      r = dsign(1.0d0,roe)*r
      c = da/r
      s = db/r
   20 z = 1.0d0
      if( dabs(da) .gt. dabs(db) ) z = s
      if( dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0 ) z = 1.0d0/c
      da = r
      db = z
      return
      end
c-----------------------------------------------------------------------
      subroutine dscaldg (n,a,ja,ia,diag,job)
      real*8 a(*), diag(*),t
      integer ia(*),ja(*)
c----------------------------------------------------------------------- 
c scales rows by diag where diag is either given (job=0)
c or to be computed:
c  job = 1 ,scale row i by  by  +/- max |a(i,j) | and put inverse of 
c       scaling factor in diag(i),where +/- is the sign of a(i,i).
c  job = 2 scale by 2-norm of each row..
c if diag(i) = 0,then diag(i) is replaced by one
c (no scaling)..
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
      goto (12,11,10) job+1
 10   do 110 j=1,n
         k1= ia(j)
         k2 = ia(j+1)-1
         t = 0.0d0
         do 111 k = k1,k2
 111        t = t+a(k)*a(k)
 110        diag(j) = sqrt(t)
            goto 12
 11   continue
      call retmx (n,a,ja,ia,diag)
c------
 12   do 1 j=1,n
         if (diag(j) .ne. 0.0d0) then 
            diag(j) = 1.0d0/diag(j)
         else 
            diag(j) = 1.0d0
         endif
 1    continue
      do 2 i=1,n
         t = diag(i)
         do 21 k=ia(i),ia(i+1) -1
            a(k) = a(k)*t
 21      continue
 2    continue
      return 
c--------end of dscaldg -----------------------------------------------
c-----------------------------------------------------------------------
      end

      subroutine  dscal(n,da,dx,incx)
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision da,dx(1)
      integer i,incx,m,mp1,n,nincx
c
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end
c-----------------------------------------------------------------------
      subroutine dse2way(n,ja,ia,ip1,ip2,nfirst,riord,ndom,dom,idom,
     *     mask,jwk,link) 
c-----------------------------------------------------------------------
c     uses centers obtained from dblstr partition to get new partition
c----------------------------------------------------------------------- 
c     input: n, ja, ia   = matrix
c     nfirst = number of first points 
c     riord  = riord(1:nfirst) initial points 
c     output 
c     ndom   = number of domains
c     dom, idom = pointer array structure for domains. 
c     mask , jwk, link = work arrays,
c-----------------------------------------------------------------------
      implicit none 
      integer n, ja(*), ia(*), ip1, ip2, nfirst, riord(*), dom(*),
     *     idom(*), mask(*), jwk(*),ndom,link(*)  
c
c-----------------------------------------------------------------------
c     local variables
      integer i, mid,nsiz, maskval,init, outer, nouter, k
      call dblstr(n,ja,ia,ip1,ip2,nfirst,riord,ndom,dom,idom,mask,
     *     link,jwk)
c
      nouter = 3
c----------------------------------------------------------------------- 

      do outer =1, nouter 
c
c     set masks 
c
      do i=1, ndom
         do k=idom(i),idom(i+1)-1
            mask(dom(k)) = i
         enddo
      enddo
c
c     get centers 
c 
      do i =1, ndom
         nsiz = idom(i+1) - idom(i) 
         init = dom(idom(i))
         maskval = i 
c
c         use link for local riord -- jwk for other arrays -- 
c 
         call find_ctr(n,nsiz,ja,ia,init,mask,maskval,link, 
     *    jwk,mid,jwk(nsiz+1)) 
         riord(i) = mid 
      enddo
c
c     do level-set expansion from centers -- save previous diameter 
c 
      call mapper4(n,ja,ia,ndom,riord,jwk,mask,link) 
      call get_domns2(ndom,riord,link,jwk,dom,idom)
c----------------------------------------------------------------------- 
      enddo 
      return 
      end 
c----------------------------------------------------------------------- 
      subroutine dse(n,ja,ia,ndom,riord,dom,idom,mask,jwk,link) 
      implicit none 
      integer n, ja(*), ia(*), ndom, riord(*), dom(*),
     *     idom(*), mask(*), jwk(*),link(*)  
c-----------------------------------------------------------------------
c     uses centers produced from rdis to get a new partitioning -- 
c     see calling sequence in rdis.. 
c-----------------------------------------------------------------------
c     local variables
      integer i, mid, nsiz, maskval,init, outer, nouter, k 
c-----------------------------------------------------------------------
      nouter = 3
c 
      call rdis(n,ja,ia,ndom,dom,idom,mask,link,jwk,jwk(ndom+1)) 
c
c     initial points = 
c
      do outer =1, nouter 
c
c     set masks 
c
      do i=1, ndom
         do k=idom(i),idom(i+1)-1
            mask(dom(k)) = i
         enddo
      enddo
c
c     get centers 
c 
      do i =1, ndom
         nsiz = idom(i+1) - idom(i) 
         init = dom(idom(i))
         maskval = i 
c
c         use link for local riord -- jwk for other arrays -- 
c 

         call find_ctr(n,nsiz,ja,ia,init,mask,maskval,link, 
     *    jwk,mid,jwk(nsiz+1)) 
         riord(i) = mid 
      enddo
c
c     do level-set expansion from centers -- save previous diameter 
c 
      call mapper4(n,ja,ia,ndom,riord,jwk,mask,link) 
      call get_domns2(ndom,riord,link,jwk,dom,idom)
c----------------------------------------------------------------------- 
      enddo 
      return 
      end 

      subroutine  dswap (n,dx,incx,dy,incy)
c
c     interchanges two vectors.
c     uses unrolled loops for increments equal one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dy(1),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c       code for unequal increments or equal increments not equal
c         to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dx(ix)
        dx(ix) = dy(iy)
        dy(iy) = dtemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c       code for both increments equal to 1
c
c
c       clean-up loop
c
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        dtemp = dx(i)
        dx(i) = dy(i)
        dy(i) = dtemp
        dtemp = dx(i + 1)
        dx(i + 1) = dy(i + 1)
        dy(i + 1) = dtemp
        dtemp = dx(i + 2)
        dx(i + 2) = dy(i + 2)
        dy(i + 2) = dtemp
   50 continue
      return
      end
c----------------------------------------------------------------------- 
      subroutine dump (i1,i2,values,a,ja,ia,iout)
      integer i1, i2, ia(*), ja(*), iout
      real*8 a(*) 
      logical values 
c-----------------------------------------------------------------------
c outputs rows i1 through i2 of a sparse matrix stored in CSR format 
c (or columns i1 through i2 of a matrix stored in CSC format) in a file, 
c one (column) row at a time in a nice readable format. 
c This is a simple routine which is useful for debugging. 
c-----------------------------------------------------------------------
c on entry:
c---------
c i1    = first row (column) to print out
c i2    = last row (column) to print out 
c values= logical. indicates whether or not to print real values.
c         if value = .false. only the pattern will be output.
c a,
c ja, 
c ia    =  matrix in CSR format (or CSC format) 
c iout  = logical unit number for output.
c---------- 
c the output file iout will have written in it the rows or columns 
c of the matrix in one of two possible formats (depending on the max 
c number of elements per row. The values are output with only 
c two digits of accuracy (D9.2). )
c-----------------------------------------------------------------------
c     local variables
      integer maxr, i, k1, k2 
c
c select mode horizontal or vertical 
c
        maxr = 0
        do 1 i=i1, i2
           maxr = max0(maxr,ia(i+1)-ia(i))
 1      continue
        
        if (maxr .le. 8) then
c
c able to do one row acros line
c
        do 2 i=i1, i2
           write(iout,100) i
	   k1=ia(i)
	   k2 = ia(i+1)-1
	   write (iout,101) (ja(k),k=k1,k2)
	   if (values) write (iout,102) (a(k),k=k1,k2)
 2      continue
      else 
c
c unable to one row acros line. do three items at a time
c across a line 
         do 3 i=i1, i2
            if (values) then
               write(iout,200) i
            else
               write(iout,203) i               
            endif
            k1=ia(i)
            k2 = ia(i+1)-1
            if (values) then
               write (iout,201) (ja(k),a(k),k=k1,k2)
            else
                write (iout,202) (ja(k),k=k1,k2)
             endif
 3       continue
      endif 
c
c formats :
c
 100  format (1h ,34(1h-),' row',i6,1x,34(1h-) )
 101  format(' col:',8(i5,6h     : ))
 102  format(' val:',8(D9.2,2h :) )
 200  format (1h ,30(1h-),' row',i3,1x,30(1h-),/
     *     3('  columns :    values  * ') )
c-------------xiiiiiihhhhhhddddddddd-*-
 201  format(3(1h ,i6,6h   :  ,D9.2,3h * ) )
 202  format(6(1h ,i5,6h  *    ) ) 
 203  format (1h ,30(1h-),' row',i3,1x,30(1h-),/
     *     3('  column  :  column   *') )
      return
c----end-of-dump--------------------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine dvperm (n, x, perm) 
      integer n, perm(n) 
      real*8 x(n)
c-----------------------------------------------------------------------
c this subroutine performs an in-place permutation of a real vector x 
c according to the permutation array perm(*), i.e., on return, 
c the vector x satisfies,
c
c	x(perm(j)) :== x(j), j=1,2,.., n
c
c-----------------------------------------------------------------------
c on entry:
c---------
c n 	= length of vector x.
c perm 	= integer array of length n containing the permutation  array.
c x	= input vector
c
c on return:
c---------- 
c x	= vector x permuted according to x(perm(*)) :=  x(*)
c
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
c local variables 
      real*8 tmp, tmp1
c
      init      = 1
      tmp	= x(init)	
      ii        = perm(init)
      perm(init)= -perm(init)
      k         = 0
c     
c loop
c 
 6    k = k+1
c
c save the chased element --
c 
      tmp1	  = x(ii) 
      x(ii)     = tmp
      next	  = perm(ii) 
      if (next .lt. 0 ) goto 65
c     
c test for end 
c
      if (k .gt. n) goto 101
      tmp       = tmp1
      perm(ii)  = - perm(ii)
      ii        = next 
c
c end loop 
c
      goto 6
c
c reinitilaize cycle --
c
 65   init      = init+1
      if (init .gt. n) goto 101
      if (perm(init) .lt. 0) goto 65
      tmp	= x(init)
      ii	= perm(init)
      perm(init)=-perm(init)
      goto 6
c     
 101  continue
      do 200 j=1, n
         perm(j) = -perm(j)
 200  continue 
c     
      return
c-------------------end-of-dvperm--------------------------------------- 
c-----------------------------------------------------------------------
      end
      SUBROUTINE ECN(N,IC,NE,IA,JA,AR,NN,IERR)
C----------------------------------------------------------------------
C
C   PURPOSE
C   -------
C   The subroutine generates sparse (square) matrices of the type
C   E(N,C).  This type of matrix has the following characteristics:
C   Symmetric, positive-definite, N x N matrices with 4 in the diagonal
C   and -1 in the two sidediagonal and in the two bands at the distance
C   C from the diagonal. These matrices are similar to matrices obtained
C   from using the five-point formula in the discretization of the
C   elliptic PDE.
C
C
C   Note: If A is the sparse matrix of type E(N,C), then
C
C       min|A(i,j)| = 1,     max|A(i,j)| = 4
C
C
C
C   CONTRIBUTOR: Ernest E. Rothman
C                Cornell Theory Center/Cornell National Supercomputer
C                Facility.
C                e-mail address: BITNET:   eer@cornellf
C                                INTERNET: eer@cornellf.tn.cornell.edu
C
C
C   REFERENCE
C   ---------
C   1) Zlatev, Zahari; Schaumburg, Kjeld; Wasniewski, Jerzy;
C      "A Testing Scheme for Subroutines Solving Large Linear Problems",
C       Computers and Chemistry, Vol. 5, No. 2-3, pp. 91-100, 1981.
C   2) Osterby, Ole and Zletev, Zahari;
C      "Direct Methods for Sparse Matrices";
C       Springer-Verlag 1983.
C
C
C
C   INPUT PARAMETERS
C   ----------------
C   N    - Integer. The size of the square matrix.
C          N > 2 must be specified.
C
C   NN   - Integer. The dimension of integer arrays IA and JA and 
C          real array AR. Must be at least NE.
C
C   NN  - Integer. The dimension of integer array JA. Must be at least
C          NE.
C
C   IC   - Integer. The sparsity pattern can be changed by means of this
C          parameter.  1 < IC < N   must be specified.
C
C
C
C   OUTPUT PARAMETERS
C   -----------------
C   NE   - Integer. The number of nonzero elements in the sparse matrix
C          of the type E(N,C). NE = 5*N - 2*IC - 2 . 
C
C   AR(NN)  - Real array.
C             Stored entries of the sparse matrix A.
C             NE is the number of nonzeros including a mandatory
C             diagonal entry for each row.
C
C   IA(NN)  - Integer array.(Double precision)
C             Pointers to specify rows for the stored nonzero entries
C             in AR.
C
C   JA(NN) - Integer array.
C             Pointers to specify columns for the stored nonzero entries
C             in AR.
C
C   IERR    - Error parameter is returned as zero on successful
C             execution of the subroutine.
C             Error diagnostics are given by means of positive values
C             of this parameter as follows:
C             IERR = 1    -  N       is out of range.
C             IERR = 2    -  IC      is out of range.
C             IERR = 3    -  NN      is out of range.
C
C---------------------------------------------------------------------
C
C
      real*8 ar(nn)
      integer ia(nn), ja(nn), n, ne, ierr
      ierr = 0
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
Check the input parameters:
c
      if(n.le.2)then
         ierr = 1
         return
      endif
      if(ic.le.1.or.ic.ge.n)then
         ierr = 2
         return
      endif
c
      ne = 5*n-2*ic-2 
      if(nn.lt.ne)then
         ierr = 3
         return
      endif
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c Begin to generate the nonzero elements as well as the row and column
c pointers:
c
      do 20 i=1,n
      ar(i) = 4.0d0
      ia(i) = i
      ja(i) = i
20    continue
      ilast = n
      do 30 i=1,n-1
      it = ilast + i
      ar(it) = -1.0d0
      ia(it) = i+1
      ja(it) = i
30    continue
      ilast = ilast + n - 1
      do 40 i=1,n-1
      it = ilast + i
      ar(it) = -1.0d0
      ia(it) = i
      ja(it) = i+1
40    continue
      ilast = ilast + n-1
      do 50 i=1,n-ic
      it = ilast + i
      ar(it) = -1.0d0
      ia(it) = i+ic
      ja(it) = i
50    continue
      ilast = ilast + n-ic
      do 60 I=1,n-ic
      it = ilast + i
      ar(it) = -1.0d0
      ia(it) = i
      ja(it) = i+ic
60    continue
c      ilast = ilast + n-ic
c      if(ilast.ne.5*n-2*ic-2) then
c      write(*,*)' ilast equal to ', ilast
c      write(*,*)' ILAST, the no. of nonzeros, should = ', 5*n-2*ic-2
c      stop
c      end if
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
c-----------------------------------------------------------------------
      subroutine ellcsr(nrow,coef,jcoef,ncoef,ndiag,a,ja,ia,nzmax,ierr)
      integer ia(nrow+1), ja(*), jcoef(ncoef,1) 
      real*8 a(*), coef(ncoef,1)
c----------------------------------------------------------------------- 
c  Ellpack - Itpack format  to  Compressed Sparse Row
c----------------------------------------------------------------------- 
c this subroutine converts a matrix stored in ellpack-itpack format 
c coef-jcoef into the compressed sparse row format. It actually checks
c whether an entry in the input matrix is a nonzero element before
c putting it in the output matrix. The test does not account for small
c values but only for exact zeros. 
c----------------------------------------------------------------------- 
c on entry:
c---------- 
c
c nrow 	= row dimension of the matrix A.
c coef	= array containing the values of the matrix A in ellpack format.
c jcoef = integer arraycontains the column indices of coef(i,j) in A.
c ncoef = first dimension of arrays coef, and jcoef.
c ndiag = number of active columns in coef, jcoef.
c 
c ndiag = on entry the number of columns made available in coef.
c
c on return: 
c----------
c a, ia, 
c    ja = matrix in a, ia, ja format where. 
c 
c nzmax	= size of arrays a and ja. ellcsr will abort if the storage 
c	   provided in a, ja is not sufficient to store A. See ierr. 
c
c ierr 	= integer. serves are output error message. 
c         ierr = 0 means normal return. 
c         ierr = 1 means that there is not enough space in
c         a and ja to store output matrix.
c----------------------------------------------------------------------- 
c first determine the length of each row of lower-part-of(A)
      ierr = 0
c-----check whether sufficient columns are available. ----------------- 
c
c------- copy elements row by row.-------------------------------------- 
      kpos = 1
      ia(1) = kpos
      do 6 i=1, nrow
         do 5 k=1,ndiag
            if (coef(i,k) .ne. 0.0d0) then
               if (kpos .gt. nzmax) then
                  ierr = kpos
                  return
               endif
               a(kpos) = coef(i,k)
               ja(kpos) = jcoef(i,k)
               kpos = kpos+1
	    endif
 5       continue
         ia(i+1) = kpos
 6    continue	
      return
c--- end of ellcsr ----------------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine estif3(nel,ske,fe,det,xe,ye,xyke,ierr)
c----------------------------------------------------------------------- 
c this subroutine constructs the element stiffness matrix for heat
c condution problem
c
c                  - Div ( K(x,y) Grad u ) = f
c                    u = 0 on boundary
c
c using 3-node triangular elements arguments:
c nel	= element number
c ske	= element stiffness matrix
c fe	= element load vector
c det	= 2*area of the triangle
c xy, ye= coordinates of the three nodal points in an element.
c xyke  = material constants (kxx, kxy, kyx, kyy)
c
c------------------------------------------------------------------------
	implicit real*8 (a-h,o-z)
	dimension ske(3,3), fe(3), xe(3), ye(3), dn(3,2),xyke(2,2)
c
c initialize
c
	area = 0.5*det
c
	do 200 i=1,3
	do 200 j=1,3
	ske(i,j) = 0.0d0
 200	continue
c
c get first gradient of shape function
c
	call gradi3(nel,xe,ye,dn,det,ierr)
	if (ierr .ne. 0) return
c
	do 100 i=1,3
	do 100 j=1,3
	t = 0.0d0
	do 102 k=1,2
	do 102 l=1,2
 102	t = t+xyke(k,l)*dn(i,k)*dn(j,l)
 100	ske(i,j) = t*area
c
	return
	end
      subroutine exphes (n,m,dt,eps,u,w,job,z,wkc,beta,errst,hh,ih,
     *                   x, y, indic,ierr) 
c     implicit  real*8 (a-h,o-z)
      integer n, m, job, ih, indic, ierr
      real*8 hh(ih+2,m+1), u(n,m+1), w(n), z(m+1), x(n), y(n)
      complex*16 wkc(m+1) 
      real*8 dt, eps, beta, errst
c-----------------------------------------------------------------------
c this subroutine computes the Arnoldi basis and the corresponding 
c coeffcient vector in the approximation 
c 
c        	w  ::= beta  Vm  ym 
c               where ym = exp(- Hm *dt) * e1
c
c to the vector exp(-A dt) w where A is an arbitary matrix and 
c w is a given input vector. In case job = 0 the arnoldi basis 
c is recomputed. Otherwise the
c code assumes assumes that  u(*) contains an already computed 
c arnoldi basis and computes only the y-vector (which is stored in v(*))
c-----------------------------------------------------------------------
c on entry:
c---------- 
c n	= dimension of matrix
c
c m	= dimension of Krylov subspace (= degree of polynomial 
c         approximation to the exponential used. )
c
c dt	= scalar by which to multiply matrix. Can be viewed
c         as a time step. dt must be positive [to be fixed].
c
c eps   = scalar indicating the relative error tolerated for the result. 
c         the code will try to compute an answer such that 
c         norm2(exactanswer-approximation) / norm2(w) .le. eps 
c
c u	= work array of size n*(m+1) to contain the Arnoldi basis
c
c w	= real array of length n = input vector to  which exp(-A) is
c         to be applied. 
c
c job	= integer. job indicator. If job .lt.  0 then the Arnoldi
c         basis is recomputed. If job .gt. 0 then it is assumed
c         that the user wants to use a previously computed Krylov
c         subspace but a different dt. Thus the Arnoldi basis and
c         the Hessenberg matrix Hm are not recomputed. 
c	  In that case the user should not modify the values of beta
c         and the matrices hh and u(n,*) when recalling phipro. 
c         job = -1 : recompute basis and get an initial estimate for 
c                    time step dt to be used.
c         job = 0  : recompute basis and do not alter dt.
c         job = 1  : do not recompute arnoldi basis. 
c
c z     = real work array of  size (m+1)
c wkc   = complex*16 work array of size (m+1) 
c
c hh    = work array of size size at least (m+2)*(m+1)
c
c ih+2	= first dimension of hh as declared in the calling program.
c         ih must be .ge. m.
c 
c-----------------------------------------------------------------------
c on return:
c-----------
c w2	= resulting vector w2 = exp(-A *dt) * w
c beta  = real equal to the 2-norm of w. Needed if exppro will
c         be recalled with the same Krylov subspace and a different dt. 
c errst = rough estimates of the 2-norm of the error.
c hh	= work array of dimension at least (m+2) x (m+1)
c
c----------------------------------------------------------------------- 
c local variables 
c 
      integer ndmax
      parameter (ndmax=20) 
      real*8 alp0, fnorm, t, rm, ddot, dabs, dsqrt, dsign,dble
      complex*16 alp(ndmax+1), rd(ndmax+1)
      integer i, j, k, ldg, i0, i1, m1 
      logical verboz
      data verboz/.true./
      save
c------use degree 14 chebyshev all the time -------------------------- 
      if (indic .ge. 3) goto 60
c
c------input fraction expansion of rational function ------------------ 
c chebyshev (14,14) 
      ldg= 7
      alp0 =  0.183216998528140087E-11
      alp(1)=( 0.557503973136501826E+02,-0.204295038779771857E+03)
      rd(1)=(-0.562314417475317895E+01, 0.119406921611247440E+01)
      alp(2)=(-0.938666838877006739E+02, 0.912874896775456363E+02)
      rd(2)=(-0.508934679728216110E+01, 0.358882439228376881E+01)
      alp(3)=( 0.469965415550370835E+02,-0.116167609985818103E+02)
      rd(3)=(-0.399337136365302569E+01, 0.600483209099604664E+01)
      alp(4)=(-0.961424200626061065E+01,-0.264195613880262669E+01)
      rd(4)=(-0.226978543095856366E+01, 0.846173881758693369E+01)
      alp(5)=( 0.752722063978321642E+00, 0.670367365566377770E+00)
      rd(5)=( 0.208756929753827868E+00, 0.109912615662209418E+02)
      alp(6)=(-0.188781253158648576E-01,-0.343696176445802414E-01)
      rd(6)=( 0.370327340957595652E+01, 0.136563731924991884E+02)
      alp(7)=( 0.143086431411801849E-03, 0.287221133228814096E-03)
      rd(7)=( 0.889777151877331107E+01, 0.166309842834712071E+02)
c-----------------------------------------------------------------------
c     
c     if job .gt. 0 skip arnoldi process:
c     
      if (job .gt. 0) goto 2
c------normalize vector w and put in first column of u -- 
      beta = dsqrt(ddot(n,w,1,w,1)) 
c-----------------------------------------------------------------------
      if(verboz) print *, ' In EXPHES, beta ', beta 
      if (beta .eq. 0.0d0) then
         ierr = -1 
         indic = 1
         return
      endif
c
      t = 1.0d0/beta 
      do 25 j=1, n
         u(j,1) = w(j)*t 
 25   continue
c------------------Arnoldi loop ------------------------------------- 
c      fnorm = 0.0d0
      i1 = 1
 58   i = i1
      i1 = i + 1
      do 59 k=1, n
         x(k) = u(k,i)
 59   continue
      indic = 3
      return
 60   continue
      do 61 k=1, n
         u(k,i1) = y(k) 
 61   continue
      i0 =1
c     
c switch  for Lanczos version 
c     i0 = max0(1, i-1)
      call mgsr (n, i0, i1, u, hh(1,i))
      fnorm = fnorm + ddot(i1, hh(1,i),1, hh(1,i),1)
      if (hh(i1,i) .eq. 0.0) m = i
      if  (i .lt. m) goto 58
c--------------done with arnoldi loop ---------------------------------
      rm = dble(m) 
      fnorm = dsqrt( fnorm / rm )
c-------get  : beta*e1 into z 
      m1 = m+1 
      do 4 i=1,m1
         hh(i,m1) = 0.0
 4    continue
c
c     compute initial dt when  job .lt. 1 
c
      if (job .ge. 0) goto 2 
c
c     t = eps / beta 
c     
      t = eps 
      do 41 k=1, m-1
         t = t*(1.0d0 - dble(m-k)/rm ) 
 41   continue
c
      t = 2.0d0*rm* (t**(1.0d0/rm) )  / fnorm 
      if(verboz) print *, ' t, dt = ', t, dt
      t = dmin1(dabs(dt),t) 
      dt = dsign(t, dt) 
c     
 2    continue 
      z(1) = beta 
      do 3 k=2, m1
         z(k) = 0.0d0 
 3    continue
c-------get  : exp(H) * beta*e1
      call hes(ldg,m1,hh,ih,dt,z,rd,alp,alp0,wkc)
c-------error estimate 
      errst = dabs(z(m1))
      if(verboz) print *, ' error estimate =', errst 
c-----------------------------------------------------------------------
      indic = 2
      return
      end
      subroutine expprod (n, m, eps, tn, u, w, x, y, a, ioff, ndiag)
      real*8 eps, tn 
      real*8 a(n,ndiag), u(n,m+1), w(n), x(n), y(n)
      integer n, m, ndiag, ioff(ndiag)

c-----------------------------------------------------------------------
c this subroutine computes an approximation to the vector
c
c        	w :=  exp( - A * tn ) * w
c
c for matrices stored in diagonal (DIA) format.
c
c this routine constitutes an interface for the routine exppro for
c matrices stored in diagonal (DIA) format.
c-----------------------------------------------------------------------
c ARGUMENTS
c---------- 
c see exppro for meaning of parameters n, m, eps, tn, u, w, x, y.
c 
c a, ioff, and ndiag are the arguments of the matrix:
c
c a(n,ndiag) = a rectangular array with a(*,k) containing the diagonal 
c              offset by ioff(k) (negative or positive or zero), i.e., 
c              a(i,jdiag) contains the element A(i,i+ioff(jdiag)) in 
c              the usual dense storage scheme.
c 
c ioff	     = integer array containing the offsets  of the ndiag diagonals
c ndiag      = integer. the number of diagonals.
c 
c-----------------------------------------------------------------------
c local variables 
c 
      integer indic, ierr

      indic = 0
 101  continue
      call exppro (n, m, eps, tn, u, w, x, y, indic, ierr) 
      if (indic .eq. 1) goto 102
c     
c     matrix vector-product for diagonal storage --
c     
      call oped(n, x, y, a, ioff, ndiag)
      goto 101
 102  continue
      return
      end
      subroutine exppro (n, m, eps, tn, u, w, x, y, indic, ierr)
c     implicit  real*8 (a-h,o-z)
      integer n, m, indic, ierr
      real*8 eps, tn, u(n,m+1), w(n), x(n), y(n) 
c-----------------------------------------------------------------------
c     
c this subroutine computes an approximation to the vector
c
c        	w :=  exp( - A * tn ) * w
c
c where A is an arbitary matrix and w is a given input vector 
c uses a dynamic estimation of internal time advancement (dt) 
c----------------------------------------------------------------------- 
c THIS IS A REVERSE COMMUNICATION IMPLEMENTATION. 
c------------------------------------------------- 
c USAGE: (see also comments on indic below).
c------ 
c
c      indic = 0
c 1    continue
c      call exppro (n, m, eps, tn, u, w, x, y, indic)
c      if (indic .eq. 1) goto 2 <-- indic .eq. 1 means job is finished
c      call matvec(n, x, y)     <--- user's matrix-vec. product
c                                    with x = input vector, and
c                                     y = result = A * x.
c      goto 1
c 2    continue
c      .....
c
c-----------------------------------------------------------------------
c
c en entry:
c---------- 
c n	= dimension of matrix
c
c m	= dimension of Krylov subspace (= degree of polynomial 
c         approximation to the exponential used. )
c
c eps   = scalar indicating the relative error tolerated for the result. 
c         the code will try to compute an answer such that 
c         norm2(exactanswer-approximation) / norm2(w) .le. eps 
c
c tn	= scalar by which to multiply matrix. (may be .lt. 0)
c         the code will compute an approximation to exp(- tn * A) w
c         and overwrite the result onto w.
c
c u	= work array of size n*(m+1) (used to hold the Arnoldi basis )
c
c w	= real array of length n = input vector to  which exp(-A) is
c         to be applied. this is also an output argument 
c
c x, y  = two real work vectors of length at least  n each. 
c         see indic for usage.
c
c indic = integer used as indicator for the reverse communication.
c         in the first call enter indic = 0. See below for more.
c
c on return:
c-----------
c 
c w     = contains the resulting vector exp(-A * tn ) * w when 
c         exppro has finished (see indic) 
c
c indic = indicator for the reverse communication protocole.
c       * INDIC .eq. 1  means that exppro has finished and w contains the
c         result. 
c       * INDIC .gt. 1 ,  means that exppro has not finished and that
c         it is requesting another matrix vector product before
c         continuing. The user must compute Ax where A is the matrix
c         and x is the vector provided by exppro, and return the 
c         result in y. Then exppro must be called again without
c         changing any other argument. typically this must be
c         implemented in a loop with exppro being called as long
c         indic is returned with a value .ne. 1.
c
c ierr  = error indicator. 
c         ierr = 1 means phipro was called with indic=1 (not allowed)
c         ierr = -1 means that the input is zero the solution has been 
c         unchanged.
c 
c NOTES:  m should not exceed 60 in this version  (see mmax below)
c-----------------------------------------------------------------------
c written by Y. Saad -- version feb, 1991.
c----------------------------------------------------------------------- 
c For reference see following papers : 
c (1) E. Gallopoulos and Y. Saad: Efficient solution of parabolic 
c     equations by Krylov approximation methods. RIACS technical
c     report 90-14.
c (2) Y.Saad: Analysis of some Krylov subspace approximations to the
c     matrix exponential operator. RIACS Tech report. 90-14 
c-----------------------------------------------------------------------
c local variables 
c 
      integer mmax
      parameter (mmax=60) 
      real*8 errst, tcur, told, dtl, beta, red, dabs, dble
      real*8 hh(mmax+2,mmax+1), z(mmax+1)
      complex*16   wkc(mmax+1) 
      integer ih, job
      logical verboz

      data verboz/.true./

      save
c-----------------------------------------------------------------------
c indic = 3  means  passing through only with result of y= Ax to exphes
c indic = 2  means exphes has finished its job
c indic = 1  means exppro has finished its job (real end)/
c-----------------------------------------------------------------------
      ierr = 0 

      if (indic .eq. 3) goto 101

      if (indic .eq. 1) then 
         ierr = 1
         return
      endif
c----- 
      ih = mmax 
      m  = min0(m,mmax) 
      tcur = 0.0d0
      dtl = tn-tcur
      job = -1
c-------------------- outer loop ----------------------------- 
 100  continue
      if(verboz) print *,'In EXPPRO, current time = ', tcur ,'---------'
c------------------------------------------------------------- 
c ---- call exponential propagator --------------------------- 
c------------------------------------------------------------- 
      told = tcur 
 101  continue
c     if (told + dtl .gt. tn) dtl = tn-told
      call  exphes (n,m,dtl,eps,u,w,job,z,wkc,beta,errst,hh,ih,
     *              x,y,indic,ierr) 
c-----------------------------------------------------------------------
      if (ierr .ne. 0) return
      if (indic .ge. 3) return
      tcur = told + dtl 
      if(verboz) print *, ' tcur now = ', tcur, ' dtl = ', dtl
c
c     relative error 
c      if(verboz) print *, ' beta', beta
      errst = errst / beta
c---------
      if ((errst .le. eps) .and. ( (errst .gt. eps/100.0) .or.
     *     (tcur .eq. tn))) goto 102
c     
c     use approximation :  [ new err ] = fact**m  * [cur. error]
c     
      red =  (0.5*eps / errst)**(1.0d0 /dble(m) ) 
      dtl = dtl*red 
      if (dabs(told+dtl) .gt. dabs(tn) )  dtl = tn-told
      if(verboz) print *, ' red =',red,' , reducing dt to: ', dtl
c-------
      job = 1 
      goto 101
c-------
 102  continue 
c
      call project(n,m,u,z,w)
c never go beyond tcur
      job = 0
      dtl = dmin1(dtl, tn-tcur)
      if (dabs(tcur+dtl) .gt. dabs(tn)) dtl = tn-tcur 
      if (dabs(tcur) .lt. dabs(tn)) goto 100
      indic = 1
c
      return
      end
c-----------------------------------------------------------------------
      subroutine extbdg (n,a,ja,ia,bdiag,nblk,ao,jao,iao)
      implicit real*8 (a-h,o-z)
      real*8 bdiag(*),a(*),ao(*)
      integer ia(*),ja(*),jao(*),iao(*) 
c-----------------------------------------------------------------------
c this subroutine extracts the main diagonal blocks of a 
c matrix stored in compressed sparse row format and puts the result
c into the array bdiag and the remainder in ao,jao,iao.
c-----------------------------------------------------------------------
c on entry:
c----------
c n	= integer. The row dimension of the matrix a.
c a,
c ja,
c ia    = matrix stored in csr format
c nblk  = dimension of each diagonal block. The diagonal blocks are
c         stored in compressed format rowwise,i.e.,we store in 
c	  succession the i nonzeros of the i-th row after those of
c	  row number i-1..
c
c on return:
c----------
c bdiag = real*8 array of size (n x nblk) containing the diagonal
c	  blocks of A on return
c ao,
c jao,
C iao   = remainder of the matrix stored in csr format.
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
      m = 1 + (n-1)/nblk
c this version is sequential -- there is a more parallel version
c that goes through the structure twice ....
      ltr =  ((nblk-1)*nblk)/2 
      l = m * ltr
      do 1 i=1,l
         bdiag(i) = 0.0d0
 1    continue
      ko = 0
      kb = 1
      iao(1) = 1
c-------------------------
      do 11 jj = 1,m
         j1 = (jj-1)*nblk+1
         j2 =  min0 (n,j1+nblk-1)
         do 12 j=j1,j2
            do 13 i=ia(j),ia(j+1) -1
               k = ja(i)
               if (k .lt. j1) then
                  ko = ko+1
                  ao(ko) = a(i)
                  jao(ko) = k
               else if (k .lt. j) then
c     kb = (jj-1)*ltr+((j-j1)*(j-j1-1))/2+k-j1+1
c     bdiag(kb) = a(i)
                  bdiag(kb+k-j1) = a(i)
               endif
 13         continue 	
            kb = kb + j-j1
            iao(j+1) = ko+1
 12      continue
 11   continue
      return
c---------end-of-extbdg------------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c-----end of fdreduce-----------------------------------------------------
c-----------------------------------------------------------------------
      subroutine fdaddbc(nx,ny,nz,a,ja,ia,iau,rhs,al,h)
      integer nx, ny, nz, ia(nx*ny*nz), ja(7*nx*ny*nz), iau(nx*ny*nz)
      real*8  h, al(6), a(7*nx*ny*nz), rhs(nx*ny*nz)
c-----------------------------------------------------------------------
c This subroutine will add the boundary condition to the linear system
c consutructed without considering the boundary conditions
c
c The Boundary condition is specified in the following form:
c           du
c     alpha -- + beta u = gamma
c           dn
c Alpha is stored in array AL.  The six side of the boundary appares
c in AL in the following order: left(west), right(east), bottom(south),
c top(north), front, back(rear). (see also the illustration in gen57pt)
c Beta and gamma appears as the functions, betfun and gamfun.
c They have the following prototype
c
c real*8 function xxxfun(x, y, z)
c real*8 x, y, z
c
c where x, y, z are vales in the range of [0, 1][0, (ny-1)*h]
c [0, (nz-1)*h]
c
c At the corners or boundary lines, the boundary conditions are applied
c in the follow order:
c 1) if one side is Dirichlet boundary condition, the Dirichlet boundary
c    condition is used;
c 2) if more than one sides are Dirichlet, the Direichlet condition
c    specified for X direction boundary will overwrite the one specified
c    for Y direction boundary which in turn has priority over Z
c     direction boundaries.
c 3) when all sides are non-Dirichlet, the average values are used.
c-----------------------------------------------------------------------
c     some constants
c
      real*8   half,zero,one,two
      parameter(half=0.5D0,zero=0.0D0,one=1.0D0,two=2.0D0)
c
c     local variables
c
      character*2 side
      integer  i,j,k,kx,ky,kz,node,nbr,ly,uy,lx,ux
      real*8   coeff, ctr, hhalf, x, y, z
      real*8   afun, bfun, cfun, dfun, efun, ffun, gfun, hfun
      external afun, bfun, cfun, dfun, efun, ffun, gfun, hfun
      real*8   betfun, gamfun
      integer  lctcsr
      external lctcsr, betfun, gamfun
c
      hhalf = half * h
      kx = 1
      ky = nx
      kz = nx*ny
c
c     In 3-D case, we need to go through all 6 faces one by one. If
c     the actual dimension is lower, test on ny is performed first.
c     If ny is less or equals to 1, then the value of nz is not
c     checked.
c-----
c     the surface on the left (west) side
c     Concentrate on the contribution from the derivatives related to x,
c     The terms with derivative of x was assumed to be:
c
c     a(3/2,j,k)*[u(2,j,k)-u(1,j,k)] + a(1/2,j,k)*[u(0,j,k)-u(1,j,k)] +
c     h*d(1,j,k)*[u(2,j,k)-u(0,j,k)]/2
c
c     But they actually are:
c
c     2*{a(3/2,j,k)*[u(2,j,k)-u(1,j,k)] -
c     h*a(1,j,k)*[beta*u(1,j,k)-gamma]/alpha]} +
c     h*h*d(1,j,k)*[beta*u(1,j,k)-gamma]/alpha
c
c     Therefore, in terms of local stencil the right neighbor of a node
c     should be changed to 2*a(3/2,j,k),
c     The matrix never contains the left neighbor on this border, nothing
c     needs to be done about it.
c     The following terms should be added to the center stencil:
c     -a(3/2,j,k) + a(1/2,j,k) + [h*d(1,j,k)-2*a(1,j,k)]*h*beta/alpha
c
c     And these terms should be added to the corresponding right-hand side
c     [h*d(1,j,k)-2*a(1,j,k)]*h*gamma/alpha
c
c     Obviously, the formula do not apply for the Dirichlet Boundary
c     Condition, where alpha will be zero. In that case, we simply set
c     all the elements in the corresponding row to zero(0), then let
c     the diagonal element be beta, and the right-hand side be gamma.
c     Thus the value of u at that point will be set. Later on point
c     like this will be removed from the matrix, since they are of
c     know value before solving the system.(not done in this subroutine)
c
      x = zero
      side = 'x1'
      do 20 k = 1, nz
         z = (k-1)*h
         do 21 j = 1, ny
            y = (j-1)*h
            node = 1+(j-1)*ky+(k-1)*kz
c
c     check to see if it's Dirichlet Boundary condition here
c
            if (al(1) .eq. zero) then
               call clrow(node, a, ja, ia)
               a(iau(node)) = betfun(side,x,y,z)
               rhs(node) = gamfun(side,x,y,z)
            else
c
c     compute the terms formulated above to modify the matrix.
c
c     the right neighbor is stroed in nbr'th posiiton in the a
               nbr = lctcsr(node, node+kx, ja, ia)
c
               coeff = two*afun(x,y,z)
               ctr = (h*dfun(x,y,z) - coeff)*h/al(1)
               rhs(node) = rhs(node) + ctr * gamfun(side,x,y,z)
               ctr = afun(x-hhalf,y,z) + ctr * betfun(side,x,y,z)
               coeff = afun(x+hhalf,y,z)
               a(iau(node)) = a(iau(node)) - coeff + ctr
               a(nbr) = two*coeff
            end if
 21      continue
 20   continue
c
c     the right (east) side boudary, similarly, the contirbution from
c     the terms containing the derivatives of x were assumed to be
c
c     a(nx+1/2,j,k)*[u(nx+1,j,k)-u(nx,j,k)] +
c     a(nx-1/2,j,k)*[u(nx-1,j,k)-u(nx,j,k)] +
c     d(nx,j,k)*[u(nx+1,j,k)-u(nx-1,j,k)]*h/2
c
c     Actualy they are:
c
c     2*{h*a(nx,j,k)*[gamma-beta*u(nx,j,k)]/alpha +
c     a(nx-1/2,j,k)*[u(nx-1,j,k)-u(nx,j,k)]} +
c     h*h*d(nx,j,k)*[gamma-beta*u(nx,j,k)]/alpha
c
c     The left stencil has to be set to 2*a(nx-1/2,j,k)
c
c     The following terms have to be added to the center stencil:
c
c     -a(nx-1/2,j,k)+a(nx+1/2,j,k)-[2*a(nx,j,k)+h*d(nx,j,k)]*beta/alpha
c
c     The following terms have to be added to the right-hand side:
c
c     -[2*a(nx,j,k)+h*d(nx,j,k)]*h*gamma/alpha
c
      x = one
      side = 'x2'
      do 22 k = 1, nz
         z = (k-1)*h
         do 23 j = 1, ny
            y = (j-1)*h
            node = (k-1)*kz + j*ky
c
            if (al(2) .eq. zero) then
               call clrow(node, a, ja, ia)
               a(iau(node)) = betfun(side,x,y,z)
               rhs(node) = gamfun(side,x,y,z)
            else
               nbr = lctcsr(node, node-kx, ja, ia)
c
               coeff = two*afun(x,y,z)
               ctr = (coeff + h*dfun(x,y,z))*h/al(2)
               rhs(node) = rhs(node) - ctr * gamfun(side,x,y,z)
               ctr = afun(x+hhalf,y,z) - ctr * betfun(side,x,y,z)
               coeff = afun(x-hhalf,y,z)
               a(iau(node)) = a(iau(node)) - coeff + ctr
               a(nbr) = two*coeff
            end if
 23      continue
 22   continue
c
c     If only one dimension, return now
c
      if (ny .le. 1) return
c
c     the bottom (south) side suface, This similar to the situation
c     with the left side, except all the function and realted variation
c     should be on the y.
c
c     These two block if statment here is to resolve the possible conflict
c     of assign the boundary value differently by different side of the
c     Dirichlet Boundary Conditions. They ensure that the edges that have
c     be assigned a specific value will not be reassigned.
c
      if (al(1) .eq. zero) then
         lx = 2
      else
         lx = 1
      end if
      if (al(2) .eq. zero) then
         ux = nx-1
      else
         ux = nx
      end if
      y = zero
      side = 'y1'
      do 24 k = 1, nz
         z = (k-1)*h
         do 25 i = lx, ux
            x = (i-1)*h
            node = i + (k-1)*kz
c
            if (al(3) .eq. zero) then
               call clrow(node, a, ja, ia)
               a(iau(node)) = betfun(side,x,y,z)
               rhs(node) = gamfun(side,x,y,z)
            else
               nbr = lctcsr(node, node+ky, ja, ia)
c
               coeff = two*bfun(x,y,z)
               ctr = (h*efun(x,y,z) - coeff)*h/al(3)
               rhs(node) = rhs(node) + ctr * gamfun(side,x,y,z)
               ctr = bfun(x,y-hhalf,z) + ctr * betfun(side,x,y,z)
               coeff = bfun(x,y+hhalf,z)
               a(iau(node)) = a(iau(node)) - coeff + ctr
               a(nbr) = two*coeff
            end if
 25      continue
 24   continue
c
c     The top (north) side, similar to the right side
c
      y = (ny-1) * h
      side = 'y2'
      do 26 k = 1, nz
         z = (k-1)*h
         do 27 i = lx, ux
            x = (i-1)*h
            node = (k-1)*kz+(ny-1)*ky + i
c
            if (al(4) .eq. zero) then
               call clrow(node, a, ja, ia)
               a(iau(node)) = betfun(side,x,y,z)
               rhs(node) = gamfun(side,x,y,z)
            else
               nbr = lctcsr(node, node-ky, ja, ia)
c
               coeff = two*bfun(x,y,z)
               ctr = (coeff + h*efun(x,y,z))*h/al(4)
               rhs(node) = rhs(node) - ctr * gamfun(side,x,y,z)
               ctr = bfun(x,y+hhalf,z) - ctr * betfun(side,x,y,z)
               coeff = bfun(x,y-hhalf,z)
               a(iau(node)) = a(iau(node)) - coeff + ctr
               a(nbr) = two*coeff
            end if
 27      continue
 26   continue
c
c     If only has two dimesion to work on, return now
c
      if (nz .le. 1) return
c
c     The front side boundary
c
c     If the edges of the surface has been decided by Dirichlet Boundary
c     Condition, then leave them alone.
c
      if (al(3) .eq. zero) then
         ly = 2
      else
         ly = 1
      end if
      if (al(4) .eq. zero) then
         uy = ny-1
      else
         uy = ny
      end if
c
      z = zero
      side = 'z1'
      do 28 j = ly, uy
         y = (j-1)*h
         do 29 i = lx, ux
            x = (i-1)*h
            node = i + (j-1)*ky
c
            if (al(5) .eq. zero) then
               call clrow(node, a, ja, ia)
               a(iau(node)) = betfun(side,x,y,z)
               rhs(node) = gamfun(side,x,y,z)
            else
               nbr = lctcsr(node, node+kz, ja, ia)
c
               coeff = two*cfun(x,y,z)
               ctr = (h*ffun(x,y,z) - coeff)*h/al(5)
               rhs(node) = rhs(node) + ctr * gamfun(side,x,y,z)
               ctr = cfun(x,y,z-hhalf) + ctr * betfun(side,x,y,z)
               coeff = cfun(x,y,z+hhalf)
               a(iau(node)) = a(iau(node)) - coeff + ctr
               a(nbr) = two*coeff
            end if
 29      continue
 28   continue
c
c     Similiarly for the top side of the boundary suface
c
      z = (nz - 1) * h
      side = 'z2'
      do 30 j = ly, uy
         y = (j-1)*h
         do 31 i = lx, ux
            x = (i-1)*h
            node = (nz-1)*kz + (j-1)*ky + i
c
            if (al(6) .eq. zero) then
               call clrow(node, a, ja, ia)
               a(iau(node)) = betfun(side,x,y,z)
               rhs(node) = gamfun(side,x,y,z)
            else
               nbr = lctcsr(node, node-kz, ja, ia)
c
               coeff = two*cfun(x,y,z)
               ctr = (coeff + h*ffun(x,y,z))*h/al(6)
               rhs(node) = rhs(node) - ctr * gamfun(side,x,y,z)
               ctr = cfun(x,y,z+hhalf) - ctr * betfun(side,x,y,z)
               coeff = cfun(x,y,z-hhalf)
               a(iau(node)) = a(iau(node)) - coeff + ctr
               a(nbr) = two*coeff
            end if
 31      continue
 30   continue
c
c     all set
c
      return
c-----------------------------------------------------------------------
      end
      subroutine fdreduce(nx,ny,nz,alpha,n,a,ja,ia,iau,rhs,stencil)
      implicit none
      integer nx,ny, nz, n, ia(*), ja(*), iau(*)
      real*8  alpha(*), a(*), rhs(*), stencil(*)
c-----------------------------------------------------------------------
c This subroutine tries to reduce the size of the matrix by looking
c for Dirichlet boundary conditions at each surface and solve the boundary
c value and modify the right-hand side of related nodes, then clapse all
c the boundary nodes.
c-----------------------------------------------------------------------
c     parameters
c
      real*8   zero
      parameter(zero=0.0D0)
c
c     local variables
c
      integer  i,j,k,kx,ky,kz,lx,ux,ly,uy,lz,uz,node,nbnode,lk,ld,iedge
      real*8   val
      integer  lctcsr
      external lctcsr
c
c     The first half of this subroutine will try to change the right-hand
c     side of all the nodes that has a neighbor with Dirichlet boundary
c     condition, since in this case the value of the boundary point is
c     known.
c     Then in the second half, we will try to eliminate the boundary
c     points with known values (with Dirichlet boundary condition).
c
      kx = 1
      ky = nx
      kz = nx*ny
      lx = 1
      ux = nx
      ly = 1
      uy = ny
      lz = 1
      uz = nz
c
c     Here goes the first part. ----------------------------------------
c
c     the left (west) side
c
      if (alpha(1) .eq. zero) then
         lx = 2
         do 10 k = 1, nz
            do 11 j = 1, ny
               node = (k-1)*kz + (j-1)*ky + 1
               nbnode = node + kx
               lk = lctcsr(nbnode, node, ja, ia)
               ld = iau(node)
               val = rhs(node)/a(ld)
c     modify the rhs
               rhs(nbnode) = rhs(nbnode) - a(lk)*val
 11         continue
 10      continue
      endif
c
c     right (east) side
c
      if (alpha(2) .eq. zero) then
         ux = nx - 1
         do 20 k = 1, nz
            do 21 j = 1, ny
               node = (k-1)*kz + (j-1)*ky + nx
               nbnode = node - kx
               lk = lctcsr(nbnode, node, ja, ia)
               ld = iau(node)
               val = rhs(node)/a(ld)
c     modify the rhs
               rhs(nbnode) = rhs(nbnode) - a(lk)*val
 21         continue
 20      continue
      endif
c
c     if it's only 1-D, skip the following part
c
      if (ny .le. 1) goto 100
c
c     the bottom (south) side
c
      if (alpha(3) .eq. zero) then
         ly = 2
         do 30 k = 1, nz
            do 31 i = lx, ux
               node = (k-1)*kz + i
               nbnode = node + ky
               lk = lctcsr(nbnode, node, ja, ia)
               ld = iau(node)
               val = rhs(node)/a(ld)
c     modify the rhs
               rhs(nbnode) = rhs(nbnode) - a(lk)*val
 31         continue
 30      continue
      endif
c
c     top (north) side
c
      if (alpha(4) .eq. zero) then
         uy = ny - 1
         do 40 k = 1, nz
            do 41 i = lx, ux
               node = (k-1)*kz + i + (ny-1)*ky
               nbnode = node - ky
               lk = lctcsr(nbnode, node, ja, ia)
               ld = iau(node)
               val = rhs(node)/a(ld)
c     modify the rhs
               rhs(nbnode) = rhs(nbnode) - a(lk)*val
 41         continue
 40      continue
      endif
c
c     if only 2-D skip the following section on z
c
      if (nz .le. 1) goto 100
c
c     the front surface
c
      if (alpha(5) .eq. zero) then
         lz = 2
         do 50 j = ly, uy
            do 51 i = lx,  ux
               node = (j-1)*ky + i
               nbnode = node + kz
               lk = lctcsr(nbnode, node, ja, ia)
               ld = iau(node)
               val = rhs(node)/a(ld)
c     modify the rhs
               rhs(nbnode) = rhs(nbnode) - a(lk)*val
 51         continue
 50      continue
      endif
c
c     rear surface
c
      if (alpha(6) .eq. zero) then
         uz = nz - 1
         do 60 j = ly, uy
            do 61 i = lx, ux
               node = (nz-1)*kz + (j-1)*ky + i
               nbnode = node - kz
               lk = lctcsr(nbnode, node, ja, ia)
               ld = iau(node)
               val = rhs(node)/a(ld)
c     modify the rhs
               rhs(nbnode) = rhs(nbnode) - a(lk)*val
 61         continue
 60      continue
      endif
c
c     now the second part ----------------------------------------------
c
c     go through all the actual nodes with unknown values, collect all
c     of them to form a new matrix in compressed sparse row format.
c
 100  kx = 1
      ky = ux - lx + 1
      kz = (uy - ly + 1) * ky
      node = 1
      iedge = 1
      do 80 k = lz, uz
         do 81 j = ly, uy
            do 82 i = lx, ux
c
c     the corresponding old node number
               nbnode = ((k-1)*ny + j-1)*nx + i
c
c     copy the row into local stencil, copy is done is the exact
c     same order as the stencil is written into array a
               lk = ia(nbnode)
               if (i.gt.1) then
                  stencil(2) = a(lk)
                  lk = lk + 1
               end if
               if (j.gt.1) then
                  stencil(4) = a(lk)
                  lk = lk + 1
               end if
               if (k.gt.1) then
                  stencil(6) = a(lk)
                  lk = lk + 1
               end if
               stencil(1) = a(lk)
               lk = lk + 1
               if (i.lt.nx) then
                  stencil(3) = a(lk)
                  lk = lk + 1
               endif
               if (j.lt.ny) then
                  stencil(5) = a(lk)
                  lk = lk + 1
               end if
               if (k.lt.nz) stencil(7) = a(lk)
c
c     first the ia pointer -- points to the beginning of each row
               ia(node) = iedge
c
c     move the values from the local stencil to the new matrix
c
c     the neighbor on the left (west)
               if (i.gt.lx) then
                  ja(iedge)=node-kx
                  a(iedge) =stencil(2)
                  iedge=iedge + 1
               end if
c     the neighbor below (south)
               if (j.gt.ly) then
                  ja(iedge)=node-ky
                  a(iedge)=stencil(4)
                  iedge=iedge + 1
               end if
c     the neighbor in the front
               if (k.gt.lz) then
                  ja(iedge)=node-kz
                  a(iedge)=stencil(6)
                  iedge=iedge + 1
               endif
c     center node (itself)
               ja(iedge) = node
               iau(node) = iedge
               a(iedge) = stencil(1)
               iedge = iedge + 1
c     the neighbor to the right (east)
               if (i.lt.ux) then
                  ja(iedge)=node+kx
                  a(iedge)=stencil(3)
                  iedge=iedge + 1
               end if
c     the neighbor above (north)
               if (j.lt.uy) then
                  ja(iedge)=node+ky
                  a(iedge)=stencil(5)
                  iedge=iedge + 1
               end if
c     the neighbor at the back
               if (k.lt.uz) then
                  ja(iedge)=node+kz
                  a(iedge)=stencil(7)
                  iedge=iedge + 1
               end if
c     the right-hand side
               rhs(node) = rhs(nbnode)
c------next node -------------------------
               node=node+1
c
 82         continue
 81      continue
 80   continue
c
      ia(node) = iedge
c
c     the number of nodes in the final matrix is stored in n
c
      n = node - 1
      return
c-----------------------------------------------------------------------
      end
c-----end-of-dqgmres
c-----------------------------------------------------------------------
      subroutine fgmres(n, rhs, sol, ipar, fpar, w)
      implicit none
      integer n, ipar(16)
      real*8 rhs(n), sol(n), fpar(16), w(*)
c-----------------------------------------------------------------------
c     This a version of FGMRES implemented with reverse communication.
c
c     ipar(5) == the dimension of the Krylov subspace
c
c     the space of the `w' is used as follows:
c     >> V: the bases for the Krylov subspace, size n*(m+1);
c     >> W: the above bases after (left-)multiplying with the
c     right-preconditioner inverse, size m*n;
c     >> a temporary vector of size n;
c     >> the Hessenberg matrix, only the upper triangular portion
c     of the matrix is stored, size (m+1)*m/2 + 1
c     >> three vectors, first two are of size m, they are the cosine
c     and sine of the Givens rotations, the third one holds the
c     residuals, it is of size m+1.
c
c     TOTAL SIZE REQUIRED == n*(2m+1) + (m+1)*m/2 + 3*m + 2
c     Note: m == ipar(5). The default value for this is 15 if
c     ipar(5) <= 1.
c-----------------------------------------------------------------------
c     external functions used
c
      real*8 distdot
      external distdot
c
      real*8 one, zero
      parameter(one=1.0D0, zero=0.0D0)
c
c     local variables, ptr and p2 are temporary pointers,
c     hess points to the Hessenberg matrix,
c     vc, vs point to the cosines and sines of the Givens rotations
c     vrn points to the vectors of residual norms, more precisely
c     the right hand side of the least square problem solved.
c
      integer i,ii,idx,iz,k,m,ptr,p2,hess,vc,vs,vrn
      real*8 alpha, c, s
      logical lp, rp
      save
c
c     check the status of the call
c
      if (ipar(1).le.0) ipar(10) = 0
      goto (10, 20, 30, 40, 50, 60) ipar(10)
c
c     initialization
c
      if (ipar(5).le.1) then
         m = 15
      else
         m = ipar(5)
      endif
      idx = n * (m+1)
      iz = idx + n
      hess = iz + n*m
      vc = hess + (m+1) * m / 2 + 1
      vs = vc + m
      vrn = vs + m
      i = vrn + m + 1
      call bisinit(ipar,fpar,i,1,lp,rp,w)
      if (ipar(1).lt.0) return
c
c     request for matrix vector multiplication A*x in the initialization
c
 100  ipar(1) = 1
      ipar(8) = n+1
      ipar(9) = 1
      ipar(10) = 1
      k = 0
      do ii = 1, n
         w(ii+n) = sol(ii)
      enddo
      return
 10   ipar(7) = ipar(7) + 1
      ipar(13) = ipar(13) + 1
      fpar(11) = fpar(11) + n
      if (lp) then
         do i = 1, n
            w(n+i) = rhs(i) - w(i)
         enddo
         ipar(1) = 3
         ipar(10) = 2
         return
      else
         do i = 1, n
            w(i) = rhs(i) - w(i)
         enddo
      endif
c
 20   alpha = sqrt(distdot(n,w,1,w,1))
      fpar(11) = fpar(11) + n + n
      if (ipar(7).eq.1 .and. ipar(3).ne.999) then
         if (abs(ipar(3)).eq.2) then
            fpar(4) = fpar(1) * sqrt(distdot(n,rhs,1,rhs,1)) + fpar(2)
            fpar(11) = fpar(11) + 2*n
         else
            fpar(4) = fpar(1) * alpha + fpar(2)
         endif
         fpar(3) = alpha
      endif
      fpar(5) = alpha
      w(vrn+1) = alpha
      if (alpha.le.fpar(4) .and. ipar(3).ge.0 .and. ipar(3).ne.999) then
         ipar(1) = 0
         fpar(6) = alpha
         goto 300
      endif
      alpha = one / alpha
      do ii = 1, n
         w(ii) = w(ii) * alpha
      enddo
      fpar(11) = fpar(11) + n
c
c     request for (1) right preconditioning
c     (2) matrix vector multiplication
c     (3) left preconditioning
c
 110  k = k + 1
      if (rp) then
         ipar(1) = 5
         ipar(8) = k*n - n + 1
         ipar(9) = iz + ipar(8)
         ipar(10) = 3
         return
      else
         do ii = 0, n-1
            w(iz+k*n-ii) = w(k*n-ii)
         enddo
      endif
c
 30   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = (k-1)*n + 1
      endif
      if (lp) then
         ipar(9) = idx + 1
      else
         ipar(9) = 1 + k*n
      endif
      ipar(10) = 4
      return
c
 40   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = k*n + 1
         ipar(10) = 5
         return
      endif
c
c     Modified Gram-Schmidt orthogonalization procedure
c     temporary pointer 'ptr' is pointing to the current column of the
c     Hessenberg matrix. 'p2' points to the new basis vector
c
 50   ptr = k * (k - 1) / 2 + hess
      p2 = ipar(9)
      ipar(7) = ipar(7) + 1
      call mgsro(.false.,n,n,k+1,k+1,fpar(11),w,w(ptr+1),
     $     ipar(12))
      if (ipar(12).lt.0) goto 200
c
c     apply previous Givens rotations and generate a new one to eliminate
c     the subdiagonal element.
c
      p2 = ptr + 1
      do i = 1, k-1
         ptr = p2
         p2 = p2 + 1
         alpha = w(ptr)
         c = w(vc+i)
         s = w(vs+i)
         w(ptr) = c * alpha + s * w(p2)
         w(p2) = c * w(p2) - s * alpha
      enddo
      call givens(w(p2), w(p2+1), c, s)
      w(vc+k) = c
      w(vs+k) = s
      p2 = vrn + k
      alpha = - s * w(p2)
      w(p2) = c * w(p2)
      w(p2+1) = alpha
      fpar(11) = fpar(11) + 6 * k
c
c     end of one Arnoldi iteration, alpha will store the estimated
c     residual norm at current stage
c
      alpha = abs(alpha)
      fpar(5) = alpha
      if (k.lt.m .and. .not.(ipar(3).ge.0 .and. alpha.le.fpar(4))
     +      .and. (ipar(6).le.0 .or. ipar(7).lt.ipar(6))) goto 110
c
c     update the approximate solution, first solve the upper triangular
c     system, temporary pointer ptr points to the Hessenberg matrix,
c     p2 points to the right-hand-side (also the solution) of the system.
c
 200  ptr = hess + k * (k + 1 ) / 2
      p2 = vrn + k
      if (w(ptr).eq.zero) then
c
c     if the diagonal elements of the last column is zero, reduce k by 1
c     so that a smaller trianguler system is solved [It should only
c     happen when the matrix is singular!]
c
         k = k - 1
         if (k.gt.0) then
            goto 200
         else
            ipar(1) = -3
            ipar(12) = -4
            goto 300
         endif
      endif
      w(p2) = w(p2) / w(ptr)
      do i = k-1, 1, -1
         ptr = ptr - i - 1
         do ii = 1, i
            w(vrn+ii) = w(vrn+ii) - w(p2) * w(ptr+ii)
         enddo
         p2 = p2 - 1
         w(p2) = w(p2) / w(ptr)
      enddo
c
      do i = 0, k-1
         ptr = iz+i*n
         do ii = 1, n
            sol(ii) = sol(ii) + w(p2)*w(ptr+ii)
         enddo
         p2 = p2 + 1
      enddo
      fpar(11) = fpar(11) + 2*k*n + k*(k+1)
c
c     process the complete stopping criteria
c
      if (ipar(3).eq.999) then
         ipar(1) = 10
         ipar(8) = -1
         ipar(9) = idx + 1
         ipar(10) = 6
         return
      else if (ipar(3).lt.0) then
         if (ipar(7).le.m+1) then
            fpar(3) = abs(w(vrn+1))
            if (ipar(3).eq.-1) fpar(4) = fpar(1)*fpar(3)+fpar(2)
         endif
         fpar(6) = abs(w(vrn+k))
      else if (ipar(3).ne.999) then
         fpar(6) = fpar(5)
      endif
c
c     do we need to restart ?
c
 60   if (ipar(12).ne.0) then
         ipar(1) = -3
         goto 300
      endif
      if ((ipar(7).lt.ipar(6) .or. ipar(6).le.0).and.
     +     ((ipar(3).eq.999.and.ipar(11).eq.0) .or.
     +     (ipar(3).ne.999.and.fpar(6).gt.fpar(4)))) goto 100
c
c     termination, set error code, compute convergence rate
c
      if (ipar(1).gt.0) then
         if (ipar(3).eq.999 .and. ipar(11).eq.1) then
            ipar(1) = 0
         else if (ipar(3).ne.999 .and. fpar(6).le.fpar(4)) then
            ipar(1) = 0
         else if (ipar(7).ge.ipar(6) .and. ipar(6).gt.0) then
            ipar(1) = -1
         else
            ipar(1) = -10
         endif
      endif
 300  if (fpar(3).ne.zero .and. fpar(6).ne.zero .and.
     $     ipar(7).gt.ipar(13)) then
         fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7)-ipar(13))
      else
         fpar(7) = zero
      endif
      return
      end
c-----------------------------------------------------------------------
      subroutine filter(n,job,drptol,a,ja,ia,b,jb,ib,len,ierr)
      real*8 a(*),b(*),drptol
      integer ja(*),jb(*),ia(*),ib(*),n,job,len,ierr
c-----------------------------------------------------------------------
c     This module removes any elements whose absolute value
c     is small from an input matrix A and puts the resulting
c     matrix in B.  The input parameter job selects a definition
c     of small.
c-----------------------------------------------------------------------
c on entry:
c---------
c  n	 = integer. row dimension of matrix
c  job   = integer. used to determine strategy chosen by caller to
c         drop elements from matrix A. 
c          job = 1  
c              Elements whose absolute value is less than the
c              drop tolerance are removed.
c          job = 2
c              Elements whose absolute value is less than the 
c              product of the drop tolerance and the Euclidean
c              norm of the row are removed. 
c          job = 3
c              Elements whose absolute value is less that the
c              product of the drop tolerance and the largest
c              element in the row are removed.
c 
c drptol = real. drop tolerance used for dropping strategy.
c a	
c ja
c ia     = input matrix in compressed sparse format
c len	 = integer. the amount of space available in arrays b and jb.
c
c on return:
c---------- 
c b	
c jb
c ib    = resulting matrix in compressed sparse format. 
c 
c ierr	= integer. containing error message.
c         ierr .eq. 0 indicates normal return
c         ierr .gt. 0 indicates that there is'nt enough
c         space is a and ja to store the resulting matrix.
c         ierr then contains the row number where filter stopped.
c note:
c------ This module is in place. (b,jb,ib can ne the same as 
c       a, ja, ia in which case the result will be overwritten).
c----------------------------------------------------------------------c
c           contributed by David Day,  Sep 19, 1989.                   c
c----------------------------------------------------------------------c
c local variables
      real*8 norm,loctol
      integer index,row,k,k1,k2 
c
      index = 1
      do 10 row= 1,n
         k1 = ia(row)
         k2 = ia(row+1) - 1
         ib(row) = index
	 goto (100,200,300) job
 100     norm = 1.0d0
         goto 400
 200     norm = 0.0d0
         do 22 k = k1,k2
            norm = norm + a(k) * a(k)
 22      continue
         norm = sqrt(norm)
         goto 400
 300     norm = 0.0d0
         do 23 k = k1,k2
            if( abs(a(k))  .gt. norm) then
               norm = abs(a(k))
            endif
 23      continue
 400     loctol = drptol * norm
	 do 30 k = k1,k2
	    if( abs(a(k)) .gt. loctol)then 
               if (index .gt. len) then
               ierr = row 
               return
            endif
            b(index) =  a(k)
            jb(index) = ja(k)
            index = index + 1
         endif
 30   continue
 10   continue
      ib(n+1) = index
      return
c--------------------end-of-filter -------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine filterm (n,job,drop,a,ja,b,jb,len,ierr)
      real*8 a(*),b(*),drop
      integer ja(*),jb(*),n,job,len,ierr
c-----------------------------------------------------------------------
c     This subroutine removes any elements whose absolute value
c     is small from an input matrix A. Same as filter but
c     uses the MSR format.
c-----------------------------------------------------------------------
c on entry:
c---------
c  n	 = integer. row dimension of matrix
c  job   = integer. used to determine strategy chosen by caller to
c         drop elements from matrix A. 
c          job = 1  
c              Elements whose absolute value is less than the
c              drop tolerance are removed.
c          job = 2
c              Elements whose absolute value is less than the 
c              product of the drop tolerance and the Euclidean
c              norm of the row are removed. 
c          job = 3
c              Elements whose absolute value is less that the
c              product of the drop tolerance and the largest
c              element in the row are removed.
c 
c drop = real. drop tolerance used for dropping strategy.
c a	
c ja     = input matrix in Modifief Sparse Row format
c len	 = integer. the amount of space in arrays b and jb.
c
c on return:
c---------- 
c
c b, jb = resulting matrix in Modifief Sparse Row format
c 
c ierr	= integer. containing error message.
c         ierr .eq. 0 indicates normal return
c         ierr .gt. 0 indicates that there is'nt enough
c         space is a and ja to store the resulting matrix.
c         ierr then contains the row number where filter stopped.
c note:
c------ This module is in place. (b,jb can ne the same as 
c       a, ja in which case the result will be overwritten).
c----------------------------------------------------------------------c
c           contributed by David Day,  Sep 19, 1989.                   c
c----------------------------------------------------------------------c
c local variables
c
      real*8 norm,loctol
      integer index,row,k,k1,k2 
c
      index = n+2
      do 10 row= 1,n
         k1 = ja(row)
         k2 = ja(row+1) - 1
         jb(row) = index
	 goto (100,200,300) job
 100     norm = 1.0d0
         goto 400
 200     norm = a(row)**2 
         do 22 k = k1,k2
            norm = norm + a(k) * a(k)
 22      continue
         norm = sqrt(norm)
         goto 400
 300     norm = abs(a(row)) 
         do 23 k = k1,k2
            norm = max(abs(a(k)),norm) 
 23      continue
 400     loctol = drop * norm
	 do 30 k = k1,k2
	    if( abs(a(k)) .gt. loctol)then 
               if (index .gt. len) then
                  ierr = row 
                  return
               endif
               b(index) =  a(k)
               jb(index) = ja(k)
               index = index + 1
            endif
 30      continue
 10   continue
      jb(n+1) = index
      return
c--------------------end-of-filterm-------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine find_ctr(n,nsiz,ja,ia,init,mask,maskval,riord,
     *     levels,center,iwk) 
      implicit none
      integer n,nsiz,ja(*),ia(*),init,mask(*),maskval,riord(*),
     *     levels(*),center,iwk(*) 
c-----------------------------------------------------------------------
c     finds a center point of a subgraph -- 
c-----------------------------------------------------------------------
c     n, ja, ia = graph
c     nsiz = size of current domain.
c     init = initial node in search
c     mask
c     maskval 
c-----------------------------------------------------------------------
c     local variables 
      integer midlev, nlev,newmask, k, kr, kl, init0, nlev0  
      call perphn(n,ja,ia,init,mask,maskval,nlev,riord,levels)
c-----------------------------------------------------------------------
c     midlevel = level which cuts domain into 2 roughly equal-size 
c     regions 
c
      midlev = 1
      k = 0 
 1    continue
      k = k + levels(midlev+1)-levels(midlev) 
      if (k*2 .lt. nsiz) then
         midlev = midlev+1
         goto 1 
      endif
c-----------------------------------------------------------------------
      newmask = n+maskval
c     
c     assign temporary masks to mid-level elements
c     
      do k=levels(midlev),levels(midlev+1)-1
         mask(riord(k)) = newmask
      enddo
c     
c     find pseudo-periph node for mid-level `line'
c
      kr = 1
      kl = kr + nsiz 
      init0 = riord(levels(midlev))
      call perphn(n,ja,ia,init0,mask,newmask,nlev0,iwk(kr),iwk(kl)) 
c-----------------------------------------------------------------------
c     restore  mask to initial state 
c-----------------------------------------------------------------------
      do k=levels(midlev),levels(midlev+1)-1
         mask(riord(k)) = maskval 
      enddo
c-----------------------------------------------------------------------
c     define center 
c-----------------------------------------------------------------------  
      midlev = 1 + (nlev0-1)/2
      k = iwk(kl+midlev-1)
      center = iwk(k) 
c----------------------------------------------------------------------- 
      return 
      end 
c----------------------------------------------------------------------
      subroutine FixHeap (a,ind,rind,jkey,vacant,last)
      integer a(*),ind(*),rind(*),jkey,vacant,last
c----------------------------------------------------------------------
c     inserts a key (key and companion index) at the vacant position 
c     in a (min) heap -
c arguments
c     a(1:last)    = real array
c     ind(1:last)  = integer array -- permutation of initial data
c     rind(1:last) = integer array -- reverse permutation 
c     jkey         = position of key to be inserted. a(jkey) 
c                    will be inserted into the heap
c     vacant       = vacant where a key is to be inserted
c     last         = number of elements in the heap.
c----------------------------------------------------------------------
c local variables 
c
      integer child,lchild,rchild,xkey
      xkey = a(jkey) 
      ikey = ind(jkey) 
      lchild = 2*vacant
 1    continue 
      rchild = lchild+1  
      child = lchild  
      if (rchild .le. last .and. a(rchild) .lt. a(child))
     *     child = rchild 
      if (xkey .le. a(child) .or. child .gt. last) goto 2 
      a(vacant) = a(child) 
      ind(vacant) = ind(child) 
      rind(ind(vacant)) = vacant 
      vacant = child 
      lchild = 2*vacant 
      if (lchild .le.  last) goto 1
 2    continue 
      a(vacant) = xkey 
      ind(vacant) = ikey 
      rind(ikey) = vacant 
      return
c----------------------------------------------------------------------      
      end
c----------------------------------------------------------------------
      subroutine FixHeapM (a,ind,rind,jkey,vacant,last)
      integer a(*),ind(*),rind(*),jkey,vacant,last
c----      
c     inserts a key (key and companion index) at the vacant position 
c     in a heap -  THIS IS A MAX HEAP VERSION
c arguments
c     a(1:last)    = real array
c     ind(1:last)  = integer array -- permutation of initial data
c     rind(1:last) = integer array -- reverse permutation 
c     jkey         = position of key to be inserted. a(jkey) 
c                    will be inserted into the heap
c     vacant       = vacant where a key is to be inserted
c     last         = number of elements in the heap.
c----     
c local variables 
c
      integer child,lchild,rchild,xkey
      xkey = a(jkey) 
      ikey = ind(jkey) 
      lchild = 2*vacant
 1    continue 
      rchild = lchild+1  
      child = lchild  
      if (rchild .le. last .and. a(rchild) .gt. a(child))
     *     child = rchild 
      if (xkey .ge. a(child) .or. child .gt. last) goto 2
      a(vacant) = a(child) 
      ind(vacant) = ind(child) 
      rind(ind(vacant)) = vacant 
      vacant = child 
      lchild = 2*vacant 
      if (lchild .le.  last) goto 1
 2    continue 
      a(vacant) = xkey 
      ind(vacant) = ikey 
      rind(ikey) = vacant 
      return
      end
c----------------------------------------------------------------------- 
      subroutine fmesh1 (nx,nelx,node,x,y,nodcode,ijk)
c--------------------------------------------------------------
c 
c initial mesh for a simple square with two elemnts
c      3             4
c       --------------
c       |          . |
c       |   2    .   |
c       |      .     |
c       |   .    1   |
c       | .          |
c       --------------
c      1              2
c--------------------------------------------------------------
c input parameters: node = first dimensoin of ijk (must be .ge. 3)
c output parameters:
c    nx    = number of nodes
c    nelx = number of elemnts
c    (x(1:nx), y(1:nx)) = coordinates of nodes
c    nodcode(1:nx) = integer code for each node with the 
c	    following meening:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point.
c   ijk(1:3,1:nelx) = connectivity matrix. for a given element
c	    number nel, ijk(k,nel), k=1,2,3 represent the nodes
c	    composing the element nel.
c--------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension x(*),y(*),nodcode(*),ijk(node,*)
      real*8 x1(4),y1(4)
      integer ijk1(2),ijk2(2),ijk3(2)
c--------------------------------------------------------------
c coordinates of nodal points
c--------------------------------------------------------------
      data  x1/0.0, 1.0, 0.0, 1.0/
      data  y1/0.0, 0.0, 1.0, 1.0/
c
c------------------|--|
c elements         1  2
c------------------|--|
      data ijk1   /1, 1/
      data ijk2   /2, 4/
      data ijk3   /4, 3/
c
      nx = 4
c
      do 1 k=1, nx
         x(k) = x1(k)
         y(k) = y1(k)
         nodcode(k) = 1
 1    continue
c     
      nodcode(2) = 2
      nodcode(3) = 2
c
      nelx = 2
c
      do  2 k=1,nelx
         ijk(1,k) = ijk1(k) 
         ijk(2,k) = ijk2(k)
         ijk(3,k) = ijk3(k)
 2    continue
c
      return
      end
c-----------------------------------------------------------------------
      subroutine fmesh2 (nx,nelx,node,x,y,nodcode,ijk)
c---------------------------------------------------------------
c initial mesh for a simple D-shaped region with 4 elemnts
c       6
c       | .
c       |    .
c       |      .
c       |   4     .
c       |           .
c     4 -------------- 5
c       |          . |
c       |   3    .   |
c       |      .     |
c       |   .    2   |
c       | .          |
c       --------------
c       | 2         . 3
c       |         .
c       |   1   .
c       |     .
c       |  .
c       |.
c       1  
c--------------------------------------------------------------
c input parameters: node = first dimensoin of ijk (must be .ge. 3)
c output parameters:
c    nx    = number of nodes
c    nelx = number of elemnts
c    (x(1:nx), y(1:nx)) = coordinates of nodes
c    nodcode(1:nx) = integer code for each node with the 
c	    following meening:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point.
c   ijk(1:3,1:nelx) = connectivity matrix. for a given element
c	    number nel, ijk(k,nel), k=1,2,3 represent the nodes
c	    composing the element nel.
c
c--------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension x(*),y(*),nodcode(*),ijk(node,*)
      real*8 x1(6),y1(6)
      integer ijk1(4),ijk2(4),ijk3(4)
c--------------------------------------------------------------
c coordinates of nodal points
c--------------------------------------------------------------
      data x1/0.0, 0.0, 1.0, 0.0, 1.0, 0.0/
      data y1/0.0, 1.0, 1.0, 2.0, 2.0, 3.0/
c
c------------------|--|--|--|
c elements         1  2  3  4
c------------------|--|--|--|
      data ijk1   /1, 2, 2, 4/
      data ijk2   /3, 3, 5, 5/
      data ijk3   /2, 5, 4, 6/
c
      nx = 6
c
      do 1 k=1, nx
         x(k) = x1(k)
         y(k) = y1(k)
         nodcode(k) = 1
 1    continue
c
      nelx = 4
c     
      do  2 k=1,nelx
         ijk(1,k) = ijk1(k) 
         ijk(2,k) = ijk2(k)
         ijk(3,k) = ijk3(k)
 2    continue
c
      return
      end
c-----------------------------------------------------------------------
      subroutine fmesh3 (nx,nelx,node,x,y,nodcode,ijk)
c---------------------------------------------------------------
c initial mesh for a C-shaped region composed of 10 elements --
c
c
c      10           11            12             
c       ---------------------------
c       |          . |          . |
c       |  7     .   |   9    .   |
c       |      .     |      .     |
c       |   .    8   |   .   10   |
c       | .          | .          |
c     7 ---------------------------
c       |          . |8           9
c       |   5    .   |
c       |      .     |
c       |   .    6   |
c     4 | .          |5           6
c       ---------------------------
c       |          . |          . |
c       |   1    .   |  3     .   |
c       |      .     |      .     |
c       |   .    2   |   .   4    |
c       | .          | .          |
c       ---------------------------
c      1             2            3
c
c--------------------------------------------------------------
c input parameters: node = first dimensoin of ijk (must be .ge. 3)
c    nx    = number of nodes
c    nelx = number of elemnts
c    (x(1:nx), y(1:nx)) = coordinates of nodes
c    nodcode(1:nx) = integer code for each node with the 
c	    following meening:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point.
c   ijk(1:3,1:nelx) = connectivity matrix. for a given element
c	    number nel, ijk(k,nel), k=1,2,3 represent the nodes
c	    composing the element nel.
c
c--------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension x(*),y(*),nodcode(*),ijk(node,*)
      real*8 x1(12),y1(12)
      integer ijk1(10),ijk2(10),ijk3(10)
c--------------------------------------------------------------
c coordinates of nodal points
c--------------------------------------------------------------
      data x1/0.0,1.0,2.0,0.0,1.0,2.0,0.0,1.0,2.0,0.0,1.0,2.0/
      data y1/0.0,0.0,0.0,1.0,1.0,1.0,2.0,2.0,2.0,3.0,3.0,3.0/
c
c------------------|--|--|--|--|--|--|---|---|---|
c elements         1  2  3  4  5  6  7   8   9  10
c------------------|--|--|--|--|--|--|---|---|---|
      data ijk1   /1, 1, 2, 2, 4, 4, 7,  7,  8, 8/
      data ijk2   /5, 2, 6, 3, 8, 5, 11, 8, 12, 9/
      data ijk3   /4, 5, 5, 6, 7, 8, 10, 11,11, 12/
c
      nx = 12
c
      do 1 k=1, nx
         x(k) = x1(k)
         y(k) = y1(k)
         nodcode(k) = 1
 1    continue
c
      nodcode(3) = 2
      nodcode(10) = 2
      nodcode(9) = 2
c     
      nelx = 10
c     
      do  2 k=1,nelx
         ijk(1,k) = ijk1(k) 
         ijk(2,k) = ijk2(k)
         ijk(3,k) = ijk3(k)
 2    continue
c
      return
      end
c-----------------------------------------------------------------------
      subroutine fmesh4 (nx,nelx,node,x,y,nodcode,ijk)
c----------------------------------------------------------------------- 
c initial mesh for a C-shaped region composed of 10 elements --
c      10                   11 
c       +------------------+ .
c       | .                |    .
c       |    .       8     |       . 12
c       |        .         |  9   . |
c       |     7      .     |   .    |
c     7 |                . | .   10 |
c       -------------------+--------+ 9
c       |                 .| 8 
c       |     5       .    |
c       |         .        |
c       |    .       6     |
c       |.                 | 5      6
c    4  +------------------+--------+ 
c       |               .  | .   4  |
c       |    1       .     |    .   |
c       |        .         |  3    .| 3
c       |    .        2    |    .
c       | .                | .
c       -------------------- 
c       1                  2 
c--------------------------------------------------------------
c input parameters: node = first dimensoin of ijk (must be .ge. 3)
c    nx    = number of nodes
c    nelx = number of elemnts
c    (x(1:nx), y(1:nx)) = coordinates of nodes
c    nodcode(1:nx) = integer code for each node with the 
c	    following meening:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point.
c   ijk(1:3,1:nelx) = connectivity matrix. for a given element
c	    number nel, ijk(k,nel), k=1,2,3 represent the nodes
c	    composing the element nel.
c
c--------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension x(*),y(*),nodcode(*),ijk(node,*)
      real*8 x1(12),y1(12)
      integer ijk1(10),ijk2(10),ijk3(10)
c--------------------------------------------------------------
c coordinates of nodal points
c--------------------------------------------------------------
      data x1/0.0,1.0,1.5,0.0,1.0,1.5,0.0,1.0,1.5,0.0,1.0,1.5/
      data y1/0.0,0.0,0.5,1.0,1.0,1.0,2.0,2.0,2.0,3.0,3.0,2.5/
c
c------------------|--|--|--|--|--|--|---|---|---|
c elements         1  2  3  4  5  6  7   8   9  10
c------------------|--|--|--|--|--|--|---|---|---|
      data ijk1   /1, 1, 2, 5, 4, 4, 7, 10,  8, 8/
      data ijk2   /5, 2, 3, 3, 8, 5, 8,  8, 12, 9/
      data ijk3   /4, 5, 5, 6, 7, 8, 10, 11,11, 12/
c
      nx = 12
c     
      do 1 k=1, nx
         x(k) = x1(k)
         y(k) = y1(k)
         nodcode(k) = 1
 1    continue
c
      nodcode(6) = 2
      nodcode(9) = 2
c
      nelx = 10
c     
      do  2 k=1,nelx
         ijk(1,k) = ijk1(k) 
         ijk(2,k) = ijk2(k)
         ijk(3,k) = ijk3(k)
 2    continue
c     
      return
      end
c-----------------------------------------------------------------------
      subroutine fmesh5 (nx,nelx,node,x,y,nodcode,ijk)
c---------------------------------------------------------------
c     initial mesh for a whrench shaped region composed of 14 elements --
c     
c                                      13            15    
c                                        . ----------.           |-3
c                                      .   .   13  .   .         |
c                                   .   12   .   .  14    .      |
c 9        10        11       12  .            . 14        . 16  |
c ----------------------------------------------------------     |-2
c |       . |       . |       . |            . |                 |  
c | 1   .   |  3  .   |  5  .   |    7   .     |                 |
c |   .  2  |   .  4  |   .  6  |     .    8   |                 |
c |.        |.        |.        | .            |                 |
c -----------------------------------------------------------    |-1
c 1         2         3       4  .           6 .           . 8   |
c                                   .   9    .   .   11   .      |
c                                      .   .  10    .   .        |
c                                        .___________.           |-0
c                                       5             7
c
c 0---------1--------2----------3--------------4-------------5
c--------------------------------------------------------------
c input parameters: node = first dimensoin of ijk (must be .ge. 3)
c    nx    = number of nodes
c    nelx = number of elemnts
c    (x(1:nx), y(1:nx)) = coordinates of nodes
c    nodcode(1:nx) = integer code for each node with the 
c	    following meening:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point.
c   ijk(1:3,1:nelx) = connectivity matrix. for a given element
c	    number nel, ijk(k,nel), k=1,2,3 represent the nodes
c	    composing the element nel.
c
c--------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension x(*),y(*),nodcode(*),ijk(node,*)
      real*8 x1(16),y1(16)
      integer ijk1(14),ijk2(14),ijk3(14)
c--------------------------------------------------------------
c     coordinates of nodal points
c--------------------------------------------------------------
      data x1/0.,1.,2.,3.,3.5,4.,4.5,5.,0.,1.,2.,3.,3.5,4.,4.5,5./
      data y1/1.,1.,1.,1.,0.,1.,0.,1.,2.,2.,2.,2.,3.,2.,3.,2./
c     
c------------------|--|--|--|--|--|--|---|---|---|--|---|---|---|
c elements         1  2  3  4  5  6  7   8   9  10  11  12  13  14
c------------------|--|--|--|--|--|--|---|---|---|--|---|---|---|
      data ijk1   /1, 1, 2, 2, 3, 3, 4,  4,  4,  5, 6, 12, 14, 14/
      data ijk2   /10,2,11, 3,12, 4,14,  6,  5,  7, 7, 14, 15, 16/
      data ijk3   /9,10,10,11,11,12,12, 14,  6,  6, 8, 13, 13, 15/
c
      nx = 16
c     
      do 1 k=1, nx
         x(k) = x1(k)
         y(k) = y1(k)
         nodcode(k) = 1
 1    continue
c     
      nodcode(9) = 2
      nodcode(8) = 2
      nodcode(16) = 2
c     
      nelx = 14
c     
      do  2 k=1,nelx
         ijk(1,k) = ijk1(k) 
         ijk(2,k) = ijk2(k)
         ijk(3,k) = ijk3(k)
 2    continue
c     
      return
      end
c----------------------------------------------------------------------- 
      subroutine fmesh6 (nx,nelx,node,x,y,nodcode,ijk)
c---------------------------------------------------------------
c this generates a finite element mesh for an ellipse-shaped 
c domain.
c---------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension x(*),y(*),nodcode(*),ijk(node,*), ijktr(200,3)
      integer nel(200) 
c--------------------------------------------------------------
c     coordinates of nodal points
c--------------------------------------------------------------
      nd = 8 
      nr = 3
c     
c     define axes of ellipse
c     
      a = 2.0
      b = 1.30 
c     
      nx = 1
      pi = 4.0* atan(1.0) 
      theta = 2.0 * pi / real(nd)  
      x(1) = 0.0
      y(1) = 0.0 
      delr = a / real(nr)
      nx = 0
      do i = 1, nr 
         ar = real(i)*delr
         br = ar*b / a
         do j=1, nd 
            nx = nx+1
            x(nx) = a +ar*cos(real(j)*theta)
            y(nx) = b +br*sin(real(j)*theta)
c            write (13,*) ' nod ', nx, ' x,y', x(nx), y(nx) 
            nodcode(nx) = 0
            if (i .eq. nr) nodcode(nx) = 1
         enddo
      enddo
c     
      nemax = 200
      call dlauny(x,y,nx,ijktr,nemax,nelx)
c     
c     print *, ' delauny -- nx, nelx ', nx, nelx
      do 3 j=1,nx
         nel(j) = 0
 3    continue
c     transpose ijktr into ijk and count the number of 
c     elemnts to which each node belongs
c     
      do 4 j=1, nelx
         do 41 k=1, node
	    i = ijktr(j,k)
	    ijk(k,j) = i
	    nel(i) = nel(i)+1
 41      continue
 4    continue
c     
c     take care of ordering within each element
c     
      call chkelmt (nx, x, y, nelx, ijk, node)
c     
      return
      end
c--------------------------------------------------------
      subroutine fmesh7 (nx,nelx,node,x,y,nodcode,ijk,iperm)
      implicit none 
      real*8 x(*),y(*) 
      integer nx,nelx,node,nodcode(nx),ijk(node,nelx),iperm(nx) 
c---------------------------------------------------------------
c     this generates a U-shaped domain with an elliptic inside.
c     then a Delauney triangulation is used to generate the mesh.
c     mesh needs to be post-processed -- see inmesh -- 
c---------------------------------------------------------------
      integer nr,nsec,i,k,nemax,j,nel(200),ijktr(200,3),nodexc
      real*8 a,b,x1, y1, x2, y2, xcntr,ycntr, rad,pi,delr,xnew,ynew,
     *     arx, ary, cos, sin, theta,excl   
c--------------------------------------------------------------
c     coordinates of nodal points
c--------------------------------------------------------------
      data x1/0.0/,y1/0.0/,x2/6.0/,y2/6.0/,nodexc/1/
      xcntr = x2/3.0
      ycntr = y2/2.0 
      rad = 1.8
      nsec = 20 
      nr = 3 
c
c     exclusion zone near the boundary 
c 
      excl = 0.02*x2 
c-----------------------------------------------------------------------
c     enter the four corner points. 
c-----------------------------------------------------------------------
      nx = 1 
      x(nx) = x1
      y(nx) = y1
      nodcode(nx) = 1
      nx = nx+1
      x(nx) = x2
      y(nx) = y1
      nodcode(nx) = 1
      nx = nx+1
      x(nx) = x2
      y(nx) = y2 
      nodcode(nx) = 1
      nx = nx+1
      x(nx) = x1
      y(nx) = y2
      nodcode(nx) = 1
c     
c     define axes of ellipse 
c     
      a = 2.0
      b = 1.30
c-----------------------------------------------------------------------
      pi = 4.0*atan(1.0) 
      delr = a / real(nr)
      do 2 i = 1, nsec 
         theta = 2.0 * real(i-1) * pi / real(nsec)  
         xnew = xcntr + rad*cos(theta)
         ynew = ycntr + rad*b*sin(theta)/a 
         if ((xnew .ge. x2) .or. (xnew .le. x1) .or. (ynew .ge. y2)
     *        .or. (ynew .le. y1)) goto 2 
         nx = nx+1 
         x(nx) = xnew 
         y(nx) = ynew 
         nodcode(nx) = nodexc 
         arx = delr*cos(theta)
         ary = delr*b*sin(theta)/a 
c     
c     while inside domain do: 
c     
 1       continue 
         xnew = x(nx) + arx 
         ynew = y(nx) + ary 
         if (xnew .ge. x2) then 
            x(nx) = x2 
            nodcode(nx) = 1
         else if (xnew .le. x1) then
            x(nx) = x1 
            nodcode(nx) = 1         
         else if (ynew .ge. y2) then 
            y(nx) = y2 
            nodcode(nx) = 1
         else if (ynew .le. y1) then
            y(nx) = y1 
            nodcode(nx) = 1         
         else
            nx = nx+1 
            x(nx) = xnew 
            y(nx) = ynew 
            nodcode(nx) = 0
            call clos2bdr(nx,xnew,ynew,x,y,x1,x2,y1,y2,excl,nodcode)
         endif 
c        write (13,*) ' nod ', nx, ' x,y', x(nx), y(nx)
c     *         ,' arx--ary ', arx, ary
         arx = arx*1.2 
         ary = ary*1.2
         if (nodcode(nx) .le. 0) goto 1 
 2    continue 
c     
      nemax = 200
      call dlauny(x,y,nx,ijktr,nemax,nelx)
c     
c     print *, ' delauny -- nx, nelx ', nx, nelx
      do 3 j=1,nx
         nel(j) = 0
 3    continue
c     
c     transpose ijktr into ijk and count the number of 
c     elemnts to which each node belongs
c     
      do 4 j=1, nelx
         do 41 k=1, node
	    i = ijktr(j,k)
	    ijk(k,j) = i
	    nel(i) = nel(i)+1
 41      continue
 4    continue
c
c     this mesh needs cleaning up --
c 
      call cleanel (nelx,ijk, node,nodcode,nodexc) 
      call cleannods(nx,x,y,nelx,ijk,node,nodcode,iperm)       
c
c     take care of ordering within each element
c
      call chkelmt (nx, x, y, nelx, ijk, node)
      return
      end
c-----------------------------------------------------------------------
      subroutine fmesh8 (nx,nelx,node,x,y,nodcode,ijk,iperm)
      implicit none 
      real*8 x(*),y(*) 
      integer nx,nelx,node,nodcode(nx),ijk(node,nelx),iperm(nx) 
c---------------------------------------------------------------
c     this generates a small rocket type shape inside a rectangle
c     then a Delauney triangulation is used to generate the mesh.
c     mesh needs to be post-processed -- see inmesh -- 
c---------------------------------------------------------------
      integer nr,nsec,i,k,nemax,j,nel(1500),ijktr(1500,3),nodexc 
      real*8 a,b,x1, y1, x2, y2, xcntr,ycntr, rad,pi,delr,xnew,ynew,
     *     arx, ary, cos, sin, theta,radi,excl 
c--------------------------------------------------------------
c     coordinates of corners + some additional data 
c--------------------------------------------------------------
      data x1/0.0/,y1/0.0/,x2/6.0/,y2/6.0/,nodexc/3/  
      xcntr = 4.0 
      ycntr = y2/2.0 
      rad = 0.6
c
c     exclusion zone near the boundary. 
c 
      excl = 0.02*x2 
      nsec = 30
      nr = 4
c-----------------------------------------------------------------------
c     enter the four corner points. 
c-----------------------------------------------------------------------
      nx = 1 
      x(nx) = x1
      y(nx) = y1
      nodcode(nx) = 1
      nx = nx+1
      x(nx) = x2
      y(nx) = y1
      nodcode(nx) = 1
      nx = nx+1
      x(nx) = x2
      y(nx) = y2 
      nodcode(nx) = 1
      nx = nx+1
      x(nx) = x1
      y(nx) = y2
      nodcode(nx) = 1
c     
c     define axes of ellipse /circle / object
c     
      a = 2.0
      b = 1.0
c-----------------------------------------------------------------------
      pi = 4.0*atan(1.0) 
      delr = 2.0*rad / real(nr)
      do 2 i = 1, nsec 
         theta = 2.0*real(i-1) * pi / real(nsec) 
         if (theta .gt. pi) theta = theta - 2.0*pi 
         radi=rad*(1.0+0.05*((pi/2.0)**2-theta**2)**2)
     *        /(1.0+0.05*(pi/2.0)**4) 
         arx = radi*cos(theta)/real(nr) 
         ary = radi*sin(theta)/real(nr) 
c      a hack!
c         arx = (abs(theta)+0.25)*cos(theta)/real(nr) 
c         ary = (abs(theta)+0.25)*sin(theta)/real(nr) 
c
         xnew = xcntr + radi*cos(theta)
         ynew = ycntr + radi*b*sin(theta)/a 
         if ((xnew .ge. x2) .or. (xnew .le. x1) .or. (ynew .ge. y2)
     *        .or. (ynew .le. y1)) goto 2 
         nx = nx+1 
         x(nx) = xnew 
         y(nx) = ynew 
         nodcode(nx) = nodexc 
c     
c     while inside domain do: 
c     
 1       continue 
         xnew = xnew  + arx 
         ynew = ynew  + ary 
         if (xnew .ge. x2) then 
            x(nx) = x2 
            nodcode(nx) = 1
         else if (xnew .le. x1) then
            x(nx) = x1 
            nodcode(nx) = 1         
         else if (ynew .ge. y2) then 
            y(nx) = y2 
            nodcode(nx) = 1
         else if (ynew .le. y1) then
            y(nx) = y1 
            nodcode(nx) = 1         
c
c     else we can add this as interior point
c 
         else
            nx = nx+1 
            x(nx) = xnew 
            y(nx) = ynew 
            nodcode(nx) = 0
c
c     do something if point is too close to boundary 
c 
            call clos2bdr(nx,xnew,ynew,x,y,x1,x2,y1,y2,excl,nodcode)
         endif
c            
         arx = arx*1.1
         ary = ary*1.1 
         if (nodcode(nx) .eq. 0) goto 1 
 2    continue 
c     
      nemax = 1500 
      call dlauny(x,y,nx,ijktr,nemax,nelx)
c     
      print *, ' delauney -- nx, nelx ', nx, nelx
      do 3 j=1,nx
         nel(j) = 0
 3    continue
c-----------------------------------------------------------------------     
c     transpose ijktr into ijk and count the number of 
c     elemnts to which each node belongs
c----------------------------------------------------------------------- 
      do 4 j=1, nelx
         do 41 k=1, node
	    i = ijktr(j,k)
	    ijk(k,j) = i
	    nel(i) = nel(i)+1
 41      continue
 4    continue
c
c     this mesh needs cleaning up --
c 
      call cleanel (nelx,ijk, node,nodcode,nodexc) 
      call cleannods(nx,x,y,nelx,ijk,node,nodcode,iperm)       
c
c     take care of ordering within each element
c
      call chkelmt (nx, x, y, nelx, ijk, node)
      return
      end
c-----------------------------------------------------------------------  
      subroutine fmesh9 (nx,nelx,node,x,y,nodcode,ijk,iperm)
      implicit none 
      real*8 x(*),y(*) 
      integer nx,nelx,node,nodcode(nx),ijk(node,nelx),iperm(nx) 
c---------------------------------------------------------------
c     this generates a U-shaped domain with an elliptic inside.
c     then a Delauney triangulation is used to generate the mesh.
c     mesh needs to be post-processed -- see inmesh -- 
c---------------------------------------------------------------
      integer nr,nsec,i,k,nemax,j,nel(1500),ijktr(1500,3),nodexc 
      real*8 x1, y1, x2, y2, xcntr,ycntr, rad,pi,delr,xnew,ynew,
     *     arx, ary, cos, sin, theta,excl   
c--------------------------------------------------------------
c     coordinates of nodal points
c--------------------------------------------------------------
      data x1/0.0/,y1/0.0/,x2/11.0/,y2/5.5/,nodexc/3/ 
      xcntr = 1.50
      ycntr = y2/2.0 
      rad = 0.6
      nsec = 30
      nr = 3 
c
c-----------------------------------------------------------------------
c     enter the four corner points. 
c-----------------------------------------------------------------------
      nx = 1 
      x(nx) = x1
      y(nx) = y1
      nodcode(nx) = 1
      nx = nx+1
      x(nx) = x2
      y(nx) = y1
      nodcode(nx) = 1
      nx = nx+1
      x(nx) = x2
      y(nx) = y2 
      nodcode(nx) = 1
      nx = nx+1
      x(nx) = x1
      y(nx) = y2
      nodcode(nx) = 1
c     
c     define axes of ellipse 
c     
c-----------------------------------------------------------------------
      pi = 4.0*atan(1.0) 
      delr = rad / real(nr) 
      do 2 i = 1, nsec 
         theta = 2.0 * real(i-1) * pi / real(nsec)  
         xnew = xcntr + rad*cos(theta)
         ynew = ycntr + rad*sin(theta) 
         if ((xnew .ge. x2) .or. (xnew .le. x1) .or. (ynew .ge. y2)
     *        .or. (ynew .le. y1)) goto 2 
         nx = nx+1 
         x(nx) = xnew 
         y(nx) = ynew 
         nodcode(nx) = nodexc 
         arx = delr*cos(theta)
         ary = delr*sin(theta)
c 
c     exclusion zone near the boundary 
c
c         excl = 0.1*delr 
         excl = 0.15*delr
c     
c     while inside domain do: 
c     
 1       continue 
         xnew = x(nx) + arx 
         ynew = y(nx) + ary 
         if (xnew .ge. x2) then 
            x(nx) = x2 
            nodcode(nx) = 1
         else if (xnew .le. x1) then
            x(nx) = x1 
            nodcode(nx) = 1         
         else if (ynew .ge. y2) then 
            y(nx) = y2 
            nodcode(nx) = 1
         else if (ynew .le. y1) then
            y(nx) = y1 
            nodcode(nx) = 1         
         else
            nx = nx+1 
            x(nx) = xnew 
            y(nx) = ynew 
            nodcode(nx) = 0
            call clos2bdr(nx,xnew,ynew,x,y,x1,x2,y1,y2,excl,nodcode)
         endif 
         arx = arx*1.1 
         ary = ary*1.1
         excl = excl*1.1 
         if (nodcode(nx) .le. 0) goto 1 
 2    continue 
c     
      nemax = 1500
      call dlauny(x,y,nx,ijktr,nemax,nelx)
c     
c     print *, ' delauny -- nx, nelx ', nx, nelx
      do 3 j=1,nx
         nel(j) = 0
 3    continue
c     
c     transpose ijktr into ijk and count the number of 
c     elemnts to which each node belongs
c     
      do 4 j=1, nelx
         do 41 k=1, node
	    i = ijktr(j,k)
	    ijk(k,j) = i
	    nel(i) = nel(i)+1
 41      continue
 4    continue
c
c     this mesh needs cleaning up --
c 
      call cleanel (nelx,ijk, node,nodcode,nodexc) 
      call cleannods(nx,x,y,nelx,ijk,node,nodcode,iperm)       
c
c     take care of ordering within each element
c
      call chkelmt (nx, x, y, nelx, ijk, node)
      return
      end
c-----end-of-tfqmr
c-----------------------------------------------------------------------
      subroutine fom(n, rhs, sol, ipar, fpar, w)
      implicit none
      integer n, ipar(16)
      real*8 rhs(n), sol(n), fpar(16), w(*)
c-----------------------------------------------------------------------
c     This a version of The Full Orthogonalization Method (FOM) 
c     implemented with reverse communication. It is a simple restart 
c     version of the FOM algorithm and is implemented with plane 
c     rotations similarly to GMRES.
c
c  parameters:
c  ----------- 
c     ipar(5) == the dimension of the Krylov subspace
c     after every ipar(5) iterations, the FOM will restart with
c     the updated solution and recomputed residual vector.
c
c     the work space in `w' is used as follows:
c     (1) the basis for the Krylov subspace, size n*(m+1);
c     (2) the Hessenberg matrix, only the upper triangular
c     portion of the matrix is stored, size (m+1)*m/2 + 1
c     (3) three vectors, all are of size m, they are
c     the cosine and sine of the Givens rotations, the third one holds
c     the residuals, it is of size m+1.
c
c     TOTAL SIZE REQUIRED == (n+3)*(m+2) + (m+1)*m/2
c     Note: m == ipar(5). The default value for this is 15 if
c     ipar(5) <= 1.
c-----------------------------------------------------------------------
c     external functions used
c
      real*8 distdot
      external distdot
c
      real*8 one, zero
      parameter(one=1.0D0, zero=0.0D0)
c
c     local variables, ptr and p2 are temporary pointers,
c     hes points to the Hessenberg matrix,
c     vc, vs point to the cosines and sines of the Givens rotations
c     vrn points to the vectors of residual norms, more precisely
c     the right hand side of the least square problem solved.
c
      integer i,ii,idx,k,m,ptr,p2,prs,hes,vc,vs,vrn
      real*8 alpha, c, s 
      logical lp, rp
      save
c
c     check the status of the call
c
      if (ipar(1).le.0) ipar(10) = 0
      goto (10, 20, 30, 40, 50, 60, 70) ipar(10)
c
c     initialization
c
      if (ipar(5).le.1) then
         m = 15
      else
         m = ipar(5)
      endif
      idx = n * (m+1)
      hes = idx + n
      vc = hes + (m+1) * m / 2 + 1
      vs = vc + m
      vrn = vs + m
      i = vrn + m + 1
      call bisinit(ipar,fpar,i,1,lp,rp,w)
      if (ipar(1).lt.0) return
c
c     request for matrix vector multiplication A*x in the initialization
c
 100  ipar(1) = 1
      ipar(8) = n+1
      ipar(9) = 1
      ipar(10) = 1
      k = 0
      do i = 1, n
         w(n+i) = sol(i)
      enddo
      return
 10   ipar(7) = ipar(7) + 1
      ipar(13) = ipar(13) + 1
      if (lp) then
         do i = 1, n
            w(n+i) = rhs(i) - w(i)
         enddo
         ipar(1) = 3
         ipar(10) = 2
         return
      else
         do i = 1, n
            w(i) = rhs(i) - w(i)
         enddo
      endif
      fpar(11) = fpar(11) + n
c
 20   alpha = sqrt(distdot(n,w,1,w,1))
      fpar(11) = fpar(11) + 2*n + 1
      if (ipar(7).eq.1 .and. ipar(3).ne.999) then
         if (abs(ipar(3)).eq.2) then
            fpar(4) = fpar(1) * sqrt(distdot(n,rhs,1,rhs,1)) + fpar(2)
            fpar(11) = fpar(11) + 2*n
         else
            fpar(4) = fpar(1) * alpha + fpar(2)
         endif
         fpar(3) = alpha
      endif
      fpar(5) = alpha
      w(vrn+1) = alpha
      if (alpha.le.fpar(4) .and. ipar(3).ge.0 .and. ipar(3).ne.999) then
         ipar(1) = 0
         fpar(6) = alpha
         goto 300
      endif
      alpha = one / alpha
      do ii = 1, n
         w(ii) = alpha * w(ii)
      enddo
      fpar(11) = fpar(11) + n
c
c     request for (1) right preconditioning
c     (2) matrix vector multiplication
c     (3) left preconditioning
c
 110  k = k + 1
      if (rp) then
         ipar(1) = 5
         ipar(8) = k*n - n + 1
         if (lp) then
            ipar(9) = k*n + 1
         else
            ipar(9) = idx + 1
         endif
         ipar(10) = 3
         return
      endif
c
 30   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = (k-1)*n + 1
      endif
      if (lp) then
         ipar(9) = idx + 1
      else
         ipar(9) = 1 + k*n
      endif
      ipar(10) = 4
      return
c
 40   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = k*n + 1
         ipar(10) = 5
         return
      endif
c
c     Modified Gram-Schmidt orthogonalization procedure
c     temporary pointer 'ptr' is pointing to the current column of the
c     Hessenberg matrix. 'p2' points to the new basis vector
c
 50   ipar(7) = ipar(7) + 1
      ptr = k * (k - 1) / 2 + hes
      p2 = ipar(9)
      call mgsro(.false.,n,n,k+1,k+1,fpar(11),w,w(ptr+1),
     $     ipar(12))
      if (ipar(12).lt.0) goto 200
c
c     apply previous Givens rotations to column.
c
      p2 = ptr + 1
      do i = 1, k-1
         ptr = p2
         p2 = p2 + 1
         alpha = w(ptr)
         c = w(vc+i)
         s = w(vs+i)
         w(ptr) = c * alpha + s * w(p2)
         w(p2) = c * w(p2) - s * alpha
      enddo
c
c     end of one Arnoldi iteration, alpha will store the estimated
c     residual norm at current stage
c
      fpar(11) = fpar(11) + 6*k

      prs = vrn+k
      alpha = fpar(5) 
      if (w(p2) .ne. zero) alpha = abs(w(p2+1)*w(prs)/w(p2)) 
      fpar(5) = alpha
c
      if (k.ge.m .or. (ipar(3).ge.0 .and. alpha.le.fpar(4))
     +     .or. (ipar(6).gt.0 .and. ipar(7).ge.ipar(6)))
     +     goto 200
c
      call givens(w(p2), w(p2+1), c, s)
      w(vc+k) = c
      w(vs+k) = s
      alpha = - s * w(prs)
      w(prs) = c * w(prs)
      w(prs+1) = alpha
c
      if (w(p2).ne.zero) goto 110
c
c     update the approximate solution, first solve the upper triangular
c     system, temporary pointer ptr points to the Hessenberg matrix,
c     prs points to the right-hand-side (also the solution) of the system.
c
 200  ptr = hes + k * (k + 1) / 2
      prs = vrn + k
      if (w(ptr).eq.zero) then
c
c     if the diagonal elements of the last column is zero, reduce k by 1
c     so that a smaller trianguler system is solved
c
         k = k - 1
         if (k.gt.0) then
            goto 200
         else
            ipar(1) = -3
            ipar(12) = -4
            goto 300
         endif
      endif
      w(prs) = w(prs) / w(ptr)
      do i = k-1, 1, -1
         ptr = ptr - i - 1
         do ii = 1, i
            w(vrn+ii) = w(vrn+ii) - w(prs) * w(ptr+ii)
         enddo
         prs = prs - 1
         w(prs) = w(prs) / w(ptr)
      enddo
c
      do ii = 1, n
         w(ii) = w(ii) * w(prs)
      enddo
      do i = 1, k-1
         prs = prs + 1
         ptr = i*n
         do ii = 1, n
            w(ii) = w(ii) + w(prs) * w(ptr+ii)
         enddo
      enddo
      fpar(11) = fpar(11) + 2*(k-1)*n + n + k*(k+1)
c
      if (rp) then
         ipar(1) = 5
         ipar(8) = 1
         ipar(9) = idx + 1
         ipar(10) = 6
         return
      endif
c
 60   if (rp) then
         do i = 1, n
            sol(i) = sol(i) + w(idx+i)
         enddo
      else
         do i = 1, n
            sol(i) = sol(i) + w(i)
         enddo
      endif
      fpar(11) = fpar(11) + n
c
c     process the complete stopping criteria
c
      if (ipar(3).eq.999) then
         ipar(1) = 10
         ipar(8) = -1
         ipar(9) = idx + 1
         ipar(10) = 7
         return
      else if (ipar(3).lt.0) then
         if (ipar(7).le.m+1) then
            fpar(3) = abs(w(vrn+1))
            if (ipar(3).eq.-1) fpar(4) = fpar(1)*fpar(3)+fpar(2)
         endif
         alpha = abs(w(vrn+k))
      endif
      fpar(6) = alpha
c
c     do we need to restart ?
c
 70   if (ipar(12).ne.0) then
         ipar(1) = -3
         goto 300
      endif
      if (ipar(7).lt.ipar(6) .or. ipar(6).le.0) then
         if (ipar(3).ne.999) then
            if (fpar(6).gt.fpar(4)) goto 100
         else
            if (ipar(11).eq.0) goto 100
         endif
      endif
c
c     termination, set error code, compute convergence rate
c
      if (ipar(1).gt.0) then
         if (ipar(3).eq.999 .and. ipar(11).eq.1) then
            ipar(1) = 0
         else if (ipar(3).ne.999 .and. fpar(6).le.fpar(4)) then
            ipar(1) = 0
         else if (ipar(7).ge.ipar(6) .and. ipar(6).gt.0) then
            ipar(1) = -1
         else
            ipar(1) = -10
         endif
      endif
 300  if (fpar(3).ne.zero .and. fpar(6).ne.zero .and.
     +     ipar(7).gt.ipar(13)) then
         fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7)-ipar(13))
      else
         fpar(7) = zero
      endif
      return
      end
c-----------------------------------------------------------------------
      subroutine frobnorm(n,sym,a,ja,ia,Fnorm)
      implicit none 
      integer n 
      real*8 a(*),Fnorm
      integer ja(*),ia(n+1)
      logical sym
c--------------------------------------------------------------------------
c     this routine computes the Frobenius norm of A.
c--------------------------------------------------------------------------
c     on entry:
c-----------
c n      = integer colum dimension of matrix
c a      = real array containing the nonzero elements of the matrix
c          the elements are stored by columns in order
c          (i.e. column i comes before column i+1, but the elements
c          within each column can be disordered).
c ja     = integer array containing the row indices of elements in a.
c ia     = integer array containing of length n+1 containing the
c          pointers to the beginning of the columns in arrays a and 
c          ja. It is assumed that ia(*)= 1 and ia(n+1)  = nnz +1.
c sym    = logical variable indicating whether or not the matrix is
c          symmetric.
c
c on return
c-----------
c Fnorm  = Frobenius norm of A.
c--------------------------------------------------------------------------
      real*8 Fdiag
      integer i, k 
      Fdiag = 0.0
      Fnorm = 0.0
      do i =1,n
         do k = ia(i), ia(i+1)-1
            if (ja(k) .eq. i) then
               Fdiag = Fdiag + a(k)**2
            else
               Fnorm = Fnorm + a(k)**2
            endif
         enddo 
      enddo 
      if (sym) then
         Fnorm = 2*Fnorm +Fdiag
      else
        Fnorm = Fnorm + Fdiag
      endif
      Fnorm = sqrt(Fnorm)
      return
      end

      function gamfun(side, x, y, z)
      real*8 gamfun, x, y, z
      character*2 side
      if (side.eq.'x2') then
         gamfun = 5.0
      else if (side.eq.'y1') then
         gamfun = 2.0
      else if (side.eq.'y2') then
         gamfun = 7.0
      else
         gamfun = 0.0
      endif
      return
      end
c-----------------------------------------------------------------------
      subroutine gen57bl (nx,ny,nz,nfree,na,n,a,ja,ia,iau,stencil)
c     implicit real*8 (a-h,o-z)
      integer ja(*),ia(*),iau(*),nx,ny,nz,nfree,na,n
      real*8 a(na,1), stencil(7,1)
c--------------------------------------------------------------------
c This subroutine computes the sparse matrix in compressed
c format for the elliptic operator
c
c L u = delx( a . delx u ) + dely ( b . dely u) + delz ( c . delz u ) +
c	delx ( d . u ) + dely (e . u) + delz( f . u ) + g . u
c
c Here u is a vector of nfree componebts and each of the functions
c a, b, c, d, e, f, g   is an (nfree x nfree) matrix depending of
c the coordinate (x,y,z).
c with Dirichlet Boundary conditions, on a rectangular 1-D,
c 2-D or 3-D grid using centered difference schemes.
c
c The functions a, b, ..., g are known through the
c subroutines  afunbl, bfunbl, ..., gfunbl. (user supplied) .
c
c uses natural ordering, first x direction, then y, then z
c mesh size h is uniform and determined by grid points
c in the x-direction.
c 
c The output matrix is in Block -- Sparse Row format. 
c
c--------------------------------------------------------------------
c parameters:
c-------------
c Input:
c ------
c nx      = number of points in x direction
c ny	  = number of points in y direction
c nz	  = number of points in z direction
c nfree   = number of degrees of freedom per point
c na	  = first dimension of array a as declared in calling
c           program. Must be .ge. nfree**2
c
c Output: 
c ------ 
c n	  = dimension of matrix (output)
c
c a, ja, ia = resulting matrix in  Block Sparse Row format
c           a(1:nfree**2, j ) contains a nonzero block and ja(j) 
c           contains the (block) column number of this block.
c           the block dimension of the matrix is n (output) and 
c           therefore the total number of (scalar) rows is n x nfree.
c     
c iau     = integer*n containing the position of the diagonal element
c           in the a, ja, ia structure
c
c Work space:
c------------ 
c stencil =  work array of size (7,nfree**2) [stores local stencils]
c
c--------------------------------------------------------------------
c
c     stencil (1:7,*) has the following meaning:
c
c     center point = stencil(1)
c     west point   = stencil(2)
c     east point   = stencil(3)
c     south point  = stencil(4)
c     north point  = stencil(5)
c     front point  = stencil(6)
c     back point   = stencil(7)
c
c
c                           st(5)
c                            |
c                            |
c                            |
c                            |          .st(7)
c                            |     .
c                            | .
c         st(2) ----------- st(1) ---------- st(3)
c                       .    |
c                   .        |
c               .            |
c            st(6)           |
c                            |
c                            |
c                           st(4)
c
c-------------------------------------------------------------------
c     some constants
c
      real*8 one
      parameter (one=1.0D0)
c
c     local variables
c
      integer iedge,ix,iy,iz,k,kx,ky,kz,nfree2,node
      real*8  h
c
      h = one/dble(nx+1)
      kx = 1
      ky = nx
      kz = nx*ny
      nfree2 = nfree*nfree
      iedge = 1
      node = 1
      do 100 iz = 1,nz
         do 90 iy = 1,ny
            do 80 ix = 1,nx
               ia(node) = iedge
               call bsten(nx,ny,nz,ix,iy,iz,nfree,stencil,h)
c     west
               if (ix.gt.1) then
                  ja(iedge)=node-kx
	          do 4 k=1,nfree2
		     a(k,iedge) = stencil(2,k)
 4		  continue
                  iedge=iedge + 1
               end if
c     south
               if (iy.gt.1) then
                  ja(iedge)=node-ky
	          do 5 k=1,nfree2
		     a(k,iedge) = stencil(4,k)
 5		  continue
                  iedge=iedge + 1
               end if
c     front plane
               if (iz.gt.1) then
                  ja(iedge)=node-kz
	          do 6 k=1,nfree2
		     a(k,iedge) = stencil(6,k)
 6		  continue
                  iedge=iedge + 1
               endif
c     center node
               ja(iedge) = node
               iau(node) = iedge
               do 7 k=1,nfree2
                  a(k,iedge) = stencil(1,k)
 7             continue
               iedge = iedge + 1
c     -- upper part
c     east
               if (ix.lt.nx) then
                  ja(iedge)=node+kx
	          do 8 k=1,nfree2
		     a(k,iedge) = stencil(3,k)
 8		  continue
                  iedge=iedge + 1
               end if
c     north
               if (iy.lt.ny) then
                  ja(iedge)=node+ky
	          do 9 k=1,nfree2
		     a(k,iedge) = stencil(5,k)
 9		  continue
                  iedge=iedge + 1
               end if
c     back plane
               if (iz.lt.nz) then
                  ja(iedge)=node+kz
	          do 10 k=1,nfree2
                     a(k,iedge) = stencil(7,k)
 10		  continue
                  iedge=iedge + 1
               end if
c------next node -------------------------
               node=node+1
 80         continue
 90      continue
 100  continue
c     
c     -- new version of BSR -- renumbering removed. 
c     change numbering of nodes so that each ja(k) will contain the
c     actual column number in the original matrix of entry (1,1) of each
c     block (k).
c      do 101 k=1,iedge-1
c         ja(k) = (ja(k)-1)*nfree+1
c 101  continue
c
c      n = (node-1)*nfree
      n = node-1 
      ia(node)=iedge
      return
c--------------end-of-gen57bl-------------------------------------------
c-----------------------------------------------------------------------
      end
c--------------------------------------------------------------------
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c    MATRIX GENERATION ROUTINES  -- FINITE DIFFERENCE MATRICES         c
c----------------------------------------------------------------------c
c contents:                                                            c
c----------                                                            c
c gen57pt  : generates 5-point and 7-point matrices.                   c
c gen57bl  : generates block 5-point and 7-point matrices.             c
c                                                                      c
c supporting routines:                                                 c
c---------                                                             c
c gensten  : generate the stencil (point version)                      c
c bsten    : generate the stencil (block version)                      c
c fdaddbc  : finite difference add boundary conditions                 c
c fdreduce : reduce the system to eliminate node with known values     c
c clrow    : clear a row of a CSR matrix                               c
c lctcsr   : locate the position of A(i,j) in CSR format               c
c----------------------------------------------------------------------c
      subroutine gen57pt(nx,ny,nz,al,mode,n,a,ja,ia,iau,rhs)
      integer ja(*),ia(*),iau(*), nx, ny, nz, mode, n
      real*8 a(*), rhs(*), al(6)
c-----------------------------------------------------------------------
c On entry:
c
c nx      = number of grid points in x direction
c ny	  = number of grid points in y direction
c nz	  = number of grid points in z direction
c al      = array of size 6, carries the coefficient alpha of the
c           boundary conditions
c mode    = what to generate:
c           < 0 : generate the graph only,
c           = 0 : generate the matrix,
c           > 0 : generate the matrix and the right-hand side.
c
c On exit:
c
c n       = number of nodes with unknown values, ie number of rows
c           in the matrix
c
c a,ja,ia = resulting matrix in row-sparse format
c
c iau     = integer*n, containing the poisition of the diagonal element
c           in the a, ja, ia structure
c
c rhs     = the right-hand side
c
c External functions needed (must be supplied by caller)
c     afun, bfun, cfun, dfun, efun, ffun, gfun, hfun
c     betfun, gamfun
c They have the following prototype:
c     real*8 function xfun(x, y, z)
c     real*8 x, y, z
c-----------------------------------------------------------------------
c This subroutine computes the sparse matrix in compressed sparse row
c format for the elliptic equation:
c       d    du    d    du    d    du      du     du     du
c L u = --(A --) + --(B --) + --(C --) + D -- + E -- + F -- + G u = H u
c       dx   dx    dy   dy    dz   dz      dx     dy     dz
c
c with general Mixed Boundary conditions, on a rectangular 1-D,
c 2-D or 3-D grid using 2nd order centered difference schemes.
c
c The functions a, b, ..., g, h are known through the
c as afun, bfun, ..., gfun, hfun in this subroutine.
c NOTE: To obtain the correct matrix, any function that is not
c needed should be set to zero.  For example for two-dimensional
c problems, nz should be set to 1 and the functions cfun and ffun
c should be zero functions.
c
c The Boundary condition is specified in the following form:
c           du
c     alpha -- + beta u = gamma
c           dn
c Where alpha is constant at each side of the boundary surfaces.  Alpha
c is represented by parameter al.  It is expected to an array that
c contains enough elements to specify the boundaries for the problem,
c 1-D case needs two elements, 2-D needs 4 and 3-D needs 6.  The order
c of the boundaries in the array is left(west), right(east),
c bottom(south), top(north), front, rear.  Beta and gamma are functions
c of type real with three arguments x, y, z.  These two functions are
c known subroutine 'addbc' as betfun and gamfun.  They should following
c the same notion as afun ... hfun.  For more restriction on afun ...
c hfun, please read the documentation follows the subroutine 'getsten',
c and, for more on betfun and gamfun, please refer to the documentation
c under subroutine 'fdaddbc'.
c
c The nodes are ordered using natural ordering, first x direction, then
c y, then z.  The mesh size h is uniform and determined by grid points
c in the x-direction.
c
c The domain specified for the problem is [0 .ge. x .ge. 1],
c [0 .ge. y .ge. (ny-1)*h] and [0 .ge. z .ge. (nz-1)*h], where h is
c 1 / (nx-1).  Thus if non-Dirichlet boundary condition is specified,
c the mesh will have nx points along the x direction, ny along y and
c nz along z.  For 1-D case, both y and z value are assumed to zero
c when calling relavent functions that have three parameters.
c Similarly, for 2-D case, z is assumed to be zero.
c
c About the expectation of nx, ny and nz:
c nx is required to be .gt. 1 always;
c if the second dimension is present in the problem, then ny should be
c .gt. 1, else 1;
c if the third dimension is present in the problem, nz .gt. 1, else 1.
c when ny is 1, nz must be 1.
c-----------------------------------------------------------------------
c
c     stencil [1:7] has the following meaning:
c
c     center point = stencil(1)
c     west point = stencil(2)
c     east point = stencil(3)
c     south point = stencil(4)
c     north point = stencil(5)
c     front point = stencil(6)
c     back point = stencil(7)
c
c     al[1:6] carry the coefficient alpha in the similar order
c
c     west  side = al(1)
c     east  side = al(2)
c     south side = al(3)
c     north side = al(4)
c     front side = al(5)
c     back  side = al(6)
c
c                           al(4)
c                           st(5)
c                            |
c                            |
c                            |           al(6)
c                            |          .st(7)
c                            |     .
c         al(1)              | .             al(2)
c         st(2) ----------- st(1) ---------- st(3)
c                       .    |
c                   .        |
c               .            |
c            st(6)           |
c            al(5)           |
c                            |
c                           st(4)
c                           al(3)
c
c-------------------------------------------------------------------
c     some constants
c
      real*8 one
      parameter (one=1.0D0)
c
c     local variables
c
      integer ix, iy, iz, kx, ky, kz, node, iedge
      real*8  r, h, stencil(7)
      logical value, genrhs
c
c     nx has to be larger than 1
c
      if (nx.le.1) return
      h = one / dble(nx-1)
c
c     the mode
c
      value = (mode.ge.0)
      genrhs = (mode.gt.0)
c
c     first generate the whole matrix as if the boundary condition does
c     not exist
c
      kx = 1
      ky = nx
      kz = nx*ny
      iedge = 1
      node = 1
      do 100 iz = 1,nz
         do 90 iy = 1,ny
            do 80 ix = 1,nx
               ia(node) = iedge
c
c     compute the stencil at the current node
c
               if (value) call
     &              getsten(nx,ny,nz,mode,ix-1,iy-1,iz-1,stencil,h,r)
c     west
               if (ix.gt.1) then
                  ja(iedge)=node-kx
		  if (value) a(iedge) = stencil(2)
                  iedge=iedge + 1
               end if
c     south
               if (iy.gt.1) then
                  ja(iedge)=node-ky
		  if (value) a(iedge) = stencil(4)
                  iedge=iedge + 1
               end if
c     front plane
               if (iz.gt.1) then
                  ja(iedge)=node-kz
		  if (value) a(iedge) = stencil(6)
                  iedge=iedge + 1
               endif
c     center node
               ja(iedge) = node
               iau(node) = iedge
               if (value) a(iedge) = stencil(1)
               iedge = iedge + 1
c     east
               if (ix.lt.nx) then
                  ja(iedge)=node+kx
		  if (value) a(iedge) = stencil(3)
                  iedge=iedge + 1
               end if
c     north
               if (iy.lt.ny) then
                  ja(iedge)=node+ky
		  if (value) a(iedge) = stencil(5)
                  iedge=iedge + 1
               end if
c     back plane
               if (iz.lt.nz) then
                  ja(iedge)=node+kz
                  if (value) a(iedge) = stencil(7)
                  iedge=iedge + 1
               end if
c     the right-hand side
               if (genrhs) rhs(node) = r
               node=node+1
 80         continue
 90      continue
 100  continue
      ia(node)=iedge
c
c     Add in the boundary conditions
c
      call fdaddbc(nx,ny,nz,a,ja,ia,iau,rhs,al,h)
c
c     eliminate the boudary nodes from the matrix
c
      call fdreduce(nx,ny,nz,al,n,a,ja,ia,iau,rhs,stencil)
c
c     done
c
      return
c-----end-of-gen57pt----------------------------------------------------
c-----------------------------------------------------------------------
      end
c----------end-of-ddot--------------------------------------------------
c----------------------------------------------------------------------- 


c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c      MATRIX GENERATION ROUTINES - FINITE ELEMENT MATRICES            c
c----------------------------------------------------------------------c
c contents:                                                            c
c----------                                                            c
c genfea       : generates finite element matrices in assembled form   c
c genfea_wbc   : generates finite element matrices in assembled form   c
c                without applying the boundary conditions              c
c genfeu       : generates finite element matrices in unassembled form c
c genfeu_wbc   : generates finite element matrices in unassembled form c
c                without applying the boundary conditions              c
c genfeu_lstif : generates finite element matrices in unassembled form c
c                using the lstif problem appearing in elmtlib2.f       c
c assmb1       : assembles an unassembled matrix (produced by genfeu)  c
c----------------------------------------------------------------------c
      subroutine genfea (nx,nelx,node,job,x,y,ijk,nodcode,fs,nint,
     *     a,ja,ia,f,iwk,jwk,ierr,xyk)
c-----------------------------------------------------------------------
c this subroutine generates a finite element matrix in assembled form.
c the matrix is assembled in compressed sparse row format. See genfeu
c for matrices in unassembled form. The user must provide the grid, 
c (coordinates x, y and connectivity matrix ijk) as well as some 
c information on the nodes (nodcode) and the material properties 
c (the function K(x,y) above) in the form of a subroutine xyk. 
c----------------------------------------------------------------------
c
c on entry:
c ---------
c 
c nx	    = integer . the number of nodes in the grid . 
c nelx	    = integer . the number of elements in the grid.
c node      = integer = the number of nodes per element (should be
c             set to three in this version). also the first dimension
c             of ijk
c job	    = integer. If job=0, it is assumed that there is no heat
c             source (i.e. fs = 0) and the right hand side
c             produced will therefore be a zero vector.
c             If job = 1 on entry then the contributions from the
c             heat source in each element are taken into account.
c 
c x, y      = two real arrays containing the coordinates of the nodes.
c 
c ijk       =  an integer array containing the connectivity matrix.
c              ijk(i,nel), i=1,2,..node, is the list of the nodes
c              constituting the element nel, ans listed in 
c              counter clockwise order.
c
c nodcode   = an integer array containing the boundary information for
c             each node with the following meaning.
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner node. [This node and the
c             corresponmding element are discarded.]
c
c fs	    = real array of length nelx on entry containing the heat 
c             source for each element (job = 1 only) 
c
c xyk	    = subroutine defining the material properties at each 
c	      element. Form: 
c 	      call xyk(nel,xyke,x,y,ijk,node) with on return
c             xyke =  material constant matrices. 
c	      for each element nel, xyke(1,nel),xyke(2,nel) 
c             and xyke(3,nel) represent the constants
c             K11, K22, and K12 at that element.
c                           
c on return
c --------- 
c nint	    = integer. The number of active (nonboundary) nodes. Also 
c             equal to the dimension of the assembled matrix.
c
c a, ja, ia = assembled matrix in compressed sparse row format.
c
c f	    = real array containing the right hand for the linears 
c             system to solve.
c 
c ierr	    = integer. Error message. If (ierr .ne. 0) on return
c             it means that one of the elements has a negative or zero
c             area probably because of a bad ordering of the nodes 
c             (see ijk above). Use the subroutine chkelmt to reorder
c             the nodes properly if necessary.
c iwk, jwk  = two integer work arrays of length nx each.
c
c-----------------------------------------------------------------------
      real*8 a(*),x(*),y(*),f(*),fs(*)
      integer ijk(node,*), nodcode(*),ia(*),ja(*),iwk(*),jwk(*)
      external xyk, funb, func, fung 
c     
      ierr = 0 
c     
c     take into boundary conditions to remove boundary nodes.
c     
      call bound (nx,nelx,ijk,nodcode,node,nint,jwk,
     *     x,y,f,iwk)
c     
c     assemble the matrix
c     
       call assmbo (nx,nelx,node,ijk,nodcode,x,y,
     *     a,ja,ia,f,iwk,jwk,ierr,xyk, funb, func, fung)
c     
c     if applicable (job .eq. 1) get heat source function
c     
      indic = 1
      if (job .eq. 1) 
     *     call hsourc (indic,nx,nelx,node,x,y,ijk,fs,f) 
c     
c     call diric for Dirichlet conditions
c     
      call diric(nx,nint,a,ja,ia,f)
c     done
      return
c------end of genfea ---------------------------------------------------
c-----------------------------------------------------------------------
      end
      subroutine genfea_wbc (nx,nelx,node,job,x,y,ijk,nodcode,fs,
     *     a,ja,ia,f,iwk,jwk,ierr,xyk)
c-----------------------------------------------------------------------
c this subroutine generates a finite element matrix in assembled form.
c the matrix is assembled in compressed sparse row format. See genfeu
c for matrices in unassembled form. The user must provide the grid, 
c (coordinates x, y and connectivity matrix ijk) as well as some 
c information on the nodes (nodcode) and the material properties 
c (the function K(x,y) above) in the form of a subroutine xyk. 
c----------------------------------------------------------------------
c Irene Moulitsas, moulitsa@cs.umn.edu  : It does not apply boundary
c                 conditions; variable nint is eliminated
c----------------------------------------------------------------------
c
c on entry:
c ---------
c 
c nx	    = integer . the number of nodes in the grid . 
c nelx	    = integer . the number of elements in the grid.
c node      = integer = the number of nodes per element (should be
c             set to three in this version). also the first dimension
c             of ijk
c job	    = integer. If job=0, it is assumed that there is no heat
c             source (i.e. fs = 0) and the right hand side
c             produced will therefore be a zero vector.
c             If job = 1 on entry then the contributions from the
c             heat source in each element are taken into account.
c 
c x, y      = two real arrays containing the coordinates of the nodes.
c 
c ijk       =  an integer array containing the connectivity matrix.
c              ijk(i,nel), i=1,2,..node, is the list of the nodes
c              constituting the element nel, ans listed in 
c              counter clockwise order.
c
c nodcode   = an integer array containing the boundary information for
c             each node with the following meaning.
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner node. [This node and the
c             corresponmding element are discarded.]
c
c fs	    = real array of length nelx on entry containing the heat 
c             source for each element (job = 1 only) 
c
c xyk	    = subroutine defining the material properties at each 
c	      element. Form: 
c 	      call xyk(nel,xyke,x,y,ijk,node) with on return
c             xyke =  material constant matrices. 
c	      for each element nel, xyke(1,nel),xyke(2,nel) 
c             and xyke(3,nel) represent the constants
c             K11, K22, and K12 at that element.
c                           
c on return
c --------- 
c a, ja, ia = assembled matrix in compressed sparse row format.
c
c f	    = real array containing the right hand for the linears 
c             system to solve.
c 
c ierr	    = integer. Error message. If (ierr .ne. 0) on return
c             it means that one of the elements has a negative or zero
c             area probably because of a bad ordering of the nodes 
c             (see ijk above). Use the subroutine chkelmt to reorder
c             the nodes properly if necessary.
c iwk, jwk  = two integer work arrays of length nx each.
c
c-----------------------------------------------------------------------
      real*8 a(*),x(*),y(*),f(*),fs(*)
      integer ijk(node,*), nodcode(*),ia(*),ja(*),iwk(*),jwk(*)
      external xyk, funb, func, fung 
c     
      ierr = 0 
c     
c     assemble the matrix
c     
       call assmbo (nx,nelx,node,ijk,nodcode,x,y,
     *     a,ja,ia,f,iwk,jwk,ierr,xyk, funb, func, fung)
c     
c     if applicable (job .eq. 1) get heat source function
c     
      indic = 1
      if (job .eq. 1) 
     *     call hsourc (indic,nx,nelx,node,x,y,ijk,fs,f) 
c
c     done
      return
c------end of genfea_wbc -----------------------------------------------
c-----------------------------------------------------------------------
       end
c----------------------------------------------------------------------- 
      subroutine genfeu (nx,nelx,node,job,x,y,ijk,nodcode,fs,
     *     nint,a,na,f,iwk,jwk,ierr,xyk)
c-----------------------------------------------------------------------
c this subroutine generates finite element matrices for heat 
c condution problem 
c
c                  - Div ( K(x,y) Grad u ) = f
c                    u = 0 on boundary 
c 
c (with Dirichlet boundary conditions). The matrix is returned 
c in unassembled form. The user must provide the grid, 
c (coordinates x, y and connectivity matrix ijk) as well as some 
c information on the nodes (nodcode) and the material properties 
c (the function K(x,y) above) in the form of a subroutine xyk. 
c
c----------------------------------------------------------------------
c
c on entry:
c ---------
c 
c nx	    = integer . the number of nodes in the grid . 
c nelx	    = integer . the number of elements in the grid.
c node      = integer = the number of nodes per element (should be
c             set to three in this version). also the first dimension
c             of ijk
c job	    = integer. If job=0, it is assumed that there is no heat
c             source (i.e. fs = 0) and the right hand side
c             produced will therefore be a zero vector.
c             If job = 1 on entry then the contributions from the
c             heat source in each element are taken into account.
c 
c na	    = integer. The first dimension of the array a. 
c             a is declared as an array of dimension a(na,node,node).
c
c x, y      = two real arrays containing the coordinates of the nodes.
c 
c ijk       =  an integer array containing the connectivity matrix.
c              ijk(i,nel), i=1,2,..node, is the list of the nodes
c              constituting the element nel, ans listed in 
c              counter clockwise order.
c
c xyk	    = subroutine defining the material properties at each 
c	      element. Form: 
c 	      call xyk(nel,xyke,x,y,ijk,node) with on return
c             xyke =  material constant matrices. 
c	      for each element nel, xyke(1,nel),xyke(2,nel) 
c             and xyke(3,nel) represent the constants
c             K11, K22, and K12 at that element.
c                           
c nodcode   = an integer array containing the boundary information for
c             each node with the following meaning.
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner node. [This node and the
c             corresponmding element are discarded.]
c
c fs	    = real array of length nelx on entry containing the heat 
c             source for each element (job = 1 only) 
c                           
c on return
c --------- 
c nint	    = integer. The number of active (nonboundary) nodes. Also 
c             equal to the dimension of the assembled matrix.
c
c a         = matrix in unassembled form. a(nel,*,*) contains the 
c             element matrix for element nel.
c
c f	    = real array containing the right hand for the linears 
c             system to solve, in assembled form. 
c 
c ierr	    = integer. Error message. If (ierr .ne. 0) on return
c             it means that one of the elements has a negative or zero
c             area probably because of a bad ordering of the nodes 
c             (see ijk above). Use the subroutine chkelmt to reorder
c             the nodes properly if necessary.
c iwk, jwk  = two integer work arrays of length nx each.
c
c-----------------------------------------------------------------------
      real*8 a(na,node,node),x(*),y(*),f(*), fs(*)
      integer ijk(node,*), nodcode(*),iwk(*),jwk(*)
      external xyk
c     
      ierr = 0 
c     
c     take boundary conditions into account to move boundary nodes to
c     the end..
c     
      call bound (nx,nelx,ijk,nodcode,node,nint,jwk,
     *     x,y,f,iwk)
c     
c     assemble the matrix
c     
      call unassbl (a,na,f,nx,nelx,ijk,nodcode,
     *     node,x,y,ierr,xyk) 
c     
c     if applicable (job .eq. 1) get heat source function
c     
      indic = 0
      if (job .eq. 1) 
     *     call hsourc (indic,nx,nelx,node,x,y,ijk,fs,f) 
c     
c     done
c     
      return
      end
c----- end of genfeu_wbc ----------------------------------------------- 
      subroutine genfeu_lstif (nx,nelx,node,job,x,y,ijk,nodcode,fs,
     *          a,na,f,iwk,jwk,ierr,xyk)
c-----------------------------------------------------------------------
c this subroutine generates finite element matrices using unassmbl_lstif.
c The matrix is returned in unassembled form.
c The user must provide the grid, coordinates x, y and connectivity matrix
c ijk) as well as some information on the nodes (nodcode) and the material
c properties (the function K(x,y) above) in the form of a subroutine xyk. 
c
c----------------------------------------------------------------------
c moulitsa@cs.umn.edu   : It does not apply boundary conditions
c                         variable nint is eliminated
c----------------------------------------------------------------------
c
c on entry:
c ---------
c 
c nx	    = integer . the number of nodes in the grid . 
c nelx	    = integer . the number of elements in the grid.
c node      = integer = the number of nodes per element (should be
c             set to three in this version). also the first dimension
c             of ijk
c job	    = integer. If job=0, it is assumed that there is no heat
c             source (i.e. fs = 0) and the right hand side
c             produced will therefore be a zero vector.
c             If job = 1 on entry then the contributions from the
c             heat source in each element are taken into account.
c 
c na	    = integer. The first dimension of the array a. 
c             a is declared as an array of dimension a(na,node,node).
c
c x, y      = two real arrays containing the coordinates of the nodes.
c 
c ijk       =  an integer array containing the connectivity matrix.
c              ijk(i,nel), i=1,2,..node, is the list of the nodes
c              constituting the element nel, ans listed in 
c              counter clockwise order.
c
c xyk	    = subroutine defining the material properties at each 
c	      element. Form: 
c 	      call xyk(nel,xyke,x,y,ijk,node) with on return
c             xyke =  material constant matrices. 
c	      for each element nel, xyke(1,nel),xyke(2,nel) 
c             and xyke(3,nel) represent the constants
c             K11, K22, and K12 at that element.
c                           
c nodcode   = an integer array containing the boundary information for
c             each node with the following meaning.
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner node. [This node and the
c             corresponmding element are discarded.]
c
c fs	    = real array of length nelx on entry containing the heat 
c             source for each element (job = 1 only) 
c                           
c on return
c --------- 
c a         = matrix in unassembled form. a(nel,*,*) contains the 
c             element matrix for element nel.
c
c f	    = real array containing the right hand for the linears 
c             system to solve, in assembled form. 
c 
c ierr	    = integer. Error message. If (ierr .ne. 0) on return
c             it means that one of the elements has a negative or zero
c             area probably because of a bad ordering of the nodes 
c             (see ijk above). Use the subroutine chkelmt to reorder
c             the nodes properly if necessary.
c iwk, jwk  = two integer work arrays of length nx each.
c
c-----------------------------------------------------------------------
      real*8 a(na,node,node),x(*),y(*),f(*), fs(*)
      integer ijk(node,*), nodcode(*),iwk(*),jwk(*)
      external xyk, funb, func, fung
c     
      ierr = 0 
c     
c     assemble the matrix
c     
      call unassbl_lstif (a,na,f,nx,nelx,ijk,nodcode,
     *     node,x,y,ierr,xyk,funb,func,fung) 
c     
c     if applicable (job .eq. 1) get heat source function
c     
      indic = 0
      if (job .eq. 1) 
     *     call hsourc (indic,nx,nelx,node,x,y,ijk,fs,f) 
c     
c     done
c     
      return
      end
c----- end of genfeu ---------------------------------------------------- 
      subroutine genfeu_wbc (nx,nelx,node,job,x,y,ijk,nodcode,fs,
     *          a,na,f,iwk,jwk,ierr,xyk)
c-----------------------------------------------------------------------
c this subroutine generates finite element matrices for heat 
c condution problem 
c
c                  - Div ( K(x,y) Grad u ) = f
c                    u = 0 on boundary 
c 
c (with Dirichlet boundary conditions). The matrix is returned 
c in unassembled form. The user must provide the grid, 
c (coordinates x, y and connectivity matrix ijk) as well as some 
c information on the nodes (nodcode) and the material properties 
c (the function K(x,y) above) in the form of a subroutine xyk. 
c
c----------------------------------------------------------------------
c moulitsa@cs   : It does not apply boundary conditions
c                 variable nint is eliminated
c----------------------------------------------------------------------
c
c on entry:
c ---------
c 
c nx	    = integer . the number of nodes in the grid . 
c nelx	    = integer . the number of elements in the grid.
c node      = integer = the number of nodes per element (should be
c             set to three in this version). also the first dimension
c             of ijk
c job	    = integer. If job=0, it is assumed that there is no heat
c             source (i.e. fs = 0) and the right hand side
c             produced will therefore be a zero vector.
c             If job = 1 on entry then the contributions from the
c             heat source in each element are taken into account.
c 
c na	    = integer. The first dimension of the array a. 
c             a is declared as an array of dimension a(na,node,node).
c
c x, y      = two real arrays containing the coordinates of the nodes.
c 
c ijk       =  an integer array containing the connectivity matrix.
c              ijk(i,nel), i=1,2,..node, is the list of the nodes
c              constituting the element nel, ans listed in 
c              counter clockwise order.
c
c xyk	    = subroutine defining the material properties at each 
c	      element. Form: 
c 	      call xyk(nel,xyke,x,y,ijk,node) with on return
c             xyke =  material constant matrices. 
c	      for each element nel, xyke(1,nel),xyke(2,nel) 
c             and xyke(3,nel) represent the constants
c             K11, K22, and K12 at that element.
c                           
c nodcode   = an integer array containing the boundary information for
c             each node with the following meaning.
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner node. [This node and the
c             corresponmding element are discarded.]
c
c fs	    = real array of length nelx on entry containing the heat 
c             source for each element (job = 1 only) 
c                           
c on return
c --------- 
c a         = matrix in unassembled form. a(nel,*,*) contains the 
c             element matrix for element nel.
c
c f	    = real array containing the right hand for the linears 
c             system to solve, in assembled form. 
c 
c ierr	    = integer. Error message. If (ierr .ne. 0) on return
c             it means that one of the elements has a negative or zero
c             area probably because of a bad ordering of the nodes 
c             (see ijk above). Use the subroutine chkelmt to reorder
c             the nodes properly if necessary.
c iwk, jwk  = two integer work arrays of length nx each.
c
c-----------------------------------------------------------------------
      real*8 a(na,node,node),x(*),y(*),f(*), fs(*)
      integer ijk(node,*), nodcode(*),iwk(*),jwk(*)
      external xyk
c     
      ierr = 0 
c     
c     assemble the matrix
c     
      call unassbl (a,na,f,nx,nelx,ijk,nodcode,
     *     node,x,y,ierr,xyk) 
c     
c     if applicable (job .eq. 1) get heat source function
c     
      indic = 0
      if (job .eq. 1) 
     *     call hsourc (indic,nx,nelx,node,x,y,ijk,fs,f) 
c     
c     done
c     
      return
      end
c----------------------------------------------------------------------- 
      subroutine get1up (n,ja,ia,ju)
      integer  n, ja(*),ia(*),ju(*)
c----------------------------------------------------------------------
c obtains the first element of each row of the upper triangular part
c of a matrix. Assumes that the matrix is already sorted.
c-----------------------------------------------------------------------
c parameters
c input
c ----- 
c ja      = integer array containing the column indices of aij
c ia      = pointer array. ia(j) contains the position of the 
c           beginning of row j in ja
c 
c output 
c ------ 
c ju      = integer array of length n. ju(i) is the address in ja 
c           of the first element of the uper triangular part of
c           of A (including rthe diagonal. Thus if row i does have
c           a nonzero diagonal element then ju(i) will point to it.
c           This is a more general version of diapos.
c-----------------------------------------------------------------------
c local vAriables
      integer i, k 
c     
      do 5 i=1, n
         ju(i) = 0
         k = ia(i) 
c
 1       continue
         if (ja(k) .ge. i) then
            ju(i) = k
            goto 5
         elseif (k .lt. ia(i+1) -1) then
            k=k+1
c 
c go try next element in row 
c 
            goto 1
         endif 
 5    continue
      return
c-----end-of-get1up-----------------------------------------------------
      end 
c----------------------------------------------------------------------- 
      subroutine getbwd(n,a,ja,ia,ml,mu)
c-----------------------------------------------------------------------
c gets the bandwidth of lower part and upper part of A.
c does not assume that A is sorted.
c-----------------------------------------------------------------------
c on entry:
c----------
c n	= integer = the row dimension of the matrix
c a, ja,
c    ia = matrix in compressed sparse row format.
c 
c on return:
c----------- 
c ml	= integer. The bandwidth of the strict lower part of A
c mu	= integer. The bandwidth of the strict upper part of A 
c
c Notes:
c ===== ml and mu are allowed to be negative or return. This may be 
c       useful since it will tell us whether a band is confined 
c       in the strict  upper/lower triangular part. 
c       indeed the definitions of ml and mu are
c
c       ml = max ( (i-j)  s.t. a(i,j) .ne. 0  )
c       mu = max ( (j-i)  s.t. a(i,j) .ne. 0  )
c----------------------------------------------------------------------c
c Y. Saad, Sep. 21 1989                                                c
c----------------------------------------------------------------------c
      real*8 a(*) 
      integer ja(*),ia(n+1),ml,mu,ldist,i,k 
      ml = - n
      mu = - n
      do 3 i=1,n
         do 31 k=ia(i),ia(i+1)-1 
            ldist = i-ja(k)
            ml = max(ml,ldist)
            mu = max(mu,-ldist)
 31      continue
 3    continue
      return
c---------------end-of-getbwd ------------------------------------------ 
c----------------------------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine getdia (nrow,ncol,job,a,ja,ia,len,diag,idiag,ioff)
      real*8 diag(*),a(*)
      integer nrow, ncol, job, len, ioff, ia(*), ja(*), idiag(*)
c-----------------------------------------------------------------------
c this subroutine extracts a given diagonal from a matrix stored in csr 
c format. the output matrix may be transformed with the diagonal removed
c from it if desired (as indicated by job.) 
c----------------------------------------------------------------------- 
c our definition of a diagonal of matrix is a vector of length nrow
c (always) which contains the elements in rows 1 to nrow of
c the matrix that are contained in the diagonal offset by ioff
c with respect to the main diagonal. if the diagonal element
c falls outside the matrix then it is defined as a zero entry.
c thus the proper definition of diag(*) with offset ioff is 
c
c     diag(i) = a(i,ioff+i) i=1,2,...,nrow
c     with elements falling outside the matrix being defined as zero.
c 
c----------------------------------------------------------------------- 
c 
c on entry:
c---------- 
c
c nrow	= integer. the row dimension of the matrix a.
c ncol	= integer. the column dimension of the matrix a.
c job   = integer. job indicator.  if job = 0 then
c         the matrix a, ja, ia, is not altered on return.
c         if job.ne.0  then getdia will remove the entries
c         collected in diag from the original matrix.
c         this is done in place.
c
c a,ja,
c    ia = matrix stored in compressed sparse row a,ja,ia,format
c ioff  = integer,containing the offset of the wanted diagonal
c	  the diagonal extracted is the one corresponding to the
c	  entries a(i,j) with j-i = ioff.
c	  thus ioff = 0 means the main diagonal
c
c on return:
c----------- 
c len   = number of nonzero elements found in diag.
c         (len .le. min(nrow,ncol-ioff)-max(1,1-ioff) + 1 )
c
c diag  = real*8 array of length nrow containing the wanted diagonal.
c	  diag contains the diagonal (a(i,j),j-i = ioff ) as defined 
c         above. 
c
c idiag = integer array of  length len, containing the poisitions 
c         in the original arrays a and ja of the diagonal elements
c         collected in diag. a zero entry in idiag(i) means that 
c         there was no entry found in row i belonging to the diagonal.
c         
c a, ja,
c    ia = if job .ne. 0 the matrix is unchanged. otherwise the nonzero
c         diagonal entries collected in diag are removed from the 
c         matrix and therefore the arrays a, ja, ia will change.
c	  (the matrix a, ja, ia will contain len fewer elements) 
c 
c----------------------------------------------------------------------c
c     Y. Saad, sep. 21 1989 - modified and retested Feb 17, 1996.      c 
c----------------------------------------------------------------------c
c     local variables
      integer istart, max, iend, i, kold, k, kdiag, ko
c     
      istart = max(0,-ioff)
      iend = min(nrow,ncol-ioff)
      len = 0
      do 1 i=1,nrow
         idiag(i) = 0
	 diag(i) = 0.0d0 
 1    continue
c     
c     extract  diagonal elements
c     
      do 6 i=istart+1, iend
         do 51 k= ia(i),ia(i+1) -1
            if (ja(k)-i .eq. ioff) then
               diag(i)= a(k)
               idiag(i) = k
               len = len+1
               goto 6
            endif
 51      continue
 6    continue
      if (job .eq. 0 .or. len .eq.0) return
c
c     remove diagonal elements and rewind structure
c 
      ko = 0
      do  7 i=1, nrow 
         kold = ko
         kdiag = idiag(i) 
         do 71 k= ia(i), ia(i+1)-1 
            if (k .ne. kdiag) then
               ko = ko+1
               a(ko) = a(k)
               ja(ko) = ja(k)
            endif
 71      continue
         ia(i) = kold+1
 7    continue
c
c     redefine ia(nrow+1)
c
      ia(nrow+1) = ko+1
      return
c------------end-of-getdia----------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine get_domns2(ndom,nodes,link,levst,riord,iptr)
      implicit none 
      integer ndom,nodes(*),link(*),levst(*),riord(*),iptr(*) 
c-----------------------------------------------------------------------
c     constructs the subdomains from its linked list data structure
c-----------------------------------------------------------------------
c     input:
c     ndom  = number of subdomains
c     nodes = sequence of nodes are produced by mapper4. 
c     link  = link list array as produced by mapper4.
c     on return:
c----------
c     riord = contains the nodes in each subdomain in succession.
c     iptr  = pointer in riord for beginnning of each subdomain.
c     Thus subdomain number i consists of nodes 
c     riord(k1),riord(k1)+1,...,riord(k2) 
c     where k1 = iptr(i), k2= iptr(i+1)-1
c     
c-----------------------------------------------------------------------
c     local variables 
      integer nod, j, next, ii 
      nod = 1
      iptr(1) = nod 
      do 21 j=1, ndom 
         next = levst(j)
 22      ii = nodes(next)
         riord(nod) = ii 
         nod = nod+1 
         next = link(next) 
         if (next .gt.  0) goto 22
         iptr(j+1) = nod 
 21   continue
c
      return
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      double precision function getelm (i,j,a,ja,ia,iadd,sorted) 
c-----------------------------------------------------------------------
c     purpose:
c     -------- 
c     this function returns the element a(i,j) of a matrix a, 
c     for any pair (i,j).  the matrix is assumed to be stored 
c     in compressed sparse row (csr) format. getelm performs a
c     binary search in the case where it is known that the elements 
c     are sorted so that the column indices are in increasing order. 
c     also returns (in iadd) the address of the element a(i,j) in 
c     arrays a and ja when the search is successsful (zero if not).
c----- 
c     first contributed by noel nachtigal (mit). 
c     recoded jan. 20, 1991, by y. saad [in particular
c     added handling of the non-sorted case + the iadd output] 
c-----------------------------------------------------------------------
c     parameters:
c     ----------- 
c on entry: 
c---------- 
c     i      = the row index of the element sought (input).
c     j      = the column index of the element sought (input).
c     a      = the matrix a in compressed sparse row format (input).
c     ja     = the array of column indices (input).
c     ia     = the array of pointers to the rows' data (input).
c     sorted = logical indicating whether the matrix is knonw to 
c              have its column indices sorted in increasing order 
c              (sorted=.true.) or not (sorted=.false.).
c              (input). 
c on return:
c----------- 
c     getelm = value of a(i,j). 
c     iadd   = address of element a(i,j) in arrays a, ja if found,
c              zero if not found. (output) 
c
c     note: the inputs i and j are not checked for validity. 
c-----------------------------------------------------------------------
c     noel m. nachtigal october 28, 1990 -- youcef saad jan 20, 1991.
c----------------------------------------------------------------------- 
      integer i, ia(*), iadd, j, ja(*)
      double precision a(*)
      logical sorted 
c
c     local variables.
c
      integer ibeg, iend, imid, k
c
c     initialization 
c
      iadd = 0 
      getelm = 0.0
      ibeg = ia(i)
      iend = ia(i+1)-1
c
c     case where matrix is not necessarily sorted
c     
      if (.not. sorted) then 
c
c scan the row - exit as soon as a(i,j) is found
c
         do 5  k=ibeg, iend
            if (ja(k) .eq.  j) then
               iadd = k 
               goto 20 
            endif
 5       continue
c     
c     end unsorted case. begin sorted case
c     
      else
c     
c     begin binary search.   compute the middle index.
c     
 10      imid = ( ibeg + iend ) / 2
c     
c     test if  found
c     
         if (ja(imid).eq.j) then
            iadd = imid 
            goto 20
         endif
         if (ibeg .ge. iend) goto 20
c     
c     else     update the interval bounds. 
c     
         if (ja(imid).gt.j) then
            iend = imid -1
         else 
            ibeg = imid +1
         endif
         goto 10  
c     
c     end both cases
c     
      endif
c     
 20   if (iadd .ne. 0) getelm = a(iadd) 
c
      return
c--------end-of-getelm--------------------------------------------------
c-----------------------------------------------------------------------
      end 
c------------------------------------------------------------------------ 
      subroutine getl (n,a,ja,ia,ao,jao,iao)
      integer n, ia(*), ja(*), iao(*), jao(*)
      real*8 a(*), ao(*)
c------------------------------------------------------------------------
c this subroutine extracts the lower triangular part of a matrix 
c and writes the result ao, jao, iao. The routine is in place in
c that ao, jao, iao can be the same as a, ja, ia if desired.
c-----------
c on input:
c
c n     = dimension of the matrix a.
c a, ja, 
c    ia = matrix stored in compressed sparse row format.
c On return:
c ao, jao, 
c    iao = lower triangular matrix (lower part of a) 
c	stored in a, ja, ia, format
c note: the diagonal element is the last element in each row.
c i.e. in  a(ia(i+1)-1 ) 
c ao, jao, iao may be the same as a, ja, ia on entry -- in which case
c getl will overwrite the result on a, ja, ia.
c
c------------------------------------------------------------------------
c local variables
      real*8 t
      integer ko, kold, kdiag, k, i
c
c inititialize ko (pointer for output matrix)
c
      ko = 0
      do  7 i=1, n
         kold = ko
         kdiag = 0
         do 71 k = ia(i), ia(i+1) -1
            if (ja(k)  .gt. i) goto 71
            ko = ko+1
            ao(ko) = a(k)
            jao(ko) = ja(k)
            if (ja(k)  .eq. i) kdiag = ko
 71      continue
         if (kdiag .eq. 0 .or. kdiag .eq. ko) goto 72
c
c     exchange
c
         t = ao(kdiag)
         ao(kdiag) = ao(ko)
         ao(ko) = t
c     
         k = jao(kdiag)
         jao(kdiag) = jao(ko)
         jao(ko) = k
 72      iao(i) = kold+1
 7    continue
c     redefine iao(n+1)
      iao(n+1) = ko+1
      return
c----------end-of-getl ------------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine getsten (nx,ny,nz,mode,kx,ky,kz,stencil,h,rhs)
      integer nx,ny,nz,mode,kx,ky,kz
      real*8 stencil(*),h,rhs,afun,bfun,cfun,dfun,efun,ffun,gfun,hfun
      external afun,bfun,cfun,dfun,efun,ffun,gfun,hfun
c-----------------------------------------------------------------------
c     This subroutine calculates the correct stencil values for
c     centered difference discretization of the elliptic operator
c     and the right-hand side
c
c L u = delx( A delx u ) + dely ( B dely u) + delz ( C delz u ) +
c	delx ( D u ) + dely (E u) + delz( F u ) + G u = H
c
c   For 2-D problems the discretization formula that is used is:
c
c h**2 * Lu == A(i+1/2,j)*{u(i+1,j) - u(i,j)} +
c	       A(i-1/2,j)*{u(i-1,j) - u(i,j)} +
c              B(i,j+1/2)*{u(i,j+1) - u(i,j)} +
c              B(i,j-1/2)*{u(i,j-1) - u(i,j)} +
c              (h/2)*D(i,j)*{u(i+1,j) - u(i-1,j)} +
c              (h/2)*E(i,j)*{u(i,j+1) - u(i,j-1)} +
c              (h/2)*E(i,j)*{u(i,j+1) - u(i,j-1)} +
c              (h**2)*G(i,j)*u(i,j)
c-----------------------------------------------------------------------
c     some constants
c
      real*8 zero, half
      parameter (zero=0.0D0,half=0.5D0)
c
c     local variables
c
      integer k
      real*8 hhalf,cntr, x, y, z, coeff
c
c     if mode < 0, we shouldn't have come here
c
      if (mode .lt. 0) return
c
      do 200 k=1,7
         stencil(k) = zero
 200  continue
c
      hhalf = h*half
      x = h*dble(kx)
      y = h*dble(ky)
      z = h*dble(kz)
      cntr = zero
c     differentiation wrt x:
      coeff = afun(x+hhalf,y,z)
      stencil(3) = stencil(3) + coeff
      cntr = cntr + coeff
c
      coeff = afun(x-hhalf,y,z)
      stencil(2) = stencil(2) + coeff
      cntr = cntr + coeff
c
      coeff = dfun(x,y,z)*hhalf
      stencil(3) = stencil(3) + coeff
      stencil(2) = stencil(2) - coeff
      if (ny .le. 1) goto 99
c
c     differentiation wrt y:
c
      coeff = bfun(x,y+hhalf,z)
      stencil(5) = stencil(5) + coeff
      cntr = cntr + coeff
c
      coeff = bfun(x,y-hhalf,z)
      stencil(4) = stencil(4) + coeff
      cntr = cntr + coeff
c
      coeff = efun(x,y,z)*hhalf
      stencil(5) = stencil(5) + coeff
      stencil(4) = stencil(4) - coeff
      if (nz .le. 1) goto 99
c
c differentiation wrt z:
c
      coeff = cfun(x,y,z+hhalf)
      stencil(7) = stencil(7) + coeff
      cntr = cntr + coeff
c
      coeff = cfun(x,y,z-hhalf)
      stencil(6) = stencil(6) + coeff
      cntr = cntr + coeff
c
      coeff = ffun(x,y,z)*hhalf
      stencil(7) = stencil(7) + coeff
      stencil(6) = stencil(6) - coeff
c
c contribution from function G:
c
 99   coeff = gfun(x,y,z)
      stencil(1) = h*h*coeff - cntr
c
c     the right-hand side
c
      if (mode .gt. 0) rhs = h*h*hfun(x,y,z)
c
      return
c------end-of-getsten---------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine getu (n,a,ja,ia,ao,jao,iao)
      integer n, ia(*), ja(*), iao(*), jao(*)
      real*8 a(*), ao(*)
c------------------------------------------------------------------------
c this subroutine extracts the upper triangular part of a matrix 
c and writes the result ao, jao, iao. The routine is in place in
c that ao, jao, iao can be the same as a, ja, ia if desired.
c-----------
c on input:
c
c n     = dimension of the matrix a.
c a, ja, 
c    ia = matrix stored in a, ja, ia, format
c On return:
c ao, jao, 
c    iao = upper triangular matrix (upper part of a) 
c	stored in compressed sparse row format
c note: the diagonal element is the last element in each row.
c i.e. in  a(ia(i+1)-1 ) 
c ao, jao, iao may be the same as a, ja, ia on entry -- in which case
c getu will overwrite the result on a, ja, ia.
c
c------------------------------------------------------------------------
c local variables
      real*8 t 
      integer ko, k, i, kdiag, kfirst 
      ko = 0
      do  7 i=1, n
         kfirst = ko+1
         kdiag = 0
         do 71 k = ia(i), ia(i+1) -1
            if (ja(k)  .lt. i) goto 71
            ko = ko+1
            ao(ko) = a(k)
            jao(ko) = ja(k)
            if (ja(k)  .eq. i) kdiag = ko
 71      continue
         if (kdiag .eq. 0 .or. kdiag .eq. kfirst) goto 72
c     exchange
         t = ao(kdiag)
         ao(kdiag) = ao(kfirst)
         ao(kfirst) = t
c     
         k = jao(kdiag)
         jao(kdiag) = jao(kfirst)
         jao(kfirst) = k
 72      iao(i) = kfirst
 7    continue
c     redefine iao(n+1)
      iao(n+1) = ko+1
      return
c----------end-of-getu ------------------------------------------------- 
c-----------------------------------------------------------------------
      end
      subroutine givens(x,y,c,s)
      real*8 x,y,c,s
c-----------------------------------------------------------------------
c     Given x and y, this subroutine generates a Givens' rotation c, s.
c     And apply the rotation on (x,y) ==> (sqrt(x**2 + y**2), 0).
c     (See P 202 of "matrix computation" by Golub and van Loan.)
c-----------------------------------------------------------------------
      real*8 t,one,zero
      parameter (zero=0.0D0,one=1.0D0)
c
      if (x.eq.zero .and. y.eq.zero) then
         c = one
         s = zero
      else if (abs(y).gt.abs(x)) then
         t = x / y
         x = sqrt(one+t*t)
         s = sign(one / x, y)
         c = t*s
      else if (abs(y).le.abs(x)) then
         t = y / x
         y = sqrt(one+t*t)
         c = sign(one / y, x)
         s = t*c
      else
c
c     X or Y must be an invalid floating-point number, set both to zero
c
         x = zero
         y = zero
         c = one
         s = zero
      endif
      x = abs(x*y)
c
c     end of givens
c
      return
      end
c-----end-of-fom-------------------------------------------------------- 
c-----------------------------------------------------------------------
      subroutine gmres(n, rhs, sol, ipar, fpar, w)
      implicit none
      integer n, ipar(16)
      real*8 rhs(n), sol(n), fpar(16), w(*)
c-----------------------------------------------------------------------
c     This a version of GMRES implemented with reverse communication.
c     It is a simple restart version of the GMRES algorithm.
c
c     ipar(5) == the dimension of the Krylov subspace
c     after every ipar(5) iterations, the GMRES will restart with
c     the updated solution and recomputed residual vector.
c
c     the space of the `w' is used as follows:
c     (1) the basis for the Krylov subspace, size n*(m+1);
c     (2) the Hessenberg matrix, only the upper triangular
c     portion of the matrix is stored, size (m+1)*m/2 + 1
c     (3) three vectors, all are of size m, they are
c     the cosine and sine of the Givens rotations, the third one holds
c     the residuals, it is of size m+1.
c
c     TOTAL SIZE REQUIRED == (n+3)*(m+2) + (m+1)*m/2
c     Note: m == ipar(5). The default value for this is 15 if
c     ipar(5) <= 1.
c-----------------------------------------------------------------------
c     external functions used
c
      real*8 distdot
      external distdot
c
      real*8 one, zero
      parameter(one=1.0D0, zero=0.0D0)
c
c     local variables, ptr and p2 are temporary pointers,
c     hess points to the Hessenberg matrix,
c     vc, vs point to the cosines and sines of the Givens rotations
c     vrn points to the vectors of residual norms, more precisely
c     the right hand side of the least square problem solved.
c
      integer i,ii,idx,k,m,ptr,p2,hess,vc,vs,vrn
      real*8 alpha, c, s
      logical lp, rp
      save
c
c     check the status of the call
c
      if (ipar(1).le.0) ipar(10) = 0
      goto (10, 20, 30, 40, 50, 60, 70) ipar(10)
c
c     initialization
c
      if (ipar(5).le.1) then
         m = 15
      else
         m = ipar(5)
      endif
      idx = n * (m+1)
      hess = idx + n
      vc = hess + (m+1) * m / 2 + 1
      vs = vc + m
      vrn = vs + m
      i = vrn + m + 1
      call bisinit(ipar,fpar,i,1,lp,rp,w)
      if (ipar(1).lt.0) return
c
c     request for matrix vector multiplication A*x in the initialization
c
 100  ipar(1) = 1
      ipar(8) = n+1
      ipar(9) = 1
      ipar(10) = 1
      k = 0
      do i = 1, n
         w(n+i) = sol(i)
      enddo
      return
 10   ipar(7) = ipar(7) + 1
      ipar(13) = ipar(13) + 1
      if (lp) then
         do i = 1, n
            w(n+i) = rhs(i) - w(i)
         enddo
         ipar(1) = 3
         ipar(10) = 2
         return
      else
         do i = 1, n
            w(i) = rhs(i) - w(i)
         enddo
      endif
      fpar(11) = fpar(11) + n
c
 20   alpha = sqrt(distdot(n,w,1,w,1))
      fpar(11) = fpar(11) + 2*n
      if (ipar(7).eq.1 .and. ipar(3).ne.999) then
         if (abs(ipar(3)).eq.2) then
            fpar(4) = fpar(1) * sqrt(distdot(n,rhs,1,rhs,1)) + fpar(2)
            fpar(11) = fpar(11) + 2*n
         else
            fpar(4) = fpar(1) * alpha + fpar(2)
         endif
         fpar(3) = alpha
      endif
      fpar(5) = alpha
      w(vrn+1) = alpha
      if (alpha.le.fpar(4) .and. ipar(3).ge.0 .and. ipar(3).ne.999) then
         ipar(1) = 0
         fpar(6) = alpha
         goto 300
      endif
      alpha = one / alpha
      do ii = 1, n
         w(ii) = alpha * w(ii)
      enddo
      fpar(11) = fpar(11) + n
c
c     request for (1) right preconditioning
c     (2) matrix vector multiplication
c     (3) left preconditioning
c
 110  k = k + 1
      if (rp) then
         ipar(1) = 5
         ipar(8) = k*n - n + 1
         if (lp) then
            ipar(9) = k*n + 1
         else
            ipar(9) = idx + 1
         endif
         ipar(10) = 3
         return
      endif
c
 30   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = (k-1)*n + 1
      endif
      if (lp) then
         ipar(9) = idx + 1
      else
         ipar(9) = 1 + k*n
      endif
      ipar(10) = 4
      return
c
 40   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = k*n + 1
         ipar(10) = 5
         return
      endif
c
c     Modified Gram-Schmidt orthogonalization procedure
c     temporary pointer 'ptr' is pointing to the current column of the
c     Hessenberg matrix. 'p2' points to the new basis vector
c
 50   ipar(7) = ipar(7) + 1
      ptr = k * (k - 1) / 2 + hess
      p2 = ipar(9)
      call mgsro(.false.,n,n,k+1,k+1,fpar(11),w,w(ptr+1),
     $     ipar(12))
      if (ipar(12).lt.0) goto 200
c
c     apply previous Givens rotations and generate a new one to eliminate
c     the subdiagonal element.
c
      p2 = ptr + 1
      do i = 1, k-1
         ptr = p2
         p2 = p2 + 1
         alpha = w(ptr)
         c = w(vc+i)
         s = w(vs+i)
         w(ptr) = c * alpha + s * w(p2)
         w(p2) = c * w(p2) - s * alpha
      enddo
      call givens(w(p2), w(p2+1), c, s)
      w(vc+k) = c
      w(vs+k) = s
      p2 = vrn + k
      alpha = - s * w(p2)
      w(p2) = c * w(p2)
      w(p2+1) = alpha
c
c     end of one Arnoldi iteration, alpha will store the estimated
c     residual norm at current stage
c
      fpar(11) = fpar(11) + 6*k + 2
      alpha = abs(alpha)
      fpar(5) = alpha
      if (k.lt.m .and. .not.(ipar(3).ge.0 .and. alpha.le.fpar(4))
     +     .and. (ipar(6).le.0 .or. ipar(7).lt.ipar(6))) goto 110
c
c     update the approximate solution, first solve the upper triangular
c     system, temporary pointer ptr points to the Hessenberg matrix,
c     p2 points to the right-hand-side (also the solution) of the system.
c
 200  ptr = hess + k * (k + 1) / 2
      p2 = vrn + k
      if (w(ptr).eq.zero) then
c
c     if the diagonal elements of the last column is zero, reduce k by 1
c     so that a smaller trianguler system is solved [It should only
c     happen when the matrix is singular, and at most once!]
c
         k = k - 1
         if (k.gt.0) then
            goto 200
         else
            ipar(1) = -3
            ipar(12) = -4
            goto 300
         endif
      endif
      w(p2) = w(p2) / w(ptr)
      do i = k-1, 1, -1
         ptr = ptr - i - 1
         do ii = 1, i
            w(vrn+ii) = w(vrn+ii) - w(p2) * w(ptr+ii)
         enddo
         p2 = p2 - 1
         w(p2) = w(p2) / w(ptr)
      enddo
c
      do ii = 1, n
         w(ii) = w(ii) * w(p2)
      enddo
      do i = 1, k-1
         ptr = i*n
         p2 = p2 + 1
         do ii = 1, n
            w(ii) = w(ii) + w(p2) * w(ptr+ii)
         enddo
      enddo
      fpar(11) = fpar(11) + 2*k*n - n + k*(k+1)
c
      if (rp) then
         ipar(1) = 5
         ipar(8) = 1
         ipar(9) = idx + 1
         ipar(10) = 6
         return
      endif
c
 60   if (rp) then
         do i = 1, n
            sol(i) = sol(i) + w(idx+i)
         enddo
      else
         do i = 1, n
            sol(i) = sol(i) + w(i)
         enddo
      endif
      fpar(11) = fpar(11) + n
c
c     process the complete stopping criteria
c
      if (ipar(3).eq.999) then
         ipar(1) = 10
         ipar(8) = -1
         ipar(9) = idx + 1
         ipar(10) = 7
         return
      else if (ipar(3).lt.0) then
         if (ipar(7).le.m+1) then
            fpar(3) = abs(w(vrn+1))
            if (ipar(3).eq.-1) fpar(4) = fpar(1)*fpar(3)+fpar(2)
         endif
         fpar(6) = abs(w(vrn+k))
      else
         fpar(6) = fpar(5)
      endif
c
c     do we need to restart ?
c
 70   if (ipar(12).ne.0) then
         ipar(1) = -3
         goto 300
      endif
      if ((ipar(7).lt.ipar(6) .or. ipar(6).le.0) .and.
     +     ((ipar(3).eq.999.and.ipar(11).eq.0) .or.
     +     (ipar(3).ne.999.and.fpar(6).gt.fpar(4)))) goto 100
c
c     termination, set error code, compute convergence rate
c
      if (ipar(1).gt.0) then
         if (ipar(3).eq.999 .and. ipar(11).eq.1) then
            ipar(1) = 0
         else if (ipar(3).ne.999 .and. fpar(6).le.fpar(4)) then
            ipar(1) = 0
         else if (ipar(7).ge.ipar(6) .and. ipar(6).gt.0) then
            ipar(1) = -1
         else
            ipar(1) = -10
         endif
      endif
 300  if (fpar(3).ne.zero .and. fpar(6).ne.zero .and.
     +     ipar(7).gt.ipar(13)) then
         fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7)-ipar(13))
      else
         fpar(7) = zero
      endif
      return
      end
c-------------------------------------------------------
        subroutine gradi3(nel, xe, ye, dn, det,ierr)
c-------------------------------------------------------
c constructs the first derivative of the shape functions.
c arguments:
c nel	= element nuumber
c xy, ye= coordinates of the three nodal points in an element.
c dn	= gradients (1-st derivatives) of the shape functions.
c area	= area of the triangle
c
c------------------------------------------------------- 
   	implicit real*8 (a-h,o-z)
	dimension xe(3), ye(3), dn(3,2)
        data eps/1.d-17/
c compute area
	ierr = 0 
	if (det .le. eps) goto 100
c
	dn(1,1) = (ye(2)-ye(3))/det
	dn(2,1) = (ye(3)-ye(1))/det
	dn(3,1) = (ye(1)-ye(2))/det
	dn(1,2) = (xe(3)-xe(2))/det
	dn(2,2) = (xe(1)-xe(3))/det
	dn(3,2) = (xe(2)-xe(1))/det
c
	return
c
 100	continue
	ierr = 3
  	write(iout,*) 'ERROR:negative area encountered at elmt: ',nel
c	write(iout,*) det,(xe(i),ye(i),i=1,3)
	return
	end
c----------------------------------------------------------------------      
      subroutine HeapInsert (a,ind,rind,jkey,child,node) 
      integer a(*),ind(*),rind(*),jkey,child,node
c----------------------------------------------------------------------
c inserts a key to a heap from `node'. Checks values up
c only -- i.e.,assumes that the subtree (if any) whose root
c is node is such that the keys are all inferior to those
c to ge inserted. 
c 
c child is where the key ended up.
c---------------------------------------------------------------------- 
c---- local variables 
      integer parent,xkey,ikey
      xkey = a(jkey) 
      ikey = ind(jkey) 
c      node = node + 1 
      a(node) = xkey 
      ind(node) = ikey 
      rind(ikey) = node 
      if (node .le. 1) return
      child=node 
 1    parent = child/2  
      if (a(parent) .le. a(child)) goto 2
      call interchange(a,ind,rind,child,parent) 
      child = parent 
      if (child .gt. 1) goto 1 
 2    continue
      return
      end 
c      
      subroutine HeapInsertM (a,ind,rind,jkey,child,node) 
      integer a(*),ind(*),rind(*),jkey,child,node
c----------------------------------------------------------------------
c inserts a key to a heap from `node'. Checks values up
c only -- i.e.,assumes that the subtree (if any) whose root
c is node is such that the keys are all inferior to those
c to ge inserted. 
c 
c child is where the key ended up.
c---------------------------------------------------------------------- 
c---- local variables 
      integer parent,xkey,ikey
      xkey = a(jkey) 
      ikey = ind(jkey) 
c      node = node + 1 
      a(node) = xkey 
      ind(node) = ikey 
      rind(ikey) = node 
      if (node .le. 1) return
      child=node 
 1    parent = child/2  
      if (a(parent) .ge. a(child)) goto 2
      call interchange(a,ind,rind,child,parent) 
      child = parent 
      if (child .gt. 1) goto 1 
 2    continue
      return
      end 
c-----------------------------------------------------------------------
      subroutine HeapSort (a,ind,rind,n,ncut)
      integer a(*),ind(n),rind(n),n, ncut 
c----------------------------------------------------------------------  
c integer version -- min heap sorts decreasinly. 
c---------------------------------------------------------------------- 
c sorts inger keys in array a increasingly and permutes the companion
c array ind rind accrodingly. 
c n    = size of array
c ncut = integer indicating when to cut the process.the process is
c        stopped after ncut outer steps of the heap-sort algorithm.
c        The first ncut values are sorted and they are the smallest
c        ncut values of the array.
c----------------------------------------------------------------------
c local variables 
c
      integer i,last, j,jlast
c   
c    Heap sort algorithm ---
c    
c    build heap 
       do 1 i=n/2,1,-1 
          j = i
          call FixHeap (a,ind,rind,j,j,n) 
 1    continue
c     
c   done -- now remove keys one by one 
c
      jlast = max(2,n-ncut+1)
      do 2 last=n,jlast,-1 
         call moveback (a,ind,rind,last) 
 2    continue
      return
      end 
c-----------------------------------------------------------------------
      subroutine hes (ndg,m1,hh,ih,dt,y,root,coef,coef0,w2)
c     implicit  real*8 (a-h,o-z)
      integer ndg, m1, ih 
      real*8 hh(ih+2,m1), y(m1)
      complex*16   coef(ndg), root(ndg), w2(m1)
      real*8 dt, coef0
c--------------------------------------------------------------------  
c computes phi ( H dt) * y    (1)
c where H = Hessenberg matrix (hh)
c y	  = arbitrary vector.
c ----------------------------
c ndg	= number of poles as determined by getrat
c m1    = dimension of hessenberg matrix
c hh	= hessenberg matrix (real)
c ih+2	= first dimension of hh
c dt	= scaling factor used for hh (see (1)) 
c y	= real vector. on return phi(H dt ) y is computed
c         and overwritten on y.
c root  = poles of the rational approximation to phi as
c         computed by getrat
c coef, 
c coef0 = coefficients of partial fraction phiansion 
c         
c  phi(t) ~ coef0 +  sum     Real [   coef(i) / (t - root(i)  ]
c                  i=1,ndg  
c
c valid for real t.
c coef0 is real, coef(*) is a complex array.
c
c--------------------------------------------------------------------  
c local variables 
c
      integer m1max
      parameter (m1max=70) 
      complex*16   hloc(m1max+1,m1max), t, zpiv, dcmplx
      real*8 yloc(m1max), dble
      integer i, j, ii
c     
c      if (m1 .gt. m1max) print *, ' *** ERROR : In HES, M+1 TOO LARGE'
c     
c     loop associated with the poles.
c     
      do 10 j=1,m1
         yloc(j) = y(j)
         y(j)    = y(j)*coef0
 10   continue
c     
      do 8 ii = 1, ndg
c     
c     copy Hessenberg matrix into temporary
c     
         do 2 j=1, m1
            do 1 i=1, j+1
               hloc(i,j) = dcmplx( dt*hh(i,j) )
 1          continue
            hloc(j,j) = hloc(j,j) - root(ii) 
            w2(j)     = dcmplx(yloc(j)) 
 2       continue 
c
c forward solve 
c 
         do 4 i=2,m1
            zpiv  = hloc(i,i-1) / hloc(i-1,i-1)
            do 3 j=i,m1
               hloc(i,j) = hloc(i,j) - zpiv*hloc(i-1,j)
 3          continue
            w2(i)     = w2(i) - zpiv*w2(i-1)
 4       continue 
c     
c     backward solve
c     
         do 6 i=m1,1,-1 
            t=w2(i)
            do 5 j=i+1,m1
               t = t-hloc(i,j)*w2(j)
 5          continue
            w2(i) = t/hloc(i,i)
 6       continue
c     
c     accumulate result in y.
c     
         do 7 i=1,m1
            y(i) = y(i) + dble ( coef(ii) * w2(i) ) 
 7       continue
 8    continue
      return
      end
      
      function hfun (x,y,z)
      real*8 hfun, x,y, z, gammax, gammay, alpha
      common /func/ gammax, gammay, alpha
      hfun = alpha * sin(gammax*x+gammay*y-z)
      return 
      end
c----------------------------------------------------------------------- 
        subroutine hsourc (indic,nx,nelx,node,x,y,ijk,fs,f) 
	implicit real*8 (a-h,o-z) 
        real*8 x(*),y(*),fs(*),f(*),xe(3),ye(3),det,areao3
	integer ijk(node,*)
c
c generates the load vector f in assembled/unassembled form from the
c the element contributions fs. 
c indic = indicates if f is to be assembled (1) or not (zero) 
c note: f(*) not initilazed. because might use values from boundary 
c conditions.
c 
	jnod = 0
	do 130 nel = 1,nelx
c
c get coordinates of nodal points
c	
	do 104 i=1, node
	j = ijk(i,nel)
	xe(i) = x(j)
	ye(i) = y(j)
 104	continue
c
c compute determinant
c
	det=xe(2)*(ye(3)-ye(1))+xe(3)*(ye(1)-ye(2))+xe(1)*(ye(2)-ye(3))
c area3 = area/3 
	areao3 = det/6.0
c 
c contributions to nodes in the element
c 
	if (indic .eq. 0) then
	   do 115 ka=1,node
	   jnod = jnod+1
	   f(jnod) = fs(nel)*areao3
 115	   continue
	else
	    do 120 ka=1, node
            ii = ijk(ka,nel)
	    f(ii) = f(ii) + fs(nel)*areao3
 120	    continue
	endif 
c
 130	continue
	return
        end

      integer function idamax(n,dx,incx)
c
c     finds the index of element having max. absolute value.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dmax
      integer i,incx,ix,n
c
      idamax = 0
      if( n .lt. 1 ) return
      idamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      dmax = dabs(dx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(dabs(dx(ix)).le.dmax) go to 5
         idamax = i
         dmax = dabs(dx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 dmax = dabs(dx(1))
      do 30 i = 2,n
         if(dabs(dx(i)).le.dmax) go to 30
         idamax = i
         dmax = dabs(dx(i))
   30 continue
      return
      end
c----------------------------------------------------------------------
	subroutine ilu0(n, a, ja, ia, alu, jlu, ju, iw, ierr)
	implicit real*8 (a-h,o-z)
	real*8 a(*), alu(*)
        integer ja(*), ia(*), ju(*), jlu(*), iw(*)
c------------------ right preconditioner ------------------------------*
c                    ***   ilu(0) preconditioner.   ***                *
c----------------------------------------------------------------------*
c Note that this has been coded in such a way that it can be used
c with pgmres. Normally, since the data structure of the L+U matrix is
c the same as that the A matrix, savings can be made. In fact with
c some definitions (not correct for general sparse matrices) all we
c need in addition to a, ja, ia is an additional diagonal.
c ILU0 is not recommended for serious problems. It is only provided
c here for comparison purposes.
c-----------------------------------------------------------------------
c
c on entry:
c---------
c n       = dimension of matrix
c a, ja,
c ia      = original matrix in compressed sparse row storage.
c
c on return:
c-----------
c alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
c           the L and U factors together. The diagonal (stored in
c           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
c           contains the i-th row of L (excluding the diagonal entry=1)
c           followed by the i-th row of U.
c
c ju	  = pointer to the diagonal elements in alu, jlu.
c
c ierr	  = integer indicating error code on return
c	     ierr = 0 --> normal return
c	     ierr = k --> code encountered a zero pivot at step k.
c work arrays:
c-------------
c iw	    = integer work array of length n.
c------------
c IMPORTANT
c-----------
c it is assumed that the the elements in the input matrix are stored
c    in such a way that in each row the lower part comes first and
c    then the upper part. To get the correct ILU factorization, it is
c    also necessary to have the elements of L sorted by increasing
c    column number. It may therefore be necessary to sort the
c    elements of a, ja, ia prior to calling ilu0. This can be
c    achieved by transposing the matrix twice using csrcsc.
c
c-----------------------------------------------------------------------
        ju0 = n+2
        jlu(1) = ju0
c
c initialize work vector to zero's
c
	do 31 i=1, n
           iw(i) = 0
 31     continue
c
c main loop
c
	do 500 ii = 1, n
           js = ju0
c
c generating row number ii of L and U.
c
           do 100 j=ia(ii),ia(ii+1)-1
c
c     copy row ii of a, ja, ia into row ii of alu, jlu (L/U) matrix.
c
              jcol = ja(j)
              if (jcol .eq. ii) then
                 alu(ii) = a(j)
                 iw(jcol) = ii
                 ju(ii)  = ju0
              else
                 alu(ju0) = a(j)
                 jlu(ju0) = ja(j)
                 iw(jcol) = ju0
                 ju0 = ju0+1
              endif
 100       continue
           jlu(ii+1) = ju0
           jf = ju0-1
           jm = ju(ii)-1
c
c     exit if diagonal element is reached.
c
           do 150 j=js, jm
              jrow = jlu(j)
              tl = alu(j)*alu(jrow)
              alu(j) = tl
c
c     perform  linear combination
c
              do 140 jj = ju(jrow), jlu(jrow+1)-1
                 jw = iw(jlu(jj))
                 if (jw .ne. 0) alu(jw) = alu(jw) - tl*alu(jj)
 140          continue
 150       continue
c
c     invert  and store diagonal element.
c
           if (alu(ii) .eq. 0.0d0) goto 600
           alu(ii) = 1.0d0/alu(ii)
c
c     reset pointer iw to zero
c
           iw(ii) = 0
           do 201 i = js, jf
 201          iw(jlu(i)) = 0
 500       continue
           ierr = 0
           return
c
c     zero pivot :
c
 600       ierr = ii
c
           return
c------- end-of-ilu0 ---------------------------------------------------
c-----------------------------------------------------------------------
           end
c-----------------------------------------------------------------------
      subroutine ilud(n,a,ja,ia,alph,tol,alu,jlu,ju,iwk,w,jw,ierr)
c-----------------------------------------------------------------------
      implicit none 
      integer n
      real*8 a(*),alu(*),w(2*n),tol, alph 
      integer ja(*),ia(n+1),jlu(*),ju(n),jw(2*n),iwk,ierr
c----------------------------------------------------------------------*
c                     *** ILUD preconditioner ***                      *
c    incomplete LU factorization with standard droppoing strategy      *
c----------------------------------------------------------------------*
c Author: Yousef Saad * Aug. 1995 --                                   * 
c----------------------------------------------------------------------*
c This routine computes the ILU factorization with standard threshold  *
c dropping: at i-th step of elimination, an element a(i,j) in row i is *
c dropped  if it satisfies the criterion:                              *
c                                                                      *
c  abs(a(i,j)) < tol * [average magnitude of elements in row i of A]   *
c                                                                      *
c There is no control on memory size required for the factors as is    *
c done in ILUT. This routines computes also various diagonal compensa- * 
c tion ILU's such MILU. These are defined through the parameter alph   *
c----------------------------------------------------------------------* 
c on entry:
c========== 
c n       = integer. The row dimension of the matrix A. The matrix 
c
c a,ja,ia = matrix stored in Compressed Sparse Row format              
c
c alph    = diagonal compensation parameter -- the term: 
c
c           alph*(sum of all dropped out elements in a given row) 
c
c           is added to the diagonal element of U of the factorization 
c           Thus: alph = 0 ---> ~ ILU with threshold,
c                 alph = 1 ---> ~ MILU with threshold. 
c 
c tol     = Threshold parameter for dropping small terms in the
c           factorization. During the elimination, a term a(i,j) is 
c           dropped whenever abs(a(i,j)) .lt. tol * [weighted norm of
c           row i]. Here weighted norm = 1-norm / number of nnz 
c           elements in the row. 
c  
c iwk     = The length of arrays alu and jlu -- this routine will stop
c           if storage for the factors L and U is not sufficient 
c
c On return:
c=========== 
c
c alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
c           the L and U factors together. The diagonal (stored in
c           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
c           contains the i-th row of L (excluding the diagonal entry=1)
c           followed by the i-th row of U.
c
c ju      = integer array of length n containing the pointers to
c           the beginning of each row of U in the matrix alu,jlu.
c
c ierr    = integer. Error message with the following meaning.
c           ierr  = 0    --> successful return.
c           ierr .gt. 0  --> zero pivot encountered at step number ierr.
c           ierr  = -1   --> Error. input matrix may be wrong.
c                            (The elimination process has generated a
c                            row in L or U whose length is .gt.  n.)
c           ierr  = -2   --> Insufficient storage for the LU factors --
c                            arrays alu/ jalu are  overflowed. 
c           ierr  = -3   --> Zero row encountered.
c
c Work Arrays:
c=============
c jw      = integer work array of length 2*n.
c w       = real work array of length n 
c  
c----------------------------------------------------------------------
c
c w, ju (1:n) store the working array [1:ii-1 = L-part, ii:n = u] 
c jw(n+1:2n)  stores the nonzero indicator. 
c 
c Notes:
c ------
c All diagonal elements of the input matrix must be  nonzero.
c
c----------------------------------------------------------------------- 
c     locals
      integer ju0,k,j1,j2,j,ii,i,lenl,lenu,jj,jrow,jpos,len 
      real*8 tnorm, t, abs, s, fact, dropsum  
c-----------------------------------------------------------------------
c     initialize ju0 (points to next element to be added to alu,jlu)
c     and pointer array.
c-----------------------------------------------------------------------
      ju0 = n+2
      jlu(1) = ju0
c
c     initialize nonzero indicator array. 
c
      do 1 j=1,n
         jw(n+j)  = 0
 1    continue
c-----------------------------------------------------------------------
c     beginning of main loop.
c-----------------------------------------------------------------------
      do 500 ii = 1, n
         j1 = ia(ii)
         j2 = ia(ii+1) - 1
         dropsum = 0.0d0 
         tnorm = 0.0d0
         do 501 k=j1,j2
            tnorm = tnorm + abs(a(k)) 
 501     continue
         if (tnorm .eq. 0.0) goto 997
         tnorm = tnorm / real(j2-j1+1) 
c     
c     unpack L-part and U-part of row of A in arrays w 
c     
         lenu = 1
         lenl = 0
         jw(ii) = ii
         w(ii) = 0.0
         jw(n+ii) = ii
c
         do 170  j = j1, j2
            k = ja(j)
            t = a(j)
            if (k .lt. ii) then
               lenl = lenl+1
               jw(lenl) = k
               w(lenl) = t
               jw(n+k) = lenl
            else if (k .eq. ii) then
               w(ii) = t
            else
               lenu = lenu+1
               jpos = ii+lenu-1 
               jw(jpos) = k
               w(jpos) = t
               jw(n+k) = jpos
            endif
 170     continue
         jj = 0
         len = 0 
c     
c     eliminate previous rows
c     
 150     jj = jj+1
         if (jj .gt. lenl) goto 160
c-----------------------------------------------------------------------
c     in order to do the elimination in the correct order we must select
c     the smallest column index among jw(k), k=jj+1, ..., lenl.
c-----------------------------------------------------------------------
         jrow = jw(jj)
         k = jj
c     
c     determine smallest column index
c     
         do 151 j=jj+1,lenl
            if (jw(j) .lt. jrow) then
               jrow = jw(j)
               k = j
            endif
 151     continue
c
         if (k .ne. jj) then
c     exchange in jw
            j = jw(jj)
            jw(jj) = jw(k)
            jw(k) = j
c     exchange in jr
            jw(n+jrow) = jj
            jw(n+j) = k
c     exchange in w
            s = w(jj)
            w(jj) = w(k)
            w(k) = s
         endif
c
c     zero out element in row by setting resetting jw(n+jrow) to zero.
c     
         jw(n+jrow) = 0
c
c     drop term if small
c     
c         if (abs(w(jj)) .le. tol*tnorm) then
c            dropsum = dropsum + w(jj) 
c            goto 150
c         endif
c     
c     get the multiplier for row to be eliminated (jrow).
c     
         fact = w(jj)*alu(jrow)
c
c     drop term if small
c     
         if (abs(fact) .le. tol) then
            dropsum = dropsum + w(jj) 
            goto 150
         endif
c     
c     combine current row and row jrow
c
         do 203 k = ju(jrow), jlu(jrow+1)-1
            s = fact*alu(k)
            j = jlu(k)
            jpos = jw(n+j)
            if (j .ge. ii) then
c     
c     dealing with upper part.
c     
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c     
                  lenu = lenu+1
                  if (lenu .gt. n) goto 995
                  i = ii+lenu-1
                  jw(i) = j
                  jw(n+j) = i
                  w(i) = - s
               else
c
c     this is not a fill-in element 
c
                  w(jpos) = w(jpos) - s
               endif
            else
c     
c     dealing with lower part.
c     
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c
                  lenl = lenl+1
                  if (lenl .gt. n) goto 995
                  jw(lenl) = j
                  jw(n+j) = lenl
                  w(lenl) = - s
               else
c
c     this is not a fill-in element 
c
                  w(jpos) = w(jpos) - s
               endif
            endif
 203     continue
         len = len+1 
         w(len) = fact
         jw(len)  = jrow
         goto 150
 160     continue
c     
c     reset double-pointer to zero (For U-part only)
c     
         do 308 k=1, lenu
            jw(n+jw(ii+k-1)) = 0
 308     continue
c
c     update l-matrix
c
         do 204 k=1, len
            if (ju0 .gt. iwk) goto 996
            alu(ju0) =  w(k) 
            jlu(ju0) =  jw(k)
            ju0 = ju0+1
 204     continue
c     
c     save pointer to beginning of row ii of U
c     
         ju(ii) = ju0
c
c     go through elements in U-part of w to determine elements to keep
c
         len = 0
         do k=1, lenu-1
c            if (abs(w(ii+k)) .gt. tnorm*tol) then 
            if (abs(w(ii+k)) .gt. abs(w(ii))*tol) then 
               len = len+1
               w(ii+len) = w(ii+k) 
               jw(ii+len) = jw(ii+k)
            else
               dropsum = dropsum + w(ii+k) 
            endif
         enddo
c
c     now update u-matrix
c
         if (ju0 + len-1 .gt. iwk) goto 996
         do 302 k=ii+1,ii+len
            jlu(ju0) = jw(k)
            alu(ju0) = w(k)
            ju0 = ju0+1
 302     continue
c
c     define diagonal element 
c 
         w(ii) = w(ii) + alph*dropsum 
c
c     store inverse of diagonal element of u
c              
         if (w(ii) .eq. 0.0) w(ii) = (0.0001 + tol)*tnorm
c     
         alu(ii) = 1.0d0/ w(ii) 
c     
c     update pointer to beginning of next row of U.
c     
         jlu(ii+1) = ju0
c-----------------------------------------------------------------------
c     end main loop
c-----------------------------------------------------------------------
 500  continue
      ierr = 0
      return
c
c     incomprehensible error. Matrix must be wrong.
c     
 995  ierr = -1
      return
c     
c     insufficient storage in alu/ jlu arrays for  L / U factors 
c     
 996  ierr = -2
      return
c     
c     zero row encountered
c     
 997  ierr = -3 
      return
c----------------end-of-ilud  ------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------
      subroutine iludp(n,a,ja,ia,alph,droptol,permtol,mbloc,alu,
     *     jlu,ju,iwk,w,jw,iperm,ierr)
c-----------------------------------------------------------------------
      implicit none
      integer n,ja(*),ia(n+1),mbloc,jlu(*),ju(n),jw(2*n),iwk,
     *     iperm(2*n),ierr
      real*8 a(*), alu(*), w(2*n), alph, droptol, permtol 
c----------------------------------------------------------------------*
c                     *** ILUDP preconditioner ***                     *
c    incomplete LU factorization with standard droppoing strategy      *
c    and column pivoting                                               * 
c----------------------------------------------------------------------*
c author Yousef Saad -- Aug 1995.                                      *
c----------------------------------------------------------------------*
c on entry:
c==========
c n       = integer. The dimension of the matrix A.
c
c a,ja,ia = matrix stored in Compressed Sparse Row format.
c           ON RETURN THE COLUMNS OF A ARE PERMUTED.
c
c alph    = diagonal compensation parameter -- the term: 
c
c           alph*(sum of all dropped out elements in a given row) 
c
c           is added to the diagonal element of U of the factorization 
c           Thus: alph = 0 ---> ~ ILU with threshold,
c                 alph = 1 ---> ~ MILU with threshold. 
c 
c droptol = tolerance used for dropping elements in L and U.
c           elements are dropped if they are .lt. norm(row) x droptol
c           row = row being eliminated
c
c permtol = tolerance ratio used for determning whether to permute
c           two columns.  Two columns are permuted only when 
c           abs(a(i,j))*permtol .gt. abs(a(i,i))
c           [0 --> never permute; good values 0.1 to 0.01]
c
c mbloc   = if desired, permuting can be done only within the diagonal
c           blocks of size mbloc. Useful for PDE problems with several
c           degrees of freedom.. If feature not wanted take mbloc=n.
c
c iwk     = integer. The declared lengths of arrays alu and jlu
c           if iwk is not large enough the code will stop prematurely
c           with ierr = -2 or ierr = -3 (see below).
c
c On return:
c===========
c
c alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
c           the L and U factors together. The diagonal (stored in
c           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
c           contains the i-th row of L (excluding the diagonal entry=1)
c           followed by the i-th row of U.
c
c ju      = integer array of length n containing the pointers to
c           the beginning of each row of U in the matrix alu,jlu.
c iperm   = contains the permutation arrays ..
c           iperm(1:n) = old numbers of unknowns
c           iperm(n+1:2*n) = reverse permutation = new unknowns.
c
c ierr    = integer. Error message with the following meaning.
c           ierr  = 0    --> successful return.
c           ierr .gt. 0  --> zero pivot encountered at step number ierr.
c           ierr  = -1   --> Error. input matrix may be wrong.
c                            (The elimination process has generated a
c                            row in L or U whose length is .gt.  n.)
c           ierr  = -2   --> The L/U matrix overflows the arrays alu,jlu
c           ierr  = -3   --> zero row encountered.
c
c work arrays:
c=============
c jw      = integer work array of length 2*n.
c w       = real work array of length 2*n 
c
c Notes:
c ------
c IMPORTANT: TO AVOID PERMUTING THE SOLUTION VECTORS ARRAYS FOR EACH 
c LU-SOLVE, THE MATRIX A IS PERMUTED ON RETURN. [all column indices are
c changed]. SIMILARLY FOR THE U MATRIX. 
c To permute the matrix back to its original state use the loop:
c
c      do k=ia(1), ia(n+1)-1
c         ja(k) = perm(ja(k)) 
c      enddo
c 
c-----------------------------------------------------------------------
c     local variables
c
      integer k,i,j,jrow,ju0,ii,j1,j2,jpos,len,imax,lenu,lenl,jj,icut
      real*8 s,tmp,tnorm,xmax,xmax0,fact,abs,t,dropsum 
c----------------------------------------------------------------------- 
c     initialize ju0 (points to next element to be added to alu,jlu)
c     and pointer array.
c-----------------------------------------------------------------------
      ju0 = n+2
      jlu(1) = ju0
c
c  integer double pointer array.
c
      do 1 j=1,n
         jw(n+j)  = 0
         iperm(j) = j
         iperm(n+j) = j
 1    continue
c-----------------------------------------------------------------------
c     beginning of main loop.
c-----------------------------------------------------------------------
      do 500 ii = 1, n
         j1 = ia(ii)
         j2 = ia(ii+1) - 1
         dropsum = 0.0d0 
         tnorm = 0.0d0
         do 501 k=j1,j2
            tnorm = tnorm+abs(a(k))
 501     continue
         if (tnorm .eq. 0.0) goto 997
         tnorm = tnorm/(j2-j1+1)
c
c     unpack L-part and U-part of row of A in arrays  w  --
c
         lenu = 1
         lenl = 0
         jw(ii) = ii
         w(ii) = 0.0
         jw(n+ii) = ii
c
         do 170  j = j1, j2
            k = iperm(n+ja(j))
            t = a(j)
            if (k .lt. ii) then
               lenl = lenl+1
               jw(lenl) = k
               w(lenl) = t
               jw(n+k) = lenl
            else if (k .eq. ii) then
               w(ii) = t
            else
               lenu = lenu+1
               jpos = ii+lenu-1 
               jw(jpos) = k
               w(jpos) = t
               jw(n+k) = jpos
            endif
 170     continue
         jj = 0
         len = 0 
c
c     eliminate previous rows
c
 150     jj = jj+1
         if (jj .gt. lenl) goto 160
c-----------------------------------------------------------------------
c     in order to do the elimination in the correct order we must select
c     the smallest column index among jw(k), k=jj+1, ..., lenl.
c-----------------------------------------------------------------------
         jrow = jw(jj)
         k = jj
c
c     determine smallest column index
c
         do 151 j=jj+1,lenl
            if (jw(j) .lt. jrow) then
               jrow = jw(j)
               k = j
            endif
 151     continue
c
         if (k .ne. jj) then
c     exchange in jw
            j = jw(jj)
            jw(jj) = jw(k)
            jw(k) = j
c     exchange in jr
            jw(n+jrow) = jj
            jw(n+j) = k
c     exchange in w
            s = w(jj)
            w(jj) = w(k)
            w(k) = s
         endif
c
c     zero out element in row by resetting jw(n+jrow) to zero.
c     
         jw(n+jrow) = 0
c
c     drop term if small
c     
         if (abs(w(jj)) .le. droptol*tnorm) then
            dropsum = dropsum + w(jj) 
            goto 150
         endif      
c
c     get the multiplier for row to be eliminated: jrow
c
         fact = w(jj)*alu(jrow)
c
c     combine current row and row jrow
c
         do 203 k = ju(jrow), jlu(jrow+1)-1
            s = fact*alu(k)
c     new column number
            j = iperm(n+jlu(k))
            jpos = jw(n+j)
c
c     if fill-in element is small then disregard:
c     
            if (j .ge. ii) then
c
c     dealing with upper part.
c
               if (jpos .eq. 0) then
c     this is a fill-in element
                  lenu = lenu+1
                  i = ii+lenu-1 
                  if (lenu .gt. n) goto 995
                  jw(i) = j
                  jw(n+j) = i 
                  w(i) = - s
               else
c     no fill-in element --
                  w(jpos) = w(jpos) - s
               endif
            else
c
c     dealing with lower part.
c
               if (jpos .eq. 0) then
c     this is a fill-in element
                 lenl = lenl+1
                 if (lenl .gt. n) goto 995
                 jw(lenl) = j
                 jw(n+j) = lenl
                 w(lenl) = - s
              else
c     no fill-in element --
                 w(jpos) = w(jpos) - s
              endif
           endif
 203	continue
        len = len+1 
        w(len) = fact
        jw(len)  = jrow
	goto 150
 160    continue
c
c     reset double-pointer to zero (U-part)
c     
        do 308 k=1, lenu
           jw(n+jw(ii+k-1)) = 0
 308	continue
c
c     update L-matrix
c
        do 204 k=1, len
           if (ju0 .gt. iwk) goto 996
           alu(ju0) =  w(k)
           jlu(ju0) = iperm(jw(k))
           ju0 = ju0+1
 204    continue
c
c     save pointer to beginning of row ii of U
c
        ju(ii) = ju0
c
c     update u-matrix -- first apply dropping strategy 
c
         len = 0
         do k=1, lenu-1
            if (abs(w(ii+k)) .gt. tnorm*droptol) then 
               len = len+1
               w(ii+len) = w(ii+k) 
               jw(ii+len) = jw(ii+k) 
            else
               dropsum = dropsum + w(ii+k) 
            endif
         enddo
c
        imax = ii
        xmax = abs(w(imax))
        xmax0 = xmax
        icut = ii - 1 + mbloc - mod(ii-1,mbloc)
c
c     determine next pivot -- 
c 
        do k=ii+1,ii+len 
           t = abs(w(k))
           if (t .gt. xmax .and. t*permtol .gt. xmax0 .and.
     *          jw(k) .le. icut) then
              imax = k
              xmax = t
           endif
        enddo
c
c     exchange w's
c
        tmp = w(ii)
        w(ii) = w(imax)
        w(imax) = tmp
c
c     update iperm and reverse iperm
c
        j = jw(imax)
        i = iperm(ii)
        iperm(ii) = iperm(j)
        iperm(j) = i
c     reverse iperm
        iperm(n+iperm(ii)) = ii
        iperm(n+iperm(j)) = j
c----------------------------------------------------------------------- 
        if (len + ju0-1 .gt. iwk) goto 996
c
c     copy U-part in original coordinates
c     
        do 302 k=ii+1,ii+len
           jlu(ju0) = iperm(jw(k))
           alu(ju0) = w(k)
           ju0 = ju0+1
 302	continue
c
c     define diagonal element 
c 
         w(ii) = w(ii) + alph*dropsum 
c
c     store inverse of diagonal element of u
c
        if (w(ii) .eq. 0.0) w(ii) = (1.0D-4 + droptol)*tnorm
c
        alu(ii) = 1.0d0/ w(ii) 
c
c     update pointer to beginning of next row of U.
c
	jlu(ii+1) = ju0
c-----------------------------------------------------------------------
c     end main loop
c-----------------------------------------------------------------------
 500  continue
c
c     permute all column indices of LU ...
c
      do k = jlu(1),jlu(n+1)-1
         jlu(k) = iperm(n+jlu(k))
      enddo
c
c     ...and of A
c
      do k=ia(1), ia(n+1)-1
         ja(k) = iperm(n+ja(k))
      enddo
c
      ierr = 0
      return
c
c     incomprehensible error. Matrix must be wrong.
c
 995  ierr = -1
      return
c
c     insufficient storage in arrays alu, jlu to store factors
c
 996  ierr = -2
      return
c
c     zero row encountered
c
 997  ierr = -3 
      return
c----------------end-of-iludp---------------------------!----------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine iluk(n,a,ja,ia,lfil,alu,jlu,ju,levs,iwk,w,jw,ierr)
      implicit none 
      integer n
      real*8 a(*),alu(*),w(n)
      integer ja(*),ia(n+1),jlu(*),ju(n),levs(*),jw(3*n),lfil,iwk,ierr
c----------------------------------------------------------------------* 
c     SPARSKIT ROUTINE ILUK -- ILU WITH LEVEL OF FILL-IN OF K (ILU(k)) *
c----------------------------------------------------------------------*
c
c on entry:
c========== 
c n       = integer. The row dimension of the matrix A. The matrix 
c
c a,ja,ia = matrix stored in Compressed Sparse Row format.              
c
c lfil    = integer. The fill-in parameter. Each element whose
c           leve-of-fill exceeds lfil during the ILU process is dropped.
c           lfil must be .ge. 0 
c
c tol     = real*8. Sets the threshold for dropping small terms in the
c           factorization. See below for details on dropping strategy.
c  
c iwk     = integer. The minimum length of arrays alu, jlu, and levs.
c
c On return:
c===========
c
c alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
c           the L and U factors together. The diagonal (stored in
c           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
c           contains the i-th row of L (excluding the diagonal entry=1)
c           followed by the i-th row of U.
c
c ju      = integer array of length n containing the pointers to
c           the beginning of each row of U in the matrix alu,jlu.
c
c levs    = integer (work) array of size iwk -- which contains the 
c           levels of each element in alu, jlu.
c
c ierr    = integer. Error message with the following meaning.
c           ierr  = 0    --> successful return.
c           ierr .gt. 0  --> zero pivot encountered at step number ierr.
c           ierr  = -1   --> Error. input matrix may be wrong.
c                            (The elimination process has generated a
c                            row in L or U whose length is .gt.  n.)
c           ierr  = -2   --> The matrix L overflows the array al.
c           ierr  = -3   --> The matrix U overflows the array alu.
c           ierr  = -4   --> Illegal value for lfil.
c           ierr  = -5   --> zero row encountered in A or U.
c
c work arrays:
c=============
c jw      = integer work array of length 3*n.
c w       = real work array of length n 
c
c Notes/known bugs: This is not implemented efficiently storage-wise.
c       For example: Only the part of the array levs(*) associated with
c       the U-matrix is needed in the routine.. So some storage can 
c       be saved if needed. The levels of fills in the LU matrix are
c       output for information only -- they are not needed by LU-solve. 
c        
c----------------------------------------------------------------------
c w, ju (1:n) store the working array [1:ii-1 = L-part, ii:n = u] 
c jw(n+1:2n)  stores the nonzero indicator. 
c 
c Notes:
c ------
c All the diagonal elements of the input matrix must be  nonzero.
c
c----------------------------------------------------------------------* 
c     locals
      integer ju0,k,j1,j2,j,ii,i,lenl,lenu,jj,jrow,jpos,n2,
     *     jlev, min 
      real*8 t, s, fact 
      if (lfil .lt. 0) goto 998
c-----------------------------------------------------------------------
c     initialize ju0 (points to next element to be added to alu,jlu)
c     and pointer array.
c-----------------------------------------------------------------------
      n2 = n+n 
      ju0 = n+2
      jlu(1) = ju0
c
c     initialize nonzero indicator array + levs array -- 
c
      do 1 j=1,2*n 
         jw(j)  = 0
 1    continue
c-----------------------------------------------------------------------
c     beginning of main loop.
c-----------------------------------------------------------------------
      do 500 ii = 1, n
         j1 = ia(ii)
         j2 = ia(ii+1) - 1
c     
c     unpack L-part and U-part of row of A in arrays w 
c     
         lenu = 1
         lenl = 0
         jw(ii) = ii
         w(ii) = 0.0
         jw(n+ii) = ii
c
         do 170  j = j1, j2
            k = ja(j)
            t = a(j)
            if (t .eq. 0.0) goto 170 
            if (k .lt. ii) then
               lenl = lenl+1
               jw(lenl) = k
               w(lenl) = t
               jw(n2+lenl) = 0 
               jw(n+k) = lenl
            else if (k .eq. ii) then
               w(ii) = t
               jw(n2+ii) = 0 
            else
               lenu = lenu+1
               jpos = ii+lenu-1 
               jw(jpos) = k
               w(jpos) = t
               jw(n2+jpos) = 0 
               jw(n+k) = jpos
            endif
 170     continue
c
         jj = 0
c
c     eliminate previous rows
c     
 150     jj = jj+1
         if (jj .gt. lenl) goto 160
c-----------------------------------------------------------------------
c     in order to do the elimination in the correct order we must select
c     the smallest column index among jw(k), k=jj+1, ..., lenl.
c-----------------------------------------------------------------------
         jrow = jw(jj)
         k = jj
c     
c     determine smallest column index
c     
         do 151 j=jj+1,lenl
            if (jw(j) .lt. jrow) then
               jrow = jw(j)
               k = j
            endif
 151     continue
c
         if (k .ne. jj) then
c     exchange in jw
            j = jw(jj)
            jw(jj) = jw(k)
            jw(k) = j
c     exchange in jw(n+  (pointers/ nonzero indicator).
            jw(n+jrow) = jj
            jw(n+j) = k
c     exchange in jw(n2+  (levels) 
            j = jw(n2+jj) 
            jw(n2+jj)  = jw(n2+k) 
            jw(n2+k) = j
c     exchange in w
            s = w(jj)
            w(jj) = w(k)
            w(k) = s
         endif
c
c     zero out element in row by resetting jw(n+jrow) to zero.
c     
         jw(n+jrow) = 0
c     
c     get the multiplier for row to be eliminated (jrow) + its level
c     
         fact = w(jj)*alu(jrow)
         jlev = jw(n2+jj) 
         if (jlev .gt. lfil) goto 150
c
c     combine current row and row jrow
c
         do 203 k = ju(jrow), jlu(jrow+1)-1
            s = fact*alu(k)
            j = jlu(k)
            jpos = jw(n+j)
            if (j .ge. ii) then
c     
c     dealing with upper part.
c     
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c     
                  lenu = lenu+1
                  if (lenu .gt. n) goto 995
                  i = ii+lenu-1
                  jw(i) = j
                  jw(n+j) = i
                  w(i) = - s
                  jw(n2+i) = jlev+levs(k)+1 
               else
c
c     this is not a fill-in element 
c
                  w(jpos) = w(jpos) - s
                  jw(n2+jpos) = min(jw(n2+jpos),jlev+levs(k)+1)
               endif
            else
c     
c     dealing with lower part.
c     
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c
                  lenl = lenl+1
                  if (lenl .gt. n) goto 995
                  jw(lenl) = j
                  jw(n+j) = lenl
                  w(lenl) = - s
                  jw(n2+lenl) = jlev+levs(k)+1 
               else
c
c     this is not a fill-in element 
c
                  w(jpos) = w(jpos) - s
                  jw(n2+jpos) = min(jw(n2+jpos),jlev+levs(k)+1)
               endif
            endif
 203     continue
         w(jj) = fact
         jw(jj)  = jrow
         goto 150 
 160     continue 
c     
c     reset double-pointer to zero (U-part) 
c     
         do 308 k=1, lenu
            jw(n+jw(ii+k-1)) = 0
 308     continue
c
c     update l-matrix
c         
         do 204 k=1, lenl 
            if (ju0 .gt. iwk) goto 996
            if (jw(n2+k) .le. lfil) then
               alu(ju0) =  w(k)
               jlu(ju0) =  jw(k)
               ju0 = ju0+1
            endif
 204     continue
c     
c     save pointer to beginning of row ii of U
c     
         ju(ii) = ju0
c
c     update u-matrix
c
         do 302 k=ii+1,ii+lenu-1 
            if (jw(n2+k) .le. lfil) then
               jlu(ju0) = jw(k)
               alu(ju0) = w(k)
               levs(ju0) = jw(n2+k) 
               ju0 = ju0+1
            endif
 302     continue

         if (w(ii) .eq. 0.0) goto 999 
c     
         alu(ii) = 1.0d0/ w(ii) 
c     
c     update pointer to beginning of next row of U.
c     
         jlu(ii+1) = ju0
c-----------------------------------------------------------------------
c     end main loop
c-----------------------------------------------------------------------
 500  continue
      ierr = 0
      return
c
c     incomprehensible error. Matrix must be wrong.
c     
 995  ierr = -1
      return
c     
c     insufficient storage in L.
c     
 996  ierr = -2
      return
c     
c     insufficient storage in U.
c     
 997  ierr = -3
      return
c     
c     illegal lfil entered.
c     
 998  ierr = -4
      return
c     
c     zero row encountered in A or U. 
c     
 999  ierr = -5
      return
c----------------end-of-iluk--------------------------------------------
c-----------------------------------------------------------------------
      end
c-----end of lctcsr-----------------------------------------------------


c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c                   ITERATIVE SOLVERS MODULE                           c
c----------------------------------------------------------------------c
c This Version Dated: August 13, 1996. Warning: meaning of some        c
c ============ arguments have changed w.r.t. earlier versions. Some    c
c              Calling sequences may also have changed                 c
c----------------------------------------------------------------------c 
c Contents:                                                            c
c-------------------------preconditioners------------------------------c 
c                                                                      c
c ILUT    : Incomplete LU factorization with dual truncation strategy  c
c ILUTP   : ILUT with column  pivoting                                 c
c ILUD    : ILU with single dropping + diagonal compensation (~MILUT)  c
c ILUDP   : ILUD with column pivoting                                  c
c ILUK    : level-k ILU                                                c
c ILU0    : simple ILU(0) preconditioning                              c
c MILU0   : MILU(0) preconditioning                                    c
c                                                                      c
c----------sample-accelerator-and-LU-solvers---------------------------c 
c                                                                      c
c PGMRES  : preconditioned GMRES solver                                c
c LUSOL   : forward followed by backward triangular solve (Precond.)   c
c LUTSOL  : solving v = (LU)^{-T} u (used for preconditioning)         c
c                                                                      c
c-------------------------utility-routine------------------------------c
c                                                                      c 
c QSPLIT  : quick split routine used by ilut to sort out the k largest c
c           elements in absolute value                                 c
c                                                                      c
c----------------------------------------------------------------------c
c                                                                      c 
c Note: all preconditioners are preprocessors to pgmres.               c
c usage: call preconditioner then call pgmres                          c
c                                                                      c
c----------------------------------------------------------------------c
      subroutine ilut(n,a,ja,ia,lfil,droptol,alu,jlu,ju,iwk,w,jw,ierr)
c-----------------------------------------------------------------------
      implicit none 
      integer n 
      real*8 a(*),alu(*),w(n+1),droptol
      integer ja(*),ia(n+1),jlu(*),ju(n),jw(2*n),lfil,iwk,ierr
c----------------------------------------------------------------------*
c                      *** ILUT preconditioner ***                     *
c      incomplete LU factorization with dual truncation mechanism      *
c----------------------------------------------------------------------*
c     Author: Yousef Saad *May, 5, 1990, Latest revision, August 1996  *
c----------------------------------------------------------------------*
c PARAMETERS                                                           
c-----------                                                           
c
c on entry:
c========== 
c n       = integer. The row dimension of the matrix A. The matrix 
c
c a,ja,ia = matrix stored in Compressed Sparse Row format.              
c
c lfil    = integer. The fill-in parameter. Each row of L and each row
c           of U will have a maximum of lfil elements (excluding the 
c           diagonal element). lfil must be .ge. 0.
c           ** WARNING: THE MEANING OF LFIL HAS CHANGED WITH RESPECT TO
c           EARLIER VERSIONS. 
c
c droptol = real*8. Sets the threshold for dropping small terms in the
c           factorization. See below for details on dropping strategy.
c
c  
c iwk     = integer. The lengths of arrays alu and jlu. If the arrays
c           are not big enough to store the ILU factorizations, ilut
c           will stop with an error message. 
c
c On return:
c===========
c
c alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
c           the L and U factors together. The diagonal (stored in
c           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
c           contains the i-th row of L (excluding the diagonal entry=1)
c           followed by the i-th row of U.
c
c ju      = integer array of length n containing the pointers to
c           the beginning of each row of U in the matrix alu,jlu.
c
c ierr    = integer. Error message with the following meaning.
c           ierr  = 0    --> successful return.
c           ierr .gt. 0  --> zero pivot encountered at step number ierr.
c           ierr  = -1   --> Error. input matrix may be wrong.
c                            (The elimination process has generated a
c                            row in L or U whose length is .gt.  n.)
c           ierr  = -2   --> The matrix L overflows the array al.
c           ierr  = -3   --> The matrix U overflows the array alu.
c           ierr  = -4   --> Illegal value for lfil.
c           ierr  = -5   --> zero row encountered.
c
c work arrays:
c=============
c jw      = integer work array of length 2*n.
c w       = real work array of length n+1.
c  
c----------------------------------------------------------------------
c w, ju (1:n) store the working array [1:ii-1 = L-part, ii:n = u] 
c jw(n+1:2n)  stores nonzero indicators
c 
c Notes:
c ------
c The diagonal elements of the input matrix must be  nonzero (at least
c 'structurally'). 
c
c----------------------------------------------------------------------* 
c---- Dual drop strategy works as follows.                             *
c                                                                      *
c     1) Theresholding in L and U as set by droptol. Any element whose *
c        magnitude is less than some tolerance (relative to the abs    *
c        value of diagonal element in u) is dropped.                   *
c                                                                      *
c     2) Keeping only the largest lfil elements in the i-th row of L   * 
c        and the largest lfil elements in the i-th row of U (excluding *
c        diagonal elements).                                           *
c                                                                      *
c Flexibility: one  can use  droptol=0  to get  a strategy  based on   *
c keeping  the largest  elements in  each row  of L  and U.   Taking   *
c droptol .ne.  0 but lfil=n will give  the usual threshold strategy   *
c (however, fill-in is then mpredictible).                             *
c----------------------------------------------------------------------*
c     locals
      integer ju0,k,j1,j2,j,ii,i,lenl,lenu,jj,jrow,jpos,len 
      real*8 tnorm, t, abs, s, fact 
      if (lfil .lt. 0) goto 998
c-----------------------------------------------------------------------
c     initialize ju0 (points to next element to be added to alu,jlu)
c     and pointer array.
c-----------------------------------------------------------------------
      ju0 = n+2
      jlu(1) = ju0
c
c     initialize nonzero indicator array. 
c
      do 1 j=1,n
         jw(n+j)  = 0
 1    continue
c-----------------------------------------------------------------------
c     beginning of main loop.
c-----------------------------------------------------------------------
      do 500 ii = 1, n
         j1 = ia(ii)
         j2 = ia(ii+1) - 1
         tnorm = 0.0d0
         do 501 k=j1,j2
            tnorm = tnorm+abs(a(k))
 501     continue
         if (tnorm .eq. 0.0) goto 999
         tnorm = tnorm/real(j2-j1+1)
c     
c     unpack L-part and U-part of row of A in arrays w 
c     
         lenu = 1
         lenl = 0
         jw(ii) = ii
         w(ii) = 0.0
         jw(n+ii) = ii
c
         do 170  j = j1, j2
            k = ja(j)
            t = a(j)
            if (k .lt. ii) then
               lenl = lenl+1
               jw(lenl) = k
               w(lenl) = t
               jw(n+k) = lenl
            else if (k .eq. ii) then
               w(ii) = t
            else
               lenu = lenu+1
               jpos = ii+lenu-1 
               jw(jpos) = k
               w(jpos) = t
               jw(n+k) = jpos
            endif
 170     continue
         jj = 0
         len = 0 
c     
c     eliminate previous rows
c     
 150     jj = jj+1
         if (jj .gt. lenl) goto 160
c-----------------------------------------------------------------------
c     in order to do the elimination in the correct order we must select
c     the smallest column index among jw(k), k=jj+1, ..., lenl.
c-----------------------------------------------------------------------
         jrow = jw(jj)
         k = jj
c     
c     determine smallest column index
c     
         do 151 j=jj+1,lenl
            if (jw(j) .lt. jrow) then
               jrow = jw(j)
               k = j
            endif
 151     continue
c
         if (k .ne. jj) then
c     exchange in jw
            j = jw(jj)
            jw(jj) = jw(k)
            jw(k) = j
c     exchange in jr
            jw(n+jrow) = jj
            jw(n+j) = k
c     exchange in w
            s = w(jj)
            w(jj) = w(k)
            w(k) = s
         endif
c
c     zero out element in row by setting jw(n+jrow) to zero.
c     
         jw(n+jrow) = 0
c
c     get the multiplier for row to be eliminated (jrow).
c     
         fact = w(jj)*alu(jrow)
         if (abs(fact) .le. droptol) goto 150
c     
c     combine current row and row jrow
c
         do 203 k = ju(jrow), jlu(jrow+1)-1
            s = fact*alu(k)
            j = jlu(k)
            jpos = jw(n+j)
            if (j .ge. ii) then
c     
c     dealing with upper part.
c     
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c     
                  lenu = lenu+1
                  if (lenu .gt. n) goto 995
                  i = ii+lenu-1
                  jw(i) = j
                  jw(n+j) = i
                  w(i) = - s
               else
c
c     this is not a fill-in element 
c
                  w(jpos) = w(jpos) - s

               endif
            else
c     
c     dealing  with lower part.
c     
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c     
                  lenl = lenl+1
                  if (lenl .gt. n) goto 995
                  jw(lenl) = j
                  jw(n+j) = lenl
                  w(lenl) = - s
               else
c     
c     this is not a fill-in element 
c     
                  w(jpos) = w(jpos) - s
               endif
            endif
 203     continue
c     
c     store this pivot element -- (from left to right -- no danger of
c     overlap with the working elements in L (pivots). 
c     
         len = len+1 
         w(len) = fact
         jw(len)  = jrow
         goto 150
 160     continue
c     
c     reset double-pointer to zero (U-part)
c     
         do 308 k=1, lenu
            jw(n+jw(ii+k-1)) = 0
 308     continue
c     
c     update L-matrix
c     
         lenl = len 
         len = min0(lenl,lfil)
c     
c     sort by quick-split
c
         call qsplit (w,jw,lenl,len)
c
c     store L-part
c 
         do 204 k=1, len 
            if (ju0 .gt. iwk) goto 996
            alu(ju0) =  w(k)
            jlu(ju0) =  jw(k)
            ju0 = ju0+1
 204     continue
c     
c     save pointer to beginning of row ii of U
c     
         ju(ii) = ju0
c
c     update U-matrix -- first apply dropping strategy 
c
         len = 0
         do k=1, lenu-1
            if (abs(w(ii+k)) .gt. droptol*tnorm) then 
               len = len+1
               w(ii+len) = w(ii+k) 
               jw(ii+len) = jw(ii+k) 
            endif
         enddo
         lenu = len+1
         len = min0(lenu,lfil)
c
         call qsplit (w(ii+1), jw(ii+1), lenu-1,len)
c
c     copy
c 
         t = abs(w(ii))
         if (len + ju0 .gt. iwk) goto 997
         do 302 k=ii+1,ii+len-1 
            jlu(ju0) = jw(k)
            alu(ju0) = w(k)
            t = t + abs(w(k) )
            ju0 = ju0+1
 302     continue
c     
c     store inverse of diagonal element of u
c     
         if (w(ii) .eq. 0.0) w(ii) = (0.0001 + droptol)*tnorm
c     
         alu(ii) = 1.0d0/ w(ii) 
c     
c     update pointer to beginning of next row of U.
c     
         jlu(ii+1) = ju0
c-----------------------------------------------------------------------
c     end main loop
c-----------------------------------------------------------------------
 500  continue
      ierr = 0
      return
c
c     incomprehensible error. Matrix must be wrong.
c     
 995  ierr = -1
      return
c     
c     insufficient storage in L.
c     
 996  ierr = -2
      return
c     
c     insufficient storage in U.
c     
 997  ierr = -3
      return
c     
c     illegal lfil entered.
c     
 998  ierr = -4
      return
c     
c     zero row encountered
c     
 999  ierr = -5
      return
c----------------end-of-ilut--------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------
      subroutine ilutp(n,a,ja,ia,lfil,droptol,permtol,mbloc,alu,
     *     jlu,ju,iwk,w,jw,iperm,ierr)
c-----------------------------------------------------------------------
c     implicit none
      integer n,ja(*),ia(n+1),lfil,jlu(*),ju(n),jw(2*n),iwk,
     *     iperm(2*n),ierr
      real*8 a(*), alu(*), w(n+1), droptol
c----------------------------------------------------------------------*
c       *** ILUTP preconditioner -- ILUT with pivoting  ***            *
c      incomplete LU factorization with dual truncation mechanism      *
c----------------------------------------------------------------------*
c author Yousef Saad *Sep 8, 1993 -- Latest revision, August 1996.     *
c----------------------------------------------------------------------*
c on entry:
c==========
c n       = integer. The dimension of the matrix A.
c
c a,ja,ia = matrix stored in Compressed Sparse Row format.
c           ON RETURN THE COLUMNS OF A ARE PERMUTED. SEE BELOW FOR 
c           DETAILS. 
c
c lfil    = integer. The fill-in parameter. Each row of L and each row
c           of U will have a maximum of lfil elements (excluding the 
c           diagonal element). lfil must be .ge. 0.
c           ** WARNING: THE MEANING OF LFIL HAS CHANGED WITH RESPECT TO
c           EARLIER VERSIONS. 
c
c droptol = real*8. Sets the threshold for dropping small terms in the
c           factorization. See below for details on dropping strategy.
c
c lfil    = integer. The fill-in parameter. Each row of L and
c           each row of U will have a maximum of lfil elements.
c           WARNING: THE MEANING OF LFIL HAS CHANGED WITH RESPECT TO
c           EARLIER VERSIONS. 
c           lfil must be .ge. 0.
c
c permtol = tolerance ratio used to  determne whether or not to permute
c           two columns.  At step i columns i and j are permuted when 
c
c                     abs(a(i,j))*permtol .gt. abs(a(i,i))
c
c           [0 --> never permute; good values 0.1 to 0.01]
c
c mbloc   = if desired, permuting can be done only within the diagonal
c           blocks of size mbloc. Useful for PDE problems with several
c           degrees of freedom.. If feature not wanted take mbloc=n.
c
c  
c iwk     = integer. The lengths of arrays alu and jlu. If the arrays
c           are not big enough to store the ILU factorizations, ilut
c           will stop with an error message. 
c
c On return:
c===========
c
c alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
c           the L and U factors together. The diagonal (stored in
c           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
c           contains the i-th row of L (excluding the diagonal entry=1)
c           followed by the i-th row of U.
c
c ju      = integer array of length n containing the pointers to
c           the beginning of each row of U in the matrix alu,jlu.
c
c iperm   = contains the permutation arrays. 
c           iperm(1:n) = old numbers of unknowns
c           iperm(n+1:2*n) = reverse permutation = new unknowns.
c
c ierr    = integer. Error message with the following meaning.
c           ierr  = 0    --> successful return.
c           ierr .gt. 0  --> zero pivot encountered at step number ierr.
c           ierr  = -1   --> Error. input matrix may be wrong.
c                            (The elimination process has generated a
c                            row in L or U whose length is .gt.  n.)
c           ierr  = -2   --> The matrix L overflows the array al.
c           ierr  = -3   --> The matrix U overflows the array alu.
c           ierr  = -4   --> Illegal value for lfil.
c           ierr  = -5   --> zero row encountered.
c
c work arrays:
c=============
c jw      = integer work array of length 2*n.
c w       = real work array of length n 
c
c IMPORTANR NOTE:
c --------------
c TO AVOID PERMUTING THE SOLUTION VECTORS ARRAYS FOR EACH LU-SOLVE, 
C THE MATRIX A IS PERMUTED ON RETURN. [all column indices are
c changed]. SIMILARLY FOR THE U MATRIX. 
c To permute the matrix back to its original state use the loop:
c
c      do k=ia(1), ia(n+1)-1
c         ja(k) = iperm(ja(k)) 
c      enddo
c 
c-----------------------------------------------------------------------
c     local variables
c
      integer k,i,j,jrow,ju0,ii,j1,j2,jpos,len,imax,lenu,lenl,jj,mbloc,
     *     icut
      real*8 s, tmp, tnorm,xmax,xmax0, fact, abs, t, permtol
c     
      if (lfil .lt. 0) goto 998
c----------------------------------------------------------------------- 
c     initialize ju0 (points to next element to be added to alu,jlu)
c     and pointer array.
c-----------------------------------------------------------------------
      ju0 = n+2
      jlu(1) = ju0
c
c  integer double pointer array.
c
      do 1 j=1, n
         jw(n+j)  = 0
         iperm(j) = j
         iperm(n+j) = j
 1    continue
c-----------------------------------------------------------------------
c     beginning of main loop.
c-----------------------------------------------------------------------
      do 500 ii = 1, n
         j1 = ia(ii)
         j2 = ia(ii+1) - 1
         tnorm = 0.0d0
         do 501 k=j1,j2
            tnorm = tnorm+abs(a(k))
 501     continue
         if (tnorm .eq. 0.0) goto 999
         tnorm = tnorm/(j2-j1+1)
c
c     unpack L-part and U-part of row of A in arrays  w  --
c
         lenu = 1
         lenl = 0
         jw(ii) = ii
         w(ii) = 0.0
         jw(n+ii) = ii
c
         do 170  j = j1, j2
            k = iperm(n+ja(j))
            t = a(j)
            if (k .lt. ii) then
               lenl = lenl+1
               jw(lenl) = k
               w(lenl) = t
               jw(n+k) = lenl
            else if (k .eq. ii) then
               w(ii) = t
            else
               lenu = lenu+1
               jpos = ii+lenu-1 
               jw(jpos) = k
               w(jpos) = t
               jw(n+k) = jpos
            endif
 170     continue
         jj = 0
         len = 0 
c
c     eliminate previous rows
c
 150     jj = jj+1
         if (jj .gt. lenl) goto 160
c-----------------------------------------------------------------------
c     in order to do the elimination in the correct order we must select
c     the smallest column index among jw(k), k=jj+1, ..., lenl.
c-----------------------------------------------------------------------
         jrow = jw(jj)
         k = jj
c
c     determine smallest column index
c
         do 151 j=jj+1,lenl
            if (jw(j) .lt. jrow) then
               jrow = jw(j)
               k = j
            endif
 151     continue
c
         if (k .ne. jj) then
c     exchange in jw
            j = jw(jj)
            jw(jj) = jw(k)
            jw(k) = j
c     exchange in jr
            jw(n+jrow) = jj
            jw(n+j) = k
c     exchange in w
            s = w(jj)
            w(jj) = w(k)
            w(k) = s
         endif
c
c     zero out element in row by resetting jw(n+jrow) to zero.
c     
         jw(n+jrow) = 0
c
c     get the multiplier for row to be eliminated: jrow
c
         fact = w(jj)*alu(jrow)
c
c     drop term if small
c     
         if (abs(fact) .le. droptol) goto 150
c
c     combine current row and row jrow
c
         do 203 k = ju(jrow), jlu(jrow+1)-1
            s = fact*alu(k)
c     new column number
            j = iperm(n+jlu(k))
            jpos = jw(n+j)
            if (j .ge. ii) then
c
c     dealing with upper part.
c
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c
                  lenu = lenu+1
                  i = ii+lenu-1 
                  if (lenu .gt. n) goto 995
                  jw(i) = j
                  jw(n+j) = i 
                  w(i) = - s
               else
c     no fill-in element --
                  w(jpos) = w(jpos) - s
               endif
            else
c
c     dealing with lower part.
c
               if (jpos .eq. 0) then
c
c     this is a fill-in element
c
                 lenl = lenl+1
                 if (lenl .gt. n) goto 995
                 jw(lenl) = j
                 jw(n+j) = lenl
                 w(lenl) = - s
              else
c
c     this is not a fill-in element
c
                 w(jpos) = w(jpos) - s
              endif
           endif
 203	continue
c     
c     store this pivot element -- (from left to right -- no danger of
c     overlap with the working elements in L (pivots). 
c     
        len = len+1 
        w(len) = fact
        jw(len)  = jrow
	goto 150
 160    continue
c
c     reset double-pointer to zero (U-part)
c     
        do 308 k=1, lenu
           jw(n+jw(ii+k-1)) = 0
 308	continue
c
c     update L-matrix
c
        lenl = len 
        len = min0(lenl,lfil)
c     
c     sort by quick-split
c
        call qsplit (w,jw,lenl,len)
c
c     store L-part -- in original coordinates ..
c
        do 204 k=1, len
           if (ju0 .gt. iwk) goto 996
           alu(ju0) =  w(k)  
           jlu(ju0) = iperm(jw(k))
           ju0 = ju0+1
 204    continue
c
c     save pointer to beginning of row ii of U
c
        ju(ii) = ju0
c
c     update U-matrix -- first apply dropping strategy 
c
         len = 0
         do k=1, lenu-1
            if (abs(w(ii+k)) .gt. droptol*tnorm) then 
               len = len+1
               w(ii+len) = w(ii+k) 
               jw(ii+len) = jw(ii+k) 
            endif
         enddo
         lenu = len+1
         len = min0(lenu,lfil)
         call qsplit (w(ii+1), jw(ii+1), lenu-1,len)
c
c     determine next pivot -- 
c
        imax = ii
        xmax = abs(w(imax))
        xmax0 = xmax
        icut = ii - 1 + mbloc - mod(ii-1,mbloc)
        do k=ii+1,ii+len-1
           t = abs(w(k))
           if (t .gt. xmax .and. t*permtol .gt. xmax0 .and.
     *          jw(k) .le. icut) then
              imax = k
              xmax = t
           endif
        enddo
c
c     exchange w's
c
        tmp = w(ii)
        w(ii) = w(imax)
        w(imax) = tmp
c
c     update iperm and reverse iperm
c
        j = jw(imax)
        i = iperm(ii)
        iperm(ii) = iperm(j)
        iperm(j) = i
c
c     reverse iperm
c
        iperm(n+iperm(ii)) = ii
        iperm(n+iperm(j)) = j
c----------------------------------------------------------------------- 
c
        if (len + ju0 .gt. iwk) goto 997
c
c     copy U-part in original coordinates
c     
        do 302 k=ii+1,ii+len-1 
           jlu(ju0) = iperm(jw(k))
           alu(ju0) = w(k)
           ju0 = ju0+1
 302	continue
c
c     store inverse of diagonal element of u
c
        if (w(ii) .eq. 0.0) w(ii) = (1.0D-4 + droptol)*tnorm
        alu(ii) = 1.0d0/ w(ii) 
c
c     update pointer to beginning of next row of U.
c
	jlu(ii+1) = ju0
c-----------------------------------------------------------------------
c     end main loop
c-----------------------------------------------------------------------
 500  continue
c
c     permute all column indices of LU ...
c
      do k = jlu(1),jlu(n+1)-1
         jlu(k) = iperm(n+jlu(k))
      enddo
c
c     ...and of A
c
      do k=ia(1), ia(n+1)-1
         ja(k) = iperm(n+ja(k))
      enddo
c
      ierr = 0
      return
c
c     incomprehensible error. Matrix must be wrong.
c
 995  ierr = -1
      return
c
c     insufficient storage in L.
c
 996  ierr = -2
      return
c
c     insufficient storage in U.
c
 997  ierr = -3
      return
c
c     illegal lfil entered.
c
 998  ierr = -4
      return
c
c     zero row encountered
c
 999  ierr = -5
      return
c----------------end-of-ilutp-------------------------------------------
c-----------------------------------------------------------------------
      end
c-----end-of-dbcg-------------------------------------------------------
c-----------------------------------------------------------------------
      subroutine implu(np,umm,beta,ypiv,u,permut,full)
      real*8 umm,beta,ypiv(*),u(*),x, xpiv
      logical full, perm, permut(*)
      integer np,k,npm1
c-----------------------------------------------------------------------
c     performs implicitly one step of the lu factorization of a
c     banded hessenberg matrix.
c-----------------------------------------------------------------------
      if (np .le. 1) goto 12
      npm1 = np - 1
c
c     -- perform  previous step of the factorization-
c
      do 6 k=1,npm1
         if (.not. permut(k)) goto 5
         x=u(k)
         u(k) = u(k+1)
         u(k+1) = x
 5       u(k+1) = u(k+1) - ypiv(k)*u(k)
 6    continue
c-----------------------------------------------------------------------
c     now determine pivotal information to be used in the next call
c-----------------------------------------------------------------------
 12   umm = u(np)
      perm = (beta .gt. abs(umm))
      if (.not. perm) goto 4
      xpiv = umm / beta
      u(np) = beta
      goto 8
 4    xpiv = beta/umm
 8    permut(np) = perm
      ypiv(np) = xpiv
      if (.not. full) return
c     shift everything up if full...
      do 7 k=1,npm1
         ypiv(k) = ypiv(k+1)
         permut(k) = permut(k+1)
 7    continue
      return
c-----end-of-implu
      end
c-----------------------------------------------------------------------

      subroutine indset0 (n,ja,ia,nset,iord,riord,sym,iptr) 
      integer n, nset, ja(*),ia(*),riord(*),iord(*) 
      logical sym
c---------------------------------------------------------------------- 
c greedy algorithm for independent set ordering
c----------------------------------------------------------------------
c parameters:
c ----------
c n      = row dimension of matrix
c ja, ia = matrix pattern in CRS format
c nset   = (output) number of elements in the independent set
c iord   = permutation array corresponding to the independent set 
c          ordering. Row number i will become row number iord(i) in 
c          permuted matrix.
c riord  = reverse permutation array. Row number i in the permutated 
c          matrix is row number riord(i) in original matrix. 
c----------------------------------------------------------------------
c notes: works for CSR, MSR, and CSC formats but assumes that the
c matrix has a symmetric structure. 
c---------------------------------------------------------------------- 
c local variables
c 
      integer j, k1, k2, nod, k, mat
      do 1 j=1, n
         iord(j) = 0
 1    continue
      nummat = 1
      if (.not. sym) nummat = 2
c     
c     iord used as a marker
c     
      nset = 0
      do 12  nod=1, n
         if (iord(nod) .ne. 0) goto 12 
         nset = nset+1
         iord(nod) = 1
c     
c     visit all neighbors of current nod 
c     
         ipos = 0
         do 45 mat=1, nummat
            do 4 k=ia(ipos+nod), ia(ipos+nod+1)-1 
               j = ja(k)
               if (j .ne. nod) iord(j) = 2
 4          continue
            ipos = iptr-1
 45      continue
 12   continue
c     
c     get permutation
c     
      k1 = 0 
      k2 = nset 
      do 6 j=1,n
         if (iord(j) .eq. 1) then
            k1 = k1+1
            k = k1
         else
            k2 = k2+1
            k = k2 
         endif
         riord(k) = j         
         iord(j) = k
 6    continue
      return
c---------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------
      subroutine indset1 (n,ja,ia,nset,iord,riord,iw,sym,iptr) 
      integer n, nset, iptr, ja(*),ia(*),riord(*),iord(*),iw(*)
      logical sym
c---------------------------------------------------------------------- 
c greedy algorithm for independent set ordering -- with intial 
c order of traversal given by that of min degree. 
c----------------------------------------------------------------------
c parameters:
c ----------
c n      = row dimension of matrix
c ja, ia = matrix pattern in CRS format
c nset   = (output) number of elements in the independent set
c iord   = permutation array corresponding to the independent set 
c          ordering. Row number i will become row number iord(i) in 
c          permuted matrix.
c riord  = reverse permutation array. Row number i in the permutated 
c          matrix is row number riord(i) in original matrix. 
c----------------------------------------------------------------------
c notes: works for CSR, MSR, and CSC formats but assumes that the
c matrix has a symmetric structure. 
c---------------------------------------------------------------------- 
c local variables
      integer j,k1,k2,nummat,nod,k,ipos 
c
c     nummat is the number of matrices to loop through (A in symmetric
c     pattern case (nummat=1) or A,and transp(A) otherwise (mummat=2) 
c
      if (sym) then
         nummat = 1
      else
         nummat = 2
      endif
      iptrm1 = iptr-1
c     
c     initialize arrays
c
      do 1 j=1,n
         iord(j) = j 
         riord(j) = j 
         iw(j) = 0 
 1    continue
c     
c     initialize degrees of all nodes
c     
      ipos = 0
      do 100 imat =1,nummat
         do 15 j=1,n
            iw(j) = iw(j) + ia(ipos+j+1)-ia(ipos+j) 
 15      continue
         ipos = iptrm1
 100  continue 
c     
c     call heapsort -- sorts nodes in increasing degree. 
c     
      call HeapSort (iw,iord,riord,n,n) 
c     
c     weights no longer needed -- use iw to store order of traversal.
c     
      do 16 j=1, n
         iw(n-j+1) = iord(j)
         iord(j) = 0
 16   continue
c     
c     iord used as a marker
c     
      nset = 0
      do 12  ii = 1, n
         nod = iw(ii) 
         if (iord(nod) .ne. 0) goto 12 
         nset = nset+1
         iord(nod) = 1
c     
c     visit all neighbors of current nod 
c     
         ipos = 0
         do 45 mat=1, nummat
            do 4 k=ia(ipos+nod), ia(ipos+nod+1)-1 
               j = ja(k)
               if (j .ne. nod) iord(j) = 2
 4          continue
            ipos = iptrm1 
 45      continue
 12   continue
c     
c     get permutation
c     
      k1 = 0 
      k2 = nset 
      do 6 j=1,n
         if (iord(j) .eq. 1) then
            k1 = k1+1
            k = k1
         else
            k2 = k2+1
            k = k2 
         endif
         riord(k) = j         
         iord(j) = k
 6    continue
      return
c----------------------------------------------------------------------
      end
c----------------------------------------------------------------------
      subroutine indset2(n,ja,ia,nset,iord,riord,iw,sym,iptr) 
      integer n,nset,iptr,ja(*),ia(*),riord(n),iord(n),iw(n) 
      logical sym 
c---------------------------------------------------------------------- 
c greedy algorithm for independent set ordering -- local minimization
c using heap strategy -- 
c----------------------------------------------------------------------
c This version for BOTH unsymmetric and symmetric patterns
c----------------------------------------------------------------------
c on entry
c -------- 
c n     = row and column dimension of matrix
c ja    = column indices of nonzero elements of matrix,stored rowwise.
c ia    = pointer to beginning of each row in ja.
c sym   = logical indicating whether the matrix has a symmetric pattern.
c         If not the transpose must also be provided -- appended to the
c         ja, ia structure -- see description of iptr next.
c iptr  = in case the matrix has an unsymmetric pattern,the transpose
c         is assumed to be stored in the same arrays ia,ja. iptr is the
c         location in ia of the pointer to the first row of transp(A).
c         more generally, ia(iptr),...,ia(iptr+n) are the pointers to 
c         the beginnings of rows 1, 2, ...., n+1 (row n+1 is fictitious)
c         of the transpose of A in the array ja. For example,when using 
c         the msr format,one can write:
c          iptr = ja(n+1)
c          ipos = iptr+n+2                ! get the transpose of A:
c          call csrcsc (n,0,ipos,a,ja,ja,a,ja,ja(iptr))    ! and then:
c          call indset(n,ja,ja,nset,iord,riord,iwk,.false.,iptr) 
c
c iw    = work space of length n.
c
c on return: 
c---------- 
c nset  = integer. The number of unknowns in the independent set. 
c iord  = permutation array corresponding to the new ordering. The 
c         first nset unknowns correspond to the independent set.
c riord = reverse permutation array.  
c----------------------------------------------------------------------
c local variables --
c 
      integer j,k1,k2,nummat,nod,k,ipos,i,last,lastlast,jold,jnew,
     *     jo,jn 
c
c     nummat is the number of matrices to loop through (A in symmetric
c     pattern case (nummat=1) or A,and transp(A) otherwise (mummat=2) 
c
      if (sym) then
         nummat = 1
      else
         nummat = 2
      endif
      iptrm1 = iptr-1
c
c     initialize arrays
c
      do 1 j=1,n
         iord(j) = j
         riord(j) = j
         iw(j) = 0 
 1    continue
c
c     initialize degrees of all nodes
c
      ipos = 0
      do 100 imat =1,nummat
         do 15 j=1,n
            iw(j) = iw(j) + ia(ipos+j+1)-ia(ipos+j) 
 15      continue
 100     ipos = iptrm1
c     
c start by constructing a heap
c 
      do 2 i=n/2,1,-1 
         j = i
         call FixHeap (iw,iord,riord,j,j,n) 
 2    continue
c     
c main loop -- remove nodes one by one. 
c 
      last = n
      nset = 0
 3    continue
      lastlast = last
      nod = iord(1) 
c     
c     move first element to end
c 
      call moveback (iw,iord,riord,last) 
      last = last -1 
      nset = nset + 1 
c
c     scan all neighbors of accepted node -- move them to back -- 
c     
      ipos = 0
      do 101 imat =1,nummat      
         do 5 k=ia(ipos+nod),ia(ipos+nod+1)-1
            jold = ja(k)
            jnew = riord(jold)
            if (jold .eq. nod .or. jnew .gt. last) goto 5 
            iw(jnew) = -1
            call HeapInsert (iw,iord,riord,jnew,ichild,jnew) 
            call moveback (iw,iord,riord,last) 
            last = last -1 
 5       continue 
         ipos = iptrm1
 101  continue
c
c update the degree of each edge
c 
         do 6 k=last+1,lastlast-1
            jold = iord(k) 
c     
c     scan the neighbors of current node
c     
         ipos = 0
         do 102 imat =1,nummat
            do 61 i=ia(ipos+jold),ia(ipos+jold+1)-1 
               jo = ja(i) 
               jn = riord(jo) 
c
c     consider this node only if it has not been moved          
c     
               if (jn .gt. last) goto 61
c     update degree of this neighbor 
               iw(jn) = iw(jn)-1
c     and fix the heap accordingly
               call HeapInsert (iw,iord,riord,jn,ichild,jn)
 61         continue
            ipos = iptrm1
 102     continue
 6    continue
c
c     stopping test -- end main "while"loop 
c
      if (last .gt. 1) goto 3
      nset = nset + last
c
c     rescan all nodes one more time to determine the permutations 
c
      k1 = 0 
      k2 = nset
      do 7 j=n,1,-1
         if (iw(j) .ge. 0) then
            k1 = k1+1
            k = k1
         else
            k2 = k2+1
            k = k2 
         endif
         riord(k) = iord(j) 
 7    continue
      do j=1,n
         iord(riord(j)) = j
      enddo
      return
c----------------------------------------------------------------------
      end
c----------------------------------------------------------------------
      subroutine indset3(n,ja,ia,nset,iord,riord,iw,sym,iptr) 
      integer n,nset,iptr,ja(*),ia(*),riord(n),iord(n),iw(n) 
      logical sym 
c---------------------------------------------------------------------- 
c greedy algorithm for independent set ordering -- local minimization
c using heap strategy -- VERTEX COVER ALGORITHM --  
c ASSUMES MSR FORMAT (no diagonal element) -- ADD A SWITCH FOR CSR -- 
c----------------------------------------------------------------------
c This version for BOTH unsymmetric and symmetric patterns
c----------------------------------------------------------------------
c on entry
c -------- 
c n     = row and column dimension of matrix
c ja    = column indices of nonzero elements of matrix,stored rowwise.
c ia    = pointer to beginning of each row in ja.
c sym   = logical indicating whether the matrix has a symmetric pattern.
c         If not the transpose must also be provided -- appended to the
c         ja, ia structure -- see description of iptr next.
c iptr  = in case the matrix has an unsymmetric pattern,the transpose
c         is assumed to be stored in the same arrays ia,ja. iptr is the
c         location in ia of the pointer to the first row of transp(A).
c         more generally, ia(iptr),...,ia(iptr+n) are the pointers to 
c         the beginnings of rows 1, 2, ...., n+1 (row n+1 is fictitious)
c         of the transpose of A in the array ja. For example,when using 
c         the msr format,one can write:
c          iptr = ja(n+1)
c          ipos = iptr+n+2                ! get the transpose of A:
c          call csrcsc (n,0,ipos,a,ja,ja,a,ja,ja(iptr))    ! and then:
c          call indset(n,ja,ja,nset,iord,riord,iwk,.false.,iptr) 
c
c iw    = work space of length n.
c
c on return: 
c---------- 
c nset  = integer. The number of unknowns in the independent set. 
c iord  = permutation array corresponding to the new ordering. The 
c         first nset unknowns correspond to the independent set.
c riord = reverse permutation array.  
c----------------------------------------------------------------------
c local variables --
c 
      integer j,nummat,nod,k,ipos,i,lastnset,jold,jnew
c
c     nummat is the number of matrices to loop through (A in symmetric
c     pattern case (nummat=1) or A,and transp(A) otherwise (mummat=2) 
c
      if (sym) then
         nummat = 1
      else
         nummat = 2
      endif
      iptrm1 = iptr-1
c
c     initialize arrays
c
      do 1 j=1,n
         riord(j) = j
         iord(j) = j
         iw(j) = 0 
 1    continue
c
c     initialize degrees of all nodes
c
      nnz = 0
      ipos = 0
      do 100 imat =1,nummat
         do 15 j=1,n
            ideg = ia(ipos+j+1)-ia(ipos+j) 
            iw(j) = iw(j) + ideg 
            nnz = nnz + ideg
 15      continue
 100     ipos = iptrm1
c
c     number of edges
c     
         if (sym) then nnz = 2*nnz
c     
c start by constructing a Max heap
c 
      do 2 i=n/2,1,-1 
         j = i
         call FixHeapM (iw,riord,iord,j,j,n) 
 2    continue
      nset = n
c----------------------------------------------------------------------     
c main loop -- remove nodes one by one. 
c----------------------------------------------------------------------
 3    continue
      lastnset = nset
      nod = riord(1) 
c     
c     move first element to end
c 
      call movebackM (iw,riord,iord,nset) 
      nnz = nnz - iw(nset) 
      nset = nset -1 
c
c     scan all neighbors of accepted node -- 
c     
      ipos = 0
      do 101 imat =1,nummat      
         do 5 k=ia(ipos+nod),ia(ipos+nod+1)-1
            jold = ja(k)
            jnew = iord(jold)
            if (jold .eq. nod .or. jnew .gt. nset) goto 5 
            iw(jnew) = iw(jnew) - 1
            nnz = nnz-1 
            call FixHeapM (iw,riord,iord,jnew,jnew,nset) 
 5       continue 
         ipos = iptrm1
 101  continue
c      
      if (nnz .gt. 0) goto 3
      return
c----------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------
      subroutine indsetr (n,ja,ia,nset,iord,riord,sym,iptr) 
      integer n, nset, ja(*),ia(*),riord(*),iord(*) 
      logical sym
c---------------------------------------------------------------------- 
c greedy algorithm for independent set ordering -- RANDOM TRAVERSAL -- 
c----------------------------------------------------------------------
c parameters:
c ----------
c n      = row dimension of matrix
c ja, ia = matrix pattern in CRS format
c nset   = (output) number of elements in the independent set
c iord   = permutation array corresponding to the independent set 
c          ordering. Row number i will become row number iord(i) in 
c          permuted matrix.
c riord  = reverse permutation array. Row number i in the permutated 
c          matrix is row number riord(i) in original matrix. 
c----------------------------------------------------------------------
c notes: works for CSR, MSR, and CSC formats but assumes that the
c matrix has a symmetric structure. 
c---------------------------------------------------------------------- 
c local variables
c 
      integer j, k1, k2, nod, k, mat
      do 1 j=1, n
         iord(j) = 0
 1    continue
c
c generate random permutation
c 
      iseed = 0 
      call rndperm(n, riord, iseed)
      write (8,'(10i6)') (riord(j),j=1,n) 
c
      nummat = 1
      if (.not. sym) nummat = 2
c     
c iord used as a marker
c 
      nset = 0
      do 12  ii=1, n
         nod = riord(ii) 
         if (iord(nod) .ne. 0) goto 12 
         nset = nset+1
         iord(nod) = 1
c
c visit all neighbors of current nod 
c     
         ipos = 0
         do 45 mat=1, nummat
            do 4 k=ia(ipos+nod), ia(ipos+nod+1)-1 
               j = ja(k)
               if (j .ne. nod) iord(j) = 2
 4          continue
            ipos = iptr-1
 45      continue
 12   continue
c
c get permutation
c     
      k1 = 0 
      k2 = nset 
      do 6 j=1,n
         if (iord(j) .eq. 1) then
            k1 = k1+1
            k = k1
         else
            k2 = k2+1
            k = k2 
         endif
            riord(k) = j         
            iord(j) = k
 6    continue
      return
c----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine infdia (n,ja,ia,ind,idiag) 
      integer ia(*), ind(*), ja(*)
c-----------------------------------------------------------------------
c     obtains information on the diagonals of A. 
c----------------------------------------------------------------------- 
c this subroutine finds the lengths of each of the 2*n-1 diagonals of A
c it also outputs the number of nonzero diagonals found. 
c----------------------------------------------------------------------- 
c on entry:
c---------- 
c n	= dimension of the matrix a.
c
c a,    ..... not needed here.
c ja, 			
c ia    = matrix stored in csr format
c
c on return:
c----------- 
c
c idiag = integer. number of nonzero diagonals found. 
c 
c ind   = integer array of length at least 2*n-1. The k-th entry in
c         ind contains the number of nonzero elements in the diagonal
c         number k, the numbering beeing from the lowermost diagonal
c         (bottom-left). In other words ind(k) = length of diagonal
c         whose offset wrt the main diagonal is = - n + k.
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
      n2= n+n-1
      do 1 i=1,n2
         ind(i) = 0
 1    continue
      do 3 i=1, n
         do 2 k=ia(i),ia(i+1)-1
            j = ja(k)
            ind(n+j-i) = ind(n+j-i) +1
 2       continue 
 3    continue
c     count the nonzero ones.
      idiag = 0 
      do 41 k=1, n2
         if (ind(k) .ne. 0) idiag = idiag+1
 41   continue
      return
c done
c------end-of-infdia ---------------------------------------------------
c-----------------------------------------------------------------------
      end
      subroutine inmesh (nmesh,iin,nx,nelx,node,x,y,nodcode,ijk,iperm)
      implicit none 
      real*8 x(*),y(*)
      integer nmesh,iin,nx,nelx,node,nodcode(nx),ijk(node,nelx), 
     *     iperm(nx) 
c-----------------------------------------------------------------------
c this subroutine selects and initializes a mesh among a few
c choices. So far there are 9 initial meshes provided and the user can
c also enter his own mesh as a 10th option. 
c 
c on entry:
c--------- 
c nmesh	    = integer indicating the mesh chosen. nmesh=1,...,9
c             corresponds to one of the 9 examples supplied by
c             SPARSKIT. nmesh = 0 is a user supplied initial mesh.
c             see below for additional information for the format.
c iin       = integer containing the I/O unit number where to read
c             the data from in case nmesh = 1. A dummy integer 
c             otherwise.
c node      = integer = the number of nodes per element (should be
c             set to node=3 in this version). node is also the first 
c             dimension of the array ijk.
c
c on return
c ---------
c nx	    = integer . the number of nodes
c nelx	    = integer . the number of elements
c x, y      = two real arrays containing the coordinates of the nodes.
c nodcode   = an integer array containing the boundary information for
c             each node with the following meaning.
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner node. 
c                           
c ijk(node,*)= an integer array containing the connectivity matrix.
c
c-----------------------------------------------------------------------
c format for user supplied mesh (when nmesh = 7) 
c
c option nmesh = 0, is a user definied initial mesh. 
c--------- 
c format is as follows:
c line 1: two integers, the first containing the number of nodes 
c         the second the number of elements.
c line 2: to line nx+1:  node information.
c        enter the following, one line per node:
c     * the number of the node in the numbering chosen (integer 
c          taking the values 1 to nx), followed by,
c        * the coordinates of the nodes (2 reals)  followed by 
c	   the boundary information, an integer taking one of the 
c          values 0, 1, or 2,  with the meaning explained above.
c
c line nx+2 to nx+nelx+1: connectivity matrix
c       enter the following one line per element: 
c       * number of the element in the numbering chosen, followed by
c       * The three numbers of the nodes (according to the above numbering
c       of the nodes) that constitute the element, in a counter clock-wise
c       order (this is in fact not important since it is checked by the
c       subroutine chkelemt). 
c
c AN EXAMPLE: consisting on one single element (a triangle) 
c------------ 
c    3    1
c    1    0.0000    0.0000    2    
c    2    4.0000    0.0000    2     
c    3    0.0000    4.0000    2                  
c    1    1    2    3
c
c----------------------------------------------------------------------- 
c     local variables      
      integer i, j, ii
c
c     print *, ' ----- nmesh = ', nmesh 
      goto (10,1,2,3,4,5,6,7,8,9) nmesh+1 
 1    continue
      call fmesh1  (nx,nelx,node,x,y,nodcode,ijk)
      goto 18
 2    continue
      call fmesh2  (nx,nelx,node,x,y,nodcode,ijk)
      goto 18
 3    continue
      call fmesh3  (nx,nelx,node,x,y,nodcode,ijk)
      goto 18
 4    continue
      call fmesh4  (nx,nelx,node,x,y,nodcode,ijk)
      goto 18
 5    continue
      call fmesh5  (nx,nelx,node,x,y,nodcode,ijk)
      goto 18
 6    continue
      call fmesh6 (nx,nelx,node,x,y,nodcode,ijk)
      goto 18
 7    continue
      call fmesh7 (nx,nelx,node,x,y,nodcode,ijk,iperm) 
      goto 18
 8    continue
      call fmesh8 (nx,nelx,node,x,y,nodcode,ijk,iperm) 
      goto 18
 9    continue
      call fmesh9(nx,nelx,node,x,y,nodcode,ijk,iperm) 
      goto 18
 10   continue
c
c-------option 0 : reading mesh from IO unit iin.
c
      read (iin,*) nx, nelx
c     
      do 16 i=1,nx
         read(iin,*) ii,x(ii),y(ii),nodcode(ii)
 16   continue
      do 17 i=1,nelx
         read(iin,*) ii,(ijk(j,ii),j=1,node)
         if (ii. gt. nelx) nelx = ii
 17   continue
c-----------------------------------------------------------------------
 18    continue
c     and return
      return
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine interchange (a,ind,rind,i,j)
      integer a(*),ind(*),rind(*),i,j 
      integer tmp,itmp
      tmp = a(i)  
      itmp = ind(i) 
c
      a(i) = a(j) 
      ind(i) = ind(j) 
c
      a(j) = tmp
      ind(j) = itmp 
      rind(ind(j)) = j
      rind(ind(i)) = i
c
      return
      end 
C **********************************************************************
      SUBROUTINE INVLPW(n, lpw, kpw)
c.......................................................................
c 
c     KPW is the inverse of LPW
c
c.......................................................................
      dimension lpw(n), kpw(n)
c.......................................................................
c     Laura C. Dutto - Novembre 1993
c.......................................................................
C$DOACROSS if(n .gt. 200), local(i0, i1)
      do i0 = 1, n
         i1 = lpw(i0)
         kpw(i1) = i0
      enddo
c
      return
      end
C **********************************************************************
      SUBROUTINE IPLUSA (n, nalpha, nbeta, ia)
c.......................................................................
C
c     We add NALPHA to each element of NBETA * IA:
c
c            ia(i) = nalpha + nbeta * ia(i)
c
c.......................................................................
      integer ia(n)
c.......................................................................
c     Laura C. Dutto - February 1994
c.......................................................................
      if(n .le. 0) return
c
      nmax = 500
      if(nalpha .eq. 0) then
           if(nbeta .eq. 1) return
           if(nbeta .eq. -1) then
C$DOACROSS if(n .gt. nmax), local (i)
                do i = 1, n
                   ia(i) = - ia(i)
                enddo
           else
C$DOACROSS if(n .gt. nmax/2), local (i)
                do i = 1, n
                   ia(i) = nbeta * ia(i)
                enddo
           endif
           return
      endif
      if(nbeta .eq. 0) then
C$DOACROSS if(n .gt. nmax), local (i)
           do i = 1, n
              ia(i) = nalpha
           enddo
           return
      endif
      if(nbeta .eq. -1) then
C$DOACROSS if(n .gt. nmax/2), local (i)
           do i = 1, n
              ia(i) = nalpha - ia(i)
           enddo
      else if(nbeta .eq. 1) then
C$DOACROSS if(n .gt. nmax/2), local (i)
           do i = 1, n
              ia(i) = nalpha + ia(i)
           enddo
      else
C$DOACROSS if(n .gt. nmax/3), local (i)
           do i = 1, n
              ia(i) = nalpha + nbeta * ia(i)
           enddo
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine ivperm (n, ix, perm) 
      integer n, perm(n), ix(n)
c-----------------------------------------------------------------------
c this subroutine performs an in-place permutation of an integer vector 
c ix according to the permutation array perm(*), i.e., on return, 
c the vector x satisfies,
c
c	ix(perm(j)) :== ix(j), j=1,2,.., n
c
c-----------------------------------------------------------------------
c on entry:
c---------
c n 	= length of vector x.
c perm 	= integer array of length n containing the permutation  array.
c ix	= input vector
c
c on return:
c---------- 
c ix	= vector x permuted according to ix(perm(*)) :=  ix(*)
c
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
c local variables
      integer tmp, tmp1
c
      init      = 1
      tmp	= ix(init)	
      ii        = perm(init)
      perm(init)= -perm(init)
      k         = 0
c     
c loop
c 
 6    k = k+1
c
c save the chased element --
c 
      tmp1	  = ix(ii) 
      ix(ii)     = tmp
      next	  = perm(ii) 
      if (next .lt. 0 ) goto 65
c     
c test for end 
c
      if (k .gt. n) goto 101
      tmp       = tmp1
      perm(ii)  = - perm(ii)
      ii        = next 
c
c end loop 
c
      goto 6
c
c reinitilaize cycle --
c
 65   init      = init+1
      if (init .gt. n) goto 101
      if (perm(init) .lt. 0) goto 65
      tmp	= ix(init)
      ii	= perm(init)
      perm(init)=-perm(init)
      goto 6
c     
 101  continue
      do 200 j=1, n
         perm(j) = -perm(j)
 200  continue 
c     
      return
c-------------------end-of-ivperm--------------------------------------- 
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine jadcsr (nrow, idiag, a, ja, ia, iperm, ao, jao, iao) 
      integer ja(*), jao(*), ia(idiag+1), iperm(nrow), iao(nrow+1) 
      real*8 a(*), ao(*)
c-----------------------------------------------------------------------
c     Jagged Diagonal Storage   to     Compressed Sparse Row  
c----------------------------------------------------------------------- 
c this subroutine converts a matrix stored in the jagged diagonal format
c to the compressed sparse row format.
c----------------------------------------------------------------------- 
c on entry:
c---------- 
c nrow 	  = integer. the row dimension of the matrix A.
c 
c idiag   = integer. The  number of jagged diagonals in the data
c           structure a, ja, ia.
c 
c a, 
c ja,
c ia      = input matrix in jagged diagonal format. 
c
c iperm   = permutation of the rows used to obtain the JAD ordering. 
c        
c on return: 
c----------
c 
c ao, jao,
c iao     = matrix in CSR format.
c-----------------------------------------------------------------------
c determine first the pointers for output matrix. Go through the
c structure once:
c
      do 137 j=1,nrow
         jao(j) = 0
 137  continue
c     
c     compute the lengths of each row of output matrix - 
c     
      do 140 i=1, idiag
         len = ia(i+1)-ia(i) 
         do 138 k=1,len
            jao(iperm(k)) = jao(iperm(k))+1
 138     continue
 140  continue
c     
c     remember to permute
c     
      kpos = 1
      iao(1) = 1
      do 141 i=1, nrow 
         kpos = kpos+jao(i) 
         iao(i+1) = kpos
 141  continue
c     
c     copy elemnts one at a time.
c     
      do 200 jj = 1, idiag
         k1 = ia(jj)-1
         len = ia(jj+1)-k1-1 
         do 160 k=1,len
            kpos = iao(iperm(k))
            ao(kpos) = a(k1+k) 
            jao(kpos) = ja(k1+k) 
            iao(iperm(k)) = kpos+1
 160     continue
 200  continue
c     
c     rewind pointers
c     
      do 5 j=nrow,1,-1
         iao(j+1) = iao(j)
 5    continue
      iao(1) = 1
      return
c----------end-of-jadcsr------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
c------------------------end-of-csrkvstc--------------------------------
c-----------------------------------------------------------------------
      subroutine kvstmerge(nr, kvstr, nc, kvstc, n, kvst)
c-----------------------------------------------------------------------
      integer nr, kvstr(nr+1), nc, kvstc(nc+1), n, kvst(*)
c-----------------------------------------------------------------------
c     Merges block partitionings, for conformal row/col pattern.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     nr,nc   = matrix block row and block column dimension
c     kvstr   = first row number for each block row
c     kvstc   = first column number for each block column
c
c     On return:
c---------------
c     n       = conformal row/col matrix block dimension
c     kvst    = conformal row/col block partitioning
c
c     Notes:
c-----------
c     If matrix is not square, this routine returns without warning.
c
c-----------------------------------------------------------------------
c-----local variables
      integer i,j
c---------------------------------
      if (kvstr(nr+1) .ne. kvstc(nc+1)) return
      i = 1
      j = 1
      n = 1
  200 if (i .gt. nr+1) then
         kvst(n) = kvstc(j)
         j = j + 1
      elseif (j .gt. nc+1) then
         kvst(n) = kvstr(i)
         i = i + 1
      elseif (kvstc(j) .eq. kvstr(i)) then
         kvst(n) = kvstc(j)
         j = j + 1
         i = i + 1
      elseif (kvstc(j) .lt. kvstr(i)) then
         kvst(n) = kvstc(j)
         j = j + 1
      else
         kvst(n) = kvstr(i)
         i = i + 1
      endif
      n = n + 1
      if (i.le.nr+1 .or. j.le.nc+1) goto 200
      n = n - 2
c---------------------------------
      return
c------------------------end-of-kvstmerge-------------------------------
      end
c-----------------------------------------------------------------------
      function lctcsr(i,j,ja,ia)
      integer lctcsr, i, j, ja(*), ia(*), k
c-----------------------------------------------------------------------
c     locate the position of a matrix element in a CSR format
c     returns -1 if the desired element is zero
c-----------------------------------------------------------------------
      lctcsr = -1
      k = ia(i)
 10   if (k .lt. ia(i+1) .and. (lctcsr .eq. -1)) then
         if (ja(k) .eq. j) lctcsr = k
         k = k + 1
         goto 10
      end if
c
      return
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine ldsolc (n,x,y,al,jal) 
      integer n, jal(*)
      real*8 x(n), y(n), al(*)
c-----------------------------------------------------------------------
c    Solves     L x = y ;    L = nonunit Low. Triang. MSC format 
c----------------------------------------------------------------------- 
c solves a (non-unit) lower triangular system by standard (sequential) 
c forward elimination - matrix stored in Modified Sparse Column format 
c with diagonal elements already inverted (otherwise do inversion,
c al(1:n) = 1.0/al(1:n),  before calling ldsol).
c-----------------------------------------------------------------------
c
c On entry:
c---------- 
c n      = integer. dimension of problem.
c y      = real array containg the right hand side.
c
c al,
c jal,
c ial,    = Lower triangular matrix stored in Modified Sparse Column
c           format.
c
c On return:
c----------- 
c	x = The solution of  L x = y .
c--------------------------------------------------------------------
c local variables
c
      integer k, j
      real*8 t 
c-----------------------------------------------------------------------
      do 140 k=1,n
         x(k) = y(k) 
 140  continue
      do 150 k = 1, n 
         x(k) = x(k)*al(k) 
         t = x(k) 
         do 100 j = jal(k), jal(k+1)-1
            x(jal(j)) = x(jal(j)) - t*al(j) 
 100     continue
 150  continue
c
      return
c----------end-of-lsolc------------------------------------------------ 
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine ldsol (n,x,y,al,jal) 
      integer n, jal(*) 
      real*8 x(n), y(n), al(*) 
c----------------------------------------------------------------------- 
c     Solves L x = y    L = triangular. MSR format 
c-----------------------------------------------------------------------
c solves a (non-unit) lower triangular system by standard (sequential) 
c forward elimination - matrix stored in MSR format 
c with diagonal elements already inverted (otherwise do inversion,
c al(1:n) = 1.0/al(1:n),  before calling ldsol).
c-----------------------------------------------------------------------
c
c On entry:
c---------- 
c n      = integer. dimension of problem.
c y      = real array containg the right hand side.
c
c al,
c jal,   = Lower triangular matrix stored in Modified Sparse Row 
c          format. 
c
c On return:
c----------- 
c	x = The solution of  L x = y .
c--------------------------------------------------------------------
c local variables 
c
      integer k, j 
      real*8 t 
c-----------------------------------------------------------------------
      x(1) = y(1)*al(1) 
      do 150 k = 2, n
         t = y(k) 
         do 100 j = jal(k), jal(k+1)-1
            t = t - al(j)*x(jal(j))
 100     continue
         x(k) = al(k)*t 
 150  continue
      return
c----------end-of-ldsol-------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------  
      subroutine ldsoll (n,x,y,al,jal,nlev,lev,ilev) 
      integer n, nlev, jal(*), ilev(nlev+1), lev(n)
      real*8 x(n), y(n), al(*)
c-----------------------------------------------------------------------
c    Solves L x = y    L = triangular. Uses LEVEL SCHEDULING/MSR format 
c-----------------------------------------------------------------------
c
c On entry:
c---------- 
c n      = integer. dimension of problem.
c y      = real array containg the right hand side.
c
c al,
c jal,   = Lower triangular matrix stored in Modified Sparse Row 
c          format. 
c nlev   = number of levels in matrix
c lev    = integer array of length n, containing the permutation
c          that defines the levels in the level scheduling ordering.
c ilev   = pointer to beginning of levels in lev.
c          the numbers lev(i) to lev(i+1)-1 contain the row numbers
c          that belong to level number i, in the level shcheduling
c          ordering.
c
c On return:
c----------- 
c	x = The solution of  L x = y .
c--------------------------------------------------------------------
      integer ii, jrow, i 
      real*8 t 
c     
c     outer loop goes through the levels. (SEQUENTIAL loop)
c     
      do 150 ii=1, nlev
c     
c     next loop executes within the same level. PARALLEL loop
c     
         do 100 i=ilev(ii), ilev(ii+1)-1 
            jrow = lev(i)
c
c compute inner product of row jrow with x
c 
            t = y(jrow) 
            do 130 k=jal(jrow), jal(jrow+1)-1 
               t = t - al(k)*x(jal(k))
 130        continue
            x(jrow) = t*al(jrow) 
 100     continue
 150  continue
      return
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine leftblk(n, nrow, ncol, a, ja, ia)
      implicit none
      integer n, nrow, ncol, ja(1:*), ia(1:*)
      real*8  a(1:*)
c-----------------------------------------------------------------------
c     Generate the subdiagonal block for the problem given.
c-----------------------------------------------------------------------
      integer i, k
      nrow = 1 + (n-1)/2
      ncol = n/2
      k = 1
      do 10 i = 1, nrow
         ia(i) = k
         if (nrow.ne.ncol) then
            if (i.gt.1) then
               ja(k) = i-1
               a(k) = -1.0
               k = k+1
            end if
         end if
         if (i.le.ncol) then
            ja(k) = i
            a(k) = -1.0
            k = k+1
         end if
         if (nrow.eq.ncol) then
            if (i.lt.ncol) then
               ja(k) = i+1
               a(k) = -1.0
               k = k+1
            end if
         end if
 10   continue
      ia(nrow+1) = k
      return
c---------end-of-leftblk------------------------------------------------ 
      end
      integer function lenstr0(s)
c-----------------------------------------------------------------------
c return length of the string S
c-----------------------------------------------------------------------
      character*(*) s
      integer len
      intrinsic len
      integer n
c----------------------------------------------------------------------- 
      n = len(s)
10    continue
        if (s(n:n).eq.' ') then
          n = n-1
          if (n.gt.0) go to 10
        end if
      lenstr0 = n
c
      return
c-----------------------------------------------------------------------
      end
c
      integer function lenstr(s)
c-----------------------------------------------------------------------
c return length of the string S
c-----------------------------------------------------------------------
      character*(*) s
      integer len
      intrinsic len
      integer n
c----------------------------------------------------------------------- 
      n = len(s)
10    continue
        if (s(n:n).eq.' ') then
          n = n-1
          if (n.gt.0) go to 10
        end if
      lenstr = n
c
      return
c--------end-of-pspltm--------------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine levels (n, jal, ial, nlev, lev, ilev, levnum)
      integer jal(*),ial(*), levnum(*), ilev(*), lev(*) 
c-----------------------------------------------------------------------
c levels gets the level structure of a lower triangular matrix 
c for level scheduling in the parallel solution of triangular systems
c strict lower matrices (e.g. unit) as well matrices with their main 
c diagonal are accepted. 
c-----------------------------------------------------------------------
c on entry:
c----------
c n        = integer. The row dimension of the matrix
c jal, ial = 
c 
c on return:
c-----------
c nlev     = integer. number of levels found
c lev      = integer array of length n containing the level
c            scheduling permutation.
c ilev     = integer array. pointer to beginning of levels in lev.
c            the numbers lev(i) to lev(i+1)-1 contain the row numbers
c            that belong to level number i, in the level scheduling
c            ordering. The equations of the same level can be solved
c            in parallel, once those of all the previous levels have
c            been solved.
c work arrays:
c-------------
c levnum   = integer array of length n (containing the level numbers
c            of each unknown on return)
c-----------------------------------------------------------------------
      do 10 i = 1, n
         levnum(i) = 0
 10   continue
c
c     compute level of each node --
c
      nlev = 0
      do 20 i = 1, n
         levi = 0
         do 15 j = ial(i), ial(i+1) - 1
            levi = max (levi, levnum(jal(j)))
 15      continue
         levi = levi+1 
         levnum(i) = levi 
         nlev = max(nlev,levi) 
 20   continue
c-------------set data structure  --------------------------------------
      do 21 j=1, nlev+1
         ilev(j) = 0
 21   continue
c------count  number   of elements in each level ----------------------- 
      do 22 j=1, n
         i = levnum(j)+1
         ilev(i) = ilev(i)+1
 22   continue
c---- set up pointer for  each  level ---------------------------------- 
      ilev(1) = 1
      do 23 j=1, nlev
         ilev(j+1) = ilev(j)+ilev(j+1)
 23   continue
c-----determine elements of each level -------------------------------- 
      do 30 j=1,n
         i = levnum(j)
         lev(ilev(i)) = j
         ilev(i) = ilev(i)+1
 30   continue
c     reset pointers backwards
      do 35 j=nlev, 1, -1
         ilev(j+1) = ilev(j) 
 35   continue
      ilev(1) = 1
      return
c----------end-of-levels------------------------------------------------ 
C-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine lnkcsr (n, a, jcol, istart, link, ao, jao, iao) 
      real*8 a(*), ao(*) 
      integer n, jcol(*), istart(n), link(*), jao(*), iao(*) 
c----------------------------------------------------------------------- 
c     Linked list storage format   to      Compressed Sparse Row  format
c----------------------------------------------------------------------- 
c this subroutine translates a matrix stored in linked list storage 
c format into the compressed sparse row format. 
c----------------------------------------------------------------------- 
c Coded by Y. Saad, Feb 21, 1991.
c----------------------------------------------------------------------- 
c
c on entry:
c----------
c n	= integer equal to the dimension of A.	
c         
c a	= real array of size nna containing the nonzero elements
c jcol	= integer array of size	nnz containing the column positions
c 	  of the corresponding elements in a.
c istart= integer array of size n poiting to the beginning of the rows.
c         istart(i) contains the position of the first element of 
c         row i in data structure. (a, jcol, link).
c         if a row is empty istart(i) must be zero.
c link	= integer array of size nnz containing the links in the linked 
c         list data structure. link(k) points to the next element 
c         of the row after element ao(k), jcol(k). if link(k) = 0, 
c         then there is no next element, i.e., ao(k), jcol(k) is 
c         the last element of the current row.
c
c on return:
c-----------
c ao, jao, iao = matrix stored in csr format:
c
c ao    = real array containing the values of the nonzero elements of 
c         the matrix stored row-wise. 
c jao	= integer array of size nnz containing the column indices.
c iao	= integer array of size n+1 containing the pointers array to the 
c         beginning of each row. iao(i) is the address in ao,jao of
c         first element of row i.
c
c----------------------------------------------------------------------- 
c first determine individial bandwidths and pointers.
c----------------------------------------------------------------------- 
c local variables
      integer irow, ipos, next
c-----------------------------------------------------------------------
      ipos = 1
      iao(1) = ipos
c     
c     loop through all rows
c     
      do 100 irow =1, n
c     
c     unroll i-th row.
c     
         next = istart(irow)
 10      if (next .eq. 0) goto 99
         jao(ipos) = jcol(next)
         ao(ipos)  = a(next)
         ipos = ipos+1
         next = link(next) 
         goto 10
 99      iao(irow+1) = ipos 
 100  continue
c     
      return
c-------------end-of-lnkcsr ------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine lsolc (n,x,y,al,jal,ial)
      integer n, jal(*),ial(*) 
      real*8  x(n), y(n), al(*) 
c-----------------------------------------------------------------------
c       SOLVES     L x = y ;    where L = unit lower trang. CSC format
c-----------------------------------------------------------------------
c solves a unit lower triangular system by standard (sequential )
c forward elimination - matrix stored in CSC format. 
c-----------------------------------------------------------------------
c
c On entry:
c---------- 
c n      = integer. dimension of problem.
c y      = real*8 array containg the right side.
c
c al,
c jal,
c ial,    = Lower triangular matrix stored in compressed sparse column 
c          format. 
c
c On return:
c----------- 
c	x  = The solution of  L x  = y.
c-----------------------------------------------------------------------
c local variables 
c
      integer k, j
      real*8 t
c-----------------------------------------------------------------------
      do 140 k=1,n
         x(k) = y(k) 
 140  continue
      do 150 k = 1, n-1
         t = x(k) 
         do 100 j = ial(k), ial(k+1)-1
            x(jal(j)) = x(jal(j)) - t*al(j) 
 100     continue
 150  continue
c
      return
c----------end-of-lsolc------------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
c----------------------end-of-vbrmv-------------------------------------
c-----------------------------------------------------------------------
c----------------------------------------------------------------------c
c 2)     T R I A N G U L A R    S Y S T E M    S O L U T I O N S       c
c----------------------------------------------------------------------c
      subroutine lsol (n,x,y,al,jal,ial)
      integer n, jal(*),ial(n+1) 
      real*8  x(n), y(n), al(*) 
c-----------------------------------------------------------------------
c   solves    L x = y ; L = lower unit triang. /  CSR format
c----------------------------------------------------------------------- 
c solves a unit lower triangular system by standard (sequential )
c forward elimination - matrix stored in CSR format. 
c-----------------------------------------------------------------------
c
c On entry:
c---------- 
c n      = integer. dimension of problem.
c y      = real array containg the right side.
c
c al,
c jal,
c ial,    = Lower triangular matrix stored in compressed sparse row
c          format. 
c
c On return:
c----------- 
c	x  = The solution of  L x  = y.
c--------------------------------------------------------------------
c local variables 
c
      integer k, j 
      real*8  t
c-----------------------------------------------------------------------
      x(1) = y(1) 
      do 150 k = 2, n
         t = y(k) 
         do 100 j = ial(k), ial(k+1)-1
            t = t-al(j)*x(jal(j))
 100     continue
         x(k) = t 
 150  continue
c
      return
c----------end-of-lsol-------------------------------------------------- 
c-----------------------------------------------------------------------
      end

	subroutine lstif3(ske, fe, xe, ye, 
     1	   xyk, funb, func, fung)
c---------------------------------------------------------------------------
c
c  This subroutine computes the local stiffness matrix for the 
c   Diffusion-Convection Equation with the 
c    variable cofficients, 'K(x,y), B(x,y), C(x,y) '
c
c  -Div( K(x,y) T(x,y)) + B(x,y) Tx + C(x,y) Ty = G
c
c   Here K(x,y) is a 2x2 Matrix, where each entry is a function of x and y.
c
c   K, B, C and G need to be supplied by user.
c    They need to be defined as externals in the calling routines.
c
c   PSI(i,x,y) : i-th shape fucntions on the standard triangle N, i=1, 2, 3
c      where N is the following.
c
c             (-1,1)
c                .
c                . .
c		 .   .
c		 .       .
c		 . . . . . . (1,-1)
c	       (-1,-1)
c
c   Local stiffness matrix is obtained by integral on the current 
c   element. To do so, change the current coordinates to N 
c    by Affine mapping, sending
c
c    (xe(1),ye(1))   ---> (-1,-1)
c    (xe(2),ye(2))   ---> (1,-1)
c    (xe(3),ye(3))   ---> (-1,1) .
c
c    Then we perform the integration on N
c     by Gaussian Quadrature with 9 points.
c
c---------------------------------------------------------------------------
c
c  on entry
c  ---------
c
c   xe       = x coordinates of the nodes in the current element.
c   ye       = y coordinates of the nodes in the current element.
c   xyk      = subroutine defining the function K(x,y). 
c   funb     = function defining the function b(x,y).
c   func     = function defining the function c(x,y).
c   fung     = function defining the function g(x,y).
c
c---------------------------------------------------------------------------
c
c  on return
c  ---------
c
c   ske : Local Stiffness Matrix.( 3x3 in this subroutine.)
c   fe  : Local Load Vector.
c
c---------------------------------------------------------------------------
	implicit real*8(a-h,o-z)
	dimension ske(3,3), fe(3), xe(3), ye(3), dn(3,2), 
     1            xyke(2,2), wei(9), gau1(9), gau2(9)
	external xyk, funb, func, fung

c  Gau1 and Gau2 are the Gaussian Quadrature Points for the Traingle N,
c   and Wei, are the corresponding weights.
c
c   They are derived from the 1-D case by Reiterated integrals.
c
	data gau1/-0.8, -0.1127016654, 0.5745966692, -0.8872983346, -0.5, 
     1        -0.1127016654,  -0.9745966692, -0.8872983346, -0.8 /
	data gau2/3*-0.7745966692, 3*0., 3*0.7745966692 /
	data wei/0.2738575107, 0.4381720172, 0.2738551072, 0.2469135803, 
     1     0.3950617284, 0.2469135803, 0.03478446464, 
     2     0.05565514341, 0.03478446464 /

	npt = 9

c
c    Compute the Affine mappings from the current triangle to the
c     standard triangle N. Integration will be performed on that
c     triangle by Gaussian quadrature.
c
c    T = A X + B
c   
c    A11, A12, A21, A22, B1, B2 will denote the entries of
c     A & B.
c
	x1 = xe(1)
	x2 = xe(2)
	x3 = xe(3)
	y1 = ye(1)
	y2 = ye(2)
	y3 = ye(3)

	rj1 = (x3-x1)*(y2-y3) - (x2-x3)*(y3-y1)
	rj2 = (x3-x1)*(y1-y2) - (x1-x2)*(y3-y1)
	a11 = 2*(y1-y3)/rj1
	a12 = 2*(x3-x1)/rj1
	a21 = 2*(y1-y2)/rj2
	a22 = 2*(x2-x1)/rj2
	b1 = 1. - a11*x2 - a12*y2
	b2 = -1. - a21*x2 - a22*y2

c
c  Compute the first order partial derivatives of the shape functions.
c   dn(i,1) and dn(i,2) are the first order partial derivativ of i-th shape function
c    with respect to x and y, respectively.
c
	dn(1,1) = -0.5*(a11+a21)
	dn(1,2) = -0.5*(a12+a22)
	dn(2,1) = 0.5*a11
	dn(2,2) = 0.5*a12
	dn(3,1) = 0.5*a21
	dn(3,2) = 0.5*a22
c  Compute the Jacobian associated with T.
	Rja = a11*a22 - a12*a21
c
c  Find the inverse mapping of T
c

	u11 = a22/rja
	u12 = -a12/Rja
	u21 = -a21/rja
	u22 = a11/rja
	v1 = -u11*b1 - u12*b2
	v2 = -u21*b1 - u22*b2

	do 200 i = 1 , 3
	  T4 = 0.
	do 220 j = 1 , 3
	  T1 = 0.
	  T2 = 0.
	  T3 = 0.
	  do 250 k = 1, npt
	    r = gau1(k)
	    s = gau2(k)
	    w = wei(k)

	    x = u11*r + u12*s + v1
	    y = u21*r + u22*s + v2

	    call xyk(xyke, x, y)

	    derv2 = dn(i,1)*dn(j,1)*xyke(1,1) 
     1            + dn(i,2)*dn(j,2)*xyke(2,2)
     2            + dn(i,1)*dn(j,2)*xyke(1,2)
     3            + dn(i,2)*dn(j,1)*xyke(2,1)
	    if(j .eq. 1) then
	      T4 = T4 + w*fung(x,y)*psi(i,r,s)
	    endif

	    T1 = T1 + w*derv2
	    T2 = T2 + w*funb(x,y)*psi(i,r,s)
	    T3 = T3 + w*func(x,y)*psi(i,r,s)
250	  continue
	
	ske(i,j) = (T1 + T2*dn(j,1) + T3*dn(j,2))/Rja
220	continue	
	fe(i) = T4/Rja
200	continue

	return
	end
c-----------------------------------------------------------------------
	subroutine lusol(n, y, x, alu, jlu, ju)
        real*8 x(n), y(n), alu(*)
	integer n, jlu(*), ju(*)
c-----------------------------------------------------------------------
c
c This routine solves the system (LU) x = y, 
c given an LU decomposition of a matrix stored in (alu, jlu, ju) 
c modified sparse row format 
c
c-----------------------------------------------------------------------
c on entry:
c n   = dimension of system 
c y   = the right-hand-side vector
c alu, jlu, ju 
c     = the LU matrix as provided from the ILU routines. 
c
c on return
c x   = solution of LU x = y.     
c-----------------------------------------------------------------------
c 
c Note: routine is in place: call lusol (n, x, x, alu, jlu, ju) 
c       will solve the system with rhs x and overwrite the result on x . 
c
c-----------------------------------------------------------------------
c local variables
c
        integer i,k
c
c forward solve
c
        do 40 i = 1, n
           x(i) = y(i)
           do 41 k=jlu(i),ju(i)-1
              x(i) = x(i) - alu(k)* x(jlu(k))
 41        continue
 40     continue
c
c     backward solve.
c
	do 90 i = n, 1, -1
	   do 91 k=ju(i),jlu(i+1)-1
              x(i) = x(i) - alu(k)*x(jlu(k))
 91	   continue
           x(i) = alu(i)*x(i)
 90     continue
c
  	return
c----------------end of lusol ------------------------------------------
c-----------------------------------------------------------------------
	end
c-----------------------------------------------------------------------
	subroutine lutsol(n, y, x, alu, jlu, ju) 
        real*8 x(n), y(n), alu(*)
	integer n, jlu(*), ju(*)
c-----------------------------------------------------------------------
c
c This routine solves the system  Transp(LU) x = y,
c given an LU decomposition of a matrix stored in (alu, jlu, ju) 
c modified sparse row format. Transp(M) is the transpose of M. 
c----------------------------------------------------------------------- 
c on entry:
c n   = dimension of system 
c y   = the right-hand-side vector
c alu, jlu, ju 
c     = the LU matrix as provided from the ILU routines. 
c
c on return
c x   = solution of transp(LU) x = y.   
c-----------------------------------------------------------------------
c
c Note: routine is in place: call lutsol (n, x, x, alu, jlu, ju) 
c       will solve the system with rhs x and overwrite the result on x . 
c 
c-----------------------------------------------------------------------
c local variables
c
        integer i,k
c
        do 10 i = 1, n
           x(i) = y(i)
 10     continue
c
c forward solve (with U^T)
c
        do 20 i = 1, n
           x(i) = x(i) * alu(i)
           do 30 k=ju(i),jlu(i+1)-1
              x(jlu(k)) = x(jlu(k)) - alu(k)* x(i)
 30        continue
 20     continue
c     
c     backward solve (with L^T)
c     
	do 40 i = n, 1, -1 
	   do 50 k=jlu(i),ju(i)-1
              x(jlu(k)) = x(jlu(k)) - alu(k)*x(i)
 50        continue
 40     continue
c
  	return
c----------------end of lutsol -----------------------------------------
c-----------------------------------------------------------------------
	end
c-----------------------------------------------------------------------
      subroutine mapper4 (n,ja,ia,ndom,nodes,levst,marker,link)
      implicit none 
      integer n,ndom,ja(*),ia(*),marker(n),levst(2*ndom), 
     *     nodes(*),link(*) 
c-----------------------------------------------------------------------
c     finds domains given ndom centers -- by doing a level set expansion 
c-----------------------------------------------------------------------
c     on entry:
c     ---------
c     n      = dimension of matrix 
c     ja, ia = adajacency list of matrix (CSR format without values) -- 
c     ndom   = number of subdomains (nr output by coarsen)  
c     nodes  = array of size at least n. On input the first ndom entries
c              of nodes should contain the labels of the centers of the 
c              ndom domains from which to do the expansions. 
c     
c     on return 
c     --------- 
c     link  = linked list array for the ndom domains. 
c     nodes = contains the list of nodes of the domain corresponding to
c             link. (nodes(i) and link(i) are related to the same node). 
c    
c     levst = levst(j) points to beginning of subdomain j in link.
c
c     work arrays:
c     ----------- 
c     levst : work array of length 2*ndom -- contains the beginning and
c     end of  current level in link. 
c     beginning of last level in link for each processor.
c     also ends in levst(ndom+i) 
c     marker : work array of length n. 
c
c     Notes on implementation: 
c     ----------------------- 
c     for j .le. ndom link(j) is <0  and indicates the end of the
c     linked list. The most recent element added to the linked
c     list is added at the end of the list (traversal=backward) 
c     For  j .le. ndom, the value of -link(j) is the size of 
c     subdomain j. 
c
c-----------------------------------------------------------------------
c     local variables 
      integer mindom,j,lkend,nod,nodprev,idom,next,i,kk,ii,ilast,nstuck,
     *     isiz, nsize 
c     
c     initilaize nodes and link arrays
c
      do 10 j=1, n
         marker(j) = 0
 10   continue
c     
      do 11 j=1, ndom
         link(j) = -1 
         marker(nodes(j)) = j
         levst(j) = j
         levst(ndom+j) = j 
 11   continue
c
c     ii = next untouched node for restarting new connected component. 
c 
      ii = 0
c     
      lkend = ndom
      nod   = ndom  
      nstuck = 0 
c-----------------------------------------------------------------------
 100  continue 
      idom = mindom(n,ndom,link) 
c-----------------------------------------------------------------------
c     begin level-set loop 
c-----------------------------------------------------------------------
 3    nodprev = nod 
      ilast = levst(ndom+idom) 
      levst(ndom+idom) = lkend       
      next = levst(idom) 
c     
c     linked list traversal loop
c 
      isiz = 0 
      nsize = link(idom) 
 1    i = nodes(next) 
      isiz = isiz + 1 
c     
c     adjacency list traversal loop 
c     
      do 2 kk=ia(i), ia(i+1)-1
         j = ja(kk) 
         if (marker(j) .eq. 0) then 
            call add_lk(j,nod,idom,ndom,lkend,levst,link,nodes,marker) 
         endif
 2    continue
c     
c     if last element of the previous level not reached continue
c     
      if (next .gt. ilast) then
         next = link(next) 
         if (next .gt. 0) goto 1
      endif
c-----------------------------------------------------------------------
c     end level-set traversal --  
c-----------------------------------------------------------------------
      if (nodprev .eq. nod) then
c     
c     link(idom) >0 indicates that set is stuck  --  
c
         link(idom) = -link(idom) 
         nstuck = nstuck+1
      endif
c     
      if (nstuck .lt. ndom) goto 100 
c
c     reset sizes -- 
c 
      do j=1, ndom
         if (link(j) .gt. 0) link(j) = -link(j)
      enddo
c
      if (nod .eq. n) return 
c
c     stuck. add first non assigned point to smallest domain
c     
 20   ii = ii+1
      if (ii .le. n) then
         if (marker(ii) .eq. 0) then 
            idom = 0 
            isiz = n+1 
            do 30 kk=ia(ii), ia(ii+1)-1
               i = marker(ja(kk)) 
               if (i .ne. 0) then 
                  nsize = abs(link(i)) 
                  if (nsize .lt. isiz) then 
                     isiz = nsize
                     idom = i 
                  endif
               endif
 30            continue
c
c     if no neighboring domain select smallest one 
c
               if (idom .eq. 0) idom = mindom(n,ndom,link) 
c     
c     add ii to sudomain idom at end of linked list  
c     
            call add_lk(ii,nod,idom,ndom,lkend,levst,link,nodes,marker) 
            goto 3 
         else
            goto 20 
         endif
      endif
      return
      end
c----------------------------------------------------------------------- 
      integer function maskdeg (ja,ia,nod,mask,maskval) 
      implicit none 
      integer ja(*),ia(*),nod,mask(*),maskval
c-----------------------------------------------------------------------
      integer deg, k 
      deg = 0 
      do k =ia(nod),ia(nod+1)-1
         if (mask(ja(k)) .eq. maskval) deg = deg+1 
      enddo
      maskdeg = deg 
      return
      end 
c----------------------------------------------------------------------- 
      SUBROUTINE MATRF2(M,N,C,INDEX,ALPHA,NN,NZ,A,SNR,RNR,FEJLM)
C--------------------------------------------------------------------
C
C   PURPOSE
C   -------
C   The subroutine generates sparse (rectangular or square) matrices.
C   The dimensions of the matrix and the average number of nonzero
C   elements per row can be specified by the user. Moreover, the user
C   can also change the sparsity pattern and the condition number of the
C   matrix. The non-zero elements of the desired matrix will be
C   accumulated (in an arbitrary order) in the first NZ positions of
C   array A. The column and the row numbers of the non-zero element
C   stored in A(I), I=1,...,NZ, will be found in SNR(I) and RNR(I),
C   respectively. The matrix generated by this subroutine is of the
C   class F(M,N,C,R,ALPHA) (see reference).
C
C   Note: If A is the sparse matrix of type F(M,N,C,R,ALPHA), then
C
C           min|A(i,j)| = 1/ALPHA,
C
C           max|A(i,j)| = max(INDEX*N - N,10*ALPHA).
C
C
C   CONTRIBUTOR: Ernest E. Rothman
C                Cornell Theory Center/Cornell National Supercomputer
C                Facility.
C                e-mail address: BITNET:   eer@cornellf
C                                INTERNET: eer@cornellf.tn.cornell.edu
C
C   minor modifications by Y. Saad. April 26, 1990.
C
C   Note: This subroutine has been copied from the following reference.
C         The allowable array sizes have been changed.
C
C   REFERENCE: Zlatev, Zahari; Schaumburg, Kjeld; Wasniewski, Jerzy;
C      "A testing Scheme for Subroutines Solving Large Linear Problems",
C      Computers and Chemistry, Vol. 5, No. 2-3, pp. 91-100, 1981.
C
C
C   INPUT PARAMETERS
C   ----------------
C   M    - Integer. The number of rows in the desired matrix.
C          N < M+1 < 9000001 must be specified.
C
C   N    - Integer. The number of columns in the desired matrix.
C          21 < N < 9000001 must be specified.
C
C   C    - Integer. The sparsity pattern can be changed by means of this
C          parameter.  10 < C < N-10  must be specified.
C
C   INDEX - Integer.  The average number of non-zero elements per row in
C           the matrix will be equal to INDEX.
C           1 < INDEX < N-C-8 must be specified.
C
C   ALPHA - Real. The condition number of the matrix can be changed
C           BY THIS PARAMETER. ALPHA > 0.0 MUST BE SPECIFIED.
C           If ALPHA is approximately equal to 1.0 then the generated
C           matrix is well-conditioned. Large values of ALPHA will
C           usually produce ill-conditioned matrices. Note that no
C           round-off errors during the computations in this subroutine
C           are made if ALPHA = 2**I (where I is an arbitrary integer
C           which produces numbers in the machine range).
C
C   NN    - Integer. The length of arrays A, RNR, and SNR (see below).
C           INDEX*M+109 < NN < 9000001 must be specified.
C
C
C   OUTPUT PARAMETERS
C   -----------------
C   NZ    - Integer. The number of non-zero elements in the matrix.
C
C   A(NN) - Real array. The non-zero elements of the matrix generated
C           are accumulated in the first NZ locations of array A.
C
C   SNR(NN) - INTEGER array. The column number of the non-zero element
C           kept in A(I), I=1,...NZ, is stored in SNR(I).
C
C   RNR(NN) - Integer array. The row number of the non-zero element
C           kept in A(I), I=1,...NZ, is stored in RNR(I).
C
C   FEJLM - Integer. FEJLM=0 indicates that the call is successful.
C           Error diagnostics are given by means of positive values of
C           this parameter as follows:
C             FEJLM = 1    -  N       is out of range.
C             FEJLM = 2    -  M       is out of range.
C             FEJLM = 3    -  C       is out of range.
C             FEJLM = 4    -  INDEX   is out of range.
C             FEJLM = 5    -  NN      is out of range.
C             FEJLM = 7    -  ALPHA   is out of range.
C
C
C
C
      REAL*8 A, ALPHA, ALPHA1
      INTEGER M, N, NZ, C, NN, FEJLM, M1, NZ1, RR1, RR2, RR3, K
      INTEGER M2, N2
      INTEGER SNR, RNR
      DIMENSION A(NN), SNR(NN), RNR(NN)
      M1 = M
      FEJLM = 0
      NZ1 = INDEX*M + 110
      K = 1
      ALPHA1 = ALPHA
      INDEX1 = INDEX - 1
C
C  Check the parameters.
C
      IF(N.GE.22) GO TO 1
2     FEJLM = 1
      RETURN
1     IF(N.GT.9000000) GO TO 2
      IF(M.GE.N) GO TO 3
4     FEJLM = 2
      RETURN
3     IF(M.GT.9000000) GO TO 4
      IF(C.LT.11)GO TO 6
      IF(N-C.GE.11)GO TO 5
6     FEJLM = 3
      RETURN
5     IF(INDEX.LT.1) GO TO 12
      IF(N-C-INDEX.GE.9)GO TO 13
12    FEJLM = 4
13    IF(NN.GE.NZ1)GO TO 7
8     FEJLM = 5
      RETURN
7     IF(NN.GT.9000000)GO TO 8
      IF(ALPHA.GT.0.0)GO TO 9
      FEJLM = 6
      RETURN
9     CONTINUE
C
C  End of the error check. Begin to generate the non-zero elements of
C  the required matrix.
C
      DO 20 I=1,N
      A(I) = 1.0d0
      SNR(I) = I
20    RNR(I) = I
      NZ = N
      J1 = 1
      IF(INDEX1.EQ.0) GO TO 81
      DO 21 J = 1,INDEX1
      J1 = -J1
      DO 22 I=1,N
      A(NZ+I) = dfloat(J1*J*I)
      IF(I+C+J-1.LE.N)SNR(NZ+I) = I + C + J - 1
      IF(I+C+J-1.GT.N)SNR(NZ+I) = C + I + J - 1 - N
22    RNR(NZ + I) = I
21    NZ = NZ + N
81    RR1 = 10
      RR2 = NZ
      RR3 = 1
25    CONTINUE
      DO 26 I=1,RR1
      A(RR2 + I) = ALPHA*dfloat(I)
      SNR(RR2+I) = N - RR1 + I
      RNR(RR2+I) = RR3
26    CONTINUE
      IF(RR1.EQ.1) GO TO 27
      RR2 = RR2 + RR1
      RR1 = RR1 - 1
      RR3 = RR3 + 1
      GO TO 25
27    NZ = NZ + 55
29    M1 = M1 - N
      ALPHA = 1.0d0/ALPHA
      IF(M1.LE.0) GO TO 28
      N2 = K*N
      IF(M1.GE.N)M2 = N
      IF(M1.LT.N)M2 = M1
      DO 30 I=1,M2
      A(NZ+I) = ALPHA*dfloat(K+1)
      SNR(NZ + I) = I
30    RNR(NZ + I) = N2 + I
      NZ = NZ + M2
      IF(INDEX1.EQ.0) GO TO 82
      J1 = 1
      DO 41 J = 1,INDEX1
      J1 = -J1
      DO 42 I = 1,M2
      A(NZ+I) = ALPHA*dFLOAT(J*J1)*(dfloat((K+1)*I)+1.0d0)
      IF(I+C+J-1.LE.N)SNR(NZ+I) = I + C + J - 1
      IF(I+C+J-1.GT.N)SNR(NZ+I) = C + I + J - 1 - N
42    RNR(NZ + I) = N2 + I
41    NZ = NZ +M2
82    K = K + 1
      GO TO 29
28    CONTINUE
      ALPHA = 1.0d0/ALPHA1
      RR1 = 1
      RR2 = NZ
35    CONTINUE
      DO 36 I = 1,RR1
      A(RR2+I) = ALPHA*dfloat(RR1+1-I)
      SNR(RR2+I) = I
      RNR(RR2+I) = N - 10 + RR1
36    CONTINUE
      IF(RR1.EQ.10) GO TO 34
      RR2 = RR2 + RR1
      RR1 = RR1 + 1
      GO TO 35
34    NZ = NZ + 55
      ALPHA = ALPHA1
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine mgsr (n, i0, i1, ss, r)
c     implicit  real*8 (a-h,o-z)
      integer n, i0, i1
      real*8 ss(n,i1), r(i1)
c-----------------------------------------------------------------------
c modified gram - schmidt  with  partial  reortho. the vector ss(*,i1) is
c orthogonalized against the first i vectors  of ss  (which  are  already
c orthogonal).  the coefficients of the orthogonalization are returned in
c the array r
c------------------------------------------------------------------------
c local variables 
c 
      integer i, j, k, it
      real*8 hinorm, tet, ddot, t, dsqrt
      data  tet/10.0d0/

      do 53 j=1, i1
         r(j) = 0.0d0
 53   continue
      i = i1-1
      it = 0
 54   hinorm = 0.0d0
      it = it +1
      if (i .eq. 0) goto 56
c     
      do 55 j=i0, i
         t = ddot(n, ss(1,j),1,ss(1,i1),1)
         hinorm = hinorm + t**2
         r(j) = r(j) + t
         call daxpy(n,-t,ss(1,j),1,ss(1,i1),1)
 55   continue
      t = ddot(n, ss(1,i1), 1, ss(1,i1), 1)
 56   continue
c     
c     test for reorthogonalization see daniel et. al.
c     two reorthogonalization allowed ---
c     
      if (t*tet .le. hinorm .and. it .lt. 2) goto 54
      t =dsqrt(t)
      r(i1)= t
      if (t .eq. 0.0d0) return
      t = 1.0d0/t
      do 57  k=1,n
         ss(k,i1) = ss(k,i1)*t
 57   continue
      return
      end
c-----------------------------------------------------------------------
      subroutine mgsro(full,lda,n,m,ind,ops,vec,hh,ierr)
      implicit none
      logical full
      integer lda,m,n,ind,ierr
      real*8  ops,hh(m),vec(lda,m)
c-----------------------------------------------------------------------
c     MGSRO  -- Modified Gram-Schmidt procedure with Selective Re-
c               Orthogonalization
c     The ind'th vector of VEC is orthogonalized against the rest of
c     the vectors.
c
c     The test for performing re-orthogonalization is performed for
c     each indivadual vectors. If the cosine between the two vectors
c     is greater than 0.99 (REORTH = 0.99**2), re-orthogonalization is
c     performed. The norm of the 'new' vector is kept in variable NRM0,
c     and updated after operating with each vector.
c
c     full   -- .ture. if it is necessary to orthogonalize the ind'th
c               against all the vectors vec(:,1:ind-1), vec(:,ind+2:m)
c               .false. only orthogonalize againt vec(:,1:ind-1)
c     lda    -- the leading dimension of VEC
c     n      -- length of the vector in VEC
c     m      -- number of vectors can be stored in VEC
c     ind    -- index to the vector to be changed
c     ops    -- operation counts
c     vec    -- vector of LDA X M storing the vectors
c     hh     -- coefficient of the orthogonalization
c     ierr   -- error code
c               0 : successful return
c               -1: zero input vector
c               -2: input vector contains abnormal numbers
c               -3: input vector is a linear combination of others
c
c     External routines used: real*8 distdot
c-----------------------------------------------------------------------
      integer i,k
      real*8  nrm0, nrm1, fct, thr, distdot, zero, one, reorth
      parameter (zero=0.0D0, one=1.0D0, reorth=0.98D0)
      external distdot
c
c     compute the norm of the input vector
c
      nrm0 = distdot(n,vec(1,ind),1,vec(1,ind),1)
      ops = ops + n + n
      thr = nrm0 * reorth
      if (nrm0.le.zero) then
         ierr = - 1
         return
      else if (nrm0.gt.zero .and. one/nrm0.gt.zero) then
         ierr = 0
      else
         ierr = -2
         return
      endif
c
c     Modified Gram-Schmidt loop
c
      if (full) then
         do 40 i = ind+1, m
            fct = distdot(n,vec(1,ind),1,vec(1,i),1)
            hh(i) = fct
            do 20 k = 1, n
               vec(k,ind) = vec(k,ind) - fct * vec(k,i)
 20         continue
            ops = ops + 4 * n + 2
            if (fct*fct.gt.thr) then
               fct = distdot(n,vec(1,ind),1,vec(1,i),1)
               hh(i) = hh(i) + fct
               do 30 k = 1, n
                  vec(k,ind) = vec(k,ind) - fct * vec(k,i)
 30            continue
               ops = ops + 4*n + 1
            endif
            nrm0 = nrm0 - hh(i) * hh(i)
            if (nrm0.lt.zero) nrm0 = zero
            thr = nrm0 * reorth
 40      continue
      endif
c
      do 70 i = 1, ind-1
         fct = distdot(n,vec(1,ind),1,vec(1,i),1)
         hh(i) = fct
         do 50 k = 1, n
            vec(k,ind) = vec(k,ind) - fct * vec(k,i)
 50      continue
         ops = ops + 4 * n + 2
         if (fct*fct.gt.thr) then
            fct = distdot(n,vec(1,ind),1,vec(1,i),1)
            hh(i) = hh(i) + fct
            do 60 k = 1, n
               vec(k,ind) = vec(k,ind) - fct * vec(k,i)
 60         continue
            ops = ops + 4*n + 1
         endif
         nrm0 = nrm0 - hh(i) * hh(i)
         if (nrm0.lt.zero) nrm0 = zero
         thr = nrm0 * reorth
 70   continue
c
c     test the resulting vector
c
      nrm1 = sqrt(distdot(n,vec(1,ind),1,vec(1,ind),1))
      ops = ops + n + n
 75   hh(ind) = nrm1
      if (nrm1.le.zero) then
         ierr = -3
         return
      endif
c
c     scale the resulting vector
c
      fct = one / nrm1
      do 80 k = 1, n
         vec(k,ind) = vec(k,ind) * fct
 80   continue
      ops = ops + n + 1
c
c     normal return
c
      ierr = 0
      return
c     end surbotine mgsro
      end
c----------------------------------------------------------------------
	subroutine milu0(n, a, ja, ia, alu, jlu, ju, iw, ierr)
	implicit real*8 (a-h,o-z)
	real*8 a(*), alu(*)
	integer ja(*), ia(*), ju(*), jlu(*), iw(*)
c----------------------------------------------------------------------*
c                *** simple milu(0) preconditioner. ***                *
c----------------------------------------------------------------------*
c Note that this has been coded in such a way that it can be used
c with pgmres. Normally, since the data structure of a, ja, ia is
c the same as that of a, ja, ia, savings can be made. In fact with
c some definitions (not correct for general sparse matrices) all we
c need in addition to a, ja, ia is an additional diagonal.
c Ilu0 is not recommended for serious problems. It is only provided
c here for comparison purposes.
c-----------------------------------------------------------------------
c
c on entry:
c----------
c n       = dimension of matrix
c a, ja,
c ia      = original matrix in compressed sparse row storage.
c
c on return:
c----------
c alu,jlu = matrix stored in Modified Sparse Row (MSR) format containing
c           the L and U factors together. The diagonal (stored in
c           alu(1:n) ) is inverted. Each i-th row of the alu,jlu matrix
c           contains the i-th row of L (excluding the diagonal entry=1)
c           followed by the i-th row of U.
c
c ju	  = pointer to the diagonal elements in alu, jlu.
c
c ierr	  = integer indicating error code on return
c	     ierr = 0 --> normal return
c	     ierr = k --> code encountered a zero pivot at step k.
c work arrays:
c-------------
c iw	    = integer work array of length n.
c------------
c Note (IMPORTANT):
c-----------
C it is assumed that the the elements in the input matrix are ordered
c    in such a way that in each row the lower part comes first and
c    then the upper part. To get the correct ILU factorization, it is
c    also necessary to have the elements of L ordered by increasing
c    column number. It may therefore be necessary to sort the
c    elements of a, ja, ia prior to calling milu0. This can be
c    achieved by transposing the matrix twice using csrcsc.
c-----------------------------------------------------------
          ju0 = n+2
          jlu(1) = ju0
c initialize work vector to zero's
	do 31 i=1, n
 31           iw(i) = 0
c
c-------------- MAIN LOOP ----------------------------------
c
	do 500 ii = 1, n
           js = ju0
c
c generating row number ii or L and U.
c
           do 100 j=ia(ii),ia(ii+1)-1
c
c     copy row ii of a, ja, ia into row ii of alu, jlu (L/U) matrix.
c     
              jcol = ja(j)
              if (jcol .eq. ii) then
                 alu(ii) = a(j)
                 iw(jcol) = ii
                 ju(ii)  = ju0
              else
                 alu(ju0) = a(j)
                 jlu(ju0) = ja(j)
                 iw(jcol) = ju0
                 ju0 = ju0+1
              endif
 100       continue
           jlu(ii+1) = ju0
           jf = ju0-1
           jm = ju(ii)-1
c     s accumulates fill-in values
           s = 0.0d0
           do 150 j=js, jm
              jrow = jlu(j)
              tl = alu(j)*alu(jrow)
              alu(j) = tl
c-----------------------perform linear combination --------
              do 140 jj = ju(jrow), jlu(jrow+1)-1
                 jw = iw(jlu(jj))
                 if (jw .ne. 0) then
                       alu(jw) = alu(jw) - tl*alu(jj)
                    else
                       s = s + tl*alu(jj)
                    endif
 140          continue
 150       continue
c----------------------- invert and store diagonal element.
           alu(ii) = alu(ii)-s
           if (alu(ii) .eq. 0.0d0) goto 600
           alu(ii) = 1.0d0/alu(ii)
c----------------------- reset pointer iw to zero
           iw(ii) = 0
           do 201 i = js, jf
 201          iw(jlu(i)) = 0
 500       continue
           ierr = 0
           return
c     zero pivot :
 600       ierr = ii
           return
c------- end-of-milu0 --------------------------------------------------
c-----------------------------------------------------------------------
           end
c----------------------------------------------------------------------- 
      function mindom(n, ndom, link) 
      implicit none 
      integer mindom, n, ndom, link(n) 
c-----------------------------------------------------------------------
c     returns  the domain with smallest size
c----------------------------------------------------------------------- 
c      locals
c
      integer i, nsize, isiz 
c
      isiz = n+1 
      do 10 i=1, ndom
         nsize = - link(i) 
         if (nsize .lt. 0) goto 10 
         if (nsize .lt. isiz) then 
            isiz = nsize
            mindom = i
         endif
 10   continue
      return
      end 
c----------------------------------------------------------------------
      subroutine moveback (a,ind,rind,last) 
      integer a(*),ind(*),rind(*),last 
c moves the front key to the back and inserts the last
c one back in from the top -- 
c 
c local variables 
c
      integer vacant,xmin 
c
         vacant = 1 
         xmin = a(vacant)
         imin = ind(vacant) 
         call FixHeap(a,ind,rind,last,vacant,last-1) 
         a(last) = xmin 
        ind(last) = imin 
         rind(ind(last)) = last
c
         return
         end 
c----------------------------------------------------------------------
      subroutine movebackM (a,ind,rind,last) 
      integer a(*),ind(*),rind(*),last 
c----------------------------------------------------------------------
c moves the front key to the back and inserts the last
c one back in from the top --  MAX HEAP VERSION 
c----------------------------------------------------------------------
c 
c local variables 
c
      integer vacant,xmin 
c
      vacant = 1 
      xmin = a(vacant)
      imin = ind(vacant) 
      call FixHeapM(a,ind,rind,last,vacant,last-1) 
      a(last) = xmin 
      ind(last) = imin 
      rind(ind(last)) = last
c----------------------------------------------------------------------
      return
      end 
c-----------------------------------------------------------------------
      subroutine msrcop (nrow,a,ja,ao,jao,job) 
      real*8 a(*),ao(*) 
      integer nrow, ja(*),jao(*), job 
c----------------------------------------------------------------------
c copies the MSR matrix a, ja, into the MSR matrix ao, jao 
c----------------------------------------------------------------------
c on entry:
c---------
c nrow	= row dimension of the matrix 
c a,ja  = input matrix in Modified compressed sparse row format. 
c job   = job indicator. Values are not copied if job .ne. 1 
c       
c on return:
c----------
c ao, jao   = output matrix containing the same data as a, ja.
c-----------------------------------------------------------------------
c           Y. Saad, 
c-----------------------------------------------------------------------
c local variables
      integer i, k 
c
      do 100 i = 1, nrow+1
         jao(i) = ja(i) 
 100  continue
c     
      do 200 k=ja(1), ja(nrow+1)-1
         jao(k)= ja(k)
 200  continue
c
      if (job .ne. 1) return
      do 201 k=ja(1), ja(nrow+1)-1
         ao(k) = a(k)
 201  continue
      do 202 k=1,nrow
         ao(k) = a(k)
 202  continue
c
      return
c--------end-of-msrcop -------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine msrcsr (n,a,ja,ao,jao,iao,wk,iwk)
      real*8 a(*),ao(*),wk(n)
      integer ja(*),jao(*),iao(n+1),iwk(n+1)
c----------------------------------------------------------------------- 
c       Modified - Sparse Row  to   Compressed Sparse Row   
c
c----------------------------------------------------------------------- 
c converts a compressed matrix using a separated diagonal 
c (modified sparse row format) in the Compressed Sparse Row   
c format.
c does not check for zero elements in the diagonal.
c
c
c on entry :
c---------
c n          = row dimension of matrix
c a, ja      = sparse matrix in msr sparse storage format
c              see routine csrmsr for details on data structure 
c        
c on return :
c-----------
c
c ao,jao,iao = output matrix in csr format.  
c
c work arrays:
c------------
c wk       = real work array of length n
c iwk      = integer work array of length n+1
c
c notes:
c   The original version of this was NOT in place, but has
c   been modified by adding the vector iwk to be in place.
c   The original version had ja instead of iwk everywhere in
c   loop 500.  Modified  Sun 29 May 1994 by R. Bramley (Indiana).
c   
c----------------------------------------------------------------------- 
      logical added
      do 1 i=1,n
         wk(i) = a(i)
         iwk(i) = ja(i)
 1    continue
      iwk(n+1) = ja(n+1)
      iao(1) = 1
      iptr = 1
c---------
      do 500 ii=1,n 
         added = .false.
         idiag = iptr + (iwk(ii+1)-iwk(ii)) 
         do 100 k=iwk(ii),iwk(ii+1)-1
            j = ja(k)
            if (j .lt. ii) then
               ao(iptr) = a(k)
               jao(iptr) = j 
               iptr = iptr+1
            elseif (added) then
               ao(iptr) = a(k)
               jao(iptr) = j 
               iptr = iptr+1
            else 
c add diag element - only reserve a position for it. 
               idiag = iptr
               iptr = iptr+1
               added = .true.
c     then other element
               ao(iptr) = a(k)
               jao(iptr) = j 
               iptr = iptr+1
            endif
 100     continue
         ao(idiag) = wk(ii)
         jao(idiag) = ii
         if (.not. added) iptr = iptr+1
         iao(ii+1) = iptr 
 500  continue
      return    
c------------ end of subroutine msrcsr --------------------------------- 
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c          REORDERING ROUTINES -- COLORING BASED ROUTINES              c 
c----------------------------------------------------------------------c
c contents:                                                            c
c----------                                                            c
c multic  : greedy algorithm for multicoloring                         c
c indset0 : greedy algorithm for independent set ordering              c
c indset1 : independent set ordering using minimal degree traversal    c
c indset2 : independent set ordering with local minimization           c
c indset3 : independent set ordering by vertex cover algorithm         c
c HeapSort, FixHeap, HeapInsert, interchange, MoveBack, FiHeapM,       c
c           FixHeapM, HeapInsertM,indsetr,rndperm, are utility         c
c           routines for sorting, generating random permutations, etc. c
c----------------------------------------------------------------------c
      subroutine multic (n,ja,ia,ncol,kolrs,il,iord,maxcol,ierr) 
      integer n, ja(*),ia(n+1),kolrs(n),iord(n),il(maxcol+1),ierr 
c-----------------------------------------------------------------------
c     multicoloring ordering -- greedy algorithm -- 
c     determines the coloring permutation and sets up
c     corresponding data structures for it.
c-----------------------------------------------------------------------
c on entry
c -------- 
c n     = row and column dimention of matrix
c ja    = column indices of nonzero elements of matrix, stored rowwise.
c ia    = pointer to beginning of each row in ja.
c maxcol= maximum number of colors allowed -- the size of il is
c         maxcol+1 at least. Note: the number of colors does not
c         exceed the maximum degree of each node +1.
c iord  = en entry iord gives the order of traversal of the nodes
c         in the multicoloring algorithm. If there is no preference 
c         then set iord(j)=j for j=1,...,n
c
c on return
c --------- 
c ncol  = number of colours found 
c kolrs = integer array containing the color number assigned to each node 
c il    = integer array containing the pointers to the
c         beginning of each color set. In the permuted matrix
c         the rows /columns il(kol) to il(kol+1)-1 have the same color.
c iord  = permutation array corresponding to the multicolor ordering.
c         row number i will become row nbumber iord(i) in permuted 
c         matrix. (iord = destination permutation array).
c ierr  = integer. Error message. normal return ierr = 0. If ierr .eq.1
c         then the array il was overfilled. 
c 
c-----------------------------------------------------------------------
c     
      integer kol, i, j, k, maxcol, mycol 
c     
      ierr = 0
      do 1 j=1, n
         kolrs(j) = 0
 1    continue 
      do 11 j=1, maxcol
         il(j) = 0
 11   continue
c     
      ncol = 0
c     
c     scan all nodes 
c     
      do 4 ii=1, n
         i = iord(ii) 
c     
c     look at adjacent nodes to determine colors already assigned
c     
         mcol = 0
         do 2 k=ia(i), ia(i+1)-1
            j = ja(k)
            icol = kolrs(j)
            if (icol .ne. 0) then
               mcol = max(mcol,icol) 
c     
c     il used as temporary to record already assigned colors.
c     
               il(icol) = 1 
            endif
 2       continue
c     
c     taken colors determined. scan il until a slot opens up.
c     
         mycol = 1
 3       if (il(mycol) .eq. 1) then
            mycol = mycol+1 
            if (mycol .gt. maxcol) goto 99
            if (mycol .le. mcol) goto 3
         endif
c     
c     reset il to zero for next nodes
c     
         do 35 j=1, mcol
            il(j) = 0
 35      continue
c     
c     assign color and update number of colors so far
c     
         kolrs(i) = mycol
         ncol = max(ncol,mycol)
 4    continue
c     
c     every node has now been colored. Count nodes of each color
c     
      do 6 j=1, n
         kol = kolrs(j)+1
         il(kol) = il(kol)+1
 6    continue
c     
c     set pointers il
c     
      il(1) = 1
      do 7 j=1, ncol
         il(j+1) = il(j)+il(j+1)
 7    continue
c     
c     set iord
c     
      do 8 j=1, n
         kol = kolrs(j) 
         iord(j) = il(kol)
         il(kol) = il(kol)+1
 8    continue
c     
c     shift il back 
c     
      do 9 j=ncol,1,-1
         il(j+1) = il(j)
 9    continue
      il(1) = 1
c     
      return
 99   ierr = 1
      return
c----end-of-multic------------------------------------------------------
c-----------------------------------------------------------------------
      end
c **********************************************************************
      subroutine NEWCNX(n, mccnex, iccnex, index, kpw, mark)
C-----------------------------------------------------------------------
c     
c     We put in ICCNEX the vertices marked in the component MCCNEX. We
c     modify also the vector KPW.
C
C-----------------------------------------------------------------------
c     include "NSIMPLIC"
      dimension kpw(*), mark(n)
C-----------------------------------------------------------------------
C     Laura C. Dutto - email: dutto@cerca.umontreal.ca - December 1993
C-----------------------------------------------------------------------
      do i = 1, n
         if( mark(i) .eq. mccnex) then
              mark(i) = iccnex
              index = index + 1
              kpw(index) = i
         endif
      enddo
c
      return
      end
c-----------------------------------------------------------------------      
      subroutine  n_imp_diag(n,nnz,dist, ipar1,ndiag,ioff,dcount)
      implicit real*8 (a-h, o-z)
      real*8  dcount(*)
      integer n,nnz, dist(*), ndiag, ioff(*), ipar1
c-----------------------------------------------------------------------
c     this routine computes the most important diagonals.
c-----------------------------------------------------------------------
c
c On entry :
c-----------
c n     = integer. column dimension of matrix
c nnz   = number of nonzero elements of matrix
c dist  = integer array containing the numbers of elements in the 
c         matrix with different distance of row indices and column 
c         indices. ipar1 = percentage of nonzero  elements of A that 
c         a diagonal should have in order to be an important diagonal 
c on return
c----------
c ndiag = number of the most important diagonals
c ioff  = the offsets with respect to the main diagonal
c dcount= the accumulated percentages
c-----------------------------------------------------------------------
      n2 = n+n-1
      ndiag = 10
      ndiag = min0(n2,ndiag)
      itot  = 0
      ii = 0
      idiag = 0
c     sort diagonals by decreasing order of weights.
 40   jmax = 0
      i    = 1
      do 41 k=1, n2
         j = dist(k)
         if (j .lt. jmax) goto 41
         i = k
         jmax = j
 41   continue
c     permute ----
c     save offsets and accumulated count if diagonal is acceptable
c     (if it has at least ipar1*nnz/100 nonzero elements)
c     quite if no more acceptable diagonals --
c     
      if (jmax*100 .lt. ipar1*nnz) goto 4
      ii = ii+1
      ioff(ii) = i-n
      dist(i)   = - jmax
      itot = itot + jmax
      dcount(ii) = real(100*itot)/real(nnz)
      if (ii .lt. ndiag) goto 40
 4    continue
      ndiag = ii
      return
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine nonz(n,sym, ja, ia, iao, nzmaxc, nzminc, 
     *     nzmaxr, nzminr, nzcol, nzrow)
      implicit none 
      integer n , nzmaxc, nzminc, nzmaxr, nzminr, nzcol, nzrow
      integer ja(*), ia(n+1), iao(n+1)
      logical sym
c----------------------------------------------------------------------
c     this routine computes maximum numbers of nonzero elements 
c     per column/row, minimum numbers of nonzero elements per column/row, 
c     and  numbers of zero columns/rows.
c----------------------------------------------------------------------
c     On Entry:
c----------
c     n     = integer column dimension of matrix
c     ja    = integer array containing the row indices of elements in a
c     ia    = integer array containing of length n+1 containing the
c     pointers to the beginning of the columns in arrays a and ja.
c     It is assumed that ia(*) = 1 and ia(n+1) = nzz+1.
c     iao   = similar array for the transpose of the matrix. 
c     sym   = logical variable indicating whether or not the matrix is 
c     stored in symmetric mode.
c     on return
c----------
c     nzmaxc = max length of columns
c     nzminc = min length of columns 
c     nzmaxr = max length of rows
c nzminr = min length of rows
c     nzcol  = number of zero columns
c     nzrow = number of zero rows
c-----------------------------------------------------------------------
      integer  i, j0, j0r, j1r, indiag, k, j1, lenc, lenr 
c
      nzmaxc = 0
      nzminc = n
      nzmaxr = 0
      nzminr = n
      nzcol  = 0
      nzrow  = 0
c-----------------------------------------------------------------------
      do 10 i = 1, n
         j0 = ia(i)
         j1 = ia(i+1) 
         j0r = iao(i)
         j1r = iao(i+1)
         indiag = 0
         do 20 k=j0, j1-1 
            if (ja(k) .eq. i) indiag = 1
 20      continue
c         
         lenc = j1-j0
         lenr = j1r-j0r
c         
         if (sym) lenc = lenc + lenr - indiag
         if (lenc .le. 0) nzcol = nzcol +1
         nzmaxc = max0(nzmaxc,lenc)
         nzminc = min0(nzminc,lenc)
         if (lenr .le. 0) nzrow = nzrow+1
         nzmaxr = max0(nzmaxr,lenr)
         nzminr = min0(nzminr,lenr)
 10   continue
      return
      end
c-----------------------------------------------------------------------
      subroutine nonz_lud(n,ja,ia,nlower, nupper, ndiag)
      implicit real*8 (a-h, o-z)
      integer n,  ja(*), ia(n+1)
      integer nlower, nupper, ndiag
c-----------------------------------------------------------------------
c this routine computes the number of nonzero elements in strict lower
c part, strict upper part, and main diagonal.
c-----------------------------------------------------------------------
c
c On entry :
c-----------
c n     = integer. column dimension of matrix
c ja    = integer array containing the row indices of elements in a
c ia    = integer array containing of length n+1 containing the
c         pointers to the beginning of the columns in arrays a and ja.
c         It is assumed that ia(*) = 1 and ia(n+1) = nzz+1.
c on return
c----------
c nlower= number of nonzero elements in strict lower part
c nupper= number of nonzero elements in strict upper part
c ndiag = number of nonzero elements in main diagonal
c------------------------------------------------------------------- 
c
c number of nonzero elements in upper part
c
      nupper = 0
      ndiag = 0
      
      do 3 i=1,n
c     indiag = nonzero diagonal element indicator
         do 31 k=ia(i), ia(i+1)-1
            j=ja(k)
            if (j .lt. i) nupper = nupper+1
            if (j .eq. i) ndiag = ndiag + 1 
 31      continue
 3    continue
      nlower = ia(n+1)-1-nupper-ndiag
      return
      end
C **********************************************************************
      subroutine NUMINI(n, lpw)
c.......................................................................
      dimension lpw(n)
c.......................................................................
c
c     The vector LPW is initialized as the identity.
c
c.......................................................................
c     Laura C. Dutto - Novembre 1993
c.......................................................................
C$DOACROSS if(n .gt. 250), local(i)
      do i=1,n
         lpw(i) = i
      enddo
c
      return
      end
c-----------------------------------------------------------------------
      subroutine perphn(n,ja,ia,init,mask,maskval,nlev,riord,levels) 
      implicit none
      integer n,ja(*),ia(*),init,mask(*),maskval,
     *     nlev,riord(*),levels(*)
c-----------------------------------------------------------------------
c     finds a peripheral node and does a BFS search from it. 
c-----------------------------------------------------------------------
c     see routine  dblstr for description of parameters
c input:
c-------
c ja, ia  = list pointer array for the adjacency graph
c mask    = array used for masking nodes -- see maskval
c maskval = value to be checked against for determing whether or
c           not a node is masked. If mask(k) .ne. maskval then
c           node k is not considered.
c init    = init node in the pseudo-peripheral node algorithm.
c
c output:
c-------
c init    = actual pseudo-peripherial node found.
c nlev    = number of levels in the final BFS traversal.
c riord   =
c levels  =
c----------------------------------------------------------------------- 
      integer j,nlevp,deg,nfirst,mindeg,nod,maskdeg
      integer iperm(1) 
      nlevp = 0 
 1    continue
      riord(1) = init
      nfirst = 1 
      iperm(1) = 0
c
      call BFS(n,ja,ia,nfirst,iperm,mask,maskval,riord,levels,nlev)
      if (nlev .gt. nlevp) then 
         mindeg = n+1 
         do j=levels(nlev),levels(nlev+1)-1
            nod = riord(j) 
            deg = maskdeg(ja,ia,nod,mask,maskval)
            if (deg .lt. mindeg) then
               init = nod
               mindeg = deg
            endif 
         enddo
         nlevp = nlev 
         goto 1 
      endif
      return
      end
c-----------------------------------------------------------------------
       subroutine pgmres(n, im, rhs, sol, vv, eps, maxits, iout,
     *                    aa, ja, ia, alu, jlu, ju, ierr)
c-----------------------------------------------------------------------
       implicit real*8 (a-h,o-z)
       integer n, im, maxits, iout, ierr, ja(*), ia(n+1), jlu(*), ju(n)
       real*8 vv(n,*), rhs(n), sol(n), aa(*), alu(*), eps
c----------------------------------------------------------------------*
c                                                                      *
c                 *** ILUT - Preconditioned GMRES ***                  *
c                                                                      *
c----------------------------------------------------------------------*
c This is a simple version of the ILUT preconditioned GMRES algorithm. *
c The ILUT preconditioner uses a dual strategy for dropping elements   *
c instead  of the usual level of-fill-in approach. See details in ILUT *
c subroutine documentation. PGMRES uses the L and U matrices generated *
c from the subroutine ILUT to precondition the GMRES algorithm.        *
c The preconditioning is applied to the right. The stopping criterion  *
c utilized is based simply on reducing the residual norm by epsilon.   *
c This preconditioning is more reliable than ilu0 but requires more    *
c storage. It seems to be much less prone to difficulties related to   *
c strong nonsymmetries in the matrix. We recommend using a nonzero tol *
c (tol=.005 or .001 usually give good results) in ILUT. Use a large    *
c lfil whenever possible (e.g. lfil = 5 to 10). The higher lfil the    *
c more reliable the code is. Efficiency may also be much improved.     *
c Note that lfil=n and tol=0.0 in ILUT  will yield the same factors as *
c Gaussian elimination without pivoting.                               *
c                                                                      *
c ILU(0) and MILU(0) are also provided for comparison purposes         *
c USAGE: first call ILUT or ILU0 or MILU0 to set up preconditioner and *
c then call pgmres.                                                    *
c----------------------------------------------------------------------*
c Coded by Y. Saad - This version dated May, 7, 1990.                  *
c----------------------------------------------------------------------*
c parameters                                                           *
c-----------                                                           *
c on entry:                                                            *
c==========                                                            *
c                                                                      *
c n     == integer. The dimension of the matrix.                       *
c im    == size of krylov subspace:  should not exceed 50 in this      *
c          version (can be reset by changing parameter command for     *
c          kmax below)                                                 *
c rhs   == real vector of length n containing the right hand side.     *
c          Destroyed on return.                                        *
c sol   == real vector of length n containing an initial guess to the  *
c          solution on input. approximate solution on output           *
c eps   == tolerance for stopping criterion. process is stopped        *
c          as soon as ( ||.|| is the euclidean norm):                  *
c          || current residual||/||initial residual|| <= eps           *
c maxits== maximum number of iterations allowed                        *
c iout  == output unit number number for printing intermediate results *
c          if (iout .le. 0) nothing is printed out.                    *
c                                                                      *
c aa, ja,                                                              *
c ia    == the input matrix in compressed sparse row format:           *
c          aa(1:nnz)  = nonzero elements of A stored row-wise in order *
c          ja(1:nnz) = corresponding column indices.                   *
c          ia(1:n+1) = pointer to beginning of each row in aa and ja.  *
c          here nnz = number of nonzero elements in A = ia(n+1)-ia(1)  *
c                                                                      *
c alu,jlu== A matrix stored in Modified Sparse Row format containing   *
c           the L and U factors, as computed by subroutine ilut.       *
c                                                                      *
c ju     == integer array of length n containing the pointers to       *
c           the beginning of each row of U in alu, jlu as computed     *
c           by subroutine ILUT.                                        *
c                                                                      *
c on return:                                                           *
c==========                                                            *
c sol   == contains an approximate solution (upon successful return).  *
c ierr  == integer. Error message with the following meaning.          *
c          ierr = 0 --> successful return.                             *
c          ierr = 1 --> convergence not achieved in itmax iterations.  *
c          ierr =-1 --> the initial guess seems to be the exact        *
c                       solution (initial residual computed was zero)  *
c                                                                      *
c----------------------------------------------------------------------*
c                                                                      *
c work arrays:                                                         *
c=============                                                         *
c vv    == work array of length  n x (im+1) (used to store the Arnoli  *
c          basis)                                                      *
c----------------------------------------------------------------------*
c subroutines called :                                                 *
c amux   : SPARSKIT routine to do the matrix by vector multiplication  *
c          delivers y=Ax, given x  -- see SPARSKIT/BLASSM/amux         *
c lusol : combined forward and backward solves (Preconditioning ope.) *
c BLAS1  routines.                                                     *
c----------------------------------------------------------------------*
       parameter (kmax=50)
       real*8 hh(kmax+1,kmax), c(kmax), s(kmax), rs(kmax+1),t
c-------------------------------------------------------------
c arnoldi size should not exceed kmax=50 in this version..
c to reset modify paramter kmax accordingly.
c-------------------------------------------------------------
       data epsmac/1.d-16/
       n1 = n + 1
       its = 0
c-------------------------------------------------------------
c outer loop starts here..
c-------------- compute initial residual vector --------------
       call amux (n, sol, vv, aa, ja, ia)
       do 21 j=1,n
          vv(j,1) = rhs(j) - vv(j,1)
 21    continue
c-------------------------------------------------------------
 20    ro = dnrm2(n, vv, 1)
       if (iout .gt. 0 .and. its .eq. 0)
     *      write(iout, 199) its, ro
       if (ro .eq. 0.0d0) goto 999
       t = 1.0d0/ ro
       do 210 j=1, n
          vv(j,1) = vv(j,1)*t
 210   continue
       if (its .eq. 0) eps1=eps*ro
c     ** initialize 1-st term  of rhs of hessenberg system..
       rs(1) = ro
       i = 0
 4     i=i+1
       its = its + 1
       i1 = i + 1
       call lusol (n, vv(1,i), rhs, alu, jlu, ju)
       call amux (n, rhs, vv(1,i1), aa, ja, ia)
c-----------------------------------------
c     modified gram - schmidt...
c-----------------------------------------
       do 55 j=1, i
          t = ddot(n, vv(1,j),1,vv(1,i1),1)
          hh(j,i) = t
          call daxpy(n, -t, vv(1,j), 1, vv(1,i1), 1)
 55    continue
       t = dnrm2(n, vv(1,i1), 1)
       hh(i1,i) = t
       if ( t .eq. 0.0d0) goto 58
       t = 1.0d0/t
       do 57  k=1,n
          vv(k,i1) = vv(k,i1)*t
 57    continue
c
c     done with modified gram schimd and arnoldi step..
c     now  update factorization of hh
c
 58    if (i .eq. 1) goto 121
c--------perfrom previous transformations  on i-th column of h
       do 66 k=2,i
          k1 = k-1
          t = hh(k1,i)
          hh(k1,i) = c(k1)*t + s(k1)*hh(k,i)
          hh(k,i) = -s(k1)*t + c(k1)*hh(k,i)
 66    continue
 121   gam = sqrt(hh(i,i)**2 + hh(i1,i)**2)
c
c     if gamma is zero then any small value will do...
c     will affect only residual estimate
c
       if (gam .eq. 0.0d0) gam = epsmac
c
c     get  next plane rotation
c
       c(i) = hh(i,i)/gam
       s(i) = hh(i1,i)/gam
       rs(i1) = -s(i)*rs(i)
       rs(i) =  c(i)*rs(i)
c
c     detrermine residual norm and test for convergence-
c
       hh(i,i) = c(i)*hh(i,i) + s(i)*hh(i1,i)
       ro = abs(rs(i1))
 131   format(1h ,2e14.4)
       if (iout .gt. 0)
     *      write(iout, 199) its, ro
       if (i .lt. im .and. (ro .gt. eps1))  goto 4
c
c     now compute solution. first solve upper triangular system.
c
       rs(i) = rs(i)/hh(i,i)
       do 30 ii=2,i
          k=i-ii+1
          k1 = k+1
          t=rs(k)
          do 40 j=k1,i
             t = t-hh(k,j)*rs(j)
 40       continue
          rs(k) = t/hh(k,k)
 30    continue
c
c     form linear combination of v(*,i)'s to get solution
c
       t = rs(1)
       do 15 k=1, n
          rhs(k) = vv(k,1)*t
 15    continue
       do 16 j=2, i
          t = rs(j)
          do 161 k=1, n
             rhs(k) = rhs(k)+t*vv(k,j)
 161      continue
 16    continue
c
c     call preconditioner.
c
       call lusol (n, rhs, rhs, alu, jlu, ju)
       do 17 k=1, n
          sol(k) = sol(k) + rhs(k)
 17    continue
c
c     restart outer loop  when necessary
c
       if (ro .le. eps1) goto 990
       if (its .ge. maxits) goto 991
c
c     else compute residual vector and continue..
c
       do 24 j=1,i
          jj = i1-j+1
          rs(jj-1) = -s(jj-1)*rs(jj)
          rs(jj) = c(jj-1)*rs(jj)
 24    continue
       do 25  j=1,i1
          t = rs(j)
          if (j .eq. 1)  t = t-1.0d0
          call daxpy (n, t, vv(1,j), 1,  vv, 1)
 25    continue
 199   format('   its =', i4, ' res. norm =', d20.6)
c     restart outer loop.
       goto 20
 990   ierr = 0
       return
 991   ierr = 1
       return
 999   continue
       ierr = -1
       return
c-----------------end of pgmres ---------------------------------------
c-----------------------------------------------------------------------
       end
c----------end-of-phipro------------------------------------------------
c-----------------------------------------------------------------------
      subroutine phihes (n,m,dt,eps,u,job,z,wkc,beta,errst,hh,ih,
     *                   x, y, indic,ierr) 
c     implicit  real*8 (a-h,o-z)
      integer n, m, job, ih, indic, ierr
      real*8 hh(ih+2,m+1), u(n,m+1), z(m+1), x(n), y(n)
      complex*16 wkc(m+1) 
      real*8 dt, eps, beta, errst
c-----------------------------------------------------------------------
c this subroutine computes the Arnoldi basis Vm and the corresponding 
c coeffcient vector ym in the approximation 
c 
c        	w  ::= beta  Vm  ym 
c               where ym = phi(- Hm * dt) * e1
c
c to the vector phi(-A * dt) w where A is an arbitary matrix and 
c w is a given input vector. The phi function is defined   by
c               phi(z) = (1 - exp(z) ) / z  
c 
c In case job .lt.0 the arnoldi basis is recomputed. Otherwise the
c code assumes assumes that  u(*) contains an already computed 
c arnoldi basis and computes only the y-vector (which is stored in
c v(*)). Three different options are available through the argument job. 
c-----------------------------------------------------------------------
c on entry:
c---------- 
c n	= dimension of matrix
c
c m	= dimension of Krylov subspace (= degree of polynomial 
c         approximation to the phionential used. )
c
c dt	= scalar by which to multiply matrix. Can be viewed
c         as a time step. dt must be positive [to be fixed].
c
c eps   = scalar indicating the relative error tolerated for the result. 
c         the code will try to compute an answer such that 
c         norm2(exactanswer-approximation) / norm2(w) .le. eps 
c
c u	= work array of size n*(m+1) to contain the Arnoldi basis
c
c w	= real array of length n = input vector to  which phi(-A) is
c         to be applied. 
c
c job	= integer. job indicator. If job .lt.  0 then the Arnoldi
c         basis is recomputed. If job .gt. 0 then it is assumed
c         that the user wants to use a previously computed Krylov
c         subspace but a different dt. Thus the Arnoldi basis and
c         the Hessenberg matrix Hm are not recomputed. 
c	  In that case the user should not modify the values of beta
c         and the matrices hh and u(n,*) when recalling phipro. 
c         job = -1 : recompute basis and get an initial estimate for 
c                    time step dt to be used.
c         job = 0  : recompute basis and do not alter dt.
c         job = 1  : do not recompute arnoldi basis. 
c
c z     = real work array of  size (m+1)
c wkc   = complex*16 work array of size (m+1) 
c
c hh    = work array of size size at least (m+2)*(m+1)
c
c ih+2	= first dimension of hh as declared in the calling program.
c         ih must be .ge. m.
c 
c-----------------------------------------------------------------------
c on return:
c-----------
c w2	= resulting vector w2 = phi(-A *dt) * w
c beta  = real equal to the 2-norm of w. Needed if phipro will
c         be recalled with the same Krylov subspace and a different dt. 
c errst = rough estimates of the 2-norm of the error.
c hh	= work array of dimension at least (m+2) x (m+1) 
c
c----------------------------------------------------------------------- 
c local variables 
c 
      integer ndmax
      parameter (ndmax=20) 
      real*8 alp0, fnorm, t, rm, ddot, dabs, dsqrt, dsign,dble
      complex*16 alp(ndmax+1), rd(ndmax+1)
      integer i, j, k, ldg, i0, i1, m1 
      logical verboz
      data verboz/.true./
      save
c------use degree 14 chebyshev all the time -------------------------- 
      if (indic .eq. 3) goto 60
c
c------get partial fraction expansion of rational function -----------
c-----------------------------------------------------------------------
c chebyshev (14,14) 
c      ldg= 7
c      alp0 =  0.183216998528140087E-11
c      alp(1)=( 0.557503973136501826E+02,-0.204295038779771857E+03)
c      rd(1)=(-0.562314417475317895E+01, 0.119406921611247440E+01)
c      alp(2)=(-0.938666838877006739E+02, 0.912874896775456363E+02)
c      rd(2)=(-0.508934679728216110E+01, 0.358882439228376881E+01)
c      alp(3)=( 0.469965415550370835E+02,-0.116167609985818103E+02)
c      rd(3)=(-0.399337136365302569E+01, 0.600483209099604664E+01)
c      alp(4)=(-0.961424200626061065E+01,-0.264195613880262669E+01)
c      rd(4)=(-0.226978543095856366E+01, 0.846173881758693369E+01)
c      alp(5)=( 0.752722063978321642E+00, 0.670367365566377770E+00)
c      rd(5)=( 0.208756929753827868E+00, 0.109912615662209418E+02)
c      alp(6)=(-0.188781253158648576E-01,-0.343696176445802414E-01)
c      rd(6)=( 0.370327340957595652E+01, 0.136563731924991884E+02)
c      alp(7)=( 0.143086431411801849E-03, 0.287221133228814096E-03)
c      rd(7)=( 0.889777151877331107E+01, 0.166309842834712071E+02)
c-----------------------------------------------------------------------
c Pade of  degree =  (4,4) 
c
c        ldg= 2
c        alp(1)=(-0.132639894655051648E+03,-0.346517448171383875E+03)
c        rd(1)=(-0.579242120564063611E+01, 0.173446825786912484E+01)
c        alp(2)=( 0.926398946550511936E+02, 0.337809095284865179E+02)
c        rd(2)=(-0.420757879435933546E+01, 0.531483608371348736E+01)
c
c Pade of degree =  8
c
        ldg= 4
        alp(1)=( 0.293453004361944040E+05, 0.261671093076405813E+05)
        rd(1)=(-0.104096815812822569E+02, 0.523235030527069966E+01)
        alp(2)=(-0.212876889060526154E+05,-0.764943398790569044E+05)
        rd(2)=(-0.111757720865218743E+02, 0.173522889073929320E+01)
        alp(3)=(-0.853199767523084301E+04,-0.439758928252937039E+03)
        rd(3)=(-0.873657843439934822E+01, 0.882888500094418304E+01)
        alp(4)=( 0.330386145089576530E+03,-0.438315990671386316E+03)
        rd(4)=(-0.567796789779646360E+01, 0.127078225972105656E+02)
c
      do 102 k=1, ldg
         alp(k) = - alp(k) / rd(k)
 102  continue
      alp0 = 0.0d0
c     
c     if job .gt. 0 skip arnoldi process:
c     
      if (job .gt. 0) goto 2
c------normalize vector u and put in first column of u -- 
      beta = dsqrt(ddot(n,u,1,u,1))
c-----------------------------------------------------------------------
      if(verboz) print *, ' In PHIHES, beta ', beta 
      if (beta .eq. 0.0d0) then
         ierr = -1
         indic = 1
         return
      endif 
c
      t = 1.0d0/beta 
      do 25 j=1, n
         u(j,1) = u(j,1)*t 
 25   continue
c------------------Arnoldi loop ----------------------------------------- 
c      fnorm = 0.0d0
      i1 = 1
 58   i = i1
      i1 = i + 1
      do 59 k=1, n
         x(k) = u(k,i)
 59   continue
      indic = 3
      return
 60   continue
      do 61 k=1, n
         u(k,i1) = y(k) 
 61   continue
      i0 =1
c switch  for Lanczos version 
c     i0 = max0(1, i-1)
      call mgsr (n, i0, i1, u, hh(1,i))
      fnorm = fnorm + ddot(i1, hh(1,i),1, hh(1,i),1)
      if (hh(i1,i) .eq. 0.0) m = i
      if  (i .lt. m) goto 58
c--------------done with arnoldi loop ---------------------------------
      rm = dble(m) 
      fnorm = dsqrt( fnorm / rm )
c------- put beta*e1 into z ------------------------------------------- 
      m1 = m+1 
      do 4 i=1,m1
         hh(i,m1) = 0.0
 4    continue
c     
c     compute initial dt when  job .lt. 1 
c
      if (job .ge. 0) goto 2
c
      t = 2.0*eps 
      do 41 k=1, m
         t = 2.0*t*dble(k+1)/rm
 41   continue
c
      t = rm* (t**(1.0d0/rm) ) / fnorm 
      if(verboz) print *, ' t, dt = ', t, dt
      t = dmin1(dabs(dt),t) 
      dt = dsign(t, dt) 
c---------------------- get the vector phi(Hm)e_1 + estimate ----------- 
 2    continue 
      z(1) = beta 
      do 3 k=2, m1
         z(k) = 0.0d0 
 3    continue
c-------get  : phi(H) * beta*e1
      call hes(ldg,m1,hh,ih,dt,z,rd,alp,alp0,wkc)
c-------error estimate 
      errst = dabs(z(m1))
      if(verboz) print *, ' error estimate =', errst 
c----------------------------------------------------------------------- 
      indic = 2
      return
      end
      subroutine phiprod (n, m, eps, tn, u, w, r, x, y, a, ioff, ndiag)
      real*8 eps, tn 
      real*8 a(n,ndiag), u(n,m+1), w(n), r(n), x(n), y(n)
      integer n, m, ndiag, ioff(ndiag)

c-----------------------------------------------------------------------
c this subroutine computes an approximation to the vector
c
c   	w(tn) = w(t0) + tn *  phi( - A * tn ) * (r - A w(t0))
c
c where phi(z) = (1-exp(z)) / z
c 
c i.e. solves   dw/dt = - A w + r in [t0,t0+ tn] (returns only w(t0+tn))
c 
c for matrices stored in diagonal (DIA) format.
c
c this routine constitutes an interface for the routine phipro for
c matrices stored in diagonal (DIA) format. The phipro routine uses
c reverse communication and as a result does not depend on any
c data structure of the matrix.

c-----------------------------------------------------------------------
c ARGUMENTS
c---------- 
c see phipro for meaning of parameters n, m, eps, tn, u, w, x, y.
c 
c a, ioff, and ndiag are the arguments of the matrix:
c
c a(n,ndiag) = a rectangular array with a(*,k) containing the diagonal 
c              offset by ioff(k) (negative or positive or zero), i.e., 
c              a(i,jdiag) contains the element A(i,i+ioff(jdiag)) in 
c              the usual dense storage scheme.
c 
c ioff	     = integer array containing the offsets  of the ndiag diagonals
c ndiag      = integer. the number of diagonals.
c 
c-----------------------------------------------------------------------
c local variables 
c 
      integer indic, ierr

      indic = 0
 101  continue
      call phipro (n, m, eps, tn, w, r, u, x, y, indic, ierr)
      if (indic .eq. 1) goto 102
c     
c     matrix vector-product for diagonal storage --
c     
      call oped(n, x, y, a, ioff, ndiag)
      goto 101
 102  continue
      return
      end
c----------end-of-phiprod----------------------------------------------- 
c-----------------------------------------------------------------------
      subroutine phipro (n, m, eps, tn, w, r, u, x, y, indic, ierr)
c     implicit  real*8 (a-h,o-z)
      integer n, m, indic, ierr
      real*8 eps, tn, w(n), r(n), u(n,m+1), x(n), y(n) 
c-----------------------------------------------------------------------
c
c this subroutine computes an approximation to the vector
c
c     w(tn) = w(t0) + tn *  phi( - A * tn ) * (r - A w(t0))
c     where phi(z) = (1-exp(z)) / z
c 
c     i.e. solves dw/dt=-Aw+r in [t0,t0+tn] (returns w(t0+tn))
c     t0 need not be known.
c 
c note that for w(t0)=0 the answer is    w=tn *phi(-tn * A) r
c in other words this allows to compute phi(A tn) v. 
c This code will work well only for cases where eigenvalues are
c real (or nearly real) and positive. It has also been coded to 
c work for cases where tn .lt. 0.0 (and A has real negative spectrum)
c 
c----------------------------------------------------------------------- 
c
c THIS IS A REVERSE COMMUNICATION IMPLEMENTATION. 
c------------------------------------------------- 
c USAGE: (see also comments on argument indic below).
c------ 
c
c      indic = 0
c 1    continue
c      call phipro (n, m, eps, tn, u, w, x, y, indic)
c      if (indic .eq. 1) goto 2 <-- indic .eq. 1 means phipro has finished
c      call matvec(n, x, y)     <--- user's matrix-vec. product
c                                    with x = input vector, and
c                                     y = result = A * x.
c      goto 1
c 2    continue
c      .....
c
c-----------------------------------------------------------------------
c
c en entry:
c---------- 
c n	= dimension of matrix
c
c m	= dimension of Krylov subspace (= degree of polynomial 
c         approximation to the exponential used. )
c
c eps   = scalar indicating the relative error tolerated for the result. 
c         the code will try to compute an answer such that 
c         norm2(exactanswer-approximation) / norm2(w) .le. eps 
c
c tn	= scalar by which to multiply matrix. (may be .lt. 0)
c         the code will compute a solution to dw/dt = -A w + r,
c         and overwrite the result w(tn) onto in w.
c
c w	= real array of length n. Initial condition for the ODE system
c         on input, result w(tn) on output (input and output argument) 
c
c r     = real array of length n. the constant term in the system 
c         dw/dt = -A w + r to be solved.
c
c u	= work array of size n*(m+1) (used to hold the Arnoldi basis )
c
c x, y  = two real work vectors of length n each. x and y are used to
c         carry the input and output vectors for the matrix-vector
c         products y=Ax in the reverse communication protocole.
c         see argument indic (return) below for details on their usage.
c
c indic = integer used as indicator for the reverse communication.
c         in the first call enter indic = 0.
c
c ierr  = error indicator. 
c         ierr = 1 means phipro was called with indic=1 (not allowed)
c         ierr = -1 means that the input is zero the solution has been 
c         unchanged.
c 
c on return:
c-----------
c 
c w     = contains the result w(tn)=w(t0)+tn*phi(-A*tn)*(r-Aw(t0))
c         when phipro has finished (as indicated by indic see below)
c
c indic = indicator for the reverse communication protocole.
c       * INDIC .eq. 1  means that phipro has finished and w contains the
c         result. 
c       * INDIC .gt. 1 means that phipro has not finished and that 
c         it is requesting another matrix vector product before
c         continuing. The user must compute Ax where A is the matrix
c         and x is the vector provided by phipro and return the 
c         result in y. Then phipro must be called again without
c         changing any other argument. typically this is best 
c         implemented in a loop with phipro being called as long
c         indic is returned with a value .ne. 1.
c 
c NOTES:  m should not exceed 60 in this version  (see mmax below)
c-----------------------------------------------------------------------
c local variables 
c 
      integer mmax
      parameter (mmax=60) 
      real*8 errst, tcur, told, dtl, beta, red, dabs, dble
      real*8 hh(mmax+2,mmax+1), z(mmax+1)
      complex*16   wkc(mmax+1) 
      integer ih, k, job
      logical verboz
      data verboz/.true./
      save
c-----------------------------------------------------------------------
c indic = 4  means  getting y=Ax needed in phipro 
c indic = 3  means  passing through only with result of y= Ax to phihes
c indic = 2  means phihes has finished its job
c indic = 1  means phipro has finished its job (real end)/
c-----------------------------------------------------------------------
      ierr = 0 
      if (indic .eq. 3) goto 101 
      if (indic .eq. 4) goto 11
      if (indic .eq. 1) then 
         ierr = 1
         return
      endif
c----- 
      ih = mmax 
      m  = min0(m,mmax) 
      tcur = 0.0d0
      dtl = tn - tcur
      job = -1
c-------------------- outer loop ----------------------------- 
 100  continue
      if(verboz) print *,'In PHIPRO, current time = ', tcur ,'---------'
c------------------------------------------------------------- 
c ---- call phionential propagator --------------------------- 
c------------------------------------------------------------- 
      told = tcur 
c
c     if (told + dtl .gt. tn) dtl = tn-told
c      construct initial vector for Arnoldi:  r - A w(old) 
c
      do 10 k=1, n
         x(k) = w(k)
 10   continue
      indic = 4
      return
 11   continue
      do 12 k=1, n
         u(k,1) = r(k) - y(k)
 12   continue
c
 101  continue
      call phihes (n,m,dtl,eps,u,job,z,wkc,beta,errst,hh,ih,x, y,indic,
     *            ierr) 
c-----------------------------------------------------------------------
      if (ierr .ne. 0) return
      if (indic .eq. 3) return
      tcur = told + dtl 
      if(verboz) print *, ' tcur now = ', tcur, ' dtl = ', dtl
c
c     relative error 
c      if(verboz) print *, ' beta', beta
      errst = errst / beta
c---------
      if ((errst .le. eps) .and. ( (errst .gt. eps/100.0) .or.
     *     (tcur .eq. tn))) goto 102
c
c     use approximation :  [ new err ] = fact**m  * [cur. error]
c 
      red =  (0.5*eps / errst)**(1.0d0 /dble(m) ) 
      dtl = dtl*red
      if (dabs(told+dtl) .gt. dabs(tn) )  dtl = tn-told
      if(verboz) print *, ' red =',red,' , reducing dt to: ', dtl
c-------
      job = 1 
      goto 101
c-------
 102  continue 
c
      call project(n, m, w, dtl, u, z) 
c never go beyond tcur
      job = 0
      dtl = dmin1(dtl, tn-tcur)
      if (dabs(tcur+dtl) .gt. dabs(tn)) dtl = tn-tcur 
      if (dabs(tcur) .lt. dabs(tn)) goto 100
      indic = 1
      return
      end
c----------------------------------------------------------------------- 
      subroutine pltmt (nrow,ncol,mode,ja,ia,title,key,type,
     1     job, iounit)
c-----------------------------------------------------------------------
c this subroutine creates a 'pic' file for plotting the pattern of 
c a sparse matrix stored in general sparse format. it is not intended
c to be a means of plotting large matrices (it is very inefficient).
c It is however useful for small matrices and can be used for example
c for inserting matrix plots in a text. The size of the plot can be
c 7in x 7in or 5 in x 5in .. There is also an option for writing a 
c 3-line header in troff (see description of parameter job).
c Author: Youcef Saad - Date: Sept., 1989
c See SPARSKIT/UNSUPP/ for a version of this to produce a post-script
c file. 
c-----------------------------------------------------------------------
c nrow   = number of rows in matrix
c
c ncol	 = number of columns in matrix 
c
c mode   = integer indicating whether the matrix is stored
c          row-wise (mode = 0) or column-wise (mode=1)
c
c ja     = column indices of nonzero elements when matrix is
c	   stored rowise. Row indices if stores column-wise.
c ia     = integer array of containing the pointers to the 
c	   beginning of the columns in arrays a, ja.
c
c title  = character*71 = title of matrix test ( character a*71 ).
c key    = character*8  = key of matrix 
c type   = character*3  = type of matrix. 
c
c job    = this integer parameter allows to set a few minor 
c          options. First it tells pltmt whether or not to
c          reduce the plot. The standard size of 7in is then
c          replaced by a 5in plot. It also tells pltmt whether or
c          not to append to the pic file a few 'troff' lines that 
c          produce a centered caption includingg the title, key and 
c          types as well as the size and number of nonzero elements.
c          job =  0 : do not reduce and do not make caption.
c          job =  1 : reduce and do not make caption.
c          job = 10 : do not reduce and make caption
c          job = 11 : reduce and make caption.
c          (i.e. trailing digit for reduction, leading digit for caption)
c
c iounit = logical unit number where to write the matrix into.
c
c-----------------------------------------------------------------------
c example of usage . 
c-----------------
c In the fortran code:
c  a) read a Harwell/Boeing matrix
c          call readmt (.....)
c	   iout = 13
c  b) generate pic file:
c          call  pltmt (nrow,ncol,mode,ja,ia,title,key,type,iout)
c	   stop
c ---------
c Then in a unix environment plot the matrix by the command
c
c	pic FOR013.DAT | troff -me | lpr -Ppsx
c
c-----------------------------------------------------------------------
c notes: 1) Plots square as well as rectangular matrices.
c            (however not as much tested with rectangular matrices.)
c	  2) the dot-size is adapted according to the size of the
c            matrix.
c	  3) This is not meant at all as a way of plotting large
c            matrices. The pic file generaled will have one line for
c            each nonzero element. It is  only meant for use in
c	     such things as document poreparations etc..
c         4) The caption written will print the 71 character long
c            title. This may not be centered correctly if the
c            title has trailing blanks (a problem with Troff).
c            if you want the title centered then you can center
c            the string in title before calling pltmt. 
c       
c-----------------------------------------------------------------------
      integer ja(*), ia(*)
      character key*8,title*72,type*3
      real x, y
c-------
      n = ncol
      if (mode .eq. 0) n = nrow
      nnz = ia(n+1) - ia(1) 
      maxdim = max0 (nrow, ncol)
      xnrow = real(nrow)
      ptsize = 0.08
      hscale = (7.0 -2.0*ptsize)/real(maxdim-1) 
      vscale = hscale 
      xwid  = ptsize + real(ncol-1)*hscale + ptsize
      xht   = ptsize + real(nrow-1)*vscale + ptsize
      xshift = (7.0-xwid)/2.0
      yshift = (7.0-xht)/2.0 
c------
      if (mod(job,10) .eq. 1) then
         write (iounit,88)
      else
         write (iounit,89)
      endif
 88   format('.PS 5in',/,'.po 1.8i')
 89   format('.PS',/,'.po 0.7i')
      write(iounit,90) 
 90   format('box invisible wid 7.0 ht 7.0 with .sw at (0.0,0.0) ') 
      write(iounit,91) xwid, xht, xshift, yshift
 91   format('box wid ',f5.2,' ht ',f5.2,
     *     ' with .sw at (',f5.2,',',f5.2,')' )
c     
c     shift points slightly to account for size of dot , etc..
c     
      tiny = 0.03
      if (mod(job,10) .eq. 1) tiny = 0.05
      xshift = xshift + ptsize - tiny
      yshift = yshift + ptsize + tiny
c     
c-----------------------------------------------------------------------
c     
      ips = 8
      if (maxdim .le. 500) ips = 10
      if (maxdim .le. 300) ips = 12
      if (maxdim .le. 100) ips = 16
      if (maxdim .lt. 50) ips = 24
      write(iounit,92) ips
 92   format ('.ps ',i2)
c     
c-----------plottingloop --------------------------------------------- 
c     
      do 1 ii=1, n
         istart = ia(ii)
         ilast  = ia(ii+1)-1 
         if (mode .ne. 0) then
            x = real(ii-1)
            do 2 k=istart, ilast
               y = xnrow-real(ja(k))
               write(iounit,128) xshift+x*hscale, yshift+y*vscale
 2          continue 
         else
            y = xnrow - real(ii)
            do 3 k=istart, ilast
               x = real(ja(k)-1)
               write(iounit,128) xshift+x*hscale, yshift+y*vscale
 3          continue	    
         endif
 1    continue
c-----------------------------------------------------------------------
 128  format(7h"." at ,f6.3,1h,,f6.3,8h ljust  )
      write (iounit, 129)
 129  format('.PE')
c     quit if caption not desired. 
      if ( (job/10) .ne. 1)  return
c     
      write(iounit,127) key, type, title
      write(iounit,130) nrow,ncol,nnz
 127  format('.sp 4'/'.ll 7i'/'.ps 12'/'.po 0.7i'/'.ce 3'/,
     *     'Matrix:  ',a8,',  Type:  ',a3,/,a72)
 130  format('Dimension: ',i4,' x ',i4,',  Nonzero elements: ',i5)
      return
c----------------end-of-pltmt ------------------------------------------
c----------------------------------------------------------------------- 
      end
c----------end-of-mgsr--------------------------------------------------
c-----------------------------------------------------------------------
      subroutine project(n, m, w, t, u, v) 
      integer n, m
      real*8 u(n,m), v(m), w(n), t, scal
c
c     computes the vector w = w + t * u * v
c
c local variables 
c
      integer j, k

      do 100 j=1,m
         scal = t*v(j) 
         do 99 k=1,n
            w(k) = w(k) + scal*u(k,j) 
 99      continue
 100  continue
      return
      end
c-----------------------------------------------------------------------
      subroutine prtmt (nrow,ncol,a,ja,ia,rhs,guesol,title,key,type,
     1     ifmt,job,iounit)
c-----------------------------------------------------------------------
c writes a matrix in Harwell-Boeing format into a file.
c assumes that the matrix is stored in COMPRESSED SPARSE COLUMN FORMAT.
c some limited functionality for right hand sides. 
c Author: Youcef Saad - Date: Sept., 1989 - updated Oct. 31, 1989 to
c cope with new format. 
c-----------------------------------------------------------------------
c on entry:
c---------
c nrow   = number of rows in matrix
c ncol	 = number of columns in matrix 
c a	 = real*8 array containing the values of the matrix stored 
c          columnwise
c ja 	 = integer array of the same length as a containing the column
c          indices of the corresponding matrix elements of array a.
c ia     = integer array of containing the pointers to the beginning of 
c	   the row in arrays a and ja.
c rhs    = real array  containing the right-hand-side (s) and optionally
c          the associated initial guesses and/or exact solutions
c          in this order. See also guesol for details. the vector rhs will
c          be used only if job .gt. 2 (see below). Only full storage for
c          the right hand sides is supported. 
c
c guesol = a 2-character string indicating whether an initial guess 
c          (1-st character) and / or the exact solution (2-nd)
c          character) is provided with the right hand side.
c	   if the first character of guesol is 'G' it means that an
c          an intial guess is provided for each right-hand sides. 
c          These are assumed to be appended to the right hand-sides in 
c          the array rhs.
c	   if the second character of guesol is 'X' it means that an
c          exact solution is provided for each right-hand side.
c          These are assumed to be appended to the right hand-sides 
c          and the initial guesses (if any) in the array rhs.
c
c title  = character*72 = title of matrix test ( character a*72 ).
c key    = character*8  = key of matrix 
c type   = charatcer*3  = type of matrix.
c
c ifmt	 = integer specifying the format chosen for the real values
c	   to be output (i.e., for a, and for rhs-guess-sol if 
c          applicable). The meaning of ifmt is as follows.
c	  * if (ifmt .lt. 100) then the D descriptor is used,
c           format Dd.m, in which the length (m) of the mantissa is 
c           precisely the integer ifmt (and d = ifmt+6)
c	  * if (ifmt .gt. 100) then prtmt will use the 
c           F- descriptor (format Fd.m) in which the length of the 
c           mantissa (m) is the integer mod(ifmt,100) and the length 
c           of the integer part is k=ifmt/100 (and d = k+m+2)
c	    Thus  ifmt= 4   means  D10.4  +.xxxxD+ee    while
c	          ifmt=104  means  F7.4   +x.xxxx
c	          ifmt=205  means  F9.5   +xx.xxxxx
c	    Note: formats for ja, and ia are internally computed.
c
c job	 = integer to indicate whether matrix values and
c	   a right-hand-side is available to be written
c          job = 1   write srtucture only, i.e., the arrays ja and ia.
c          job = 2   write matrix including values, i.e., a, ja, ia
c          job = 3   write matrix and one right hand side: a,ja,ia,rhs.
c	   job = nrhs+2 write matrix and nrhs successive right hand sides
c	   Note that there cannot be any right-hand-side if the matrix
c	   has no values. Also the initial guess and exact solutions when 
c          provided are for each right hand side. For example if nrhs=2 
c          and guesol='GX' there are 6 vectors to write.
c          
c
c iounit = logical unit number where to write the matrix into.
c
c on return:
c---------- 
c the matrix a, ja, ia will be written in output unit iounit
c in the Harwell-Boeing format. None of the inputs is modofied.
c  
c Notes: 1) This code attempts to pack as many elements as possible per
c        80-character line. 
c        2) this code attempts to avoid as much as possible to put
c        blanks in the formats that are written in the 4-line header
c	 (This is done for purely esthetical reasons since blanks
c        are ignored in format descriptors.)
c        3) sparse formats for right hand sides and guesses are not
c        supported.
c-----------------------------------------------------------------------
      character title*72,key*8,type*3,ptrfmt*16,indfmt*16,valfmt*20,
     1     guesol*2, rhstyp*3
      integer totcrd, ptrcrd, indcrd, valcrd, rhscrd, nrow, ncol,
     1     nnz, nrhs, len, nperli, nrwindx
      integer ja(*), ia(*) 	
      real*8 a(*),rhs(*)
c--------------
c     compute pointer format
c--------------
       nnz    = ia(ncol+1) -1
       if (nnz .eq. 0) then
	   return
	endif
      len    = int ( alog10(0.1+real(nnz+1))) + 1
      nperli = 80/len
      ptrcrd = ncol/nperli + 1
      if (len .gt. 9) then
         assign 101 to ix
      else
         assign 100 to ix
      endif
      write (ptrfmt,ix) nperli,len
 100  format(1h(,i2,1HI,i1,1h) )
 101  format(1h(,i2,1HI,i2,1h) )
c----------------------------
c compute ROW index format
c----------------------------
      len    = int ( alog10(0.1+real(nrow) )) + 1
      nperli = min0(80/len,nnz)
      indcrd = (nnz-1)/nperli+1
      write (indfmt,100) nperli,len
c---------------
c compute values and rhs format (using the same for both)
c--------------- 
      valcrd	= 0
      rhscrd  = 0
c quit this part if no values provided.
      if (job .le. 1) goto 20
c     
      if (ifmt .ge. 100) then
         ihead = ifmt/100
         ifmt = ifmt-100*ihead
         len = ihead+ifmt+2
         nperli = 80/len
c     
         if (len .le. 9 ) then
            assign 102 to ix
         elseif (ifmt .le. 9) then
            assign 103 to ix
         else 
            assign 104 to ix
         endif
c     
         write(valfmt,ix) nperli,len,ifmt
 102     format(1h(,i2,1hF,i1,1h.,i1,1h) )
 103     format(1h(,i2,1hF,i2,1h.,i1,1h) )
 104     format(1h(,i2,1hF,i2,1h.,i2,1h) )
C
      else
         len = ifmt + 6
         nperli = 80/len
c     try to minimize the blanks in the format strings.
         if (nperli .le. 9) then
	    if (len .le. 9 ) then
	       assign 105 to ix
	    elseif (ifmt .le. 9) then
	       assign 106 to ix
	    else 
	       assign 107 to ix
	    endif
	 else 
	    if (len .le. 9 ) then
	       assign 108 to ix
	    elseif (ifmt .le. 9) then
	       assign 109 to ix
	    else 
               assign 110 to ix
            endif
         endif
c-----------
         write(valfmt,ix) nperli,len,ifmt
 105     format(1h(,i1,1hD,i1,1h.,i1,1h) )
 106     format(1h(,i1,1hD,i2,1h.,i1,1h) )
 107     format(1h(,i1,1hD,i2,1h.,i2,1h) )
 108     format(1h(,i2,1hD,i1,1h.,i1,1h) )
 109     format(1h(,i2,1hD,i2,1h.,i1,1h) )
 110     format(1h(,i2,1hD,i2,1h.,i2,1h) )
c     
      endif 	    
      valcrd = (nnz-1)/nperli+1
      nrhs   = job -2
      if (nrhs .ge. 1) then
         i = (nrhs*nrow-1)/nperli+1
         rhscrd = i
         if (guesol(1:1) .eq. 'G' .or. guesol(1:1) .eq. 'g')
     +      rhscrd = rhscrd+i
         if (guesol(2:2) .eq. 'X' .or. guesol(2:2) .eq. 'x')
     +      rhscrd = rhscrd+i
         rhstyp = 'F'//guesol
      endif 
 20   continue
c     
      totcrd = ptrcrd+indcrd+valcrd+rhscrd
c     write 4-line or five line header
      write(iounit,10) title,key,totcrd,ptrcrd,indcrd,valcrd,
     1     rhscrd,type,nrow,ncol,nnz,nrhs,ptrfmt,indfmt,valfmt,valfmt
c-----------------------------------------------------------------------
      nrwindx = 0
      if (nrhs .ge. 1) write (iounit,11) rhstyp, nrhs, nrwindx
 10   format (a72, a8 / 5i14 / a3, 11x, 4i14 / 2a16, 2a20)
 11   format(A3,11x,i14,i14)
c     
      write(iounit,ptrfmt) (ia (i), i = 1, ncol+1)
      write(iounit,indfmt) (ja (i), i = 1, nnz)
      if (job .le. 1) return
      write(iounit,valfmt) (a(i), i = 1, nnz)
      if (job .le. 2) return 
      len = nrow*nrhs 
      next = 1
      iend = len
      write(iounit,valfmt) (rhs(i), i = next, iend)
c     
c     write initial guesses if available
c     
      if (guesol(1:1) .eq. 'G' .or. guesol(1:1) .eq. 'g') then
         next = next+len
         iend = iend+ len
         write(iounit,valfmt) (rhs(i), i = next, iend)
      endif
c     
c     write exact solutions if available
c     
      if (guesol(2:2) .eq. 'X' .or. guesol(2:2) .eq. 'x') then
         next = next+len
         iend = iend+ len
         write(iounit,valfmt) (rhs(i), i = next, iend)
      endif
c     
      return
c----------end of prtmt ------------------------------------------------ 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine prtunf(n, a, ja, ia, iout, ierr)
c-----------------------------------------------------------------------
c This subroutine dumps the arrays used for storing sparse compressed row
c format in machine code, i.e. unformatted using standard FORTRAN term.
c-----------------------------------------------------------------------
c First coded by Kesheng Wu on Oct 21, 1991 under the instruction of
c Prof. Y. Saad
c-----------------------------------------------------------------------
c On entry:
c     n: the size of the matrix (matrix is n X n)
c    ia: integer array stores the stariting position of each row.
c    ja: integer array stores the column indices of each entry.
c     a: the non-zero entries of the matrix.
c  iout: the unit number opened for storing the matrix.
c On return:
c  ierr: a error, 0 if everything's OK, else 1 if error in writing data.
c On error:
c  set ierr to 1.
c  No redirection is made, since direct the machine code to the standard
c output may cause unpridictable consequences.
c-----------------------------------------------------------------------
      integer iout, n, nnz, ierr, ia(*), ja(*)
      real*8  a(*)
      nnz = ia(n+1)-ia(1) 
c
      write(unit=iout, err=1000)  n
      write(unit=iout, err=1000) (ia(k),k=1,n+1) 
      if (nnz .gt. 0) then
         write(unit=iout, err=1000) (ja(k),k=1,nnz)
         write(unit=iout, err=1000) ( a(k),k=1,nnz)
      endif 
c
      ierr = 0
      return
c
 1000 ierr = 1
      return
      end
c----------end-of-ddot--------------------------------------------------
c----------------------------------------------------------------------- 

      subroutine psgrid (npts,ja,ia,xx,yy,title,ptitle,size,munt,iunt) 
c-----------------------------------------------------------------------
c     plots a symmetric graph defined by ja,ia and the coordinates
c     xx(*),yy(*) 
c----------------------------------------------------------------------- 
c npts   = number of points in mesh
c ja, ia = connectivity of mesh -- as given by pattern of sparse
c            matrix.
c xx, yy = cordinates of the points. 
c
c title  = character*(*). a title of arbitrary length to be printed 
c          as a caption to the figure. Can be a blank character if no
c          caption is desired.
c
c ptitle = position of title; 0 under the drawing, else above
c
c size   = size of the drawing  
c
c munt   = units used for size : 'cm' or 'in'
c
c     iunt   = logical unit number where to write the matrix into.
c----------------------------------------------------------------------- 
      implicit none 
      integer npts,ptitle,iunt, ja(*), ia(*) 
      character title*(*), munt*2 
      real*8 xx(npts),yy(npts) 
      real size
c----------------------------------------------------------------------- 
c     local variables --------------------------------------------------
c----------------------------------------------------------------------- 
      integer nr,ii,k,ltit
      real xi,yi,xj,yj,lrmrgn,botmrgn,xtit,ytit,ytitof,fnstit,siz,
     *     xl,xr, yb,yt, scfct,u2dot,frlw,delt,paperx,conv,dimen,
     *     haf,zero, xdim, ydim
      real*8 xmin,xmax,ymin,ymax,max,min
      integer j
c-----------------------------------------------------------------------
      integer LENSTR
      external LENSTR
c----------------------------------------------------------------------- 
      data haf /0.5/, zero/0.0/, conv/2.54/
      siz = size
c
c     get max and min dimensions
c
      xmin = xx(1) 
      xmax = xmin 
      ymin = yy(1)
      ymax = ymin
      do j=2, npts
         xmax = max(xmax,xx(j))
         xmin = min(xmin,xx(j))
         ymax = max(ymax,yy(j))
         ymin = min(ymin,yy(j))
      enddo
c----------------------------------------------------------------------- 
c      n = npts
      nr = npts 
      xdim = xmax -xmin
      ydim = ymax -ymin
      dimen = max(xdim,ydim) 
c-----------------------------------------------------------------------
      print *, ' xmin', xmin, ' xmax', xmax 
      print *, ' ymin', ymin, ' ymax', ymax, ' dimen ', dimen
c-----------------------------------------------------------------------
c
c units (cm or in) to dot conversion factor and paper size
c 
      if (munt.eq.'cm' .or. munt.eq.'CM') then
         u2dot = 72.0/conv
         paperx = 21.0
      else
        u2dot = 72.0
        paperx = 8.5*conv
        siz = siz*conv
      end if
c
c left and right margins (drawing is centered)
c 
      lrmrgn = (paperx-siz)/2.0
c
c bottom margin : 2 cm
c
      botmrgn = 2.0/dimen
c     scaling factor
      scfct = siz*u2dot/dimen
c     frame line witdh
      frlw = 0.25/dimen
c     font siz for title (cm)
      fnstit = 0.5/dimen
      ltit = LENSTR(title)
c
c     position of title : centered horizontally
c                     at 1.0 cm vertically over the drawing
      ytitof = 1.0/dimen
      xtit = paperx/2.0
      ytit = botmrgn+siz*nr/dimen + ytitof
c almost exact bounding box
      xl = lrmrgn*u2dot - scfct*frlw/2
      xr = (lrmrgn+siz)*u2dot + scfct*frlw/2
      yb = botmrgn*u2dot - scfct*frlw/2
      yt = (botmrgn+siz*ydim/dimen)*u2dot + scfct*frlw/2
      if (ltit.gt.0) then
        yt = yt + (ytitof+fnstit*0.70)*u2dot
      end if
c add some room to bounding box
      delt = 10.0
      xl = xl-delt
      xr = xr+delt
      yb = yb-delt
      yt = yt+delt
c
c correction for title under the drawing
c
      if (ptitle.eq.0 .and. ltit.gt.0) then
      ytit = botmrgn + fnstit*0.3
      botmrgn = botmrgn + ytitof + fnstit*0.7
      end if
c
c     begin output
c
      write(iunt,10) '%!'
      write(iunt,10) '%%Creator: PSPLTM routine'
      write(iunt,12) '%%BoundingBox:',xl,yb,xr,yt
      write(iunt,10) '%%EndComments'
      write(iunt,10) '/cm {72 mul 2.54 div} def'
      write(iunt,10) '/mc {72 div 2.54 mul} def'
      write(iunt,10) '/pnum { 72 div 2.54 mul 20 string'
      write(iunt,10) 'cvs print ( ) print} def'
      write(iunt,10)
     1  '/Cshow {dup stringwidth pop -2 div 0 rmoveto show} def'
c
c     we leave margins etc. in cm so it is easy to modify them if
c     needed by editing the output file
c
      write(iunt,10) 'gsave'
      if (ltit.gt.0) then
      write(iunt,*) '/Helvetica findfont',fnstit,' cm scalefont setfont'
      write(iunt,*) xtit,' cm',ytit,' cm moveto'
      write(iunt,'(3A)') '(',title(1:ltit),') Cshow'
      end if
c
      write(iunt,*) lrmrgn,' cm ',botmrgn,' cm translate'
      write(iunt,*) siz,' cm ',dimen,' div dup scale '
c     
c     draw a frame around the matrix  // REMOVED 
c
c       del = 0.005
c       del2 = del*2.0 
c       write(iunt,*) del, ' setlinewidth'
c       write(iunt,10) 'newpath'
c       write(iunt,11) -del2, -del2, ' moveto'
c       write(iunt,11) dimen+del2,-del2,' lineto'
c       write(iunt,11) dimen+del2, dimen+del2, ' lineto'
c       write(iunt,11) -del2,dimen+del2,' lineto'
c       write(iunt,10) 'closepath stroke'
c     
c----------- plotting loop ---------------------------------------------
       write(iunt,*)  ' 0.01 setlinewidth'
c
       do 1 ii=1, npts 
       
c     if (mask(ii) .eq. 0) goto 1
          xi = xx(ii) - xmin 
          yi = yy(ii) - ymin
c     write (iout+1,*) ' ******** ii pt', xi, yi, xmin, ymin 
          do 2 k=ia(ii),ia(ii+1)-1 
             j = ja(k) 
             if (j .le. ii) goto 2 
             xj = xx(j) - xmin 
             yj = yy(j) - ymin
c     write (iout+1,*) ' j pt -- j= ',j, 'pt=', xj, yj
c
c     draw a line from ii to j
c
             write(iunt,11) xi, yi, ' moveto '
             write(iunt,11) xj, yj, ' lineto'
             write(iunt,10) 'closepath stroke'
 2        continue 
 1     continue
c-----------------------------------------------------------------------
       write(iunt,10) 'showpage'
       return
c
 10   format (A)
 11   format (2F9.2,A)
 12   format (A,4F9.2)
 13   format (2F9.2,A)
c-----------------------------------------------------------------------
      end
c--- end of lstif3 ---------------------------------------------------------
c---------------------------------------------------------------------------
C  Piecewise linear fucntions on triangle.
	function psi(i,r,s)
	implicit real*8(a-h,o-z)

	goto (100,200,300) ,i

100	  psi = -(r+s)/2.
	return
200	  psi = (r+1.)/2.
	return
300	  psi = (s+1.)/2.
	return
	end
c-----------------------------------------------------------------------
      subroutine pspltm(nrow,ncol,mode,ja,ia,title,ptitle,size,munt,
     *     nlines,lines,iunt)
c-----------------------------------------------------------------------
      integer nrow,ncol,ptitle,mode,iunt, ja(*), ia(*), lines(nlines) 
      real size
      character title*(*), munt*2 
c----------------------------------------------------------------------- 
c PSPLTM - PostScript PLoTer of a (sparse) Matrix
c This version by loris renggli (renggli@masg1.epfl.ch), Dec 1991
c and Youcef Saad 
c------
c Loris RENGGLI, Swiss Federal Institute of Technology, Math. Dept
c CH-1015 Lausanne (Switzerland)  -- e-mail:  renggli@masg1.epfl.ch
c Modified by Youcef Saad -- June 24, 1992 to add a few features:
c separation lines + acceptance of MSR format.
c-----------------------------------------------------------------------
c input arguments description :
c
c nrow   = number of rows in matrix
c
c ncol   = number of columns in matrix 
c
c mode   = integer indicating whether the matrix is stored in 
c           CSR mode (mode=0) or CSC mode (mode=1) or MSR mode (mode=2) 
c
c ja     = column indices of nonzero elements when matrix is
c          stored rowise. Row indices if stores column-wise.
c ia     = integer array of containing the pointers to the 
c          beginning of the columns in arrays a, ja.
c
c title  = character*(*). a title of arbitrary length to be printed 
c          as a caption to the figure. Can be a blank character if no
c          caption is desired.
c
c ptitle = position of title; 0 under the drawing, else above
c
c size   = size of the drawing  
c
c munt   = units used for size : 'cm' or 'in'
c
c nlines = number of separation lines to draw for showing a partionning
c          of the matrix. enter zero if no partition lines are wanted.
c
c lines  = integer array of length nlines containing the coordinates of 
c          the desired partition lines . The partitioning is symmetric: 
c          a horizontal line across the matrix will be drawn in 
c          between rows lines(i) and lines(i)+1 for i=1, 2, ..., nlines
c          an a vertical line will be similarly drawn between columns
c          lines(i) and lines(i)+1 for i=1,2,...,nlines 
c
c iunt   = logical unit number where to write the matrix into.
c----------------------------------------------------------------------- 
c additional note: use of 'cm' assumes european format for paper size
c (21cm wide) and use of 'in' assumes american format (8.5in wide).
c The correct centering of the figure depends on the proper choice. Y.S.
c-----------------------------------------------------------------------
c external 
      integer LENSTR
      external LENSTR
c local variables ---------------------------------------------------
      integer n,nr,nc,maxdim,istart,ilast,ii,k,ltit
      real lrmrgn,botmrgn,xtit,ytit,ytitof,fnstit,siz
      real xl,xr, yb,yt, scfct,u2dot,frlw,delt,paperx,conv,xx,yy
      logical square 
c change square to .true. if you prefer a square frame around
c a rectangular matrix
      data haf /0.5/, zero/0.0/, conv/2.54/,square/.false./
c-----------------------------------------------------------------------
      siz = size
      nr = nrow
      nc = ncol
      n = nc
      if (mode .eq. 0) n = nr
c      nnz = ia(n+1) - ia(1) 
      maxdim = max(nrow, ncol)
      m = 1 + maxdim
      nc = nc+1
      nr = nr+1
c
c units (cm or in) to dot conversion factor and paper size
c 
      if (munt.eq.'cm' .or. munt.eq.'CM') then
         u2dot = 72.0/conv
        paperx = 21.0
      else
        u2dot = 72.0
        paperx = 8.5*conv
        siz = siz*conv
      end if
c
c left and right margins (drawing is centered)
c 
      lrmrgn = (paperx-siz)/2.0
c
c bottom margin : 2 cm
c
      botmrgn = 2.0
c scaling factor
      scfct = siz*u2dot/m
c matrix frame line witdh
      frlw = 0.25
c font size for title (cm)
      fnstit = 0.5
      ltit = LENSTR(title)
c position of title : centered horizontally
c                     at 1.0 cm vertically over the drawing
      ytitof = 1.0
      xtit = paperx/2.0
      ytit = botmrgn+siz*nr/m + ytitof
c almost exact bounding box
      xl = lrmrgn*u2dot - scfct*frlw/2
      xr = (lrmrgn+siz)*u2dot + scfct*frlw/2
      yb = botmrgn*u2dot - scfct*frlw/2
      yt = (botmrgn+siz*nr/m)*u2dot + scfct*frlw/2
      if (ltit.gt.0) then
        yt = yt + (ytitof+fnstit*0.70)*u2dot
      end if
c add some room to bounding box
      delt = 10.0
      xl = xl-delt
      xr = xr+delt
      yb = yb-delt
      yt = yt+delt
c
c correction for title under the drawing
      if (ptitle.eq.0 .and. ltit.gt.0) then
      ytit = botmrgn + fnstit*0.3
      botmrgn = botmrgn + ytitof + fnstit*0.7
      end if
c begin of output
c
      write(iunt,10) '%!'
      write(iunt,10) '%%Creator: PSPLTM routine'
      write(iunt,12) '%%BoundingBox:',xl,yb,xr,yt
      write(iunt,10) '%%EndComments'
      write(iunt,10) '/cm {72 mul 2.54 div} def'
      write(iunt,10) '/mc {72 div 2.54 mul} def'
      write(iunt,10) '/pnum { 72 div 2.54 mul 20 string'
      write(iunt,10) 'cvs print ( ) print} def'
      write(iunt,10)
     1  '/Cshow {dup stringwidth pop -2 div 0 rmoveto show} def'
c
c we leave margins etc. in cm so it is easy to modify them if
c needed by editing the output file
      write(iunt,10) 'gsave'
      if (ltit.gt.0) then
      write(iunt,*) '/Helvetica findfont ',fnstit,
     &             ' cm scalefont setfont '
      write(iunt,*) xtit,' cm ',ytit,' cm moveto '
      write(iunt,'(3A)') '(',title(1:ltit),') Cshow'
      end if
      write(iunt,*) lrmrgn,' cm ',botmrgn,' cm translate'
      write(iunt,*) siz,' cm ',m,' div dup scale '
c------- 
c draw a frame around the matrix
      write(iunt,*) frlw,' setlinewidth'
      write(iunt,10) 'newpath'
      write(iunt,11) 0, 0, ' moveto'
      if (square) then
      write(iunt,11) m,0,' lineto'
      write(iunt,11) m, m, ' lineto'
      write(iunt,11) 0,m,' lineto'
      else
      write(iunt,11) nc,0,' lineto'
      write(iunt,11) nc,nr,' lineto'
      write(iunt,11) 0,nr,' lineto'
      end if
      write(iunt,10) 'closepath stroke'
c
c     drawing the separation lines 
c 
      write(iunt,*)  ' 0.2 setlinewidth'
      do 22 kol=1, nlines 
         isep = lines(kol) 
c
c     horizontal lines 
c
         yy =  real(nrow-isep) + haf 
         xx = real(ncol+1) 
         write(iunt,13) zero, yy, ' moveto '
         write(iunt,13)  xx, yy, ' lineto stroke '
c
c vertical lines 
c
         xx = real(isep) + haf 
         yy = real(nrow+1)  
         write(iunt,13) xx, zero,' moveto '
         write(iunt,13) xx, yy, ' lineto stroke '             
 22     continue
c 
c----------- plotting loop ---------------------------------------------
c
      write(iunt,10) '1 1 translate'
      write(iunt,10) '0.8 setlinewidth'
      write(iunt,10) '/p {moveto 0 -.40 rmoveto '
      write(iunt,10) '           0  .80 rlineto stroke} def'
c     
      do 1 ii=1, n
        istart = ia(ii)
        ilast  = ia(ii+1)-1 
        if (mode .eq. 1) then
          do 2 k=istart, ilast
            write(iunt,11) ii-1, nrow-ja(k), ' p'
 2        continue 
        else
          do 3 k=istart, ilast
            write(iunt,11) ja(k)-1, nrow-ii, ' p'
 3        continue          
c add diagonal element if MSR mode.
          if (mode .eq. 2) 
     *         write(iunt,11) ii-1, nrow-ii, ' p' 
c
        endif
 1    continue
c-----------------------------------------------------------------------
      write(iunt,10) 'showpage'
      return
c
 10   format (A)
 11   format (2(I6,1x),A)
 12   format (A,4(1x,F9.2))
 13   format (2(F9.2,1x),A)
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
        subroutine qsplit(a,ind,n,ncut)
        real*8 a(n)
        integer ind(n), n, ncut
c-----------------------------------------------------------------------
c     does a quick-sort split of a real array.
c     on input a(1:n). is a real array
c     on output a(1:n) is permuted such that its elements satisfy:
c
c     abs(a(i)) .ge. abs(a(ncut)) for i .lt. ncut and
c     abs(a(i)) .le. abs(a(ncut)) for i .gt. ncut
c
c     ind(1:n) is an integer array which permuted in the same way as a(*).
c-----------------------------------------------------------------------
        real*8 tmp, abskey
        integer itmp, first, last
c-----
        first = 1
        last = n
        if (ncut .lt. first .or. ncut .gt. last) return
c
c     outer loop -- while mid .ne. ncut do
c
 1      mid = first
        abskey = abs(a(mid))
        do 2 j=first+1, last
           if (abs(a(j)) .gt. abskey) then
              mid = mid+1
c     interchange
              tmp = a(mid)
              itmp = ind(mid)
              a(mid) = a(j)
              ind(mid) = ind(j)
              a(j)  = tmp
              ind(j) = itmp
           endif
 2      continue
c
c     interchange
c
        tmp = a(mid)
        a(mid) = a(first)
        a(first)  = tmp
c
        itmp = ind(mid)
        ind(mid) = ind(first)
        ind(first) = itmp
c
c     test for while loop
c
        if (mid .eq. ncut) return
        if (mid .gt. ncut) then
           last = mid-1
        else
           first = mid+1
        endif
        goto 1
c----------------end-of-qsplit------------------------------------------
c-----------------------------------------------------------------------
        end
c-----------------------------------------------------------------------
      subroutine rdis(n,ja,ia,ndom,map,mapptr,mask,levels,size,iptr) 
      implicit none
      integer n,ja(*),ia(*),ndom,map(*),mapptr(*),mask(*),levels(*),
     *     size(ndom),iptr(ndom)
c-----------------------------------------------------------------------
c     recursive dissection algorithm for partitioning.
c     initial graph is cut in two - then each time, the largest set
c     is cut in two until we reach desired number of domains.
c----------------------------------------------------------------------- 
c     input
c     n, ja, ia = graph 
c     ndom      = desired number of subgraphs
c     output 
c     ------
c     map, mapptr  = pointer array data structure for domains. 
c             if k1 = mapptr(i), k2=mapptr(i+1)-1 then 
c             map(k1:k2) = points in domain number i
c    work arrays:
c    -------------
c    mask(1:n)    integer 
c    levels(1:n)  integer 
c    size(1:ndom) integer 
c    iptr(1:ndom) integer 
c----------------------------------------------------------------------- 
      integer idom,maskval,k,nlev,init,nextsiz,wantsiz,lev,ko,
     *     maxsiz,j,nextdom  
c-----------------------------------------------------------------------
      idom = 1
c-----------------------------------------------------------------------
c     size(i) = size of domnain  i
c     iptr(i)  = index of first element of domain i
c-----------------------------------------------------------------------
      size(idom) = n
      iptr(idom) = 1
      do j=1, n
         mask(j) = 1 
      enddo
c     
c     domain loop
c
 1    continue
c
c     select domain with largest size
c     
      maxsiz = 0 
      do j=1, idom
         if (size(j) .gt. maxsiz) then
            maxsiz = size(j)
            nextdom = j
         endif
      enddo
c
c     do a Prphn/ BFS on nextdom
c     
      maskval = nextdom
      init = iptr(nextdom) 
      call perphn(n,ja,ia,init,mask,maskval,nlev,map,levels) 
c
c     determine next subdomain
c
      nextsiz = 0
      wantsiz = maxsiz/2 
      idom = idom+1
      lev = nlev 
      do while (nextsiz .lt. wantsiz) 
         do k = levels(lev), levels(lev+1)-1
            mask(map(k)) = idom
        enddo
        nextsiz = nextsiz + levels(lev+1) - levels(lev) 
        lev = lev-1
      enddo
c
      size(nextdom) = size(nextdom) - nextsiz
      size(idom) = nextsiz
c
c     new initial point = last point of previous domain
c
       iptr(idom) = map(levels(nlev+1)-1) 
c       iptr(idom) = map(levels(lev)+1) 
c      iptr(idom) = 1 
c
c alternative 
c      lev = 1 
c      do while (nextsiz .lt. wantsiz) 
c         do k = levels(lev), levels(lev+1)-1
c            mask(map(k)) = idom
c         enddo
c         nextsiz = nextsiz + levels(lev+1) - levels(lev) 
c         lev = lev+1
c      enddo
c
c     set size of new domain and adjust previous one
c
c      size(idom) = nextsiz 
c      size(nextdom) = size(nextdom) - nextsiz 
c      iptr(idom) = iptr(nextdom) 
c      iptr(nextdom) = map(levels(lev))

      if (idom .lt. ndom) goto 1
c
c     domains found -- build data structure 
c     
      mapptr(1) = 1
      do idom=1, ndom 
         mapptr(idom+1) = mapptr(idom) + size(idom) 
      enddo
      do k=1, n
         idom = mask(k) 
         ko = mapptr(idom) 
         map(ko) = k
         mapptr(idom) = ko+1
      enddo
c
c     reset pointers
c     
      do j = ndom,1,-1
         mapptr(j+1) = mapptr(j) 
      enddo
      mapptr(1) = 1 
c
      return
      end 
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
C                        INPUT-OUTPUT MODULE                           c
c----------------------------------------------------------------------c
c contents:                                                            c
c----------                                                            c
c  readmt : reads matrices in the Boeing/Harwell format.               c
c  prtmt  : prints matrices in the Boeing/Harwell format.              c
c  dump   : outputs matrix rows in a simple format (debugging purposes)c 
c  pspltm : generates a post-script plot of the non-zero pattern of A  c
c  pltmt  : produces a 'pic' file for plotting a sparse matrix         c
c  smms   : write the matrx in a format used in SMMS package           c
c  readsm : reads matrics in coordinate format (as in SMMS package)    c
c  readsk : reads matrices in CSR format (simplified H/B formate).     c
c  skit   : writes matrics to a file, format same as above.            c
c  prtunf : writes matrics (in CSR format) unformatted                 c
c  readunf: reads unformatted data of matrics (in CSR format)          c
c----------------------------------------------------------------------c
	subroutine readmt (nmax,nzmax,job,iounit,a,ja,ia,rhs,nrhs,
     *                     guesol,nrow,ncol,nnz,title,key,type,ierr)
c-----------------------------------------------------------------------
c this subroutine reads  a boeing/harwell matrix. handles right hand 
c sides in full format only (no sparse right hand sides).
c Also the matrix must be in assembled forms.
c Author: Youcef Saad - Date: Sept., 1989
c         updated Oct 31, 1989.
c-----------------------------------------------------------------------
c on entry:
c---------
c nmax 	 =  max column dimension  allowed for matrix. The array ia should 
c	    be of length at least ncol+1 (see below) if job.gt.0
c nzmax	 = max number of nonzeros elements allowed. the arrays a, 
c          and ja should be of length equal to nnz (see below) if these
c          arrays are to be read (see job).
c          
c job	 = integer to indicate what is to be read. (note: job is an
c          input and output parameter, it can be modified on return)
c          job = 0    read the values of ncol, nrow, nnz, title, key,
c                     type and return. matrix is not read and arrays
c                     a, ja, ia, rhs are not touched.
c          job = 1    read srtucture only, i.e., the arrays ja and ia.
c          job = 2    read matrix including values, i.e., a, ja, ia
c          job = 3    read matrix and right hand sides: a,ja,ia,rhs.
c		      rhs may contain initial guesses and exact 
c                     solutions appended to the actual right hand sides.
c		      this will be indicated by the output parameter
c                     guesol [see below]. 
c                     
c nrhs   = integer. nrhs is an input as well as ouput parameter.
c          at input nrhs contains the total length of the array rhs.
c          See also ierr and nrhs in output parameters.
c
c iounit = logical unit number where to read the matrix from.
c
c on return:
c---------- 
c job    = on return job may be modified to the highest job it could
c          do: if job=2 on entry but no matrix values are available it
c          is reset to job=1 on return. Similarly of job=3 but no rhs 
c          is provided then it is rest to job=2 or job=1 depending on 
c          whether or not matrix values are provided.
c          Note that no error message is triggered (i.e. ierr = 0 
c          on return in these cases. It is therefore important to
c          compare the values of job on entry and return ).
c
c a	 = the a matrix in the a, ia, ja (column) storage format
c ja 	 = row number of element a(i,j) in array a.
c ia     = pointer  array. ia(i) points to the beginning of column i.
c
c rhs    = real array of size nrow + 1 if available (see job)
c
c nrhs   = integer containing the number of right-hand sides found
c          each right hand side may be accompanied with an intial guess
c          and also the exact solution.
c
c guesol = a 2-character string indicating whether an initial guess 
c          (1-st character) and / or the exact solution (2-nd
c          character) is provided with the right hand side.
c	   if the first character of guesol is 'G' it means that an
c          an intial guess is provided for each right-hand side.
c          These are appended to the right hand-sides in the array rhs.
c	   if the second character of guesol is 'X' it means that an
c          exact solution is provided for each right-hand side.
c          These are  appended to the right hand-sides 
c          and the initial guesses (if any) in the array rhs.
c
c nrow   = number of rows in matrix
c ncol	 = number of columns in matrix 
c nnz	 = number of nonzero elements in A. This info is returned
c          even if there is not enough space in a, ja, ia, in order
c          to determine the minimum storage needed. 
c
c title  = character*72 = title of matrix test ( character a*72). 
c key    = character*8  = key of matrix 
c type   = charatcer*3  = type of matrix.
c          for meaning of title, key and type refer to documentation 
c          Harwell/Boeing matrices.
c
c ierr   = integer used for error messages 
c         * ierr  =  0 means that  the matrix has been read normally. 
c         * ierr  =  1 means that  the array matrix could not be read 
c         because ncol+1 .gt. nmax
c         * ierr  =  2 means that  the array matrix could not be read 
c         because nnz .gt. nzmax 
c         * ierr  =  3 means that  the array matrix could not be read 
c         because both (ncol+1 .gt. nmax) and  (nnz .gt. nzmax )
c         * ierr  =  4 means that  the right hand side (s) initial 
c         guesse (s) and exact solution (s)   could  not be
c         read because they are stored in sparse format (not handled
c         by this routine ...) 
c         * ierr  =  5 means that the right-hand-sides, initial guesses
c         and exact solutions could not be read because the length of 
c         rhs as specified by the input value of nrhs is not 
c         sufficient to store them. The rest of the matrix may have
c         been read normally.
c 
c Notes:
c-------
c 1) The file inout must be open (and possibly rewound if necessary)
c    prior to calling readmt.
c 2) Refer to the documentation on the Harwell-Boeing formats
c    for details on the format assumed by readmt.
c    We summarize the format here for convenience.
c  
c    a) all lines in inout are assumed to be 80 character long.
c    b) the file consists of a header followed by the block of the 
c       column start pointers followed by the block of the
c       row indices, followed by the block of the real values and
c       finally the numerical values of the right-hand-side if a 
c       right hand side is supplied. 
c    c) the file starts by a header which contains four lines if no
c       right hand side is supplied and five lines otherwise.
c       * first line contains the title (72 characters long) followed by
c         the 8-character identifier (name of the matrix, called key)
c        [ A72,A8 ]
c       * second line contains the number of lines for each
c         of the following data blocks (4 of them) and the total number 
c         of lines excluding the header.
c        [5i4]
c       * the third line contains a three character string identifying
c         the type of matrices as they are referenced in the Harwell
c         Boeing documentation [e.g., rua, rsa,..] and the number of
c         rows, columns, nonzero entries.
c         [A3,11X,4I14]
c       * The fourth line contains the variable fortran format
c         for the following data blocks.
c         [2A16,2A20] 
c       * The fifth line is only present if right-hand-sides are 
c         supplied. It consists of three one character-strings containing
c         the storage format for the right-hand-sides 
c         ('F'= full,'M'=sparse=same as matrix), an initial guess 
c         indicator ('G' for yes), an exact solution indicator 
c         ('X' for yes), followed by the number of right-hand-sides
c         and then the number of row indices. 
c         [A3,11X,2I14] 
c     d) The three following blocks follow the header as described 
c        above.
c     e) In case the right hand-side are in sparse formats then 
c        the fourth block uses the same storage format as for the matrix
c        to describe the NRHS right hand sides provided, with a column
c        being replaced by a right hand side.
c-----------------------------------------------------------------------
      character title*72, key*8, type*3, ptrfmt*16, indfmt*16,
     1       valfmt*20, rhsfmt*20, rhstyp*3, guesol*2
      integer totcrd, ptrcrd, indcrd, valcrd, rhscrd, nrow, ncol,
     1     nnz, neltvl, nrhs, nmax, nzmax, nrwindx
      integer ia (nmax+1), ja (nzmax) 
      real*8 a(nzmax), rhs(*) 
c-----------------------------------------------------------------------
      ierr = 0
      lenrhs = nrhs
c
      read (iounit,10) title, key, totcrd, ptrcrd, indcrd, valcrd, 
     1     rhscrd, type, nrow, ncol, nnz, neltvl, ptrfmt, indfmt, 
     2     valfmt, rhsfmt
 10   format (a72, a8 / 5i14 / a3, 11x, 4i14 / 2a16, 2a20)
c
      if (rhscrd .gt. 0) read (iounit,11) rhstyp, nrhs, nrwindx
 11   format (a3,11x,i14,i14)
c
c anything else to read ?
c
      if (job .le. 0) return
c     ---- check whether matrix is readable ------ 
      n = ncol
      if (ncol .gt. nmax) ierr = 1
      if (nnz .gt. nzmax) ierr = ierr + 2
      if (ierr .ne. 0) return
c     ---- read pointer and row numbers ---------- 
      read (iounit,ptrfmt) (ia (i), i = 1, n+1)
      read (iounit,indfmt) (ja (i), i = 1, nnz)
c     --- reading values of matrix if required....
      if (job .le. 1)  return
c     --- and if available ----------------------- 
      if (valcrd .le. 0) then
	 job = 1
	 return
      endif
      read (iounit,valfmt) (a(i), i = 1, nnz)
c     --- reading rhs if required ---------------- 
      if (job .le. 2)  return
c     --- and if available ----------------------- 
      if ( rhscrd .le. 0) then
	 job = 2
	 nrhs = 0
	 return
      endif
c     
c     --- read right-hand-side.-------------------- 
c     
      if (rhstyp(1:1) .eq. 'M') then 
         ierr = 4
         return
      endif
c
      guesol = rhstyp(2:3) 
c     
      nvec = 1 
      if (guesol(1:1) .eq. 'G' .or. guesol(1:1) .eq. 'g') nvec=nvec+1
      if (guesol(2:2) .eq. 'X' .or. guesol(2:2) .eq. 'x') nvec=nvec+1
c     
      len = nrhs*nrow 
c     
      if (len*nvec .gt. lenrhs) then
         ierr = 5
         return
      endif
c
c read right-hand-sides
c
      next = 1
      iend = len
      read(iounit,rhsfmt) (rhs(i), i = next, iend)
c
c read initial guesses if available
c
      if (guesol(1:1) .eq. 'G' .or. guesol(1:1) .eq. 'g') then
         next = next+len
         iend = iend+ len
         read(iounit,valfmt) (rhs(i), i = next, iend)
      endif
c     
c read exact solutions if available
c
      if (guesol(2:2) .eq. 'X' .or. guesol(2:2) .eq. 'x') then
         next = next+len
         iend = iend+ len
         read(iounit,valfmt) (rhs(i), i = next, iend)
      endif
c     
      return
c--------- end of readmt -----------------------------------------------
c----------------------------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine readsk (nmax,nzmax,n,nnz,a,ja,ia,iounit,ierr)
      integer nmax, nzmax, iounit, n, nnz, i, ierr
      integer ia(nmax+1), ja(nzmax) 
      real*8 a(nzmax)
c-----------------------------------------------------------------------
c Reads matrix in Compressed Saprse Row format. The data is supposed to
c appear in the following order -- n, ia, ja, a
c Only square matrices accepted. Format has following features
c (1) each number is separated by at least one space (or end-of-line), 
c (2) each array starts with a new line.
c-----------------------------------------------------------------------
c coded by Kesheng Wu on Oct 21, 1991 with supervision of Y. Saad
c-----------------------------------------------------------------------
c on entry:
c---------
c nmax 	 = max column dimension  allowed for matrix.
c nzmax	 = max number of nonzeros elements allowed. the arrays a, 
c          and ja should be of length equal to nnz (see below).
c iounit = logical unit number where to read the matrix from.
c
c on return:
c---------- 
c ia,
c ja,
c a      = matrx in CSR format
c n      = number of rows(columns) in matrix
c nnz	 = number of nonzero elements in A. This info is returned
c          even if there is not enough space in a, ja, ia, in order
c          to determine the minimum storage needed.
c ierr   = error code,
c          0 : OK;
c          1 : error when try to read the specified I/O unit.
c          2 : end-of-file reached during reading of data file.
c          3 : array size in data file is negtive or larger than nmax;
c          4 : nunmer of nonzeros in data file is negtive or larger than nzmax
c in case of errors:
c---------
c     n is set to 0 (zero), at the same time ierr is set.
c-----------------------------------------------------------------------
c     
c     read the size of the matrix
c
      rewind(iounit)
      read (iounit, *, err=1000, end=1010) n
      if ((n.le.0).or.(n.gt.nmax)) goto 1020
c     
c     read the pointer array ia(*)
c     
      read (iounit, *, err=1000, end=1010) (ia(i), i=1, n+1)
c     
c     Number of None-Zeros
c     
      nnz = ia(n+1) - 1
      if ((nnz.le.0).or.(nnz.gt.nzmax)) goto 1030
c     
c     read the column indices array
c     
      read (iounit, *, err=1000, end=1010) (ja(i), i=1, nnz)
c     
c     read the matrix elements
c     
      read (iounit, *, err=1000, end=1010) (a(i), i=1, nnz)
c     
c     normal return
c
      ierr = 0
      return
c     
c     error handling code
c     
c     error in reading I/O unit
 1000 ierr = 1
      goto 2000
c
c     EOF reached in reading
 1010 ierr =2
      goto 2000
c
c     n non-positive or too large
 1020 ierr = 3
      n = 0
      goto 2000
c
c     NNZ non-positive or too large
 1030 ierr = 4
c     
c     the real return statement
c     
 2000 n = 0
      return
c---------end of readsk ------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine readsm (nmax,nzmax,n,nnz,ia,ja,a,iout,ierr)
      integer nmax, nzmax, row, n, iout, i, j, k, ierr
      integer ia(nmax+1), ja(nzmax)
      real*8  a(nzmax), x
c-----------------------------------------------------------------------
c     read a matrix in coordinate format as is used in the SMMS
c     package (F. Alvarado), i.e. the row is in ascending order.
c     Outputs the matrix in CSR format.
c-----------------------------------------------------------------------
c coded by Kesheng Wu on Oct 21, 1991 with the supervision of Y. Saad
c-----------------------------------------------------------------------
c on entry:
c---------
c nmax  = the maximum size of array
c nzmax = the maximum number of nonzeros
c iout  = the I/O unit that has the data file
c
c on return:
c---------- 
c n     = integer = size of matrix
c nnz   = number of non-zero entries in the matrix
c a,
c ja, 
c ia    = matrix in CSR format
c ierr  = error code,
c         0 -- subroutine end with intended job done
c         1 -- error in I/O unit iout
c         2 -- end-of-file reached while reading n, i.e. a empty data file
c         3 -- n non-positive or too large
c         4 -- nnz is zero or larger than nzmax
c         5 -- data file is not orgnized in the order of ascending
c              row indices
c
c in case of errors:
c   n will be set to zero (0). In case the data file has more than nzmax
c   number of entries, the first nzmax entries will be read, and are not 
c   cleared on return. The total number of entry is determined.
c   Ierr is set.
c-----------------------------------------------------------------------
c
      rewind(iout)
      nnz = 0
      ia(1) = 1
      row = 1
c
      read (iout,*, err=1000, end=1010) n
      if ((n.le.0) .or. (n.gt.nmax)) goto 1020
c
 10   nnz = nnz + 1
      read (iout, *, err=1000, end=100) i, j, x

c     set the pointers when needed
      if (i.gt.row) then
         do 20 k = row+1, i
            ia(k) = nnz
 20      continue
         row = i
      else if (i.lt.row) then
         goto 1040
      endif

      ja(nnz) = j
      a (nnz) = x

      if (nnz.lt.nzmax) then
         goto 10
      else
         goto 1030
      endif

c     normal return -- end of file reached
 100  ia(row+1) = nnz
      nnz = nnz - 1
      if (nnz.eq.0) goto 1030
c
c     everything seems to be OK.
c
      ierr = 0
      return
c
c     error handling code
c
c     error in reading data entries
c
 1000 ierr = 1
      goto 2000
c
c     empty file
c
 1010 ierr  = 2
      goto 2000
c
c     problem with n
c
 1020 ierr = 3
      goto 2000
c
c     problem with nnz
c
 1030 ierr = 4
c
c     try to determine the real number of entries, in case needed
c
      if (nnz.ge.nzmax) then
 200     read(iout, *, err=210, end=210) i, j, x
         nnz = nnz + 1
         goto 200
 210     continue
      endif
      goto 2000
c
c     data entries not ordered
c
 1040 ierr = 5
 2000 n = 0
      return
c----end-of-readsm------------------------------------------------------
c-----------------------------------------------------------------------
      end
c---------end of prtunf ------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine readunf(nmax,nzmax,n,nnz,a,ja,ia,iounit,ierr)
c-----------------------------------------------------------------------
c This subroutine reads a matix store in machine code (FORTRAN
c unformatted form). The matrix is in CSR format.
c-----------------------------------------------------------------------
c First coded by Kesheng Wu on Oct 21, 1991 under the instruction of
c Prof. Y. Saad
c-----------------------------------------------------------------------
c On entry:
c    nmax: the maximum value of matrix size.
c   nzmax: the maximum number of non-zero entries.
c  iounit: the I/O unit that opened for reading.
c On return:
c       n: the actual size of array.
c     nnz: the actual number of non-zero entries.
c ia,ja,a: the matrix in CSR format.
c    ierr: a error code, it's same as that used in reaadsk
c          0 -- OK
c          1 -- error in reading iounit
c          2 -- end-of-file reached while reading data file
c          3 -- n is non-positive or too large
c          4 -- nnz is non-positive or too large
c On error:
c     return with n set to 0 (zero). nnz is kept if it's set already,
c     in case one want to use it to determine the size of array needed
c     to hold the data.
c-----------------------------------------------------------------------
c
      integer nmax, nzmax, n, iounit, nnz, k
      integer ia(nmax+1), ja(nzmax)
      real*8  a(nzmax)
c
      rewind iounit
c      
      read (unit=iounit, err=1000, end=1010) n
      if ((n.le.0) .or. (n.gt.nmax)) goto 1020
c
      read(unit=iounit, err=1000, end=1010) (ia(k),k=1,n+1)
c
      nnz = ia(n+1) - 1
      if ((nnz.le.0) .or. (nnz.gt.nzmax)) goto 1030
c
      read(unit=iounit, err=1000, end=1010) (ja(k),k=1,nnz)
      read(unit=iounit, err=1000, end=1010) (a(k),k=1,nnz)
c
c     everything seems to be OK.
c
      ierr = 0
      return
c
c     error handling
c
 1000 ierr = 1
      goto 2000
 1010 ierr = 2
      goto 2000
 1020 ierr = 3
      goto 2000
 1030 ierr = 4
 2000 n = 0
      return
      end
c-----------------------------------------------------------------------
      subroutine refall(nx, nelx,ijk,node,ndeg,x,y,
     *     ichild,iparnts,nodcode,nxmax,nelmax,ierr)
      implicit real*8  (a-h,o-z)
      integer nx, nelx, node, ndeg, nxmax, nelmax 
      integer ichild(ndeg,1),iparnts(2,nx),ijk(node,*), nodcode(nx)
      integer midnode(20),inod(20) 
      real*8  x(*),y(*)
c-------------------------------------------------------------
c refines a finite element grid using triangular elements.
c uses mid points to refine all the elements of the grid.
c
c nx	= number of nodes at input
c nelx	= number of elements at input
c ijk	= connectivity matrix: for node k, ijk(*,k) point to the
c         nodes of element k. 
c node  = first dimension of array ijk [should be >=3] 
c ndeg	= first dimension of array ichild which is at least as large
c         as the max degree of each node
c x,y   = real*8 arrays containing the x(*) and y(*) coordinates 
c	  resp. of the nodes.
c ichild= list of the children of a node: ichild(1,k) stores 
c         the position in ichild(*,k)  of the last child so far.
c         (local use)
c iparnts= list of the 2 parents of each node.
c         (local use)
c nodcode= boundary information list for each node with the
c	   following meaning:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point.
c corner elements are used only to generate the grid by refinement 
c since they do not  correspond to real elements. 
c nxmax  = maximum number of nodes allowed. If during the algorithm
c          the number of nodes being created exceeds nxmax then 
c	   refall  quits without modifying the (x,y) xoordinates
c	   and nx, nelx. ijk is modified. Also ierr is set to 1.
c nelmax = same as above for number of elements allowed. See ierr..
c ierr	 = error message: 
c	   0 --> normal return
c	   1 --> refall quit because nxmax  was exceeded.
c	   2 --> refall quit because nelmax was exceeded.
c--------------------------------------------------------------
c---------------------------------------------------------------  
c inilitialize lists of children and parents --
c data structure is as follows
c ichild(1,k) stores the position of last child of node k so far in list
c ichild(j,k) , j .ge. 2 = list of children of node k.
c iparnts(1,k) and iparnts(2,k) are the two parents of node k. 
c---------------------------------------------------------------  
c------ do a first check :
      if (nx .ge. nxmax) goto 800
      if (nelx .ge. nelmax) goto 900
c------ initialize
      do 1 k=1,nx
         do 2 j=2,ndeg
            ichild(j,k) = 0
 2       continue
         ichild(1,k) = 1
         iparnts(1,k)= 0
         iparnts(2,k)= 0
 1    continue
c------- initialize nelxnew and nxnew 
      nelxnew = nelx
      nxnew   = nx
      ierr    = 0
c--------------------------------------------------------------
c main loop: scan all elements
c--------------------------------------------------------------
c     do 100 nel = nelx,1,-1
      do 100 nel = 1, nelx
c note : interesting question which order is best for parallelism?
c alternative order: do 100 nel = nelx, 1, -1
c
c------ unpack nodes of element
         do 101 i=1,node
            inod(i) = ijk(i,nel)
c convention: node after last node = first node. 
            inod(node+i) = inod(i)
            midnode(i) = 0
 101     continue
c--------------------------------------------------------------
c for each new potential node determine if it has already been 
c numbered. a potential node is the middle of any two nodes ..
c-------------------------------------------------------------- 
         do 80 ii=1,node
        	k1 = inod(ii)
	        k2 = inod(ii+1)
c------- test for current pair :
                last = ichild(1,k1)
                do 21 k=2,last
                   jchild = ichild(k,k1) 
                   ipar1 = iparnts(1,jchild)
                   ipar2 = iparnts(2,jchild)
                   if( (ipar1 .eq. k1 .and. ipar2 .eq. k2) .or. 
     *                  (ipar2 .eq. k1 .and. ipar1 .eq. k2)) then
c node has already been created and numbered ....
                      midnode(ii) = jchild
c... therefore it must be an internal node
                      nodcode(jchild) = 0
c... and no new node to create.
                      goto 80
                   endif
c-----------------------------------------------------	    
 21         continue
c     
c else  create a new node
c
            nxnew = nxnew + 1
            if (nxnew .gt. nxmax) goto 800
c-------
            x(nxnew) = (x(k1) + x(k2))*0.5
            y(nxnew) = (y(k1) + y(k2))*0.5
            midnode(ii) = nxnew
c
c update nodcode information -- normally min0(nodcode(k1),nodcode(k2))
c 
            nodcode(nxnew) = min0(1,nodcode(k1),nodcode(k2))
c     
c update parents and children's lists
c 
            iparnts(1,nxnew) = k1
            iparnts(2,nxnew) = k2
c     
            last = last+1
            ichild(last,k1) = nxnew
            ichild(1,k1) = last
c     
            last = ichild(1,k2)+1
            ichild(last,k2) = nxnew
            ichild(1,k2) = last
c     
 80      continue		
c
c------- replace current element by new one
c
         do 81 i=1,node
            jnod = midnode(i)
            ijk(i,nel) = jnod
 81      continue
c-------create new elements
         do 82 ii=1, node
            nelxnew = nelxnew+1
            if (nelxnew .gt. nelmax) goto 900
            ijk(1,nelxnew) = inod(ii)
            k = ii
            do jj=2,node
               ijk(jj,nelxnew) = midnode(k)
               k = k+2
               if (k .gt. node) k =  k-node
           enddo
 82     continue
c------ done !
 100  continue
      nx = nxnew
      nelx = nelxnew
      return
 800  ierr = 1
      return
 900  ierr = 2
      return
      end
c-----------------------------------------------------------------------  
      subroutine retmx (n,a,ja,ia,dd)
      real*8 a(*),dd(*)
      integer n,ia(*),ja(*)
c-----------------------------------------------------------------------
c returns in dd(*) the max absolute value of elements in row *.
c used for scaling purposes. superseded by rnrms  .
c
c on entry:
c n	= dimension of A
c a,ja,ia
c	= matrix stored in compressed sparse row format
c dd	= real*8 array of length n. On output,entry dd(i) contains
c	  the element of row i that has the largest absolute value.
c	  Moreover the sign of dd is modified such that it is the
c	  same as that of the diagonal element in row i.
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
c local variables
      integer k2, i, k1, k
      real*8 t, t1, t2
c
c initialize 
c
      k2 = 1
      do 11 i=1,n
         k1 = k2
         k2 = ia(i+1) - 1
         t = 0.0d0
         do 101  k=k1,k2
            t1 = abs(a(k))
            if (t1 .gt. t) t = t1
            if (ja(k) .eq. i) then 
               if (a(k) .ge. 0.0) then 
                  t2 = a(k) 
               else 
                  t2 = - a(k)
               endif
            endif
 101     continue		
         dd(i) =  t2*t
c     we do not invert diag
 11   continue
      return
c---------end of retmx -------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine rightblk(n, nrow, ncol, a, ja, ia)
      implicit none
      integer n, nrow, ncol, ja(1:*), ia(1:*)
      real*8  a(1:*)
      integer i, k
      nrow = 1 + (n-1)/2
      ncol = 1 + n/2
      k = 1
      do 10 i = 1, nrow
         ia(i) = k
         if (nrow.eq.ncol) then
            if (i.gt.1) then
               ja(k) = i-1
               a(k) = -1.0
               k = k+1
            end if
         end if
         ja(k) = i
         a(k) = -1.0
         k = k+1
         if (nrow.ne.ncol) then
            ja(k) = i+1
            a(k) = -1.0
            k = k+1
         end if
 10   continue
      ia(nrow+1) = k
c---------end-of-rightblk----------------------------------------------- 
      end
c----------------------------------------------------------------------
      subroutine rndperm(n,iord,iseed) 
      integer n, iseed, iord(n) 
c----------------------------------------------------------------------
c this subroutine will generate a pseudo random permutation of the
c n integers 1,2, ...,n.
c iseed is the initial seed. any integer.
c----------------------------------------------------------------------
c local
c
      integer i, j, itmp 
c----------------------------------------------------------------------
      do j=1, n
         iord(j) = j
      enddo
c
      do i=1, n
         j = mod(irand(0),n) + 1
         itmp = iord(i) 
         iord(i) = iord(j) 
         iord(j) = itmp
      enddo
c----------------------------------------------------------------------
      return
c----------------------------------------------------------------------
      end 
c-----------------------------------------------------------------------
      subroutine rnrms   (nrow, nrm, a, ja, ia, diag) 
      real*8 a(*), diag(nrow), scal 
      integer ja(*), ia(nrow+1) 
c-----------------------------------------------------------------------
c gets the norms of each row of A. (choice of three norms)
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c on return:
c----------
c
c diag = real vector of length nrow containing the norms
c
c-----------------------------------------------------------------
      do 1 ii=1,nrow
c
c     compute the norm if each element.
c     
         scal = 0.0d0
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         if (nrm .eq. 0) then
            do 2 k=k1, k2
               scal = max(scal,abs(a(k) ) ) 
 2          continue
         elseif (nrm .eq. 1) then
            do 3 k=k1, k2
               scal = scal + abs(a(k) ) 
 3          continue
         else
            do 4 k=k1, k2
               scal = scal+a(k)**2
 4          continue
         endif 
         if (nrm .eq. 2) scal = sqrt(scal) 
         diag(ii) = scal
 1    continue
      return
c-----------------------------------------------------------------------
c-------------end-of-rnrms----------------------------------------------
      end 
c----------------------------------------------------------------------- 
      subroutine roscal(nrow,job,nrm,a,ja,ia,diag,b,jb,ib,ierr) 
      real*8 a(*), b(*), diag(nrow) 
      integer nrow,job,nrm,ja(*),jb(*),ia(nrow+1),ib(nrow+1),ierr 
c-----------------------------------------------------------------------
c scales the rows of A such that their norms are one on return
c 3 choices of norms: 1-norm, 2-norm, max-norm.
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow	= integer. The row dimension of A
c
c job   = integer. job indicator. Job=0 means get array b only
c         job = 1 means get b, and the integer arrays ib, jb.
c
c nrm   = integer. norm indicator. nrm = 1, means 1-norm, nrm =2
c                  means the 2-nrm, nrm = 0 means max norm
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c on return:
c----------
c
c diag = diagonal matrix stored as a vector containing the matrix
c        by which the rows have been scaled, i.e., on return 
c        we have B = Diag*A.
c
c b, 
c jb, 
c ib	= resulting matrix B in compressed sparse row sparse format.
c	    
c ierr  = error message. ierr=0     : Normal return 
c                        ierr=i > 0 : Row number i is a zero row.
c Notes:
c-------
c 1)        The column dimension of A is not needed. 
c 2)        algorithm in place (B can take the place of A).
c-----------------------------------------------------------------
      call rnrms (nrow,nrm,a,ja,ia,diag)
      ierr = 0
      do 1 j=1, nrow
         if (diag(j) .eq. 0.0d0) then
            ierr = j 
            return
         else
            diag(j) = 1.0d0/diag(j)
         endif
 1    continue
      call diamua(nrow,job,a,ja,ia,diag,b,jb,ib)
      return
c-------end-of-roscal---------------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine rperm (nrow,a,ja,ia,ao,jao,iao,perm,job)
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(nrow),job
      real*8 a(*),ao(*) 
c-----------------------------------------------------------------------
c this subroutine permutes the rows of a matrix in CSR format. 
c rperm  computes B = P A  where P is a permutation matrix.  
c the permutation P is defined through the array perm: for each j, 
c perm(j) represents the destination row number of row number j. 
c Youcef Saad -- recoded Jan 28, 1991.
c-----------------------------------------------------------------------
c on entry:
c----------
c n 	= dimension of the matrix
c a, ja, ia = input matrix in csr format
c perm 	= integer array of length nrow containing the permutation arrays
c	  for the rows: perm(i) is the destination of row i in the
c         permuted matrix. 
c         ---> a(i,j) in the original matrix becomes a(perm(i),j) 
c         in the output  matrix.
c
c job	= integer indicating the work to be done:
c 		job = 1	permute a, ja, ia into ao, jao, iao 
c                       (including the copying of real values ao and
c                       the array iao).
c 		job .ne. 1 :  ignore real values.
c                     (in which case arrays a and ao are not needed nor
c                      used).
c
c------------
c on return: 
c------------ 
c ao, jao, iao = input matrix in a, ja, ia format
c note : 
c        if (job.ne.1)  then the arrays a and ao are not used.
c----------------------------------------------------------------------c
c           Y. Saad, May  2, 1990                                      c
c----------------------------------------------------------------------c
      logical values
      values = (job .eq. 1) 
c     
c     determine pointers for output matix. 
c     
      do 50 j=1,nrow
         i = perm(j)
         iao(i+1) = ia(j+1) - ia(j)
 50   continue
c
c get pointers from lengths
c
      iao(1) = 1
      do 51 j=1,nrow
         iao(j+1)=iao(j+1)+iao(j)
 51   continue
c
c copying 
c
      do 100 ii=1,nrow
c
c old row = ii  -- new row = iperm(ii) -- ko = new pointer
c        
         ko = iao(perm(ii)) 
         do 60 k=ia(ii), ia(ii+1)-1 
            jao(ko) = ja(k) 
            if (values) ao(ko) = a(k)
            ko = ko+1
 60      continue
 100  continue
c
      return
c---------end-of-rperm ------------------------------------------------- 
c-----------------------------------------------------------------------
      end
c---------end of readunf ----------------------------------------------
      subroutine runrc(n,rhs,sol,ipar,fpar,wk,guess,a,ja,ia,
     +     au,jau,ju,solver)
      implicit none
      integer n,ipar(16),ia(n+1),ja(*),ju(*),jau(*)
      real*8 fpar(16),rhs(n),sol(n),guess(n),wk(*),a(*),au(*)
      external solver
c-----------------------------------------------------------------------
c     the actual tester. It starts the iterative linear system solvers
c     with a initial guess suppied by the user.
c
c     The structure {au, jau, ju} is assumed to have the output from
c     the ILU* routines in ilut.f.
c
c-----------------------------------------------------------------------
c     local variables
c
      integer i, iou, its
      real*8 res, dnrm2
c     real dtime, dt(2), time
c     external dtime
      external dnrm2
      save its,res
c
c     ipar(2) can be 0, 1, 2, please don't use 3
c
      if (ipar(2).gt.2) then
         print *, 'I can not do both left and right preconditioning.'
         return
      endif
c
c     normal execution
c
      its = 0
      res = 0.0D0
c
      do i = 1, n
         sol(i) = guess(i)
      enddo
c
      iou = 6
      ipar(1) = 0
c     time = dtime(dt)
 10   call solver(n,rhs,sol,ipar,fpar,wk)
c
c     output the residuals
c
      if (ipar(7).ne.its) then
         write (iou, *) its, real(res)
         its = ipar(7)
      endif
      res = fpar(5)
c
      if (ipar(1).eq.1) then
         call amux(n, wk(ipar(8)), wk(ipar(9)), a, ja, ia)
         goto 10
      else if (ipar(1).eq.2) then
         call atmux(n, wk(ipar(8)), wk(ipar(9)), a, ja, ia)
         goto 10
      else if (ipar(1).eq.3 .or. ipar(1).eq.5) then
         call lusol(n,wk(ipar(8)),wk(ipar(9)),au,jau,ju)
         goto 10
      else if (ipar(1).eq.4 .or. ipar(1).eq.6) then
         call lutsol(n,wk(ipar(8)),wk(ipar(9)),au,jau,ju)
         goto 10
      else if (ipar(1).le.0) then
         if (ipar(1).eq.0) then
            print *, 'Iterative sovler has satisfied convergence test.'
         else if (ipar(1).eq.-1) then
            print *, 'Iterative solver has iterated too many times.'
         else if (ipar(1).eq.-2) then
            print *, 'Iterative solver was not given enough work space.'
            print *, 'The work space should at least have ', ipar(4),
     &           ' elements.'
         else if (ipar(1).eq.-3) then
            print *, 'Iterative sovler is facing a break-down.'
         else
            print *, 'Iterative solver terminated. code =', ipar(1)
         endif
      endif
c     time = dtime(dt)
      write (iou, *) ipar(7), real(fpar(6))
      write (iou, *) '# retrun code =', ipar(1),
     +     '	convergence rate =', fpar(7)
c     write (iou, *) '# total execution time (sec)', time
c
c     check the error
c
      call amux(n,sol,wk,a,ja,ia)
      do i = 1, n
         wk(n+i) = sol(i) -1.0D0
         wk(i) = wk(i) - rhs(i)
      enddo
      write (iou, *) '# the actual residual norm is', dnrm2(n,wk,1)
      write (iou, *) '# the error norm is', dnrm2(n,wk(1+n),1)
c
      if (iou.ne.6) close(iou)
      return
      end
c-----------------------------------------------------------------------
      subroutine rversp (n, riord)
      integer n, riord(n)
c-----------------------------------------------------------------------
c     this routine does an in-place reversing of the permutation array
c     riord --
c-----------------------------------------------------------------------
      integer j, k
      do 26 j=1,n/2
         k = riord(j)
         riord(j) = riord(n-j+1)
         riord(n-j+1) = k
 26   continue
      return
      end
c-----------------------------------------------------------------------
      subroutine skit (n, a, ja, ia, ifmt, iounit, ierr)
c-----------------------------------------------------------------------
c     Writes a matrix in Compressed Sparse Row format to an I/O unit.
c     It tryes to pack as many number as possible into lines of less than
c     80 characters. Space is inserted in between numbers for separation
c     to avoid carrying a header in the data file. This can be viewed
c     as a simplified Harwell-Boeing format. 
c-----------------------------------------------------------------------
c Modified from subroutine prtmt written by Y. Saad
c-----------------------------------------------------------------------
c on entry:
c---------
c n      = number of rows(columns) in matrix
c a      = real*8 array containing the values of the matrix stored 
c          columnwise
c ja     = integer array of the same length as a containing the column
c          indices of the corresponding matrix elements of array a.
c ia     = integer array of containing the pointers to the beginning of 
c          the row in arrays a and ja.
c ifmt   = integer specifying the format chosen for the real values
c          to be output (i.e., for a, and for rhs-guess-sol if 
c          applicable). The meaning of ifmt is as follows.
c          * if (ifmt .lt. 100) then the D descriptor is used,
c          format Dd.m, in which the length (m) of the mantissa is 
c          precisely the integer ifmt (and d = ifmt+6)
c          * if (ifmt .gt. 100) then prtmt will use the 
c          F- descriptor (format Fd.m) in which the length of the 
c          mantissa (m) is the integer mod(ifmt,100) and the length 
c          of the integer part is k=ifmt/100 (and d = k+m+2)
c          Thus  ifmt= 4   means  D10.4  +.xxxxD+ee    while
c          ifmt=104  means  F7.4   +x.xxxx
c          ifmt=205  means  F9.5   +xx.xxxxx
c          Note: formats for ja, and ia are internally computed.
c     
c iounit = logical unit number where to write the matrix into.
c     
c on return:
c----------
c ierr   = error code, 0 for normal 1 for error in writing to iounit.
c
c on error:
c--------
c     If error is encontacted when writing the matrix, the whole matrix
c     is written to the standard output.
c     ierr is set to 1.
c-----------------------------------------------------------------------
      character ptrfmt*16,indfmt*16,valfmt*20
      integer iounit, n, ifmt, len, nperli, nnz, i, ihead
      integer ja(*), ia(*), ierr
      real*8 a(*)
c--------------
c     compute pointer format
c--------------
      nnz    = ia(n+1)
      len    = int ( alog10(0.1+real(nnz))) + 2
      nnz    = nnz - 1
      nperli = 80/len
      
      print *, ' skit entries:', n, nnz, len, nperli

      if (len .gt. 9) then
         assign 101 to ix
      else
         assign 100 to ix
      endif
      write (ptrfmt,ix) nperli,len
 100  format(1h(,i2,1HI,i1,1h) )
 101  format(1h(,i2,1HI,i2,1h) )
c----------------------------
c     compute ROW index format
c----------------------------
      len    = int ( alog10(0.1+real(n) )) + 2
      nperli = min0(80/len,nnz)
      write (indfmt,100) nperli,len
c---------------------------
c     compute value format
c---------------------------
      if (ifmt .ge. 100) then
         ihead = ifmt/100
         ifmt = ifmt-100*ihead
         len = ihead+ifmt+3
         nperli = 80/len
c     
         if (len .le. 9 ) then
            assign 102 to ix
         elseif (ifmt .le. 9) then
            assign 103 to ix
         else 
            assign 104 to ix
         endif
c     
         write(valfmt,ix) nperli,len,ifmt
 102     format(1h(,i2,1hF,i1,1h.,i1,1h) )
 103     format(1h(,i2,1hF,i2,1h.,i1,1h) )
 104     format(1h(,i2,1hF,i2,1h.,i2,1h) )
C     
      else
         len = ifmt + 7
         nperli = 80/len
c     try to minimize the blanks in the format strings.
         if (nperli .le. 9) then
	    if (len .le. 9 ) then
	       assign 105 to ix
	    elseif (ifmt .le. 9) then
	       assign 106 to ix
	    else 
	       assign 107 to ix
	    endif
	 else 
	    if (len .le. 9 ) then
	       assign 108 to ix
	    elseif (ifmt .le. 9) then
	       assign 109 to ix
	    else 
               assign 110 to ix
            endif
         endif
c-----------
         write(valfmt,ix) nperli,len,ifmt
 105     format(1h(,i1,1hD,i1,1h.,i1,1h) )
 106     format(1h(,i1,1hD,i2,1h.,i1,1h) )
 107     format(1h(,i1,1hD,i2,1h.,i2,1h) )
 108     format(1h(,i2,1hD,i1,1h.,i1,1h) )
 109     format(1h(,i2,1hD,i2,1h.,i1,1h) )
 110     format(1h(,i2,1hD,i2,1h.,i2,1h) )
c     
      endif 	    
c     
c     output the data
c     
      write(iounit, *) n
      write(iounit,ptrfmt,err=1000) (ia(i), i = 1, n+1)
      write(iounit,indfmt,err=1000) (ja(i), i = 1, nnz)
      write(iounit,valfmt,err=1000) ( a(i), i = 1, nnz)
c
c     done, if no trouble is encounted in writing data
c
      ierr = 0
      return
c     
c     if can't write the data to the I/O unit specified, should be able to
c     write everything to standard output (unit 6)
c     
 1000 write(0, *) 'Error, Can''t write data to sepcified unit',iounit
      write(0, *) 'Write the matrix into standard output instead!'
      ierr = 1
      write(6,*) n
      write(6,ptrfmt) (ia(i), i=1, n+1)
      write(6,indfmt) (ja(i), i=1, nnz)
      write(6,valfmt) ( a(i), i=1, nnz)
      return
c----------end of skit ------------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine skyline(n,sym,ja,ia,jao,iao,nsky)
      implicit real*8 (a-h, o-z)
      integer n, ja(*), ia(n+1), jao(*), iao(n+1)
      integer nskyl, nskyu, nsky
      logical sym
c-------------------------------------------------------------------
c this routine computes the number of nonzeros in the skyline storage.
c-------------------------------------------------------------------
c
c On entry :
c-----------
c n     = integer. column dimension of matrix
c ja    = integer array containing the row indices of elements in a
c ia    = integer array containing of length n+1 containing the
c         pointers to the beginning of the columns in arrays a and ja.
c         It is assumed that ia(*) = 1 and ia(n+1) = nzz+1.
c iao   = integer array containing of length n+1 containing the
c         pointers to the beginning of the rows in arrays ao and jao.
c         It is assumed that iao(*) = 1 and iao(n+1) = nzz+1.
c jao   = integer array containing the column indices of elements in ao.
c sym   = logical variable indicating whether or not the matrix is
c         symmetric.
c on return
c----------
c nsky  = number of nonzeros in skyline storage
c-------------------------------------------------------------------
c
c nskyu = skyline storage for upper part
      nskyu = 0
c nskyl = skyline storage for lower part
      nskyl = 0
      do 10 i=1,n
         j0 = ia(i)
         j0r = iao(i)
         
         jminc = ja(j0)
         jminr = jao(j0r)
         if (sym) jminc = jminr
         
         nskyl = nskyl + i-jminr + 1
         nskyu = nskyu + i-jminc + 1
         
 10   continue
      nsky = nskyl+nskyu-n
      if (sym)  nsky = nskyl
      return
      end
c-----------------------------------------------------------------------
      subroutine smms (n,first,last,mode,a,ja,ia,iout)
      integer ia(*), ja(*), n, first, last, mode, iout
      real*8 a(*)
c-----------------------------------------------------------------------
c writes a matrix in Coordinate (SMMS) format -- 
c-----------------------------------------------------------------------
c on entry:
c---------
c n     = integer = size of matrix -- number of rows (columns if matrix
c         is stored columnwise) 
c first  = first row (column) to be output. This routine will output 
c          rows (colums) first to last. 
c last   = last row (column) to be output. 
c mode   = integer giving some information about the storage of the 
c          matrix. A 3-digit decimal number. 'htu' 
c         * u = 0 means that matrix is stored row-wise 
c         * u = 1 means that matrix is stored column-wise 
c         * t = 0 indicates that the matrix is stored in CSR format 
c         * t = 1 indicates that the matrix is stored in MSR format. 
c         * h = ... to be added. 
c a,
c ja,
c ia    =  matrix in CSR or MSR format (see mode) 
c iout  = output unit number.
c
c on return:
c----------
c the output file iout will have written in it the matrix in smms
c (coordinate format)
c
c-----------------------------------------------------------------------
        logical msr, csc 
c
c determine mode ( msr or csr )
c
        msr = .false.
        csc = .false. 
	if (mod(mode,10) .eq. 1) csc = .true.
        if ( (mode/10) .eq. 1) msr = .true. 
        
        write (iout,*) n
      do 2 i=first, last 
         k1=ia(i)
         k2 = ia(i+1)-1
c     write (iout,*) ' row ', i 
         if (msr) write(iout,'(2i6,e22.14)')  i, i, a(i) 
         do 10 k=k1, k2
            if (csc) then
               write(iout,'(2i6,e22.14)')  ja(k), i, a(k)
            else
               write(iout,'(2i6,e22.14)')  i, ja(k), a(k)
            endif 
 10      continue
 2    continue
c----end-of-smms--------------------------------------------------------
c-----------------------------------------------------------------------
      end
      subroutine sobel(n,nrowc,ncolc,c,jc,ic,a,ja,ia,b,jb,ib,nzmax,ierr)
      integer i, n, ia(*), ja(*), ib(*), jb(*)
      integer nrowa, ncola, nrowb, ncolb, nrowc, ncolc, ipos
      integer ic(*), jc(*), offset, ierr
      real*8  a(*), b(*), c(*)
c-----------------------------------------------------------------------
c     This subroutine generates a matrix used in the statistical problem
c     presented by Prof. Sobel. The matrix is formed by a series of
c     small submatrix on or adjancent to the diagonal. The submatrix on
c     the diagonal is square and the size goes like 1, 1, 2, 2, 3, 3,...
c     Each of the diagonal block is a triadiagonal matrix, each of the
c     off-diagonal block is a bidiagonal block. The values of elements
c     in the off-diagonal block are all -1. So are the values of the
c     elements on the sub- and super-diagonal of the blocks on the
c     diagonal. The first element(1,1) of the diagonal block is alternating
c     between 3 and 5, the rest of the diagonal elements (of the block
c     on the diagonal) are 6.
c-----------------------------------------------------------------------
c     This subroutine calls following subroutines to generate the three
c     thypes of submatrices:
c     diagblk -- generates diagonal block.
c     leftblk -- generates the block left of the diagonal one.
c     rightblk-- generates the block right of the diagonal one.
c-----------------------------------------------------------------------
      if (n.lt.2) return

      ipos = 1
      offset = 1
      call diagblk(1, nrowc, ncolc, c, jc, ic)
      do 10 i=2, n-2
         nrowa = nrowc
         ncola = ncolc
         call copmat (nrowc,c,jc,ic,a,ja,ia,1,1)
         call rightblk(i-1, nrowb, ncolb, b,jb,ib)
         call addblk(nrowa,ncola,a,ja,ia,ipos,ipos+offset,1,
     $        nrowb,ncolb,b,jb,ib,nrowc,ncolc,c,jc,ic,nzmax,ierr)
         call leftblk(i,nrowb,ncolb,b,jb,ib)
         call addblk(nrowc,ncolc,c,jc,ic,ipos+offset,ipos,1,
     $        nrowb,ncolb,b,jb,ib,nrowa,ncola,a,ja,ia,nzmax,ierr)
         ipos = ipos + offset
         call diagblk(i,nrowb,ncolb,b,jb,ib)
         call addblk(nrowa,ncola,a,ja,ia,ipos,ipos,1,
     $        nrowb,ncolb,b,jb,ib,nrowc,ncolc,c,jc,ic,nzmax,ierr)
         offset = 1 + (i-1)/2
 10   continue
      end
c-----------------------------------------------------------------------
      subroutine sskssr (n,imod,asky,isky,ao,jao,iao,nzmax,ierr)
      real*8 asky(*),ao(nzmax) 
      integer n, imod,nzmax,ierr, isky(n+1),iao(n+1),jao(nzmax) 
c----------------------------------------------------------------------- 
c     Symmetric Skyline Format  to  Symmetric Sparse Row format.
c----------------------------------------------------------------------- 
c  tests for exact zeros in skyline matrix (and ignores them in
c  output matrix).  In place routine (a, isky :: ao, iao)
c----------------------------------------------------------------------- 
c this subroutine translates a  symmetric skyline format into a 
c symmetric sparse row format. Each element is tested to see if it is
c a zero element. Only the actual nonzero elements are retained. Note 
c that the test used is simple and does take into account the smallness 
c of a value. the subroutine filter (see unary module) can be used
c for this purpose. 
c----------------------------------------------------------------------- 
c Coded by Y. Saad, Oct 5, 1989. Revised Feb 18, 1991./
c----------------------------------------------------------------------- 
c
c on entry:
c----------
c n	= integer equal to the dimension of A.	
c imod  = integer indicating the variant of skyline format used:
c         imod = 0 means the pointer iao points to the `zeroth' 
c         element of the row, i.e., to the position of the diagonal
c         element of previous row (for i=1, iao(1)= 0)
c         imod = 1 means that itpr points to the beginning of the row. 
c         imod = 2 means that iao points to the end of the row 
c                  (diagonal element) 
c asky  = real array containing the values of the matrix. asky contains 
c         the sequence of active rows from i=1, to n, an active row 
c         being the row of elemnts of the matrix contained between the 
c         leftmost nonzero element and the diagonal element. 
c isky 	= integer array of size n+1 containing the pointer array to 
c         each row. isky (k) contains the address of the beginning of the
c         k-th active row in the array asky. 
c nzmax = integer. equal to the number of available locations in the 
c         output array ao.  
c
c on return:
c ---------- 
c ao	= real array of size nna containing the nonzero elements
c jao	= integer array of size	nnz containing the column positions
c 	  of the corresponding elements in a.
c iao	= integer of size n+1. iao(k) contains the position in a, ja of
c	  the beginning of the k-th row.
c ierr  = integer. Serving as error message. If the length of the 
c         output arrays ao, jao exceeds nzmax then ierr returns 
c         the row number where the algorithm stopped: rows
c         i, to ierr-1 have been processed succesfully.
c         ierr = 0 means normal return.
c         ierr = -1  : illegal value for imod
c Notes:
c------- 
c This module is in place: ao and iao can be the same as asky, and isky.
c-----------------------------------------------------------------------
c local variables
      integer next, kend, kstart, i, j 
      ierr = 0
c
c check for validity of imod
c 
      if (imod.ne.0 .and. imod.ne.1 .and. imod .ne. 2) then
         ierr =-1
         return
      endif 
c
c next  = pointer to next available position in output matrix
c kend  = pointer to end of current row in skyline matrix. 
c
      next = 1
c
c set kend = start position -1 in  skyline matrix.
c 
      kend = 0 
      if (imod .eq. 1) kend = isky(1)-1
      if (imod .eq. 0) kend = isky(1) 
c
c loop through all rows
c     
      do 50 i=1,n
c
c save value of pointer to ith row in output matrix
c
         iao(i) = next
c
c get beginnning and end of skyline  row 
c
         kstart = kend+1
         if (imod .eq. 0) kend = isky(i+1)
         if (imod .eq. 1) kend = isky(i+1)-1
         if (imod .eq. 2) kend = isky(i) 
c 
c copy element into output matrix unless it is a zero element.
c 
         do 40 k=kstart,kend
            if (asky(k) .eq. 0.0d0) goto 40
            j = i-(kend-k) 
            jao(next) = j
            ao(next)  = asky(k)
            next=next+1
            if (next .gt. nzmax+1) then
               ierr = i
               return
            endif 
 40      continue
 50    continue
      iao(n+1) = next
      return
c-------------end-of-sskssr -------------------------------------------- 
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine ssrcsr(job, value2, nrow, a, ja, ia, nzmax,
     &                  ao, jao, iao, indu, iwk, ierr)
c     .. Scalar Arguments ..
      integer            ierr, job, nrow, nzmax, value2
c     ..
c     .. Array Arguments ..
      integer            ia(nrow+1), iao(nrow+1), indu(nrow),
     &                   iwk(nrow+1), ja(*), jao(nzmax)
      real*8             a(*), ao(nzmax)
c     ..
c-----------------------------------------------------------------------
c     Symmetric Sparse Row to Compressed Sparse Row format
c-----------------------------------------------------------------------
c     This subroutine converts a given matrix in SSR format to regular
c     CSR format by computing Ao = A + A' - diag(A), where A' is A
c     transpose.
c
c     Typically this routine is used to expand the SSR matrix of
c     Harwell Boeing matrices, or to obtain a symmetrized graph of
c     unsymmetric matrices.
c
c     This routine is inplace, i.e., (Ao,jao,iao) may be same as
c     (a,ja,ia).
c
c     It is possible to input an arbitrary CSR matrix to this routine,
c     since there is no syntactical difference between CSR and SSR
c     format. It also removes duplicate entries and perform a partial
c     ordering. The output matrix has an order of lower half, main
c     diagonal and upper half after the partial ordering.
c-----------------------------------------------------------------------
c on entry:
c---------
c
c job   = options
c         0 -- duplicate entries are not removed. If the input matrix is
c              SSR (not an arbitary CSR) matrix, no duplicate entry should
c              arise from this routine.
c         1 -- eliminate duplicate entries, zero entries.
c         2 -- eliminate duplicate entries and perform partial ordering.
c         3 -- eliminate duplicate entries, sort the entries in the
c              increasing order of clumn indices.
c              
c value2= will the values of A be copied?
c         0 -- only expand the graph (a, ao are not touched)
c         1 -- expand the matrix with the values.
c
c nrow  = column dimension of inout matrix
c a,
c ia,
c ja    = matrix in compressed sparse row format.
c
c nzmax = size of arrays ao and jao. SSRCSR will abort if the storage
c          provided in ao, jao is not sufficient to store A. See ierr.
c
c on return:
c----------
c ao, jao, iao
c       = output matrix in compressed sparse row format. The resulting
c         matrix is symmetric and is equal to A+A'-D. ao, jao, iao,
c         can be the same as a, ja, ia in the calling sequence.
c
c indu  = integer array of length nrow. INDU will contain pointers
c         to the beginning of upper traigular part if job > 1.
c         Otherwise it is also used as a work array (size nrow).
c
c iwk   = integer work space (size nrow+1).
c
c ierr  = integer. Serving as error message. If the length of the arrays
c         ao, jao exceeds nzmax, ierr returns the minimum value
c         needed for nzmax. otherwise ierr=0 (normal return).
c
c-----------------------------------------------------------------------
c     .. Local Scalars ..
      integer            i, ipos, j, k, kfirst, klast, ko, kosav, nnz
      real*8             tmp
c     ..
c     .. Executable Statements ..
      ierr = 0
      do 10 i = 1, nrow
         indu(i) = 0
         iwk(i) = 0
 10   continue
      iwk(nrow+1) = 0
c
c     .. compute number of elements in each row of (A'-D)
c     put result in iwk(i+1)  for row i.
c
      do 30 i = 1, nrow
         do 20 k = ia(i), ia(i+1) - 1
            j = ja(k)
            if (j.ne.i)
     &         iwk(j+1) = iwk(j+1) + 1
 20      continue
 30   continue
c
c     .. find addresses of first elements of ouput matrix. result in iwk
c
      iwk(1) = 1
      do 40 i = 1, nrow
         indu(i) = iwk(i) + ia(i+1) - ia(i)
         iwk(i+1) = iwk(i+1) + indu(i)
         indu(i) = indu(i) - 1
 40   continue
c.....Have we been given enough storage in ao, jao ?
      nnz = iwk(nrow+1) - 1
      if (nnz.gt.nzmax) then
         ierr = nnz
         return
      endif
c
c     .. copy the existing matrix (backwards).
c
      kosav = iwk(nrow+1)
      do 60 i = nrow, 1, -1
         klast = ia(i+1) - 1
         kfirst = ia(i)
         iao(i+1) = kosav
         kosav = iwk(i)
         ko = iwk(i) - kfirst
         iwk(i) = ko + klast + 1
         do 50 k = klast, kfirst, -1
            if (value2.ne.0)
     &         ao(k+ko) = a(k)
            jao(k+ko) = ja(k)
 50      continue
 60   continue
      iao(1) = 1
c
c     now copy (A'-D). Go through the structure of ao, jao, iao
c     that has already been copied. iwk(i) is the address
c     of the next free location in row i for ao, jao.
c
      do 80 i = 1, nrow
         do 70 k = iao(i), indu(i)
            j = jao(k)
            if (j.ne.i) then
               ipos = iwk(j)
               if (value2.ne.0)
     &            ao(ipos) = ao(k)
               jao(ipos) = i
               iwk(j) = ipos + 1
            endif
 70      continue
 80   continue
      if (job.le.0) return
c
c     .. eliminate duplicate entries --
c     array INDU is used as marker for existing indices, it is also the
c     location of the entry.
c     IWK is used to stored the old IAO array.
c     matrix is copied to squeeze out the space taken by the duplicated
c     entries.
c
      do 90 i = 1, nrow
         indu(i) = 0
         iwk(i) = iao(i)
 90   continue
      iwk(nrow+1) = iao(nrow+1)
      k = 1
      do 120 i = 1, nrow
         iao(i) = k
         ipos = iwk(i)
         klast = iwk(i+1)
 100     if (ipos.lt.klast) then
            j = jao(ipos)
            if (indu(j).eq.0) then
c     .. new entry ..
               if (value2.ne.0) then
                  if (ao(ipos) .ne. 0.0D0) then
                     indu(j) = k
                     jao(k) = jao(ipos)
                     ao(k) = ao(ipos)
                     k = k + 1
                  endif
               else
                  indu(j) = k
                  jao(k) = jao(ipos)
                  k = k + 1
               endif
            else if (value2.ne.0) then
c     .. duplicate entry ..
               ao(indu(j)) = ao(indu(j)) + ao(ipos)
            endif
            ipos = ipos + 1
            go to 100
         endif
c     .. remove marks before working on the next row ..
         do 110 ipos = iao(i), k - 1
            indu(jao(ipos)) = 0
 110     continue
 120  continue
      iao(nrow+1) = k
      if (job.le.1) return
c
c     .. partial ordering ..
c     split the matrix into strict upper/lower triangular
c     parts, INDU points to the the beginning of the strict upper part.
c
      do 140 i = 1, nrow
         klast = iao(i+1) - 1
         kfirst = iao(i)
 130     if (klast.gt.kfirst) then
            if (jao(klast).lt.i .and. jao(kfirst).ge.i) then
c     .. swap klast with kfirst ..
               j = jao(klast)
               jao(klast) = jao(kfirst)
               jao(kfirst) = j
               if (value2.ne.0) then
                  tmp = ao(klast)
                  ao(klast) = ao(kfirst)
                  ao(kfirst) = tmp
               endif
            endif
            if (jao(klast).ge.i)
     &         klast = klast - 1
            if (jao(kfirst).lt.i)
     &         kfirst = kfirst + 1
            go to 130
         endif
c
         if (jao(klast).lt.i) then
            indu(i) = klast + 1
         else
            indu(i) = klast
         endif
 140  continue
      if (job.le.2) return
c
c     .. order the entries according to column indices
c     bubble-sort is used
c
      do 190 i = 1, nrow
         do 160 ipos = iao(i), indu(i)-1
            do 150 j = indu(i)-1, ipos+1, -1
               k = j - 1
               if (jao(k).gt.jao(j)) then
                  ko = jao(k)
                  jao(k) = jao(j)
                  jao(j) = ko
                  if (value2.ne.0) then
                     tmp = ao(k)
                     ao(k) = ao(j)
                     ao(j) = tmp
                  endif
               endif
 150        continue
 160     continue
         do 180 ipos = indu(i), iao(i+1)-1
            do 170 j = iao(i+1)-1, ipos+1, -1
               k = j - 1
               if (jao(k).gt.jao(j)) then
                  ko = jao(k)
                  jao(k) = jao(j)
                  jao(j) = ko
                  if (value2.ne.0) then
                     tmp = ao(k)
                     ao(k) = ao(j)
                     ao(j) = tmp
                  endif
               endif
 170        continue
 180     continue
 190  continue
c
      return
c---- end of ssrcsr ----------------------------------------------------
c-----------------------------------------------------------------------
      end
c
      subroutine ssscsr (nrow,a,ja,ia,diag,al,jal,ial,au) 
      real*8 a(*),al(*),diag(*),au(*) 
      integer ja(*),ia(nrow+1),jal(*),ial(nrow+1) 
c-----------------------------------------------------------------------
c Unsymmetric Sparse Skyline   format   to Compressed Sparse Row 
c----------------------------------------------------------------------- 
c this subroutine converts a matrix stored in nonsymmetric sparse 
c skyline format into csr format. The sparse skyline format is 
c described in routine csruss. 
c----------------------------------------------------------------------- 
c On entry
c--------- 
c diag  = array containing the diagonal entries of A
c al,jal,ial = matrix in csr format storing the strict lower 
c              trangular part of A.
c au    = values of strict upper part. 
c
c On return
c --------- 
c nrow  = dimension of the matrix a.
c a     = real array containing the nonzero values of the matrix 
c         stored rowwise.
c ja    = column indices of the values in array a
c ia    = integer array of length n+1 containing the pointers to
c         beginning of each row in arrays a, ja.
c 
c-----------------------------------------------------------------------
c
c count elements in lower part + diagonal 
c 
      do 1 i=1, nrow
         ia(i+1) = ial(i+1)-ial(i)+1
 1    continue
c
c count elements in upper part
c 
      do 3 i=1, nrow
         do 2 k=ial(i), ial(i+1)-1 
            j = jal(k)
            ia(j+1) = ia(j+1)+1
 2       continue 
 3    continue
c---------- compute pointers from lengths ------------------------------
      ia(1) = 1
      do 4 i=1,nrow
         ia(i+1) = ia(i)+ia(i+1)
 4    continue
c
c copy lower part + diagonal 
c 
      do 6 i=1, nrow
         ka = ia(i) 
         do 5 k=ial(i), ial(i+1)-1
            a(ka) = al(k) 
            ja(ka) = jal(k) 
            ka = ka+1
 5       continue
         a(ka) = diag(i) 
         ia(i) = ka+1
 6    continue
c     
c     copy upper part
c     
      do 8 i=1, nrow
         do 7 k=ial(i), ial(i+1)-1
c
c row number
c
            jak = jal(k) 
c
c where element goes
c
            ka = ia(jak) 
            a(ka) = au(k) 
            ja(ka) = i
            ia(jak) = ka+1
 7       continue
 8    continue
c
c readjust ia
c
      do 9 i=nrow,1,-1
         ia(i+1) = ia(i)
 9    continue
      ia(1) = 1
c----------end-of-ssscsr------------------------------------------------
      end
c-----end-of-givens
c-----------------------------------------------------------------------
      logical function stopbis(n,ipar,mvpi,fpar,r,delx,sx)
      implicit none
      integer n,mvpi,ipar(16)
      real*8 fpar(16), r(n), delx(n), sx, distdot
      external distdot
c-----------------------------------------------------------------------
c     function for determining the stopping criteria. return value of
c     true if the stopbis criteria is satisfied.
c-----------------------------------------------------------------------
      if (ipar(11) .eq. 1) then
         stopbis = .true.
      else
         stopbis = .false.
      endif
      if (ipar(6).gt.0 .and. ipar(7).ge.ipar(6)) then
         ipar(1) = -1
         stopbis = .true.
      endif
      if (stopbis) return
c
c     computes errors
c
      fpar(5) = sqrt(distdot(n,r,1,r,1))
      fpar(11) = fpar(11) + 2 * n
      if (ipar(3).lt.0) then
c
c     compute the change in the solution vector
c
         fpar(6) = sx * sqrt(distdot(n,delx,1,delx,1))
         fpar(11) = fpar(11) + 2 * n
         if (ipar(7).lt.mvpi+mvpi+1) then
c
c     if this is the end of the first iteration, set fpar(3:4)
c
            fpar(3) = fpar(6)
            if (ipar(3).eq.-1) then
               fpar(4) = fpar(1) * fpar(3) + fpar(2)
            endif
         endif
      else
         fpar(6) = fpar(5)
      endif
c
c     .. the test is struct this way so that when the value in fpar(6)
c       is not a valid number, STOPBIS is set to .true.
c
      if (fpar(6).gt.fpar(4)) then
         stopbis = .false.
         ipar(11) = 0
      else
         stopbis = .true.
         ipar(11) = 1
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine stripes0 (ip,nlev,il,ndom,iptr)
      integer ip, nlev, il(*), ndom, iptr(*)
c-----------------------------------------------------------------------
c     This routine is a simple level-set partitioner. It scans
c     the level-sets as produced by BFS from one to nlev.
c     each time the number of nodes in the accumulated set of
c     levels traversed exceeds the parameter ip, this set defines 
c     a new subgraph. 
c-------------------------parameter-list---------------------------------
c on entry:
c --------
c ip     = desired number of nodes per subgraph.
c nlev   = number of levels found  as output by BFS
c il     = integer array containing the pointer array for
c          the level data structure as output by BFS. 
c          thus il(lev+1) - il(lev) = the number of 
c          nodes that constitute the level numbe lev.
c on return
c ---------
c ndom   = number of sungraphs found
c iptr   = pointer array for the sugraph data structure. 
c          thus, iptr(idom) points to the first level that 
c          consistutes the subgraph number idom, in the 
c          level data structure. 
c-----------------------------------------------------------------------
      ktr = 0
      iband = 1 
      iptr(iband) = 1 
c-----------------------------------------------------------------------

      do 10 ilev = 1, nlev
         ktr = ktr + il(ilev+1) - il(ilev)
         if (ktr .gt. ip) then
            iband = iband+1 
            iptr(iband) = ilev+1
            ktr = 0
         endif
c
 10   continue
c-----------returning --------------------
      iptr(iband) = nlev + 1 
      ndom = iband-1
      return
c-----------------------------------------------------------------------
c-----end-of-stripes0--------------------------------------------------- 
      end
c----------------------------------------------------------------------- 
      subroutine stripes (nlev,riord,levels,ip,map,mapptr,ndom)
      implicit none
      integer nlev,riord(*),levels(nlev+1),ip,map(*),
     *    mapptr(*), ndom
c-----------------------------------------------------------------------
c    this is a post processor to BFS. stripes uses the output of BFS to 
c    find a decomposition of the adjacency graph by stripes. It fills 
c    the stripes level by level until a number of nodes .gt. ip is 
c    is reached. 
c---------------------------parameters-----------------------------------
c on entry: 
c --------
c nlev   = number of levels as found by BFS 
c riord  = reverse permutation array produced by BFS -- 
c levels = pointer array for the level structure as computed by BFS. If 
c          lev is a level number, and k1=levels(lev),k2=levels(lev+1)-1, 
c          then all the nodes of level number lev are:
c                      riord(k1),riord(k1+1),...,riord(k2) 
c  ip    = number of desired partitions (subdomains) of about equal size.
c 
c on return
c ---------
c ndom     = number of subgraphs (subdomains) found 
c map      = node per processor list. The nodes are listed contiguously
c            from proc 1 to nproc = mpx*mpy. 
c mapptr   = pointer array for array map. list for proc. i starts at 
c            mapptr(i) and ends at mapptr(i+1)-1 in array map.
c-----------------------------------------------------------------------
c local variables. 
c
      integer ib,ktr,ilev,k,nsiz,psiz 
      ndom = 1 
      ib = 1
c to add: if (ip .le. 1) then ...
      nsiz = levels(nlev+1) - levels(1) 
      psiz = (nsiz-ib)/max(1,(ip - ndom + 1)) + 1 
      mapptr(ndom) = ib 
      ktr = 0 
      do 10 ilev = 1, nlev
c
c     add all nodes of this level to domain
c     
         do 3 k=levels(ilev), levels(ilev+1)-1
            map(ib) = riord(k)
            ib = ib+1
            ktr = ktr + 1 
            if (ktr .ge. psiz  .or. k .ge. nsiz) then 
               ndom = ndom + 1
               mapptr(ndom) = ib 
               psiz = (nsiz-ib)/max(1,(ip - ndom + 1)) + 1 
               ktr = 0
            endif
c
 3       continue
 10   continue
      ndom = ndom-1
      return 
      end
c-----------------------------------------------------------------------
c------------------------end-of-csorted---------------------------------
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c                     UNARY SUBROUTINES MODULE                         c
c----------------------------------------------------------------------c
c contents:                                                            c
c----------                                                            c
c submat : extracts a submatrix from a sparse matrix.                  c
c filter : filters elements from a matrix according to their magnitude.c
c filterm: same as above, but for the MSR format                       c
c csort  : sorts the elements in increasing order of columns           c
c clncsr : clean up the CSR format matrix, remove duplicate entry, etc c
c transp : in-place transposition routine (see also csrcsc in formats) c
c copmat : copy of a matrix into another matrix (both stored csr)      c
c msrcop : copies a matrix in MSR format into a matrix in MSR format   c
c getelm : returns a(i,j) for any (i,j) from a CSR-stored matrix.      c
c getdia : extracts a specified diagonal from a matrix.                c
c getl   : extracts lower triangular part                              c
c getu   : extracts upper triangular part                              c
c levels : gets the level scheduling structure for lower triangular    c
c          matrices.                                                   c
c amask  : extracts     C = A mask M                                   c
c rperm  : permutes the rows of a matrix (B = P A)                     c
c cperm  : permutes the columns of a matrix (B = A Q)                  c
c dperm  : permutes both the rows and columns of a matrix (B = P A Q ) c
c dperm1 : general extractiob routine (extracts arbitrary rows)        c
c dperm2 : general submatrix permutation/extraction routine            c
c dmperm : symmetric permutation of row and column (B=PAP') in MSR fmt c
c dvperm : permutes a real vector (in-place)                           c
c ivperm : permutes an integer vector (in-place)                       c
c retmx  : returns the max absolute value in each row of the matrix    c
c diapos : returns the positions of the diagonal elements in A.        c
c extbdg : extracts the main diagonal blocks of a matrix.              c
c getbwd : returns the bandwidth information on a matrix.              c
c blkfnd : finds the block-size of a matrix.                           c
c blkchk : checks whether a given integer is the block size of A.      c
c infdia : obtains information on the diagonals of A.                  c
c amubdg : gets number of nonzeros in each row of A*B (as well as NNZ) c 
c aplbdg : gets number of nonzeros in each row of A+B (as well as NNZ) c
c rnrms  : computes the norms of the rows of A                         c
c cnrms  : computes the norms of the columns of A                      c
c roscal : scales the rows of a matrix by their norms.                 c
c coscal : scales the columns of a matrix by their norms.              c
c addblk : Adds a matrix B into a block of A.                          c
c get1up : Collects the first elements of each row of the upper        c
c          triangular portion of the matrix.                           c
c xtrows : extracts given rows from a matrix in CSR format.            c
c csrkvstr:  Finds block row partitioning of matrix in CSR format      c
c csrkvstc:  Finds block column partitioning of matrix in CSR format   c
c kvstmerge: Merges block partitionings, for conformal row/col pattern c
c----------------------------------------------------------------------c
      subroutine submat (n,job,i1,i2,j1,j2,a,ja,ia,nr,nc,ao,jao,iao)
      integer n,job,i1,i2,j1,j2,nr,nc,ia(*),ja(*),jao(*),iao(*)
      real*8 a(*),ao(*) 
c-----------------------------------------------------------------------
c extracts the submatrix A(i1:i2,j1:j2) and puts the result in 
c matrix ao,iao,jao
c---- In place: ao,jao,iao may be the same as a,ja,ia.
c-------------- 
c on input
c---------
c n	= row dimension of the matrix 
c i1,i2 = two integers with i2 .ge. i1 indicating the range of rows to be
c          extracted. 
c j1,j2 = two integers with j2 .ge. j1 indicating the range of columns 
c         to be extracted.
c         * There is no checking whether the input values for i1, i2, j1,
c           j2 are between 1 and n. 
c a,
c ja,
c ia    = matrix in compressed sparse row format. 
c
c job	= job indicator: if job .ne. 1 then the real values in a are NOT
c         extracted, only the column indices (i.e. data structure) are.
c         otherwise values as well as column indices are extracted...
c         
c on output
c-------------- 
c nr	= number of rows of submatrix 
c nc	= number of columns of submatrix 
c	  * if either of nr or nc is nonpositive the code will quit.
c
c ao,
c jao,iao = extracted matrix in general sparse format with jao containing
c	the column indices,and iao being the pointer to the beginning 
c	of the row,in arrays a,ja.
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
      nr = i2-i1+1
      nc = j2-j1+1
c     
      if ( nr .le. 0 .or. nc .le. 0) return
c     
      klen = 0
c     
c     simple procedure. proceeds row-wise...
c     
      do 100 i = 1,nr
         ii = i1+i-1
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         iao(i) = klen+1
c-----------------------------------------------------------------------
         do 60 k=k1,k2
            j = ja(k)
            if (j .ge. j1 .and. j .le. j2) then
               klen = klen+1
               if (job .eq. 1) ao(klen) = a(k)
               jao(klen) = j - j1+1
            endif
 60      continue
 100  continue
      iao(nr+1) = klen+1
      return
c------------end-of submat---------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine symbound (nx,nelx,ijk,nodcode,node,nint,
     *     iperm,wk,iwk)
c-----------------------------------------------------------------------
c     this routine is a symbolic version of routine bound.
c     
c   nx, nelx, ijk, nodcode, node: see other subroutines
c   iperm = permutation array from old orderin to new ordering,
c   iwk   = reverse permutation array or return.
c   wk	  = real work array
c   On return
c   ijk   = is updated according to new oerdering.
c   nint  = number of interior points.
c 
c-----------------------------------------------------------------------
	implicit real*8  (a-h,o-z)
	dimension ijk(node,1),wk(1),iwk(1),iperm(1),
     *		  nodcode(1)

c put all boundary points at the end, backwards
	nint = 1
	nbound = nx
	do 1 j=1, nx
	if (nodcode(j) .eq. 0) then
	  iperm(nint) = j
	  nint = nint+1
	else
	iperm(nbound) = j
	nbound = nbound-1
	endif
 1	continue
c-------------------------------------------------------------------
	nint = nint-1
c 
c permute the boundary information
c
	do 6 k=1, nx
           iwk(k) = nodcode(k)
 6      continue 
	do 7 k=1,nx
           nodcode(k) = iwk(iperm(k))
 7	continue
c
c get reverse permutation
c
	do 8 k=1, nx
           iwk(iperm(k)) = k
 8	continue
c
c update the elements connectivity matrix
c
	do 10 nel = 1, nelx
	   do 9 j=1, node
	      knod = ijk(j,nel)
              ijk(j,nel) = iwk(knod) 
 9	   continue
 10	continue
	return
	end    
c-----------------------------------------------------------------------
	subroutine symdiric (nx,nint,a,ja,ia, f)
c--------------------------------------------------------------
c this routine takes into account the boundary conditions
c and removes the unnecessary boundary points.
c--------------------------------------------------------------
	implicit real*8  (a-h,o-z)
	dimension a(*),ia(*),ja(*),f(*)
c     call submat from UNARY, with job = 0,
c     meaning no movement of real values.
	call submat (nx,0,1,nint,1,nint,a,ja,ia,nr,nc,a,ja,ia)
	return
c----------- end of symdiric ------------------------------------- 
	end
C***********************************************************************
      SUBROUTINE TBZERO(M,NMOT)
C.......................................................................
C     We initialize to ZERO an integer vector of length NMOT.
C.......................................................................
      DIMENSION M(NMOT)
C.......................................................................
      IF(NMOT.le.0) return
C$DOACROSS if(nmot.gt.500), LOCAL(i)
      DO 1 I=1,NMOT
           M(I)=0
 1         CONTINUE
      RETURN
      END
      subroutine texgrd(npts,ja,ia,xx,yy,munt,size,vsize,hsize,
     *     xleft,bot,job,title,ptitle,ijk,node,nelx,iunt)
c-----------------------------------------------------------------------
      integer npts,iunt,ptitle,ja(*),ia(*), ijk(node,*) 
      character title*(*), munt*2 
      real*8 xx(npts), yy(npts) 
c-----------------------------------------------------------------------
c     allows to have several grids in same picture by calling texgrd 
c     several times and exploiting job and different shifts.  
c-----------------------------------------------------------------------
c     input arguments description :
c     
c npts   = number of rows in matrix
c
c ncol   = number of columns in matrix 
c
c mode   = integer indicating whether the matrix is stored in 
c          CSR mode (mode=0) or CSC mode (mode=1) or MSR mode (mode=2) 
c
c ja     = column indices of nonzero elements when matrix is
c          stored rowise. Row indices if stores column-wise.
c ia     = integer array of containing the pointers to the 
c          beginning of the columns in arrays a, ja.
c
c munt   = units used for sizes : either 'cm' or 'in'
c
c size   = size of the matrix box in 'munt' units 
c
c vsize  = vertical size of the frame containing the picture
c          in 'munt' units 
c
c hsize  = horizontal size of the frame containing the picture 
c          in 'munt' units 
c
c xleft  =  position of left border of matrix in 'munt' units 
c
c bot    =  position of bottom border of matrix in 'munt' units 
c
c job    = job indicator for preamble and post process
c          can be viewed as a 2-digit number job = [job1,job2]      
c          where job1 = job /10 , job2 = job - 10*job1 = mod(job,10)
c          job2  relates to preamble/post processing:
c          job2 = 0: all preambles+end-document lines 
c          job2 = 1: preamble only
c          job2 = 2: end-document only
c          anything else:  no preamble or end-docuiment lines
c       Useful for plotting several matrices in same frame.
c
c          job1 relates to the way in which the nodes and elements must
c          be processed:
c          job1  relates to options for the plot. 
c          job1 = 0 : only a filled circle for the nodes, no labeling 
c          job1 = 1 : labels the nodes (in a circle) 
c          job1 = 2 : labels both nodes and elements. 
c          job1 = 3 : no circles, no labels 
c
c title  = character*(*). a title of arbitrary length to be printed 
c          as a caption to the matrix. Can be a blank character if no
c          caption is desired. Can be put on top or bottom of matrix
c          se ptitle.
c
c ptitle = position of title; 0 under the frame, else above
c
c nlines = number of separation lines to draw for showing a partionning
c          of the matrix. enter zero if no partition lines are wanted.
c
c lines  = integer array of length nlines containing the coordinates of 
c          the desired partition lines . The partitioning is symmetric: 
c          a horizontal line across the matrix will be drawn in 
c          between rows lines(i) and lines(i)+1 for i=1, 2, ..., nlines
c          an a vertical line will be similarly drawn between columns
c          lines(i) and lines(i)+1 for i=1,2,...,nlines 
c
c iunt   = logical unit number where to write the matrix into.
c----------------------------------------------------------------------- 
      real*8 xmin, xmax, ymin, ymax

      n = npts 
      siz = size
      job1 = job /10 
      job2 = job - 10*job1 
c
c     get max and min dimensions
c
      xmin = xx(1) 
      xmax = xmin 
      ymin = yy(1)
      ymax = ymin
c
      do j=2, npts
         xmax = max(xmax,xx(j))
         xmin = min(xmin,xx(j))
         ymax = max(ymax,yy(j))
         ymin = min(ymin,yy(j))
      enddo
c----------------------------------------------------------------------- 
      n = npts
      xdim = xmax -xmin
      ydim = ymax -ymin
      dimen = max(xdim,ydim) 
c-----------------------------------------------------------------------
      print *, ' xmin', xmin, ' xmax', xmax 
      print *, ' ymin', ymin, ' ymax', ymax, ' dimen ', dimen
c-----------------------------------------------------------------------
c
c units (cm or in) to dot conversion factor and paper size
c 
      tdim = max(ydim,xdim) 
      unit0 = size/tdim 
      hsiz = hsize/unit0
      vsiz = vsize/unit0
      siz  = size/unit0 
c
c     size of little circle for each node -- cirr = in local units
c     cirabs in inches (or cm) -- rad = radius in locl units --
c 
      cirabs = 0.15
      if (job1 .le. 0) cirabs = 0.08
      if (job1 .eq. 3) cirabs = 0.0 
      cirr   = cirabs/unit0 
      rad = cirr/2.0
c
c     begin document generation
c 
      if (job2 .le. 1) then 
         write (iunt,*) ' \\documentstyle[epic,eepic,12pt]{article} '
         write (iunt,*) ' \\begin{document}'
         write (iunt,100) unit0, munt 
         write (iunt,99) hsiz, vsiz 
      else 
c     redeclare unitlength 
         write (iunt,100) unit0, munt 
      endif 
      if (job1 .le. 0) then 
         write (iunt,101) cirr
      else
         write (iunt,102) cirr
      endif 
 102  format('\\def\\cird{\\circle{',f5.2,'} }') 
 101   format('\\def\\cird{\\circle*{',f5.2,'} }') 
 99   format('\\begin{picture}(',f8.2,1h,,f8.2,1h)) 
 100  format(' \\setlength{\\unitlength}{',f5.3,a2,'}')
c
c     bottom margin between cir and title
c
      xs = xleft /  unit0 + (tdim - xdim)*0.5 + (hsiz - siz)*0.5 
      ys = bot / unit0 - ymin
c
      xmargin = 0.30/unit0
      if (munt .eq. 'cm' .or. munt .eq. 'CM') xmargin = xmargin*2.5
      xtit = xs + xmin 
      if (ptitle .eq. 0) then 
         ytit = ys + ymin - xmargin 
      else
         ytit = ys + ymax + xmargin 
      endif 
      ltit = LENSTR(title)
      write(iunt,111) xtit,ytit,xdim,xmargin,title(1:ltit) 
c      
 111  format ('\\put(',F6.2,',',F6.2,
     *     '){\\makebox(',F6.2,1h,,F6.2,'){',A,2h}}) 
c      
c     print all the circles if needed
c 
c ##### temporary for showing f
c      write (iunt,102) 0.01      
c      write (iunt,112)xs+xx(1), ys+yy(1) 
c-----------------------------------------------------------------------
      if (job1 .eq. 3) goto 230
      do 22 i=1, npts 
         x = xs + xx(i) 
         y = ys + yy(i) 
         write(iunt,112) x, y 
 112     format ('\\put(',F6.2,',',F6.2,'){\\cird}')
c         write(iunt,113) x-rad, y-rad, cirr, cirr, i 
c 113     format ('\\put(',F6.2,',',F6.2,
c     *     '){\\makebox(',F6.2,1h,,F6.2,'){\\scriptsize ',i4,2h}}) 
         if (job1 .ge. 1) then 
            write(iunt,113) x, y, i 
         endif
 113     format ('\\put(',F6.2,',',F6.2,
     *        '){\\makebox(0.0,0.0){\\scriptsize ',i4,2h}}) 
 22   continue
 230  continue 
c         
c     number the elements if needed
c
      if (job1 .eq. 2) then
         do 23 iel = 1, nelx 
            x = 0.0
            y = 0.0
            do j=1, node
               x = x+xx(ijk(j,iel)) 
               y = y+yy(ijk(j,iel)) 
            enddo
            x = xs + x / real(node) 
            y = ys + y / real(node) 
            write(iunt,113) x, y, iel 
 23      continue
      endif 
c
c     draw lines
c
      write (iunt,*) ' \\Thicklines '
      do 1 ii=1, npts 
         xi = xs+ xx(ii) 
         yi = ys+ yy(ii) 
         do 2 k=ia(ii),ia(ii+1)-1 
            j = ja(k) 
            if (j .le. ii) goto 2
            xj = xs + xx(j) 
            yj = ys + yy(j) 
            xspan = xj - xi
            yspan = yj - yi
            tlen = sqrt(xspan**2 + yspan**2) 
            if (abs(xspan) .gt. abs(yspan)) then
               ss = yspan / tlen
               cc = sqrt(abs(1.0 - ss**2))
               cc = sign(cc,xspan) 
            else
               cc = xspan / tlen 
               ss = sqrt(abs(1.0 - cc**2))
               ss = sign(ss,yspan) 
            endif
c            print *, ' ss -- cc ', ss, cc 
c            write(iunt,114)xi,yi,xj,yj
            write(iunt,114)xi+cc*rad,yi+ss*rad,xj-cc*rad,yj-ss*rad 
 114        format('\\drawline(',f6.2,1h,,f6.2,')(',f6.2,1h,,f6.2,1h))
c            tlen = tlen - 2.0*cirr
c            write(iunt,114) xi+cc*cirr,yi+ss*cirr,cc,ss,tlen 
c 114        format('\\put(',f6.2,1h,,f6.2,'){\\line(',
c     *           f6.2,1h,,f6.2,'){',f6.2,'}}') 
 2       continue 
 1    continue
c-----------------------------------------------------------------------
      if (job2 .eq. 0 .or. job2 .eq. 2) then 
         write (iunt,*) ' \\end{picture} '
         write (iunt,*) ' \\end{document} '
      endif
c
      return
      end
      subroutine texplt(nrow,ncol,mode,ja,ia,munt,size,vsize,hsize,
     *     xleft,bot,job,title,ptitle,nlines,lines,iunt)
c-----------------------------------------------------------------------
      integer nrow,ncol,mode,iunt,ptitle,ja(*),ia(*),lines(nlines) 
      character title*(*), munt*2 
c-----------------------------------------------------------------------
c     allows to have several matrices in same picture by calling texplt
c     several times and exploiting job and different shifts.  
c-----------------------------------------------------------------------
c     input arguments description :
c     
c nrow   = number of rows in matrix
c
c ncol   = number of columns in matrix 
c
c mode   = integer indicating whether the matrix is stored in 
c          CSR mode (mode=0) or CSC mode (mode=1) or MSR mode (mode=2) 
c
c ja     = column indices of nonzero elements when matrix is
c          stored rowise. Row indices if stores column-wise.
c ia     = integer array of containing the pointers to the 
c          beginning of the columns in arrays a, ja.
c
c munt   = units used for sizes : either 'cm' or 'in'
c
c size   = size of the matrix box in 'munt' units 
c
c vsize  = vertical size of the frame containing the picture
c          in 'munt' units 
c
c hsize  = horizontal size of the frame containing the picture 
c          in 'munt' units 
c
c xleft  =  position of left border of matrix in 'munt' units 
c
c bot    =  position of bottom border of matrix in 'munt' units 
c
c job    = job indicator for preamble and post process
c          
c          can be thought of as a 2-digit number job = [job1,job2]
c          where job1 = job /10 , job2 = job - 10*job1 = mod(job,10)
c          job2 = 0: all preambles+end-document lines 
c          job2 = 1: preamble only
c          job2 = 2: end-document only
c          anything else for job2:  no preamble or end-docuiment lines
c       Useful for plotting several matrices in same frame.
c
c          job1 indicates what to put for a nonzero dot.
c          job1  relates to preamble/post processing:
c          job1 = 0 : a filled squate 
c          job1 = 1 : a filled circle 
c          job1 = 2 : the message $a_{ij}$ where i,j are the trow/column
c                     positions of the nonzero element. 
c
c title  = character*(*). a title of arbitrary length to be printed 
c          as a caption to the matrix. Can be a blank character if no
c          caption is desired. Can be put on top or bottom of matrix
c          se ptitle.
c
c ptitle = position of title; 0 under the frame, else above
c
c nlines = number of separation lines to draw for showing a partionning
c          of the matrix. enter zero if no partition lines are wanted.
c
c lines  = integer array of length nlines containing the coordinates of 
c          the desired partition lines . The partitioning is symmetric: 
c          a horizontal line across the matrix will be drawn in 
c          between rows lines(i) and lines(i)+1 for i=1, 2, ..., nlines
c          an a vertical line will be similarly drawn between columns
c          lines(i) and lines(i)+1 for i=1,2,...,nlines 
c
c iunt   = logical unit number where to write the matrix into.
c----------------------------------------------------------------------- 
      data haf /0.5/, zero/0.0/, conv/2.54/
c-----------------------------------------------------------------------
      n = ncol 
      if (mode .eq. 0) n = nrow 
      job1 = job /10 
      job2 = job - 10*job1 
      maxdim = max(nrow, ncol)
      rwid = real(ncol-1)
      rht  = real(nrow-1)
      unit0 = size/real(maxdim) 
      hsiz = hsize/unit0
      vsiz = vsize/unit0
      siz  = size/unit0
c     
c     size of little box for each dot -- boxr = in local units
c     boxabs in inches (or cm)
c 
      boxr   = 0.6
      boxabs = unit0*boxr 
c
c     spaces between frame to nearest box
c
      space = 0.03/unit0+(1.0-boxr)/2.0 
c
c     begin document generation
c  for very first call better have \unitlength set first..
      if (job2 .le. 1) then 
         write (iunt,*) ' \\documentstyle[epic,12pt]{article} '
         write (iunt,*) ' \\begin{document}'
         write (iunt,100) unit0, munt 
         write (iunt,99) hsiz, vsiz 
      else 
c     redeclare unitlength 
         write (iunt,100) unit0, munt 
      endif 
c----- always redefine units 

      if (job1 .eq. 0) then
         write (iunt,101) boxabs, boxabs
      else 
         write (iunt,102) boxabs/unit0
      endif 
 100  format(' \\setlength{\\unitlength}{',f5.3,a2,'}') 
 99   format('\\begin{picture}(',f8.2,1h,,f8.2,1h)) 
 101  format('\\def\\boxd{\\vrule height',f7.4,'in width',f7.4,'in }') 
 102  format('\\def\\boxd{\\circle*{',f7.4,'}}')
c
c     draw a frame around the matrix
c     get shifts from real inches to local units
c
      xs = xleft/unit0 + (hsiz-siz)*0.5
      ys = bot/unit0
c     
      eps = 0.0 
      xmin = xs 
      xmax = xs +rwid + boxr + 2.0*space 
      ymin = ys 
      ymax = ys+rht + boxr + 2.0*space
c     
c     bottom margin between box and title
c
      xmargin = 0.30/unit0
      if (munt .eq. 'cm' .or. munt .eq. 'CM') xmargin = xmargin*2.5
      xtit = 0.5*(xmin+xmax) 
      xtit = xmin
      ytit = ymax 
      if (ptitle .eq. 0) ytit = ymin - xmargin 
      xdim = xmax-xmin
      ltit = LENSTR(title)
      write(iunt,111) xtit,ytit,xdim,xmargin,title(1:ltit) 
c      
 111  format ('\\put(',F6.2,',',F6.2,
     *     '){\\makebox(',F6.2,1h,F6.2,'){',A,2h}}) 
c      
      write(iunt,*)  ' \\thicklines'
      write (iunt,108) xmin,ymin,xmax,ymin,xmax,ymax,
     *     xmin,ymax,xmin,ymin
 108  format('\\drawline',1h(,f8.2,1h,,f8.2,1h), 
     *     1h(,f8.2,1h,,f8.2,1h), 1h(,f8.2,1h,,f8.2,1h), 
     *     1h(,f8.2,1h,,f8.2,1h), 1h(,f8.2,1h,,f8.2,1h)) 
c     
c     draw the separation lines 
c     
c      if (job1 .gt.0) then
c         xs = xs + 0.25
c         ys = ys + 0.25
c      endif
      write(iunt,*)  ' \\thinlines'
      do 22 kol=1, nlines 
         isep = lines(kol)
c     
c     horizontal lines 
c     
         yy =  ys + real(nrow-isep)  
         write(iunt,109) xmin, yy, xmax, yy
c     
c     vertical lines 
c     
         xx = xs+real(isep) 
         write(iunt,109) xx, ymin, xx, ymax 
 22   continue
c     
 109  format('\\drawline',
     *     1h(,f8.2,1h,,f8.2,1h), 1h(,f8.2,1h,,f8.2,1h)) 
      
c-----------plotting loop ---------------------------------------------
c     
c     add some space right of the frame and up from the bottom
c     
      xs = xs+space
      ys = ys+space
c-----------------------------------------------------------------------
      do 1 ii=1, n
         istart = ia(ii)
         ilast  = ia(ii+1)-1 
         if (mode .eq. 1) then
            do 2 k=istart, ilast
               if (job1 .le. 1) then               
                  write(iunt,12) xs+real(ii-1),ys+real(nrow-ja(k)) 
               else
                  write(iunt,13) xs+real(ii-1),ys+real(nrow-ja(k)),
     *                 ii,ja(k) 
               endif
 2          continue 
         else
            y = ys+real(nrow-ii)
            do 3 k=istart, ilast
               if (job1 .le. 1) then
                  write(iunt,12) xs+real(ja(k)-1), y 
               else 
                  write(iunt,13) xs+real(ja(k)-1), y, ii, ja(k) 
               endif
 3          continue          
c     add diagonal element if MSR mode.
            if (mode .eq. 2) 
     *           write(iunt,12) xs+real(ii-1), ys+real(nrow-ii) 
         endif
 1    continue
c-----------------------------------------------------------------------
 12   format ('\\put(',F6.2,',',F6.2,')','{\\boxd}') 
 13   format ('\\put(',F6.2,',',F6.2,')','{$a_{',i3,1h,,i3,'}$}')
c-----------------------------------------------------------------------
      if (job2 .eq. 0 .or. job2 .eq. 2) then 
         write (iunt,*) ' \\end{picture} '
         write (iunt,*) ' \\end{document} '
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine tfqmr(n, rhs, sol, ipar, fpar, w)
      implicit none
      integer n, ipar(16)
      real*8 rhs(n), sol(n), fpar(16), w(n,*)
c-----------------------------------------------------------------------
c     TFQMR --- transpose-free Quasi-Minimum Residual method
c     This is developed from BCG based on the principle of Quasi-Minimum
c     Residual, and it is transpose-free.
c
c     It uses approximate residual norm.
c
c     Internally, the fpar's are used as following:
c     fpar(3) --- initial residual norm squared
c     fpar(4) --- target residual norm squared
c     fpar(5) --- current residual norm squared
c
c     w(:,1) -- R, residual
c     w(:,2) -- R0, the initial residual
c     w(:,3) -- W
c     w(:,4) -- Y
c     w(:,5) -- Z
c     w(:,6) -- A * Y
c     w(:,7) -- A * Z
c     w(:,8) -- V
c     w(:,9) -- D
c     w(:,10) -- intermediate results of preconditioning
c     w(:,11) -- changes in the solution
c-----------------------------------------------------------------------
c     external functions
c
      real*8 distdot
      logical brkdn
      external brkdn, distdot
c
      real*8 one,zero
      parameter(one=1.0D0,zero=0.0D0)
c
c     local variables
c
      integer i
      logical lp, rp
      real*8 eta,sigma,theta,te,alpha,rho,tao
      save
c
c     status of the call (where to go)
c
      if (ipar(1).le.0) ipar(10) = 0
      goto (10,20,40,50,60,70,80,90,100,110), ipar(10)
c
c     initializations
c
      call bisinit(ipar,fpar,11*n,2,lp,rp,w)
      if (ipar(1).lt.0) return
      ipar(1) = 1
      ipar(8) = 1
      ipar(9) = 1 + 6*n
      do i = 1, n
         w(i,1) = sol(i)
      enddo
      ipar(10) = 1
      return
 10   ipar(7) = ipar(7) + 1
      ipar(13) = ipar(13) + 1
      do i = 1, n
         w(i,1) = rhs(i) - w(i,7)
         w(i,9) = zero
      enddo
      fpar(11) = fpar(11) + n
c
      if (lp) then
         ipar(1) = 3
         ipar(9) = n+1
         ipar(10) = 2
         return
      endif
 20   continue
      if (lp) then
         do i = 1, n
            w(i,1) = w(i,2)
            w(i,3) = w(i,2)
         enddo
      else
         do i = 1, n
            w(i,2) = w(i,1)
            w(i,3) = w(i,1)
         enddo
      endif
c
      fpar(5) = sqrt(distdot(n,w,1,w,1))
      fpar(3) = fpar(5)
      tao = fpar(5)
      fpar(11) = fpar(11) + n + n
      if (abs(ipar(3)).eq.2) then
         fpar(4) = fpar(1) * sqrt(distdot(n,rhs,1,rhs,1)) + fpar(2)
         fpar(11) = fpar(11) + n + n
      else if (ipar(3).ne.999) then
         fpar(4) = fpar(1) * tao + fpar(2)
      endif
      te = zero
      rho = zero
c
c     begin iteration
c
 30   sigma = rho
      rho = distdot(n,w(1,2),1,w(1,3),1)
      fpar(11) = fpar(11) + n + n
      if (brkdn(rho,ipar)) goto 900
      if (ipar(7).eq.1) then
         alpha = zero
      else
         alpha = rho / sigma
      endif
      do i = 1, n
         w(i,4) = w(i,3) + alpha * w(i,5)
      enddo
      fpar(11) = fpar(11) + n + n
c
c     A * x -- with preconditioning
c
      if (rp) then
         ipar(1) = 5
         ipar(8) = 3*n + 1
         if (lp) then
            ipar(9) = 5*n + 1
         else
            ipar(9) = 9*n + 1
         endif
         ipar(10) = 3
         return
      endif
c
 40   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = 3*n + 1
      endif
      if (lp) then
         ipar(9) = 9*n + 1
      else
         ipar(9) = 5*n + 1
      endif
      ipar(10) = 4
      return
c
 50   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = 5*n + 1
         ipar(10) = 5
         return
      endif
 60   ipar(7) = ipar(7) + 1
      do i = 1, n
         w(i,8) = w(i,6) + alpha * (w(i,7) + alpha * w(i,8))
      enddo
      sigma = distdot(n,w(1,2),1,w(1,8),1)
      fpar(11) = fpar(11) + 6 * n
      if (brkdn(sigma,ipar)) goto 900
      alpha = rho / sigma
      do i = 1, n
         w(i,5) = w(i,4) - alpha * w(i,8)
      enddo
      fpar(11) = fpar(11) + 2*n
c
c     the second A * x
c
      if (rp) then
         ipar(1) = 5
         ipar(8) = 4*n + 1
         if (lp) then
            ipar(9) = 6*n + 1
         else
            ipar(9) = 9*n + 1
         endif
         ipar(10) = 6
         return
      endif
c
 70   ipar(1) = 1
      if (rp) then
         ipar(8) = ipar(9)
      else
         ipar(8) = 4*n + 1
      endif
      if (lp) then
         ipar(9) = 9*n + 1
      else
         ipar(9) = 6*n + 1
      endif
      ipar(10) = 7
      return
c
 80   if (lp) then
         ipar(1) = 3
         ipar(8) = ipar(9)
         ipar(9) = 6*n + 1
         ipar(10) = 8
         return
      endif
 90   ipar(7) = ipar(7) + 1
      do i = 1, n
         w(i,3) = w(i,3) - alpha * w(i,6)
      enddo
c
c     update I
c
      theta = distdot(n,w(1,3),1,w(1,3),1) / (tao*tao)
      sigma = one / (one + theta)
      tao = tao * sqrt(sigma * theta)
      fpar(11) = fpar(11) + 4*n + 6
      if (brkdn(tao,ipar)) goto 900
      eta = sigma * alpha
      sigma = te / alpha
      te = theta * eta
      do i = 1, n
         w(i,9) = w(i,4) + sigma * w(i,9)
         w(i,11) = w(i,11) + eta * w(i,9)
         w(i,3) = w(i,3) - alpha * w(i,7)
      enddo
      fpar(11) = fpar(11) + 6 * n + 6
      if (ipar(7).eq.1) then
         if (ipar(3).eq.-1) then
            fpar(3) = eta * sqrt(distdot(n,w(1,9),1,w(1,9),1))
            fpar(4) = fpar(1)*fpar(3) + fpar(2)
            fpar(11) = fpar(11) + n + n + 4
         endif
      endif
c
c     update II
c
      theta = distdot(n,w(1,3),1,w(1,3),1) / (tao*tao)
      sigma = one / (one + theta)
      tao = tao * sqrt(sigma * theta)
      fpar(11) = fpar(11) + 8 + 2*n
      if (brkdn(tao,ipar)) goto 900
      eta = sigma * alpha
      sigma = te / alpha
      te = theta * eta
      do i = 1, n
         w(i,9) = w(i,5) + sigma * w(i,9)
         w(i,11) = w(i,11) + eta * w(i,9)
      enddo
      fpar(11) = fpar(11) + 4*n + 3
c
c     this is the correct over-estimate
c      fpar(5) = sqrt(real(ipar(7)+1)) * tao
c     this is an approximation
      fpar(5) = tao
      if (ipar(3).eq.999) then
         ipar(1) = 10
         ipar(8) = 10*n + 1
         ipar(9) = 9*n + 1
         ipar(10) = 9
         return
      else if (ipar(3).lt.0) then
         fpar(6) = eta * sqrt(distdot(n,w(1,9),1,w(1,9),1))
         fpar(11) = fpar(11) + n + n + 2
      else
         fpar(6) = fpar(5)
      endif
      if (fpar(6).gt.fpar(4) .and. (ipar(7).lt.ipar(6)
     +     .or. ipar(6).le.0)) goto 30
 100  if (ipar(3).eq.999.and.ipar(11).eq.0) goto 30
c
c     clean up
c
 900  if (rp) then
         if (ipar(1).lt.0) ipar(12) = ipar(1)
         ipar(1) = 5
         ipar(8) = 10*n + 1
         ipar(9) = ipar(8) - n
         ipar(10) = 10
         return
      endif
 110  if (rp) then
         call tidycg(n,ipar,fpar,sol,w(1,10))
      else
         call tidycg(n,ipar,fpar,sol,w(1,11))
      endif
c
      return
      end
c-----end-of-stopbis
c-----------------------------------------------------------------------
      subroutine tidycg(n,ipar,fpar,sol,delx)
      implicit none
      integer i,n,ipar(16)
      real*8 fpar(16),sol(n),delx(n)
c-----------------------------------------------------------------------
c     Some common operations required before terminating the CG routines
c-----------------------------------------------------------------------
      real*8 zero
      parameter(zero=0.0D0)
c
      if (ipar(12).ne.0) then
         ipar(1) = ipar(12) 
      else if (ipar(1).gt.0) then
         if ((ipar(3).eq.999 .and. ipar(11).eq.1) .or.
     +        fpar(6).le.fpar(4)) then
            ipar(1) = 0
         else if (ipar(7).ge.ipar(6) .and. ipar(6).gt.0) then
            ipar(1) = -1
         else
            ipar(1) = -10
         endif
      endif
      if (fpar(3).gt.zero .and. fpar(6).gt.zero .and.
     +     ipar(7).gt.ipar(13)) then
         fpar(7) = log10(fpar(3) / fpar(6)) / dble(ipar(7)-ipar(13))
      else
         fpar(7) = zero
      endif
      do i = 1, n
         sol(i) = sol(i) + delx(i)
      enddo
      return
      end
c-----------------------------------------------------------------------
      subroutine transp (nrow,ncol,a,ja,ia,iwk,ierr)
      integer nrow, ncol, ia(*), ja(*), iwk(*), ierr
      real*8 a(*) 
c------------------------------------------------------------------------
c In-place transposition routine.
c------------------------------------------------------------------------
c this subroutine transposes a matrix stored in compressed sparse row 
c format. the transposition is done in place in that the arrays a,ja,ia
c of the transpose are overwritten onto the original arrays.
c------------------------------------------------------------------------
c on entry:
c--------- 
c nrow	= integer. The row dimension of A.
c ncol	= integer. The column dimension of A.
c a	= real array of size nnz (number of nonzero elements in A).
c         containing the nonzero elements 
c ja	= integer array of length nnz containing the column positions
c 	  of the corresponding elements in a.
c ia	= integer of size n+1, where n = max(nrow,ncol). On entry
c         ia(k) contains the position in a,ja of  the beginning of 
c         the k-th row.
c
c iwk	= integer work array of same length as ja.
c
c on return:
c----------
c
c ncol	= actual row dimension of the transpose of the input matrix.
c         Note that this may be .le. the input value for ncol, in
c         case some of the last columns of the input matrix are zero
c         columns. In the case where the actual number of rows found
c         in transp(A) exceeds the input value of ncol, transp will
c         return without completing the transposition. see ierr.
c a,
c ja,
c ia	= contains the transposed matrix in compressed sparse
c         row format. The row dimension of a, ja, ia is now ncol.
c
c ierr	= integer. error message. If the number of rows for the
c         transposed matrix exceeds the input value of ncol,
c         then ierr is  set to that number and transp quits.
c         Otherwise ierr is set to 0 (normal return).
c
c Note: 
c----- 1) If you do not need the transposition to be done in place
c         it is preferrable to use the conversion routine csrcsc 
c         (see conversion routines in formats).
c      2) the entries of the output matrix are not sorted (the column
c         indices in each are not in increasing order) use csrcsc
c         if you want them sorted.
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c  modified Oct. 11, 1989.                                             c
c----------------------------------------------------------------------c
c local variables
      real*8 t, t1
      ierr = 0
      nnz = ia(nrow+1)-1
c
c     determine column dimension
c
      jcol = 0
      do 1 k=1, nnz
         jcol = max(jcol,ja(k))
 1    continue
      if (jcol .gt. ncol) then
         ierr = jcol
         return
      endif
c     
c     convert to coordinate format. use iwk for row indices.
c     
      ncol = jcol
c     
      do 3 i=1,nrow
         do 2 k=ia(i),ia(i+1)-1
            iwk(k) = i
 2       continue 
 3    continue
c     find pointer array for transpose. 
      do 35 i=1,ncol+1
         ia(i) = 0
 35   continue
      do 4 k=1,nnz
         i = ja(k)
         ia(i+1) = ia(i+1)+1
 4    continue 
      ia(1) = 1 
c------------------------------------------------------------------------
      do 44 i=1,ncol
         ia(i+1) = ia(i) + ia(i+1)
 44   continue 
c     
c     loop for a cycle in chasing process. 
c     
      init = 1
      k = 0
 5    t = a(init)
      i = ja(init)
      j = iwk(init)
      iwk(init) = -1
c------------------------------------------------------------------------
 6    k = k+1 		
c     current row number is i.  determine  where to go. 
      l = ia(i)
c     save the chased element. 
      t1 = a(l)
      inext = ja(l)
c     then occupy its location.
      a(l)  = t
      ja(l) = j
c     update pointer information for next element to be put in row i. 
      ia(i) = l+1
c     determine  next element to be chased
      if (iwk(l) .lt. 0) goto 65
      t = t1
      i = inext
      j = iwk(l)
      iwk(l) = -1
      if (k .lt. nnz) goto 6
      goto 70
 65   init = init+1
      if (init .gt. nnz) goto 70
      if (iwk(init) .lt. 0) goto 65
c     restart chasing --	
      goto 5
 70   continue
      do 80 i=ncol,1,-1 
         ia(i+1) = ia(i)
 80   continue
      ia(1) = 1
c
      return
c------------------end-of-transp ----------------------------------------
c------------------------------------------------------------------------
      end 
c-----------------------------------------------------------------------
      subroutine udsolc (n,x,y,au,jau)   
      integer n, jau(*) 
      real*8 x(n), y(n), au(*)  
c-----------------------------------------------------------------------
c    Solves     U x = y ;    U = nonunit Up. Triang. MSC format 
c----------------------------------------------------------------------- 
c solves a (non-unit) upper triangular system by standard (sequential) 
c forward elimination - matrix stored in Modified Sparse Column format 
c with diagonal elements already inverted (otherwise do inversion,
c auuuul(1:n) = 1.0/au(1:n),  before calling ldsol).
c-----------------------------------------------------------------------
c
c On entry:
c---------- 
c n      = integer. dimension of problem.
c y      = real*8 array containg the right hand side.
c
c au,
c jau,   = Upper triangular matrix stored in Modified Sparse Column
c          format.
c
c On return:
c----------- 
c	x = The solution of  U x = y .
c--------------------------------------------------------------------
c local variables 
c 
      integer k, j
      real*8 t
c----------------------------------------------------------------------- 
      do 140 k=1,n
         x(k) = y(k) 
 140  continue
      do 150 k = n,1,-1
         x(k) = x(k)*au(k) 
         t = x(k) 
         do 100 j = jau(k), jau(k+1)-1
            x(jau(j)) = x(jau(j)) - t*au(j) 
 100     continue
 150  continue
c
      return
c----------end-of-udsolc------------------------------------------------ 
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine udsol (n,x,y,au,jau) 
      integer n, jau(*) 
      real*8  x(n), y(n),au(*) 
c----------------------------------------------------------------------- 
c             Solves   U x = y  ;   U = upper triangular in MSR format
c-----------------------------------------------------------------------
c solves a non-unit upper triangular matrix by standard (sequential )
c backward elimination - matrix stored in MSR format. 
c with diagonal elements already inverted (otherwise do inversion,
c au(1:n) = 1.0/au(1:n),  before calling).
c-----------------------------------------------------------------------
c
c On entry:
c---------- 
c n      = integer. dimension of problem.
c y      = real array containg the right side.
c
c au,
c jau,    = Lower triangular matrix stored in modified sparse row
c          format. 
c
c On return:
c----------- 
c	x = The solution of  U x = y .
c--------------------------------------------------------------------
c local variables 
c
      integer k, j
      real*8 t
c-----------------------------------------------------------------------
      x(n) = y(n)*au(n)
      do 150 k = n-1,1,-1
         t = y(k) 
         do 100 j = jau(k), jau(k+1)-1
            t = t - au(j)*x(jau(j))
 100     continue
         x(k) = au(k)*t 
 150  continue
c
      return
c----------end-of-udsol-------------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
	subroutine unassbl (a,na,f,nx,nelx,ijk,nodcode,
     *			   node,x,y,ierr,xyk)
c----------------------------------------------------------------------- 
c a      = un-assembled matrix on output
c na	 = 1-st dimension of a.  a(na,node,node)
c
c f      = right hand side (global load vector) in un-assembled form
c nx     = number of nodes at input
c nelx	 = number of elements at input
c ijk	 = connectivity matrix: for node k, ijk(*,k) point to the
c          nodes of element k.
c node	 = total number of nodal points in each element
c	   also second dimension of a.
c
c nodcode= boundary information list for each node with the
c	   following meaning:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point (corner points
c
c x,y   = real*8 arrays containing the $x$ and $y$ coordinates 
c	  resp. of the nodes.
c         K11, K22, and K12 at that element.
c ierr	= error message integer . 
c	  ierr = 0 --> normal return
c	  ierr = 1 --> negative area encountered (due to bad 
c	           numbering of nodes of an element)
c
c xyk	= subroutine defining the material properties at each 
c         element. Form: 
c 	call xyk(nel,xyke,x,y,ijk,node) 
c--------------------------------------------------------------
	implicit real*8 (a-h,o-z)
        dimension a(na,node,node),ijk(node,1),x(1),y(1),f(node,1),
     *	    ske(3,3),fe(3),xe(3),ye(3),xyke(2,2)
            integer nodcode(1)
	 external xyk

c--------------------------------------------------------------
c   initialize
c--------------------------------------------------------------
	do 100 i=1, node 
	do 100 j=1, nx
               f(i,j) = 0.0d0
 100	continue
c---------------------------------------------------
c main loop
c--------------------------------------------------- 
	do 102 nel=1, nelx
c
c get coordinetes of nodal points
c 
	do 104 i=1, node
	j = ijk(i,nel)
	xe(i) = x(j)
	ye(i) = y(j)
 104	continue
c
c compute determinant
c
 	det=xe(2)*(ye(3)-ye(1))+xe(3)*(ye(1)-ye(2))+xe(1)*(ye(2)-ye(3))
        if ( det .le. 0.) then
          print *, 'nel', nel, ' det = ' , det
          print *, xe(1), xe(2), xe(3)
          print *, ye(1), ye(2), ye(3)
        end if
c
c set material properties
c 
	call xyk(xyke,x,y)
c
c construct element stiffness matrix
c
	ierr = 0
	call estif3(nel,ske,fe,det,xe,ye,xyke,ierr)
	if (ierr .ne. 0) then
          write (*,*) 'ERROR: estif3 gave an error',ierr
          return
        endif
c	write (8,'(9f8.4)') ((ske(i,j),j=1,3),i=1,3)
c assemble: add element stiffness matrix to global matrix
c 
	do 120 ka=1, node
            f(ka,nel) = fe(ka)
        do 108 kb = 1,node
            a(nel,ka,kb) = ske(ka,kb)
 108	continue
 120	continue
 102	continue
        return
	end
c-----------------------------------------------------------------------
	subroutine unassbl_lstif(a, na, f, nx, nelx, ijk, nodcode,
     *  	           node, x, y, ierr, xyk, funb, func, fung)
c----------------------------------------------------------------------- 
c a      = un-assembled matrix on output
c
c na	 = 1-st dimension of a.  a(na,node,node)
c
c f      = right hand side (global load vector) in un-assembled form
c
c nx     = number of nodes at input
c
c nelx	 = number of elements at input
c
c ijk	 = connectivity matrix: for node k, ijk(*,k) point to the
c          nodes of element k.
c
c nodcode= boundary information list for each node with the
c	   following meaning:
c	nodcode(i) = 0 -->  node i is internal
c	nodcode(i) = 1 -->  node i is a boundary but not a corner point
c	nodcode(i) = 2 -->  node i is a corner point (corner points
c
c node	 = total number of nodal points in each element
c	   also second dimension of a.
c
c x,y   = real*8 arrays containing the $x$ and $y$ coordinates 
c	  resp. of the nodes.
c         K11, K22, and K12 at that element.
c
c ierr	= error message integer . 
c	  ierr = 0 --> normal return
c	  ierr = 1 --> negative area encountered (due to bad 
c	           numbering of nodes of an element)
c
c xyk	= subroutine defining the material properties at each 
c         element. Form:  	call xyk(xyke,x,y) 
c
c funb, = functions needed for the definition of lstif3 problem
c func,
c fung
c--------------------------------------------------------------
c moulitsa@cs.umn.edu : It uses lstif3 problem
c--------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      dimension a(na,node,node), ijk(node,1), x(1), y(1), f(node,1),
     &          ske(3,3), fe(3), xe(3), ye(3) 
      integer nodcode(1)
      external xyk, funb, func, fung
c--------------------------------------------------------------
c   initialize
c--------------------------------------------------------------
      do i=1, node 
	do j=1, nx
          f(i,j) = 0.0d0
        end do
      end do

c---------------------------------------------------
c main loop
c--------------------------------------------------- 
      do nel=1, nelx
c
c get coordinetes of nodal points
c 
	do i=1, node
	  j = ijk(i,nel)
	  xe(i) = x(j)
	  ye(i) = y(j)
        end do
c
c compute determinant
c
c	det=xe(2)*(ye(3)-ye(1))+xe(3)*(ye(1)-ye(2))+xe(1)*(ye(2)-ye(3))
c       if ( det .le. 0.) then
c         print *, 'nel', nel, ' det = ' , det
c         print *, xe(1), xe(2), xe(3)
c         print *, ye(1), ye(2), ye(3)
c       end if
c
c construct element stiffness matrix
c
	ierr = 0

        call lstif3(ske, fe, xe, ye, xyk, funb, func, fung)
c	write (8,'(9f8.4)') ((ske(i,j),j=1,3),i=1,3)
c
c assemble: add element stiffness matrix to global matrix
c 
	do ka=1, node
          f(ka,nel) = fe(ka)
          do kb = 1,node
            a(nel,ka,kb) = ske(ka,kb)
          end do
        end do

      end do

      return
      end
c-----------------------------------------------------------------------
      subroutine uppdir(n,p,np,lbp,indp,y,u,usav,flops)
      real*8 p(n,lbp), y(*), u(*), usav(*), x, flops
      integer k,np,n,npm1,j,ju,indp,lbp
c-----------------------------------------------------------------------
c     updates the conjugate directions p given the upper part of the
c     banded upper triangular matrix u.  u contains the non zero
c     elements of the column of the triangular matrix..
c-----------------------------------------------------------------------
      real*8 zero
      parameter(zero=0.0D0)
c
      npm1=np-1
      if (np .le. 1) goto 12
      j=indp
      ju = npm1
 10   if (j .le. 0) j=lbp
      x = u(ju) /usav(j)
      if (x .eq. zero) goto 115
      do 11 k=1,n
         y(k) = y(k) - x*p(k,j)
 11   continue
      flops = flops + 2*n
 115  j = j-1
      ju = ju -1
      if (ju .ge. 1) goto 10
 12   indp = indp + 1
      if (indp .gt. lbp) indp = 1
      usav(indp) = u(np)
      do 13 k=1,n
         p(k,indp) = y(k)
 13   continue
 208  return
c-----------------------------------------------------------------------
c-------end-of-uppdir---------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine usolc (n,x,y,au,jau,iau)
      real*8  x(*), y(*), au(*) 
      integer n, jau(*),iau(*)
c-----------------------------------------------------------------------
c       SOUVES     U x = y ;    where U = unit upper trang. CSC format
c-----------------------------------------------------------------------
c solves a unit upper triangular system by standard (sequential )
c forward elimination - matrix stored in CSC format. 
c-----------------------------------------------------------------------
c
c On entry:
c---------- 
c n      = integer. dimension of problem.
c y      = real*8 array containg the right side.
c
c au,
c jau,
c iau,    = Uower triangular matrix stored in compressed sparse column 
c          format. 
c
c On return:
c----------- 
c	x  = The solution of  U x  = y.
c-----------------------------------------------------------------------
c local variables 
c     
      integer k, j
      real*8 t
c-----------------------------------------------------------------------
      do 140 k=1,n
         x(k) = y(k) 
 140  continue
      do 150 k = n,1,-1
         t = x(k) 
         do 100 j = iau(k), iau(k+1)-1
            x(jau(j)) = x(jau(j)) - t*au(j) 
 100     continue
 150  continue
c
      return
c----------end-of-usolc------------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine usol (n,x,y,au,jau,iau)
      integer n, jau(*),iau(n+1) 
      real*8  x(n), y(n), au(*) 
c----------------------------------------------------------------------- 
c             Solves   U x = y    U = unit upper triangular. 
c-----------------------------------------------------------------------
c solves a unit upper triangular system by standard (sequential )
c backward elimination - matrix stored in CSR format. 
c-----------------------------------------------------------------------
c
c On entry:
c---------- 
c n      = integer. dimension of problem.
c y      = real array containg the right side.
c
c au,
c jau,
c iau,    = Lower triangular matrix stored in compressed sparse row
c          format. 
c
c On return:
c----------- 
c	x = The solution of  U x = y . 
c-------------------------------------------------------------------- 
c local variables 
c
      integer k, j 
      real*8  t
c-----------------------------------------------------------------------
      x(n) = y(n) 
      do 150 k = n-1,1,-1 
         t = y(k) 
         do 100 j = iau(k), iau(k+1)-1
            t = t - au(j)*x(jau(j))
 100     continue
         x(k) = t 
 150  continue
c
      return
c----------end-of-usol-------------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine usscsr (nrow,a,ja,ia,diag,al,jal,ial,au,jau,iau) 
      real*8 a(*),al(*),diag(*),au(*) 
      integer ja(*),ia(nrow+1),jal(*),ial(nrow+1),jau(*),iau(nrow+1)
c-----------------------------------------------------------------------
c Unsymmetric Sparse Skyline   format   to Compressed Sparse Row 
c----------------------------------------------------------------------- 
c this subroutine converts a matrix stored in nonsymmetric sparse
c skyline format into csr format. The sparse skyline format is 
c described in routine csruss. 
c----------------------------------------------------------------------- 
c----------------------------------------------------------------------- 
c On entry
c-----------------------------------------------------------------------
c nrow  = dimension of the matrix a.
c diag  = array containing the diagonal entries of A
c al,jal,ial = matrix in CSR format storing the strict lower 
c              trangular part of A.
c au,jau,iau = matrix in CSC format storing the strict upper
c              trangular part of A.
c On return
c --------- 
c a     = real array containing the nonzero values of the matrix 
c         stored rowwise.
c ja    = column indices of the values in array a
c ia    = integer array of length n+1 containing the pointers to
c         beginning of each row in arrays a, ja.
c 
c-----------------------------------------------------------------------
c
c count elements in lower part + diagonal 
c 
      do 1 i=1, nrow
         ia(i+1) = ial(i+1)-ial(i)+1
 1    continue
c
c count elements in upper part
c 
      do 3 i=1, nrow
         do 2 k=iau(i), iau(i+1)-1 
            j = jau(k)
            ia(j+1) = ia(j+1)+1
 2       continue 
 3    continue
c---------- compute pointers from lengths ------------------------------
      ia(1) = 1
      do 4 i=1,nrow
         ia(i+1) = ia(i)+ia(i+1)
 4    continue
c
c copy lower part + diagonal 
c 
      do 6 i=1, nrow
         ka = ia(i) 
         do 5 k=ial(i), ial(i+1)-1
            a(ka) = al(k) 
            ja(ka) = jal(k) 
            ka = ka+1
 5       continue
         a(ka) = diag(i) 
         ja(ka) = i
         ia(i) = ka+1
 6    continue
c     
c     copy upper part
c     
      do 8 i=1, nrow
         do 7 k=iau(i), iau(i+1)-1
c
c row number
c
            jak = jau(k) 
c
c where element goes
c
            ka = ia(jak) 
            a(ka) = au(k) 
            ja(ka) = i
            ia(jak) = ka+1
 7       continue
 8    continue
c
c readjust ia
c
      do 9 i=nrow,1,-1
         ia(i+1) = ia(i)
 9    continue
      ia(1) = 1
c----------end-of-usscsr------------------------------------------------
      end 
c-----------------------------------------------------------------------
c----------------------------end-of-csrvbr------------------------------
c----------------------------------------------------------------------c
      subroutine vbrcsr(ia, ja, a, nr, kvstr, kvstc, ib, jb, kb,
     &   b, nzmax, ierr)
c-----------------------------------------------------------------------
      integer ia(*), ja(*), nr, ib(nr+1), jb(*), kb(*)
      integer kvstr(nr+1), kvstc(*), nzmax, ierr
      real*8  a(*), b(nzmax)
c-----------------------------------------------------------------------
c     Converts variable block row to compressed sparse row format.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     nr      = number of block rows
c     kvstr   = first row number for each block row
c     kvstc   = first column number for each block column
c     ib,jb,kb,b = input matrix in VBR format
c     nzmax   = size of supplied ja and a arrays
c
c     On return:
c---------------
c     ia,ja,a = output matrix in CSR format
c
c     ierr    = error message
c               ierr = 0 means normal return
c               ierr = negative row number when out of space in
c                      ja and a arrays
c
c     Work space:
c----------------
c     None
c
c     Algorithm:
c---------------
c     The VBR data structure is traversed in the order that is required
c     to fill the CSR data structure.  In a given block row, consecutive
c     entries in the CSR data structure are entries in the VBR data
c     structure with stride equal to the row dimension of the block.
c     The VBR data structure is assumed to be sorted by block columns.
c
c-----------------------------------------------------------------------
c     Local variables:
c---------------------
      integer neqr, numc, a0, b0, i, ii, j, jj
c
c     neqr = number of rows in block row
c     numc = number of nonzero columns in row
c     a0 = index for entries in CSR a array
c     b0 = index for entries in VBR b array
c     i  = loop index for block rows
c     ii = loop index for scalar rows in block row
c     j  = loop index for block columns
c     jj = loop index for scalar columns in block column
c
c-----------------------------------------------------------------------
      ierr = 0
      a0 = 1
      b0 = 1
c-----loop on block rows
      do i = 1, nr
c--------set num of rows in block row, and num of nonzero cols in row
         neqr = kvstr(i+1) - kvstr(i)
         numc = ( kb(ib(i+1)) - kb(ib(i)) ) / neqr
c--------construct ja for a scalar row
         do j = ib(i), ib(i+1)-1
            do jj = kvstc(jb(j)), kvstc(jb(j)+1)-1
               ja(a0) = jj
               a0 = a0 + 1
            enddo
         enddo
c--------construct neqr-1 additional copies of ja for the block row
         do ii = 1, neqr-1
            do j = 1, numc
               ja(a0) = ja(a0-numc)
               a0 = a0 + 1
            enddo
         enddo
c--------reset a0 back to beginning of block row
         a0 = kb(ib(i))
c--------loop on scalar rows in block row
         do ii = 0, neqr-1
            ia(kvstr(i)+ii) = a0
            b0 = kb(ib(i)) + ii
c-----------loop on elements in a scalar row
            do jj = 1, numc
c--------------check there is enough space in a array
               if (a0 .gt. nzmax) then
                  ierr = -(kvstr(i)+ii)
                  write (*,*) 'vbrcsr: no space for row ', -ierr
                  return
               endif
               a(a0) = b(b0)
               a0 = a0 + 1
               b0 = b0 + neqr
            enddo
         enddo
c-----endloop on block rows
      enddo
      ia(kvstr(nr+1)) = a0
      return
      end
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      subroutine vbrinfo(nr, nc, kvstr, kvstc, ia, ja, ka, iwk, iout)
c-----------------------------------------------------------------------
      integer nr, nc, kvstr(nr+1), kvstc(nc+1), ia(nr+1), ja(*), ka(*)
      integer iwk(nr+nc+2+nr), iout
c-----------------------------------------------------------------------
c     Print info on matrix in variable block row format.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     nr,nc   = matrix block row and block column dimension
c     kvstr   = first row number for each block row
c     kvstc   = first column number for each block column
c     ia,ja,ka,a = input matrix in VBR format
c     iout    = unit number for printed output
c
c     On return:
c---------------
c     Printed output to unit number specified in iout.  If a non-square
c     matrix is provided, the analysis will be performed on the block
c     rows, otherwise a row/column conformal partitioning will be used.
c
c     Work space:
c----------------
c     iwk(1:nb+1) = conformal block partitioning
c        (nb is unknown at start but is no more than nr+nc)
c     iwk(nb+2:nb+2+nr) = frequency of each blocksize
c     The workspace is not assumed to be initialized to zero, nor is it
c     left that way.
c
c-----------------------------------------------------------------------
c-----local variables
      integer n, nb, nnz, nnzb, i, j, neq, max, num
      character*101 tmpst
      integer bsiz(10), freq(10)
c-----------------------------------------------------------------------
      n = kvstr(nr+1)-1
      nnz = ka(ia(nr+1)) - ka(1)
      nnzb = ia(nr+1) - ia(1)
      write (iout, 96)
      write (iout, 100) n, nnz, real(nnz)/real(n)
      write (iout, 101) nr, nnzb, real(nnzb)/real(nr)
c-----if non-square matrix, do analysis on block rows,
c     else do analysis on conformal partitioning
      if (kvstr(nr+1) .ne. kvstc(nc+1)) then
         write (iout, 103)
         do i = 1, nr+1
            iwk(i) = kvstr(i)
         enddo
         nb = nr
      else
         call kvstmerge(nr, kvstr, nc, kvstc, nb, iwk)
         if ((nr .ne. nc) .or. (nc .ne. nb)) write (iout, 104) nb
      endif
c-----accumulate frequencies of each blocksize
      max = 1
      iwk(1+nb+2) = 0
      do i = 1, nb
         neq = iwk(i+1) - iwk(i)
         if (neq .gt. max) then
            do j = max+1, neq
               iwk(j+nb+2) = 0
            enddo
            max = neq
         endif
         iwk(neq+nb+2) = iwk(neq+nb+2) + 1
      enddo
c-----store largest 10 of these blocksizes
      num = 0
      do i = max, 1, -1
         if ((iwk(i+nb+2) .ne. 0) .and. (num .lt. 10)) then
            num = num + 1
            bsiz(num) = i
            freq(num) = iwk(i+nb+2)
         endif
      enddo
c-----print information about blocksizes
      write (iout, 109) num
      write (tmpst,'(10i6)') (bsiz(j),j=1,num)
      write (iout,110) num,tmpst
      write (tmpst,'(10i6)') (freq(j),j=1,num)
      write (iout,111) tmpst
      write (iout, 96)
c-----------------------------------------------------------------------
 99    format (2x,38(2h *))
 96    format (6x,' *',65(1h-),'*')
 100   format(
     * 6x,' *  Number of rows                                   = ',
     * i10,'  *'/
     * 6x,' *  Number of nonzero elements                       = ',
     * i10,'  *'/
     * 6x,' *  Average number of nonzero elements/Row           = ',
     * f10.4,'  *')
 101   format(
     * 6x,' *  Number of block rows                             = ',
     * i10,'  *'/
     * 6x,' *  Number of nonzero blocks                         = ',
     * i10,'  *'/
     * 6x,' *  Average number of nonzero blocks/Block row       = ',
     * f10.4,'  *')
 103   format(
     * 6x,' *  Non-square matrix.                                 ',
     *     '            *'/
     * 6x,' *  Performing analysis on block rows.                 ',
     *     '            *')
 104   format(
     * 6x,' *  Non row-column conformal partitioning supplied.    ',
     *     '            *'/
     * 6x,' *  Using conformal partitioning.  Number of bl rows = ',
     * i10,'  *')
 109   format(
     * 6x,' *  Number of different blocksizes                   = ',
     * i10,'  *')
 110   format(6x,' *  The ', i2, ' largest dimension nodes',
     *     ' have dimension    : ',10x,'  *',/,
     * 6x,' *',a61,3x,' *')
 111   format(6x,' *  The frequency of nodes these ',
     *     'dimensions are      : ',10x,'  *',/,
     * 6x,' *',a61,3x,' *')
c---------------------------------
      return
      end
c-----------------------------------------------------------------------
      subroutine vbrmv(nr, nc, ia, ja, ka, a, kvstr, kvstc, x, b)
c-----------------------------------------------------------------------
      integer nr, nc, ia(nr+1), ja(*), ka(*), kvstr(nr+1), kvstc(*)
      real*8  a(*), x(*), b(*)
c-----------------------------------------------------------------------
c     Sparse matrix-full vector product, in VBR format.
c-----------------------------------------------------------------------
c     On entry:
c--------------
c     nr, nc  = number of block rows and columns in matrix A
c     ia,ja,ka,a,kvstr,kvstc = matrix A in variable block row format
c     x       = multiplier vector in full format
c
c     On return:
c---------------
c     b = product of matrix A times vector x in full format
c
c     Algorithm:
c---------------
c     Perform multiplication by traversing a in order.
c
c-----------------------------------------------------------------------
c-----local variables
      integer n, i, j, ii, jj, k, istart, istop
      real*8  xjj
c---------------------------------
      n = kvstc(nc+1)-1
      do i = 1, n
         b(i) = 0.d0
      enddo
c---------------------------------
      k = 1
      do i = 1, nr
         istart = kvstr(i)
         istop  = kvstr(i+1)-1
         do j = ia(i), ia(i+1)-1
            do jj = kvstc(ja(j)), kvstc(ja(j)+1)-1
               xjj = x(jj)
               do ii = istart, istop
                  b(ii) = b(ii) + xjj*a(k)
                  k = k + 1
               enddo
            enddo
         enddo
      enddo
c---------------------------------
      return
      end
c-----end-of-cooell-----------------------------------------------------
c-----------------------------------------------------------------------
      subroutine xcooell(n,nnz,a,ja,ia,ac,jac,nac,ner,ncmax,ierr)
C-----------------------------------------------------------------------
C   coordinate format to ellpack format.
C-----------------------------------------------------------------------
C
C   DATE WRITTEN: June 4, 1989. 
C
C   PURPOSE
C   -------
C  This subroutine takes a sparse matrix in coordinate format and
C  converts it into the Ellpack-Itpack storage.
C
C  Example:
C  -------
C       (   11   0   13    0     0     0  )
C       |   21  22    0   24     0     0  |
C       |    0  32   33    0    35     0  |
C   A = |    0   0   43   44     0    46  |
C       |   51   0    0   54    55     0  |
C       (   61  62    0    0    65    66  )
C
C   Coordinate storage scheme:
C
C    A  = (11,22,33,44,55,66,13,21,24,32,35,43,46,51,54,61,62,65)
C    IA = (1, 2, 3, 4, 5, 6, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6 )
C    JA = ( 1, 2, 3, 4, 5, 6, 3, 1, 4, 2, 5, 3, 6, 1, 4, 1, 2, 5)
C
C   Ellpack-Itpack storage scheme:
C
C       (   11  13    0    0   )          (   1   3   *    *  )
C       |   22  21   24    0   |          |   2   1   4    *  |
C  AC = |   33  32   35    0   |    JAC = |   3   2   5    *  |
C       |   44  43   46    0   |          |   4   3   6    *  |
C       |   55  51   54    0   |          |   5   1   4    *  |
C       (   66  61   62   65   )          (   6   1   2    5  )
C
C   Note: * means that you can store values from 1 to 6 (1 to n, where
C         n is the order of the matrix) in that position in the array.
C
C   Contributed by:
C   --------------- 
C   Ernest E. Rothman
C   Cornell Thoery Center/Cornell National Supercomputer Facility
C   e-mail address: BITNET:   EER@CORNELLF.BITNET
C                   INTERNET: eer@cornellf.tn.cornell.edu
C   
C   checked and modified  04/13/90 Y.Saad.
C
C   REFERENCES
C   ----------
C   Kincaid, D. R.; Oppe, T. C.; Respess, J. R.; Young, D. M. 1984.
C   ITPACKV 2C User's Guide, CNA-191. Center for Numerical Analysis,
C   University of Texas at Austin.
C
C   "Engineering and Scientific Subroutine Library; Guide and
C   Reference; Release 3 (SC23-0184-3). Pp. 79-86.
C
C-----------------------------------------------------------------------
C
C   INPUT PARAMETERS
C   ----------------
C  N       - Integer. The size of the square matrix.
C
C  NNZ     - Integer. Must be greater than or equal to the number of
C            nonzero elements in the sparse matrix. Dimension of A, IA 
C            and JA.
C
C  NCA     - Integer. First dimension of output arrays ca and jac.
C
C  A(NNZ)  - Real array. (Double precision)
C            Stored entries of the sparse matrix A.
C            NNZ is the number of nonzeros.
C
C  IA(NNZ) - Integer array.
C            Pointers to specify rows for the stored nonzero entries
C            in A.
C
C  JA(NNZ) - Integer array.
C            Pointers to specify columns for the stored nonzero
C            entries in A.
C
C  NER     - Integer. Must be set greater than or equal to the maximum
C            number of nonzeros in any row of the sparse matrix.
C
C  OUTPUT PARAMETERS
C  -----------------
C  AC(NAC,*)  - Real array. (Double precision)
C               Stored entries of the sparse matrix A in compressed
C               storage mode.
C
C  JAC(NAC,*) - Integer array.
C               Contains the column numbers of the sparse matrix
C               elements stored in the corresponding positions in
C               array AC.
C
C  NCMAX   -  Integer. Equals the maximum number of nonzeros in any
C             row of the sparse matrix.
C
C  IERR    - Error parameter is returned as zero on successful
C             execution of the subroutin<e.
C             Error diagnostics are given by means of positive values
C             of this parameter as follows:
C
C             IERR = -1   -  NER is too small and should be set equal
C                            to NCMAX. The array AC may not be large
C                            enough to accomodate all the non-zeros of
C                            of the sparse matrix.
C             IERR =  1   -  The array AC has a zero column. (Warning) 
C             IERR =  2   -  The array AC has a zero row.    (Warning)
C
C---------------------------------------------------------------------
      real*8 a(nnz), ac(nac,ner)
      integer ja(nnz), ia(nnz), jac(nac,ner), ierr, ncmax, icount
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c   Initial error parameter to zero:
c
      ierr = 0
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c   Initial output arrays to zero:
c
      do 4 in = 1,ner
         do 4 innz =1,n
            jac(innz,in) = n
            ac(innz,in) = 0.0d0
 4    continue
c     
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c   Assign nonzero elements of the sparse matrix (stored in the one
c   dimensional array A to the two dimensional array AC.
c   Also, assign the correct values with information about their
c   column indices to the two dimensional array KA. And at the same
c   time count the number of nonzeros in each row so that the
c   parameter NCMAX equals the maximum number of nonzeros in any row
c   of the sparse matrix.
c
      ncmax = 1
      do 10 is = 1,n
         k = 0
         do 30 ii = 1,nnz
            if(ia(ii).eq.is)then
               k = k + 1
               if (k .le. ner) then
                  ac(is,k) = a(ii)
                  jac(is,k) = ja(ii)
               endif 
            endif
 30      continue
         if (k.ge.ncmax) ncmax = k
 10   continue
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c     
c     Perform some simple error checks:
c     
check maximum number of nonzeros in each row:
      if (ncmax.eq.ner) ierr = 0
      if (ncmax.gt.ner) then
         ierr = -1
         return
      endif
c     
check if there are any zero columns in AC:
c     
      do 45 in = 1,ncmax
         icount = 0
         do 44 inn =1,n
            if (ac(inn,in).ne.0.0d0) icount = 1
 44      continue
         if (icount.eq.0) then
            ierr = 1
            return
         endif
 45   continue
c     
check if there are any zero rows in AC:
c     
      do 55 inn = 1,n
         icount = 0
         do 54 in =1,ncmax
            if (ac(inn,in).ne.0.0d0) icount = 1
 54      continue
         if (icount.eq.0) then
            ierr = 2
            return
         endif
 55   continue
      return
c------------- end of xcooell ------------------------------------------- 
      end
c-----------------------------------------------------------------------
      subroutine xssrcsr (nrow,a,ja,ia,nzmax,ao,jao,iao,indu,ierr)
      integer ia(nrow+1),iao(nrow+1),ja(*),jao(nzmax),indu(nrow+1)
      real*8 a(*),ao(nzmax)
c-----------------------------------------------------------------------
c Symmetric Sparse Row   to    (regular) Compressed Sparse Row
c----------------------------------------------------------------------- 
c this subroutine converts  a symmetric  matrix in which only the lower 
c part is  stored in compressed sparse row format, i.e.,
c a matrix stored in symmetric sparse format, into a fully stored matrix
c i.e., a matrix where both the lower and upper parts are stored in 
c compressed sparse row format. the algorithm is in place (i.e. result 
c may be overwritten onto the input matrix a, ja, ia ----- ). 
c the output matrix delivered by ssrcsr is such that each row starts with
c the elements of the lower part followed by those of the upper part.
c----------------------------------------------------------------------- 
c on entry:
c--------- 
c	
c nrow  = row dimension of inout matrix
c a, 
c ia, 
c ja    = matrix in compressed sparse row format. This is assumed to be
c         a lower triangular matrix. 
c
c nzmax	= size of arrays ao and jao. ssrcsr will abort if the storage 
c	   provided in a, ja is not sufficient to store A. See ierr. 
c	
c on return:
c----------
c ao, iao, 
c   jao = output matrix in compressed sparse row format. The resulting 
c         matrix is symmetric and is equal to A+A**T - D, if
c         A is the original lower triangular matrix. ao, jao, iao,
c         can be the same as a, ja, ia in the calling sequence.
c      
c indu  = integer array of length nrow+1. If the input matrix is such 
c         that the last element in each row is its diagonal element then
c         on return, indu will contain the pointers to the diagonal 
c         element in each row of the output matrix. Otherwise used as
c         work array.
c ierr  = integer. Serving as error message. If the length of the arrays
c         ao, jao exceeds nzmax, ierr returns the minimum value
c         needed for nzmax. otherwise ierr=0 (normal return).
c 
c----------------------------------------------------------------------- 
      ierr = 0
      do 1 i=1,nrow+1
         indu(i) = 0     
 1    continue
c     
c     compute  number of elements in each row of strict upper part. 
c     put result in indu(i+1)  for row i. 
c     
      do 3 i=1, nrow
         do 2 k=ia(i),ia(i+1)-1 
            j = ja(k)
            if (j .lt. i) indu(j+1) = indu(j+1)+1
 2       continue 
 3    continue
c-----------
c     find addresses of first elements of ouput matrix. result in indu
c-----------
      indu(1) = 1 
      do 4 i=1,nrow
         lenrow = ia(i+1)-ia(i)
         indu(i+1) = indu(i) + indu(i+1) + lenrow
 4    continue
c--------------------- enough storage in a, ja ? --------
      nnz = indu(nrow+1)-1 
      if (nnz .gt. nzmax) then
         ierr = nnz
         return
      endif
c
c     now copy lower part (backwards).
c     
      kosav = indu(nrow+1)
      do 6 i=nrow,1,-1
         klast = ia(i+1)-1
         kfirst = ia(i)
         iao(i+1) = kosav
         ko = indu(i) 
         kosav = ko
         do 5 k = kfirst, klast
            ao(ko) = a(k)
            jao(ko) = ja(k)
	    ko = ko+1
 5       continue
         indu(i) = ko 
 6    continue
      iao(1) = 1
c
c     now copy upper part. Go through the structure of ao, jao, iao
c     that has already been copied (lower part). indu(i) is the address
c     of the next free location in row i for ao, jao.
c     
      do 8 i=1,nrow
c     i-th row is now in ao, jao, iao structure -- lower half part
         do 9 k=iao(i), iao(i+1)-1 
            j = jao(k)
            if (j .ge. i)  goto 8
            ipos = indu(j)
            ao(ipos) = ao(k)
            jao(ipos) = i
            indu(j) = indu(j) + 1 
 9       continue
 8    continue
      return
c----- end of xssrcsr -------------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------
      subroutine xtrows (i1,i2,a,ja,ia,ao,jao,iao,iperm,job)
      integer i1,i2,ja(*),ia(*),jao(*),iao(*),iperm(*),job
      real*8 a(*),ao(*) 
c-----------------------------------------------------------------------
c this subroutine extracts given rows from a matrix in CSR format. 
c Specifically, rows number iperm(i1), iperm(i1+1), ...., iperm(i2)
c are extracted and put in the output matrix ao, jao, iao, in CSR
c format.  NOT in place. 
c Youcef Saad -- coded Feb 15, 1992. 
c-----------------------------------------------------------------------
c on entry:
c----------
c i1,i2   = two integers indicating the rows to be extracted.
c           xtrows will extract rows iperm(i1), iperm(i1+1),..,iperm(i2),
c           from original matrix and stack them in output matrix 
c           ao, jao, iao in csr format
c
c a, ja, ia = input matrix in csr format
c
c iperm	= integer array of length nrow containing the reverse permutation 
c         array for the rows. row number iperm(j) in permuted matrix PA
c         used to be row number j in unpermuted matrix.
c         ---> a(i,j) in the permuted matrix was a(iperm(i),j) 
c         in the inout matrix.
c
c job	= integer indicating the work to be done:
c 		job .ne. 1 : get structure only of output matrix,,
c               i.e., ignore real values. (in which case arrays a 
c               and ao are not used nor accessed).
c 		job = 1	get complete data structure of output matrix. 
c               (i.e., including arrays ao and iao).
c------------
c on return: 
c------------ 
c ao, jao, iao = input matrix in a, ja, ia format
c note : 
c        if (job.ne.1)  then the arrays a and ao are not used.
c----------------------------------------------------------------------c
c           Y. Saad, revised May  2, 1990                              c
c----------------------------------------------------------------------c
      logical values
      values = (job .eq. 1) 
c
c copying 
c
      ko = 1
      iao(1) = ko
      do 100 j=i1,i2 
c
c ii=iperm(j) is the index of old row to be copied.
c        
         ii = iperm(j) 
         do 60 k=ia(ii), ia(ii+1)-1 
            jao(ko) = ja(k) 
            if (values) ao(ko) = a(k)
            ko = ko+1
 60      continue
         iao(j-i1+2) = ko
 100  continue
c
      return
c---------end-of-xtrows------------------------------------------------- 
c-----------------------------------------------------------------------
      end
