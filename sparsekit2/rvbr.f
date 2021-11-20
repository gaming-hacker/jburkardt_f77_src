      program rvbr
c-----------------------------------------------------------------------
c     SPARSKIT test program for Variable Block Matrix Support
c-----------------------------------------------------------------------
c     This program tests all three conversion routines of csrvbr.
c     For each conversion to VBR, the format is converted back to CSR
c     with vbrcsr.  The subroutines csrkvstr, csrkvstc, and kvstmerge
c     become tested in the process.  The subroutines vbrinfo and vbrmv 
c     are also tested.
c-----------------------------------------------------------------------
      integer nxmax, nmx, nnzmax
      parameter (nxmax = 10, nmx = nxmax*nxmax, nnzmax=10*nmx)
      integer ia(nmx+1),ja(nnzmax),ia1(nnzmax),ja1(nnzmax), iwk(nmx*2+1)
      real*8 stencil(7,100),a(nnzmax),a1(nnzmax)
      integer ib(nmx+1),kvstr(nmx+1),kvstc(nmx+1),jb(nmx*10),kb(nmx*10)
      real*8 b(nnzmax)
      real*8 x(nmx),rhs(nmx),ans(nmx)
      integer n, na, nx, ny, nz, nfree
      integer nr, nc, i, job, maxblock, ierr
      real*8 rnd
c-----dimension of grid
      nx = 4
      ny = 2
      nz = 1
      nfree = 2
c-----generate grid problem.
      na = nfree*nfree
      call gen57bl (nx,ny,nz,nfree,na,n,a1,ja1,ia1,iwk,stencil)
c-----convert matrix to CSR
      call bsrcsr (1,n,nfree,na,a1,ja1,ia1,a,ja,ia)
      n = n * nfree 
c     call dump(1, n, .true., a, ja, ia, 6)
c-----generate random x vector for testing matrix-vector product
      do i = 1, n
         x(i) = rnd()
      enddo
c-----generate correct solution for matrix-vector product
      call amux(n, x, ans, a, ja, ia)
      do job = 0, 2
         write (*,*) 'Testing job = ', job
         if (job .eq. 0) then
c-----------maximum blocksize for random block partitioning
            maxblock = n/4
c-----------generate random block partitioning for rows
            nr = 1
            kvstr(1) = 1
 2000       continue
               nr = nr + 1
               kvstr(nr) = kvstr(nr-1) + int(rnd()*maxblock)+1
            if (kvstr(nr) .lt. n+1) goto 2000
            kvstr(nr) = n+1
            nr = nr - 1
c-----------generate random block partitioning for columns
            nc = 1
            kvstc(1) = 1
 2010       continue
               nc = nc + 1
               kvstc(nc) = kvstc(nc-1) + int(rnd()*maxblock)+1
            if (kvstc(nc) .lt. n+1) goto 2010
            kvstc(nc) = n+1
            nc = nc - 1
         endif
c--------convert to VBR format------------------------------------------
         call csrvbr(n, ia, ja, a, nr, nc, kvstr, kvstc, ib, jb, kb,
     &      b, job, iwk, nmx*10, nnzmax, ierr)
c--------convert back to CSR format-------------------------------------
         call vbrcsr(ia1, ja1, a1, nr, kvstr, kvstc, ib, jb, kb,
     &      b, nnzmax, ierr)
c--------compare original and converted CSR structures if job not 0
         write (*,*) 'Checking conversions....'
         if (job .ne. 0) then
            do i = 1, n
               if (ia(i) .ne. ia1(i)) then
                  write (*,*) 'csrvbr or vbrcsr conversion mismatch'
                  stop
               endif
            enddo
            do i = 1, ia(n+1)-1
               if ((ja(i) .ne. ja1(i)) .or. (a(i) .ne. a1(i))) then
                  write (*,*) 'csrvbr or vbrcsr conversion mismatch'
                  stop
               endif
            enddo
         endif
c--------test vbrinfo---------------------------------------------------
         call vbrinfo(nr, nc, kvstr, kvstc, ib, jb, kb, iwk, 6)
c--------test vbrmv-----------------------------------------------------
         call vbrmv(nr, nc, ib, jb, kb, b, kvstr, kvstc, x, rhs)
c--------compare answer with answer computed with CSR format
         do i = 1, n
            if (abs(ans(i) - rhs(i)) .gt. abs(0.001d0*ans(i))) then
               write (*,*) 'VBR matrix-vector product is erroneous ',i
               stop
            endif
         enddo
c--------fill CSR structure with garbage
         do i = 1, ia1(n+1)-1
            ja1(i) = -1
            a1(i) = -1.d0
         enddo
         do i = 1, n+1
            ia1(i) = -1
         enddo
c--------fill VBR structure with garbage
         do i = 1, kb(ib(nr+1))-1
            b(i) = -1.d0
         enddo
         do i = 1, ib(nr+1)
            jb(i) = -1
            kb(i) = -1
         enddo
         do i = 1, nr+1
            ib(i) = -1
         enddo
c--------fill kvstr and kvstc with garbage
         do i = 1, nr+1
            kvstr(i) = -1
         enddo
         do i = 1, nc+1
            kvstc(i) = -1
         enddo
c--------fill rhs with garbage
         do i = 1, n
            rhs(i) = -1.d0
         enddo
c-----endloop on job
      enddo
      stop
      end
c-----------------------------------------------------------------------
      function rnd()
      real*8 rnd
      integer im, ia, ic, jran
      save jran
      data im /6075/, ia /106/, ic /1283/, jran/1/
      jran = mod(jran*ia+ic, im)
      rnd = dble(jran)/dble(im)
      return
      end
c-----------------------------------------------------------------------
c     contains the functions needed for defining the PDE poroblems. 
c
c     first for the scalar 5-point and 7-point PDE 
c-----------------------------------------------------------------------
      function afun (x,y,z)
      real*8 afun, x,y,z 
      afun = -1.0d0
      return 
      end

      function bfun (x,y,z)
      real*8 bfun, x,y,z 
      bfun = -1.0d0
      return 
      end

      function cfun (x,y,z)
      real*8 cfun, x,y,z 
      cfun = -1.0d0
      return 
      end

      function dfun (x,y,z)
      real*8 dfun, x,y,z 
      data gamma /100.0/ 
c     dfun = gamma * exp( x * y )
      dfun = 10.d0
      return 
      end

      function efun (x,y,z)
      real*8 efun, x,y,z
      data gamma /100.0/ 
c     efun = gamma * exp( (- x) * y ) 
      efun = 0.d0
      return 
      end

      function ffun (x,y,z)
      real*8 ffun, x,y,z 
      ffun = 0.0
      return 
      end

      function gfun (x,y,z)
      real*8 gfun, x,y,z 
      gfun = 0.0 
      return 
      end

      function hfun(x, y, z)
      real*8 hfun, x, y, z
      hfun = 0.0
      return
      end

      function betfun(side, x, y, z)
      real*8 betfun, x, y, z
      character*2 side
      betfun = 1.0
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
c     functions for the block PDE's 
c-----------------------------------------------------------------------
      subroutine afunbl (nfree,x,y,z,coeff)
      real*8 x, y, z, coeff(100) 
      do 2 j=1, nfree
         do 1 i=1, nfree
            coeff((j-1)*nfree+i) = 0.0d0
 1       continue
         coeff((j-1)*nfree+j) = -1.0d0
 2    continue
      return 
      end

      subroutine bfunbl (nfree,x,y,z,coeff)
      real*8 x, y, z, coeff(100) 
      do 2 j=1, nfree
         do 1 i=1, nfree
            coeff((j-1)*nfree+i) = 0.0d0
 1       continue
         coeff((j-1)*nfree+j) = -1.0d0
 2    continue
      return 
      end

      subroutine cfunbl (nfree,x,y,z,coeff)
      real*8 x, y, z, coeff(100) 
      do 2 j=1, nfree
         do 1 i=1, nfree
            coeff((j-1)*nfree+i) = 0.0d0
 1       continue
         coeff((j-1)*nfree+j) = -1.0d0
 2    continue
      return 
      end

      subroutine dfunbl (nfree,x,y,z,coeff)
      real*8 x, y, z, coeff(100) 
      do 2 j=1, nfree
         do 1 i=1, nfree
            coeff((j-1)*nfree+i) = 0.0d0
 1       continue
 2    continue
      return 
      end

      subroutine efunbl (nfree,x,y,z,coeff)
      real*8 x, y, z, coeff(100) 
      do 2 j=1, nfree
         do 1 i=1, nfree
            coeff((j-1)*nfree+i) = 0.0d0
 1       continue
 2    continue
      return 
      end

      subroutine ffunbl (nfree,x,y,z,coeff)
      real*8 x, y, z, coeff(100) 
      do 2 j=1, nfree
         do 1 i=1, nfree
            coeff((j-1)*nfree+i) = 0.0d0
 1       continue
 2    continue
      return 
      end

      subroutine gfunbl (nfree,x,y,z,coeff)
      real*8 x, y, z, coeff(100) 
      do 2 j=1, nfree
         do 1 i=1, nfree
            coeff((j-1)*nfree+i) = 0.0d0
 1       continue
 2    continue
      return 
      end
c-----------------------------------------------------------------------
c     The material property function xyk for the 
c     finite element problem 
c-----------------------------------------------------------------------
      subroutine xyk(nel,xyke,x,y,ijk,node)
      implicit real*8 (a-h,o-z)
      dimension xyke(2,2), x(*), y(*), ijk(node,*)
c     
c     this is the identity matrix.
c     
      xyke(1,1) = 1.0d0
      xyke(2,2) = 1.0d0
      xyke(1,2) = 0.0d0
      xyke(2,1) = 0.0d0

      return
      end
