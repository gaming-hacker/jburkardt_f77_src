      subroutine cccslz(a,x,r,m,l,k,lda)

c*********************************************************************72
c
cc CCCSLZ solves a linear system with a CCC matrix.
c
c  Discussion:
c
c     cccslz solves the double complex linear system
c     a * x = b
c     with the ccc - matrix a .
c
c     on entry
c
c        a       double complex(m*l,k)
c                the first row of outer blocks of the ccc - matrix .
c                each outer block is represented by its first row
c                of inner blocks. each inner block is represented
c                by its first row. on return a has been destroyed .
c
c        x       double complex(m*l*k)
c                the right hand side vector b .
c
c        r       double complex(max(m,2*l,2*k))
c                a work vector .
c
c        m       integer
c                the order of the inner blocks of the matrix a .
c
c        l       integer
c                the number of inner blocks in a row or column
c                of an outer block of the matrix a .
c
c        k       integer
c                the number of outer blocks in a row or column
c                of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c     on return
c
c        x       the solution vector .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,k,lda
      double complex a(lda,k),x(m,l,k),r(1)
      integer i1,i2,i3,ml
      double precision rk
c
      rk = dfloat(k)
      ml = m*l
c
c     reduce the ccc - matrix to a block-diagonal matrix
c     by the inverse discrete fourier transformation .
c
      call salwz(a,r,r(k+1),ml,k,lda,-1)
c
c     compute the discrete fourier transformation of
c     the right hand side vector .
c
      call salwz(x,r,r(k+1),ml,k,ml,1)
c
c     solve the block-diagonal system, blocks of which
c     are cc - matrices .
c
      do 10 i3 = 1, k
         call ccslz(a(1,i3),x(1,1,i3),r,m,l,m)
   10 continue
c
c     compute the solution of the given system by
c     the inverse discrete fourier transformation .
c
      call salwz(x,r,r(k+1),ml,k,ml,-1)
c
      do 40 i3 = 1, k
         do 30 i2 = 1, l
            do 20 i1 = 1, m
               x(i1,i2,i3) = x(i1,i2,i3)/rk
   20       continue
   30    continue
   40 continue
      return
      end
      subroutine ccgslz(a,x,r,m,l,k,lda)

c*********************************************************************72
c
cc CCGSLZ solves a linear system with a CCG matrix.
c
c  Discussion:
c
c     ccgslz solves the double complex linear system
c     a * x = b
c     with the ccg - matrix a .
c
c     on entry
c
c        a       double complex(m**2*l,k)
c                the first row of outer blocks of the ccg - matrix .
c                each outer block is represented by its first row
c                of inner blocks. each inner block is represented
c                by columns. on return a has been destroyed .
c
c        x       double complex(m*l*k)
c                the right hand side vector b .
c
c        r       double complex(max(m,2*l,2*k))
c                a work vector .
c
c        m       integer
c                the order of the inner blocks of the matrix a .
c
c        l       integer
c                the number of inner blocks in a row or column
c                of an outer block of the matrix a .
c
c        k       integer
c                the number of outer blocks in a row or column
c                of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c     on return
c
c        x       the solution vector .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,k,lda
      double complex a(lda,k),x(m,l,k),r(1)
      integer i1,i2,i3,ml,mm
      double precision rk
c
      rk = dfloat(k)
      mm = m**2
      ml = m*l
c
c     reduce the ccg - matrix to a block-diagonal matrix
c     by the inverse discrete fourier transformation .
c
      call salwz(a,r,r(k+1),mm*l,k,lda,-1)
c
c     compute the discrete fourier transformation of
c     the right hand side vector .
c
      call salwz(x,r,r(k+1),ml,k,ml,1)
c
c     solve the block-diagonal system, blocks of which
c     are cg - matrices .
c
      do 10 i3 = 1, k
         call cgslz(a(1,i3),x(1,1,i3),r,m,l,mm)
   10 continue
c
c     compute the solution of the given system by
c     the inverse discrete fourier transformation .
c
      call salwz(x,r,r(k+1),ml,k,ml,-1)
c
      do 40 i3 = 1, k
         do 30 i2 = 1, l
            do 20 i1 = 1, m
               x(i1,i2,i3) = x(i1,i2,i3)/rk
   20       continue
   30    continue
   40 continue
      return
      end
      subroutine ccslz(a,x,r,m,l,lda)

c*********************************************************************72
c
cc CCSLZ solves a linear system with a CC matrix.
c
c  Discussion:
c
c     ccslz solves the double complex linear system
c     a * x = b
c     with the cc - matrix a .
c
c     on entry
c
c        a       double complex(m,l)
c                the first row of blocks of the cc - matrix .
c                each block is represented by its first row .
c                on return a has been destroyed .
c
c        x       double complex(m*l)
c                the right hand side vector b .
c
c        r       double complex(max(m,2*l))
c                a work vector .
c
c        m       integer
c                the order of the blocks of the matrix a .
c
c        l       integer
c                the number of blocks in a row or column
c                of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c     on return
c
c        x       the solution vector .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,lda
      double complex a(lda,l),x(m,l),r(1)
      integer i1,i2
      double precision rl
c
      rl = dfloat(l)
c
c     reduce the cc - matrix to a block-diagonal matrix
c     by the inverse discrete fourier transformation .
c
      call salwz(a,r,r(l+1),m,l,lda,-1)
c
c     compute the discrete fourier transformation of
c     the right hand side vector .
c
      call salwz(x,r,r(l+1),m,l,m,1)
c
c     solve the block-diagonal system, blocks of which
c     are c - matrices .
c
      do 10 i2 = 1, l
         call cslz(a(1,i2),x(1,i2),r,m)
   10 continue
c
c     compute the solution of the given system by
c     the inverse discrete fourier transformation .
c
      call salwz(x,r,r(l+1),m,l,m,-1)
c
      do 30 i2 = 1, l
         do 20 i1 = 1, m
            x(i1,i2) = x(i1,i2)/rl
   20    continue
   30 continue
      return
      end
      subroutine cctslz(a,x,r,m,l,k,lda)

c*********************************************************************72
c
cc CCTSLZ solves a linear system with a CCT matrix.
c
c  Discussion:
c
c     cctslz solves the double complex linear system
c     a * x = b
c     with the cct - matrix a .
c
c     on entry
c
c        a       double complex((2*m - 1)*l,k)
c                the first row of outer blocks of the cct - matrix .
c                each outer block is represented by its first row
c                of inner blocks. each inner block is represented
c                by its first row followed by its first column
c                beginning with the second element .
c                on return a has been destroyed .
c
c        x       double complex(m*l*k)
c                the right hand side vector b .
c
c        r       double complex(max(2*m - 2,2*l,2*k))
c                a work vector .
c
c        m       integer
c                the order of the inner blocks of the matrix a .
c
c        l       integer
c                the number of inner blocks in a row or column
c                of an outer block of the matrix a .
c
c        k       integer
c                the number of outer blocks in a row or column
c                of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c     on return
c
c        x       the solution vector .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,k,lda
      double complex a(lda,k),x(m,l,k),r(1)
      integer i1,i2,i3,m2,ml
      double precision rk
c
      rk = dfloat(k)
      m2 = 2*m - 1
      ml = m*l
c
c     reduce the cct - matrix to a block-diagonal matrix
c     by the inverse discrete fourier transformation .
c
      call salwz(a,r,r(k+1),m2*l,k,lda,-1)
c
c     compute the discrete fourier transformation of
c     the right hand side vector .
c
      call salwz(x,r,r(k+1),ml,k,ml,1)
c
c     solve the block-diagonal system, blocks of which
c     are ct - matrices .
c
      do 10 i3 = 1, k
         call ctslz(a(1,i3),x(1,1,i3),r,m,l,m2)
   10 continue
c
c     compute the solution of the given system by
c     the inverse discrete fourier transformation .
c
      call salwz(x,r,r(k+1),ml,k,ml,-1)
c
      do 40 i3 = 1, k
         do 30 i2 = 1, l
            do 20 i1 = 1, m
               x(i1,i2,i3) = x(i1,i2,i3)/rk
   20       continue
   30    continue
   40 continue
      return
      end
      subroutine cgslz(a,x,r,m,l,lda)

c*********************************************************************72
c
cc CGSLZ solves a linear system with a CG matrix.
c
c  Discussion:
c
c     cgslz solves the double complex linear system
c     a * x = b
c     with the cg - matrix a .
c
c     on entry
c
c        a       double complex(m**2,l)
c                the first row of blocks of the cg - matrix .
c                each block is represented by columns .
c                on return a has been destroyed .
c
c        x       double complex(m*l)
c                the right hand side vector b .
c
c        r       double complex(max(m,2*l))
c                a work vector .
c
c        m       integer
c                the order of the blocks of the matrix a .
c
c        l       integer
c                the number of blocks in a row or column
c                of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c     on return
c
c        x       the solution vector .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,lda
      double complex a(lda,l),x(m,l),r(1)
      integer i1,i2,ii
      double precision rl
c
      rl = dfloat(l)
c
c     reduce the cg - matrix to a block-diagonal matrix
c     by the inverse discrete fourier transformation .
c
      call salwz(a,r,r(l+1),m**2,l,lda,-1)
c
c     compute the discrete fourier transformation of
c     the right hand side vector .
c
      call salwz(x,r,r(l+1),m,l,m,1)
c
c     solve the block-diagonal system, blocks of which
c     are g - matrices .
c
      do 10 i2 = 1, l
         call zgefa(a(1,i2),m,m,r,ii)
         call zgesl(a(1,i2),m,m,r,x(1,i2),0)
   10 continue
c
c     compute the solution of the given system by
c     the inverse discrete fourier transformation .
c
      call salwz(x,r,r(l+1),m,l,m,-1)
c
      do 30 i2 = 1, l
         do 20 i1 = 1, m
            x(i1,i2) = x(i1,i2)/rl
   20    continue
   30 continue
      return
      end
      subroutine cqrd(a,q,s,m,l,ldq,lds)

c*********************************************************************72
c
cc CQRD computes the QR factorization of a CC matrix.
c
c  Discussion:
c
c     cqrd computes the qr factorization in the form
c     a * r(inverse) = q
c     of the double precision column-circulant matrix a .
c
c     on entry
c
c        a       double precision(m)
c                the first column of the column-circulant matrix .
c                on return a is unaltered .
c
c        m       integer
c                the number of rows of the matrices a and q .
c                m must be at least as large as l .
c
c        l       integer
c                the number of columns of the matrices a and q
c                and the order of the upper triangular matrix s .
c
c        ldq     integer
c                the leading dimension of the array q .
c
c        lds     integer
c                the leading dimension of the array s .
c
c     on return
c
c        q       double precision(m,l)
c                the q matrix of the factorization .
c                the columns of q are orthonormal .
c
c        s       double precision(l,l)
c                the inverse of the r matrix of the factorization .
c                elements below the main diagonal are not accessed .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,ldq,lds
      double precision a(m),q(ldq,l),s(lds,l)
      integer i,j,j1,ji
      double precision scale,dnrm2
      double precision c,ddot
c
c     initialization (last column of q used as work vector) .
c
      do 10 i = 1, m
         q(i,1) = a(i)
         q(i,l) = a(i)
   10 continue
c
c     recurrent process for the lattice algorithm with normalization .
c
      do 70 j1 = 1, l
         j = j1 + 1
         scale = 1.0d0/dnrm2(m,q(1,j1),1)
         if (j1 .eq. l) go to 60
            c = -scale*(q(m,j1)*q(1,l) +
     *           ddot(m-1,q(1,j1),1,q(2,l),1))/dnrm2(m,q(1,l),1)
            q(1,j) = q(m,j1) + c*q(1,l)
            do 20 i = 2, m
               q(i,j) = q(i-1,j1) + c*q(i,l)
   20       continue
            if (j .eq. l) go to 30
               q(1,l) = q(1,l) + c*q(m,j1)
               call daxpy(m-1,c,q(1,j1),1,q(2,l),1)
   30       continue
            s(1,j) = c
            if (j .eq. 2) go to 50
               do 40 i = 2, j1
                  ji = j - i
                  s(i,j) = s(i-1,j1) + c*s(ji,j1)
   40          continue
   50       continue
   60    continue
         call dscal(m,scale,q(1,j1),1)
         s(j1,j1) = 1.0d0
         call dscal(j1,scale,s(1,j1),1)
   70 continue
      return
      end
      subroutine cqrz(a,q,s,m,l,ldq,lds)

c*********************************************************************72
c
cc CQRZ computes the QR factorization of a CCC matrix.
c
c  Discussion:
c
c     cqrz computes the qr factorization in the form
c     a * r(inverse) = q
c     of the double complex column-circulant matrix a .
c
c     on entry
c
c        a       double complex(m)
c                the first column of the column-circulant matrix .
c                on return a is unaltered .
c
c        m       integer
c                the number of rows of the matrices a and q .
c                m must be at least as large as l .
c
c        l       integer
c                the number of columns of the matrices a and q
c                and the order of the upper triangular matrix s .
c
c        ldq     integer
c                the leading dimension of the array q .
c
c        lds     integer
c                the leading dimension of the array s .
c
c     on return
c
c        q       double complex(m,l)
c                the q matrix of the factorization .
c                the columns of q are orthonormal .
c
c        s       double complex(l,l)
c                the inverse of the r matrix of the factorization .
c                elements below the main diagonal are not accessed .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,ldq,lds
      double complex a(m),q(ldq,l),s(lds,l)
      integer i,j,j1,ji
      double precision scale,dznrm2
      double complex c,zdotc
c
c     initialization (last column of q used as work vector) .
c
      do 10 i = 1, m
         q(i,1) = a(i)
         q(i,l) = a(i)
   10 continue
c
c     recurrent process for the lattice algorithm with normalization .
c
      do 70 j1 = 1, l
         j = j1 + 1
         scale = 1.0d0/dznrm2(m,q(1,j1),1)
         if (j1 .eq. l) go to 60
            c = -scale*(dconjg(q(m,j1))*q(1,l) +
     *           zdotc(m-1,q(1,j1),1,q(2,l),1))/dznrm2(m,q(1,l),1)
            q(1,j) = q(m,j1) + c*q(1,l)
            do 20 i = 2, m
               q(i,j) = q(i-1,j1) + c*q(i,l)
   20       continue
            if (j .eq. l) go to 30
               q(1,l) = q(1,l) + c*q(m,j1)
               call zaxpy(m-1,c,q(1,j1),1,q(2,l),1)
   30       continue
            s(1,j) = c
            if (j .eq. 2) go to 50
               do 40 i = 2, j1
                  ji = j - i
                  s(i,j) = s(i-1,j1) + c*s(ji,j1)
   40          continue
   50       continue
   60    continue
         call zdscal(m,scale,q(1,j1),1)
         s(j1,j1) = (1.0d0,0.0d0)
         call zdscal(j1,scale,s(1,j1),1)
   70 continue
      return
      end
      subroutine cslz ( a, x, r, m )

c*********************************************************************72
c
cc CSLZ solves a linear system with a C matrix.
c
c  Discussion:
c
c    cslz solves the double complex linear system
c      a * x = b
c    with the c - matrix a .
c
c     on entry
c
c        a       double complex(m)
c                the first row of the c - matrix .
c                on return a is unaltered .
c
c        x       double complex(m)
c                the right hand side vector b .
c
c        r       double complex(m)
c                a work vector .
c
c        m       integer
c                the order of the matrix a .
c
c     on return
c
c        x       the solution vector .
c
      integer m
      double complex a(m),x(m),r(m)

      integer i1,i2
      double precision p,ri,rm,v1,v2
      double complex e,e1,f,f1,t,t1
c
      t1 = x(1)
      x(1) = t1/a(1)
      if (m .eq. 1) go to 50
      rm = dfloat(m)
c
c     compute the inverse discrete fourier transformation
c     of the first row of the matrix and the discrete
c     fourier transformation of the right hand side vector .
c
      t = (0.0d0,0.0d0)
      ri = -1.0d0
      do 20 i1 = 1, m
         ri = ri + 1.0d0
c
c        minimize error in forming multiples of 2*pi .
c
         p = ((201.d0/32.d0)*ri + 1.93530717958647692528d-3*ri)/rm

         v1 = dcos(p)
         v2 = dsin(p)
         e = dcmplx(v1,-v2)
         e1 = dcmplx(v1,v2)
         f = a(1)
         f1 = t1
         do 10 i2 = 2, m
            f = e*f + a(i2)
            f1 = e1*f1 + x(i2)
   10    continue
         r(i1) = (e1*f1)/(e*f)
         t = t + r(i1)
   20 continue
c
c     compute the solution of the given system by
c     the inverse discrete fourier transformation .
c
      x(1) = t/rm
      ri = 0.0d0
      do 40 i1 = 2, m
         ri = ri + 1.0d0
c
c        minimize error in forming multiples of 2*pi .
c
         p = ((201.d0/32.d0)*ri + 1.93530717958647692528d-3*ri)/rm
c
         v1 = dcos(p)
         v2 = dsin(p)
         e = dcmplx(v1,-v2)
         f = r(1)
         do 30 i2 = 2, m
            f = e*f + r(i2)
   30    continue
         x(i1) = e*f/rm
   40 continue
   50 continue
      return
      end
      subroutine ctgslz(a,x,r,m,l,k,lda)

c*********************************************************************72
c
cc CTGSLZ solves a linear system with a CTG matrix.
c
c  Discussion:
c
c     ctgslz solves the double complex linear system
c     a * x = b
c     with the ctg - matrix a .
c
c     on entry
c
c        a       double complex(m**2*(2*l - 1),k)
c                the first row of outer blocks of the ctg - matrix .
c                each outer block is represented by its first row
c                of inner blocks followed by its first column
c                of inner blocks beginning with the second block .
c                each inner block is represented by columns .
c                on return a has been destroyed .
c
c        x       double complex(m*l*k)
c                the right hand side vector b .
c
c        r       double complex(max(m**2*(2*l + 3) + m,2*k))
c                a work vector .
c
c        m       integer
c                the order of the inner blocks of the matrix a .
c
c        l       integer
c                the number of inner blocks in a row or column
c                of an outer block of the matrix a .
c
c        k       integer
c                the number of outer blocks in a row or column
c                of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c     on return
c
c        x       the solution vector .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,k,lda
      double complex a(lda,k),x(m,l,k),r(1)
      integer i1,i2,i3,ml,mm
      double precision rk
c
      rk = dfloat(k)
      mm = m**2
      ml = m*l
c
c     reduce the ctg - matrix to a block-diagonal matrix
c     by the inverse discrete fourier transformation .
c
      call salwz(a,r,r(k+1),mm*(2*l - 1),k,lda,-1)
c
c     compute the discrete fourier transformation of
c     the right hand side vector .
c
      call salwz(x,r,r(k+1),ml,k,ml,1)
c
c     solve the block-diagonal system, blocks of which
c     are tg - matrices .
c
      do 10 i3 = 1, k
         call tgslz(a(1,i3),x(1,1,i3),r,m,l,mm)
   10 continue
c
c     compute the solution of the given system by
c     the inverse discrete fourier transformation .
c
      call salwz(x,r,r(k+1),ml,k,ml,-1)
c
      do 40 i3 = 1, k
         do 30 i2 = 1, l
            do 20 i1 = 1, m
               x(i1,i2,i3) = x(i1,i2,i3)/rk
   20       continue
   30    continue
   40 continue
      return
      end
      subroutine ctslz(a,x,r,m,l,lda)

c*********************************************************************72
c
cc CTSLZ solves a linear system with a CT matrix.
c
c  Discussion:
c
c     ctslz solves the double complex linear system
c     a * x = b
c     with the ct - matrix a .
c
c     on entry
c
c        a       double complex(2*m - 1,l)
c                the first row of blocks of the ct - matrix .
c                each block is represented by its first row
c                followed by its first column beginning with the
c                second element. on return a has been destroyed .
c
c        x       double complex(m*l)
c                the right hand side vector b .
c
c        r       double complex(max(2*m - 2,2*l))
c                a work vector .
c
c        m       integer
c                the order of the blocks of the matrix a .
c
c        l       integer
c                the number of blocks in a row or column
c                of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c     on return
c
c        x       the solution vector .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,lda
      double complex a(lda,l),x(m,l),r(1)
      integer i1,i2
      double precision rl
c
      rl = dfloat(l)
c
c     reduce the ct - matrix to a block-diagonal matrix
c     by the inverse discrete fourier transformation .
c
      call salwz(a,r,r(l+1),2*m - 1,l,lda,-1)
c
c     compute the discrete fourier transformation of
c     the right hand side vector .
c
      call salwz(x,r,r(l+1),m,l,m,1)
c
c     solve the block-diagonal system, blocks of which
c     are t - matrices .
c
      do 10 i2 = 1, l
         call tslz(a(1,i2),x(1,i2),r,m)
   10 continue
c
c     compute the solution of the given system by
c     the inverse discrete fourier transformation .
c
      call salwz(x,r,r(l+1),m,l,m,-1)
c
      do 30 i2 = 1, l
         do 20 i1 = 1, m
            x(i1,i2) = x(i1,i2)/rl
   20    continue
   30 continue
      return
      end
      subroutine daxpy(n,da,dx,incx,dy,incy)

c*********************************************************************72
c
cc DAXPY adds a multiple of one vector to another.
c
c  Discussion:
c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision dx(1),dy(1),da
      integer i,incx,incy,ixiy,m,mp1,n
c
      if(n.le.0)return
      if (da .eq. 0.0d0) return
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
        dy(iy) = dy(iy) + da*dx(ix)
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
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end
      function dcabs1(z)

c*********************************************************************72
c
cc DCABS1 computes the L1 norm of a complex value.
c
c  Discussion:
c
      double precision dcabs1
      double complex z,zz
      double precision t(2)
      equivalence (zz,t(1))

      zz = z
      dcabs1 = dabs(t(1)) + dabs(t(2))

      return
      end
      function ddot(n,dx,incx,dy,incy)

c*********************************************************************72
c
cc DDOT forms the dot product of two vectors.
c
c  Discussion:
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      double precision ddot
      double precision dx(1),dy(1),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n

      ddot = 0.0d0
      dtemp = 0.0d0
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
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end
      subroutine dgefa(a,lda,n,ipvt,info)

c*********************************************************************72
c
cc DGEFA factors a general matrix.
c
c  Discussion:
c
c     dgefa factors a double precision matrix by gaussian elimination.
c
c     dgefa is usually called by dgeco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the matrix to be factored.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix and the multipliers
c                which were used to obtain it.
c                the factorization can be written  a = l*u  where
c                l  is a product of permutation and unit lower
c                triangular matrices and  u  is upper triangular.
c
c        ipvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if  u(k,k) .eq. 0.0 .  this is not an error
c                     condition for this subroutine, but it does
c                     indicate that dgesl or dgedi will divide by zero
c                     if called.  use  rcond  in dgeco for a reliable
c                     indication of singularity.
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
      integer lda,n,ipvt(1),info
      double precision a(lda,1)
      double precision t
      integer idamax,j,k,kp1,l,nm1
c
c     gaussian elimination with partial pivoting
c
      info = 0
      nm1 = n - 1
      if (nm1 .lt. 1) go to 70
      do 60 k = 1, nm1
         kp1 = k + 1
c
c        find l = pivot index
c
         l = idamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k) = l
c
c        zero pivot implies this column already triangularized
c
         if (a(l,k) .eq. 0.0d0) go to 40
c
c           interchange if necessary
c
            if (l .eq. k) go to 10
               t = a(l,k)
               a(l,k) = a(k,k)
               a(k,k) = t
   10       continue
c
c           compute multipliers
c
            t = -1.0d0/a(k,k)
            call dscal(n-k,t,a(k+1,k),1)
c
c           row elimination with column indexing
c
            do 30 j = kp1, n
               t = a(l,j)
               if (l .eq. k) go to 20
                  a(l,j) = a(k,j)
                  a(k,j) = t
   20          continue
               call daxpy(n-k,t,a(k+1,k),1,a(k+1,j),1)
   30       continue
         go to 50
   40    continue
            info = k
   50    continue
   60 continue
   70 continue
      ipvt(n) = n
      if (a(n,n) .eq. 0.0d0) info = n
      return
      end
      subroutine dgesl(a,lda,n,ipvt,b,job)

c*********************************************************************72
c
cc DGESL solves a linear system with a factored general matrix.
c
c  Discussion:
c
c     dgesl solves the double precision system
c     a * x = b  or  trans(a) * x = b
c     using the factors computed by dgeco or dgefa.
c
c     on entry
c
c        a       double precision(lda, n)
c                the output from dgeco or dgefa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        ipvt    integer(n)
c                the pivot vector from dgeco or dgefa.
c
c        b       double precision(n)
c                the right hand side vector.
c
c        job     integer
c                = 0         to solve  a*x = b ,
c                = nonzero   to solve  trans(a)*x = b  where
c                            trans(a)  is the transpose.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains a
c        zero on the diagonal.  technically this indicates singularity
c        but it is often caused by improper arguments or improper
c        setting of lda .  it will not occur if the subroutines are
c        called correctly and if dgeco has set rcond .gt. 0.0
c        or dgefa has set info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dgeco(a,lda,n,ipvt,rcond,z)
c           if (rcond is too small) go to ...
c           do 10 j = 1, p
c              call dgesl(a,lda,n,ipvt,c(1,j),0)
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
      integer lda,n,ipvt(1),job
      double precision a(lda,1),b(1)
      double precision ddot,t
      integer k,kb,l,nm1
c
      nm1 = n - 1
      if (job .ne. 0) go to 50
c
c        job = 0 , solve  a * x = b
c        first solve  l*y = b
c
         if (nm1 .lt. 1) go to 30
         do 20 k = 1, nm1
            l = ipvt(k)
            t = b(l)
            if (l .eq. k) go to 10
               b(l) = b(k)
               b(k) = t
   10       continue
            call daxpy(n-k,t,a(k+1,k),1,b(k+1),1)
   20    continue
   30    continue
c
c        now solve  u*x = y
c
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/a(k,k)
            t = -b(k)
            call daxpy(k-1,t,a(1,k),1,b(1),1)
   40    continue
      go to 100
   50 continue
c
c        job = nonzero, solve  trans(a) * x = b
c        first solve  trans(u)*y = b
c
         do 60 k = 1, n
            t = ddot(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k) - t)/a(k,k)
   60    continue
c
c        now solve trans(l)*x = y
c
         if (nm1 .lt. 1) go to 90
         do 80 kb = 1, nm1
            k = n - kb
            b(k) = b(k) + ddot(n-k,a(k+1,k),1,b(k+1),1)
            l = ipvt(k)
            if (l .eq. k) go to 70
               t = b(l)
               b(l) = b(k)
               b(k) = t
   70       continue
   80    continue
   90    continue
  100 continue
      return
      end
      function dnrm2 ( n, dx, incx)

c*********************************************************************72
c
cc DNRM2 computes the L2 norm of a vector.
c
c  Discussion:
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
      double precision dnrm2
      integer          next
      double precision   dx(1), cutlo, cuthi, hitest, sum, xmax,zero,one
      data   zero, one /0.0d0, 1.0d0/
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
      subroutine dscal(n,da,dx,incx)

c*********************************************************************72
c
cc DSCAL scales a vector by a constant.
c
c  Discussion:
c
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
      subroutine dtrdi(t,ldt,n,det,job,info)

c*********************************************************************72
c
cc DTRDI computes the determinant and inverse of a triangular matrix.
c
c  Discussion:
c
c     dtrdi computes the determinant and inverse of a double precision
c     triangular matrix.
c
c     on entry
c
c        t       double precision(ldt,n)
c                t contains the triangular matrix. the zero
c                elements of the matrix are not referenced, and
c                the corresponding elements of the array can be
c                used to store other information.
c
c        ldt     integer
c                ldt is the leading dimension of the array t.
c
c        n       integer
c                n is the order of the system.
c
c        job     integer
c                = 010       no det, inverse of lower triangular.
c                = 011       no det, inverse of upper triangular.
c                = 100       det, no inverse.
c                = 110       det, inverse of lower triangular.
c                = 111       det, inverse of upper triangular.
c
c     on return
c
c        t       inverse of original matrix if requested.
c                otherwise unchanged.
c
c        det     double precision(2)
c                determinant of original matrix if requested.
c                otherwise not referenced.
c                determinant = det(1) * 10.0**det(2)
c                with  1.0 .le. dabs(det(1)) .lt. 10.0
c                or  det(1) .eq. 0.0 .
c
c        info    integer
c                info contains zero if the system is nonsingular
c                and the inverse is requested.
c                otherwise info contains the index of
c                a zero diagonal element of t.
c
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
      integer ldt,n,job,info
      double precision t(ldt,1),det(2)
      double precision temp
      double precision ten
      integer i,j,k,kb,km1,kp1
c
c     begin block permitting ...exits to 180
c
c        compute determinant
c
         if (job/100 .eq. 0) go to 70
            det(1) = 1.0d0
            det(2) = 0.0d0
            ten = 10.0d0
            do 50 i = 1, n
               det(1) = t(i,i)*det(1)
c           ...exit
               if (det(1) .eq. 0.0d0) go to 60
   10          if (dabs(det(1)) .ge. 1.0d0) go to 20
                  det(1) = ten*det(1)
                  det(2) = det(2) - 1.0d0
               go to 10
   20          continue
   30          if (dabs(det(1)) .lt. ten) go to 40
                  det(1) = det(1)/ten
                  det(2) = det(2) + 1.0d0
               go to 30
   40          continue
   50       continue
   60       continue
   70    continue
c
c        compute inverse of upper triangular
c
         if (mod(job/10,10) .eq. 0) go to 170
            if (mod(job,10) .eq. 0) go to 120
c              begin block permitting ...exits to 110
                  do 100 k = 1, n
                     info = k
c              ......exit
                     if (t(k,k) .eq. 0.0d0) go to 110
                     t(k,k) = 1.0d0/t(k,k)
                     temp = -t(k,k)
                     call dscal(k-1,temp,t(1,k),1)
                     kp1 = k + 1
                     if (n .lt. kp1) go to 90
                     do 80 j = kp1, n
                        temp = t(k,j)
                        t(k,j) = 0.0d0
                        call daxpy(k,temp,t(1,k),1,t(1,j),1)
   80                continue
   90                continue
  100             continue
                  info = 0
  110          continue
            go to 160
  120       continue
c
c              compute inverse of lower triangular
c
               do 150 kb = 1, n
                  k = n + 1 - kb
                  info = k
c     ............exit
                  if (t(k,k) .eq. 0.0d0) go to 180
                  t(k,k) = 1.0d0/t(k,k)
                  temp = -t(k,k)
                  if (k .ne. n) call dscal(n-k,temp,t(k+1,k),1)
                  km1 = k - 1
                  if (km1 .lt. 1) go to 140
                  do 130 j = 1, km1
                     temp = t(k,j)
                     t(k,j) = 0.0d0
                     call daxpy(n-k+1,temp,t(k,k),1,t(k,j),1)
  130             continue
  140             continue
  150          continue
               info = 0
  160       continue
  170    continue
  180 continue
      return
      end
      function dznrm2( n, zx, incx)

c*********************************************************************72
c
cc DZNRM2 computes the L2 norm of a vector.
c
c  Discussion:
c
c     unitary norm of the complex n-vector stored in zx() with storage
c     increment incx .
c     if    n .le. 0 return with result = 0.
c     if n .ge. 1 then incx must be .ge. 1
c
c           c.l.lawson , 1978 jan 08
c
c     four phase method     using two built-in constants that are
c     hopefully applicable to all machines.
c         cutlo = maximum of  sqrt(u/eps)  over all known machines.
c         cuthi = minimum of  sqrt(v)      over all known machines.
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
      double precision dznrm2
      logical imag, scale
      integer          next
      double precision cutlo, cuthi, hitest, sum, xmax, absx, zero, one
      double complex      zx(1)
      double precision dreal,dimag
      double complex zdumr,zdumi
      dreal(zdumr) = zdumr
      dimag(zdumi) = (0.0d0,-1.0d0)*zdumi
      data         zero, one /0.0d0, 1.0d0/
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
         dznrm2  = zero
         go to 300
c
   10 assign 30 to next
      sum = zero
      nn = n * incx
c                                                 begin main loop
      do 210 i=1,nn,incx
         absx = dabs(dreal(zx(i)))
         imag = .false.
         go to next,(30, 50, 70, 90, 110)
   30 if( absx .gt. cutlo) go to 85
      assign 50 to next
      scale = .false.
c
c                        phase 1.  sum is zero
c
   50 if( absx .eq. zero) go to 200
      if( absx .gt. cutlo) go to 85
c
c                                prepare for phase 2.
      assign 70 to next
      go to 105
c
c                                prepare for phase 4.
c
  100 assign 110 to next
      sum = (sum / absx) / absx
  105 scale = .true.
      xmax = absx
      go to 115
c
c                   phase 2.  sum is small.
c                             scale to avoid destructive underflow.
c
   70 if( absx .gt. cutlo ) go to 75
c
c                     common code for phases 2 and 4.
c                     in phase 4 sum is large.  scale to avoid overflow.
c
  110 if( absx .le. xmax ) go to 115
         sum = one + sum * (xmax / absx)**2
         xmax = absx
         go to 200
c
  115 sum = sum + (absx/xmax)**2
      go to 200
c
c
c                  prepare for phase 3.
c
   75 sum = (sum * xmax) * xmax
c
   85 assign 90 to next
      scale = .false.
c
c     for real or d.p. set hitest = cuthi/n
c     for complex      set hitest = cuthi/(2*n)
c
      hitest = cuthi/float( n )
c
c                   phase 3.  sum is mid-range.  no scaling.
c
   90 if(absx .ge. hitest) go to 100
         sum = sum + absx**2
  200 continue
c                  control selection of real and imaginary parts.
c
      if(imag) go to 210
         absx = dabs(dimag(zx(i)))
         imag = .true.
      go to next,(  50, 70, 90, 110 )
c
  210 continue
c
c              end of main loop.
c              compute square root and adjust for scaling.
c
      dznrm2 = dsqrt(sum)
      if(scale) dznrm2 = dznrm2 * xmax
  300 continue
      return
      end
      function idamax(n,dx,incx)

c*********************************************************************72
c
cc IDAMAX locates the element of maximum absolute value in a vector.
c
c  Discussion:
c
c     finds the index of element having max. absolute value.
c     jack dongarra, linpack, 3/11/78.
c
      integer idamax
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
      function izamax(n,zx,incx)

c*********************************************************************72
c
cc IZAMAX locates the element of maximum absolute value in a vector.
c
c  Discussion:
c
c     finds the index of element having max. absolute value.
c     jack dongarra, 3/11/78.
c
      integer izamax
      double complex zx(1)
      double precision smax
      double complex zdum
      double precision dcabs1
c
      izamax = 1
      if(n.le.1)return
      if(incx.eq.1)go to 20
c
c        code for increments not equal to 1
c
      ix = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      smax = dcabs1(zx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(dcabs1(zx(ix)).le.smax) go to 5
         izamax = ix
         smax = dcabs1(zx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increments equal to 1
c
   20 smax = dcabs1(zx(1))
      do 30 i = 2,n
         if(dcabs1(zx(i)).le.smax) go to 30
         izamax = i
         smax = dcabs1(zx(i))
   30 continue
      return
      end
      subroutine salwz(a,r1,r2,m,l,lda,job)

c*********************************************************************72
c
cc SALWZ: direct or inverse discrete Fourier transform of rectangular matrix.
c
c  Discussion:
c
c     salwz computes the direct or inverse discrete fourier
c     transformation for rows of a double complex rectangular matrix .
c
c     on entry
c
c        a       double complex(m,l)
c                the input matrix .
c
c        r1      double complex(l)
c                a work vector .
c
c        r2      double complex(l)
c                a work vector .
c
c        m       integer
c                the number of rows of the matrix a .
c
c        l       integer
c                the number of columns of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c        job     integer
c                = 1 for direct fourier transformation .
c                = -1 for inverse fourier transformation .
c
c     on return
c
c        a       the transformed rows of the matrix .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,lda,job
      double complex a(lda,l),r1(l),r2(l)
      integer i,i1,i2
      double precision p,ri,rl,v1,v2
      double complex e,f
c
      if (l .eq. 1) go to 60
      rl = dfloat(l)
c
      r1(1) = (1.0d0,0.0d0)
      ri = 0.0d0
      do 10 i1 = 2, l
         ri = ri + 1.0d0
c
c        minimize error in forming multiples of 2*pi .
c
         p = ((201.d0/32.d0)*ri + 1.93530717958647692528d-3*ri)/rl
c
         v1 = dcos(p)
         v2 = dsin(p)
         if (job .eq. (-1)) v2 = -v2
         r1(i1) = dcmplx(v1,v2)
   10 continue
      do 50 i = 1, m
         do 30 i1 = 1, l
            e = r1(i1)
            f = a(i,1)
            do 20 i2 = 2, l
               f = e*f + a(i,i2)
   20       continue
            r2(i1) = e*f
   30    continue
         do 40 i1 = 1, l
            a(i,i1) = r2(i1)
   40    continue
   50 continue
   60 continue
      return
      end
      subroutine tgsld1(a1,a2,b,x,c1,c2,r1,r2,r3,r5,r6,r,m,l,lda)

c*********************************************************************72
c
cc TGSLD1 solves a linear system with a TG matrix.
c
c  Discussion:
c
c     tgsld1 solves the double precision linear system
c     a * x = b
c     with the tg - matrix a .
c
c     on entry
c
c        a1      double precision(m**2,l)
c                the first row of blocks of the tg - matrix a .
c                each block is represented by columns .
c                on return a1 is unaltered .
c
c        a2      double precision(m**2,l - 1)
c                the first column of blocks of the tg - matrix a
c                beginning with the second block. each block is
c                represented by columns. on return a2 is unaltered .
c
c        b       double precision(m*l)
c                the right hand side vector .
c                on return b is unaltered .
c
c        c1      double precision(m,m,l - 1)
c                a work array .
c
c        c2      double precision(m,m,l - 1)
c                a work array .
c
c        r1      double precision(m,m)
c                a work array .
c
c        r2      double precision(m,m)
c                a work array .
c
c        r3      double precision(m,m)
c                a work array .
c
c        r5      double precision(m,m)
c                a work array .
c
c        r6      double precision(m,m)
c                a work array .
c
c        r       double precision(m)
c                a work vector .
c
c        m       integer
c                the order of the blocks of the matrix a .
c
c        l       integer
c                the number of blocks in a row or column
c                of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c     on return
c
c        x       double precision(m*l)
c                the solution vector. x may coincide with b .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,lda
      double precision a1(lda,l),a2(lda,1),b(m,l),x(m,l),c1(m,m,1),
     *     c2(m,m,1),r1(m,m),r2(m,m),r3(m,m),r5(m,m),r6(m,m),r(m)
      integer i,i1,i2,i3,ii,j,n,n1,n2
c
c     solve the system with the principal minor of order m .
c
      i3 = 1
      do 20 j = 1, m
         do 10 i = 1, m
            c1(i,j,1) = a1(i3,1)
            r1(i,j) = a1(i3,1)
            r3(i,j) = r1(i,j)
            i3 = i3 + 1
   10    continue
         x(j,1) = b(j,1)
   20 continue
      call dgefa(r3,m,m,r,ii)
      call dgesl(r3,m,m,r,x(1,1),0)
      if (l .eq. 1) go to 420
c
c     recurrent process for solving the system
c     with the tg - matrix for n = 2, l .
c
      do 410 n = 2, l
c
c        compute multiples of the first and last block columns of
c        the inverse of the principal minor of order m*n .
c
         n1 = n - 1
         n2 = n - 2
         i3 = 1
         do 40 j = 1, m
            do 30 i = 1, m
               r5(i,j) = a2(i3,n1)
               r6(i,j) = a1(i3,n)
               i3 = i3 + 1
   30       continue
   40    continue
         if (n .eq. 2) go to 100
            do 60 j = 1, m
               do 50 i = 1, m
                  c1(i,j,n1) = r2(i,j)
   50          continue
   60       continue
            do 90 i1 = 1, n2
               i2 = n - i1
               do 80 j = 1, m
                  i3 = 1
                  do 70 i = 1, m
c for in-line daxpy, activate next 5 lines and deactivate following 3 .
c                    do 65 ii = 1, m
c                       r5(ii,j) = r5(ii,j) + c1(i,j,i2)*a2(i3,i1)
c                       r6(ii,j) = r6(ii,j) + c2(i,j,i1)*a1(i3,i1+1)
c                       i3 = i3 + 1
c  65                continue
                     call daxpy(m,c1(i,j,i2),a2(i3,i1),1,r5(1,j),1)
                     call daxpy(m,c2(i,j,i1),a1(i3,i1+1),1,r6(1,j),1)
                     i3 = i3 + m
   70             continue
   80          continue
   90       continue
  100    continue
         do 120 j = 1, m
            do 110 i = 1, m
               r2(i,j) = -r5(i,j)
  110       continue
            call dgesl(r3,m,m,r,r2(1,j),0)
  120    continue
         do 140 j = 1, m
            do 130 i = 1, m
               r3(i,j) = r6(i,j)
               r6(i,j) = -c1(i,j,1)
  130       continue
  140    continue
         do 160 j = 1, m
            do 150 i = 1, m
c for in-line daxpy, activate next 3 lines and deactivate following 1 .
c              do 145 ii = 1, m
c                 c1(ii,j,1) = c1(ii,j,1) + r2(i,j)*r3(ii,i)
c 145          continue
               call daxpy(m,r2(i,j),r3(1,i),1,c1(1,j,1),1)
  150       continue
  160    continue
         call dgefa(r6,m,m,r,ii)
         do 180 j = 1, m
            call dgesl(r6,m,m,r,r3(1,j),0)
            do 170 i = 1, m
c for in-line daxpy, activate next 3 lines and deactivate following 1 .
c              do 165 ii = 1, m
c                 r1(ii,j) = r1(ii,j) + r3(i,j)*r5(ii,i)
c 165          continue
               call daxpy(m,r3(i,j),r5(1,i),1,r1(1,j),1)
  170       continue
  180    continue
         if (n .eq. 2) go to 320
            do 200 j = 1, m
               do 190 i = 1, m
                  r6(i,j) = c2(i,j,1)
  190          continue
  200       continue
            do 310 i1 = 2, n1
               if (i1 .eq. n1) go to 230
                  do 220 j = 1, m
                     do 210 i = 1, m
                        r5(i,j) = c2(i,j,i1)
  210                continue
  220             continue
  230          continue
               do 260 j = 1, m
                  do 240 i = 1, m
                     c2(i,j,i1) = r6(i,j)
  240             continue
                  do 250 i = 1, m
c for in-line daxpy, activate next 3 lines and deactivate following 1 .
c                    do 245 ii = 1, m
c                       c2(ii,j,i1) = c2(ii,j,i1) + r3(i,j)*c1(ii,i,i1)
c 245                continue
                     call daxpy(m,r3(i,j),c1(1,i,i1),1,c2(1,j,i1),1)
  250             continue
  260          continue
               do 280 j = 1, m
                  do 270 i = 1, m
c for in-line daxpy, activate next 3 lines and deactivate following 1 .
c                    do 265 ii = 1, m
c                       c1(ii,j,i1) = c1(ii,j,i1) + r2(i,j)*r6(ii,i)
c 265                continue
                     call daxpy(m,r2(i,j),r6(1,i),1,c1(1,j,i1),1)
  270             continue
  280          continue
               do 300 j = 1, m
                  do 290 i = 1, m
                     r6(i,j) = r5(i,j)
  290             continue
  300          continue
  310       continue
  320    continue
         do 340 j = 1, m
            do 330 i = 1, m
               c2(i,j,1) = r3(i,j)
  330       continue
  340    continue
c
c        compute the solution of the system with the
c        principal minor of order m*n .
c
         do 360 j = 1, m
            do 350 i = 1, m
               r3(i,j) = r1(i,j)
  350       continue
            x(j,n) = b(j,n)
  360    continue
         do 380 i1 = 1, n1
            i2 = n - i1
            i3 = 1
            do 370 i = 1, m
c for in-line daxpy, activate next 4 lines and deactivate following 2 .
c              do 365 ii = 1, m
c                 x(ii,n) = x(ii,n) - x(i,i2)*a2(i3,i1)
c                 i3 = i3 + 1
c 365          continue
               call daxpy(m,-x(i,i2),a2(i3,i1),1,x(1,n),1)
               i3 = i3 + m
  370       continue
  380    continue
         call dgefa(r3,m,m,r,ii)
         call dgesl(r3,m,m,r,x(1,n),0)
         do 400 i1 = 1, n1
            do 390 i = 1, m
c for in-line daxpy, activate next 3 lines and deactivate following 1 .
c              do 385 ii = 1, m
c                 x(ii,i1) = x(ii,i1) + x(i,n)*c2(ii,i,i1)
c 385          continue
               call daxpy(m,x(i,n),c2(1,i,i1),1,x(1,i1),1)
  390       continue
  400    continue
  410 continue
  420 continue
      return
      end
      subroutine tgsld(a,x,r,m,l,lda)

c*********************************************************************72
c
cc TGSLD solves a linear system with a TG matrix.
c
c  Discussion:
c
c     tgsld calls tgsld1 to solve the double precision linear system
c     a * x = b
c     with the tg - matrix a .
c
c     on entry
c
c        a       double precision(m**2,2*l - 1)
c                the first row of blocks of the tg - matrix
c                followed by its first column of blocks beginning
c                with the second block. each block is represented
c                by columns. on return a is unaltered .
c
c        x       double precision(m*l)
c                the right hand side vector b .
c
c        r       double precision(m**2*(2*l + 3) + m)
c                a work vector .
c
c        m       integer
c                the order of the blocks of the matrix a .
c
c        l       integer
c                the number of blocks in a row or column
c                of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c     on return
c
c        x       the solution vector .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m,l,lda
      double precision a(lda,1),x(m,l),r(1)
      integer mm,mml,mml1,mml2,mml3,mml4,mml5,mml6
c
c     call subroutine tgsld1
c
      mm = m**2
      mml = mm*(l - 1) + 1
      mml1 = 2*mml - 1
      mml2 = mml1 + mm
      mml3 = mml2 + mm
      mml4 = mml3 + mm
      mml5 = mml4 + mm
      mml6 = mml5 + mm
c
      call tgsld1(a,a(1,l+1),x,x,r,r(mml),r(mml1),r(mml2),
     *            r(mml3),r(mml4),r(mml5),r(mml6),m,l,lda)
c
      return
      end
      subroutine tgslz1(a1,a2,b,x,c1,c2,r1,r2,r3,r5,r6,r,m,l,lda)

c*********************************************************************72
c
cc TGSLZ1 solves a linear system with a TG matrix.
c
      integer m,l,lda
      double complex a1(lda,l),a2(lda,1),b(m,l),x(m,l),c1(m,m,1),
     *     c2(m,m,1),r1(m,m),r2(m,m),r3(m,m),r5(m,m),r6(m,m),r(m)
c
c     tgslz1 solves the double complex linear system
c     a * x = b
c     with the tg - matrix a .
c
c     on entry
c
c        a1      double complex(m**2,l)
c                the first row of blocks of the tg - matrix a .
c                each block is represented by columns .
c                on return a1 is unaltered .
c
c        a2      double complex(m**2,l - 1)
c                the first column of blocks of the tg - matrix a
c                beginning with the second block. each block is
c                represented by columns. on return a2 is unaltered .
c
c        b       double complex(m*l)
c                the right hand side vector .
c                on return b is unaltered .
c
c        c1      double complex(m,m,l - 1)
c                a work array .
c
c        c2      double complex(m,m,l - 1)
c                a work array .
c
c        r1      double complex(m,m)
c                a work array .
c
c        r2      double complex(m,m)
c                a work array .
c
c        r3      double complex(m,m)
c                a work array .
c
c        r5      double complex(m,m)
c                a work array .
c
c        r6      double complex(m,m)
c                a work array .
c
c        r       double complex(m)
c                a work vector .
c
c        m       integer
c                the order of the blocks of the matrix a .
c
c        l       integer
c                the number of blocks in a row or column
c                of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c     on return
c
c        x       double complex(m*l)
c                the solution vector. x may coincide with b .
c
c     toeplitz package. this version dated 07/23/82 .
c
c     subroutines and functions
c
c        linpack ... zaxpy,zgefa,zgesl
c                ... (for in-line zaxpy, see directions in comments)
c
c     internal variables
c
      integer i,i1,i2,i3,ii,j,n,n1,n2
c
c     solve the system with the principal minor of order m .
c
      i3 = 1
      do 20 j = 1, m
         do 10 i = 1, m
            c1(i,j,1) = a1(i3,1)
            r1(i,j) = a1(i3,1)
            r3(i,j) = r1(i,j)
            i3 = i3 + 1
   10    continue
         x(j,1) = b(j,1)
   20 continue
      call zgefa(r3,m,m,r,ii)
      call zgesl(r3,m,m,r,x(1,1),0)
      if (l .eq. 1) go to 420
c
c     recurrent process for solving the system
c     with the tg - matrix for n = 2, l .
c
      do 410 n = 2, l
c
c        compute multiples of the first and last block columns of
c        the inverse of the principal minor of order m*n .
c
         n1 = n - 1
         n2 = n - 2
         i3 = 1
         do 40 j = 1, m
            do 30 i = 1, m
               r5(i,j) = a2(i3,n1)
               r6(i,j) = a1(i3,n)
               i3 = i3 + 1
   30       continue
   40    continue
         if (n .eq. 2) go to 100
            do 60 j = 1, m
               do 50 i = 1, m
                  c1(i,j,n1) = r2(i,j)
   50          continue
   60       continue
            do 90 i1 = 1, n2
               i2 = n - i1
               do 80 j = 1, m
                  i3 = 1
                  do 70 i = 1, m
c for in-line zaxpy, activate next 5 lines and deactivate following 3 .
c                    do 65 ii = 1, m
c                       r5(ii,j) = r5(ii,j) + c1(i,j,i2)*a2(i3,i1)
c                       r6(ii,j) = r6(ii,j) + c2(i,j,i1)*a1(i3,i1+1)
c                       i3 = i3 + 1
c  65                continue
                     call zaxpy(m,c1(i,j,i2),a2(i3,i1),1,r5(1,j),1)
                     call zaxpy(m,c2(i,j,i1),a1(i3,i1+1),1,r6(1,j),1)
                     i3 = i3 + m
   70             continue
   80          continue
   90       continue
  100    continue
         do 120 j = 1, m
            do 110 i = 1, m
               r2(i,j) = -r5(i,j)
  110       continue
            call zgesl(r3,m,m,r,r2(1,j),0)
  120    continue
         do 140 j = 1, m
            do 130 i = 1, m
               r3(i,j) = r6(i,j)
               r6(i,j) = -c1(i,j,1)
  130       continue
  140    continue
         do 160 j = 1, m
            do 150 i = 1, m
c for in-line zaxpy, activate next 3 lines and deactivate following 1 .
c              do 145 ii = 1, m
c                 c1(ii,j,1) = c1(ii,j,1) + r2(i,j)*r3(ii,i)
c 145          continue
               call zaxpy(m,r2(i,j),r3(1,i),1,c1(1,j,1),1)
  150       continue
  160    continue
         call zgefa(r6,m,m,r,ii)
         do 180 j = 1, m
            call zgesl(r6,m,m,r,r3(1,j),0)
            do 170 i = 1, m
c for in-line zaxpy, activate next 3 lines and deactivate following 1 .
c              do 165 ii = 1, m
c                 r1(ii,j) = r1(ii,j) + r3(i,j)*r5(ii,i)
c 165          continue
               call zaxpy(m,r3(i,j),r5(1,i),1,r1(1,j),1)
  170       continue
  180    continue
         if (n .eq. 2) go to 320
            do 200 j = 1, m
               do 190 i = 1, m
                  r6(i,j) = c2(i,j,1)
  190          continue
  200       continue
            do 310 i1 = 2, n1
               if (i1 .eq. n1) go to 230
                  do 220 j = 1, m
                     do 210 i = 1, m
                        r5(i,j) = c2(i,j,i1)
  210                continue
  220             continue
  230          continue
               do 260 j = 1, m
                  do 240 i = 1, m
                     c2(i,j,i1) = r6(i,j)
  240             continue
                  do 250 i = 1, m
c for in-line zaxpy, activate next 3 lines and deactivate following 1 .
c                    do 245 ii = 1, m
c                       c2(ii,j,i1) = c2(ii,j,i1) + r3(i,j)*c1(ii,i,i1)
c 245                continue
                     call zaxpy(m,r3(i,j),c1(1,i,i1),1,c2(1,j,i1),1)
  250             continue
  260          continue
               do 280 j = 1, m
                  do 270 i = 1, m
c for in-line zaxpy, activate next 3 lines and deactivate following 1 .
c                    do 265 ii = 1, m
c                       c1(ii,j,i1) = c1(ii,j,i1) + r2(i,j)*r6(ii,i)
c 265                continue
                     call zaxpy(m,r2(i,j),r6(1,i),1,c1(1,j,i1),1)
  270             continue
  280          continue
               do 300 j = 1, m
                  do 290 i = 1, m
                     r6(i,j) = r5(i,j)
  290             continue
  300          continue
  310       continue
  320    continue
         do 340 j = 1, m
            do 330 i = 1, m
               c2(i,j,1) = r3(i,j)
  330       continue
  340    continue
c
c        compute the solution of the system with the
c        principal minor of order m*n .
c
         do 360 j = 1, m
            do 350 i = 1, m
               r3(i,j) = r1(i,j)
  350       continue
            x(j,n) = b(j,n)
  360    continue
         do 380 i1 = 1, n1
            i2 = n - i1
            i3 = 1
            do 370 i = 1, m
c for in-line zaxpy, activate next 4 lines and deactivate following 2 .
c              do 365 ii = 1, m
c                 x(ii,n) = x(ii,n) - x(i,i2)*a2(i3,i1)
c                 i3 = i3 + 1
c 365          continue
               call zaxpy(m,-x(i,i2),a2(i3,i1),1,x(1,n),1)
               i3 = i3 + m
  370       continue
  380    continue
         call zgefa(r3,m,m,r,ii)
         call zgesl(r3,m,m,r,x(1,n),0)
         do 400 i1 = 1, n1
            do 390 i = 1, m
c for in-line zaxpy, activate next 3 lines and deactivate following 1 .
c              do 385 ii = 1, m
c                 x(ii,i1) = x(ii,i1) + x(i,n)*c2(ii,i,i1)
c 385          continue
               call zaxpy(m,x(i,n),c2(1,i,i1),1,x(1,i1),1)
  390       continue
  400    continue
  410 continue
  420 continue
      return
      end
      subroutine tgslz(a,x,r,m,l,lda)

c*********************************************************************72
c
cc TGSLZ solves a linear system with a TG matrix.
c
      integer m,l,lda
      double complex a(lda,1),x(m,l),r(1)
c
c     tgslz calls tgslz1 to solve the double complex linear system
c     a * x = b
c     with the tg - matrix a .
c
c     on entry
c
c        a       double complex(m**2,2*l - 1)
c                the first row of blocks of the tg - matrix
c                followed by its first column of blocks beginning
c                with the second block. each block is represented
c                by columns. on return a is unaltered .
c
c        x       double complex(m*l)
c                the right hand side vector b .
c
c        r       double complex(m**2*(2*l + 3) + m)
c                a work vector .
c
c        m       integer
c                the order of the blocks of the matrix a .
c
c        l       integer
c                the number of blocks in a row or column
c                of the matrix a .
c
c        lda     integer
c                the leading dimension of the array a .
c
c     on return
c
c        x       the solution vector .
c
c     toeplitz package. this version dated 07/23/82 .
c
c     subroutines and functions
c
c        toeplitz package ... tgslz1
c
c     internal variables
c
      integer mm,mml,mml1,mml2,mml3,mml4,mml5,mml6
c
c     call subroutine tgslz1
c
      mm = m**2
      mml = mm*(l - 1) + 1
      mml1 = 2*mml - 1
      mml2 = mml1 + mm
      mml3 = mml2 + mm
      mml4 = mml3 + mm
      mml5 = mml4 + mm
      mml6 = mml5 + mm
c
      call tgslz1(a,a(1,l+1),x,x,r,r(mml),r(mml1),r(mml2),
     *            r(mml3),r(mml4),r(mml5),r(mml6),m,l,lda)
c
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
      subroutine tsld1(a1,a2,b,x,c1,c2,m)

c*********************************************************************72
c
cc TSLD1 solves a linear system with a T matrix.
c
      integer m
      double precision a1(m),a2(1),b(m),x(m),c1(1),c2(1)
c
c     tsld1 solves the double precision linear system
c     a * x = b
c     with the t - matrix a .
c
c     on entry
c
c        a1      double precision(m)
c                the first row of the t - matrix a .
c                on return a1 is unaltered .
c
c        a2      double precision(m - 1)
c                the first column of the t - matrix a
c                beginning with the second element .
c                on return a2 is unaltered .
c
c        b       double precision(m)
c                the right hand side vector .
c                on return b is unaltered .
c
c        c1      double precision(m - 1)
c                a work vector .
c
c        c2      double precision(m - 1)
c                a work vector .
c
c        m       integer
c                the order of the matrix a .
c
c     on return
c
c        x       double precision(m)
c                the solution vector. x may coincide with b .
c
c     toeplitz package. this version dated 07/23/82 .
c
c     internal variables
c
      integer i1,i2,n,n1,n2
      double precision r1,r2,r3,r5,r6
c
c     solve the system with the principal minor of order 1 .
c
      r1 = a1(1)
      x(1) = b(1)/r1
      if (m .eq. 1) go to 80
c
c     recurrent process for solving the system
c     with the t - matrix for n = 2, m .
c
      do 70 n = 2, m
c
c        compute multiples of the first and last columns of
c        the inverse of the principal minor of order n .
c
         n1 = n - 1
         n2 = n - 2
         r5 = a2(n1)
         r6 = a1(n)
         if (n .eq. 2) go to 20
            c1(n1) = r2
            do 10 i1 = 1, n2
               i2 = n - i1
               r5 = r5 + a2(i1)*c1(i2)
               r6 = r6 + a1(i1+1)*c2(i1)
   10       continue
   20    continue
         r2 = -r5/r1
         r3 = -r6/r1
         r1 = r1 + r5*r3
         if (n .eq. 2) go to 40
            r6 = c2(1)
            c2(n1) = 0.0d0
            do 30 i1 = 2, n1
               r5 = c2(i1)
               c2(i1) = c1(i1)*r3 + r6
               c1(i1) = c1(i1) + r6*r2
               r6 = r5
   30       continue
   40    continue
         c2(1) = r3
c
c        compute the solution of the system with the
c        principal minor of order n .
c
         r5 = 0.0d0
         do 50 i1 = 1, n1
            i2 = n - i1
            r5 = r5 + a2(i1)*x(i2)
   50    continue
         r6 = (b(n) - r5)/r1
         do 60 i1 = 1, n1
            x(i1) = x(i1) + c2(i1)*r6
   60    continue
         x(n) = r6
   70 continue
   80 continue
      return
      end
      subroutine tsld(a,x,r,m)

c*********************************************************************72
c
cc TSLD solves a linear system with a T matrix.
c
c  Discussion:
c
c     tsld calls tsld1 to solve the double precision linear system
c     a * x = b
c     with the t - matrix a .
c
c     on entry
c
c        a       double precision(2*m - 1)
c                the first row of the t - matrix followed by its
c                first column beginning with the second element .
c                on return a is unaltered .
c
c        x       double precision(m)
c                the right hand side vector b .
c
c        r       double precision(2*m - 2)
c                a work vector .
c
c        m       integer
c                the order of the matrix a .
c
c     on return
c
c        x       the solution vector .
c
c     toeplitz package. this version dated 07/23/82 .
c
      integer m

      double precision a(1),x(m),r(1)

      call tsld1 ( a, a(m+1), x, x, r, r(m), m )

      return
      end
      subroutine tslz1(a1,a2,b,x,c1,c2,m)

c*********************************************************************72
c
cc TSLZ1 solves a linear system with a T matrix.
c
      integer m
      double complex a1(m),a2(1),b(m),x(m),c1(1),c2(1)
c
c     tslz1 solves the double complex linear system
c     a * x = b
c     with the t - matrix a .
c
c     on entry
c
c        a1      double complex(m)
c                the first row of the t - matrix a .
c                on return a1 is unaltered .
c
c        a2      double complex(m - 1)
c                the first column of the t - matrix a
c                beginning with the second element .
c                on return a2 is unaltered .
c
c        b       double complex(m)
c                the right hand side vector .
c                on return b is unaltered .
c
c        c1      double complex(m - 1)
c                a work vector .
c
c        c2      double complex(m - 1)
c                a work vector .
c
c        m       integer
c                the order of the matrix a .
c
c     on return
c
c        x       double complex(m)
c                the solution vector. x may coincide with b .
c
c     toeplitz package. this version dated 07/23/82 .
c
c     internal variables
c
      integer i1,i2,n,n1,n2
      double complex r1,r2,r3,r5,r6
c
c     solve the system with the principal minor of order 1 .
c
      r1 = a1(1)
      x(1) = b(1)/r1
      if (m .eq. 1) go to 80
c
c     recurrent process for solving the system
c     with the t - matrix for n = 2, m .
c
      do 70 n = 2, m
c
c        compute multiples of the first and last columns of
c        the inverse of the principal minor of order n .
c
         n1 = n - 1
         n2 = n - 2
         r5 = a2(n1)
         r6 = a1(n)
         if (n .eq. 2) go to 20
            c1(n1) = r2
            do 10 i1 = 1, n2
               i2 = n - i1
               r5 = r5 + a2(i1)*c1(i2)
               r6 = r6 + a1(i1+1)*c2(i1)
   10       continue
   20    continue
         r2 = -r5/r1
         r3 = -r6/r1
         r1 = r1 + r5*r3
         if (n .eq. 2) go to 40
            r6 = c2(1)
            c2(n1) = (0.0d0,0.0d0)
            do 30 i1 = 2, n1
               r5 = c2(i1)
               c2(i1) = c1(i1)*r3 + r6
               c1(i1) = c1(i1) + r6*r2
               r6 = r5
   30       continue
   40    continue
         c2(1) = r3
c
c        compute the solution of the system with the
c        principal minor of order n .
c
         r5 = (0.0d0,0.0d0)
         do 50 i1 = 1, n1
            i2 = n - i1
            r5 = r5 + a2(i1)*x(i2)
   50    continue
         r6 = (b(n) - r5)/r1
         do 60 i1 = 1, n1
            x(i1) = x(i1) + c2(i1)*r6
   60    continue
         x(n) = r6
   70 continue
   80 continue
      return
      end
      subroutine tslz(a,x,r,m)

c*********************************************************************72
c
cc TSLZ solves a linear system with a T matrix.
c
c  Discussion:
c
c    tslz calls tslz1 to solve the double complex linear system
c      a * x = b
c    with the t - matrix a.
c
c     on entry
c
c        a       double complex(2*m - 1)
c                the first row of the t - matrix followed by its
c                first column beginning with the second element .
c                on return a is unaltered .
c
c        x       double complex(m)
c                the right hand side vector b .
c
c        r       double complex(2*m - 2)
c                a work vector .
c
c        m       integer
c                the order of the matrix a .
c
c     on return
c
c        x       the solution vector .
c
c     toeplitz package. this version dated 07/23/82 .
c
c     subroutines and functions
c
c        toeplitz package ... tslz1
c
c     call subroutine tslz1
c
      integer m
      double complex a(1),x(m),r(1)

      call tslz1(a,a(m+1),x,x,r,r(m),m)

      return
      end
      subroutine zaxpy(n,za,zx,incx,zy,incy)

c*********************************************************************72
c
cc ZAXPY adds a multiple of one vector to another.
c
c     constant times a vector plus a vector.
c     jack dongarra, 3/11/78.
c
      double complex zx(1),zy(1),za
      double precision dcabs1
      if(n.le.0)return
      if (dcabs1(za) .eq. 0.0d0) return
      if (incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        zy(iy) = zy(iy) + za*zx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
   20 do 30 i = 1,n
        zy(i) = zy(i) + za*zx(i)
   30 continue
      return
      end
      function zdotc(n,zx,incx,zy,incy)

c*********************************************************************72
c
cc ZDOTC computes the dot product of two vectors.
c
c     forms the dot product of a vector.
c     jack dongarra, 3/11/78.
c
      double complex zdotc
      double complex zx(1),zy(1),ztemp
      ztemp = (0.0d0,0.0d0)
      zdotc = (0.0d0,0.0d0)
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
        ztemp = ztemp + dconjg(zx(ix))*zy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      zdotc = ztemp
      return
c
c        code for both increments equal to 1
c
   20 do 30 i = 1,n
        ztemp = ztemp + dconjg(zx(i))*zy(i)
   30 continue
      zdotc = ztemp
      return
      end
      subroutine zdscal(n,da,zx,incx)

c*********************************************************************72
c
cc ZDSCAL scales a vector by a constant.
c
c     scales a vector by a constant.
c     jack dongarra, 3/11/78.
c
      double complex zx(1)
      double precision da
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increments not equal to 1
c
      ix = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      do 10 i = 1,n
        zx(ix) = dcmplx(da,0.0d0)*zx(ix)
        ix = ix + incx
   10 continue
      return
c
c        code for increments equal to 1
c
   20 do 30 i = 1,n
        zx(i) = dcmplx(da,0.0d0)*zx(i)
   30 continue
      return
      end
      subroutine zgefa(a,lda,n,ipvt,info)

c*********************************************************************72
c
cc ZGEFA factors a general matrix.
c
      integer lda,n,ipvt(1),info
      complex*16 a(lda,1)
c
c     zgefa factors a complex*16 matrix by gaussian elimination.
c
c     zgefa is usually called by zgeco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for zgeco) = (1 + 9/n)*(time for zgefa) .
c
c     on entry
c
c        a       complex*16(lda, n)
c                the matrix to be factored.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix and the multipliers
c                which were used to obtain it.
c                the factorization can be written  a = l*u  where
c                l  is a product of permutation and unit lower
c                triangular matrices and  u  is upper triangular.
c
c        ipvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if  u(k,k) .eq. 0.0 .  this is not an error
c                     condition for this subroutine, but it does
c                     indicate that zgesl or zgedi will divide by zero
c                     if called.  use  rcond  in zgeco for a reliable
c                     indication of singularity.
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas zaxpy,zscal,izamax
c     fortran dabs
c
c     internal variables
c
      complex*16 t
      integer izamax,j,k,kp1,l,nm1
c
      complex*16 zdum
      double precision cabs1
      double precision dreal,dimag
      complex*16 zdumr,zdumi
      dreal(zdumr) = zdumr
      dimag(zdumi) = (0.0d0,-1.0d0)*zdumi
      cabs1(zdum) = dabs(dreal(zdum)) + dabs(dimag(zdum))
c
c     gaussian elimination with partial pivoting
c
      info = 0
      nm1 = n - 1
      if (nm1 .lt. 1) go to 70
      do 60 k = 1, nm1
         kp1 = k + 1
c
c        find l = pivot index
c
         l = izamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k) = l
c
c        zero pivot implies this column already triangularized
c
         if (cabs1(a(l,k)) .eq. 0.0d0) go to 40
c
c           interchange if necessary
c
            if (l .eq. k) go to 10
               t = a(l,k)
               a(l,k) = a(k,k)
               a(k,k) = t
   10       continue
c
c           compute multipliers
c
            t = -(1.0d0,0.0d0)/a(k,k)
            call zscal(n-k,t,a(k+1,k),1)
c
c           row elimination with column indexing
c
            do 30 j = kp1, n
               t = a(l,j)
               if (l .eq. k) go to 20
                  a(l,j) = a(k,j)
                  a(k,j) = t
   20          continue
               call zaxpy(n-k,t,a(k+1,k),1,a(k+1,j),1)
   30       continue
         go to 50
   40    continue
            info = k
   50    continue
   60 continue
   70 continue
      ipvt(n) = n
      if (cabs1(a(n,n)) .eq. 0.0d0) info = n
      return
      end
      subroutine zgesl(a,lda,n,ipvt,b,job)

c*********************************************************************72
c
cc ZGESL solves a linear system involving a factored general matrix.
c
      integer lda,n,ipvt(1),job
      complex*16 a(lda,1),b(1)
c
c     zgesl solves the complex*16 system
c     a * x = b  or  ctrans(a) * x = b
c     using the factors computed by zgeco or zgefa.
c
c     on entry
c
c        a       complex*16(lda, n)
c                the output from zgeco or zgefa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        ipvt    integer(n)
c                the pivot vector from zgeco or zgefa.
c
c        b       complex*16(n)
c                the right hand side vector.
c
c        job     integer
c                = 0         to solve  a*x = b ,
c                = nonzero   to solve  ctrans(a)*x = b  where
c                            ctrans(a)  is the conjugate transpose.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains a
c        zero on the diagonal.  technically this indicates singularity
c        but it is often caused by improper arguments or improper
c        setting of lda .  it will not occur if the subroutines are
c        called correctly and if zgeco has set rcond .gt. 0.0
c        or zgefa has set info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call zgeco(a,lda,n,ipvt,rcond,z)
c           if (rcond is too small) go to ...
c           do 10 j = 1, p
c              call zgesl(a,lda,n,ipvt,c(1,j),0)
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas zaxpy,zdotc
c     fortran dconjg
c
c     internal variables
c
      complex*16 zdotc,t
      integer k,kb,l,nm1
      double precision dreal,dimag
      complex*16 zdumr,zdumi
      dreal(zdumr) = zdumr
      dimag(zdumi) = (0.0d0,-1.0d0)*zdumi
c
      nm1 = n - 1
      if (job .ne. 0) go to 50
c
c        job = 0 , solve  a * x = b
c        first solve  l*y = b
c
         if (nm1 .lt. 1) go to 30
         do 20 k = 1, nm1
            l = ipvt(k)
            t = b(l)
            if (l .eq. k) go to 10
               b(l) = b(k)
               b(k) = t
   10       continue
            call zaxpy(n-k,t,a(k+1,k),1,b(k+1),1)
   20    continue
   30    continue
c
c        now solve  u*x = y
c
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/a(k,k)
            t = -b(k)
            call zaxpy(k-1,t,a(1,k),1,b(1),1)
   40    continue
      go to 100
   50 continue
c
c        job = nonzero, solve  ctrans(a) * x = b
c        first solve  ctrans(u)*y = b
c
         do 60 k = 1, n
            t = zdotc(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k) - t)/dconjg(a(k,k))
   60    continue
c
c        now solve ctrans(l)*x = y
c
         if (nm1 .lt. 1) go to 90
         do 80 kb = 1, nm1
            k = n - kb
            b(k) = b(k) + zdotc(n-k,a(k+1,k),1,b(k+1),1)
            l = ipvt(k)
            if (l .eq. k) go to 70
               t = b(l)
               b(l) = b(k)
               b(k) = t
   70       continue
   80    continue
   90    continue
  100 continue
      return
      end
      subroutine zscal(n,za,zx,incx)

c*********************************************************************72
c
cc ZSCAL scales a vector by a constant.
c
c    scales a vector by a constant.
c    jack dongarra, 3/11/78.
c
      double complex za,zx(1)
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increments not equal to 1
c
      ix = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      do 10 i = 1,n
        zx(ix) = za*zx(ix)
        ix = ix + incx
   10 continue
      return
c
c        code for increments equal to 1
c
   20 do 30 i = 1,n
        zx(i) = za*zx(i)
   30 continue
      return
      end
      subroutine ztrdi(t,ldt,n,det,job,info)

c*********************************************************************72
c
cc ZTRDI computes the determinant and inverse of a triangular matrix.
c
      integer ldt,n,job,info
      complex*16 t(ldt,1),det(2)
c
c     ztrdi computes the determinant and inverse of a complex*16
c     triangular matrix.
c
c     on entry
c
c        t       complex*16(ldt,n)
c                t contains the triangular matrix. the zero
c                elements of the matrix are not referenced, and
c                the corresponding elements of the array can be
c                used to store other information.
c
c        ldt     integer
c                ldt is the leading dimension of the array t.
c
c        n       integer
c                n is the order of the system.
c
c        job     integer
c                = 010       no det, inverse of lower triangular.
c                = 011       no det, inverse of upper triangular.
c                = 100       det, no inverse.
c                = 110       det, inverse of lower triangular.
c                = 111       det, inverse of upper triangular.
c
c     on return
c
c        t       inverse of original matrix if requested.
c                otherwise unchanged.
c
c        det     complex*16(2)
c                determinant of original matrix if requested.
c                otherwise not referenced.
c                determinant = det(1) * 10.0**det(2)
c                with  1.0 .le. cabs1(det(1)) .lt. 10.0
c                or  det(1) .eq. 0.0 .
c
c        info    integer
c                info contains zero if the system is nonsingular
c                and the inverse is requested.
c                otherwise info contains the index of
c                a zero diagonal element of t.
c
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas zaxpy,zscal
c     fortran dabs,dcmplx,mod
c
c     internal variables
c
      complex*16 temp
      double precision ten
      integer i,j,k,kb,km1,kp1
c
      complex*16 zdum
      double precision cabs1
      double precision dreal,dimag
      complex*16 zdumr,zdumi
      dreal(zdumr) = zdumr
      dimag(zdumi) = (0.0d0,-1.0d0)*zdumi
      cabs1(zdum) = dabs(dreal(zdum)) + dabs(dimag(zdum))
c     begin block permitting ...exits to 180
c
c        compute determinant
c
         if (job/100 .eq. 0) go to 70
            det(1) = (1.0d0,0.0d0)
            det(2) = (0.0d0,0.0d0)
            ten = 10.0d0
            do 50 i = 1, n
               det(1) = t(i,i)*det(1)
c           ...exit
               if (cabs1(det(1)) .eq. 0.0d0) go to 60
   10          if (cabs1(det(1)) .ge. 1.0d0) go to 20
                  det(1) = dcmplx(ten,0.0d0)*det(1)
                  det(2) = det(2) - (1.0d0,0.0d0)
               go to 10
   20          continue
   30          if (cabs1(det(1)) .lt. ten) go to 40
                  det(1) = det(1)/dcmplx(ten,0.0d0)
                  det(2) = det(2) + (1.0d0,0.0d0)
               go to 30
   40          continue
   50       continue
   60       continue
   70    continue
c
c        compute inverse of upper triangular
c
         if (mod(job/10,10) .eq. 0) go to 170
            if (mod(job,10) .eq. 0) go to 120
c              begin block permitting ...exits to 110
                  do 100 k = 1, n
                     info = k
c              ......exit
                     if (cabs1(t(k,k)) .eq. 0.0d0) go to 110
                     t(k,k) = (1.0d0,0.0d0)/t(k,k)
                     temp = -t(k,k)
                     call zscal(k-1,temp,t(1,k),1)
                     kp1 = k + 1
                     if (n .lt. kp1) go to 90
                     do 80 j = kp1, n
                        temp = t(k,j)
                        t(k,j) = (0.0d0,0.0d0)
                        call zaxpy(k,temp,t(1,k),1,t(1,j),1)
   80                continue
   90                continue
  100             continue
                  info = 0
  110          continue
            go to 160
  120       continue
c
c              compute inverse of lower triangular
c
               do 150 kb = 1, n
                  k = n + 1 - kb
                  info = k
c     ............exit
                  if (cabs1(t(k,k)) .eq. 0.0d0) go to 180
                  t(k,k) = (1.0d0,0.0d0)/t(k,k)
                  temp = -t(k,k)
                  if (k .ne. n) call zscal(n-k,temp,t(k+1,k),1)
                  km1 = k - 1
                  if (km1 .lt. 1) go to 140
                  do 130 j = 1, km1
                     temp = t(k,j)
                     t(k,j) = (0.0d0,0.0d0)
                     call zaxpy(n-k+1,temp,t(k,k),1,t(k,j),1)
  130             continue
  140             continue
  150          continue
               info = 0
  160       continue
  170    continue
  180 continue
      return
      end
