      program main

c*********************************************************************72
c
cc  MAIN is the main program for EISPACK_TEST.
c
c  Discussion:
c
c    EISPACK_TEST tests the EISPACK library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer matz

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'EISPACK_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the EISPACK library.'

      call balanc_test ( )

      call bandv_test ( )

      call bisect_test ( )

      call bqr_test ( )

      call cbal_test ( )

      matz = 0
      call cg_lr_test ( matz )
      matz = 1
      call cg_lr_test ( matz )

      matz = 0
      call cg_qr_test ( matz )
      matz = 1
      call cg_qr_test ( matz )

      matz = 0
      call ch_test ( matz )
      matz = 1
      call ch_test ( matz )

      matz = 0
      call ch3_test ( matz )
      matz = 1
      call ch3_test ( matz )

      call cinvit_test ( )

      call imtqlv_test ( )

      call invit_test ( )

      call minfit_test ( )

      matz = 0
      call rg_elm_test ( matz )
      matz = 1
      call rg_elm_test ( matz )

      matz = 0
      call rg_ort_test ( matz )
      matz = 1
      call rg_ort_test ( matz )

      matz = 0
      call rgg_test ( matz )
      matz = 1
      call rgg_test ( matz )

      matz = 0
      call rs_test ( matz )
      matz = 1
      call rs_test ( matz )

      matz = 0
      call rsb_test ( matz )
      matz = 1
      call rsb_test ( matz )

      matz = 0
      call rsg_test ( matz )
      matz = 1
      call rsg_test ( matz )

      matz = 0
      call rsgab_test ( matz )
      matz = 1
      call rsgab_test ( matz )

      matz = 0
      call rsgba_test ( matz )
      matz = 1
      call rsgba_test ( matz )

      call rsm_test ( )

      matz = 0
      call rsp_test ( matz )
      matz = 1
      call rsp_test ( matz )

      matz = 0
      call rspp_test ( matz )
      matz = 1
      call rspp_test ( matz )

      matz = 0
      call rst_test ( matz ) 
      matz = 1
      call rst_test ( matz )

      matz = 0
      call rt_test ( matz )
      matz = 1
      call rt_test ( matz )

      call svd_test ( )

      call tql1_test ( )

      call tridib_test ( )

      call tsturm_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'EISPACK_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop 0
      end
      subroutine balanc_test ( )

c*********************************************************************72
c
cc BALANC_TEST tests BALANC.
c
c  Discussion:
c
c    BALANC balances a matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer nm
      parameter ( nm = n )

      double precision a(nm,n)
      integer i
      integer igh
      integer low
      double precision scale(n)

      save a

      data a /
     & 110.0D+00,  0.0D+00, 310.0D+00,  0.0D+00, 510.0D+00, 
     &  12.0D+00, 22.0D+00,  32.0D+00,  0.0D+00,   0.0D+00, 
     &  13.0D+00,  0.0D+00,  33.0D+00, 43.0D+00,  53.0D+00, 
     &   0.0D+00,  0.0D+00,   0.0D+00, 44.0D+00,   0.0D+00, 
     &  15.0D+00,  0.0D+00,  35.0D+00,  0.0D+00,  55.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BALANC_TEST'
      write ( *, '(a)' ) '  BALANC balances a real general matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      low = 1
      igh = n
      do i = 1, n
        scale(i) = 0.0D+00
      end do

      call balanc ( nm, n, a, low, igh, scale )
  
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  LOW = ', low
      write ( *, '(a,i4)' ) '  IGH = ', igh

      call r8vec_print ( n, scale, '  Scaling vector SCALE:' )

      call r8mat_print ( n, n, a, '  The balanced matrix A:' )

      return
      end
      subroutine bandv_test ( )

c*****************************************************************************80
c
cc BANDV_TEST tests RSB.
c
c  Discussion:
c
c    BANDV computes eigenvectors of a symmetric banded matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 February 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer mb
      parameter ( mb = 2 )
      integer nv
      parameter ( nv = n * ( 2 * mb - 1 ) )

      double precision a(n,mb)
      double precision a2(n,n)
      double precision e21
      double precision fv1(n)
      double precision fv2(n)
      integer i
      integer ierr
      integer j
      integer m
      integer matz
      integer nm
      double precision r(n,n)
      double precision rv(nv)
      double precision rv6(n)
      double precision w(n)
      double precision x(n,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BANDV_TEST'
      write ( *, '(a)' ) '  BANDV computes the eigenvectors'
      write ( *, '(a)' ) '  of a real symmetric band matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n
      write ( *, '(a,i8)' ) '  Half bandwidth + 1 = ', mb

      do i = 1, n
        do j = 1, mb
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        a(i,mb) = 2.0D+00
      end do

      do i = 2, n
        a(i,1) = -1.0D+00
      end do

      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a2(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a2(i,j) = - 1.0D+00
          else
            a2(i,j) = 0.0D+00
          end if
        end do
      end do
c
c  A2 contains the band matrix in full storage.
c
      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a2(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a2(i,j) = - 1.0D+00
          else
            a2(i,j) = 0.0D+00
          end if
        end do
      end do

      call r8mat_print ( n, n, a2, '  The matrix A:' )
c
c  Get eivenvalues from RSB.
c
      nm = n
      matz = 0

      call rsb ( nm, n, mb, a, w, matz, x, fv1, fv2, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'BANDV_TEST - Warning!'
        write ( *, '(a,i8)' ) '  RSB error return flag IERR = ', ierr
        return
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )
c
c  Now get eivenvectors from BANDV.
c  RSB altered A, so we have to restore it.
c
      do i = 1, n
        do j = 1, mb
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        a(i,mb) = 2.0D+00
      end do

      do i = 2, n
        a(i,1) = -1.0D+00
      end do

      e21 = 0.0D+00
      m = n
      call bandv ( nm, n, mb, a, e21, m, w, x, ierr, nv, rv, rv6 )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'BANDV_TEST - Warning!'
        write ( *, '(a,i8)' ) '  BANDV error return flag IERR = ', ierr
        return
      end if
  
      call r8mat_print ( n, n, x, '  The eigenvector matrix X:' )

      r = matmul ( a2, x )

      do j = 1, n
        do i = 1, n
          r(i,j) = r(i,j) - w(j) * x(i,j)
        end do
      end do

      call r8mat_print ( n, n, r, '  The residual (A-Lambda*I)*X:' )

      return
      end
      subroutine bisect_test ( )

c*********************************************************************72
c
cc BISECT_TEST tests BISECT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
 
      double precision a(n,n)
      double precision d(n)
      double precision e(n)
      double precision e2(n)
      double precision eps1
      integer i
      integer ierr
      integer ind(n)
      integer j
      double precision lb
      integer m
      integer mm
      integer nm
      double precision r8_epsilon
      double precision rv4(n)
      double precision rv5(n)
      double precision ub
      double precision w(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BISECT_TEST'
      write ( *, '(a)' ) '  BISECT computes some eigenvalues of '
      write ( *, '(a)' ) '  a real symmetric tridiagonal matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      nm = 5
      eps1 = 100.0D+00 * r8_epsilon ( )
!
!  Here is where the matrix is defined.
!
      do i = 1, n
        d(i) = 2.0D+00
      end do

      e(1) = 0.0D+00
      do i = 2, n
        e(i) = -1.0D+00
      end do
      do i = 1, n
        e2(i) = e(i) ** 2
      end do
      lb = -5.0D+00
      ub = +5.0D+00
      mm = 5
!
!  We only set up and store the matrix A this way in order to make it easy
!  to compute the residual.
!
      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a(i,j) = - 1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call bisect ( n, eps1, d, e, e2, lb, ub, mm, m, w, ind, 
     &  ierr, rv4, rv5 )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'BISECT_TEST - Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
        return
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      return
      end
      subroutine bqr_test ( )

c*********************************************************************72
c
cc BQR_TEST tests BQR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 February 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer mb
      parameter ( mb = 2 )
      integer n
      parameter ( n = 5 )
      integer nv
      parameter ( nv = 2 * mb**2 + 4 * mb - 3)

      double precision a(n,mb)
      integer i
      integer ierr
      integer j
      integer n2
      integer nm
      double precision r
      double precision rv(nv)
      double precision shift
      double precision t

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BQR_TEST'
      write ( *, '(a)' ) '  BQR computes some eigenvalues'
      write ( *, '(a)' ) '  of a real symmetric band matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n
      write ( *, '(a,i8)' ) '  Half bandwidth+1 = ', mb

      do i = 1, n
        do j = 1, mb
          if ( j == 1 ) then
            if ( i .eq. 1 ) then
              a(i,j) = 0.0D+00
            else
              a(i,j) = -1.0D+00
            end if
          else if ( j .eq. 2 ) then
            a(i,j) = 2.0D+00
          end if
        end do
      end do

      call r8mat_print ( n, mb, a, '  The compressed matrix A:' )

      nm = n
      n2 = n
      t = 0.0D+00
      r = 0.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Eigenvalues:'
      write ( *, '(a)' ) ''

      do i = 1, n

        shift = t
        call bqr ( nm, n2, mb, a, t, r, ierr, nv, rv )

        if ( ierr .ne. 0 ) then
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'BQR_TEST - Warning!'
          write ( *, '(a,i8)' ) '  IERR = ', ierr
          return
        end if

        write ( *, * ) i, t

        n2 = n2 - 1

      end do

      return
      end
      subroutine cbal_test ( )

c*********************************************************************72
c
cc CBAL_TEST tests CBAL.
c
c  Discussion:
c
c    CBAL balances a complex matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer nm
      parameter ( nm = n )

      double precision ar(nm,n)
      double precision ai(nm,n)
      integer i
      integer igh
      integer low
      double precision scale(n)

      save ar
      save ai

      data ar /
     & 110.0D+00,  0.0D+00, 310.0D+00,  0.0D+00, 510.0D+00, 
     &  12.0D+00, 22.0D+00,  32.0D+00,  0.0D+00,   0.0D+00, 
     &  13.0D+00,  0.0D+00,  33.0D+00, 43.0D+00,  53.0D+00, 
     &   0.0D+00,  0.0D+00,   0.0D+00, 44.0D+00,   0.0D+00, 
     &  15.0D+00,  0.0D+00,  35.0D+00,  0.0D+00,  55.0D+00 /

      data ai /
     & 110.5D+00,  0.0D+00, 310.5D+00,  0.0D+00, 510.5D+00, 
     &  12.5D+00, 22.5D+00,  32.5D+00,  0.0D+00,   0.0D+00, 
     &  13.5D+00,  0.0D+00,  33.5D+00, 43.5D+00,  53.5D+00, 
     &   0.0D+00,  0.0D+00,   0.0D+00, 44.5D+00,   0.0D+00, 
     &  15.5D+00,  0.0D+00,  35.5D+00,  0.0D+00,  55.5D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CBAL_TEST'
      write ( *, '(a)' ) '  CBAL balances a complex general matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, ar, '  The matrix AR:' )
      call r8mat_print ( n, n, ai, '  The matrix AI:' )

      low = 1
      igh = n
      do i = 1, n
        scale(i) = 0.0D+00
      end do

      call cbal ( nm, n, ar, ai, low, igh, scale )
  
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  LOW = ', low
      write ( *, '(a,i4)' ) '  IGH = ', igh

      call r8vec_print ( n, scale, '  Scaling vector SCALE:' )

      call r8mat_print ( n, n, ar, '  The balanced matrix AR:' )
      call r8mat_print ( n, n, ai, '  The balanced matrix AI:' )

      return
      end
      subroutine cg_lr_test ( matz )

c*********************************************************************72
c
cc  CG_LR_TEST tests CG_LR.
c
c  Discussion:
c
c    CG_LR is for the eigenvalues of a complex general matrix.
c
c    The eigenvalues of such a matrix are in general complex.
c    We will use the same example we used before, namely
c    a hermitian matrix, so the eigenvalues will in fact be real.
c
c    (3     1     0     0+2i)
c    (1     3     0-2i  0   )
c    (0     0+2i  1     1   )
c    (0-2i  0     1     1   )
c
c    The eigenvalues are 2+2*sqrt(2), 2-2*sqrt(2), 4 and 0
c
c    The eigenvector matrix is
c
c    (  1+sqrt(2),  1,                -1,          1)
c    (  1+sqrt(2),  1,                 1,         -1)
c    (     i,       -(1+sqrt(2))*i,    i,          i)
c    (    -i,        (1+sqrt(2))*i,    i,          i)
c
c    Note that the actual eigenvector matrix from EISPACK could
c    be scaled by a real value, or by i, and the columns may
c    appear in any order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision ai(n,n)
      double precision ar(n,n)
      double precision fv1(n)
      integer i
      integer ierr
      double precision iv1(n)
      integer j
      integer matz
      double precision wi(n)
      double precision wr(n)
      double precision xi(n,n)
      double precision xr(n,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CG_LR_TEST'
      write ( *, '(a)' ) '  CG_LR computes the eigenvalues and'
      write ( *, '(a)' ) '  eigenvectors of a complex general matrix'
      write ( *, '(a)' ) '  using elementary transformations.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n
c
c  Set the values of the matrix.
c
      ar(1,1) = 3.0D+00
      ar(1,2) = 1.0D+00
      ar(1,3) = 0.0D+00
      ar(1,4) = 0.0D+00

      ar(2,1) = 1.0D+00
      ar(2,2) = 3.0D+00
      ar(2,3) = 0.0D+00
      ar(2,4) = 0.0D+00

      ar(3,1) = 0.0D+00
      ar(3,2) = 0.0D+00
      ar(3,3) = 1.0D+00
      ar(3,4) = 1.0D+00

      ar(4,1) = 0.0D+00
      ar(4,2) = 0.0D+00
      ar(4,3) = 1.0D+00
      ar(4,4) = 1.0D+00

      ai(1,1) = 0.0D+00
      ai(1,2) = 0.0D+00
      ai(1,3) = 0.0D+00
      ai(1,4) = 2.0D+00

      ai(2,1) = 0.0D+00
      ai(2,2) = 0.0D+00
      ai(2,3) = -2.0D+00
      ai(2,4) = 0.0D+00

      ai(3,1) = 0.0D+00
      ai(3,2) = 2.0D+00
      ai(3,3) = 0.0D+00
      ai(3,4) = 0.0D+00

      ai(4,1) = -2.0D+00
      ai(4,2) = -0.0D+00
      ai(4,3) = -0.0D+00
      ai(4,4) = 0.0D+00
c
c  matz = 0 for eigenvalues only,
c  matz = 1 for eigenvalues and eigenvectors.
c
      call cg_lr ( n, n, ar, ai, wr, wi, matz, xr, xi, fv1, iv1,
     &  ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ',ierr
      end if

      call r8vec2_print ( n, wr, wi,
     &  '  Real and imaginary parts of eigenvalues:' )

      if ( matz .ne. 0 ) then
        do i = 1, n
          write ( *, '(a)' ) ''
          write ( *, '(a,i8)' ) '  Eigenvector ', i
          write ( *, '(a)' ) ''
          do j = 1, n
            write ( *, '(2g14.6)' ) xr(i,j), xi(i,j)
          end do
        end do
      end if

      return
      end
      subroutine cg_qr_test ( matz )

c*********************************************************************72
c
cc  CG_QR_TEST tests CG_QR.
c
c  Discussion:
c
c    CG_QR is for the eigenvalues of a complex general matrix.
c
c    note that the eigenvalues of such a matrix are in general complex.
c    however, we will use the same example we used before, namely
c    a hermitian matrix, so the eigenvalues will in fact be real.
c
c    (3     1     0     0+2i)
c    (1     3     0-2i  0   )
c    (0     0+2i  1     1   )
c    (0-2i  0     1     1   )
c
c    The eigenvalues are 2+2*sqrt(2), 2-2*sqrt(2), 4 and 0
c
c    The eigenvector matrix is
c
c    (  1+sqrt(2),  1,                -1,          1)
c    (  1+sqrt(2),  1,                 1,         -1)
c    (     i,       -(1+sqrt(2))*i,    i,          i)
c    (    -i,        (1+sqrt(2))*i,    i,          i)
c
c    Note that the actual eigenvector matrix from EISPACK could
c    be scaled by a real value, or by i, and the columns may
c    appear in any order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision ai(n,n)
      double precision ar(n,n)
      double precision fv1(n)
      double precision fv2(n)
      double precision fv3(n)
      integer i
      integer ierr
      integer j
      integer matz
      double precision wi(n)
      double precision wr(n)
      double precision xi(n,n)
      double precision xr(n,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CG_QR_TEST'
      write ( *, '(a)' ) '  CG_QR computes the eigenvalues and'
      write ( *, '(a)' ) '  eigenvectors of a complex general matrix'
      write ( *, '(a)' ) '  using unitary transformations.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n
c
c  Set the values of the matrix.
c
      ar(1,1) = 3.0D+00
      ar(1,2) = 1.0D+00
      ar(1,3) = 0.0D+00
      ar(1,4) = 0.0D+00

      ar(2,1) = 1.0D+00
      ar(2,2) = 3.0D+00
      ar(2,3) = 0.0D+00
      ar(2,4) = 0.0D+00

      ar(3,1) = 0.0D+00
      ar(3,2) = 0.0D+00
      ar(3,3) = 1.0D+00
      ar(3,4) = 1.0D+00

      ar(4,1) = 0.0D+00
      ar(4,2) = 0.0D+00
      ar(4,3) = 1.0D+00
      ar(4,4) = 1.0D+00

      ai(1,1) = 0.0D+00
      ai(1,2) = 0.0D+00
      ai(1,3) = 0.0D+00
      ai(1,4) = 2.0D+00

      ai(2,1) = 0.0D+00
      ai(2,2) = 0.0D+00
      ai(2,3) = -2.0D+00
      ai(2,4) = 0.0D+00

      ai(3,1) = 0.0D+00
      ai(3,2) = 2.0D+00
      ai(3,3) = 0.0D+00
      ai(3,4) = 0.0D+00

      ai(4,1) = -2.0D+00
      ai(4,2) = -0.0D+00
      ai(4,3) = -0.0D+00
      ai(4,4) = 0.0D+00
c
c  matz = 0 for eigenvalues only,
c  matz = 1 for eigenvalues and eigenvectors.
c
      call cg_qr ( n, n, ar, ai, wr, wi, matz, xr, xi, fv1, fv2, fv3,
     &  ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ',ierr
      end if

      call r8vec2_print ( n, wr, wi,
     &  '  Real and imaginary parts of eigenvalues:' )

      if ( matz .ne. 0 ) then
        do i = 1, n
          write ( *, '(a)' ) ''
          write ( *, '(a,i8)' ) '  Eigenvector ', i
          write ( *, '(a)' ) ''
          do j = 1, n
            write ( *, '(2g14.6)' ) xr(i,j), xi(i,j)
          end do
        end do
      end if

      return
      end
      subroutine ch_test ( matz )

c*********************************************************************72
c
cc  CH_TEST tests CH.
c
c  Discussion:
c
c    CH is for the eigenvalues of a complex hermitian matrix.
c
c    Eigenvalues and eigenvectors of a complex hermitian matrix
c
c    Note that the eigenvalues (though not the eigenvectors) of
c    a hermitian matrix are real.
c
c    (3     1     0     0+2i)
c    (1     3     0-2i  0   )
c    (0     0+2i  1     1   )
c    (0-2i  0     1     1   )
c
c    The eigenvalues are 2+2*sqrt(2), 2-2*sqrt(2), 4 and 0
c
c    The eigenvector matrix is
c
c    (  1+sqrt(2),  1,                -1,          1)
c    (  1+sqrt(2),  1,                 1,         -1)
c    (     i,       -(1+sqrt(2))*i,    i,          i)
c    (    -i,        (1+sqrt(2))*i,    i,          i)
c
c    Note that the actual eigenvector matrix from EISPACK could
c    be scaled by a real value, or by i, and the columns may
c    appear in any order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision ar(n,n)
      double precision ai(n,n)
      double precision fm1(2,n)
      double precision fv1(n)
      double precision fv2(n)
      integer i
      integer ierr
      integer j
      integer matz
      double precision w(n)
      double precision xr(n,n)
      double precision xi(n,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CH_TEST'
      write ( *, '(a)' ) '  CH computes the eigenvalues and'
      write ( *, '(a)' ) '  eigenvectors of a complex hermitian matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n
c
c  Set the values of the matrix.
c
      ar(1,1) = 3.0D+00
      ar(1,2) = 1.0D+00
      ar(1,3) = 0.0D+00
      ar(1,4) = 0.0D+00

      ar(2,1) = 1.0D+00
      ar(2,2) = 3.0D+00
      ar(2,3) = 0.0D+00
      ar(2,4) = 0.0D+00

      ar(3,1) = 0.0D+00
      ar(3,2) = 0.0D+00
      ar(3,3) = 1.0D+00
      ar(3,4) = 1.0D+00

      ar(4,1) = 0.0D+00
      ar(4,2) = 0.0D+00
      ar(4,3) = 1.0D+00
      ar(4,4) = 1.0D+00

      ai(1,1) = 0.0D+00
      ai(1,2) = 0.0D+00
      ai(1,3) = 0.0D+00
      ai(1,4) = 2.0D+00

      ai(2,1) = 0.0D+00
      ai(2,2) = 0.0D+00
      ai(2,3) = -2.0D+00
      ai(2,4) = 0.0D+00

      ai(3,1) = 0.0D+00
      ai(3,2) = 2.0D+00
      ai(3,3) = 0.0D+00
      ai(3,4) = 0.0D+00

      ai(4,1) = -2.0D+00
      ai(4,2) = -0.0D+00
      ai(4,3) = -0.0D+00
      ai(4,4) = 0.0D+00

      call ch ( n, n, ar, ai, w, matz, xr, xi, fv1, fv2, fm1, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      if ( matz .ne. 0 ) then
        do i = 1, n
          write ( *, '(a)' ) ''
          write ( *, '(a,i8)' ) '  Eigenvector ', i
          write ( *, '(a)' ) ''
          do j = 1, n
            write ( *, '(2g14.6)' ) xr(i,j), xi(i,j)
          end do
        end do
      end if

      return
      end
      subroutine ch3_test ( matz )

c*********************************************************************72
c
cc  CH3_TEST tests CH3.
c
c  Discussion:
c
c    CH3 is for the eigenvalues of a complex hermitian matrix.
c
c    Eigenvalues and eigenvectors of a complex hermitian matrix
c
c    Note that the eigenvalues (though not the eigenvectors) of
c    a hermitian matrix are real.
c
c    (3     1     0     0+2i)
c    (1     3     0-2i  0   )
c    (0     0+2i  1     1   )
c    (0-2i  0     1     1   )
c
c    The eigenvalues are 2+2*sqrt(2), 2-2*sqrt(2), 4 and 0
c
c    The eigenvector matrix is
c
c    (  1+sqrt(2),  1,                -1,          1)
c    (  1+sqrt(2),  1,                 1,         -1)
c    (     i,       -(1+sqrt(2))*i,    i,          i)
c    (    -i,        (1+sqrt(2))*i,    i,          i)
c
c    Note that the actual eigenvector matrix from EISPACK could
c    be scaled by a real value, or by i, and the columns may
c    appear in any order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision fm1(2,n)
      double precision fv1(n)
      double precision fv2(n)
      integer i
      integer ierr
      integer j
      integer matz
      double precision w(n)
      double precision xr(n,n)
      double precision xi(n,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CH3_TEST'
      write ( *, '(a)' ) '  CH3 computes the eigenvalues and'
      write ( *, '(a)' ) '  eigenvectors of a complex hermitian matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n
c
c  Real parts:
c
      a(1,1) = 3.0D+00
      a(2,1) = 1.0D+00
      a(3,1) = 0.0D+00
      a(4,1) = 0.0D+00

      a(2,2) = 3.0D+00
      a(3,2) = 0.0D+00
      a(4,2) = 0.0D+00

      a(3,3) = 1.0D+00
      a(4,3) = 1.0D+00

      a(4,4) = 1.0D+00
c
c  Imaginary parts:
c
      a(1,2) = 0.0D+00

      a(1,3) = 0.0D+00
      a(2,3) = -2.0D+00

      a(1,4) = 2.0D+00
      a(2,4) = 0.0D+00
      a(3,4) = 0.0D+00

      call ch3 ( n, n, a, w, matz, xr, xi, fv1, fv2, fm1, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      if ( matz .ne. 0 ) then
        do i = 1, n
          write ( *, '(a)' ) ''
          write ( *, '(a,i8)' ) '  Eigenvector ', i
          write ( *, '(a)' ) ''
          do j = 1, n
            write ( *, '(2g14.6)' ) xr(i,j), xi(i,j)
          end do
        end do
      end if

      return
      end
      subroutine cinvit_test ( )

c*********************************************************************72
c
cc CINVIT_TEST tests CINVIT.
c
c  Discussion:
c
c    We seek the eigenvectors of HESS4, a complex uppper 
c    Hessenberg matrix.
c
c    Matrix A:
c
c      4+8i  7+6i  7+10i 7+10i
c      9+9i  8+1i  8+10i 2 +5i
c      0     8+3i  7+ 2i 7 +8i
c      0     0     4+10i 0 +1i
c
c    Eigenvalues:
c
c       3.324431041502838 - 2.742026572531628i
c       0.568541187348097 + 6.826204344246118i
c      -5.153228803481162 - 8.729936381660266i
c      20.260256574630240 +16.645758609945791i
c
c    Eigenvectors:
c
c     -0.3301 - 0.2223i   1.0000 + 0.0000i   0.3355 - 0.0680i   0.9522 + 0.2507i
c      1.0000 + 0.0000i   0.5032 - 0.8242i  -0.7682 + 0.0105i   1.0000 + 0.0000i
c      0.2574 + 0.3091i  -0.2150 + 0.2755i   1.0000 + 0.0000i   0.5015 - 0.1722i
c     -0.8426 + 0.1978i  -0.2382 + 0.5972i  -0.9727 - 0.1040i   0.2186 + 0.0448i
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision ai(n,n)
      double precision ai_save(n,n)
      double precision ar(n,n)
      double precision ar_save(n,n)
      double precision fm1(n,n)
      double precision fm2(n,n)
      double precision fv1(n)
      double precision fv2(n)
      double precision fv3(n)
      integer i
      integer ierr
      integer is1
      integer is2
      integer j
      integer m
      integer mm
      integer nm
      double precision s
      logical select(n)
      double precision wi(n)
      double precision wr(n)
      double precision zi(n,n)
      double precision zr(n,n)

      save ai_save
      save ar_save

      data ai_save /
     &   8.0D+00,  9.0D+00,  0.0D+00,  0.0D+00,
     &   6.0D+00,  1.0D+00,  3.0D+00,  0.0D+00,
     &  10.0D+00, 10.0D+00,  2.0D+00, 10.0D+00,
     &  10.0D+00,  5.0D+00,  8.0D+00,  1.0D+00 /

      data ar_save /
     &   4.0D+00,  9.0D+00,  0.0D+00,  0.0D+00,
     &   7.0D+00,  8.0D+00,  8.0D+00,  0.0D+00,
     &   7.0D+00,  8.0D+00,  7.0D+00,  4.0D+00,
     &   7.0D+00,  2.0D+00,  7.0D+00,  0.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CINVIT_TEST'
      write ( *, '(a)' ) '  CINVIT computes the eigenvectors of '
      write ( *, '(a)' ) '  a complex Hessenberg matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      nm = n
      call r8mat_copy ( n, n, ar_save, ar )
      call r8mat_copy ( n, n, ai_save, ai )
c
      call cbal ( nm, n, ar, ai, is1, is2, fv1 )

      call corth ( nm, n, is1, is2, ar, ai, fv2, fv3 )

      do i = 1, n
        do j = 1, n
          fm1(i,j) = ar(i,j)
          fm2(i,j) = ai(i,j)
        end do
      end do

      call comqr ( nm, n, is1, is2, fm1, fm2, wr, wi, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'CINVIT_TEST - Warning!'
        write ( *, '(a)' ) '  COMQR returned flag IERR = ', ierr
        return
      end if

      call r8vec2_print ( n, wr, wi, 
     &  '  Real and imaginary parts of eigenvalues:' )
c
c  We need to restore the values of AR and AI before calling CINVIT.
c
      nm = n
      call r8mat_copy ( n, n, ar_save, ar )
      call r8mat_copy ( n, n, ai_save, ai )
      do i = 1, n
        select(i) = .true.
      end do
      mm = n

      call r8mat_print ( n, n, ar, '  Matrix Real Part Ar:' )
      call r8mat_print ( n, n, ai, '  Matrix Imag Part Ai:' )
      call r8vec2_print ( n, wr, wi, 
     &  '  Real and imaginary parts of eigenvalues:' )

      call cinvit ( nm, n, ar, ai, wr, wi, select, mm, m, zr, zi, 
     &  ierr, fm1, fm2, fv1, fv2 )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'CINVIT_TEST - Warning!'
        write ( *, '(a,i8)' ) '  CINVIT returned flag IERR = ', ierr
        return
      end if
c
c  Reverse transformations on eigenvectors.
c
      call cortb ( nm, is1, is2, ar, ai, fv2, fv3, m, zr, zi )
      call cbabk2 ( nm, n, is1, is2, fv1, m, zr, zi )
c
c  Normalize and print eigenvectors.
c
      do i = 1, n
        write ( *, '(a)' ) ''
        write ( *, '(a,i8)' ) '  Eigenvector ', i
        write ( *, '(a)' ) ''

        s = 0.0D+00
        do j = 1, mm
          s = max ( s, abs ( zr(i,j) ) );
          s = max ( s, abs ( zi(i,j) ) );
        end do

        do j = 1, mm
          zr(i,j) = zr(i,j) / s
          zi(i,j) = zi(i,j) / s
        end do

        do j = 1, mm
          write ( *, '(2g14.6)' ) zr(i,j), zi(i,j) 
        end do
      end do

      return
      end
      subroutine imtqlv_test ( )

c*********************************************************************72
c
cc IMTQLV_TEST tests IMTQLV.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 February 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision d(n)
      double precision e(n)
      double precision e2(n)
      integer i
      integer ierr
      integer ind(n)
      integer j
      double precision rv1(n)
      double precision w(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'IMTQLV_TEST'
      write ( *, '(a)' ) '  IMTQLV computes the eigenvalues of a real'
      write ( *, '(a)' ) '  symmetric tridiagonal matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n
c
c  Here is where the matrix is defined.
c
      do i = 1, n
        d(i) = 2.0D+00
      end do

      e(1) = 0.0D+00
      do i = 2, n
        e(i) = -1.0D+00
      end do

      do i = 1, n
        e2(i) = e(i) ** 2
      end do
c
c  We only set up and store the matrix A this way in order to make it
c  easy to compute the residual.
c
      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a(i,j) = - 1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call imtqlv ( n, d, e, e2, w, ind, ierr, rv1 )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'IMTQLV_TEST - Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
        return
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      return
      end
      subroutine invit_test ( )

c*********************************************************************72
c
cc INVIT_TEST tests INVIT.
c
c  Discussion:
c
c    We seek the eigenvectors of HESS5, an uppper Hessenberg matrix.
c
c    Matrix A:
c
c     9     4     1     3     2
c     4     3     1     7     1
c     0     3     1     2     4
c     0     0     5     5     1
c     0     0     0     1     2
c
c    Eigenvalues:
c
c      1.795071645585215 + 0.000000000000000i
c     -0.484650565840867 + 3.050399870879445i
c     -0.484650565840867 - 3.050399870879445i
c      7.232089690415871 + 0.000000000000000i
c     11.942139795680633 + 0.000000000000000i
c
c    Eigenvectors:
c
c -0.4048+0.0000i -0.2788-0.1981i -0.2788+0.1981i  1.0000+0.0000i 1.0000+0.0000i
c  1.0000+0.0000i  1.0000+0.0000i  1.0000+0.0000i  0.0372+0.0000i 0.5780+0.0000i
c  0.0565+0.0000i -0.0712-0.9695i -0.0712+0.9695i -0.2064+0.0000i 0.1887+0.0000i
c  0.1687+0.0000i -0.3560+0.6933i -0.3560-0.6933i -0.5057+0.0000i 0.1379+0.0000i
c -0.8231+0.0000i  0.1938-0.0411i  0.1938+0.0411i -0.0966+0.0000i 0.0139+0.0000i
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision a_save(n,n)
      double precision fv1(n)
      integer i
      integer ierr
      integer is1
      integer is2
      integer j
      integer k
      integer m
      integer mm
      integer nm
      double precision ort(n)
      double precision rm1(n,n)
      double precision rv1(n)
      double precision rv2(n)
      double precision s
      logical select(n)
      double precision sum1
      double precision sum2
      double precision wi(n)
      double precision wr(n)
      double precision x(n,n)
      double precision xi(n,n)
      double precision xr(n,n)

      save a_save

      data a_save /
     & 9.0D+00, 4.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 
     & 4.0D+00, 3.0D+00, 3.0D+00, 0.0D+00, 0.0D+00, 
     & 1.0D+00, 1.0D+00, 1.0D+00, 5.0D+00, 0.0D+00, 
     & 3.0D+00, 7.0D+00, 2.0D+00, 5.0D+00, 1.0D+00, 
     & 2.0D+00, 1.0D+00, 4.0D+00, 1.0D+00, 2.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'INVIT_TEST'
      write ( *, '(a)' ) '  INVIT computes the eigenvectors of '
      write ( *, '(a)' ) '  a real Hessenberg matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      nm = n
      call r8mat_copy ( n, n, a_save, a )

      call balanc ( nm, n, a, is1, is2, fv1 )

      call orthes ( nm, n, is1, is2, a, ort )

      call hqr ( nm, n, is1, is2, a, wr, wi, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'INVIT_TEST - Warning!'
        write ( *, '(a)' ) '  HQR returned flag IERR = ', ierr
        return
      end if
c
c  Prepare to get the eigenvectors.
c
      call r8mat_copy ( n, n, a_save, a )

      call r8mat_print ( n, n, a, '  Matrix A:' )

      call r8vec2_print ( n, wr, wi, 
     &  '  Real and imaginary parts of eigenvalues:' )

      do i = 1, n
        select(i) = .true.
      end do

      mm = n

      call invit ( nm, n, a, wr, wi, select, mm, m, x, ierr, 
     &  rm1, rv1, rv2 ) 

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'INVIT_TEST - Warning!'
        write ( *, '(a,i8)' ) '  INVIT returned flag IERR = ', ierr
        return
      end if
c
c  Reverse transformations on eigenvectors.
c
c  NM, N FOR ORTBAK???
c
      call ortbak ( nm, is1, is2, a, ort, n, x )
      call balbak ( nm, n, is1, is2, fv1, n, x )
c
c  Expand the compressed eigenvector information.
c
      do j = 1, n

        do i = 1, n
          if ( wi(j) .eq. 0.0D+00 ) then
            xr(i,j) = x(i,j)
            xi(i,j) = 0.0D+00
          else if ( 0.0D+00 .lt. wi(j) ) then
            xr(i,j) = x(i,j)
            xi(i,j) = x(i,j+1)
          else if ( wi(j) .lt. 0.0D+00 ) then
            xr(i,j) = x(i,j-1)
            xi(i,j) = -x(i,j)
          end if
        end do
c
c  Normalize the eigenvectors.
c
        s = 0.0D+00
        do i = 1, n
          s = max ( s, abs ( xr(i,j) ) )
          s = max ( s, abs ( xi(i,j) ) )
        end do

        do i = 1, n
          xr(i,j) = xr(i,j) / s
          xi(i,j) = xi(i,j) / s
        end do

        call r8vec2_print ( n, xr(1:n,j), xi(1:n,j), '  Eigenvector:'  )

      end do
c
c  Check.
c  First, restore the original values of A.
c
      call r8mat_copy ( n, n, a_save, a )

      do k = 1, n

        write ( *, '(a)' ) ''
        write ( *, '(a,i8)' ) 
     &    '  Residuals (A*x-Lambda*x) for eigenvalue ', k
        write ( *, '(a)' ) ''

        do i = 1, n
          sum1 = 0.0D+00
          sum2 = 0.0D+00
          do j = 1, n
            sum1 = sum1 + a(i,j) * xr(j,k)
            sum2 = sum2 + a(i,j) * xi(j,k)
          end do
          sum1 = sum1 - wr(k) * xr(i,k) + wi(k) * xi(i,k)
          sum2 = sum2 - wi(k) * xr(i,k) - wr(k) * xi(i,k)
          write ( *, '(2g14.6)' ) sum1, sum2
        end do

      end do

      return
      end
      subroutine minfit_test ( )

c*********************************************************************72
c
cc MINFIT_TEST tests MINFIT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer nb
      parameter ( nb = 1 )
      integer n
      parameter ( n = 2 )

      double precision a(m,n)
      double precision acopy(m,n)
      double precision b(m,nb)
      integer i
      integer ierr
      integer j
      double precision r(m)
      double precision rv1(n)
      double precision w(n)
      double precision x(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'MINFIT_TEST'
      write ( *, '(a)' )
     &  '  MINFIT solves an overdetermined linear system'
      write ( *, '(a)' ) '  using least squares methods.'
      write ( *, '(a,i8)' ) '  Matrix rows = ', m
      write ( *, '(a,i8)' ) '  Matrix columns = ', n

      a(1,1) =   1.00D+00
      a(2,1) =   2.05D+00
      a(3,1) =   3.06D+00
      a(4,1) = - 1.02D+00
      a(5,1) =   4.08D+00

      a(1,2) =   1.00D+00
      a(2,2) = - 1.00D+00
      a(3,2) =   1.00D+00
      a(4,2) =   2.00D+00
      a(5,2) = - 1.00D+00

      do j = 1, n
        do i = 1, m
          acopy(i,j) = a(i,j)
        end do
      end do

      b(1,1) = 1.98D+00
      b(2,1) = 0.95D+00
      b(3,1) = 3.98D+00
      b(4,1) = 0.92D+00
      b(5,1) = 2.90D+00

      do i = 1, m
        r(i) = - b(i,1)
      end do

      call r8mat_print ( m, n, a, '  The matrix A:' )

      call r8mat_print ( m, nb, b, '  The right hand side B:' )

      call minfit ( m, m, n, a, w, nb, b, ierr, rv1 )

      write ( *, '(a)' ) ''
      write ( *, '(a,i8)' ) '  MINFIT error code IERR = ', ierr

      call r8vec_print ( n, w, '  The singular values:' )
c
c  B now contains U' * B.
c  We need to divide by the singular values, and multiply by V.
c
      do i = 1, n
        b(i,1) = b(i,1) / w(i)
      end do

      do i = 1, n
        x(i) = 0.0D+00
        do j = 1, n
          x(i) = x(i) + a(i,j) * b(j,1)
        end do
      end do

      call r8vec_print ( n, x, '  The least squares solution X:' )

      do i = 1, m
        do j = 1, n
          r(i) = r(i) + acopy(i,j) * x(j)
        end do
      end do

      call r8vec_print ( m, r, '  The residual A * X - B:' )

      return
      end
      subroutine rg_elm_test ( matz )

c*********************************************************************72
c
cc RG_ELM_TEST tests RG_ELM.
c
c  Discussion:
c
c    RG_ELM is for the eigenvalues of a general real matrix.
c
c    The matrix A is nonsymmetric.  The eigenvalues may therefore be
c    complex numbers.
c
c    ( 33  16  72)
c    (-24 -10 -57)
c    ( -8  -4 -17)
c
c    The eigenvalues of A are (1,2,3)
c
c    The eigenvectors of A are
c
c    (-1.0000 -1,0000  1.0000 )
c    ( 0.8000  0.8125 -0.7500 )
c    ( 0.2667  0.2500 -0.2500 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 March 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 3 )

      double precision a(n,n)
      double precision a_save(n,n)
      double precision fv1(n)
      integer i
      integer ierr
      integer iv1(n)
      integer j
      integer k
      integer matz
      double precision s
      double precision sum1
      double precision sum2
      double precision wi(n)
      double precision wr(n)
      double precision x(n,n)
      double precision xi(n,n)
      double precision xr(n,n)

      data a_save /
     & 33.0D+00, -24.0D+00,  -8.0D+00, 
     & 16.0D+00, -10.0D+00,  -4.0D+00, 
     & 72.0D+00, -57.0D+00, -17.0D+00 /
c
c  RG overwrites A with garbage, so save a copy nowc
c
      do j = 1, n
        do i = 1, n
          a(i,j) = a_save(i,j)
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RG_ELM_TEST'
      write ( *, '(a)' )
     &  '  RG_ELM computes the eigenvalues and eigenvectors of'
      write ( *, '(a)' ) '  a real general matrix,'
      write ( *, '(a)' ) '  using elementary transforms.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call rg_elm ( n, n, a, wr, wi, matz, x, iv1, fv1, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'RG_TEST - Warning!'
        write ( *, '(a,i8)' )
     &  '  The error return flag was IERR = ', ierr
      end if

      call r8vec2_print ( n, wr, wi,
     &  '  Real and imaginary parts of eigenvalues:' )

      if ( matz .ne. 0 ) then
c
c  Expand the compressed eigenvector information.
c
        do j = 1, n

          do i = 1, n
            if ( wi(j) .eq. 0.0D+00 ) then
              xr(i,j) = x(i,j)
              xi(i,j) = 0.0D+00
            else if ( 0.0D+00 .le. wi(j) ) then
              xr(i,j) = x(i,j)
              xi(i,j) = x(i,j+1)
            else if ( wi(j) .lt. 0.0D+00 ) then
              xr(i,j) = x(i,j-1)
              xi(i,j) = -x(i,j)
            end if
          end do
c
c  Normalize the eigenvectors.
c
          s = 0.0D+00
          do i = 1, n
            s = max ( s, abs ( xr(i,j) ) )
            s = max ( s, abs ( xi(i,j) ) )
          end do

          do i = 1, n
            xr(i,j) = xr(i,j) / s
            xi(i,j) = xi(i,j) / s
          end do

          call r8vec2_print ( n, xr(1,j), xi(1,j), 
     &      '  Eigenvector:'  )

        end do
c
c  Check.
c  First, restore the original values of A.
c
        do j = 1, n
          do i = 1, n
            a(i,j) = a_save(i,j)
          end do
        end do

        do k = 1, n

          write ( *, '(a)' ) ''
          write ( *, '(a,i8)' ) 
     &      '  Residuals (A*x-Lambda*x) for eigenvalue ', k
          write ( *, '(a)' ) ''

          do i = 1, n
            sum1 = 0.0D+00
            sum2 = 0.0D+00
            do j = 1, n
              sum1 = sum1 + a(i,j) * xr(j,k)
              sum2 = sum2 + a(i,j) * xi(j,k)
            end do
            sum1 = sum1 - wr(k) * xr(i,k) + wi(k) * xi(i,k)
            sum2 = sum2 - wi(k) * xr(i,k) - wr(k) * xi(i,k)
            write ( *, '(2g14.6)' ) sum1, sum2
          end do

        end do

      end if

      return
      end
      subroutine rg_ort_test ( matz )

c*********************************************************************72
c
cc RG_ORT_TEST tests RG_ORT.
c
c  Discussion:
c
c    RG_ORT is for the eigenvalues of a general real matrix.
c
c    The matrix A is nonsymmetric.  The eigenvalues may therefore be
c    complex numbers.
c
c    ( 33  16  72)
c    (-24 -10 -57)
c    ( -8  -4 -17)
c
c    The eigenvalues of A are (1,2,3)
c
c    The eigenvectors of A are
c
c    (-1.0000 -1,0000  1.0000 )
c    ( 0.8000  0.8125 -0.7500 )
c    ( 0.2667  0.2500 -0.2500 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 March 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 3 )

      double precision a(n,n)
      double precision a_save(n,n)
      integer i
      integer ierr
      double precision fv1(n)
      integer j
      integer k
      integer matz
      double precision ort(n)
      double precision s
      double precision sum1
      double precision sum2
      double precision wi(n)
      double precision wr(n)
      double precision x(n,n)
      double precision xi(n,n)
      double precision xr(n,n)

      data a_save /
     & 33.0D+00, -24.0D+00,  -8.0D+00, 
     & 16.0D+00, -10.0D+00,  -4.0D+00, 
     & 72.0D+00, -57.0D+00, -17.0D+00 /
c
c  RG overwrites A with garbage, so save a copy nowc
c
      do j = 1, n
        do i = 1, n
          a(i,j) = a_save(i,j)
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RG_ORT_TEST'
      write ( *, '(a)' )
     &  '  RG_ORT computes the eigenvalues and eigenvectors of'
      write ( *, '(a)' ) '  a real general matrix,'
      write ( *, '(a)' ) '  using orthogonal transforms.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call rg_ort ( n, n, a, wr, wi, matz, x, ort, fv1, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'RG_ORT_TEST - Warning!'
        write ( *, '(a,i8)' )
     &  '  The error return flag was IERR = ', ierr
      end if

      call r8vec2_print ( n, wr, wi,
     &  '  Real and imaginary parts of eigenvalues:' )

      if ( matz .ne. 0 ) then
c
c  Expand the compressed eigenvector information.
c
        do j = 1, n

          do i = 1, n
            if ( wi(j) .eq. 0.0D+00 ) then
              xr(i,j) = x(i,j)
              xi(i,j) = 0.0D+00
            else if ( 0.0D+00 .le. wi(j) ) then
              xr(i,j) = x(i,j)
              xi(i,j) = x(i,j+1)
            else if ( wi(j) .lt. 0.0D+00 ) then
              xr(i,j) = x(i,j-1)
              xi(i,j) = -x(i,j)
            end if
          end do
c
c  Normalize the eigenvectors.
c
          s = 0.0D+00
          do i = 1, n
            s = max ( s, abs ( xr(i,j) ) )
            s = max ( s, abs ( xi(i,j) ) )
          end do

          do i = 1, n
            xr(i,j) = xr(i,j) / s
            xi(i,j) = xi(i,j) / s
          end do

          call r8vec2_print ( n, xr(1,j), xi(1,j), 
     &      '  Eigenvector:'  )

        end do
c
c  Check.
c  First, restore the original values of A.
c
        do j = 1, n
          do i = 1, n
            a(i,j) = a_save(i,j)
          end do
        end do

        do k = 1, n

          write ( *, '(a)' ) ''
          write ( *, '(a,i8)' ) 
     &      '  Residuals (A*x-Lambda*x) for eigenvalue ', k
          write ( *, '(a)' ) ''

          do i = 1, n
            sum1 = 0.0D+00
            sum2 = 0.0D+00
            do j = 1, n
              sum1 = sum1 + a(i,j) * xr(j,k)
              sum2 = sum2 + a(i,j) * xi(j,k)
            end do
            sum1 = sum1 - wr(k) * xr(i,k) + wi(k) * xi(i,k)
            sum2 = sum2 - wi(k) * xr(i,k) - wr(k) * xi(i,k)
            write ( *, '(2g14.6)' ) sum1, sum2
          end do

        end do

      end if

      return
      end
      subroutine rgg_test ( matz )

c*********************************************************************72
c
cc RGG_TEST tests RGG.
c
c  Discussion:
c
c    RGG is for a real generalized general eigenvalue problem.
c
c    A generalized eigenvalue problem.  Given matrices A and B, find
c    N numbers LAMBDA, and for each LAMBDA a vector X, so that
c
c      A*x = lambda*B*x
c
c    The matrix A is
c
c    ( -7 7  6  6)
c    (-10 8 10  8)
c    ( -8 3 10 11)
c    ( -4 0  4 12)
c
c    The matrix B is
c
c    (2 1 0 0)
c    (1 2 1 0)
c    (0 1 2 1)
c    (0 0 1 2)
c
c    The correct eigenvalues LAMBDA are
c
c    (1,2,3,4)
c
c    The correct eigenvectors X are
c
c    (4 3 2 1)
c    (3 3 2 1)
c    (2 2 2 1)
c    (1 1 1 1)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision acopy(n,n)
      double precision alfi(n)
      double precision alfr(n)
      double precision b(n,n)
      double precision bcopy(n,n)
      double precision beta(n)
      integer i
      integer ierr
      integer j
      integer k
      integer matz
      double precision sum3
      double precision sum1
      double precision sum2
      double precision x(n,n)
c
c  Set the values in the A matrix.
c
      a(1,1) = -7.0D+00
      a(1,2) = 7.0D+00
      a(1,3) = 6.0D+00
      a(1,4) = 6.0D+00

      a(2,1) = -10.0D+00
      a(2,2) = 8.0D+00
      a(2,3) = 10.0D+00
      a(2,4) = 8.0D+00

      a(3,1) = -8.0D+00
      a(3,2) = 3.0D+00
      a(3,3) = 10.0D+00
      a(3,4) = 11.0D+00

      a(4,1) = -4.0D+00
      a(4,2) = 0.0D+00
      a(4,3) = 4.0D+00
      a(4,4) = 12.0D+00
c
c  Save a copy of A.
c
      do j = 1, n
        do i = 1, n
          acopy(i,j) = a(i,j)
        end do
      end do
c
c  Set the values in the B matrix.
c
      b(1,1) = 2.0D+00
      b(1,2) = 1.0D+00
      b(1,3) = 0.0D+00
      b(1,4) = 0.0D+00

      b(2,1) = 1.0D+00
      b(2,2) = 2.0D+00
      b(2,3) = 1.0D+00
      b(2,4) = 0.0D+00

      b(3,1) = 0.0D+00
      b(3,2) = 1.0D+00
      b(3,3) = 2.0D+00
      b(3,4) = 1.0D+00

      b(4,1) = 0.0D+00
      b(4,2) = 0.0D+00
      b(4,3) = 1.0D+00
      b(4,4) = 2.0D+00
c
c  Save a copy of B.
c
      do j = 1, n
        do i = 1, n
          bcopy(i,j) = b(i,j)
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RGG_TEST:'
      write ( *, '(a)' ) '  RGG for real generalized problem.'
      write ( *, '(a)' ) '  Find scalars LAMBDA and vectors x so that'
      write ( *, '(a)' ) '    A*x = LAMBDA * B * x'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call r8mat_print ( n, n, b, '  The matrix B:' )

      call rgg ( n, n, a, b, alfr, alfi, beta, matz, x, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
      end if

      do i = 1, n
        alfr(i) = alfr(i) / beta(i)
        alfi(i) = alfi(i) / beta(i)
      end do

      call r8vec2_print ( n, alfr, alfi,
     &  '  Real and imaginary parts of eigenvalues:' )

      if ( matz .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '  The eigenvectors are:'
        do i = 1, n
          write ( *, '(a)' ) ''
          write ( *, '(a,i8)' ) '  Eigenvector ', i
          write ( *, '(a)' ) ''
          do j = 1, n
            write ( *, '(g14.6)' ) x(i,j)
          end do
        end do
      end if
c
c  Check.
c  First, restore the original values of A and B.
c
      if ( matz .ne. 0 ) then

        do j = 1, n
          do i = 1, n
            a(i,j) = acopy(i,j)
          end do
        end do

        do j = 1, n
          do i = 1, n
            b(i,j) = bcopy(i,j)
          end do
        end do

        do k = 1, n
          write ( *, '(a)' ) ''
          write ( *, '(a,i8)' )
     &      '  Residuals (A*x-(Alfr+Alfi*I)*B*x) for eigenvalue ', k
          write ( *, '(a)' ) ''

          if ( alfi(k) .eq. 0.0D+00 ) then

            do i = 1, n

              sum3 = 0.0D+00
              do j = 1, n
                sum3 = sum3 + a(i,j) * x(j,k)
              end do

              do j = 1, n
                sum3 = sum3 - alfr(k) * b(i,j) * x(j,k)
              end do

              write ( *, '(g14.6)' ) sum3
            end do

          else if ( 0.0D+00 .lt. alfi(k) ) then

            do i = 1, n

              sum1 = 0.0D+00
              sum2 = 0.0D+00
              do j = 1, n
                sum1 = sum1 + a(i,j) * x(j,k)
                sum2 = sum2 + a(i,j) * x(j,k+1)
              end do

              do j = 1, n
                sum1 = sum1 - alfr(k) * b(i,j) * x(j,k)
     &                      + alfi(k) * b(i,j) * x(j,k+1)

                sum2 = sum2 - alfi(k) * b(i,j) * x(j,k)
     &                      - alfr(k) * b(i,j) * x(j,k+1)

              end do

              write ( *, '(2g14.6)' ) sum1, sum2
            end do

          else if ( alfi(k) .lt. 0.0D+00 ) then

            do i = 1, n

              sum1 = 0.0D+00
              sum2 = 0.0D+00
              do j = 1, n
                sum1 = sum1 + a(i,j) * x(j,k-1)
                sum2 = sum2 - a(i,j) * x(j,k)
              end do

              do j = 1, n

                sum1 = sum1 - alfr(k) * b(i,j) * x(j,k-1)
     &                      - alfi(k) * b(i,j) * x(j,k)

                sum2 = sum2 - alfi(k) * b(i,j) * x(j,k-1)
     &                      + alfr(k) * b(i,j) * x(j,k)
              end do

              write ( *, '(2g14.6)' ) sum1, sum2
            end do

          end if

        end do

      end if

      return
      end
      subroutine rs_test ( matz )

c*********************************************************************72
c
cc RS_TEST tests RS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision a2(n,n)
      double precision fv1(n)
      double precision fv2(n)
      integer i
      integer ierr
      integer j
      integer k
      integer matz
      double precision r(n,n)
      double precision w(n)
      double precision x(n,n)
c
c  Set the values in the matrix.
c
      a(1,1) = 5.0D+00
      a(1,2) = 4.0D+00
      a(1,3) = 1.0D+00
      a(1,4) = 1.0D+00

      a(2,1) = 4.0D+00
      a(2,2) = 5.0D+00
      a(2,3) = 1.0D+00
      a(2,4) = 1.0D+00

      a(3,1) = 1.0D+00
      a(3,2) = 1.0D+00
      a(3,3) = 4.0D+00
      a(3,4) = 2.0D+00

      a(4,1) = 1.0D+00
      a(4,2) = 1.0D+00
      a(4,3) = 2.0D+00
      a(4,4) = 4.0D+00
c
c  Save a copy of the matrix.
c
      do j = 1, n
        do i = 1, n
          a2(i,j) = a(i,j)
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RS_TEST'
      write ( *, '(a)' )
     &  '  RS computes the eigenvalues and eigenvectors'
      write ( *, '(a)' ) '  of a real symmetric matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call rs ( n, n, a, w, matz, x, fv1, fv2, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      if ( matz .ne. 0 ) then

        call r8mat_print ( n, n, x, '  The eigenvector matrix:' )

        do i = 1, n
          do j = 1, n
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + a2(i,k) * x(k,j)
            end do
          end do
        end do

        do j = 1, n
          do i = 1, n
            r(i,j) = r(i,j) - w(j) * x(i,j)
          end do
        end do

        call r8mat_print ( n, n, r, '  The residual (A-Lambda*I)*X:' )

      end if

      return
      end
      subroutine rsb_test ( matz )

c*********************************************************************72
c
cc RSB_TEST tests RSB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer mb
      parameter ( mb = 2 )

      double precision a(n,mb)
      double precision a2(n,n)
      double precision fv1(n)
      double precision fv2(n)
      integer i
      integer ierr
      integer j
      integer k
      integer matz
      integer nm
      double precision r(n,n)
      double precision w(n)
      double precision x(n,n)

      do i = 1, n
        do j = 1, mb
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n
        a(i,mb) = 2.0D+00
      end do

      do i = 2, n
        a(i,1) = -1.0D+00
      end do

      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a2(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a2(i,j) = - 1.0D+00
          else
            a2(i,j) = 0.0D+00
          end if
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RSB_TEST'
      write ( *, '(a)' )
     &  '  RSB computes the eigenvalues and eigenvectors'
      write ( *, '(a)' ) '  of a real symmetric band matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a2, '  The matrix A:' )

      nm = n

      call rsb ( nm, n, mb, a, w, matz, x, fv1, fv2, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      if ( matz .ne. 0 ) then

        call r8mat_print ( n, n, x, '  The eigenvector matrix X:' )

        do i = 1, n
          do j = 1, n
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + a2(i,k) * x(k,j)
            end do
          end do
        end do

        do j = 1, n
          do i = 1, n
            r(i,j) = r(i,j) - w(j) * x(i,j)
          end do
        end do

        call r8mat_print ( n, n, r, '  The residual (A-Lambda*I)*X:' )

      end if

      return
      end
      subroutine rsg_test ( matz )

c*********************************************************************72
c
cc RSG_TEST tests RSG.
c
c  Discussion:
c
c    RGG is for a real generalized eigenvalue problem of the form
c
c      A * x = lambda * B * x
c
c    with A symmetric and B positive definite symmetric.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision a2(n,n)
      double precision b(n,n)
      double precision b2(n,n)
      double precision fv1(n)
      double precision fv2(n)
      integer i
      integer ierr
      integer j
      integer k
      integer matz
      double precision sum3
      double precision w(n)
      double precision x(n,n)

      do i = 1, n
        do j = 1, n
          a(i,j) = abs ( i - j )
        end do
      end do

      do i = 1, n
        do j = 1, n
          a2(i,j) = a(i,j)
        end do
      end do

      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            b(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            b(i,j) = - 1.0D+00
          else
            b(i,j) = 0.0D+00
          end if
        end do
      end do

      do j = 1, n
        do i = 1, n
          b2(i,j) = b(i,j)
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RSG_TEST:'
      write ( *, '(a)' ) '  RSG for real symmetric generalized problem.'
      write ( *, '(a)' ) '  Find scalars LAMBDA and vectors X so that'
      write ( *, '(a)' ) '    A*X = LAMBDA * B * X'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call r8mat_print ( n, n, b, '  The matrix B:' )

      call rsg ( n, n, a, b, w, matz, x, fv1, fv2, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ',ierr
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      if ( matz .ne. 0 ) then

        call r8mat_print ( n, n, x, '  The eigenvector matrix X:' )

        do j = 1, n
          do i = 1, n
            a(i,j) = a2(i,j)
          end do
        end do

        do j = 1, n
          do i = 1, n
            b(i,j) = b2(i,j)
          end do
        end do

        do k = 1, n

          write ( *, '(a)' ) ''
          write ( *, '(a,i8)' )
     &      'Residuals (A*x-(w*I)*B*x) for eigenvalue ', k
          write ( *, '(a)' ) ''

            do i = 1, n

              sum3 = 0.0D+00
              do j = 1, n
                sum3 = sum3 + a(i,j) * x(j,k)
              end do

              do j = 1, n
                sum3 = sum3 - w(k) * b(i,j) * x(j,k)
              end do

              write ( *, '(g14.6)' ) sum3
            end do

        end do

      end if

      return
      end
      subroutine rsgab_test ( matz )

c*********************************************************************72
c
cc RSGAB_TEST tests RSGAB.
c
c  Discussion:
c
c    RGGAB is for a real generalized eigenvalue problem of the form
c
c      A * B * x = lambda * x
c
c    with A symmetric and B positive definite symmetric.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision a2(n,n)
      double precision b(n,n)
      double precision b2(n,n)
      double precision fv1(n)
      double precision fv2(n)
      integer i
      integer ierr
      integer j
      integer k
      integer matz
      double precision r(n,n)
      double precision w(n)
      double precision x(n,n)

      do i = 1, n
        do j = 1, n
          a(i,j) = abs ( i - j )
        end do
      end do

      do j = 1, n
        do i = 1, n
          a2(i,j) = a(i,j)
        end do
      end do

      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            b(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            b(i,j) = - 1.0D+00
          else
            b(i,j) = 0.0D+00
          end if
        end do
      end do

      do j = 1, n
        do i = 1, n
          b2(i,j) = b(i,j)
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RSGAB_TEST:'
      write ( *, '(a)' )
     &  '  RSGAB for real symmetric generalized problem.'
      write ( *, '(a)' ) '  Find scalars LAMBDA and vectors X so that'
      write ( *, '(a)' ) '    A*B*X = LAMBDA * X'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call r8mat_print ( n, n, b, '  The matrix B:' )

      call rsgab ( n, n, a, b, w, matz, x, fv1, fv2, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      if ( matz .ne. 0 ) then

        call r8mat_print ( n, n, x, '  The eigenvector matrix X:' )

        do i = 1, n
          do j = 1, n
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + b2(i,k) * x(k,j)
            end do
          end do
        end do

        do i = 1, n
          do j = 1, n
            b2(i,j) = r(i,j)
          end do
        end do

        do i = 1, n
          do j = 1, n
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + a2(i,k) * b2(k,j)
            end do
          end do
        end do

        do j = 1, n
          do i = 1, n
            r(i,j) = r(i,j) - w(j) * x(i,j)
          end do
        end do

        call r8mat_print ( n, n, r,
     &  '  The residual matrix (A*B-Lambda*I)*X:' )

      end if

      return
      end
      subroutine rsgba_test ( matz )

c*********************************************************************72
c
cc RSGBA_TEST tests RSGBA.
c
c  Discussion:
c
c    RGGBA is for a real generalized eigenvalue problem of the form
c
c      B*A*x = lambda*x
c
c    with A symmetric and B positive definite symmetric.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision a(n,n)
      double precision a2(n,n)
      double precision b(n,n)
      double precision b2(n,n)
      double precision fv1(n)
      double precision fv2(n)
      integer i
      integer ierr
      integer j
      integer k
      integer matz
      double precision r(n,n)
      double precision w(n)
      double precision x(n,n)

      do i = 1, n
        do j = 1, n
          a(i,j) = abs ( i - j )
        end do
      end do

      do j = 1, n
        do i = 1, n
          a2(i,j) = a(i,j)
        end do
      end do

      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            b(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            b(i,j) = - 1.0D+00
          else
            b(i,j) = 0.0D+00
          end if
        end do
      end do

      do j = 1, n
        do i = 1, n
          b2(i,j) = b(i,j)
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RSGBA_TEST:'
      write ( *, '(a)' )
     &  '  RSGBA for real symmetric generalized problem.'
      write ( *, '(a)' ) '  Find scalars LAMBDA and vectors X so that'
      write ( *, '(a)' ) '    B*A*X = LAMBDA * X'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call r8mat_print ( n, n, b, '  The matrix B:' )

      call rsgba ( n, n, a, b, w, matz, x, fv1, fv2, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      if ( matz .ne. 0 ) then

        call r8mat_print ( n, n, x, '  The eigenvector matrix X:' )

        do i = 1, n
          do j = 1, n
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + a2(i,k) * x(k,j)
            end do
          end do
        end do

        do i = 1, n
          do j = 1, n
            a2(i,j) = r(i,j)
          end do
        end do

        do i = 1, n
          do j = 1, n
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + b2(i,k) * a2(k,j)
            end do
          end do
        end do

        do j = 1, n
          do i = 1, n
            r(i,j) = r(i,j) - w(j) * x(i,j)
          end do
        end do

        call r8mat_print ( n, n, r,
     &  '  The residual matrix (B*A-Lambda*I)*X:' )

      end if

      return
      end
      subroutine rsm_test ( )

c*********************************************************************72
c
cc RSM_TEST tests RSM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )
      integer m
      parameter ( m = n )

      double precision a(n,n)
      double precision a2(n,n)
      double precision fwork(8*n)
      integer i
      integer ierr
      integer iwork(n)
      integer j
      integer k
      double precision r(n,m)
      double precision w(n)
      double precision x(n,m)

      a(1,1) = 5.0D+00
      a(1,2) = 4.0D+00
      a(1,3) = 1.0D+00
      a(1,4) = 1.0D+00

      a(2,1) = 4.0D+00
      a(2,2) = 5.0D+00
      a(2,3) = 1.0D+00
      a(2,4) = 1.0D+00

      a(3,1) = 1.0D+00
      a(3,2) = 1.0D+00
      a(3,3) = 4.0D+00
      a(3,4) = 2.0D+00

      a(4,1) = 1.0D+00
      a(4,2) = 1.0D+00
      a(4,3) = 2.0D+00
      a(4,4) = 4.0D+00

      do j = 1, n
        do i = 1, n
          a2(i,j) = a(i,j)
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RSM_TEST'
      write ( *, '(a)' )
     &  '  RSM computes some eigenvalues and eigenvectors'
      write ( *, '(a)' ) '  of a real symmetric matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n
      write ( *, '(a,i8)' ) '  Number of eigenvectors desired = ', m

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call rsm ( n, n, a, w, m, x, fwork, iwork, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ',ierr
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      if ( 0 .lt. m ) then

        call r8mat_print ( n, m, x, '  The eigenvector matrix X:' )

        do i = 1, n
          do j = 1, m
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + a2(i,k) * x(k,j)
            end do
          end do
        end do

        do j = 1, m
          do i = 1, n
            r(i,j) = r(i,j) - w(j) * x(i,j)
          end do
        end do

        call r8mat_print ( n, m, r, '  The residual (A-Lambda*I)*X:' )

      end if

      return
      end
      subroutine rsp_test ( matz )

c*********************************************************************72
c
cc RSP_TEST tests RSP.
c
c  Discussion:
c
c    RSP is for the eigenvalues of a real symmetric packed matrix.
c
c    A is symmetric.  Because of this, we know that the eigenvalues
c    of A must be real (rather than complex) numbers.
c
c    The entries of A are
c
c    (5 4 1 1)
c    (4 5 1 1)
c    (1 1 4 2)
c    (1 1 2 4)
c
c    The eigenvalues of A are (10, 5, 2, 1)
c
c    One set of eigenvectors of A is:
c
c    ( 2 -1  0 -1)
c    ( 2 -1  0  1)
c    ( 1  2 -1  0)
c    ( 1  2  1  0)
c
c    However, this set is not orthonormal, and EISPACK will compute
c    a different set of values.
c
c    Note that the I-th eigenvector corresponding to the I-th eigenvalue
c    consists of the I-th column of the above matrix of eigenvectors.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 4 )
      integer nv
      parameter ( nv = ( n * ( n + 1 ) ) / 2 )

      double precision a(nv)
      double precision a2(n,n)
      double precision fv1(n)
      double precision fv2(n)
      integer i
      integer ierr
      integer j
      integer k
      integer matz
      double precision r(n,n)
      double precision w(n)
      double precision x(n,n)
c
c  Set the values in the matrix.
c
      a(1) = 5.0D+00

      a(2) = 4.0D+00
      a(3) = 5.0D+00

      a(4) = 1.0D+00
      a(5) = 1.0D+00
      a(6) = 4.0D+00

      a(7) = 1.0D+00
      a(8) = 1.0D+00
      a(9) = 2.0D+00
      a(10) = 4.0D+00

      a2(1,1) = 5.0D+00
      a2(1,2) = 4.0D+00
      a2(1,3) = 1.0D+00
      a2(1,4) = 1.0D+00

      a2(2,1) = 4.0D+00
      a2(2,2) = 5.0D+00
      a2(2,3) = 1.0D+00
      a2(2,4) = 1.0D+00

      a2(3,1) = 1.0D+00
      a2(3,2) = 1.0D+00
      a2(3,3) = 4.0D+00
      a2(3,4) = 2.0D+00

      a2(4,1) = 1.0D+00
      a2(4,2) = 1.0D+00
      a2(4,3) = 2.0D+00
      a2(4,4) = 4.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RSP_TEST'
      write ( *, '(a)' )
     &  '  RSP computes the eigenvalues and eigenvectors'
      write ( *, '(a)' ) '  of a real symmetric packed matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a2, '  The matrix A:' )

      call rsp ( n, n, nv, a, w, matz, x, fv1, fv2, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' )
     &  '  The error return flag was IERR = ', ierr
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      if ( matz .ne. 0 ) then

        call r8mat_print ( n, n, x, '  The eigenvector matrix X:' )

        do i = 1, n
          do j = 1, n
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + a2(i,k) * x(k,j)
            end do
          end do
        end do

        do j = 1, n
          do i = 1, n
            r(i,j) = r(i,j) - w(j) * x(i,j)
          end do
        end do

        call r8mat_print ( n, n, r,
     &  '  The residual matrix (A-Lambda*I)*X:' )

      end if

      return
      end
      subroutine rspp_test ( matz )

c*********************************************************************72
c
cc RSPP_TEST tests RSPP.
c
c  Discussion:
c
c    RSPP is for some eigenvalues of a real symmetric packed matrix.
c
c    A is symmetric.  Because of this, we know that the eigenvalues
c    of A must be real (rather than complex) numbers.
c
c    The entries of A are
c
c    (5 4 1 1)
c    (4 5 1 1)
c    (1 1 4 2)
c    (1 1 2 4)
c
c    The eigenvalues of A are (10, 5, 2, 1)
c
c    One set of eigenvectors of A is:
c
c    ( 2 -1  0 -1)
c    ( 2 -1  0  1)
c    ( 1  2 -1  0)
c    ( 1  2  1  0)
c
c    However, this set is not orthonormal, and EISPACK will compute
c    a different set of values.
c
c    Note that the I-th eigenvector corresponding to the I-th eigenvalue
c    consists of the I-th column of the above matrix of eigenvectors.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 4 )
      integer m
      parameter ( m = n )
      integer nv
      parameter ( nv = ( n * ( n + 1 ) ) / 2 )

      double precision a(nv)
      double precision a2(n,n)
      double precision bd(n)
      double precision d(n )
      double precision e(n)
      double precision e2(n)
      integer i
      integer ierr
      integer j
      integer k
      integer matz
      double precision r(n,m)
      logical type
      double precision w(m)
      double precision work1(n)
      double precision work2(n)
      double precision work3(n)
      double precision work4(n)
      double precision work6(n)
      double precision x(n,m)
c
c  Set the values in the matrix.
c
      a(1) = 5.0D+00

      a(2) = 4.0D+00
      a(3) = 5.0D+00

      a(4) = 1.0D+00
      a(5) = 1.0D+00
      a(6) = 4.0D+00

      a(7) = 1.0D+00
      a(8) = 1.0D+00
      a(9) = 2.0D+00
      a(10) = 4.0D+00

      a2(1,1) = 5.0D+00
      a2(1,2) = 4.0D+00
      a2(1,3) = 1.0D+00
      a2(1,4) = 1.0D+00

      a2(2,1) = 4.0D+00
      a2(2,2) = 5.0D+00
      a2(2,3) = 1.0D+00
      a2(2,4) = 1.0D+00

      a2(3,1) = 1.0D+00
      a2(3,2) = 1.0D+00
      a2(3,3) = 4.0D+00
      a2(3,4) = 2.0D+00

      a2(4,1) = 1.0D+00
      a2(4,2) = 1.0D+00
      a2(4,3) = 2.0D+00
      a2(4,4) = 4.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RSPP_TEST'
      write ( *, '(a)' )
     &  '  RSPP finds some eigenvalues and eigenvectors of'
      write ( *, '(a)' ) '  a real symmetric packed matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a2, '  The matrix A:' )
c
c  TYPE = TRUE to find smallest eigenvalues, FALSE for largest.
c
      type = .true.

      call rspp ( n, nv, a, w, matz, x, ierr, m, type, bd, d, e, e2,
     &  work1, work2, work3, work4, work6 )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' )
     &  '  The error return flag was IERR = ', ierr
      end if

      call r8vec_print ( m, w, '  The eigenvalues Lambda:' )

      if ( matz .ne. 0 ) then

        call r8mat_print ( n, m, x, '  The eigenvector matrix X:' )

        do i = 1, n
          do j = 1, m
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + a2(i,k) * x(k,j)
            end do
          end do
        end do

        do j = 1, m
          do i = 1, n
            r(i,j) = r(i,j) - w(j) * x(i,j)
          end do
        end do

        call r8mat_print ( n, m, r,
     &  '  The residual matrix (A-Lambda*I)*X:' )

      end if

      return
      end
      subroutine rst_test ( matz )

c*********************************************************************72
c
cc RST_TEST tests RST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision e(n)
      integer i
      integer ierr
      integer j
      integer k
      integer matz
      double precision r(n,n)
      double precision w(n)
      double precision x(n,n)
c
c  Here is where the matrix is defined.
c
      do i = 1, n
        w(i) = 2.0D+00
      end do

      e(1) = 0.0D+00
      do i = 2, n
        e(i) = -1.0D+00
      end do
c
c  We only set up and store the matrix A this way in order to make it easy
c  to compute the residual.
c
      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a(i,j) = - 1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RST_TEST'
      write ( *, '(a)' )
     &  '  RST computes the eigenvalues and eigenvectors'
      write ( *, '(a)' ) '  of a real symmetric tridiagonal matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call rst ( n, n, w, e, matz, x, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      if ( matz .ne. 0 ) then

        call r8mat_print ( n, n, x, '  The eigenvector matrix X:' )

        do i = 1, n
          do j = 1, n
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + a(i,k) * x(k,j)
            end do
          end do
        end do

        do j = 1, n
          do i = 1, n
            r(i,j) = r(i,j) - w(j) * x(i,j)
          end do
        end do

        call r8mat_print ( n, n, r,
     &  '  The residual matrix (A-Lambda*I)*X:' )

      end if

      return
      end
      subroutine rt_test ( matz )

c*********************************************************************72
c
cc RT_TEST tests RT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 February 2018
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical MATZ.
c    FALSE, eigenvalues only.
c    TRUE, eigenvalues and eigenvectors.
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,3)
      double precision a2(n,n)
      double precision fv1(n)
      integer i
      integer ierr
      integer j
      integer k
      integer matz
      double precision r(n,n)
      double precision w(n)
      double precision x(n,n)
c
c  Here is where the matrix is defined.
c
      do i = 2, n
        a(i,1) = - 1.0D+00
      end do

      do i = 1, n
        a(i,2) =   2.0D+00
      end do

      do i = 1, n - 1
        a(i,3) = - 1.0D+00
      end do
c
c  We only set up and store the matrix A this way in order to make it easy
c  to compute the residual.
c
      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a2(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a2(i,j) = - 1.0D+00
          else
            a2(i,j) = 0.0D+00
          end if
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'RT_TEST'
      write ( *, '(a)' )
     &  '  RT computes the eigenvalues and eigenvectors'
      write ( *, '(a)' )
     &  '  of a real sign-symmetric tridiagonal matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a2, '  The matrix A:' )

      call rt ( n, n, a, w, matz, x, fv1, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
      end if

      call r8vec_print ( n, w, '  The eigenvalues Lambda:' )

      if ( matz .ne. 0 ) then

        call r8mat_print ( n, n, x, '  The eigenvector matrix X:' )

        do i = 1, n
          do j = 1, n
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + a2(i,k) * x(k,j)
            end do
          end do
        end do

        do j = 1, n
          do i = 1, n
            r(i,j) = r(i,j) - w(j) * x(i,j)
          end do
        end do

        call r8mat_print ( n, n, r,
     &  '  The residual matrix (A-Lambda*I)*X:' )

      end if

      return
      end
      subroutine svd_test ( )

c*********************************************************************72
c
cc SVD_TEST tests SVD.
c
c  Discussion:
c
c    In our special example, the matrix is square and symmetric.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 February 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )
      integer m
      parameter ( m = n )

      double precision a(m,n)
      integer i
      integer ierr
      integer j
      integer k
      logical matu
      logical matv
      double precision r(m,n)
      double precision rv1(n)
      double precision u(m,n)
      double precision v(n,n)
      double precision w(n)
c
c  Set the values of the matrix.
c
      a(1,1) = 0.9900D+00
      a(1,2) = 0.0020D+00
      a(1,3) = 0.0060D+00
      a(1,4) = 0.0020D+00

      a(2,1) = 0.0020D+00
      a(2,2) = 0.9900D+00
      a(2,3) = 0.0020D+00
      a(2,4) = 0.0060D+00

      a(3,1) = 0.0060D+00
      a(3,2) = 0.0020D+00
      a(3,3) = 0.9900D+00
      a(3,4) = 0.0020D+00

      a(4,1) = 0.0020D+00
      a(4,2) = 0.0060D+00
      a(4,3) = 0.0020D+00
      a(4,4) = 0.9900D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SVD_TEST'
      write ( *, '(a)' )
     &  '  SVD computes the singular value decomposition'
      write ( *, '(a)' ) '  of a real general matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( m, n, a, '  The matrix A:' )

      matu = .true.
      matv = .true.

      call svd ( m, m, n, a, w, matu, u, matv, v, ierr, rv1 )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
      end if

      call r8vec_print ( n, w, '  The singular values S' )

      call r8mat_print ( m, n, u, '  The U matrix:' )

      call r8mat_print ( n, n, v, '  The V matrix:' )

      do j = 1, n
        do i = 1, n
          v(i,j) = w(j) * v(i,j)
        end do
      end do

        do i = 1, m
          do j = 1, n
            r(i,j) = 0.0D+00
            do k = 1, n
              r(i,j) = r(i,j) + u(i,k) * v(j,k)
            end do
          end do
        end do

      call r8mat_print ( m, n, r,
     &  '  The product U * S * Transpose(V):' )

      return
      end
      subroutine tql1_test ( )

c*********************************************************************72
c
cc TQL1_TEST tests TQL1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 January 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      double precision a(n,n)
      double precision d(n)
      double precision e(n)
      integer i
      integer ierr
      integer j
c
c  Here is where the matrix is defined.
c
      do i = 1, n
        d(i) = 2.0D+00
      end do

      e(1) = 0.0D+00
      do i = 2, n
        e(i) = -1.0D+00
      end do
c
c  We only set up and store the matrix A this way in order to make it easy
c  to compute the residual.
c
      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = d(i)
          else if ( i .eq. j - 1 ) then
            a(i,j) = e(j)
          else if ( i .eq. j + 1 ) then
            a(i,j) = e(i)
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TQL1_TEST'
      write ( *, '(a)' ) '  TQL1 computes the eigenvalues'
      write ( *, '(a)' ) '  of a real symmetric tridiagonal matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call tql1 ( n, d, e, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TQL1_TEST - Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
        return
      end if

      call r8vec_print ( n, d, '  The eigenvalues Lambda:' )

      return
      end
      subroutine tridib_test ( )

c*********************************************************************72
c
cc TRIDIB_TEST tests TRIDIB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
 
      double precision a(n,n)
      double precision d(n)
      double precision e(n)
      double precision e2(n)
      double precision eps1
      integer i
      integer ierr
      integer ind(n)
      integer j
      double precision lb
      integer m
      integer m11
      double precision r8_epsilon
      double precision rv4(n)
      double precision rv5(n)
      double precision ub
      double precision w(n)

      eps1 = 100.0D+00 * r8_epsilon ( )
!
!  Here is where the matrix is defined.
!
      do i = 1, n
        d(i) = 2.0D+00
      end do

      e(1) = 0.0D+00
      do i = 2, n
        e(i) = -1.0D+00
      end do
      do i = 1, n
        e2(i) = e(i) ** 2
      end do
      m11 = 1
      m = 5
!
!  We only set up and store the matrix A this way in order to make it easy
!  to compute the residual.
!
      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a(i,j) = - 1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TRIDIB_TEST'
      write ( *, '(a)' ) '  TRIDIB computes some eigenvalues of '
      write ( *, '(a)' ) '  a real symmetric tridiagonal matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call tridib ( n, eps1, d, e, e2, lb, ub, m11, m, w, ind, 
     &  ierr, rv4, rv5 )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TRIDIB_TEST - Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
        return
      end if

      call r8vec_print ( m, w, '  The eigenvalues Lambda:' )

      return
      end
      subroutine tsturm_test ( )

c*********************************************************************72
c
cc TSTURM_TEST tests TSTURM.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 March 2018
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
 
      double precision a(n,n)
      double precision d(n)
      double precision e(n)
      double precision e2(n)
      double precision eps1
      integer i
      integer ierr
      integer j
      integer k
      double precision lb
      integer m
      integer mm
      integer nm
      double precision r(n,n)
      double precision r8_epsilon
      double precision rv1(n)
      double precision rv2(n)
      double precision rv3(n)
      double precision rv4(n)
      double precision rv5(n)
      double precision rv6(n)
      double precision ub
      double precision w(n)
      double precision z(n,n)

      nm = 5
      eps1 = 10.0D+00 * r8_epsilon ( )
!
!  Here is where the matrix is defined.
!
      do i = 1, n
        d(i) = 2.0D+00
      end do

      e(1) = 0.0D+00
      do i = 2, n
        e(i) = -1.0D+00
      end do
      do i = 1, n
        e2(i) = e(i) ** 2
      end do
      lb = -5.0D+00
      ub = +5.0D+00
      mm = 5
!
!  We only set up and store the matrix A this way in order to make it easy
!  to compute the residual.
!
      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a(i,j) = - 1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TSTURM_TEST'
      write ( *, '(a)' ) '  TSTURM computes some eigenvalues and'
      write ( *, '(a)' ) '  eigenvectors of a real symmetric '
      write ( *, '(a)' ) '  tridiagonal matrix.'
      write ( *, '(a,i8)' ) '  Matrix order = ', n

      call r8mat_print ( n, n, a, '  The matrix A:' )

      call tsturm ( nm, n, eps1, d, e, e2, lb, ub, mm, m, w, z, ierr,
     &  rv1, rv2, rv3, rv4, rv5, rv6 )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TSTURM_TEST - Warning!'
        write ( *, '(a,i8)' ) '  The error return flag IERR = ', ierr
        return
      end if
c
c  Restore A.
c
      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            a(i,j) = 2.0D+00
          else if ( abs ( i - j ) .eq. 1 ) then
            a(i,j) = - 1.0D+00
          else
            a(i,j) = 0.0D+00
          end if
        end do
      end do

      call r8vec_print ( m, w, '  The eigenvalues Lambda:' )

      call r8mat_print ( n, m, z, '  The eigenvector matrix Z:' )

      do j = 1, m
        do i = 1, n
          r(i,j) = 0.0D+00
          do k = 1, n
            r(i,j) = r(i,j) + a(i,k) * z(k,j)
          end do
          r(i,j) = r(i,j) - w(j) * z(i,j)
        end do
      end do

      call r8mat_print ( n, m, r, '  The residual (A-Lambda*I)*X:' )

      return
      end
      function r8_epsilon ( )

c*********************************************************************72
c
cc R8_EPSILON returns the R8 roundoff unit.
c
c  Discussion:
c
c    The roundoff unit is a number R which is a power of 2 with the
c    property that, to the precision of the computer's arithmetic,
c      1 .lt. 1 + R
c    but
c      1 = ( 1 + R / 2 )
c
c    FORTRAN90 provides the superior library routine
c
c      EPSILON ( X )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_EPSILON, the R8 roundoff unit.
c
      implicit none

      double precision r8_epsilon

      r8_epsilon = 2.220446049250313D-016

      return
      end
      subroutine r8mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_COPY copies an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the order of the matrix.
c
c    Input, double precision A1(M,N), the matrix to be copied.
c
c    Output, double precision A2(M,N), a copy of the matrix.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
      end do

      return
      end
      subroutine r8mat_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_PRINT prints an R8MAT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character ( len = * ) title

      call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R8MAT_PRINT_SOME prints some of an R8MAT.
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
c    Input, character ( len = * ) TITLE, an optional title.
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
      integer title_length

      title_length = len_trim ( title )

      if ( 0 .lt. title_length ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) title(1:title_length)
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ''

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ''

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(g14.6)' ) a(i,j)

          end do

          write ( *, '(i5,1x,5a14)' ) i, ( ctemp(j), j = 1, inc )

        end do

      end do

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
c
c  Discussion:
c
c    An R8VEC is an array of double precision real values.
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
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, an optional title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      integer s_len_trim
      character ( len = * ) title
      integer title_length

      title_length = s_len_trim ( title )
      if ( 0 .lt. title_length ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) title(1:title_length)
      end if

      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,i8,2x,g16.8)' ) i, a(i)
      end do

      return
      end
      subroutine r8vec2_print ( n, a1, a2, title )

c*********************************************************************72
c
cc R8VEC2_PRINT prints an R8VEC2.
c
c  Discussion:
c
c    An R8VEC2 is a dataset consisting of N pairs of R8s, stored
c    as two separate vectors A1 and A2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 February 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A1(N), A2(N), the vectors to be printed.
c
c    Input, character ( len = * ) TITLE, a title to be printed first.
c    TITLE may be blank.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i
      integer s_len_trim
      character ( len = * ) title
      integer title_len

      title_len = s_len_trim ( title )

      if ( 0 .lt. title_len ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) title(1:title_len)
      end if

      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) i, a1(i), a2(i)
      end do

      return
      end
      function s_len_trim ( s )

c*********************************************************************72
c
cc S_LEN_TRIM returns the length of a string to the last nonblank.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 March 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) S, a string.
c
c    Output, integer S_LEN_TRIM, the length of the string to the last nonblank.
c
      implicit none

      integer i
      character*(*) s
      integer s_len_trim

      do i = len ( s ), 1, -1

        if ( s(i:i) .ne. '' ) then
          s_len_trim = i
          return
        end if

      end do

      s_len_trim = 0

      return
      end

