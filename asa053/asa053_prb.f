      program main

c*********************************************************************72
c
cc MAIN is the main program for ASA053_PRB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ASA053_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the ASA053 library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'ASA053_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 generates a random Wishart variate.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer np
      parameter ( np = 3 )

      integer nnp
      parameter ( nnp = ( np * ( np + 1 ) ) / 2 )

      double precision d(nnp)
      integer i
      integer n
      double precision sa(nnp)
      double precision sb(nnp)
      integer seed

      save d

      data d /
     &  3.0D+00,
     &  2.0D+00, 4.0D+00, 
     &  1.0D+00, 2.0D+00, 5.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Generate a single Wishart deviate.'

      n = 1
      seed = 123456789

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  The number of variables is ', np
      write ( *, '(a,i4)' ) '  The number of degrees of freedom is ', n

      call r8utp_print ( np, d, '  The upper Cholesky factor:' )

      call wshrt ( d, n, np, nnp, seed, sb, sa )

      call r8pp_print ( np, sa, '  The sample matrix:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 averages many Wishart samples.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer np
      parameter ( np = 3 )

      integer npp
      parameter ( npp = ( np * ( np + 1 ) ) / 2 )

      double precision d(npp)
      integer i
      integer j
      integer k
      integer ki
      integer kj
      integer n
      double precision s_average(npp)
      double precision sa(npp)
      double precision sb(npp)
      double precision sigma(np,np)
      integer seed
      integer test_num

      save d

      data d /
     &  3.0D+00,
     &  2.0D+00, 4.0D+00, 
     &  1.0D+00, 2.0D+00, 5.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Average many Wishart samples.'
      write ( *, '(a)' ) '  Compare to D'' * D * np / n.'

      n = 2
      seed = 123456789

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  The number of variables is ', np
      write ( *, '(a,i4)' ) '  The number of degrees of freedom is ', n

      call r8utp_print ( np, d, '  The upper Cholesky factor:' )

      do j = 1, npp
        s_average(j) = 0.0D+00
      end do

      test_num = 100000
      do i = 1, test_num
        call wshrt ( d, n, np, npp, seed, sb, sa )
        do j = 1, npp
          s_average(j) = s_average(j) + sa(j)
        end do
      end do

      do j = 1, npp
        s_average(j) = s_average(j) / dble ( test_num )
      end do

      call r8pp_print ( np, s_average, '  The averaged matrix:' )
c
c  Compare the result to ( D' * D ) * np / n.
c
      do i = 1, np
        do j = 1, np
          sigma(i,j) = 0.0D+00
          do k = 1, min ( i, j )
            ki = k + ( i * ( i - 1 ) ) / 2;
            kj = k + ( j * ( j - 1 ) ) / 2;
            sigma(i,j) = sigma(i,j) + d(ki) * d(kj);
          end do
          sigma(i,j) = sigma(i,j) * dble ( np ) / dble ( n )
        end do
      end do

      call r8mat_print ( np, np, sigma, '  Expected result:' )

      return
      end

