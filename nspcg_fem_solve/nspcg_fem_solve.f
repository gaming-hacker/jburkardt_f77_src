!  flonsp.f  11 August 1995
!
      program main

!*********************************************************************72
!
!! MAIN is the main program for NSPCG_FEM_SOLVE.
!
!  Discussion:
!
!    NSPCG_FEM_SOLVE solves a linear system derived from a finite element
!    problem using the NSPCG package.
!
!    The linear system is derived from a finite element grid:
!
!    O-O-O-O-O-O-O-O-O
!    |  /|  /|  /|  /|
!    O O O O O O O O O
!    |/  |/  |/  |/  |
!    O-O-O-O-O-O-O-O-O
!    |  /|  /|  /|  /|
!    O O O O O O O O O
!    |/  |/  |/  |/  |
!    O-O-O-O-O-O-O-O-O
!
!    We use the nonsymmetric coordinate format for storage of the matrix.
!
!  Modified:
!
!    21 November 2008
!
!  Author:
!
!    John Burkardt
!
      integer inw
      integer maxeqn
      integer maxnon
      integer maxnp
      integer maxnx
      integer maxny
      integer nw

      parameter (inw=677)
      parameter (maxnx=11)
      parameter (maxny=4)
      parameter (maxeqn=2*(2*maxnx-1)*(2*maxny-1)+maxnx*maxny)
      parameter (maxnp=(2*maxnx-1)*(2*maxny-1))
      parameter (maxnon=8734)
      parameter (nw=6000)

      double precision a(maxnon)
      double precision anab(3,3,19,maxnp)
      double precision b(maxeqn)
      double precision errmax
      integer i
      integer ier
      integer imax
      integer indx(3,maxnp)
      integer inwcopy
      integer ip(maxeqn)
      integer iparm(25)
      integer iwksp(inw)
      integer jcoef(maxnon,2)
      integer mdim
      integer nabor(19,maxnp)
      integer ndim
      integer neqn
      integer nonz
      integer np
      integer nwcopy
      integer p(maxeqn)
      double precision rparm(16)
      double precision sol(maxeqn)
      double precision temp
      double precision u(maxeqn)
      double precision ubar(1)
      double precision wksp(nw)

      external gmres
      external iom
      external landir
      external lanmin
      external lanres
      external odir
      external omin
      external ores
      external rich5
      external usymlq
      external usymqr

      write ( *, * ) ' '
      write ( *, * ) 'NSPCG_FEM_SOLVE'
      write ( *, * ) '  FORTRAN77 version'
!
!  Read the data in neigbor format.
!
      call nab_read ( anab, b, indx, maxeqn, maxnp, nabor, neqn, np, 
     &  sol )
!
!  Convert the neighbor data to sparse format.
!
      call nab_to_sparse ( a, anab, indx, jcoef, maxnon, nabor, nonz, 
     &  np )

      write ( *, * ) 'Number of nonzeros needed for A was ', nonz
      write ( *, * ) 'We allocated ', maxnon
!
!  Call DFAULT to set variables to default values.
!
      call dfault ( iparm, rparm )
!
!  Using storage mode 5.
!
      iparm(12) = 5
!
!  The matrix is not symmetric.
!
      iparm(23) = 1
!
!  Call the NSPCG solver.
!
!    RICH5 is the Richardson preconditioner, which is no preconditioning
!      at all.
!
      ndim = maxnon
      mdim = 1
      u = 0.0
      nwcopy = nw
      inwcopy = inw

      call nspcg ( rich5, gmres, ndim, mdim, neqn, maxnon, a, jcoef, p,
     &  ip, u, ubar, b, wksp, iwksp, nwcopy, inwcopy, iparm, rparm, 
     &  ier )

      write ( *, * ) 'Amount of space allocated in WKSP is ', nw
      write ( *, * ) 'Amount required was ', nwcopy
      write ( *, * ) 'Amount of space allocated in IWKSP is ', inw
      write ( *, * ) 'Amount required was ', inwcopy

      if ( ier /= 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'FLONSP - Fatal error!'
        write ( *, * ) '  Solver returns IER=',ier
      end if
!
!  Calculate the error.
!
      errmax = 0.0
      do i = 1, neqn

        temp = abs ( u(i) - sol(i) )

        if ( temp >= errmax ) then
          imax = i
          errmax = temp
        end if

      end do

      write ( *, * ) ' '
      write ( *, * ) 'Maximum solution difference is ', errmax
      write ( *, * ) 'for component ', imax
      write ( *, * ) ' '

      stop
      end
      subroutine nab_read ( anab, b, indx, maxeqn, maxnp, nabor, neqn, 
     &  np, sol )

!*********************************************************************72
!
!! NAB_READ reads the raw linear system data.
!
!  Modified:
!
!    21 November 2008
!
!  Author:
!
!    John Burkardt
!
      integer maxeqn
      integer maxnp

      double precision anab(3,3,19,maxnp)
      double precision b(maxeqn)
      integer i
      integer indx(3,maxnp)
      integer j
      integer k
      integer l
      integer nabor(19,np)
      integer neqn
      integer np
      double precision sol(maxeqn)
      double precision temp

      open (
     &  unit = 10,
     &  file = 'flospa.bin',
     &  status = 'old',
     &  form = 'unformatted' )

      read ( 10 ) np
      write ( *, * ) 'NP=',np
      read ( 10 ) neqn
      write ( *, * ) 'NEQN=',neqn

      do i = 1, np
        do j = 1, 3
          read ( 10 ) indx(j,i)
        end do
      end do

      write ( *, * ) 'Read INDX'

      do i = 1, np
        do j = 1, 19
          read ( 10 ) nabor(j,i)
        end do
      end do

      write ( *, * ) 'Read NABOR'

      do i = 1, np
        do j = 1, 19
          do k = 1, 3
            do l = 1, 3
              read ( 10 ) temp
              anab(l,k,j,i) = temp
            end do
          end do
        end do
      end do

      write ( *, * ) 'Read ANAB'

      do i = 1, neqn
        read (10) temp
        b(i) = temp
      end do

      write ( *, * ) 'Read B'

      do i = 1, neqn
        read ( 10 ) temp
        sol(i) = temp
      end do

      write ( *, * ) 'Read SOL'

      close ( unit = 10 )

      return
      end
      subroutine nab_to_sparse ( a, anab, indx, jcoef, maxnon, nabor, 
     &  nonz, np )

!*********************************************************************72
! 
!! NAB_TO_SPARSE copies the coefficients from the neighbor array ANAB into the sparse array A.
!
!  Modified:
!
!    21 November 2008
!
!  Author:
!
!    John Burkardt
!
      integer maxnon
      integer np

      double precision a(maxnon)
      double precision anab(3,3,19,np)
      integer ieqn
      integer ihi
      integer indx(3,np)
      integer ip
      integer irow
      integer jcoef(maxnon,2)
      integer jcol
      integer jhi
      integer jp
      integer nab
      integer nabor(19,np)
      integer nonz

      nonz = 0
      ieqn = 0
!
!  Consider each node in order.
!
      do ip = 1, np

        if ( indx(3,ip) == 0 ) then
          ihi = 2
        else
          ihi = 3
        end if
!
!  Consider the U, V, and possibly P equations at that node.
!
        do irow = 1, ihi

          ieqn = ieqn + 1
!
!  Look at the 19 possible neighbors of the node.
!
          do nab = 1, 19

            jp = nabor(nab,ip)
!
!  Consider each of the U, V, and P variables associated with the
!  neighbor node.
!
            if ( jp /= 0 ) then

              if ( indx(3,jp) == 0 ) then
                jhi = 2
              else
                jhi = 3
              end if

              do jcol = 1, jhi

                nonz = nonz + 1

                if ( nonz == maxnon + 1 ) then
                  write ( *, * ) ' '
                  write ( *, * ) 'NABSPA - Fatal error!'
                  write ( *, * ) '  Too many entries to store!'
                  write ( *, * ) '  Number allocated, MAXNON=',maxnon
                elseif ( nonz > maxnon + 1 ) then

                else
                  a(nonz) = anab(irow,jcol,nab,ip)
                  jcoef(nonz,1) = indx(irow,ip)
                  jcoef(nonz,2) = indx(jcol,jp)
                end if

              end do

            end if

          end do
        end do
      end do

      if ( nonz > maxnon ) then
        write ( *, * ) ' '
        write ( *, * ) 'NABSPA - Fatal error!'
        write ( *, * ) 'Number of nonzeros needed, NONZ=',nonz
        stop
      end if

      return
      end
      subroutine prnmat ( anab, ip, nabor, np ) 

!*********************************************************************72
!
!! PRNMAT prints the matrix entries for a particular node.
!
!  Discussion:
!
!    The matrix is assumed to be stored in the neighbor format.
!
!  Modified:
!
!    02 May 2000
!
!  Author:
!
!    John Burkardt
!
      integer np

      double precision anab(3,3,19,np)
      integer i
      integer ip
      integer j
      integer nab
      integer nabor(19,np)
      integer nod

      write ( *, * ) ' '
      write ( *, * ) 'PrNMat:'
      write ( *, * ) '  Matrix entries for node ',ip
      write ( *, * ) ' '

      do nab = 1, 19

        nod = nabor(nab,ip)

        if ( nod /= 0 ) then
          write ( *, * ) ' '
          write ( *, * ) 'Neighbor node ', nod
          write ( *, * ) ' '
          do i = 1, 3
            write ( *, '(1x,3g14.6)' ) ( anab(i,j,nab,ip), j = 1, 3 )
          end do
        end if

      end do

      return
      end
      subroutine prspar ( a, i1, i2, ia, j1, j2, ja, neqn, nonz )

!*********************************************************************72
!
!! PRSPAR prints all nonzero entries of rows I1 to I2, columns J1 to 
!  J2 of the matrix, plus the right hand side.
!
!  Modified:
!
!    21 November 2008
!
!  Author:
!
!    John Burkardt
!
      integer neqn
      integer nonz

      double precision a(nonz)
      integer i
      integer i1
      integer i2
      integer ia(neqn+1)
      integer j
      integer j1
      integer j2
      integer ja(nonz)
      integer k

      write ( *, * ) ' '
      write ( *, * ) 'PrSpar:'
      write ( *, * ) '  Print rows ',i1,' through ',i2
      write ( *, * ) '  Columns ',j1,' through ',j2
      write ( *, * ) ' '
      write ( *, * ) '  I, J, A(I,J)'
      write ( *, * ) ' '

      do i = i1, i2

        write ( *, * ) ' '

        do k = ia(i), ia(i+1)-1

          j = ja(k)
          if ( j1 <= j .and. j <= j2 .and. a(k) /= 0.0 ) then
            write ( *, * ) i, j, a(k)
          end if
        end do
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
