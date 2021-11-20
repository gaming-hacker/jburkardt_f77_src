c  FLOIT.F  11 August 1995
c
      program floit
c
c***********************************************************************
c
c  The linear system is derived from a finite element grid:
c
c  O-O-O-O-O-O-O-O-O
c  |  /|  /|  /|  /|
c  O O O O O O O O O
c  |/  |/  |/  |/  |
c  O-O-O-O-O-O-O-O-O
c  |  /|  /|  /|  /|
c  O O O O O O O O O
c  |/  |/  |/  |/  |
c  O-O-O-O-O-O-O-O-O
c
      integer maxeqn
      integer maxnon
      integer maxnp
      integer maxnx
      integer maxny
      integer nw
c
      parameter (maxnx=11)
      parameter (maxny=4)
      parameter (maxeqn=2*(2*maxnx-1)*(2*maxny-1)+maxnx*maxny)
      parameter (maxnp=(2*maxnx-1)*(2*maxny-1))
      parameter (maxnon=8734)
      parameter (nw=1552)
c
      double precision a(maxnon)
      double precision anab(3,3,19,maxnp)
      double precision b(maxeqn)
      double precision errmax
      integer i
      integer ia(maxeqn+1)
      integer ier
      integer imax
      integer indx(3,maxnp)
      integer iparm(12)
      integer iwksp(3,maxeqn)
      integer ja(maxnon)
      integer nabor(19,maxnp)
      integer neqn
      integer nonz
      integer np
      double precision rparm(12)
      double precision sol(maxeqn)
      double precision temp
      double precision u(maxeqn)
      double precision wksp(nw)
c
c  Set up the matrix and right hand side.
c
      call nmat(a,anab,b,ia,indx,ja,maxeqn,maxnon,maxnp,nabor,
     &  neqn,np,sol)

      nonz=ia(neqn+1)-1
      write(*,*)'Number of nonzeros needed for A was ',nonz
      write(*,*)'We allocated ',maxnon
c
c  Set the default values for ITPACK parameters.
c
      call dfault(iparm,rparm)
c
c  Call the ITPACK solver.
c
      call jcg(neqn,ia,ja,a,b,u,iwksp,nw,wksp,iparm,rparm,ier)

      if(ier.ne.0)then
        write(*,*)' '
        write(*,*)'NTST - Fatal error!'
        write(*,*)'  ITPACK solver returns IER=',ier
        stop
      endif

      write(*,*)'Amount of space allocated in WKSP is ',nw
      write(*,*)'Amount required was ',IPARM(8)
c
c  Calculate the error
c
      errmax=0.0
      do i=1,neqn

        temp=abs(u(i)-sol(i))
        if(temp.ge.errmax)then
          imax=i
          errmax=temp
        endif

      enddo

      write(*,*)' '
      write(*,*)'Maximum solution difference is ',errmax
      write(*,*)'for component ',imax
      write(*,*)' '

      stop
      end
      subroutine nmat(a,anab,b,ia,indx,ja,maxeqn,maxnon,maxnp,nabor,
     &  neqn,np,sol)
c
c***********************************************************************
c
      integer maxeqn
      integer maxnon
      integer maxnp
c
      double precision a(maxnon)
      double precision anab(3,3,19,maxnp)
      double precision b(maxeqn)
      integer ia(maxeqn+1)
      integer indx(3,maxnp)
      integer ja(maxnon)
      integer nabor(19,maxnp)
      integer neqn
      integer np
      double precision sol(maxeqn)
c
c  Read data.
c
      call rdat(anab,b,indx,maxeqn,maxnp,nabor,neqn,np,sol)
c
c  Convert matrix to sparse format.
c
      call nabspa(a,anab,ia,indx,ja,maxnon,nabor,neqn,np)

      return
      end
      subroutine rdat(anab,b,indx,maxeqn,maxnp,nabor,neqn,np,sol)
c
c***********************************************************************
c
      integer maxeqn
      integer maxnp
c
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
c
      open(unit=10,file='flospa.bin',status='unknown',
     &  form='unformatted')

      read(10)np
      write(*,*)'NP=',np
      read(10)neqn
      write(*,*)'NEQN=',neqn

      do i=1,np
        do j=1,3
          read(10)indx(j,i)
        enddo
      enddo

      write(*,*)'Read INDX'

      do i=1,np
        do j=1,19
          read(10)nabor(j,i)
        enddo
      enddo

      write(*,*)'Read NABOR'

      do i=1,np
        do j=1,19
          do k=1,3
            do l=1,3
              read(10)anab(l,k,j,i)
            enddo
          enddo
        enddo
      enddo

      write(*,*)'Read ANAB'

      do i=1,neqn
        read(10)b(i)
      enddo

      write(*,*)'Read B'

      do i=1,neqn
        read(10)sol(i)
      enddo

      write(*,*)'Read SOL'

      close(unit=10)

      return
      end
      subroutine nabspa(a,anab,ia,indx,ja,maxnon,nabor,neqn,np)
c 
c***********************************************************************
c 
c  NABSPA copies the coefficients from the neighbor array ANAB into
c  the sparse array A.
c
      integer maxnon
      integer neqn
      integer np
c
      double precision a(maxnon)
      double precision anab(3,3,19,np)
      integer ia(neqn+1)
      integer ieqn
      integer ihi
      integer indx(3,np)
      integer ip
      integer irow
      integer ja(maxnon)
      integer jcol
      integer jhi
      integer jp
      integer nab
      integer nabor(19,np)
      integer nonz
c
      nonz=0
      ieqn=0
c
c  Consider each node in order.
c
      do ip=1,np

        if(indx(3,ip).eq.0)then
          ihi=2
        else
          ihi=3
        endif
c
c  Consider the U, V, and possibly P equations at that node.
c
        do irow=1,ihi

          ieqn=ieqn+1
          ia(ieqn)=nonz+1
c
c  Look at the 19 possible neighbors of the node.
c
          do nab=1,19

            jp=nabor(nab,ip)
c
c  Consider each of the U, V, and P variables associated with the
c  neighbor node.
c
            if(jp.ne.0)then

              if(indx(3,jp).eq.0)then
                jhi=2
              else
                jhi=3
              endif

              do jcol=1,jhi

                nonz=nonz+1

                if(nonz.eq.maxnon+1)then
                  write(*,*)' '
                  write(*,*)'NabSpar - Fatal error!'
                  write(*,*)'  Too many entries to store!'
                  write(*,*)'  Number allocated, MAXNON=',maxnon
                elseif(nonz.gt.maxnon+1)then

                else
                  a(nonz)=anab(irow,jcol,nab,ip)
                  ja(nonz)=indx(jcol,jp)
                endif

              enddo

            endif

          enddo
        enddo
      enddo

      ia(neqn+1)=nonz+1

      if(nonz.gt.maxnon)then
        write(*,*)'Number of nonzeros needed, NONZ=',nonz
        stop
      endif

      return
      end
      subroutine prnmat(anab,ip,nabor,np)
c
c***********************************************************************
c
c  PRNMAT prints the matrix entries for a particular node, when the
c  matrix is stored in neigbor format.
c
      integer np
c
      double precision anab(3,3,19,np)
      integer i
      integer ip
      integer j
      integer nab
      integer nabor(19,np)
      integer nod
c
      write(*,*)' '
      write(*,*)'PrNMat:'
      write(*,*)'  Matrix entries for node ',ip
      write(*,*)' '

      do nab=1,19

        nod=nabor(nab,ip)

        if(nod.ne.0)then
          write(*,*)' '
          write(*,*)'Neighbor node ',nod
          write(*,*)' '
          do i=1,3
            write(*,'(1x,3g14.6)')(anab(i,j,nab,ip),j=1,3)
          enddo
        endif

      enddo

      return
      end
      subroutine prspar(a,i1,i2,ia,j1,j2,ja,neqn,nonz)
c
c***********************************************************************
c
c  PRSPAR prints all nonzero entries of rows I1 to I2, columns J1 to 
c  J2 of the matrix, plus the right hand side.
c
      integer neqn
      integer nonz
c
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
c
      write(*,*)' '
      write(*,*)'PrSpar:'
      write(*,*)'  Print rows ',i1,' through ',i2
      write(*,*)'  Columns ',j1,' through ',j2
      write(*,*)' '
      write(*,*)'  I, J, A(I,J)'
      write(*,*)' '
      do i=i1,i2

        write(*,*)' '

        do k=ia(i),ia(i+1)-1

          j=ja(k)
          if(j1.le.j.and.j.le.j2.and.a(k).ne.0.0)then
            write(*,*)i,j,a(k)
          endif
        enddo
      enddo

      return
      end

