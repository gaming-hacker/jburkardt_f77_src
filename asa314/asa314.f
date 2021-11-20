      subroutine i4mat_print ( m, n, a, title )

c*********************************************************************72
c
cc I4MAT_PRINT prints an I4MAT.
c
c  Discussion:
c
c    An I4MAT is an array of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 June 2003
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
c    Input, integer A(M,N), the matrix to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer ihi
      integer ilo
      integer jhi
      integer jlo
      character*(*) title

      ilo = 1
      ihi = m
      jlo = 1
      jhi = n

      call i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

      return
      end
      subroutine i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

c*********************************************************************72
c
cc I4MAT_PRINT_SOME prints some of an I4MAT.
c
c  Discussion:
c
c    An I4MAT is an array of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 November 2003
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 10 )
      integer m
      integer n

      integer a(m,n)
      character*(8) ctemp(incx)
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
      character*(*) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i8)' ) j
        end do

        write ( *, '(''  Col '',10a8)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(i8)' ) a(i,j)

          end do

          write ( *, '(i5,a,10a8)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

      end do

      return
      end
      subroutine invmod ( mat, imat, rmod, cmod, iwk, nrow, ifault )

c*********************************************************************72
c
cc INVMOD inverts a matrix using modulo arithmetic.
c
c  Modified:
c
c    28 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Roger Payne.
c
c  Reference:
c
c    Roger Payne,
c    Inversion of matrices with contents subject to modulo arithmetic,
c    Applied Statistics,
c    Volume 46, Number 2, 1997, pages 295-298.
c
c  Parameters:
c
c    Input/output, integer MAT(NROW*NROW).
c    On input, the matrix to be inverted.
c    On output, the product of the input matrix and IMAT.
c
c    Output, integer IMAT(NROW*NROW), the inverse matrix.  
c    If IFAULT = -1 on output, then IMAT is only a left inverse.
c
c    Input, integer RMOD(NROW), the modulus for values in each row.
c
c    Input, integer CMOD(NROW), the modulus for values in each column.
c
c    Workspace, integer IWK(2*NROW).
c
c    Input, integer NROW, the order of the matrix.
c
c    Output, integer IFAULT, an error flag.
c    0, no error was detected.
c    -1, only a left inverse could be formed.
c    1, the matrix contains elements that are negative, or too large.
c    2, the matrix contains nonzero elements in mixed modulus positions.
c    3, the matrix cannot be inverted.
c
      implicit none

      integer nrow

      integer cmod(nrow)
      integer i
      integer ifault
      integer imat(nrow*nrow)
      integer ir
      integer iwk(2*nrow)
      integer j
      integer k
      integer kir
      integer kjr
      integer mat(nrow*nrow)
      integer n
      integer rmod(nrow)
c
c  Check that elements in 'mixed-moduli' positions are all zero.
c
      n = 0
      do i = 1, nrow
        do j = 1, nrow

          n = n + 1

          if ( ( rmod(i) .ne. cmod(j) ) .and. ( 0 .lt. mat(n) ) ) then
            ifault = 2
            return
          end if

          if ( ( rmod(i) .lt. mat(n) ) .or. ( mat(n) .lt. 0 ) ) then
            ifault = 1
            return
          end if

        end do
      end do

      n = 0
      do i = 1, nrow
        do j = 1, nrow
          n = n + 1
          imat(n) = 0
        end do
      end do
c
c  Sort rows and columns into ascending order of moduli
c
      call msort ( mat, imat, rmod, cmod, iwk, iwk(nrow+1), nrow )
c
c  Complete initialisation of inverse matrix 
c
      do n = 1, nrow * nrow, nrow + 1
        imat(n) = 1
      end do
c
c  Invert the matrix.
c
      do ir = 1, nrow

        kir = ( ir - 1 ) * nrow

        if ( mat(kir+ir) .eq. 0 ) then
c
c  Find a row JR below IR such that K(JR,IR)>0
c
          do kjr = kir + nrow + ir, nrow * nrow, nrow
            if ( 0 .lt. mat(kjr) ) then
              go to 10
            end if
          end do
c
c  Column IR contains all zeros in rows IR or below:
c  look for a row above with zeros to left of column IR 
c  and K(JR,IR)>0
c
          do kjr = ir, kir, nrow
            if ( 0 .lt. mat(kjr) ) then
              do i = kjr - ir + 1, kjr - 1
                if ( 0 .lt. mat(i) ) then
                  ifault = 3
                  return
                end if
              end do
              go to 10
            end if
          end do
c
c  Column IR contains all zeros
c
          go to 30
c
c  Switch row JR with row IR
c
10        continue

          kjr = kjr - ir

          do i = 1, nrow

            k = mat(kir+i)
            mat(kir+i) = mat(kjr+i)
            mat(kjr+i) = k

            k = imat(kir+i)
            imat(kir+i) = imat(kjr+i)
            imat(kjr+i) = k

          end do

        end if
c
c  Find a multiplier N such that N*MAT(IR,IR)=1 mod(P{IR})
c
        k = mat(kir+ir)
        do n = 1, rmod(ir) - 1
          if ( mod ( n * k, rmod(ir) ) .eq. 1 ) then
            go to 20
          end if
        end do
c
c  Multiply row IR by N.
c
20      continue

        if ( 1 .lt. n ) then
          do i = kir + 1, ir * nrow
            mat(i) = mat(i) * n
            imat(i) = imat(i) * n
          end do
        end if
c
c  Subtract MAT(JR,IR) * row IR from each row JR
c
        do kjr = 0, nrow * nrow - 1, nrow
          n = rmod(ir) - mat(kjr+ir)
          if ( ( kjr .ne. kir ) .and. ( n .ne. 0 ) ) then
            do i = 1, nrow
              mat(kjr+i)  = 
     &          mod (  mat(kjr+i) + n *  mat(kir+i), cmod(i) )
              imat(kjr+i) = 
     &          mod ( imat(kjr+i) + n * imat(kir+i), cmod(i) )
            end do
          end if
        end do

30      continue

      end do
c
c  Check inversion was possible - that result has
c  non-zero elements only on diagonal.
c
      ifault = 0
c
c  If we encounter a zero diagonal element, then only a left inverse
c  will be formed.
c
      do n = 1, nrow * nrow, nrow + 1
        if ( mat(n) .eq. 0 ) then
          ifault = -1
        end if
        mat(n) = - mat(n)
      end do

      do n = 1, nrow * nrow
        if ( 0 .lt. mat(n) ) then
          ifault = 3
          return
        end if
      end do

      do n = 1, nrow * nrow, nrow + 1
        mat(n) = -mat(n)
      end do
c
c  Unsort the rows and columns back into their original order.
c
      call musort ( mat, imat, rmod, cmod, iwk, iwk(nrow+1), nrow )

      return
      end
      subroutine msort ( mat, imat, rmod, cmod, rsort, csort, nrow )

c*********************************************************************72
c
cc MSORT sorts matrix rows and columns in ascending order of moduli.
c
c  Modified:
c
c    28 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Roger Payne.
c
c  Reference:
c
c    Roger Payne,
c    Inversion of matrices with contents subject to modulo arithmetic,
c    Applied Statistics,
c    Volume 46, Number 2, 1997, pages 295-298.
c
c  Parameters:
c
c    Input/output, integer MAT(NROW*NROW).
c    On output, the matrix has been sorted.
c
c    Ignoreput, integer IMAT(NROW*NROW).  
c    This quantity is ignored.
c
c    Input/output, integer RMOD(NROW), the modulus for values in each row.
c    On output, these have been rearranged according to the sorting.
c
c    Input/output, integer CMOD(NROW), the modulus for values in each column.
c    On output, these have been rearranged according to the sorting.
c
c    Output, integer RSORT(NROW), the sorted row indices.
c
c    Output, integer CSORT(NROW), the sorted column indices.
c
c    Input, integer NROW, the order of the matrix.
c
      implicit none

      integer nrow

      integer cmod(nrow)
      integer csort(nrow)
      integer i
      integer imat(nrow*nrow)
      integer irc
      integer j
      integer jrc
      integer kirc
      integer kjrc
      integer mat(nrow*nrow)
      integer p
      integer rmod(nrow)
      integer rsort(nrow)
c
c  Initialize row and column addresses.
c
      do i = 1, nrow
        rsort(i) = i
        csort(i) = i
      end do
c
c  Sort the rows.
c
      do irc = 1, nrow
c
c  Find the next row.
c
        jrc = irc
        p = rmod(irc)

        do i = irc + 1, nrow
          if ( rmod(i) .lt. p ) then
            p = rmod(i)
            jrc = i
          end if
        end do

        if ( irc .ne. jrc ) then

          i = rmod(irc)
          rmod(irc) = rmod(jrc)
          rmod(jrc) = i

          i = rsort(irc)
          rsort(irc) = rsort(jrc)
          rsort(jrc) = i
c
c  Switch the rows.
c
          kirc = ( irc - 1 ) * nrow
          kjrc = ( jrc - 1 ) * nrow

          do j = 1, nrow
            i = mat(kirc+j)
            mat(kirc+j) = mat(kjrc+j)
            mat(kjrc+j) = i
          end do

        end if

      end do
c
c  Sort the columns.
c
      do irc = 1, nrow
c
c  Find the next column.
c
        jrc = irc
        p = cmod(irc)

        do i = irc + 1, nrow
          if ( cmod(i) .lt. p ) then
            p = cmod(i)
            jrc = i
          end if
        end do

        if ( irc .ne. jrc ) then

          i = cmod(irc)
          cmod(irc) = cmod(jrc)
          cmod(jrc) = i

          i = csort(irc)
          csort(irc) = csort(jrc)
          csort(jrc) = i
c
c  Switch the columns.
c
          do j = 0, nrow * nrow - 1, nrow
            i = mat(irc+j)
            mat(irc+j) = mat(jrc+j)
            mat(jrc+j) = i
          end do

        end if

      end do

      return
      end
      subroutine musort ( mat, imat, rmod, cmod, rsort, csort, nrow )

c*********************************************************************72
c
cc MUSORT unsorts the inverse matrix rows and columns into the original order.
c
c  Modified:
c
c    28 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Roger Payne.
c
c  Reference:
c
c    Roger Payne,
c    Inversion of matrices with contents subject to modulo arithmetic,
c    Applied Statistics,
c    Volume 46, Number 2, 1997, pages 295-298.
c
c  Parameters:
c
c    Input/output, integer MAT(NROW*NROW).
c    On output, the matrix has been "unsorted".
c
c    Input/output, integer IMAT(NROW*NROW).
c    On output, the matrix has been "unsorted".
c
c    Input/output, integer RMOD(NROW), the modulus for values in each row.
c    On output, these have been restored to their original ordering.
c
c    Input/output, integer CMOD(NROW), the modulus for values in each column.
c    On output, these have been restored to their original ordering.
c
c    Input/output, integer RSORT(NROW), the sorted row indices.
c
c    Input/output, integer CSORT(NROW), the sorted column indices.
c
c    Input, integer NROW, the order of the matrix.
c
      implicit none

      integer nrow

      integer cmod(nrow)
      integer csort(nrow)
      integer i
      integer imat(nrow*nrow)
      integer irc
      integer j
      integer jrc
      integer kirc
      integer kjrc
      integer mat(nrow*nrow)
      integer rmod(nrow)
      integer rsort(nrow)
c
c  Sort rows of inverse (=columns of original)
c
      do irc = 1, nrow
c
c  Find next row
c
        if ( csort(irc) .ne. irc ) then

          do jrc = irc + 1, nrow
            if ( csort(jrc) .eq. irc ) then
              go to 10
            end if
          end do

10        continue

          i = cmod(irc)
          cmod(irc) = cmod(jrc)
          cmod(jrc) = i

          i = csort(irc)
          csort(irc) = csort(jrc)
          csort(jrc) = i
c
c  Switch rows.
c
          kirc = ( irc - 1 ) * nrow
          kjrc = ( jrc - 1 ) * nrow

          do j = 1, nrow
            i = imat(kirc+j)
            imat(kirc+j) = imat(kjrc+j)
            imat(kjrc+j) = i
          end do

        end if

      end do
c
c  Sort the columns of the inverse (= rows of original)
c
      do irc = 1, nrow
c
c  Find the next column.
c
        if ( rsort(irc) .ne. irc ) then

          do jrc = irc + 1, nrow
            if ( rsort(jrc) .eq. irc ) then
              go to 20
            end if
          end do

20        continue

          i = rmod(irc)
          rmod(irc) = rmod(jrc)
          rmod(jrc) = i

          i = rsort(irc)
          rsort(irc) = rsort(jrc)
          rsort(jrc) = i
c
c  Switch the columns of IMAT.
c
          do j = 0, nrow * nrow - 1, nrow
            i = imat(irc+j)
            imat(irc+j) = imat(jrc+j)
            imat(jrc+j) = i
          end do
c
c  Switch the diagonal elements of MAT (others are zero)
c
          kirc = ( irc - 1 ) * nrow + irc
          kjrc = ( jrc - 1 ) * nrow + jrc

          i = mat(kirc)
          mat(kirc) = mat(kjrc)
          mat(kjrc) = i

        end if

      end do

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
