      subroutine dlap_file_print ( n, nelt, isym, irhs, isoln, ia, ja, 
     &  a, rhs, soln, title )

c*********************************************************************72
c
cc DLAP_FILE_PRINT prints a DLAP linear system that was stored in a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, integer NELT, the number of non-zeros stored in A.
c
c    Input, integer ISYM, a flag to indicate symmetric 
c    storage format.
c    * 0, all nonzero entries of the matrix are stored.
c    * 1, the matrix is symmetric, and only the lower triangle of the
c    matrix is stored.
c
c    Input, integer IRHS, is 1 if a right hand side vector 
c    is included.
c
c    Input, integer ISOLN, is 1 if a solution vector is included.
c
c    Input, integer IA(NELT), integer JA(NELT),
c    double precision A(NELT), the DLAP triad matrix description.
c
c    Input, double precision RHS(N), the right hand side vector.
c
c    Input, double precision SOLN(N), the solution to the linear system.
c
c    Input, character * ( * ) TITLE, a title to be printed.
c
      implicit none

      integer n
      integer nelt

      double precision a(nelt)
      integer ia(nelt)
      integer irhs
      integer isoln
      integer isym
      integer ja(nelt)
      double precision rhs(n)
      double precision soln(n)
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      call dlap_header_print ( n, nelt, isym, irhs, isoln )
c
c  Write out the matrix.
c
      call r8sp_print ( n, n, nelt, isym, ia, ja, a, 
     &  '  The sparse matrix' )
c
c  Write the right hand side.
c
      if ( irhs .eq. 1 ) then
        call dlap_rhs_print ( n, rhs )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  No right hand side vector was supplied.'
      end if
c
c  Write the solution.
c
      if ( isoln .eq. 1 ) then
        call dlap_soln_print ( n, soln )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  No solution vector was supplied.'
      end if

      return
      end
      subroutine dlap_file_read ( n_max, nelt_max, n, nelt, isym, irhs, 
     &  isoln, ia, ja, a, rhs, soln, iunit )

c*********************************************************************72
c
cc DLAP_FILE_READ reads in a DLAP matrix contained in a file.
c
c  Discussion:
c
c    This routine reads in a DLAP Triad Format Linear System,
c    including the matrix, right hand side, and solution, if known.
c
c
c    The original version of this program seems to have a minor
c    logical flaw.  If the user requests the solution but not
c    the right hand side, and the file contains both, the original
c    program would not correctly read past the right hand side
c    to get to the solution.  The current version should fix
c    that flaw.
c
c
c    The expected format of the file is as follows.  On the first line
c    are counters and flags: N, NELT, ISYM, IRHS, ISOLN.  N, NELT
c    and ISYM are described below.  IRHS is a flag indicating if
c    the RHS was written out (1 is yes, 0 is  no).  ISOLN is a
c    flag indicating if the SOLN was written out  (1 is yes, 0 is
c    no).  The format for the first line is: 5i10.  Then comes the
c    NELT Triad's IA(I), JA(I) and A(I), I = 1, NELT.  The format
c    for these lines is   :  1X,I5,1X,I5,1X,E16.7.   Then comes
c    RHS(I), I = 1, N, if IRHS = 1.  Then comes SOLN(I), I  = 1,
c    N, if ISOLN = 1.  The format for these lines is: 1X,E16.7.
c
c
c    This routine requires that the  matrix A be stored in the
c    DLAP Triad format.  In this format only the non-zeros  are
c    stored.  They may appear in ANY order.  The user supplies
c    three arrays of length NELT, where NELT is the number of
c    non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
c    each non-zero the user puts the row and column index of that
c    matrix element in the IA and JA arrays.  The value of the
c    non-zero matrix element is placed in the corresponding
c    location of the A array.   This is an extremely easy data
c    structure to generate.  On the other hand it is not too
c    efficient on vector computers for the iterative solution of
c    linear systems.  Hence,  DLAP changes this input data
c    structure to the DLAP Column format for the iteration (but
c    does not change it back).
c
c    Here is an example of the DLAP Triad storage format for a
c    5x5 Matrix.  Recall that the entries may appear in any order.
c
c        5x5 Matrix       DLAP Triad format for 5x5 matrix on left.
c                              1  2  3  4  5  6  7  8  9 10 11
c    |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
c    |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
c    | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
c    | 0  0  0 44  0|
c    |51  0 53  0 55|
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer N_MAX, the maximum value of N for which storage
c    has been allocated.
c
c    Input, integer NELT_MAX, the maximum value of NELT for which
c    storage has been allocated.
c
c    Output, integer N, the order of the matrix.
c
c    Output, integer NELT, the number of non-zeros stored in A.
c
c    Output, integer ISYM, a flag to indicate symmetric storage 
c    format.
c    * 0, all nonzero entries of the matrix are stored.
c    * 1, the matrix is symmetric, and only the lower triangle of the
c    matrix is stored.
c
c    Output, integer IRHS, is 1 if a right hand side vector is 
c    included.
c
c    Output, integer ISOLN, is 1 if a solution vector is included.
c
c    Output, integer IA(NELT), integer JA(NELT),
c    double precision A(NELT).  On output these arrays hold the matrix A in the
c    DLAP Triad format.
c
c    Output, double precision RHS(N), the right hand side vector.
c
c    Output, double precision SOLN(N), the solution to the linear system, 
c    if present.
c
c    Input, integer IUNIT, the FORTRAN device unit from which the
c    matrix is to be read.
c
      implicit none

      integer n_max
      integer nelt_max

      double precision a(nelt_max)
      integer i
      integer ia(nelt_max)
      integer ios
      integer irhs
      integer isoln
      integer isym
      integer iunit
      integer ja(nelt_max)
      integer n
      integer nelt
      double precision rhs(n_max)
      double precision soln(n_max)
c
c  Read the header line.
c
      call dlap_header_read ( iunit, n, nelt, isym, irhs, isoln, ios )

      if ( ios .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DLAP_FILE_READ - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Error while reading header line of DLAP file.'
        return
      end if

      if ( nelt_max .lt. nelt ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DLAP_FILE_READ - Fatal error!'
        write ( *, '(a)' ) '  NELT_MAX .lt. NELT.'
        write ( *, '(a,i8)' ) '  NELT_MAX = ', nelt_max
        write ( *, '(a,i8)' ) '  NELT     = ', nelt
        stop
      end if
c
c  Read the nonzero matrix entries in DLAP Triad format.
c
      do i = 1, nelt

        read ( iunit, '(1x,i5,1x,i5,1x,e16.7)', iostat = ios ) 
     &    ia(i), ja(i), a(i)

        if ( ios .ne. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DLAP_FILE_READ - Fatal error!'
          write ( *, '(a,i8)' ) 
     &      '  Error while reading matrix element ', i+1
          return
        end if

      end do
c
c  If a value for RHS is available in the file, read it in.
c
      if ( irhs .eq. 1 ) then

        if ( n_max .lt. n ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DLAP_FILE_READ - Fatal error!'
          write ( *, '(a)' ) '  N_MAX .lt. N.'
          write ( *, '(a,i8)' ) '  N_MAX = ', n_max
          write ( *, '(a,i8)' ) '  N     = ', n
          stop
        end if

        call dlap_rhs_read ( iunit, n, rhs, ios )

        if ( ios .ne. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DLAP_FILE_READ - Fatal error!'
          write ( *, '(a)' ) '  Error while reading RHS from DLAP file.'
          return
        end if

      end if
c
c  If a value of SOLN is available in the file, read it.
c
      if ( isoln .eq. 1 ) then

        if ( n_max .lt. n ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DLAP_FILE_READ - Fatal error!'
          write ( *, '(a)' ) '  N_MAX .lt. N.'
          write ( *, '(a,i8)' ) '  N_MAX = ', n_max
          write ( *, '(a,i8)' ) '  N     = ', n
          stop
        end if

        call dlap_soln_read ( iunit, n, soln, ios )

        if ( ios .ne. 0 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'DLAP_FILE_READ - Fatal error!'
          write ( *, '(a)' ) 
     &      '  Error while reading SOLN from DLAP file.'
          return
        end if

      end if

      return
      end
      subroutine dlap_file_write ( n, nelt, isym, irhs, isoln, ia, ja, 
     &  a, rhs, soln, iunit )

c*********************************************************************72
c
cc DLAP_FILE_WRITE writes out DLAP Triad Format Linear System.
c
c  Discussion:
c
c    This routine writes out a DLAP Triad Format Linear System.
c    including the matrix, right hand side, and solution to the
c    system, if known.
c
c
c    The format for the output is as follows.  On  the first line
c    are counters and flags:
c
c      N, NELT, ISYM, IRHS, ISOLN.
c
c    N, NELT and ISYM are described below.  IRHS is a flag indicating if
c    the RHS was written out (1 is  yes, 0 is  no).  ISOLN  is a
c    flag indicating if the SOLN was written out  (1 is yes, 0 is
c    no).  The format for the first line is: 5i10.  Then comes the
c    NELT Triad's IA(I), JA(I) and A(I), I = 1, NELT.  The format
c    for  these lines is   :  1X,I5,1X,I5,1X,E16.7.   Then comes
c    RHS(I), I = 1, N, if IRHS = 1.  Then comes SOLN(I), I  = 1,
c    N, if ISOLN = 1.  The format for these lines is: 1X,E16.7.
c
c
c    This routine requires that the  matrix A be stored in the
c    DLAP Triad format.  In this format only the non-zeros  are
c    stored.  They may appear in ANY order.  The user supplies
c    three arrays of length NELT, where NELT is the number of
c    non-zeros in the matrix: (IA(NELT), JA(NELT), A(NELT)).  For
c    each non-zero the user puts the row and column index of that
c    matrix element in the IA and JA arrays.  The value of the
c    non-zero matrix element is placed in the corresponding
c    location of the A array.   This is an extremely easy data
c    structure to generate.  On the other hand it is not too
c    efficient on vector computers for the iterative solution of
c    linear systems.  Hence,  DLAP changes this input data
c    structure to the DLAP Column format for the iteration (but
c    does not change it back).
c
c    Here is an example of the DLAP Triad storage format for a
c    5x5 Matrix.  Recall that the entries may appear in any order.
c
c        5x5 Matrix       DLAP Triad format for 5x5 matrix on left.
c                              1  2  3  4  5  6  7  8  9 10 11
c    |11 12  0  0 15|   A: 51 12 11 33 15 53 55 22 35 44 21
c    |21 22  0  0  0|  IA:  5  1  1  3  1  5  5  2  3  4  2
c    | 0  0 33  0 35|  JA:  1  2  1  3  5  3  5  2  5  4  1
c    | 0  0  0 44  0|
c    |51  0 53  0 55|
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, integer NELT, the number of non-zeros stored in A.
c
c    Input, integer ISYM, indicates symmetric storage format.
c    * 0, all nonzero entries of the matrix are stored.
c    * 1, the matrix is symmetric, and only the lower triangle of
c      the matrix is stored.
c
c    Input, integer IRHS, is 1 if a right hand side vector 
c    is included.
c
c    Input, integer ISOLN, is 1 if a solution vector is included.
c
c    Input, integer IA(NELT), integer JA(NELT),
c    double precision A(NELT), the DLAP triad matrix description.
c
c    Input, double precision RHS(N), the right hand side vector.  This array is
c    accessed if JOB is set to print it out.
c
c    Input, double precision SOLN(N), the solution to the linear system, 
c    if known.  This array is accessed if and only if JOB is set to print it.
c
c    Input, integer IUNIT, the FORTRAN device unit number to which
c    the matrix information is to be written.
c
      implicit none

      integer n
      integer nelt

      double precision a(nelt)
      integer i
      integer ia(nelt)
      integer irhs
      integer isoln
      integer isym
      integer iunit
      integer ja(nelt)
      double precision rhs(n)
      double precision soln(n)

      call dlap_header_write ( iunit, n, nelt, isym, irhs, isoln )
c
c  Write the matrix non-zeros in Triad format.
c
      do i = 1, nelt
        write ( iunit, '(1x,i5,1x,i5,1x,e16.7)' ) ia(i), ja(i), a(i)
      end do
c
c  Write the right hand side.
c
      if ( irhs .eq. 1 ) then
        call dlap_rhs_write ( iunit, n, rhs )
      end if
c
c  Write the solution.
c
      if ( isoln .eq. 1 ) then
        call dlap_soln_write ( iunit, n, soln )
      end if

      return
      end
      subroutine dlap_header_print ( n, nelt, isym, irhs, isoln )

c*********************************************************************72
c
cc DLAP_HEADER_PRINT prints the header line of a DLAP file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, integer NELT, the number of non-zeros stored in A.
c
c    Input, integer ISYM, indicates symmetric storage format.
c    * 0, all nonzero entries of the matrix are stored.
c    * 1, the matrix is symmetric, and only the lower triangle of
c      the matrix is stored.
c
c    Input, integer IRHS, is 1 if a right hand side vector 
c    is included.
c
c    Input, integer ISOLN, is 1 if a solution vector is included.
c
      implicit none

      integer isoln
      integer isym
      integer irhs
      integer n
      integer nelt

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  DLAP Sparse Matrix File Header:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  N,     the matrix order =                ', n
      write ( *, '(a,i8)' ) 
     &  '  NELT,  the number of nonzeros stored =   ', nelt
      write ( *, '(a,i8)' ) 
     &  '  ISYM,  1 if symmetric storage used =     ', isym
      write ( *, '(a,i8)' ) 
     &  '  IRHS,  1 if a right hand side included = ', irhs
      write ( *, '(a,i8)' ) 
     &  '  ISOLN, 1 if a solution vector included = ', isoln

      return
      end
      subroutine dlap_header_read ( iunit, n, nelt, isym, irhs, isoln, 
     &  ios )

c*********************************************************************72
c
cc DLAP_HEADER_READ reads the header line from a DLAP file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer IUNIT, the FORTRAN device unit number to which
c    the matrix information is to be written.
c
c    Output, integer N, the order of the matrix.
c
c    Output, integer NELT, the number of non-zeros stored in A.
c
c    Output, integer ISYM, indicates symmetric storage format.
c    * 0, all nonzero entries of the matrix are stored.
c    * 1, the matrix is symmetric, and only the lower triangle of
c      the matrix is stored.
c
c    Output, integer IRHS, is 1 if a right hand side vector
c    is included.
c
c    Output, integer ISOLN, is 1 if a solution vector is included.
c
c    Output, integer IOS, the I/O status variable, which is 0 if
c    no I/O error occurred.
c
      implicit none

      integer ios
      integer isoln
      integer isym
      integer irhs
      integer iunit
      integer n
      integer nelt

      read ( iunit, *, iostat = ios ) n, nelt, isym, irhs, isoln

      return
      end
      subroutine dlap_header_write ( iunit, n, nelt, isym, irhs, 
     &  isoln )

c*********************************************************************72
c
cc DLAP_HEADER_WRITE writes the header line to a DLAP file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer IUNIT, the FORTRAN device unit number to which
c    the matrix information is to be written.
c
c    Input, integer N, the order of the matrix.
c
c    Input, integer NELT, the number of non-zeros stored in A.
c
c    Input, integer ISYM, indicates symmetric storage format.
c    * 0, all nonzero entries of the matrix are stored.
c    * 1, the matrix is symmetric, and only the lower triangle of
c      the matrix is stored.
c
c    Input, integer IRHS, is 1 if a right hand side is included.
c
c    Input, integer ISOLN, is 1 if a solution vector is included.
c
      implicit none

      integer isoln
      integer isym
      integer irhs
      integer iunit
      integer n
      integer nelt

      write ( iunit, '(5i10)' ) n, nelt, isym, irhs, isoln

      return
      end
      subroutine dlap_rhs_print ( n, rhs )

c*********************************************************************72
c
cc DLAP_RHS_PRINT prints the right hand side vector from a DLAP file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision RHS(N), the right hand side vector to be written.
c
      implicit none

      integer n

      double precision rhs(n)

      call r8vec_print ( n, rhs, '  DLAP right hand side vector:' )

      return
      end
      subroutine dlap_rhs_read ( iunit, n, rhs, ios )

c*********************************************************************72
c
cc DLAP_RHS_READ reads the right hand side vector from a DLAP file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer IUNIT, the FORTRAN device unit number to which
c    the matrix information is to be written.
c
c    Input, integer N, the order of the matrix.
c
c    Output, double precision RHS(N), the right hand side vector.
c
c    Output, integer IOS, the I/O status variable, which is 0 if
c    no I/O error occurred.
c
      implicit none

      integer n

      integer ios
      integer iunit
      double precision rhs(n)

      read ( iunit, *, iostat = ios ) rhs(1:n)

      return
      end
      subroutine dlap_rhs_write ( iunit, n, rhs )

c*********************************************************************72
c
cc DLAP_RHS_WRITE writes a right hand side vector to a DLAP file.
c
c  Modified:
c
c    31 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer IUNIT, the FORTRAN device unit number to which
c    the matrix information is to be written.
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision RHS(N), the right hand side vector to be written.
c
      implicit none

      integer n

      integer iunit
      double precision rhs(n)

      write ( iunit, '(1x,e16.7)' ) rhs(1:n)

      return
      end
      subroutine dlap_soln_print ( n, soln )

c*********************************************************************72
c
cc DLAP_SOLN_PRINT prints the solution vector from a DLAP file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision SOLN(N), the solution vector to be written.
c
      implicit none

      integer n

      double precision soln(n)

      call r8vec_print ( n, soln, '  DLAP solution vector:' )

      return
      end
      subroutine dlap_soln_read ( iunit, n, soln, ios )

c*********************************************************************72
c
cc DLAP_SOLN_READ reads the solution vector from a DLAP file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer IUNIT, the FORTRAN device unit number to which
c    the matrix information is to be written.
c
c    Input, integer N, the order of the matrix.
c
c    Output, double precision SOLN(N), the solution vector.
c
c    Output, integer IOS, the I/O status variable, which is 0 if
c    no I/O error occurred.
c
      implicit none

      integer n

      integer ios
      integer iunit
      double precision soln(n)

      read ( iunit, *, iostat = ios ) soln(1:n)

      return
      end
      subroutine dlap_soln_write ( iunit, n, soln )

c*********************************************************************72
c
cc DLAP_SOLN_WRITE writes a solution vector to a DLAP file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Mark Seager,
c    A SLAP for the Masses,
c    Lawrence Livermore National Laboratory,
c    Technical Report UCRL-100267, December 1988.
c
c  Parameters:
c
c    Input, integer IUNIT, the FORTRAN device unit number to which
c    the matrix information is to be written.
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision SOLN(N), the solution vector to be written.
c
      implicit none

      integer n

      integer iunit
      double precision soln(n)

      write ( iunit, '(2x,e16.7)' ) soln(1:n)

      return
      end
      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      subroutine r8sp_print ( m, n, nz_num, isym, row, col, a, title )

c*********************************************************************72
c
cc R8SP_PRINT prints an R8SP matrix.
c
c  Discussion:
c
c    This version of R8SP_PRINT has been specifically modified to allow,
c    and correctly handle, the case in which a single matrix location
c    A(I,J) is referenced more than once by the sparse matrix structure.
c    In such cases, the routine prints out the sum of all the values.
c
c    The R8SP storage format stores the row, column and value of each nonzero
c    entry of a sparse matrix.
c
c    It is possible that a pair of indices (I,J) may occur more than
c    once.  Presumably, in this case, the intent is that the actual value
c    of A(I,J) is the sum of all such entries.  This is not a good thing
c    to do, but I seem to have come across this in MATLAB.
c
c    The R8SP format is used by CSPARSE ("sparse triplet"), DLAP/SLAP 
c    ("nonsymmetric SLAP triad"), by MATLAB, and by SPARSEKIT ("COO" format).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of 
c    the matrix.
c
c    Input, integer NZ_NUM, the number of nonzero elements in 
c    the matrix.
c
c    Input, integer ISYM, is 0 if the matrix is not symmetric, 
c    and 1 if the matrix is symmetric.  The symmetric case only makes sense
c    if the matrix is also square, that is, M = N.  In this case, only
c    the nonzeroes on the diagonal and in the lower triangle are stored.
c
c    Input, integer ROW(NZ_NUM), COL(NZ_NUM), the row and column 
c    indices of the nonzero elements.
c
c    Input, double precision A(NZ_NUM), the nonzero elements of the matrix.
c
c    Input, character * ( * ) TITLE, a title.
c 
      implicit none

      integer nz_num

      double precision a(nz_num)
      integer col(nz_num)
      integer isym
      integer m
      integer n
      integer row(nz_num)
      character * ( * ) title

      call r8sp_print_some ( m, n, nz_num, isym, row, col, a, 1, 1, 
     &  m, n, title )

      return
      end
      subroutine r8sp_print_some ( m, n, nz_num, isym, row, col, a, ilo, 
     &  jlo, ihi, jhi, title )

c*********************************************************************72
c
cc R8SP_PRINT_SOME prints some of an R8SP matrix.
c
c  Discussion:
c
c    This version of R8SP_PRINT_SOME has been specifically modified to allow,
c    and correctly handle, the case in which a single matrix location
c    A(I,J) is referenced more than once by the sparse matrix structure.
c    In such cases, the routine prints out the sum of all the values.
c
c    The R8SP storage format stores the row, column and value of each nonzero
c    entry of a sparse matrix.
c
c    It is possible that a pair of indices (I,J) may occur more than
c    once.  Presumably, in this case, the intent is that the actual value
c    of A(I,J) is the sum of all such entries.  This is not a good thing
c    to do, but I seem to have come across this in MATLAB.
c
c    The R8SP format is used by CSPARSE ("sparse triplet"), DLAP/SLAP 
c    ("nonsymmetric SLAP triad"), by MATLAB, and by SPARSEKIT ("COO" format).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    03 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns 
c    of the matrix.
c
c    Input, integer NZ_NUM, the number of nonzero elements
c    in the matrix.
c
c    Input, integer ISYM, is 0 if the matrix is not symmetric, 
c    and 1 if the matrix is symmetric.  The symmetric case only makes sense
c    if the matrix is also square, that is, M = N.  In this case, only
c    the nonzeroes on the diagonal and in the lower triangle are stored.
c
c    Input, integer ROW(NZ_NUM), COL(NZ_NUM), the row and column
c    indices of the nonzero elements.
c
c    Input, double precision A(NZ_NUM), the nonzero elements of the matrix.
c
c    Input, integer ILO, JLO, IHI, JHI, the first row and
c    column, and the last row and column to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer nz_num

      double precision a(nz_num)
      double precision aij(incx)
      integer col(nz_num)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer isym
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      integer k
      integer m
      integer n
      logical nonzero
      integer row(nz_num)
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
c
c  Print the columns of the matrix, in strips of 5.
c
      do j2lo = jlo, jhi, incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '
        write ( *, '(''  Col:  '',5(i7,7x))' ) ( j, j = j2lo, j2hi )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) '  ---'
c
c  Determine the range of the rows in this strip.
c
        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi
c
c  Print out (up to) 5 entries in row I, that lie in the current strip.
c
          nonzero = .false.
          aij(1:inc) = 0.0D+00
c
c  Is matrix entry K actually the value of A(I,J), with J2LO .le. J .le. J2HI?
c  Because MATLAB seems to allow for multiple (I,J,A) entries, we have
c  to sum up what we find.
c 
          do k = 1, nz_num

            if ( i .eq. row(k) .and. 
     &           j2lo .le. col(k) .and. 
     &           col(k) .le. j2hi ) then 

              j2 = col(k) - j2lo + 1

              if ( a(k) .ne. 0.0D+00 ) then
                nonzero = .true.
                aij(j2) = aij(j2) + a(k)
              end if

            else if ( isym .eq. 1 .and. 
     &                m .eq. n .and.
     &                i .eq. col(k) .and.
     &                j2lo .le. row(k) .and.
     &                row(k) .le. j2hi ) then

              j2 = row(k) - j2lo + 1

              if ( a(k) .ne. 0.0D+00 ) then
                nonzero = .true.
                aij(j2) = aij(j2) + a(k)
              end if

            end if

          end do

          if ( nonzero ) then
            write ( *, '(i5,1x,5g14.6)' ) i, aij(1:inc)
          end if

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
c    An R8VEC is a vector of R8's.
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
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do

      return
      end
      subroutine r8vec_print_part ( n, a, max_print, title )

c*********************************************************************72
c
cc R8VEC_PRINT_PART prints "part" of an R8VEC.
c
c  Discussion:
c
c    The user specifies MAX_PRINT, the maximum number of lines to print.
c
c    If N, the size of the vector, is no more than MAX_PRINT, then
c    the entire vector is printed, one entry per line.
c
c    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
c    followed by a line of periods suggesting an omission,
c    and the last entry.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 June 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, integer MAX_PRINT, the maximum number of lines to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      integer max_print
      character * ( * ) title

      if ( max_print .le. 0 ) then
        return
      end if

      if ( n .le. 0 ) then
        return
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) title
      write ( *, '(a)' ) ' '

      if ( n .le. max_print ) then

        do i = 1, n
          write ( *, '(2x,i8,a1,1x,g14.6)' ) i, ':', a(i)
        end do

      else if ( 3 .le. max_print ) then

        do i = 1, max_print - 2
          write ( *, '(2x,i8,a1,1x,g14.6)' ) i, ':', a(i)
        end do

        write ( *, '(a)' ) '  ........  ..............'
        i = n

        write ( *, '(2x,i8,a1,1x,g14.6)' ) i, ':', a(i)

      else

        do i = 1, max_print - 1
          write ( *, '(2x,i8,a1,1x,g14.6)' ) i, ':', a(i)
        end do

        i = max_print

        write ( *, '(2x,i8,a1,1x,g14.6,a)' )
     &    i, ':', a(i), '...more entries...'

      end if

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
