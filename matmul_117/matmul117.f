!  matmul.f  28 August 1999
!
      program matmul
!
!***********************************************************************
!
!! MATMUL is the main program for a matrix multiplication performance test.
!
!
!  MATMUL is an interactive FORTRAN program that sets up, carries
!  out, and times a matrix multiplication.  MATMUL can use several
!  different algorithms and matrix sizes, and can be run on many
!  different computers.
!
!  Author:
!
!    John Burkardt
!
!***********************************************************************
!
!  Making a version for a given machine:
!
!  1) Modify MATMUL_CPU_TIMER
!
!    MATMUL_CPU_TIMER is a routine to compute the current reading of
!    the CPU timer in seconds.  Several sets of appropiate code
!    are listed in the routine, depending on the machine and compiler
!    used.  Uncomment the one appropriate for your machine, or
!    add a new one if necessary.
!
!  2) Set the value of "MACHINE" to be the name of your machine.
!
!    This occurs in routine INIT.
!
!  3) Set the maximum problem size, LENA.
!
!     The value of LENA should be adjusted to the memory available
!     on your machine.  MATMUL needs three real matrices of size
!     LENA by LENA, and three integer, three double precision,
!     and three complex.
!
!     On the Cray, six real matrices are needed, rather than three.
!
!     My choices for LENA have been:
!
!       65 for Apple Macintosh
!       150 for Apple PowerMac
!       513 for Cray YMP or Cray C90.
!       513 for DEC Alpha.
!       100 for DECstation/ULTRix.
!       65 for IBM PC
!       300 for SGI/IRIS.
!       300 for SUN.
!       300 for VAX/VMS or VECTOR/VAX.
!
!    LENA is defined only once, in a parameter statement in the
!    main program.
!
!  4) Make some special routines available for special machines:
!
!     For the Cray,
!       uncomment the calls to
!         MXMA
!         SAXPY
!         SDOT
!         SGEMM
!         SGEMMS
!         TIMEF.
!       uncomment the declaration of the array WORK in routine USGEMMS.
!
!     For the SGI/IRIS,
!       uncomment the calls to
!         SAXPY
!         SDOT
!         SECNDS
!         SGEMM.
!
!***********************************************************************
!
!  Compilation Instructions:
!
!  On the Apple Macintosh and PowerMac:
!
!    Not a command line compilation.
!
!  On the Cray YMP or Cray C90:
!
!    cf77 matmul.f
!
!  On the DEC Alpha:
!
!    f77 -fast matmul.f
!
!  On the DECstation:
!
!    f77 matmul.f
!
!  On the IBM PC, using Microsoft FORTRAN version 5.10:
!
!    Split the source code into the following chunks:
!
!      MATMUL1.FOR - Main program.
!      MATMUL2.FOR - CAPCHR to DOCOM.
!      MATMUL3.FOR - GETN   to IUJK.
!      MATMUL4.FOR - JIK    to MULT2.
!      MATMUL5.FOR - NIJK   to TDOT.
!      MATMUL6.FOR - TERBLA to UTGEMM.
!
!    fl/c /Gt matmul1.for
!    fl/c /Gt matmul2.for
!    fl/c /Gt matmul3.for
!    fl/c /Gt matmul4.for
!    fl/c /Gt matmul5.for
!    fl/c /Gt matmul6.for
!
!    link matmul1+matmul2+matmul3+matmul4+matmul5+matmul6
!
!  On the SGI/IRIS:
!
!    f77 -mp matmul.f -lblas -lI77_mp
!
!  On the Next with the Absoft FORTRAN compiler:
!
!    /LocalApps/f77 -s matmul.f
!
!  On the SUN:
!
!    f77 -fast -cg92 matmul.f
!
!  On the VAX/VMS:
!
!    FORTRAN/NOLIST MATMUL.F
!    LINK/NOMAP MATMUL.OBJ
!
      integer lena
      parameter ( lena = 300 )
!
      character*20 command
      logical cshow
      logical fshow
      integer ido
      integer ierror
      integer itemp
      integer lchar
      integer lda
      integer lenc
      integer s_length
      logical s_eqi
      character*7 lingo
      logical lnshow
      logical lshow
      character*10 machine
      logical mshow
      integer n
      integer nhi
      integer ninc
      integer nlo
      integer nmult
      logical noshow
      integer nrep
      logical nrshow
      logical nshow
      character*6 order
      character*82 output
      logical tshow
!
!  Initialize the data.
!
      call init ( command, cshow, fshow, lda, lena, lingo, lnshow, 
     &  lshow, machine, mshow, n, nhi, ninc, nlo, nmult, noshow,
     &  nrep, nrshow, nshow, order, tshow )
!
!  Say hello to the user.
!
      call hello ( lena, machine )

10    continue
!
!  Print the prompt.
!
      write ( *, * ) ' '
      write ( *, * ) 'Command?  (Type H for help)'
!
!  Read the command.
!
      read ( *, '(a)', end = 40 ) command
!
!  Capitalize the command.
!
      call s_cap ( command )
!
!  Remove all blanks to make interpretation easier.
!
      call chrdb1 ( command )

      ierror = 0
 
20    continue
!
!  Command "A" means abort the run.
!
      if ( s_eqi ( command(1:1), 'A' ) ) then
 
        write ( *, * ) ' '
        write ( *, * ) 'MATMUL is stopping now.'
        write ( *, * ) ' '
 
        stop
!
!  Command "H" means print out the help message.
!
      else if ( s_eqi ( command(1:1), 'H' ) ) then
 
        call help
!
!  Command "LDA = " means the user wants to set lda.
!
      else if ( s_eqi ( command(1:4), 'LDA=' ) ) then
 
        call chrcti ( command(5:), itemp, ierror, lchar )
 
        if ( ierror .ne. 0 ) then

          write ( *, * ) ' '
          write ( *, * ) 'I did not understand your definition of LDA.'

        else if ( itemp .le. 0 ) then

          write ( *, * ) ' '
          write ( *, * ) 'The assignment of LDA was not acceptable!'
          write ( *, * ) 'LDA must be positive.'

        else if ( itemp .gt. lena ) then

          write ( *, * ) ' '
          write ( *, * ) 'The assignment of LDA was not acceptable!'
          write ( *, * ) 'LDA must be no greater than ', lena

        else

          lda = itemp
          write ( *, * ) ' '
          write ( *, * ) 'LDA has been set to ', lda

        end if
 
        if ( lda .lt. n ) then
          n = lda
          write ( *, * ) ' '
          write ( *, * ) 'Note: Since N must be no greater than LDA,'
          write ( *, * ) 'MATMUL has decreased the value of N.'
          write ( *, * ) 'N has been set to ', n
        end if
!
!  Command "M" means the user wants the multiplication to be carried out.
!
      else if ( s_eqi ( command(1:1), 'M' ) ) then
!
!  Carry out multiplication for one, or many values of N.
!
        n = nlo
 
30      continue
 
        call mult ( cshow, fshow, lda, lingo, lnshow, lshow, 
     &    machine, mshow, n, noshow, nrep, nrshow, nshow, order, 
     &    output, tshow )

        call nstep ( ido, n, nhi, ninc, nlo, nmult )

        if ( ido .eq. 1 ) then
          go to 30
        end if
 
        write ( *, * ) ' '
        write ( *, * ) 'The matrix multiplication has been carried out.'
!
!  Command "N=" means the user wants to set the matrix size N.
!
      else if ( s_eqi ( command(1:2), 'N=' ) ) then
 
        call getn ( command(3:), ierror, lda, lena, n, nhi, ninc, nlo, 
     &    nmult )
!
!  Command "NOSHOW" means the user wants to turn off the display of all
!  quantities.
!
      else if ( s_eqi ( command, 'NOSHOW' ) ) then
 
        command = 'NOSHOW=ALL'
 
        call getsho ( command(8:), cshow, fshow, lnshow,
     &    lshow, .false., mshow, noshow, nrshow, nshow, tshow )
!
!  COMMAND "NOSHOW=" means the user wants to turn off the display of a
!  particular quantity.
!
      else if ( s_eqi ( command(1:7), 'NOSHOW=' ) ) then
 
        call getsho ( command(8:), cshow, fshow, lnshow, lshow,
     &    .false., mshow, noshow, nrshow, nshow, tshow )
!
!  Command "NREP=" sets the number of repetitions.
!
      else if ( s_eqi ( command(1:5), 'NREP=' ) ) then
 
        call chrcti( command(6:), nrep, ierror, lchar )
        write ( *, * ) 'The repetition factor is now NREP = ', nrep
 
        if ( nrep .eq. 1 ) then
          nrshow = .false.
        else
          nrshow = .true.
        end if
!
!  Command "ORDER=" means the user wants to set the method.
!
      else if ( s_eqi ( command(1:6), 'ORDER=' ) ) then
 
        call get_order ( command(7:), ierror, machine, order )
!
!  Command "Q" means the user wants to quit.
!
      else if ( s_eqi ( command(1:1), 'Q' ) ) then
 
        write ( *, * ) ' '
        write ( *, * ) 'Type "Y" to confirm that you want to quit.'

        read ( *, '(a)', end = 20 ) command
        call s_cap ( command )

        if ( s_eqi ( command(1:1), 'Y' ) ) then
          command = 'abort'
        end if
 
        go to 20
!
!  Command "P" means the user wants to print out the current settings.
!
      else if ( s_eqi ( command(1:1), 'P' ) ) then
 
        call printr ( lda, lena, lingo, n, nhi, ninc,
     &    nlo, nmult, nrep, order )
 
        write ( *, * ) ' '
        write ( *, * ) 'Valid choices for the order are:'
        write ( *, * ) ' '
        write ( *, * ) 'ALL, CIJK, DIJK, NIJK, UIJK, IUJK, IJUK,'
        write ( *, * ) 'IJK, IKJ, JIK, JKI, KIJ, KJI, LIJK'
        write ( *, * ) 'TAXPYC, TAXPYR, TDOT, TGEMM'
 
        if ( s_eqi ( machine(1:4), 'CRAY') ) then
          write ( *, * ) 'MIJK, MXMA, NIJK46, SAXPYC, SAXPYR, SDOT, '
          write ( *, * ) 'SGEMM, SGEMMS, SIJK.'
        else if ( s_eqi ( machine, 'SGI/IRIS' ) ) then
          write ( *, * ) 'MKJI, SAXPYC, SAXPYR, SDOT, SGEMM.'
        end if
!
!  Command "SHOW" means the user wants all items to be displayed.
!
      else if ( s_eqi ( command, 'SHOW' ) ) then
 
        command = 'SHOW=ALL'
 
        call getsho ( command(6:), cshow, fshow, lnshow, lshow,
     &    .true., mshow, noshow, nrshow, nshow, tshow )
!
!  Command "SHOW=" means the user wants a particular item displayed.
!
      else if ( s_eqi(command(1:5), 'SHOW=' ) ) then
 
        call getsho ( command(6:), cshow, fshow, lnshow, lshow,
     &    .true., mshow, noshow, nrshow, nshow, tshow )
!
!  The user's input did not match any acceptable command.
!
      else

        lenc = s_length ( command )

        if ( lenc .gt. 0 ) then
          write ( *, * ) ' '
          write ( *, * ) 'Your command was not recognized.'
          write ( *, * ) 'You typed ' // command(1:lenc)
          write ( *, * ) 'Type HELP for a list of commands.'
        end if

      end if
 
      go to 10
!
!  We jump here on certain input errors.
!
40    continue

      command = 'ABORT'
      go to 20
      end
      subroutine c_cap ( c )
!
!***********************************************************************
!
!! C_CAP capitalizes a single character.
!
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character*1 C, the character to capitalize.
!
      character*1 c
      integer itemp
!
      itemp = ichar ( c )

      if ( 97 .le. itemp .and. itemp .le. 122 ) then
        c = char ( itemp - 32 )
      end if

      return
      end
      subroutine chrchp ( string, ilo, ihi )
!
!***********************************************************************
!
!! CHRCHP "chops out" a portion of a string, and closes up the hole.
!
!
!  Modified:
!
!    06 July 1998
!
!  Example:
!
!    STRING = 'Fred is not a jerk!'
!
!    call chrchp ( STRING, 9, 12 )
!
!    STRING = 'Fred is a jerk!    '
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character*(*) STRING, the string to be transformed.
!
!    Input, integer ILO, IHI, the locations of the first and last
!    characters to be removed.  ILO must be at least 1, IHI must
!    be at least ILO, and no more than the length of the string.
!
      integer ihi
      integer ihi2
      integer ilo
      integer ilo2
      integer lens
      character*(*) string
!
      lens = len ( string )

      ilo2 = max ( ilo, 1 )
      ihi2 = min ( ihi, lens )

      if ( ilo2 .gt. ihi2 ) then
        return
      end if

      string(ilo2:lens+ilo2-ihi2-1) = string(ihi2+1:lens)
      string(lens+ilo2-ihi2:lens) = ' '

      return
      end
      subroutine chrcti ( string, intval, ierror, lchar )
!
!***********************************************************************
!
!! CHRCTI reads an integer from a string.
!
!
!  CHRCTI will read as many characters as possible until it reaches
!  the end of the STRING, or encounters a character which cannot be
!  part of the number.
!
!  Legal input is
!
!    blanks,
!    initial sign,
!    blanks,
!    integer part,
!    blanks,
!    final comma or semicolon,
!
!  with most quantities optional.
!
!  Examples:
!
!    STRING            INTVAL
!
!    '1'               1
!    '     1   '       1
!    '1A'              1
!    '12,34,56'        12
!    '  34 7'          34
!    '-1E2ABCD'        -100
!    '-1X2ABCD'        -1
!    ' 2E-1'           0
!    '23.45'           23
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character*(*) STRING, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal integer.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, integer INTVAL, the integer read from the string.
!
!    Output, integer IERROR, error flag.
!    0 if no errors,
!    Value of IHAVE when error occurred otherwise.
!
!    Output, integer LCHAR, number of characters read from
!    STRING to form the number.
!
      character*1 chrtmp
      integer ierror
      integer ihave
      integer intval
      integer isgn
      integer iterm
      integer itop
      integer lchar
      integer nchar
      integer ndig
      character*(*) string
!
      nchar = len ( string )

      ierror = 0
      intval = 0
      lchar = -1
      isgn = 1
      itop = 0
      ihave = 1
      iterm = 0

10    continue

      lchar = lchar + 1
      chrtmp = string(lchar+1:lchar+1)
!
!  Blank.
!
      if ( chrtmp .eq. ' ' ) then

        if ( ihave .eq. 2 ) then

        else if ( ihave .eq. 3 ) then
          ihave = 11
        end if
!
!  Comma.
!
      else if ( chrtmp .eq. ',' .or.
     &  chrtmp .eq. ';' ) then

        if ( ihave .ne. 1 ) then
          iterm = 1
          ihave = 12
          lchar = lchar + 1
        end if
!
!  Minus sign.
!
      else if ( chrtmp .eq. '-' ) then

        if ( ihave .eq. 1 ) then
          ihave = 2
          isgn = -1
        else
          iterm = 1
        end if
!
!  Plus sign.
!
      else if ( chrtmp .eq. '+' ) then

        if ( ihave .eq. 1 ) then
          ihave = 2
        else
          iterm = 1
        end if
!
!  Digit.
!
      else if ( ihave .lt. 11 .and.
     &  lge ( chrtmp, '0' ) .and.
     &  lle ( chrtmp, '9' ) ) then

        ihave = 3

        call digten ( chrtmp, ndig )

        itop = 10 * itop + ndig
!
!  Anything else is regarded as a terminator.
!
      else
        iterm = 1
      end if

      if ( iterm .ne. 1 .and. lchar+1 .lt. nchar ) then
        go to 10
      end if

      if ( iterm .ne. 1 .and. lchar+1 .eq. nchar ) then
        lchar = nchar
      end if
!
!  Number seems to have terminated.  Have we got a legal number?
!
      if ( ihave .eq. 1 .or. ihave .eq. 2 ) then
        ierror = ihave
        return
      end if
!
!  Number seems OK.  Form it.
!
      intval = isgn * itop

      return
      end
      subroutine chrdb1 ( string )
!
!***********************************************************************
!
!! CHRDB1 removes blanks from a string, left justifying the remainder.
!
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character*(*) STRING, the string to be transformed.
!
      character*1 chrtmp
      integer i
      integer j
      integer nchar
      character*(*) string
!
      nchar = len ( string )

      j = 0
      do i = 1, nchar
        chrtmp = string(i:i)
        string(i:i) = ' '

        if ( chrtmp .ne. ' ' ) then
          j = j + 1
          string(j:j) = chrtmp
        end if

      end do

      return
      end
      subroutine cijk ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! CIJK computes A = B*C using index order IJK and complex arithmetic.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, complex A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      complex a(lda,n)
      complex b(lda,n)
      complex c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call cset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do j = 1, n
            do k = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine cset ( a, b, c, lda, n )
!
!***********************************************************************
!
!! CSET initializes the complex A, B and C matrices.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, complex A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
      integer lda
      integer n
!
      complex a(lda,n)
      complex b(lda,n)
      complex c(lda,n)
      integer i
      integer j
!
      do i = 1, n
        do j = 1, n
          a(i,j) = cmplx ( 0.0, 0.0 )
          b(i,j) = cmplx ( 2.0, 1.0 )
          c(i,j) = cmplx ( 1.0, 1.0 )
        end do
      end do
 
      return
      end
      subroutine digten ( tenval, intval )
!
!***********************************************************************
!
!! DIGTEN returns the integer value of a base 10 digit.
!
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character*1 TENVAL, the decimal digit, '0' through '9'.
!
!    Output, integer INTVAL, the corresponding integer value.
!
      integer intval
      character*1 tenval
!
      if ( lge ( tenval, '0' ) .and. lle ( tenval, '9' ) ) then

        intval = ichar ( tenval ) - 48

      else if ( tenval .eq. ' ' ) then

        intval = 0

      else

        write ( *, * ) ' '
        write ( *, * ) 'DIGTEN - Serious error!'
        write ( *, * ) '  Illegal decimal digit = ' // tenval
        write ( *, * ) '  ASCII number ', ichar ( tenval )
        intval = 0
        stop

      end if

      return
      end
      subroutine dijk ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! DIJK multiplies A = B*C using index order IJK and double precision.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, double precision A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      double precision a(lda,n)
      double precision b(lda,n)
      double precision c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call dset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do j = 1, n
            do k = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine domethod ( lda, n, nrep, order, ttime )
!
!***********************************************************************
!
!! DOMETHOD calls a specific multiplication routine.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Input, character*6 ORDER, specifies the method to be used.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lena
      parameter ( lena = 300 )
!
      integer lda
      integer n
!
      real a(lena,lena)
      real b(lena,lena)
      real c(lena,lena)
      complex ca(lena,lena)
      complex cb(lena,lena)
      complex cc(lena,lena)
      double precision da(lena,lena)
      double precision db(lena,lena)
      double precision dc(lena,lena)
      integer ia(lena,lena)
      integer ib(lena,lena)
      integer ic(lena,lena)
      logical la(lena,lena)
      logical lb(lena,lena)
      logical lc(lena,lena)
      logical s_eqi
      integer nrep
      character*6 order
      real ttime
!
      if ( s_eqi ( order, 'CIJK' ) ) then

        call cijk ( ca, cb, cc, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'DIJK' ) ) then

        call dijk ( da, db, dc, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'IJK' ) ) then
 
        call ijk ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'IJUK' ) ) then

        call ijuk ( a, b, c, lda, n, nrep, ttime )
 
      else if ( s_eqi ( order, 'IKJ' ) ) then
 
        call ikj ( a, b, c, lda, n, nrep, ttime )
 
      else if ( s_eqi ( order, 'IUJK' ) ) then
 
        call iujk ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'JIK' ) ) then
 
        call jik ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'JKI' ) ) then
 
        call jki ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'LIJK' ) ) then

        call lijk ( la, lb, lc, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'MIJK' ) ) then

        call mijk ( a, b, c, lda, n, nrep, ttime )
 
      else if ( s_eqi ( order, 'MKJI' ) ) then

        call mkji ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'MXMA' ) ) then

        call umxma ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'NIJK' ) ) then

        call nijk ( ia, ib, ic, lda, n, nrep, ttime )
 
      else if ( s_eqi ( order, 'NIJK46' ) ) then

        call nijk46 ( ia, ib, ic, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'SAXPYC' ) ) then

        call usaxpyc ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'SAXPYR' ) ) then

        call usaxpyr ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'SDOT' ) ) then

        call usdot ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'SGEMM' ) ) then

        call usgemm ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'SGEMMS' ) ) then

        call usgemms ( a, b, c, lda, n, nrep, 3*lena*lena, ttime )

      else if ( s_eqi ( order, 'SIJK' ) ) then

        call sijk ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'TAXPYC' ) ) then

        call utaxpyc ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'TAXPYR' ) ) then

        call utaxpyr ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'TDOT' ) ) then

        call utdot ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'TGEMM' ) ) then

        call utgemm ( a, b, c, lda, n, nrep, ttime )

      else if ( s_eqi ( order, 'UIJK' ) ) then

        call uijk ( a, b, c, lda, n, nrep, ttime )

      end if

      return
      end
      subroutine dset ( a, b, c, lda, n )
!
!***********************************************************************
!
!! DSET initializes the double precision A, B and C matrices.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, double precision A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
      integer lda
      integer n
!
      double precision a(lda,n)
      double precision b(lda,n)
      double precision c(lda,n)
      integer i
      integer j
!
      do i = 1, n
        do j = 1, n
          a(i,j) = 0.0
          b(i,j) = 1.0
          c(i,j) = 1.0
        end do
      end do
 
      return
      end
      subroutine getn ( string, ierror, lda, lena, n, nhi, ninc, nlo, 
     &  nmult )
!
!***********************************************************************
!
!! GETN determines the problem sizes desired by the user.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character*(*) STRING, a string containing the user's
!    data for an "N" command.  This command might have one of the
!    following forms, where "n", "nlo", "nhi", "ninc" and "nmult"
!    are actually numeric values:
!
!      n                   Solve a single problem of size N.
!      nlo, nhi            Solve every problem from N = NLO to N = NHI.
!      nlo, nhi, ninc      Solve from N = NLO to N = NHI, incrementing by NINC.
!      nlo, nhi, *nmult    Solve from N = NLO to N = NHI, multiplying by NMULT.
!
!    Output, integer IERROR, an error flag.
!    0, no error occurred.
!    nonzero, an error occurred, and the operation could not be done.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer LENA, the maximum matrix order allowed.
!
!    Output, integer N, the new value for the number of rows and columns 
!    to use in the matrices.
!
!    Output, integer NHI, the maximum value of N to use.
!
!    Output, integer NINC, the additive increment to use, if additive
!    steps are being taken.
!
!    Output, integer NLO, the smallest value of N to use.
!
!    Output, integer NMULT, the multiplier to use, if multiplicative
!    steps are being taken.
!
      integer ierror
      integer imult
      integer itemp
      integer lchar
      integer lda
      integer lena
      integer n
      integer next
      integer nhi
      integer ninc
      integer nlo
      integer nmult
      character*(*) string
!
      nhi = 0
      ninc = 0
      nmult = 1
      nlo = 0
!
!  Read the first number, N or NLO.
!
      call chrcti ( string, itemp, ierror, lchar )
      next = lchar + 1

      if ( ierror .ne. 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'I could not understand your definition of N.'
        return
      else if ( itemp .le. 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'This value of N is not acceptable!'
        write ( *, * ) 'N must be positive.'
        return
      else if ( itemp .gt. lena ) then
        write ( *, * ) ' '
        write ( *, * ) 'This value of N is not acceptable!'
        write ( *, * ) 'N must be no greater than ', lena
        return
      else
        n = itemp
        nlo = itemp
        nhi = itemp
        write ( *, * ) ' '
        write ( *, * ) 'N has been set to ', n
      end if
!
!  Read the second number, NHI.
!
      call chrcti ( string(next:), itemp, ierror, lchar )
      next = next + lchar

      if ( ierror .ne. 0 ) then
!       write ( *, * ) ' '
!       write ( *, * ) 'I could not understand your definition of NHI.'
        go to 10
      else if ( itemp .le. 0 ) then
        write ( *, * ) ' '
        write ( *, * ) 'This value of NHI is not acceptable!'
        write ( *, * ) 'NHI must be positive.'
        go to 10
      else if ( itemp .gt. lena ) then
        write ( *, * ) ' '
        write ( *, * ) 'This value of NHI is not acceptable!'
        write ( *, * ) 'NHI must be no greater than ', lena
        go to 10
      else
        nhi = itemp
        write ( *, * ) ' '
        write ( *, * ) 'NHI has been set to ', nhi
        ninc = 1
      end if
 
      if ( string(next:next) .eq. '*' ) then
        next = next+1
        imult = 1
      else
        imult = 0
      end if
!
!  Read third number, ninc or nmult
!
      call chrcti ( string(next:), itemp, ierror, lchar )

      if ( ierror .ne. 0 ) then
!       write ( *, * ) ' '
!       write ( *, * ) 'I could not understand your definition of NINC.'
      else
        if ( imult .eq. 0 ) then
          ninc = itemp
          nmult = 1
          write ( *, * ) ' '
          write ( *, * ) 'NINC has been set to ', ninc
        else
          ninc = 0
          nmult = itemp
          write ( *, * ) ' '
          write ( *, * ) 'NMULT has been set to ', nmult
        end if
      end if
!
!  Check that LDA is no less than NLO and NHI.
!
10    continue

      if ( lda .lt. max ( nlo, nhi ) ) then
        lda = max ( nlo, nhi )
        write ( *, * ) ' '
        write ( *, * ) 'Note: Since LDA must be at least as large as'
        write ( *, * ) 'N, MATMUL has increased the value of LDA.'
        write ( *, * ) 'LDA has been set to ', lda
      end if
 
      return
      end
      subroutine get_order ( string, ierror, machine, order )
!
!***********************************************************************
!
!! GET_ORDER reads a new value of order from the user.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character*(*) STRING, a string of characters, containing
!    the user's choice for the matrix multiplication method to employ.
!
!    Output, integer IERROR, an error flag.
!    0, no error occurred.
!    nonzero, an error occurred and the operation could not be completed.
!
!    Input, character*10 MACHINE, the name of the machine for which
!    the package is prepared.
!
!    Output, character*6 ORDER, specifies the method to be used.
!
      integer nchoice1
      parameter ( nchoice1 = 18 )
!
      character*6 choices1(nchoice1)
      integer i
      integer ierror
      logical s_eqi
      character*10 machine
      character*6 order
      character*(*) string
!
      data  ( choices1(i), i = 1, nchoice1 ) /
     &  'ALL'   , 'CIJK'  , 'DIJK'  , 'IJK'   , 'IJUK'  ,
     &  'IKJ'   , 'IUJK'  , 'JIK'   , 'JKI'   , 'KIJ'   ,
     &  'KJI'   , 'LIJK'  , 'NIJK'  , 'TAXPYC', 'TAXPYR',
     &  'TDOT'  , 'TGEMM' , 'UIJK' /
!
      do i = 1, nchoice1
        if ( s_eqi ( order, choices1(i) ) ) then
          go to 10
        end if
      end do

      if ( s_eqi(machine(1:4),'cray') ) then

        if ( s_eqi(string(1:4),'mijk'))go to 10
        if ( s_eqi(string(1:4),'mxma'))go to 10
        if ( s_eqi(string(1:6),'nijk46'))go to 10
        if ( s_eqi(string(1:6),'saxpyc'))go to 10
        if ( s_eqi(string(1:6),'saxpyc'))go to 10
        if ( s_eqi(string(1:4),'sdot'))go to 10
        if ( s_eqi(string(1:5),'sgemm'))go to 10
        if ( s_eqi(string(1:6),'sgemms'))go to 10
        if ( s_eqi(string(1:4),'sijk'))go to 10

      end if
 
      if (s_eqi(machine(1:8),'sgi/iris') ) then

        if (s_eqi(string(1:4),'mkji'))go to 10
        if (s_eqi(string(1:6),'saxpyc'))go to 10
        if (s_eqi(string(1:6),'saxpyc'))go to 10
        if (s_eqi(string(1:4),'sdot'))go to 10
        if (s_eqi(string(1:5),'sgemm'))go to 10

      end if
 
      write ( *, * ) 'The order you chose was not a valid choice.'
 
      write ( *, * ) ' '
      write ( *, * ) 'Valid choices for the order are:'
      write ( *, * ) ' '
      write ( *, * ) 'ALL, CIJK, DIJK, IJK, IKJ, IJUK, IUJK,'
      write ( *, * ) 'JIK, JKI, KIJ, KJI, LIJK, NIJK,'
      write ( *, * ) 'TAXPYC, TAXPYR, TDOT, TGEMM, UIJK'
 
      if (s_eqi(machine(1:4),'cray') ) then
        write ( *, * ) 'MIJK, MXMA, NIJK46, SAXPYC, SAXPYR, SDOT, '
        write ( *, * ) 'SGEMM, SGEMMS, SIJK.'
      else if (s_eqi(machine,'sgi/iris') ) then
        write ( *, * ) 'MKJI, SAXPYC, SAXPYR, SDOT, SGEMM.'
      end if
 
      write ( *, * ) ' '
      write ( *, * ) 'Your command was not carried out.'
 
      ierror = 1
      return
 
10    continue
 
      order = string(1:6)

      write ( *, * ) ' '
      write ( *, * ) 'The order has been set to ', order
 
      return
      end
      subroutine getsho ( string, cshow, fshow, lnshow, lshow,
     &  lval, mshow, noshow, nrshow, nshow, tshow )
!
!***********************************************************************
!
!! GETSHO determines what items the user wishes to print out.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character STRING*(*), a string which specifies which
!    variable is to be assigned the input value of LVAL.
!
!    Output, logical CSHOW, is TRUE if the multiplication method
!    is to be shown.
!
!    Output, logical FSHOW, is TRUE if the MegaFLOP rate is to be shown.
!
!    Output, logical LNSHOW, is TRUE if the software's programming
!    language, stored in LINGO, is to be shown.
!
!    Output, logical LSHOW, is TRUE if the variable LDA is to be shown.
!
!    Input, logical LVAL, a logical value, which is to be assigned
!    to one of the variables.
!
!    Output, logical MSHOW, is TRUE if the machine name is to be shown.
!
!    Output, logical NOSHOW, is TRUE if the number of operations is
!    to be shown.
!
!    Output, logical NRSHOW, is TRUE if the number of repetitions is
!    to be shown.
!
!    Output, logical NSHOW, is TRUE if the matrix size N is to be shown.
!
!    Output, logical TSHOW, is TRUE if the execution time is to be shown.
!
      logical cshow
      logical fshow
      logical s_eqi
      logical lnshow
      logical lshow
      logical lval
      logical mshow
      logical noshow
      logical nrshow
      logical nshow
      character*(*) string
      logical tshow
!
      if (s_eqi(string(1:3),'all') ) then
        cshow = lval
        fshow = lval
        lnshow = lval
        lshow = lval
        mshow = lval
        noshow = lval
        nrshow = lval
        nshow = lval
        tshow = lval
      else if (s_eqi(string(1:3),'cpu') ) then
        tshow = lval
      else if (s_eqi(string(1:8),'language') ) then
        lnshow = lval
      else if (s_eqi(string(1:3),'lda') ) then
        lshow = lval
      else if (s_eqi(string(1:7),'machine') ) then
        mshow = lval
      else if (s_eqi(string(1:6),'mflops') ) then
        fshow = lval
      else if (s_eqi(string(1:1),'n') ) then
        nshow = lval
      else if (s_eqi(string(1:4),'nrep') ) then
        nrshow = lval
      else if (s_eqi(string(1:3),'ops') ) then
        noshow = lval
      else if (s_eqi(string(1:5),'order') ) then
        cshow = lval
      else if (s_eqi(string(1:4),'time') ) then
        tshow = lval
      else
        write ( *, * ) ' '
        write ( *, * ) 'That is not a legal name!'
        write ( *, * )
     &  'Legal names are CPU, LANGUAGE, LDA, MACHINE, MFLOPS,'//
     &  ' N, NREP, OPS, ORDER, and TIME.'
      end if
 
      return
      end
      subroutine header ( cshow, fshow, lnshow, lshow, mshow,
     &  noshow, nrshow, nshow, output, tshow )
!
!***********************************************************************
!
!! HEADER prints out a header for the results.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical CSHOW, is TRUE if the multiplication method
!    is to be shown.
!
!    Input, logical FSHOW, is TRUE if the MegaFLOP rate is to be shown.
!
!    Input, logical LNSHOW, is TRUE if the software's programming
!    language, stored in LINGO, is to be shown.
!
!    Input, logical LSHOW, is TRUE if the variable LDA is to be shown.
!
!    Input, logical MSHOW, is TRUE if the machine name is to be shown.
!
!    Input, logical NOSHOW, is TRUE if the number of operations is
!    to be shown.
!
!    Input, logical NRSHOW, is TRUE if the number of repetitions is
!    to be shown.
!
!    Input, logical NSHOW, is TRUE if the matrix size N is to be shown.
!
!    Output, character*82 OUTPUT, a string containing a header for
!    all the variables which are to be printed.
!
!    Input, logical TSHOW, is TRUE if the execution time is to be shown.
!
      logical cshow
      logical fshow
      integer s_length
      logical lnshow
      logical lshow
      logical mshow
      integer next
      logical noshow
      logical nrshow
      logical nshow
      character*82 output
      logical tshow
!
!  Prepare the header string.  
!
      next = 1
  
      if ( cshow ) then
        output(next:) = ' Order'
        next = s_length(output) + 1
      end if

      if ( lshow ) then
        output(next:) = ' LDA'
        next = s_length(output) + 1
      end if

      if ( nshow ) then
        output(next:) = '   N'
        next = s_length(output) + 1
      end if

      if ( cshow ) then
        output(next:) = '      CPU'
        next = s_length(output) + 1
      end if

      if ( tshow ) then
        output(next:) = ' Secs'
        next = s_length(output) + 1
      end if

      if ( noshow ) then
        output(next:) = '       Ops'
        next = s_length(output) + 1
      end if

      if ( nrshow ) then
        output(next:) = ' NREP'
        next = s_length(output) + 1
      end if

      if ( fshow ) then
        output(next:) = '    MFLOPS'
        next = s_length(output) + 1
      end if

      if ( mshow ) then
        output(next:) = '  Machine'
        next = s_length(output) + 1
      end if
 
      if ( lnshow ) then
        output(next:) = '  Language'
        next = s_length(output) + 1
      end if

      write ( *, * ) ' '
      write ( *, '(a)' ) output(1:next-1)
      write ( *, * ) ' '
 
      return
      end
      subroutine hello ( lena, machine )
!
!***********************************************************************
!
!! HELLO says hello to the user, printing the version, machine, and so on.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer LENA, the maximum matrix order allowed.
!
!    Input, character*10 MACHINE, the identifier for the computer
!    for which MATMUL has been set up.
!
      integer lena
      character*10 machine
!
      write ( *, * ) ' '
      write ( *, * ) 'MATMUL'
      write ( *, * ) '  An interactive demonstration of the speed'
      write ( *, * ) '  of matrix multiplication.'
      write ( *, * ) ' '
      write ( *, * ) '  This is version 1.19'
      write ( *, * ) '  Last modified on 28 August 1999.'
      write ( *, * ) ' '
      write ( *, * ) '  This is the version for ' // machine
      write ( *, * ) '  The maximum matrix order allowed is N = ', lena
      write ( *, * ) ' '
 
      return
      end
      subroutine help
!
!***********************************************************************
!
!! HELP prints a list of the available commands.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
      write ( *, * ) ' '
      write ( *, * ) 'This is the list of legal commands.'
      write ( *, * ) ' '
      write ( *, * ) 'H                 Help. List the legal commands.'
      write ( *, * )
     &  'LDA=value         Set leading dimension of arrays.'
      write ( *, * ) 'M                 Multiply two matrices.'
      write ( *, * ) 'N=value           Assigns the size of the arrays.'
      write ( *, * ) 'N=nlo,nhi,ninc    Sets N=nlo, N=nlo+ninc, ....'
      write ( *, * ) 'N=nlo,nhi,*nmult  Sets N=nlo, N=nlo*nmult, ....'
      write ( *, * ) 'NREP=nrep         Sets the repetition factor.'
      write ( *, * ) 'ORDER=name        Chooses the algorithm.'
      write ( *, * ) 'P                 Prints out current results.'
      write ( *, * ) 'Q                 Quit.'
      write ( *, * ) 'SHOW=name         Include "name" in output.'
      write ( *, * )
     &  '                  "name" = ORDER, LDA, N, CPU, OPS,'
      write ( *, * ) '                  MFLOPS, MACHINE, or LANGUAGE.'
      write ( *, * )
     &  '                  If "name"=ALL, all items are shown.'
      write ( *, * ) 'NOSHOW=name       removes item from output list.'
      write ( *, * ) ' '
 
      return
      end
      subroutine ijk ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! IJK multiplies A = B*C using index order IJK.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do j = 1, n
            do k = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
 
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine ijuk ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! IJUK multiplies A = B*C using index order IJK and unrolling.
!
!
!  Method:
!
!    The K loop is unrolled to a depth of NROLL = 4.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer nroll
      parameter (nroll = 4)
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer khi
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        khi = ( n / nroll ) * nroll

        do i = 1, n
          do j = 1, n
            do k = 1, khi, nroll
              a(i,k)   = a(i,k)   + b(i,j) * c(j,k)
              a(i,k+1) = a(i,k+1) + b(i,j) * c(j,k+1)
              a(i,k+2) = a(i,k+2) + b(i,j) * c(j,k+2)
              a(i,k+3) = a(i,k+3) + b(i,j) * c(j,k+3)
            end do
          end do
        end do
!
!  Take care of the few cases we missed if N is not a multiple of 4.
!
        do i = 1, n
          do j = 1, n
            do k = khi+1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine ikj ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! IKJ multiplies A = B*C using index order IKJ.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do k = 1, n
            do j = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine init ( command, cshow, fshow, lda, lena, lingo, lnshow,
     &  lshow, machine, mshow, n, nhi, ninc, nlo, nmult, noshow,
     &  nrep, nrshow, nshow, order, tshow )
!
!***********************************************************************
!
!! INIT initializes data.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, character*6 COMMAND, the most recent user command.
!
!    Output, logical CSHOW, is TRUE if the multiplication method
!    is to be shown.
!
!    Output, logical FSHOW, is TRUE if the MegaFLOP rate is to be shown.
!
!    Output, integer LDA, the leading dimension used for arrays.
!
!    Input, integer LENA, the maximum matrix order allowed.
!
!    Output, character*7 LINGO, the language in which MATMUL is written.
!
!    Output, logical LNSHOW, is TRUE if the software's programming
!    language, stored in LINGO, is to be shown.
!
!    Output, logical LSHOW, is TRUE if the variable LDA is to be shown.
!
!    Output, character*10 MACHINE, the name of the computer on which
!    MATMUL has been compiled.
!
!    Output, logical MSHOW, is TRUE if the machine name is to be shown.
!
!    Output, integer N, the number of rows and columns in the matrices.
!
!    Output, integer NHI, the maximum value of N to use.
!
!    Output, integer NINC, the additive increment to use, if additive
!    steps are being taken.
!
!    Output, integer NLO, the smallest value of N to use.
!
!    Output, integer NMULT, the multiplier to use, if multiplicative
!    steps are being taken.
!
!    Output, logical NOSHOW, is TRUE if the number of operations is
!    to be shown.
!
!    Output, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, logical NRSHOW, is TRUE if the number of repetitions is
!    to be shown.
!
!    Output, logical NSHOW, is TRUE if the matrix size N is to be shown.
!
!    Output, character*6 ORDER, specifies the method to be used.
!
!    Output, logical TSHOW, is TRUE if the execution time is to be shown.
!
      integer lena
!
      character*6 command
      logical cshow
      logical fshow
      integer lda
      character*7 lingo
      logical lnshow
      logical lshow
      character*10 machine
      logical mshow
      integer n
      integer nhi
      integer ninc
      integer nlo
      integer nmult
      logical noshow
      integer nrep
      logical nrshow
      logical nshow
      character*6 order
      logical tshow
!
      command = ' '
      cshow = .true.
      fshow = .true.
      lda = lena
      lingo = 'Fortran'
      lnshow = .true.
      lshow = .true.
!     machine = 'Macintosh'
!     machine = 'Mac/6881'
!     machine = 'PowerMac'
!     machine = 'CM-2'
!     machine = 'Cray C90'
!     machine = 'Cray YMP'
!     machine = 'DEC Alpha'
!     machine = 'DECstation'
!     machine = 'IBM PC'
      machine = 'SGI/IRIS'
!     machine = 'NEXT'
!     machine = 'SUN'
!     machine = 'VAX/VMS'
!     machine = 'VECTOR/VAX'
!
      mshow = .true.
      n = 16
      nhi = n
      ninc = 0
      nlo = n
      nmult = 1
      noshow = .true.
      nrep = 1
      nrshow = .false.
      nshow = .true.
      order = 'IJK'
      tshow = .true.
 
      return
      end
      subroutine iset ( a, b, c, lda, n )
!
!***********************************************************************
!
!! ISET initializes the integer A, B and C matrices.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, integer A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
      integer lda
      integer n
!
      integer a(lda,n)
      integer b(lda,n)
      integer c(lda,n)
      integer i
      integer j
!
      do i = 1, n
        do j = 1, n
          a(i,j) = 0
          b(i,j) = 1
          c(i,j) = 1
        end do
      end do
 
      return
      end
      subroutine iujk ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! IUJK multiplies A = B*C using index order IJK, and unrolling on J.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer nroll
      parameter (nroll = 4)
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer jhi
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
        jhi = ( n / nroll ) * nroll
 
        do i = 1, n
          do j = 1, jhi, nroll
            do k = 1, n
              a(i,k) = a(i,k)
     &          + b(i,j  ) * c(j,k)
     &          + b(i,j+1) * c(j+1,k)
     &          + b(i,j+2) * c(j+2,k)
     &          + b(i,j+3) * c(j+3,k)
            end do
          end do
        end do
!
!  Take care of the few cases we missed if N is not a multiple of 4.
!
        do i = 1, n
          do j = jhi+1, n
            do k = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine jik ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! JIK multiplies A = B*C using index order JIK.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do j = 1, n
          do i = 1, n
            do k = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine jki ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! JKI multiplies A = B*C using index order JKI.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do j = 1, n
          do k = 1, n
            do i = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine kij ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! KIJ multiplies A = B*C using index order KIJ.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do k = 1, n
          do i = 1, n
            do j = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine kji ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! KJI multiplies A = B*C using index order KJI.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do k = 1, n
          do j = 1, n
            do i = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine lijk ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! LIJK "multiplies" A = B*C using index order IJK, using logical data.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, logical A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      logical a(lda,n)
      logical b(lda,n)
      logical c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call lset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do j = 1, n
            do k = 1, n
              a(i,k) = a(i,k) .or. ( b(i,j) .and. c(j,k) )
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine lset ( a, b, c, lda, n )
!
!***********************************************************************
!
!! LSET initializes the logical A, B and C matrices.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, logical A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
      integer lda
      integer n
!
      logical a(lda,n)
      logical b(lda,n)
      logical c(lda,n)
      integer i
      integer j
!
      do i = 1, n
        do j = 1, n
          a(i,j) = .false.
          b(i,j) = .true.
          c(i,j) = .true.
        end do
      end do
 
      return
      end
      subroutine mijk ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! MIJK multiplies A = B*C using index order IJK.
!
!
!  MIJK uses a Cray directive to run the triple loop using
!  multitasking.  The benefit of such a directive depends on the
!  algorithm and the load on the machine.
!
!  Except on the Cray, this routine should not be used, and in
!  particular, the call to TIMEF should be commented out.
!
!  Note that the Cray routine TIMEF must be called, rather than
!  SECOND.  TIMEF reports elapsed "real" time or "wallclock" time,
!  which should go down with multitasking, whereas CPU time should
!  remain roughly constant.
!
!  In order for parallel processing to occur, this routine must be
!  compiled on the Cray with the directive "-Zu"; moreover, the user must
!  set the environment variable NCPUS to the number of processors the
!  user would like.  For instance, a C shell user would type:
!
!    setenv NCPUS 8
!
!  while a Bourne shell user would type
!
!    NCPUS = 8
!    export NCPUS
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      time1 = 0.0
      time2 = 0.0
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
!
!       call timef ( time1 )
!
!MIC$ do GLOBAL
!
        do i = 1, n
          do j = 1, n
            do k = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
!       call timef ( time2 )
        ttime = ttime + (time2-time1) / 1000.0
 
      end do
 
      return
      end
      subroutine mkji ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! MKJI multiplies A = B*C using index order KJI and multitasking.
!
!
!  Discussion:
!
!    MKJI uses an SGI/IRIS parallel processing directive to run the
!    triple loop using multitasking.
!
!    The benefit of such a directive depends on the algorithm and the
!    load on the machine.
!
!    Except on the SGI/IRIS, this routine should not be used, and in
!    particular, the call to SECNDS should be commented out.
!
!    Note that the SGI/IRIS routine SECNDS must be called, rather than
!    SECOND.  SECNDS reports elapsed "real" time or "wallclock" time,
!    which should go down with multitasking, whereas CPU time should
!    remain roughly constant.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      time1 = 0.0
      time2 = 0.0
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_real_timer ( time1 )
!
!$DOACROSS LOCAL(I, J, K)
!
        do k = 1, n
          do j = 1, n
            do i = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_real_timer ( time2 )

        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine mult ( cshow, fshow, lda, lingo, lnshow, lshow, 
     &  machine, mshow, n, noshow, nrep, nrshow, nshow, order, 
     &  output, tshow )
!
!***********************************************************************
!
!! MULT carries out the matrix multiplication, using the requested method.  
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical CSHOW, is TRUE if the multiplication method
!    is to be shown.
!
!    Input, logical FSHOW, is TRUE if the MegaFLOP rate is to be shown.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, character*7 LINGO, the language in which MATMUL is written.
!
!    Input, logical LNSHOW, is TRUE if the software's programming
!    language, stored in LINGO, is to be shown.
!
!    Input, logical LSHOW, is TRUE if the variable LDA is to be shown.
!
!    Input, character*10 MACHINE, the computer on which MATMUL has
!    been compiled.
!
!    Input, logical MSHOW, is TRUE if the machine name is to be shown.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, logical NOSHOW, is TRUE if the number of operations is
!    to be shown.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Input, logical NRSHOW, is TRUE if the number of repetitions is
!    to be shown.
!
!    Input, logical NSHOW, is TRUE if the matrix size N is to be shown.
!
!    Input, character*6 ORDER, specifies the method to be used.
!
!    Output, character*82 OUTPUT, a string containing the data for
!    this multiplication.
!
!    Input, logical TSHOW, is TRUE if the execution time is to be shown.
!
      integer norder

      parameter ( norder = 25 )
!
      integer lda
      integer n
!
      logical cshow
      logical fshow
      integer i
      logical s_eqi
      character*7 lingo
      logical lnshow
      logical lshow
      character*10 machine
      logical mshow
      logical noshow
      integer nrep
      logical nrshow
      logical nshow
      character*6 order
      character*6 order2(norder)
      character*82 output
      logical tshow
      real ttime
!
      data order2 /
     &  'CIJK',
     &  'DIJK',
     &  'IJK',
     &  'IJUK',
     &  'IKJ',
     &  'IUJK',
     &  'JIK',
     &  'JKI',
     &  'LIJK',
     &  'MIJK',
     &  'MKJI',
     &  'MXMA',
     &  'NIJK',
     &  'NIJK46',
     &  'SAXPYC',
     &  'SAXPYR',
     &  'SDOT',
     &  'SGEMM',
     &  'SGEMMS',
     &  'SIJK',
     &  'TAXPYC',
     &  'TAXPYR',
     &  'TDOT',
     &  'TGEMM',
     &  'UIJK' /
!
      call header ( cshow, fshow, lnshow, lshow, mshow, noshow,
     &  nrshow, nshow, output, tshow )

      if ( s_eqi ( order, 'ALL' ) ) then

        do i = 1, norder

          order = order2(i)

          call domethod ( lda, n, nrep, order, ttime )

          call report ( cshow, fshow, lda, lingo, lnshow, lshow,  
     &      machine, mshow, n, noshow, nrep, nrshow, nshow, order, 
     &      output, tshow, ttime )
 
        end do

        order = 'ALL'

      else

        call domethod ( lda, n, nrep, order, ttime )

        call report ( cshow, fshow, lda, lingo, lnshow, lshow,  
     &    machine, mshow, n, noshow, nrep, nrshow, nshow, order, 
     &    output, tshow, ttime )

      end if

      return
      end
      subroutine nijk ( a, b, c, lda, n, nrep, ttime )
!DIR$ integer = 64
!
!  The above line is a Cray compiler directive, which requests that
!  integers be stored as 64 bit quantities, and that 64 bit integer
!  arithmetic be used.
!
!***********************************************************************
!
!! NIJK multiplies A = B*C using index order IJK, using integer arithmetic.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, integer A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      integer a(lda,n)
      integer b(lda,n)
      integer c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call iset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do j = 1, n
            do k = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine nijk46 ( a, b, c, lda, n, nrep, ttime )
!DIR$ integer = 46
!
!  The above line is a Cray compiler directive, which requests that
!  integers be stored as 46 bit quantities, and that 46 bit integer
!  arithmetic be used.
!
!***********************************************************************
!
!! NIJK46 multiplies A = B*C using index order IJK, and integer arithmetic.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, integer A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      integer a(lda,n)
      integer b(lda,n)
      integer c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call iset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do j = 1, n
            do k = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine nstep ( ido, n, nhi, ninc, nlo, nmult )
!
!***********************************************************************
!
!! NSTEP is used when a set of values of N is being generated.
!
!
!  Discussion:
!
!    The routine checks whether addition or multiplication is being used,
!    and increases the value of N.  It also checks whether the set of
!    values is done, or whether the input values are inconsistent.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer IDO.
!
!    0, N has reached or surpassed NHI, and no further
!    increase should be carried out.  N has been reset to NLO.
!
!    1, N had not reached or surpassed NHI, and so N has been
!    incremented by NINC, or multiplied by NMULT.
!
!    2, N had not reached or surpassed NHI, but we're never
!    going to get there!  Either:
!
!      NINC is negative but NHI is greater than NLO, or
!
!      NINC is positive but NHI is less than NLO, or
!
!      NMULT is positive but NLO is greater than NHI.
!
!    3, NINC is 0 and NMULT is less than or equal to 1.
!
!    Input/output, integer N, the number of rows and columns in the matrices.
!    This routine modifies the value of N as appropriate.
!
!    Input, integer NHI, the maximum value of N to use.
!
!    Input, integer NINC, the additive increment to use, if additive
!    steps are being taken.
!
!    Input, integer NLO, the smallest value of N to use.
!
!    Input, integer NMULT, the multiplier to use, if multiplicative
!    steps are being taken.
!
      integer ido
      integer n
      integer nhi
      integer ninc
      integer nlo
      integer nmult
!
!  If NINC is not 0, then
!    if it's pointing in the right direction, then
!      add NINC to N,
!      set a continuation flag
!      if N+NINC exceeds NHI, then
!        reset N, and
!        set a completion flag
!    else
!      set an error flag.
!
      if ( ninc .ne. 0 ) then
        if ( ( nlo .lt. nhi .and. ninc .gt. 0 )  .or. 
     &     ( nlo .gt. nhi .and. ninc .lt. 0 ) ) then
          n = n + ninc
          ido = 1
          if ( ( n .gt. nhi .and. nhi .ge. nlo )  .or. 
     &       ( n .lt. nhi .and. nhi .le. nlo ) ) then
            ido = 0
            n = nlo
          end if
        else
          ido = 2
        end if
 
        return
      end if
!
!  If NMULT is greater than 1, then
!    if it's pointing in the right direction, then
!      multiply N by NMULT,
!      set a continuation flag
!      if N*NMULT exceeds NHI, then
!        reset N, and
!        set a completion flag
!    else
!      set an error flag.
!
      if ( nmult .gt. 1 ) then
        if ( nlo .lt. nhi ) then
          n = n * nmult
          ido = 1
          if ( 
     &      ( n .gt. nhi .and. nhi .ge. nlo )  .or. 
     &      ( n .lt. nhi .and. nhi .le. nlo ) ) then
            ido = 0
            n = nlo
          end if
        else
          ido = 2
        end if
 
        return

      end if
!
!  NINC was 0, and NMULT wasn't greater than 1.
!
      ido = 3
 
      return
      end
      subroutine printr ( lda, lena, lingo, n, nhi, ninc, nlo, nmult, 
     &  nrep, order )
!
!***********************************************************************
!
!! PRINTR prints out those parameters the user wants to see.
!
!
!  Discussion:
!
!    These parameters include:
!
!      the language MATMUL is written in,
!      the algorithm,
!      the leading dimension,
!      the maximum allowable dimension,
!      the actual size of arrays,
!      the number of multiplications carried out.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer LENA, the maximum matrix order allowed.
!
!    Input, character*7 LINGO, the language in which MATMUL was written.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NHI, the maximum value of N to use.
!
!    Input, integer NINC, the additive increment to use, if additive
!    steps are being taken.
!
!    Input, integer NLO, the smallest value of N to use.
!
!    Input, integer NMULT, the multiplier to use, if multiplicative
!    steps are being taken.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Input, character*6 ORDER, specifies the method to be used.
!
      integer lda
      integer lena
      character*7 lingo
      integer n
      integer nhi
      integer ninc
      integer nlo
      integer nmult
      integer nrep
      character*6 order
!
      write ( *, * ) ' '
      write ( *, * ) 'This version of MATMUL is written in ' // lingo
      write ( *, * ) 'The algorithm chosen is ' // order
      write ( *, * ) 'The leading dimension of arrays, lda, is ', lda
      write ( *, * ) 'The maximum legal choice for LDA is ', lena
      write ( *, * ) 'The actual size of the arrays, N, is ', n

      if ( nhi .ne. nlo ) then
        write ( *, * ) 'Several problem sizes will be solved in order.'
        write ( *, * ) 'The final size of arrays, NHI, will be ', nhi
      end if
 
      if ( ninc .ne. 0 ) then
        write ( *, * ) 'Array size will be incremented by NINC = ', ninc
      end if
 
      if ( nmult .ne. 1 ) then
        write ( *, * ) 'Array size will be multiplied by NMULT = ', 
     &    nmult
      end if
 
      if ( nrep .ne. 1 ) then
        write ( *, * ) 'Multiplications repeated NREP = ', 
     &    nrep, ' times.'
      end if
 
      return
      end
      subroutine report ( cshow, fshow, lda, lingo, lnshow, lshow,  
     &  machine, mshow, n, noshow, nrep, nrshow, nshow, order, output, 
     &  tshow, ttime )
!
!***********************************************************************
!
!! REPORT reports the results for each multiplication experiment.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical CSHOW, is TRUE if the multiplication method
!    is to be shown.
!
!    Input, logical FSHOW, is TRUE if the MegaFLOP rate is to be shown.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, character*7 LINGO, the language in which MATMUL is written.
!
!    Input, logical LNSHOW, is TRUE if the software's programming
!    language, stored in LINGO, is to be shown.
!
!    Input, logical LSHOW, is TRUE if the variable LDA is to be shown.
!
!    Input, character*10 MACHINE, the computer on which MATMUL has
!    been compiled.
!
!    Input, logical MSHOW, is TRUE if the machine name is to be shown.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, logical NOSHOW, is TRUE if the number of operations is
!    to be shown.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Input, logical NRSHOW, is TRUE if the number of repetitions is
!    to be shown.
!
!    Input, logical NSHOW, is TRUE if the matrix size N is to be shown.
!
!    Input, character*6 ORDER, specifies the method to be used.
!
!    Output, character*82 OUTPUT, a string containing the data for
!    this multiplication.
!
!    Input, logical TSHOW, is TRUE if the execution time is to be shown.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      logical cshow
      logical fshow
      real ftemp
      integer ihi
      integer ilo
      character*7 lingo
      logical lnshow
      logical lshow
      character*10 machine
      logical mshow
      logical noshow
      integer nrep
      logical nrshow
      logical nshow
      integer ntemp
      character*6 order
      character*82 output
      logical tshow
      real ttime
!
      output = ' '
      ihi = 0

      if ( cshow ) then
        ilo = ihi + 1
        ihi = ilo + 5
        output(ilo:ihi) = order
      end if

      if ( lshow ) then
        ilo = ihi + 1
        ihi = ilo + 3
        write ( output(ilo:ihi), '(i4)' ) lda
      end if

      if ( nshow ) then
        ilo = ihi + 1
        ihi = ilo + 3
        write ( output(ilo:ihi), '(i4)' ) n
      end if

      if ( tshow ) then
        ilo = ihi + 1
        ihi = ilo + 13
        write ( output(ilo:ihi), '(f14.6)' ) ttime
      end if

      if ( noshow ) then
        ntemp = 2 * n**3
        ilo = ihi + 1
        ihi = ilo + 9
        write ( output(ilo:ihi), '(i10)' ) ntemp
      end if

      if ( nrshow ) then
        ilo = ihi + 1
        ihi = ilo + 4
        write ( output(ilo:ihi), '(i5)' ) nrep
      end if

      if ( fshow ) then

        if ( ttime .eq. 0.0 ) then
          ftemp = 0.0
        else
          ftemp = real ( ntemp * nrep ) / ( 1.0E6 * ttime )
        end if

        ilo = ihi + 1
        ihi = ilo + 9
        write ( output(ilo:ihi), '(f10.4)' ) ftemp

      end if

      if ( mshow ) then
        ilo = ihi + 1
        ilo = ilo + 1
        ihi = ilo + 9
        output(ilo:ihi) = machine
      end if

      if ( lnshow ) then
        ilo = ihi + 1
        ilo = ilo + 1
        ihi = ilo + 6
        output(ilo:ihi) = lingo
      end if

      if ( ihi .gt. 0 ) then
        write ( *, '(a)' ) output(1:ihi)
      end if
 
      return
      end
      subroutine rset ( a, b, c, lda, n )
!
!***********************************************************************
!
!! RSET initializes the real A, B and C matrices.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer j
!
      do i = 1, n
        do j = 1, n
          a(i,j) = 0.0
          b(i,j) = 1.0
          c(i,j) = 1.0
        end do
      end do
 
      return
      end
      subroutine s_cap ( string )
!
!***********************************************************************
!
!! S_CAP replaces any lowercase letters by uppercase ones in a string.
!
!
!  Modified:
!
!    16 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character*(*) STRING, the string to be transformed.
!
      character*1 c
      integer i
      integer nchar
      character*(*) string
!
      nchar = len ( string )

      do i = 1, nchar

        c = string(i:i)
        call c_cap ( c )
        string(i:i) = c

      end do

      return
      end
      function s_eqi ( strng1, strng2 )
!
!***********************************************************************
!
!! S_EQI is a case insensitive comparison of two strings for equality.
!
!
!  Examples:
!
!    S_EQI ( 'Anjana', 'ANJANA' ) is .TRUE.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character*(*) STRNG1, STRNG2, the strings to compare.
!
!    Output, logical S_EQI, the result of the comparison.
!
      integer i
      integer len1
      integer len2
      integer lenc
      logical s_eqi
      character*1 s1
      character*1 s2
      character*(*) strng1
      character*(*) strng2
!
      len1 = len ( strng1 )
      len2 = len ( strng2 )
      lenc = min ( len1, len2 )

      s_eqi = .false.

      do i = 1, lenc

        s1 = strng1(i:i)
        s2 = strng2(i:i)
        call c_cap ( s1 )
        call c_cap ( s2 )

        if ( s1 .ne. s2 ) then
          return
        end if

      end do

      do i = lenc + 1, len1
        if ( strng1(i:i) .ne. ' ' ) then
          return
        end if
      end do

      do i = lenc + 1, len2
        if ( strng2(i:i) .ne. ' ' ) then
          return
        end if
      end do

      s_eqi = .true.

      return
      end
      function s_length ( string )
!
!***********************************************************************
!
!! S_LENGTH returns the length of a string up to the last nonblank.
!
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character*(*) STRING, the string to be measured.
!
!    Output, integer S_LENGTH, the location of the last nonblank in STRING.
!
      integer i
      integer s_length
      character*(*) string
!
      do i = len ( string ), 1, -1

        if ( string(i:i) .ne. ' ' ) then
          s_length = i
          return
        end if

      end do

      s_length = 0

      return
      end
      subroutine matmul_cpu_timer ( cpu )
!
!***********************************************************************
!
!! MATMUL_CPU_TIMER computes total CPU seconds.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real CPU, the total CPU time, in seconds, since the
!    program began running.
!
      real cpu
!
!***********************************************************************
!  Cray
!***********************************************************************
!
!     cpu = second ( )
!
!***********************************************************************
!  VAX/VMS
!***********************************************************************
!
!     integer*4 code
!     parameter ( code = 2 )
!
!     integer*4 addr
!     integer*4 cputim
!     real oldtim
!     real temp
!
!     save addr
!     save oldtim
!
!     data addr / 0 /
!     data oldtim / 0.0 /
!
!     if ( addr .eq. 0 ) then
!       call lib$init_timer ( addr )
!     end if
!
!     call lib$stat_timer ( code, cputim, addr )
!     temp = real ( cputim ) / 100.0
!
!     oldtim = temp
!     cpu = temp
!
!***********************************************************************
!  UNIX systems
!***********************************************************************
!
      real tarray(2)
      call etime ( tarray )
      cpu = tarray(1) + tarray(2)
!
!***********************************************************************
!  PowerMac using Absoft FORTRAN.
!***********************************************************************
!
!     include "OSUtils.inc"
!
!     RECORD /DateTimeRec/ DateTime
!
!     call GetTime ( DateTime )
!
!     cpu = 3600 * DateTime.hour + 60 * DateTime.minute 
!    &  + DateTime.second
!
!***********************************************************************
!  Apple Macintosh using Absoft FORTRAN.
!***********************************************************************
!
!     integer isecnd
!
!     call time ( isecnd )
!     cpu = real ( isecnd )
!
!***********************************************************************
!  Apple Macintosh using LS FORTRAN.
!***********************************************************************
!
!     cpu = secnds ( 0.0 )
!
!***********************************************************************
!  IBM PC using Microsoft FORTRAN.
!***********************************************************************
!
!     integer*2 i100th
!     integer*2 ihr
!     integer*2 imin
!     integer*2 isec
!     real oldtim
!     real temp
!
!     save oldtim
!
!     call gettim ( ihr, imin, isec, i100th )
!
!     temp = 3600 * ihr + 60 * imin + isec + real ( i100th ) / 100.0
!
!     oldtim = temp
!     cpu = temp
!
      return
      end
      subroutine matmul_real_timer ( seconds )
!
!***********************************************************************
!
!! MATMUL_REAL_TIMER returns a reading of the real time clock.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real SECONDS, a current real time in seconds.
!
      real seconds
!
      seconds = 0.0
!
!***********************************************************************
!  Cray.
!***********************************************************************
!
!     seconds = secnds ( 0.0 )
!
      return
      end
      subroutine sijk ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! SIJK multiplies A = B*C using index order IJK, and no Cray vectorization.
!
!
!  Discussion:
!
!    SIJK uses a Cray directive to run the inner do loop WITHOUT
!    vectorization.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do j = 1, n
!DIR$ NEXTSCALAR
            do k = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine taxpy ( n, sa, sx, incx, sy, incy )
!
!***********************************************************************
!
!! TAXPY is unoptimized standard BLAS routine SAXPY.
!
!
!  Comments:
!
!    Roughly, TAXPY adds SA * SX(I) to SY(I) for I = 1 to N.
!    However, the increments INCX and INCY allow this to be done
!    even when SX or SY is a row or column of a matrix.  
!
!  Modified:
!
!    28 August 1999
!
!  Parameters:
!
!    Input, integer N, the "logical" number of items in the vectors.
!
!    Input, real SA, the multiplier.
!
!    Input, real SX(*), a vector, a multiple of which is to be added to SY.
!
!    Input, integer INCX, the increment in SX between the successive
!    elements that we will use.
!
!    Input/output, real SY(*), a vector, to which is to be added SA 
!    times an entry of SX.
!
!    Input, integer INCY, the increment in SY between the successive
!    elements that we will use.
!
      integer i
      integer incx
      integer incy
      integer ix
      integer iy
      integer m
      integer n
      real sa
      real sx(*)
      real sy(*)
!
      if (n.le.0) return
 
      if (sa.eq.0.0)return
 
      if (incx.ne.1 .or. incy.ne.1 ) then
 
        if (incx.lt.0 ) then
          ix = (-n+1)*incx+1
        else
          ix = 1
        end if
 
        if (incy.lt.0 ) then
          iy = (-n+1)*incy+1
        else
          iy = 1
        end if
 
        do i = 1, n
          sy(iy) = sy(iy)+sa*sx(ix)
          ix = ix+incx
          iy = iy+incy
        end do
 
      else
 
        m = mod(n,4)
 
        do i = 1,m
          sy(i) = sy(i)+sa*sx(i)
        end do
 
        do i = m+1,n,4
          sy(i) = sy(i)+sa*sx(i)
          sy(i+1) = sy(i+1)+sa*sx(i+1)
          sy(i+2) = sy(i+2)+sa*sx(i+2)
          sy(i+3) = sy(i+3)+sa*sx(i+3)
        end do
 
      end if
 
      return
      end
      function tdot ( n, sx, incx, sy, incy )
!
!***********************************************************************
!
!! TDOT computes the inner product of two vectors.
!
!
!  Discussion:
!
!    TDOT is a source code version of the BLAS level 1 routine SDOT,
!    which can be used to compare performance with optimized versions
!    of SDOT supplied by the compiler or operating system.
!
!  Modified:
!
!    28 August 1999
!
!  Parameters:
!
!    Input, integer N, the "logical" number of items in the vectors.
!
!    Input, real SX(*), the first vector.
!
!    Input, integer INCX, the increment in SX between the successive
!    elements that we will use.
!
!    Input/output, real SY(*), the second vector.
!
!    Input, integer INCY, the increment in SY between the successive
!    elements that we will use.
!
!    Output, real TDOT, the sum of the products of the appropriate
!    entries of SX and SY.
!
      integer i
      integer incx
      integer incy
      integer ix
      integer iy
      integer m
      integer n
      real stemp
      real sx(*)
      real sy(*)
      real tdot
!
      tdot = 0.0
 
      if ( n .le. 0 ) then
        return
      end if
 
      if ( incx .ne. 1 .or. incy .ne. 1 ) then

        if ( incx .lt. 0 ) then
          ix = ( - n + 1 ) * incx + 1
        else
          ix = 1
        end if
 
        if ( incy .lt. 0 ) then
          iy = ( - n + 1 ) * incy + 1
        else
          iy = 1
        end if
 
        stemp = 0.0
        do i = 1, n
          stemp = stemp + sx(ix) * sy(iy)
          ix = ix + incx
          iy = iy + incy
        end do
 
        tdot = stemp
 
      else
 
        m = mod ( n, 5)
 
        stemp = 0.0
 
        do i = 1, m
          stemp = stemp 
     &      + sx(i) * sy(i)
        end do
 
        do i = m+1, n, 5
          stemp = stemp
     &      + sx(i)   * sy(i)
     &      + sx(i+1) * sy(i+1)
     &      + sx(i+2) * sy(i+2)
     &      + sx(i+3) * sy(i+3)
     &      + sx(i+4) * sy(i+4)
        end do
 
      end if
 
      tdot = stemp
 
      return
      end
      subroutine terbla ( srname, info )
!
!***********************************************************************
!
!! TERBLA is the source code for the BLAS error handler.
!
!
!  Modified:
!
!    28 August 1999
!
!  Parameters:
!
!    Input, character*6 SRNAME, the routine which called TERBLA.
!
!    Input, integer INFO, the position of the invalid
!    parameter in the parameter-list of the calling routine.
!
      integer info
      character*(*) srname
!
      write ( *, 99999 ) srname, info
 
      stop
99999 format ( ' ** On entry to ', A6, ' parameter number ', i2,
     &         ' had an illegal value' )
      end
      subroutine tgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,
     & ldc)
!
!***********************************************************************
!
!! TGEMM is a source code copy of SGEMM, a BLAS matrix * matrix routine.
!
!
!  TGEMM performs one of the matrix-matrix operations
!
!     C : =  alpha*op( A )*op( B ) + beta*C,
!
!  where  op( X ) is one of
!
!     op( X ) = X   or   op( X ) = X',
!
!  alpha and beta are scalars, anda, B and C are matrices, with op( A )
!  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
!
!  Parameters
!   = =========
!
!  transa - character*1.
!           On entry, transa specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:
!
!              transa = 'N' or 'n',  op( A ) = A.
!
!              transa = 'T' or 't',  op( A ) = A'.
!
!              transa = 'C' or 'c',  op( A ) = A'.
!
!           Unchanged on exit.
!
!  transb - character*1.
!           On entry, transb specifies the form of op( B ) to be used in
!           the matrix multiplication as follows:
!
!              transa = 'N' or 'n',  op( B ) = B.
!
!              transa = 'T' or 't',  op( B ) = B'.
!
!              transa = 'C' or 'c',  op( B ) = B'.
!
!           Unchanged on exit.
!
!  m      - integer.
!           On entry,  m  specifies  the number  of rows  of the  matrix
!           op( A )  and of the  matrix  C.  m  must  be at least  zero.
!           Unchanged on exit.
!
!  N      - integer.
!           On entry,  N  specifies the number  of columns of the matrix
!           op( B ) and the number of columns of the matrix C. N must be
!           at least zero.
!           Unchanged on exit.
!
!  K      - integer.
!           On entry,  K  specifies  the number of columns of the matrix
!           op( A ) and the number of rows of the matrix op( B ). K must
!           be at least  zero.
!           Unchanged on exit.
!
!  alpha  - real .
!           On entry, alpha specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - real array of DIMENSION ( lda, ka ), where ka is
!           k  when  transa = 'N' or 'n',  and is  m  otherwise.
!           Before entry with  transa = 'N' or 'n',  the leading  m by k
!           part of the array  A  must contain the matrix a,  otherwise
!           the leading  k by m  part of the array  A  must contain  the
!           matrix A.
!           Unchanged on exit.
!
!  lda    - integer.
!           On entry, lda specifies the first dimension of A as declared
!           in the calling (sub) program. When  transa = 'N' or 'n' then
!           lda must be at least  max( 1, m ), otherwise  lda must be at
!           least  max( 1, k ).
!           Unchanged on exit.
!
!  B      - real array of DIMENSION ( ldb, kb ), where kb is
!           n  when  transb = 'N' or 'n',  and is  k  otherwise.
!           Before entry with  transb = 'N' or 'n',  the leading  k by n
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  n by k  part of the array  B  must contain  the
!           matrix B.
!           Unchanged on exit.
!
!  ldb    - integer.
!           On entry, ldb specifies the first dimension of B as declared
!           in the calling (sub) program. When  transb = 'N' or 'n' then
!           ldb must be at least  max( 1, k ), otherwise  LDB must be at
!           least  max( 1, n ).
!           Unchanged on exit.
!
!  beta   - real .
!           On entry,  beta  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.
!           Unchanged on exit.
!
!  c      - real array of DIMENSION ( LDC, n ).
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix  C,  except when  beta  is zero, in which
!           case C need not be set on entry.
!           On exit, the array  C  is overwritten by the  m by n  matrix
!           ( alpha*op( A )*op( B ) + beta*C ).
!
!  LDC    - integer.
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).
!           Unchanged on exit.
!
      real alpha, beta
      integer m, n, k, lda, ldb, ldc
      character*1 transa, transb
      real a(lda,*),b(ldb,*),c(ldc,*)
!
      real one, zero
      parameter ( one  = 1.0E+0, zero = 0.0E+0 )
!
      integer i, info, j, ncola, nrowa, nrowb
      logical nota, notb
!
      logical tlsame
!
!     Set  nota  and  notb  as  true if  A  and  B  respectively are not
!     transposed, and set  nrowa, ncola and  nrowb as the number of rows
!     and  columns  of  A  and the  number of rows  of  B  respectively.
!
      nota = tlsame( transa, 'N' )
      notb = tlsame( transb, 'N' )
      if ( nota  ) then
         nrowa = m
         ncola = k
      else
         nrowa = k
         ncola = m
      end if
      if ( notb  ) then
         nrowb = k
      else
         nrowb = n
      end if
!
!     Test the input parameters.
!
      info = 0
      if (      ( .not.nota                 ).and.
     &         ( .not.tlsame( transa, 'T' ) ).and.
     &         ( .not.tlsame( transa, 'C' ) )       ) then
         info = 1
      else if ( ( .not.notb                 ).and.
     &         ( .not.tlsame( transb, 'T' ) ).and.
     &         ( .not.tlsame( transb, 'C' ) )       ) then
         info = 2
      else if ( m.lt.0  ) then
         info = 3
      else if ( n.lt.0  ) then
         info = 4
      else if ( k.lt.0  ) then
         info = 5
      else if ( lda.lt.max( 1,nrowa )  ) then
         info = 8
      else if ( ldb.lt.max( 1,nrowb )  ) then
         info = 10
      else if ( ldc.lt.max( 1, m )  ) then
         info = 13
      end if
 
      if ( info.ne.0  ) then
        call terbla( 'SGEMM ', info )
        return
      end if
!
!  Quick return if possible.
!
      if ( 
     &  m .eq. 0 .or. 
     &  n.eq.0 .or.
     &  ( ( alpha.eq.zero .or. k.eq.0 ) .and. beta.eq.one ) ) then
        return
      end if
!
!  Start the operations.
!
      if ( k.eq.0  ) then
!
!  Form  C : =  beta*C.
!
         if ( beta.eq.zero  ) then
           do j = 1, n
             do i = 1,m
               c(i,j) = zero
             end do
           end do
         else
           do j = 1, n
             do i = 1,m
               c(i,j) = beta*c(i,j)
             end do
           end do
         end if
      else if ( notb  ) then
!
!  Form  C : =  alpha*op( A )*B + beta*C.
!
        do j = 1, n
          call tgemvf(transa, nrowa, ncola,
     &                   alpha,a, lda, b( 1,j), 1,
     &                   beta, c( 1,j), 1 )
        end do
 
      else
!
!  Form  C : =  alpha*op( A )*B' + beta*C.
!
        do j = 1, n
          call tgemvf(transa,nrowa,ncola,alpha,a,lda,b(j,1),ldb,
     &      beta,c(1,j),1)
        end do
 
      end if
 
      return
      end
      subroutine tgemvf(trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
!
!***********************************************************************
!
!! TGEMVF is a source code copy of BLAS SGEMVF, a matrix * vector routine.
!
!
!  TGEMVF performs one of the matrix-vector operations
!
!     y : =  alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
!
!  where alpha and beta are scalars, x and y are vectors and A is an
!  m by n matrix.
!
!  Parameters:
!
!  trans  - character*1.
!           On entry, trans specifies the operation to be performed as
!           follows:
!
!              trans = 'N' or 'n'   y := alpha*A*x + beta*y.
!
!              trans = 'T' or 't'   y := alpha*A'*x + beta*y.
!
!              trans = 'C' or 'c'   y := alpha*A'*x + beta*y.
!
!           Unchanged on exit.
!
!  m      - integer.
!           On entry, m specifies the number of rows of the matrix A.
!           m must be at least zero.
!           Unchanged on exit.
!
!  N      - integer.
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.
!           Unchanged on exit.
!
!  alpha  - real .
!           On entry, alpha specifies the scalar alpha.
!           Unchanged on exit.
!
!  A      - real array of DIMENSION ( lda, n ).
!           Before entry, the leading m by n part of the array A must
!           contain the matrix of coefficients.
!           Unchanged on exit.
!
!  lda    - integer.
!           On entry, lda specifies the first dimension of A as declared
!           in the calling (sub) program. lda must be at least
!           max( 1, m ).
!           Unchanged on exit.
!
!  X      - real array of DIMENSION at least
!           ( 1 + ( n - 1 )*abs( incx ) ) when trans = 'N' or 'n'
!           and at least
!           ( 1 + ( m - 1 )*abs( incx ) ) otherwise.
!           Before entry, the incremented array X must contain the
!           vector x.
!           Unchanged on exit.
!
!  incx   - integer.
!           On entry, incx specifies the increment for the elements of
!           X. incx must not be zero.
!           Unchanged on exit.
!
!  beta   - real .
!           On entry, beta specifies the scalar beta. When BETA is
!           supplied as zero then Y need not be set on input.
!           Unchanged on exit.
!
!  Y      - real array of DIMENSION at least
!           ( 1 + ( m - 1 )*abs( incy ) ) when trans = 'N' or 'n'
!           and at least
!           ( 1 + ( n - 1 )*abs( incy ) ) otherwise.
!           Before entry with beta non-zero, the incremented array Y
!           must contain the vector y. On exit, Y is overwritten by the
!           updated vector y.
!
!  incy   - integer.
!           On entry, incy specifies the increment for the elements of
!           Y. incy must not be zero.
!           Unchanged on exit.
!
      real alpha, beta
      integer incx, incy, lda, m, n
      character*1 trans
!
      real a( lda, * ), x( * ), y( * )
!
      real one, zero
      parameter( one = 1.0E+0, zero = 0.0E+0 )
!
      real temp
      integer i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
!
      logical tlsame
!
!  Test the input parameters.
!
      info = 0
      if ( .not.tlsame( trans, 'N' ).and.
     &         .not.tlsame( trans, 'T' ).and.
     &         .not.tlsame( trans, 'C' )       ) then
         info = 1
      else if ( m.lt.0  ) then
         info = 2
      else if ( n.lt.0  ) then
         info = 3
      else if ( lda.lt.max( 1, m )  ) then
         info = 6
      else if ( incx.eq.0  ) then
         info = 8
      else if ( incy.eq.0  ) then
         info = 11
      end if
      if ( info.ne.0  ) then
         call terbla( 'tgemvf ', info )
         return
      end if
!
!  Quick return if possible.
!
      if ( ( m.eq.0 ) .or. ( n.eq.0 ).or.
     &    ( ( alpha.eq.zero ).and.( beta.eq.one ) ) )
     &   return
!
!  Set  lenx  and  leny, the lengths of the vectors x and y, and set
!  up the start points in  X  and  Y.
!
      if ( tlsame( trans, 'N' )  ) then
         lenx = n
         leny = m
      else
         lenx = m
         leny = n
      end if
      if ( incx.gt.0  ) then
         kx = 1
      else
         kx = 1 - ( lenx - 1 )*incx
      end if
      if ( incy.gt.0  ) then
         ky = 1
      else
         ky = 1 - ( leny - 1 )*incy
      end if
!
!  Start the operations. In this version the elements of A are
!  accessed sequentially with one pass through A.
!
!  First form  y : =  beta*y.
!
      if ( beta.ne.one  ) then
         if ( incy.eq.1  ) then
            if ( beta.eq.zero  ) then
              do i = 1,leny
                y(i) = zero
              end do
            else
              do i = 1, leny
                y(i) = beta*y(i)
              end do
            end if
         else
            iy = ky
            if ( beta.eq.zero  ) then
              do i = 1, leny
                y(iy) = zero
                iy      = iy   + incy
              end do
            else
              do i = 1,leny
                y(iy) = beta*y(iy)
                iy      = iy           + incy
               end do
            end if
         end if
      end if
      if ( alpha.eq.zero )
     &   return
      if ( tlsame( trans, 'N' )  ) then
!
!  Form  y : =  alpha*A*x + y.
!
         jx = kx
         if ( incy.eq.1  ) then
            do j = 1, n
               if ( x( jx ).ne.zero  ) then
                  temp = alpha*x( jx )
                  do i = 1, m
                     y(i) = y( i ) + temp*a(i,j)
                  end do
               end if
               jx = jx + incx
            end do
         else
            do j = 1, n
               if ( x( jx ).ne.zero  ) then
                  temp = alpha*x( jx )
                  iy   = ky
                  do i = 1, m
                     y(iy) = y(iy) + temp*a(i,j)
                     iy      = iy      + incy
                 end do
               end if
               jx = jx + incx
            end do
         end if
      else
!
!  Form  y : =  alpha*A'*x + y.
!
         jy = ky
         if ( incx.eq.1  ) then
           do j = 1, n
               temp = zero
               do i = 1, m
                  temp = temp + a(i,j)*x(i)
               end do
               y( jy ) = y( jy ) + alpha*temp
               jy      = jy      + incy
           end do
         else
            do j = 1, n
               temp = zero
               ix   = kx
               do i = 1,m
                  temp = temp + a(i,j)*x(ix)
                  ix   = ix   + incx
               end do
               y( jy ) = y( jy ) + alpha*temp
               jy      = jy      + incy
          end do
        end if
      end if
 
      return
      end
      function tlsame(ca,cb)
!
!***********************************************************************
!
!! TLSAME is a source code copy of BLAS LSAME, testing character equality.
!
!
!  Parameters:
!
!  cb is assumed to be an upper case letter. tlsame returns .true. if
!  CA is either the same as cb or the equivalent lower case letter.
!
!  N.B. This version of the routine is only correct for ASCII code.
!       Installers must modify the routine for other character-codes.
!
!       For EBCDIC systems the constant ioff must be changed to -64.
!       For CDC systems using 6-12 bit representations, the system-
!       specific code in comments must be activated.
!
!  CA     - character*1
!  cb     - character*1
!           On entry, CA and cb specify characters to be compared.
!           Unchanged on exit.
!
      integer ioff
      parameter (ioff = 32)
!
      character*1 ca
      character*1 cb
      logical tlsame
!
!  Test if the characters are equal
!
      tlsame = ca.eq.cb
!
!  Now test for equivalence
!
      if ( .not. tlsame ) then
        tlsame = ichar(ca)-ioff.eq.ichar(cb)
      end if
 
      return
      end
      subroutine uijk ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! UIJK multiplies A = B*C using index order IJK and I unrolling to depth 4.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer nroll
      parameter (nroll = 4)
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer ihi
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        ihi = ( n / nroll ) * nroll
        do i = 1, ihi, nroll
          do j = 1, n
            do k = 1, n
              a(i,k)   = a(i,k)   + b(i,j)   * c(j,k)
              a(i+1,k) = a(i+1,k) + b(i+1,j) * c(j,k)
              a(i+2,k) = a(i+2,k) + b(i+2,j) * c(j,k)
              a(i+3,k) = a(i+3,k) + b(i+3,j) * c(j,k)
            end do
          end do
        end do
!
!  Take care of the few cases we missed if N is not a multiple of 4.
!
        do i = ihi+1, n
          do j = 1, n
            do k = 1, n
              a(i,k) = a(i,k) + b(i,j) * c(j,k)
            end do
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine umxma ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! UMXMA multiplies A = B*C using optimized MXMA.
!
!
!  Discussion:
!
!    Since the routine MXMA is only available on the Cray, in the
!    SCILIB library, the statement
!
!      call mxma(...)
!
!    should be commented out in versions of MATMUL that are to run
!    on other machines.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer inca
      integer incb
      integer incc
      integer irep
      integer nrep
      real time1
      real time2
      real ttime
!
      inca = 1
      incb = 1
      incc = 1

      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )

!       call mxma ( b, incb, lda, c, incc, lda, a, inca, lda, n, n, n )

        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine usaxpyc ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! USAXPYC multiplies A = B*C columnwise, using optimized SAXPY.
!
!
!  SAXPY is used to carry out the multiplication "columnwise".
!
!  This is equivalent to the following "JKI" code:
!
!       do j = 1, n
!         do k = 1, n
!           do i = 1, n
!              a(i,k) = a(i,k)+b(i,j)*c(j,k)
!            end do
!          end do
!        end do
!
!  Except on the Cray and SGI/IRIS, the statement "call saxpy" below
!  should be commented out.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do j = 1, n
          do k = 1, n
            call saxpy ( n, c(j,k), b(1,j), 1, a(1,k), 1 )
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine usaxpyr ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! USAXPYR multiplies A = B*C "rowwise", using optimized SAXPY.
!
!
!  This is equivalent to the following "IJK" code:
!
!     do i = 1, n
!       do j = 1, n
!          do k = 1, n
!            a(i,k) = a(i,k)+b(i,j)*c(j,k)
!          end do
!        end do
!      end do
!
!  Except on the Cray and SGI/IRIS, the statement "call saxpy" below
!  should be commented out.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do j = 1, n
            call saxpy ( n, b(i,j), c(j,1), lda, a(i,1), lda )
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine usdot ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! USDOT multiplies A = B*C using optimized SDOT.
!
!
!  This is equivalent to the following "IKJ" code:
!
!       do i = 1, n
!         do k = 1, n
!           do j = 1, n
!             a(i,k) = a(i,k)+b(i,j)*c(j,k)
!          end do
!        end do
!      end do
!
!  Except on the Cray or SGI/IRIS, the call to SDOT should be commented out.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer k
      integer nrep
      real sdot
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do k = 1, n
            a(i,k) = sdot ( n, b(i,1), lda, c(1,k), 1 )
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine usgemm ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! USGEMM multiplies A = B*C using optimized SGEMM.
!
!
!  Except on the Cray or SGI/IRIS, the call to SGEMM should be commented out.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real alpha
      real b(lda,n)
      real beta
      real c(lda,n)
      integer irep
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        alpha = 1.0
        beta = 0.0
 
        call matmul_cpu_timer ( time1 )

        call sgemm ( 'n', 'n', n, n, n, alpha, b, lda, c, lda, 
     &    beta, a, lda )

        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine usgemms ( a, b, c, lda, n, nrep, nwork, ttime )
!
!***********************************************************************
!
!! USGEMMS multiplies A = B*C using optimized SGEMMS.
!
!
!  SGEMMS is the Cray SCILIB variant of the BLAS3 routine SGEMM.  
!  The difference is that SGEMMS uses Strassen's algorithm.
!
!  Except on the Cray, the call to SGEMMS should be commented out,
!  as well as the statement "REAL WORK(NWORK)".
!
!  Notice that the dimensioning of WORK is illegal in standard FORTRAN.
!  A Cray extension allows this creation of scratch arrays.
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Input, integer NWORK, the size to be used for the locally
!    declared workspace array WORK.  Apparently, a recommended value
!    is 3*N*N, rather a lot, really.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real alpha
      real b(lda,n)
      real beta
      real c(lda,n)
      integer irep
      integer nrep
      integer nwork
      real time1
      real time2
      real ttime
!     real work(nwork)
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        alpha = 1.0
        beta = 0.0
 
        call matmul_cpu_timer ( time1 )

!       call sgemms ( 'n', 'n', n, n, n, alpha, b, lda, c, lda, beta, a, 
!    &    lda, work )

        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine utaxpyc ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! UTAXPYC multiplies A = B*C columnwise, using unoptimized SAXPY.
!
!
!  This is equivalent to the following "JKI" code:
!
!       do j = 1, n
!         do k = 1, n
!           do i = 1, n
!             a(i,k) = a(i,k)+b(i,j)*c(j,k)
!           end do
!         end do
!       end do
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer irep
      integer j
      integer k
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do j = 1, n
          do k = 1, n
            call taxpy ( n, c(j,k), b(1,j), 1, a(1,k), 1 )
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine utaxpyr ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! UTAXPYR multiplies A = B*C rowwise using source code SAXPY.
!
!
!  This is equivalent to the following "IJK" code:
!
!        do i = 1, n
!          do j = 1, n
!            do k = 1, n
!              a(i,k) = a(i,k)+b(i,j)*c(j,k)
!            end do
!          end do
!        end do
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer j
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do j = 1, n
            call taxpy ( n, b(i,j), c(j,1), lda, a(i,1), lda )
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1

      end do
 
      return
      end
      subroutine utdot ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! UTDOT multiplies A = B * C using source code SDOT.
!
!
!  Discussion:
!
!    This is equivalent to the following "IKJ" code:
!
!      do i = 1, n
!        do k = 1, n
!          do j = 1, n
!            a(i,k) = a(i,k) + b(i,j) * c(j,k)
!          end do
!        end do
!      end do
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real b(lda,n)
      real c(lda,n)
      integer i
      integer irep
      integer k
      integer nrep
      real tdot
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        call matmul_cpu_timer ( time1 )
 
        do i = 1, n
          do k = 1, n
            a(i,k) = tdot ( n, b(i,1), lda, c(1,k), 1 )
          end do
        end do
 
        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
      subroutine utgemm ( a, b, c, lda, n, nrep, ttime )
!
!***********************************************************************
!
!! UTGEMM multiplies A = B*C using SGEMM.
!
!
!  Modified:
!
!    28 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Workspace, real A(LDA,N), B(LDA,N), C(LDA,N), the three matrices
!    used in the multiplication.
!
!    Input, integer LDA, the leading dimension used for arrays.
!
!    Input, integer N, the number of rows and columns in the matrices.
!
!    Input, integer NREP, the number of times the multiplication should
!    be carried out.
!
!    Output, real TTIME, an estimate of the CPU time in seconds required
!    for the matrix multiplications.
!
      integer lda
      integer n
!
      real a(lda,n)
      real alpha
      real b(lda,n)
      real beta
      real c(lda,n)
      integer irep
      integer nrep
      real time1
      real time2
      real ttime
!
      ttime = 0.0
 
      do irep = 1, nrep
 
        call rset ( a, b, c, lda, n )
 
        alpha = 1.0
        beta = 0.0
 
        call matmul_cpu_timer ( time1 )

        call tgemm ( 'n', 'n', n, n, n, alpha, b, lda, c, lda, beta,
     &    a, lda )

        call matmul_cpu_timer ( time2 )
        ttime = ttime + time2 - time1
 
      end do
 
      return
      end
