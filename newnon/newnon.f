c  newnon.f  15 December 1998
c
      program main

c*********************************************************************72
c
cc MAIN is the main program for NEWNON.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer maxeqn
      integer maxhis

      parameter ( maxhis = 100 )
      parameter ( maxeqn = 10 )

      double precision abserr
      character*80 comman
      double precision damp
      double precision delx(maxeqn)
      double precision detjac
      double precision difjac
      double precision epmach
      double precision enorm(0:maxhis)
      double precision fnorm0
      double precision fnorm1
      double precision fpfact(maxeqn,maxeqn)
      double precision fprime(maxeqn,maxeqn)
      double precision fprime1(maxeqn,maxeqn)
      double precision fprime2(maxeqn,maxeqn)
      double precision fx(maxeqn)
      double precision fxback(maxeqn)
      double precision fxplus(maxeqn)
      integer i
      integer idamp
      integer ierror
      integer ihate
      integer iknow
      integer ilike
      integer inorm
      integer ipivot(maxeqn)
      integer iprint
      integer lenc
      integer lenchr
      logical leqi
      character*80 line
      integer maxstp
      integer method
      integer mprob
      integer neqn
      integer newmat
      integer nfacts
      integer nfpval
      integer nfxval
      integer nprob
      integer nsolve
      integer nstep
      logical pause
      double precision relerr
      double precision snorm0
      double precision snorm1
      character title*80
      double precision work(maxeqn)
      double precision xnew(maxeqn)
      double precision xdelt(maxeqn)
      double precision xnorm0
      double precision xnorm1
      double precision xold(maxeqn)
      double precision xstart(maxeqn)
      double precision xtrue(maxeqn)
c
c  Initialize data.
c
      call init ( abserr, damp, delx, difjac, epmach, fnorm0, fnorm1, 
     &  fprime, fx, idamp, ierror, ihate, iknow, ilike, inorm, ipivot, 
     &  iprint, maxeqn, maxstp, method, neqn, newmat, nfacts, nfpval,
     &  nfxval, nprob, nsolve, nstep, pause, relerr, snorm0, 
     &  snorm1, title, xnew, xnorm0, xnorm1, xold, xstart, xtrue )
c
c  Get the number of problems available.
c
      call p00_problem_num ( mprob )
c
c  Say hello.
c
      call timestamp ( )
      write ( *, * ) ' '
      write ( *, * ) 'NEWNON'
      write ( *, * ) '  An interactive program for solving systems'
      write ( *, * ) '  of nonlinear equations.'
c
c  Get the next command
c
   10 continue
 
      write ( *, * ) ' '
      write ( *, * ) 'Command? (H for help)'
      read ( *, '(a)' ) comman
      write ( *, '(a)' ) 'Command: "' // trim ( comman ) // '".'
 
11    continue
c
c  A = begin new problem
c
      if ( leqi ( comman, 'a' ) ) then
 
        call problem_choice ( mprob, nprob, title )
 
        call begin ( iknow, maxeqn, neqn, nfacts, nfpval, nfxval, 
     &    nprob, nsolve, nstep, xnew, xold, xstart, xtrue )
c
c  B = begin iteration
c
      else if ( leqi ( comman, 'b' ) ) then
 
        if ( nstep .gt. 0 ) then
          write ( *, * ) 'The previous iteration is now cancelled.'
          nfacts = 0
          nfpval = 0
          nfxval = 0
          nsolve = 0
          nstep = 0
        end if
 
        write ( *, * ) 'You may now iterate with the "I" command.'
c
c  G = Compare analytic jacobian and iteration matrix at
c  current point.
c
      else if ( leqi ( comman, 'g' ) ) then
 
        call chkjac ( difjac, fprime, fprime1, fprime2, fxback, fxplus, 
     &    method, neqn, nfpval, nfxval, nprob, nstep, 
     &    xdelt, xnew )
c
c  H = help
c
      else if ( leqi ( comman, 'h' ) ) then
 
        call help
c
c  I = iterate
c
      else if ( leqi ( comman, 'i' ) ) then
 
        if ( nstep .eq. 0 ) then
          do i = 1, neqn
            xnew(i) = xstart(i)
          end do
        end if
 
        call newcon ( abserr, damp, delx, detjac, difjac, enorm, 
     &    fnorm0, fnorm1, fpfact, fprime, fx, fxback, fxplus, idamp, 
     &    ihate, iknow, ilike, inorm, ipivot, iprint, maxhis, 
     &    maxstp, method, neqn, newmat, nfacts, nfpval, nfxval, nprob, 
     &    nsolve, nstep, pause, relerr, snorm0, snorm1, work, 
     &    xnew, xdelt, xnorm0, xnorm1, xold, xtrue )
c
c  J = print iteration matrix.
c
      else if ( leqi ( comman, 'j' ) ) then
 
        if ( nstep .eq. 0 ) then
 
          write ( *, * ) ' '
          write ( *, * ) 'No iteration is in progress.'
          write ( *, * ) 'We will display the iteration matrix'
          write ( *, * ) 'at the current starting point.'
          write ( *, * ) ' '
 
          call getjac ( difjac, fprime, fxback, fxplus, method, neqn, 
     &      nfpval, nfxval, nprob, nstep, xdelt, xnew )

          call lufact ( detjac, fpfact, fprime, ierror, ipivot,
     &      neqn, nfacts )

        end if
 
        call prijac ( detjac, fprime, neqn )
c
c  M = Set method
c
      else if ( leqi ( comman, 'm' ) ) then

        call setmth ( method )
c
c  P = print current parameters
c
      else if ( leqi ( comman, 'p' ) ) then
 
        call prival ( abserr, difjac, epmach, fx, idamp, iknow, inorm, 
     &    iprint, maxeqn, maxstp, method, mprob, neqn, newmat, nfacts, 
     &    nfpval, nfxval, nprob, nsolve, nstep, relerr, title, xnew, 
     &    xstart, xtrue )
c
c  Q = quit
c
      else if ( leqi ( comman, 'q' ) ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'NEWNON:'
        write ( *, '(a)' ) '  Normal end of execution.'
        write ( *, '(a)' ) ' '
        call timestamp ( )
        stop
c
c  S = set various parameters.
c
      else if ( leqi ( comman, 's' ) ) then
 
        call setter ( abserr, difjac, idamp, inorm, iprint, maxstp, 
     &    newmat, pause, relerr )
c
c  X = set x
c
      else if ( leqi ( comman, 'x' ) ) then
 
        if ( nstep .gt. 0 ) then
          write ( *, * ) 'The previous iteration is now cancelled.'
          nfacts = 0
          nfpval = 0
          nfxval = 0
          nsolve = 0
          nstep = 0
        end if
 
        write ( *, * ) 'Enter new x'
        read ( *, * ) ( xstart(i), i = 1, neqn )
 
        do i = 1, neqn
          xnew(i) = xstart(i)
        end do
 
        do i = 1, neqn
          xold(i) = xstart(i)
        end do
c
c  # = Begin or end a comment.
c
      else if ( comman(1:1) .eq. '#' ) then
 
        write ( *, '(a)' ) trim ( comman )
c
c  Unrecognized command
c
      else if ( comman .ne. ' ' ) then
        write ( *, * ) ' '
        write ( *, * ) 'NEWNON did not recognize your command:'
        write ( *, '(a)' ) '"' // trim ( comman ) // '".'
      end if
 
      go to 10
 
      end
      subroutine accept ( abserr, fnorm1, ilike, nstep, relerr, snorm1, 
     &  xnorm1 )

c*********************************************************************72
c
cc ACCEPT decides whether or not to accept the current iterate.
c
c  Modified:
c
c    10 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision ABSERR, the absolute error tolerance.
c
      implicit none

      double precision abserr
      double precision fnorm1
      integer ilike
      integer nstep
      double precision relerr
      double precision snorm1
      double precision xnorm1

      ilike = 0
 
      if ( nstep .eq. 0 ) then
 
        if ( fnorm1 .le. abserr ) then
          ilike = 1
        end if
 
      else
 
        if ( 
     &    fnorm1 .le. abserr .and.
     &    snorm1 .le. relerr * ( xnorm1 + abserr ) ) then
          ilike = 1
        end if
 
      end if
 
      if ( fnorm1 .eq. 0.0D+00 ) then
        ilike = 1
      end if
 
      return
      end
      subroutine begin ( iknow, maxeqn, neqn, nfacts, nfpval, 
     &  nfxval, nprob, nsolve, nstep, xnew, xold, xstart, xtrue )

c*********************************************************************72
c
cc BEGIN sets up a given problem.
c
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer maxeqn

      integer i
      integer iknow
      integer neqn
      integer nfacts
      integer nfpval
      integer nfxval
      integer nmin
      integer nprob
      integer nsolve
      integer nstep
      double precision xnew(maxeqn)
      double precision xold(maxeqn)
      double precision xstart(maxeqn)
      double precision xtrue(maxeqn)
c
c  Get the number of equations.
c
      call p00_n ( nprob, neqn )
 
      if ( neqn .lt. 0 ) then
        nmin = abs ( neqn )
        write ( *, * ) ' '
        write ( *, * ) 
     &    'This problem has a variable number of equations.'
        write ( *, * ) 'The minimum legal value is ', nmin
        write ( *, * ) 'The maximum value for this program is ', maxeqn
        write ( *, * ) 'Enter your choice for the number of equations:'
        read ( *, * ) neqn
      end if
 
      if ( neqn .eq. 0 ) then
        neqn = 4
      end if
c
c  Set the starting point.
c
      call p00_start ( nprob, neqn, xstart )
 
      do i = 1, neqn
        xnew(i) = xstart(i)
      end do
 
      do i = 1, neqn
        xold(i) = xstart(i)
      end do
c
c  Set the true solution, if known.
c
      call p00_sol ( nprob, iknow, neqn, xtrue )
c
c  Initialize work counters.
c
      nfxval = 0
      nfpval = 0
      nfacts = 0
      nsolve = 0
      nstep = 0
 
      return
      end
      subroutine capchr ( c )

c*********************************************************************72
c
cc CAPCHR capitalizes a single character.
c
c  Author:
c
c    John Burkardt
c
c  Modified:
c
c    19 July 1998
c
c  Parameters:
c
c    Input/output, character*1 C, the character to capitalize.
c
      implicit none

      character*1 c
      integer itemp

      itemp = ichar ( c )

      if ( 97 .le. itemp .and. itemp .le. 122 ) then
        c = char ( itemp - 32 )
      end if

      return
      end
      subroutine chkjac ( difjac, fprime, fprime1, fprime2, fxback, 
     &  fxplus, method, neqn, nfpval, nfxval, nprob, 
     &  nstep, xdelt, xnew )

c*********************************************************************72
c
cc CHKJAC compares the current iteration matrix to the analytic jacobian.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer neqn

      double precision dif
      double precision difjac
      double precision fprime(neqn,neqn)
      double precision fprime1(neqn,neqn)
      double precision fprime2(neqn,neqn)
      double precision fxback(neqn)
      double precision fxplus(neqn)
      integer i
      integer j
      integer method
      integer nfpval
      integer nfxval
      integer nprob
      integer nstep
      double precision xdelt(neqn)
      double precision xnew(neqn)
c
c  Store the analytic jacobian in FPRIME1.
c
      call p00_jac ( nprob, neqn, fprime1, xnew )
      nfpval = nfpval + 1
c
c  If we're carrying out an iteration, then FPRIME contains the
c  current iteration matrix.
c  If not, then we compute the iteration matrix via the currently
c  chosen method.
c
      if ( nstep .gt. 0 ) then

        do i = 1, neqn
          do j = 1, neqn
            fprime2(i,j) = fprime(i,j)
          end do
        end do

      else

        call getjac ( difjac, fprime2, fxback, fxplus, method, neqn, 
     &    nfpval, nfxval, nprob, nstep, xdelt, xnew )

      end if
c
c  Now compare the two matrices.
c
      write ( *, * ) ' '
      write ( *, * )
     &      '   i  j       Jacobian   Iteration Matrix  difference'
      write ( *, * ) ' '
      do i = 1, neqn
        write ( *, * ) ' '
        do j = 1, neqn
          dif = fprime1(i,j) - fprime2(i,j)
          write ( *, '(i3, i3, 3g14.6)' )
     &      i, j, fprime1(i,j), fprime2(i,j), dif
        end do
      end do
 
      return
      end
      subroutine chrcat ( chrpre, chrpst, chrout )

c*********************************************************************72
c
cc CHRCAT concatenates two strings to make a third string.
c
c  This is "roughly" the same as the following operation:
c    CHROUT = CHRPRE//CHRPST.
c  However, this routine was written to get around some of the
c  problems of using the FORTRAN "//" operator.
c
c  First of all, the "//" operator concatenates the entirety of two
c  strings, even if they contain unwanted trailing blanks.  If
c  CHRPRE was a ten character string, but only contained "dog" and
c  CHRPST was a ten character string that contained "cat", the
c  CHRPRE//CHRPST would create a twenty character string
c  "dog       cat       " whereas CHRCAT would create "dogcat".
c
c  Moreover, in the common case where you want to prefix or postfix
c  a string and put it back into the same variable, the "//"
c  operator won't work unless you restrict the range of the
c  operands.  For instance, the dog-cat example above might be
c  written CHRPRE = CHRPRE(1:3)//CHRPST(1:3).  This is tedious.
c
c  CHRCAT is designed to work for character strings or constants,
c  and to take as much work as possible off the user's shoulders.
c  You simply pass in the two strings to be concatenated, and the
c  string into which the concatenation is to be stored.  CHRCAT
c  will only handle up to a total of 255 nonblank characters.
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) CHRPRE, the "prefix" string.
c
c    Input, character*(*) CHRPST, the "postfix" string.
c
c    Output, character*(*) CHROUT, the string made by
c    concatenating CHRPRE and CHRPST, ignoring any trailing blanks.
c
      implicit none

      integer maxtmp
      parameter ( maxtmp = 255 )

      character*(*) chrpre
      character*(*) chrpst
      character*(*) chrout
      character*(maxtmp) chrtmp
      integer iout
      integer ipre
      integer ipst
      integer lenchr

      iout = len ( chrout )
      iout = min ( iout, maxtmp )

      ipre = lenchr ( chrpre )
      ipre = min ( ipre, iout )

      ipst = lenchr ( chrpst )
      ipst = min ( ipst, iout-ipre )

      chrtmp = ' '
      chrtmp(1:ipre+ipst) = chrpre(1:ipre) // chrpst(1:ipst)
      chrout = chrtmp

      return
      end
      subroutine dge_fa ( a, lda, n, ipivot, info )

c*********************************************************************72
c
cc DGE_FA factors a general matrix.
c
c  Discussion:
c
c    DGE_FA is a simplified version of the LINPACK routine DGEFA.
c
c  Parameters:
c
c    Input/output, double precision A(LDA,N), the matrix to be factored.
c    On output, A contains an upper triangular matrix and the multipliers
c    which were used to obtain it.  The factorization can be written
c    A = L * U, where L is a product of permutation and unit lower
c    triangular matrices and U is upper triangular.
c
c    Input, integer LDA, the leading dimension of the array.
c    LDA must be at least N.
c
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Output, integer IPIVOT(N), a vector of pivot indices.
c
c    Output, integer INFO, singularity flag.
c    0, no singularity detected.
c    nonzero, the factorization failed on the INFO-th step.
c
      implicit none

      integer lda
      integer n

      double precision a(lda,n)
      integer i
      integer info
      integer ipivot(n)
      integer j
      integer k
      integer l
      double precision t

      info = 0

      do k = 1, n-1
c
c  Find L, the index of the pivot row.
c
        l = k
        do i = k+1, n
          if ( abs ( a(i,k) ) .gt. abs ( a(l,k) ) ) then
            l = i
          end if
        end do

        ipivot(k) = l
c
c  If the pivot index is zero, the algorithm has failed.
c
        if ( a(l,k) .eq. 0.0D+00 ) then
          info = k
          write ( *, * ) ' '
          write ( *, * ) 'DGE_FA - Fatal error!'
          write ( *, * ) '  Zero pivot on step ', info
          return
        end if
c
c  Interchange rows L and K if necessary.
c
        if ( l .ne. k ) then
          t = a(l,k)
          a(l,k) = a(k,k)
          a(k,k) = t
        end if
c
c  Normalize the values that lie below the pivot entry A(K,K).
c
        do i = k+1, n
          a(i,k) = - a(i,k) / a(k,k)
        end do
c
c  Row elimination with column indexing.
c
        do j = k+1, n

          if ( l .ne. k ) then
            t = a(l,j)
            a(l,j) = a(k,j)
            a(k,j) = t
          end if

          do i = k+1, n
            a(i,j) = a(i,j) + a(i,k) * a(k,j)
          end do

        end do

      end do

      ipivot(n) = n

      if ( a(n,n) .eq. 0.0D+00 ) then
        info = n
        write ( *, * ) ' '
        write ( *, * ) 'DGE_FA - Fatal error!'
        write ( *, * ) '  Zero pivot on step ', info
      end if

      return
      end
      subroutine dge_sl ( a, lda, n, ipivot, b, job )

c*********************************************************************72
c
cc DGE_SL solves a system factored by DGE_FA.
c
c  Parameters:
c
c    Input, double precision A(LDA,N), the LU factors from DGE_FA.
c
c    Input, integer LDA, the leading dimension of the array.
c    LDA must be at least N.
c
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input, integer IPIVOT(N), the pivot vector from DGE_FA.
c
c    Input/output, double precision B(N).
c    On input, the right hand side vector.
c    On output, the solution vector.
c
c    Input, integer JOB, specifies the operation.
c    0, solve A*X=B.
c    nonzero, solve transpose(A)*X=B.
c
      implicit none

      integer lda
      integer n

      double precision a(lda,n)
      double precision b(n)
      integer ipivot(n)
      integer j
      integer job
      integer k
      integer l
      double precision t
c
c  Solve A * X = B.
c
      if ( job .eq. 0 ) then
c
c  Solve PL * Y = B.
c
        do k = 1, n-1

          l = ipivot(k)

          t = b(l)
          if ( l .ne. k ) then
            b(l) = b(k)
            b(k) = t
          end if

          do j = k+1, n
            b(j) = b(j) + t * a(j,k)
          end do

        end do
c
c  Solve U * X = Y.
c
        do k = n, 1, -1
          b(k) = b(k) / a(k,k)
          do j = 1, k-1
            b(j) = b(j) - a(j,k) * b(k)
          end do
        end do
c
c  Solve transpose ( A ) * X = B.
c
      else
c
c  Solve transpose ( U ) * Y = B.
c
        do k = 1, n
          t = 0.0D+00
          do j = 1, k-1
            t = t + a(j,k) * b(j)
          end do
          b(k) = ( b(k) - t ) / a(k,k)
        end do
c
c  Solve transpose ( PL ) * X = Y.
c
        do k = n-1, 1, -1

          t = 0.0D+00
          do j = k+1, n
            t = t + a(j,k) * b(j)
          end do

          b(k) = b(k) + t

          l = ipivot(k)
          if ( l .ne. k ) then
            t = b(l)
            b(l) = b(k)
            b(k) = t
          end if

        end do

      end if

      return
      end
      subroutine getjac ( difjac, fprime, fxback, fxplus, method, neqn, 
     &  nfpval, nfxval, nprob, nstep, xdelt, xnew )

c*********************************************************************72
c
cc GETJAC computes the iteration matrix at the current point X.
c
c  Discussion:
c
c    GETJAC can be used for Newton's method with analytic jacobian, and 
c    for Newton's method with finite difference approximated jacobians.
c
c    For Broyden's method, GETJAC may only be used to initialize the
c    iteration matrix.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer neqn

      double precision difjac
      double precision fprime(neqn,neqn)
      double precision fxback(neqn)
      double precision fxplus(neqn)
      integer i
      integer j
      integer method
      integer nfpval
      integer nfxval
      integer nprob
      integer nstep
      double precision xdelt(neqn)
      double precision xnew(neqn)
c
c  Newton's method with analytic jacobian
c
      if ( method .eq. 2 ) then
 
        call p00_jac ( nprob, neqn, fprime, xnew )
        nfpval = nfpval + 1
c
c  Broyden's method
c    Updates to the iteration matrix are made elsewhere.
c    There are two starting options:
c      Start with identity.
c      Start with jacobian.
c
      else if ( method .eq. 3 ) then
 
        if ( nstep .le. 1 ) then
 
          do i = 1, neqn
            do j = 1, neqn
              if ( i .eq. j ) then
                fprime(i,j) = 1.0D+00
              else
                fprime(i,j) = 0.0D+00
              end if
            end do
          end do
 
          nfpval = nfpval + 1
 
        end if
 
      else if ( method .eq. 4 ) then
 
        if ( nstep .le. 1 ) then
          call p00_jac ( nprob, neqn, fprime, xnew )
          nfpval = nfpval + 1
        end if
c
c  Finite difference approximation to jacobian
c
      else
 
        call jacdif ( difjac, fprime, fxback, fxplus, 
     &    method, neqn, nfxval, nprob, xdelt, xnew )
 
      end if
 
      return
      end
      subroutine help

c*********************************************************************72
c
cc HELP prints out a list of legal commands.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      write ( *, * ) ' '
      write ( *, * ) 'A = Choose new problem.'
      write ( *, * ) 'B = Force iteration to start from scratch.'
      write ( *, * ) 'G = Compare jacobian and iteration matrix.'
      write ( *, * ) 'H = Help (list commands).'
      write ( *, * ) 'I = Begin or continue iteration.'
      write ( *, * ) 'J = print iteration matrix.'
      write ( *, * ) 'M = choose method.'
      write ( *, * ) 'P = print current parameters.'
      write ( *, * ) 'Q = quit.'
      write ( *, * ) 'S = set various parameters.'
      write ( *, * ) 'X = set current X approximation.'
      write ( *, * ) '# = Begin or end a comment.'
 
      return
      end
      subroutine init ( abserr, damp, delx, difjac, epmach, fnorm0, 
     &  fnorm1, fprime, fx, idamp, ierror, ihate, iknow, ilike, 
     &  inorm, ipivot, iprint, maxeqn, maxstp, method, neqn, newmat, 
     &  nfacts, nfpval, nfxval, nprob, nsolve, nstep, pause, 
     &  relerr, snorm0, snorm1, title, xnew, xnorm0, xnorm1, xold, 
     &  xstart, xtrue )

c*********************************************************************72
c
cc INIT initializes data.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision ABSERR, the absolute error tolerance.
c
      implicit none

      integer maxeqn

      double precision abserr
      double precision damp
      double precision delx(maxeqn)
      double precision difjac
      double precision epmach
      double precision fnorm0
      double precision fnorm1
      double precision fprime(maxeqn,maxeqn)
      double precision fx(maxeqn)
      integer i
      integer idamp
      integer ierror
      integer ihate
      integer iknow
      integer ilike
      integer inorm
      integer ipivot(maxeqn)
      integer iprint
      integer j
      integer maxstp
      integer method
      integer neqn
      integer newmat
      integer nfacts
      integer nfpval
      integer nfxval
      integer nprob
      integer nsolve
      integer nstep
      logical pause
      double precision relerr
      double precision snorm0
      double precision snorm1
      character*80 title
      double precision xnew(maxeqn)
      double precision xnorm0
      double precision xnorm1
      double precision xold(maxeqn)
      double precision xstart(maxeqn)
      double precision xtrue(maxeqn)
c
c  Get the machine accuracy.
c
      epmach = 1.0D+00

   10 continue

      epmach = 0.5D+00 * epmach
      if ( 1.0D+00 + epmach .gt. 1.0D+00 ) then
        go to 10
      end if

      epmach = epmach * 2.0D+00
c
c  Initialize data.
c
      abserr = 0.00001D+00
      damp = 1.0D+00

      do i = 1, maxeqn
        delx(i) = 0.0D+00
      end do

      difjac = sqrt ( epmach )
      fnorm0 = 0.0D+00
      fnorm1 = 0.0D+00

      do i = 1, maxeqn
        do j = 1, maxeqn
          fprime(i,j) = 0.0D+00
        end do
      end do

      do i = 1, maxeqn
        fx(i) = 0.0D+00
      end do

      idamp = 0
      ierror = 0
      ihate = 0
      iknow = 0
      ilike = 0
      inorm = 0

      do i = 1, maxeqn
        ipivot(i) = 0
      end do

      iprint = 1
      maxstp = 10
      method = 2
      newmat = 1
      neqn = 0
      nfacts = 0
      nfpval = 0
      nfxval = 0
      nprob = 0
      nsolve = 0
      nstep = 0
      pause = .false.
      relerr = 0.00001D+00
      snorm0 = 0.0D+00
      snorm1 = 0.0D+00
      title = ' '

      do i = 1, maxeqn
        xnew(i) = 0.0D+00
      end do

      xnorm0 = 0.0D+00
      xnorm1 = 0.0D+00

      do i = 1, maxeqn
        xold(i) = 0.0D+00
      end do

      do i = 1, maxeqn
        xstart(i) = 0.0D+00
      end do

      do i = 1, maxeqn
        xtrue(i) = 0.0D+00
      end do
 
      return
      end
      subroutine jacdif ( difjac, fprime, fxback, fxplus, method, neqn, 
     &  nfxval, nprob, xdelt, xnnew )

c*********************************************************************72
c
cc JACDIF approximates the jacobian matrix using a difference method.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, REAL DIFJAC, a "small" number, such as the
c    square root of the machine epsilon, which is used to
c    choose points near X at which to evaluate the function.
c
c    Output, REAL FPRIME(NROW, NEQN), contains the approximate jacobian.
c
c    Workspace, REAL FXBACK(NEQN), FXPLUS(NEQN).
c
c    Input, INTEGER METHOD, backward, centered, or forward difference switch.
c    -1, backward differences are used.
c     0, centered differences are used.
c    +1, forward differences are used.
c
c    Input, INTEGER NEQN, the number of equations, and hence, 
c    number of rows and columns in FPRIME.
c
c    Input/output, INTEGER NFXVAL, the number of times the
c    function routine has been called.  DIFJAC assumes that
c    this number is correct on input, and adds to it the
c    number of function evaluations made on this call.
c
c    Input, INTEGER NPROB, the number of the problem being solved.
c
c    Input, INTEGER NROW, the formal first dimension of FPRIME.
c
c    Workspace, REAL XDELT(NEQN).
c
c    Input, REAL XNNEW(NEQN), the point at which the jacobian
c    is to be approximated.
c
      implicit none

      integer neqn

      double precision difjac
      double precision fprime(neqn,neqn)
      double precision fxback(neqn)
      double precision fxplus(neqn)
      double precision h
      integer i
      integer j
      integer method
      integer nfxval
      integer nprob
      double precision xdelt(neqn)
      double precision xnnew(neqn)
      double precision xnorm

      do j = 1, neqn
 
        xnorm = abs ( xnnew(j) )
c
c  Evaluate the function which appears with negative sign
c
        if ( method .eq. -1 .or. method .eq. 0 ) then
          h = - difjac * ( 1.0D+00 + xnorm )
        else if ( method .eq. 1 ) then
          h = 0.0D+00
        end if
 
        do i = 1, neqn
          xdelt(i) = xnnew(i)
        end do
        xdelt(j) = xdelt(j) + h
 
        call p00_fx ( nprob, fxback, neqn, xdelt )
 
        nfxval = nfxval + 1
c
c  Evaluate the function which appears with positive sign
c
        if ( method .eq. -1 ) then
          h = 0.0
        else if ( method .eq. 0 .or. method .eq. 1 ) then
          h = difjac * ( 1.0D+00 + xnorm )
        end if
 
        do i = 1, neqn
          xdelt(i) = xnnew(i)
        end do
        xdelt(j) = xdelt(j) + h
 
        call p00_fx ( nprob, fxplus, neqn, xdelt )
 
        nfxval = nfxval + 1
c
c  Estimate jacobian columns
c
        if ( method .eq. -1 .or. method .eq. 1 ) then
          h = difjac * ( 1.0D+00 + xnorm )
        else if ( method .eq. 0 ) then
          h = 2.0 * difjac * ( 1.0D+00 + xnorm )
        end if
 
        do i = 1, neqn
          fprime(i,j) = ( fxplus(i) - fxback(i) ) / h
        end do
 
      end do
 
      return
      end
      function lenchr ( string )

c*********************************************************************72
c
cc LENCHR returns the length of STRING up to the last nonblank.
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) STRING, the string to be measured.
c
c    Output, integer LENCHR, the location of the last nonblank
c    character in STRING.
c
      implicit none

      integer i
      integer lenchr
      character*(*) string

      do i = len ( string ), 1, -1

        if ( string(i:i) .ne. ' ' ) then
          lenchr = i
          return
        end if

      end do

      lenchr = 0

      return
      end
      function leqi ( strng1, strng2 )

c*********************************************************************72
c
cc LEQI is a case insensitive comparison of two strings for equality.
c
c  Example:
c
c    LEQI('Anjana','ANJANA') is .TRUE.
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character*(*) STRNG1, STRNG2, the strings to compare.
c
c    Output, logical LEQI, the result of the comparison.
c
      implicit none

      integer i
      integer len1
      integer len2
      integer lenc
      logical leqi
      character*1 s1
      character*1 s2
      character*(*) strng1
      character*(*) strng2

      len1 = len ( strng1 )
      len2 = len ( strng2 )
      lenc = min ( len1, len2 )

      leqi = .false.

      do i = 1, lenc

        s1 = strng1(i:i)
        s2 = strng2(i:i)
        call capchr ( s1 )
        call capchr ( s2 )

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

      leqi = .true.

      return
      end
      subroutine lufact ( detjac, fpfact, fprime, ierror, ipivot, 
     &  neqn, nfacts )

c*********************************************************************72
c
cc LUFACT computes the LU factors of a matrix.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision DETJAC, the determinant of the matrix.
c
      implicit none

      integer neqn

      double precision detjac
      double precision fpfact(neqn,neqn)
      double precision fprime(neqn,neqn)
      integer i
      integer ierror
      integer info
      integer ipivot(neqn)
      integer j
      integer nfacts

      ierror = 0
      nfacts = nfacts + 1
c
c  Make a copy of the iteration matrix.
c
      do i = 1, neqn
        do j = 1, neqn
          fpfact(i,j) = fprime(i,j)
        end do
      end do
c
c  Factor the copied iteration matrix.
c
      call dge_fa ( fpfact, neqn, neqn, ipivot, info )
 
      if ( info .ne. 0 ) then
        ierror = 3
        write ( *, * ) ' '
        write ( *, * ) 'LUFACT - Fatal error!'
        write ( *, * ) 'The iteration matrix is singular.'
        detjac = 0.0D+00
        return
      end if
 
      detjac = 1.0D+00
      do i = 1, neqn
        if ( ipivot(i) .ne. i ) then
          detjac = - detjac
        end if
        detjac = detjac * fpfact(i,i)
      end do
 
      return
      end
      subroutine newcon ( abserr, damp, delx, detjac, difjac, enorm, 
     &  fnorm0, fnorm1, fpfact, fprime, fx, fxback, fxplus, idamp, 
     &  ihate, iknow, ilike, inorm, ipivot, iprint, maxhis, 
     &  maxstp, method, neqn, newmat, nfacts, nfpval, nfxval, nprob, 
     &  nsolve, nstep, pause, relerr, snorm0, snorm1, work, 
     &  xnew, xdelt, xnorm0, xnorm1, xold, xtrue )

c*********************************************************************72
c
cc NEWCON carries out the solution scheme for the nonlinear equation.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision ABSERR, the absolute error tolerance.
c
      implicit none

      integer maxhis
      integer neqn

      double precision abserr
      double precision damp
      double precision delx(neqn)
      double precision detjac
      double precision difjac
      double precision enorm(0:maxhis)
      double precision fnorm0
      double precision fnorm1
      double precision fpfact(neqn,neqn)
      double precision fprime(neqn,neqn)
      double precision fx(neqn)
      double precision fxback(neqn)
      double precision fxplus(neqn)
      integer i
      integer iaccpt
      integer idamp
      integer ierror
      integer ihate
      integer iknow
      integer ilike
      integer inorm
      integer ipivot(neqn)
      integer iprint
      integer iskip
      integer maxstp
      integer method
      integer mstep
      integer newmat
      integer nfacts
      integer nfpval
      integer nfxval
      integer nprob
      integer nsolve
      integer nstep
      logical pause
      double precision relerr
      double precision snorm0
      double precision snorm1
      double precision temp
      double precision work(neqn)
      double precision xdelt(neqn)
      double precision xnew(neqn)
      double precision xnorm0
      double precision xnorm1
      double precision xold(neqn)
      double precision xtrue(neqn)

      if ( nstep .eq. 0 ) then
        detjac = 0.0D+00
      end if
 
      ilike = 0
      iskip = 0
 
      if ( nstep .eq. 0 ) then
        iaccpt = 0
      end if
 
      ihate = 0
      mstep = 0
      damp = 1.0D+00
c
c  Should we accept the initial X?
c
      if ( nstep .eq. 0 ) then
 
        call p00_fx ( nprob, fx, neqn, xnew )
        nfxval = nfxval + 1
 
        if ( iknow .eq. 1 ) then
          call seterr ( inorm, neqn, work, xnew, xtrue, enorm(nstep) )
        end if
 
        call norm ( inorm, neqn, xnew, xnorm1 )
        call norm ( inorm, neqn, fx, fnorm1 )
        snorm1 = 0
        xnorm0 = xnorm1
        fnorm0 = fnorm1
        snorm0 = 0

        call accept ( abserr, fnorm1, ilike, nstep, relerr, snorm1, 
     &    xnorm1 )

        iaccpt = iaccpt + 1
 
        call output ( damp, delx, detjac, enorm, fnorm1, fx, idamp, 
     &    ihate, iknow, ilike, iprint, iskip, maxhis, neqn, nstep, 
     &    snorm1, xnew, xnorm1, xtrue )
 
        if ( ilike .eq. 1 ) then
          return
        end if
 
      end if
c
c  Here is the iteration loop.
c
   10 continue
 
      if ( mstep .ge. maxstp ) then
        write ( *, * ) ' '
        write ( *, * ) 'Maximum number of steps taken.'
        write ( *, * ) 'Use the I command to continue iteration.'
        return
      end if
 
      nstep = nstep + 1
      mstep = mstep + 1
c
c  Is it time to reevaluate the iteration matrix?
c  This logic is different for Broyden.
c
      if ( newmat .eq. 0 .or. newmat .eq. 1 ) then
 
        iskip = 0
 
      else
 
        if ( mod ( iaccpt, newmat ) - 1 .eq. 0 ) then
          iskip = 0
        else
          iskip = 1
        end if
 
      end if
c
c  Get the iteration matrix.
c
      if ( iskip .eq. 0 ) then
 
        call getjac ( difjac, fprime, fxback, fxplus, method, neqn, 
     &    nfpval, nfxval, nprob, nstep, xdelt, xnew )
c
c  Compute the LU factors of the iteration matrix.
c
        call lufact ( detjac, fpfact, fprime, ierror, ipivot,  
     &    neqn, nfacts )
 
        if ( ierror .ne. 0 ) then
          write ( *, * ) 'NEWCON - Warning!'
          write ( *, * ) 'Error occurred in linear system routine.'
          write ( *, * ) 'The iteration must stop.'
          ierror = 0
          return
        end if
 
      end if
c
c  Set the right hand side of the system to -f(x).
c  Solve the linear system J*delta x = - f(x).
c  Update x, and check for acceptance.
c
      do i = 1, neqn
        delx(i) = - fx(i)
      end do

      call dge_sl ( fpfact, neqn, neqn, ipivot, delx, 0 )
      nsolve = nsolve + 1
c
c  Damping loop begins here
c
      damp = 1.0D+00
      fnorm0 = fnorm1
      snorm0 = snorm1
      xnorm0 = xnorm1
      call norm ( inorm, neqn, delx, snorm1 )

      do i = 1, neqn
        xold(i) = xnew(i)
      end do
 
   60 continue
 
      do i = 1, neqn
        xnew(i) = xold(i) + damp * delx(i)
      end do

      call norm ( inorm, neqn, xnew, xnorm1 )
      call p00_fx ( nprob, fx, neqn, xnew )
      nfxval = nfxval + 1
      call norm ( inorm, neqn, fx, fnorm1 )
c
c  If we are using Broyden's method, update the iteration matrix
c
      if ( damp .eq. 1 .and. ( method .eq. 3 .or. method .eq. 4 ) ) then
        call upmat ( delx, fprime, fx, neqn )
      end if
c
c  Can we accept or reject the current step?
c
      call accept ( abserr, fnorm1, ilike, nstep, relerr, snorm1, 
     &  xnorm1 )
 
      call reject ( abserr, fnorm0, fnorm1, ihate, nstep, snorm0, 
     &  snorm1 )
c
c  If we dislike the current step, can we damp it and try again?
c
      if ( ihate .eq. 1 .and. mstep .lt. maxstp ) then
 
        if ( idamp .eq. 1 .and. damp .gt. 0.03125D+00 ) then
 
          ihate = 0
 
          if ( iknow .eq. 1 ) then
            call seterr ( inorm, neqn, work, xnew, xtrue, enorm(nstep) )
          end if
 
          call output ( damp, delx, detjac, enorm, fnorm1, fx, idamp, 
     &      ihate, iknow, ilike, iprint, iskip, maxhis, neqn, nstep, 
     &      snorm1, xnew, xnorm1, xtrue )

          temp = 0.5
          damp = temp * damp
          snorm1 = temp * snorm1

          do i = 1, neqn
            delx(i) = temp * delx(i)
          end do

          iskip = 1
          nstep = nstep + 1
          mstep = mstep + 1
          go to 60
        end if
      end if
c
c  Print the results of the current step
c
      if ( iknow .eq. 1 ) then
        call seterr ( inorm, neqn, work, xnew, xtrue, enorm(nstep) )
      end if
 
      call output ( damp, delx, detjac, enorm, fnorm1, fx, idamp, 
     &  ihate, iknow, ilike, iprint, iskip, maxhis, neqn, nstep, snorm1, 
     &  xnew, xnorm1, xtrue )
 
      iaccpt = iaccpt + 1
      damp = 1.0D+00
 
      if ( ilike .eq. 1 ) then
        return
      end if
 
      if ( ihate .eq. 1 ) then
        if ( pause ) then
          write ( *, * ) 'Use the I command to continue.'
          return
        end if
      end if
 
      go to 10
      end
      subroutine norm ( inorm, n, vec, vecnrm )

c*********************************************************************72
c
cc NORM computes the norm of a vector.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer n

      integer i
      integer inorm
      double precision vec(n)
      double precision vecnrm

      vecnrm = 0.0D+00
 
      if ( n .le. 0 ) then
        return
      end if
c
c  1: Maximum norm.
c
      if ( inorm .eq. 0 ) then
 
        do i = 1, n
          vecnrm = max ( vecnrm, abs ( vec(i) ) )
        end do
c
c  2: Sum of absolute values
c
      else if ( inorm .eq. 1 ) then
 
        vecnrm = 0.0D+00
        do i = 1, n
          vecnrm = vecnrm + abs ( vec(i) )
        end do
c
c  3: Square root of sum of squares
c
      else if ( inorm .eq. 2 ) then
 
        do i = 1, n
          vecnrm = vecnrm + vec(i)**2
        end do
 
        vecnrm = sqrt ( vecnrm )
 
      end if
 
      return
      end
      subroutine output ( damp, delx, detjac, enorm, fnorm1, fx, idamp, 
     &  ihate, iknow, ilike, iprint, iskip, maxhis, neqn, nstep, 
     &  snorm1, xnew, xnorm1, xtrue )

c*********************************************************************72
c
cc OUTPUT blah blah blah
c
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer maxhis
      integer neqn

      double precision damp
      double precision delx(neqn)
      double precision detjac
      double precision enorm(0:maxhis)
      double precision error
      double precision fnorm1
      double precision fx(neqn)
      integer i
      integer idamp
      integer ihate
      integer iknow
      integer ilike
      integer iprint
      integer iskip
      character*3 label
      integer lenchr
      integer lent
      integer nstep
      double precision snorm1
      double precision temp
      character*80 title
      double precision xnew(neqn)
      double precision xnorm1
      double precision xtrue(neqn)

      if ( iskip .ne. 0 ) then
        label = 'old'
      else
        label = 'new'
      end if
 
      if ( iprint .le. 0 ) then
        go to 30
      end if
c
c  On step zero, print the heading.
c
      if ( nstep .eq. 0 ) then
 
        if ( iprint .eq. 1 ) then
            title = 'step    x-norm       fx-norm    step-norm '//
     &        'Mat  Det'
        else
            title = 'step      x            fx          step   '//
     &        'Mat  Det'
        end if
 
        if ( idamp .eq. 1 ) then
          call chrcat ( title, '  damping', title )
        end if
 
        if ( iknow .eq. 1 ) then
 
          if ( iprint.eq.1 ) then
            call chrcat ( title, '  e-norm  log(e-norm)', title )
          else
            call chrcat ( title, '   error  log(e-norm)', title )
          end if
 
        end if
 
        write ( *, * ) ' '
        lent = lenchr ( title )
        write(*, '(a)' ) title(1:lent)
        write ( *, * ) ' '
 
      else if ( nstep .gt. 0 ) then
 
        if ( iprint .eq. 1 ) then
 
          if ( idamp .eq. 0 ) then
            write ( *, 1060 ) snorm1, label, detjac
          else if ( idamp .eq. 1 ) then
            write ( *, 1060 ) snorm1, label, detjac, damp
          endif
 
        else if ( iprint .ge. 2 ) then
 
          i = 1
 
          if ( idamp .eq. 0 ) then
            write(*, 1060 ) delx(i), label, detjac
          else if ( idamp .eq. 1 ) then
            write(*, 1060 ) delx(i), label, detjac, damp
          endif
 
          do i = 2, neqn
            write ( *, 1060 ) delx(i)
          end do
 
        end if
 
      end if
 
      if ( iprint .eq. 1 ) then
 
        if ( iknow .eq. 0 ) then
          write(*, 1041 ) nstep, xnorm1, fnorm1
        else
 
          if ( enorm(nstep) .gt. 0 ) then
            temp = log10 ( enorm(nstep) )
          else
            temp = 0.0D+00
          end if
 
          if ( idamp .eq. 0 ) then
            write (*, 1041 ) nstep, xnorm1, fnorm1, enorm(nstep), temp
          else if ( idamp .eq. 1 ) then
            write (*, 1042 ) nstep, xnorm1, fnorm1, enorm(nstep), temp
          end if
 
        end if
 
      else if ( iprint .ge. 2 ) then
 
        if ( iknow .eq. 0 ) then
 
          write (*, 1041 ) nstep, xnew(1), fx(1)
 
        else
 
          if ( enorm(nstep) .gt. 0 ) then
            temp = log10 ( enorm(nstep) )
          else
            temp = 0
          end if 
 
          error = xtrue(1) - xnew(1)
 
          if ( idamp .eq. 0 ) then
            write ( *, 1041 ) nstep, xnew(1), fx(1), error, temp
          else if ( idamp .eq. 1 ) then
            write ( *, 1042 ) nstep, xnew(1), fx(1), error, temp
          end if
 
        end if
 
        do i = 2, neqn
 
          error = xtrue(i) - xnew(i)
 
          if ( idamp .eq. 0 ) then
            write ( *, 1051 )     xnew(i), fx(i), error
          else if ( idamp .eq. 1 ) then
            write ( *, 1052 )     xnew(i), fx(i), error
          end if
 
        end do
 
      end if
 
   30 continue
 
      if ( ilike .eq. 1 ) then
        write ( *, * ) ' '
        write ( *, * ) 'Iteration halted, point accepted.'
      else if ( ihate .eq. 1 ) then
        write ( *, * ) ' '
        write ( *, * ) 'Bad stepc.  A norm increased.'
      end if
 
      return
 
 1041 format ( i4, g14.6, g11.3, 11x, 5x, 11x, g11.3, g11.3 )
 1042 format ( i4, g14.6, g11.3, 11x, 5x, 11x, 11x, g11.3, g11.3 )
 1051 format ( 4x, g14.6, g11.3, 11x, 5x, 11x, g11.3, g11.3 )
 1052 format ( 4x, g14.6, g11.3, 11x, 5x, 11x, 11x, g11.3, g11.3 )
 1060 format ( 4x, 14x, 11x, g11.3, 1x, a3, 1x, g11.3, g11.3 )
      end
      subroutine prijac ( detjac, fprime, neqn )

c*********************************************************************72
c
cc PRIJAC prints out the current iteration matrix.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer neqn

      double precision detjac
      double precision fprime(neqn,neqn)
      integer i
      integer j

      write ( *, * ) ' '
      write ( *, * ) 'Current iteration matrix:'
      write ( *, * ) ' '
      do i = 1, neqn
        write ( *, '(5g14.6)' ) ( fprime(i,j), j = 1, neqn )
      end do
 
      write ( *, * ) ' '
      write ( *, * ) 'Determinant = ', detjac
 
      return
      end
      subroutine prival ( abserr, difjac, epmach, fx, idamp, iknow, 
     &  inorm, iprint, maxeqn, maxstp, method, mprob, neqn, newmat, 
     &  nfacts, nfpval, nfxval, nprob, nsolve, nstep, relerr, title, 
     &  xnew, xstart, xtrue )

c*********************************************************************72
c
cc PRIVAL prints out the value of various parameters.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision ABSERR, the absolute error tolerance.
c
c    Input, integer MPROB, the number of problems available.
c
      implicit none

      integer maxeqn

      double precision abserr
      double precision difjac
      double precision epmach
      double precision fx(maxeqn)
      integer i
      integer idamp
      integer iknow
      integer inorm
      integer iprint
      integer maxstp
      integer method
      integer mprob
      integer neqn
      integer newmat
      integer nfacts
      integer nfpval
      integer nfxval
      integer nprob
      integer nsolve
      integer nstep
      double precision relerr
      character*80 title
      double precision xnew(maxeqn)
      double precision xstart(maxeqn)
      double precision xtrue(maxeqn)

      write ( *, * ) ' '
      write ( *, * ) 'The number of problems available is ', mprob
      write ( *, * ) 'The current problem is number ', nprob
      write ( *, * ) 'The problem title is:'
      write ( *, '(1x, a)' ) title(1:60)
      write ( *, * ) ' '
      write ( *, * ) 'Number of equations = ', neqn
      write ( *, * ) ' '
      write ( *, * ) 'Absolute error tolerance=', abserr
      write ( *, * ) 'Relative error tolerance=', relerr
      write ( *, * ) 'Machine precision=', epmach
      write ( *, * ) 'Difference parameter for jacobians=', difjac
 
      write ( *, * ) ' '
      write ( *, * ) 'Solution method number:', method
      if ( method .eq. -1 ) then
        write ( *, * ) 'Newton method, backward f-d jacobian.'
      else if ( method .eq. 0 ) then
        write ( *, * ) 'Newton method, central f-d jacobian.'
      else if ( method .eq. 1 ) then
        write ( *, * ) 'Newton method, forward f-d jacobian.'
      else if ( method .eq. 2 ) then
        write ( *, * ) 'Newton method, analytic jacobian.'
      else if ( method .eq. 3 ) then
        write ( *, * ) 'Broyden method, B(0)=Identity.'
      else if ( method .eq. 4 ) then
        write ( *, * ) 'Broyden method, B(0)=Jacobian.'
      else
        write ( *, * ) 'PRIVAL - Fatal error!'
        write ( *, * ) 'Unknown method number.'
        stop
      end if
 
      write ( *, * ) ' '
      write ( *, * ) 'Output level for iterations=', iprint
      write ( *, * ) 
     &  'Maximum number of iterations before pause=', maxstp
      write ( *, * ) 'Number of steps before the iteration matrix'
      write ( *, * ) '  is updated=', newmat
 
      if ( idamp .eq. 0 ) then
        write ( *, * ) 'No damping is used.'
      else if ( idamp .eq. 1 ) then
        write ( *, * ) 'Damping via repeated step halving is used.'
      endif
 
      if ( nstep .eq. 0 ) then
        write ( *, * ) 'No steps have been taken yet.'
      end if
 
      if ( inorm .eq. 0 ) then
        write ( *, * ) 'The vector norm used is the L-infinity norm.'
      else if ( inorm .eq. 1 ) then
        write ( *, * ) 'The vector norm used is the L1 norm.'
      else if ( inorm .eq. 2 ) then
        write ( *, * ) 'The vector norm used is the L2 norm.'
      end if
 
      write ( *, * ) ' '
      write ( *, * ) 'Work so far for this problem:'
      write ( *, * ) ' '
      write ( *, * ) 'Function evaluations: ', nfxval
      write ( *, * ) 'Jacobian evaluations: ', nfpval
      write ( *, * ) 'Matrix factorizations:', nfacts
      write ( *, * ) 'Linear system solves: ', nsolve
 
      write ( *, * ) ' '
      write ( *, * ) 'Starting point:'
      write ( *, * ) ' '
      write ( *, * ) '       x             f(x)'
      write ( *, * ) ' '
      call p00_fx ( nprob, fx, neqn, xstart )
      nfxval = nfxval + 1
 
      do i = 1, neqn
        write ( *, '(2g14.6)' ) xstart(i), fx(i)
      end do
 
      if ( nstep .gt. 0 ) then
 
        write ( *, * ) ' '
        write ( *, * ) 'Estimate of the root after ', nstep, ' steps.'
        write ( *, * ) ' '
        write ( *, * ) '       x             f(x)'
        write ( *, * ) ' '
        call p00_fx ( nprob, fx, neqn, xnew )
        nfxval = nfxval + 1
        do i = 1, neqn
          write ( *, '(2g14.6)' ) xnew(i), fx(i)
        end do
 
      end if
 
      if ( iknow .eq. -1 ) then
 
        write ( *, * ) ' '
        write ( *, * ) 'There is no solution to this system.'

      else if ( iknow .eq. 0 ) then
 
        write ( *, * ) ' '
        write ( *, * ) 'The exact solution is not known.'
 
      else
 
        write ( *, * ) ' '
        write ( *, * ) 'Exact solution:'
        write ( *, * ) ' '
        write ( *, * ) '       x             f(x)'
        write ( *, * ) ' '
        call p00_fx ( nprob, fx, neqn, xtrue )
        nfxval = nfxval + 1
 
        do i = 1, neqn
          write ( *, '(2g14.6)' ) xtrue(i), fx(i)
        end do
 
      end if
 
      return
      end
      subroutine problem_choice ( mprob, nprob, title )

c*********************************************************************72
c
cc PROBLEM_CHOICE allows the user to choose a problem by number.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer MPROB, the number of problems available.
c
c    Output, integer NPROB, the number of the problem chosen.
c
c    Output, character*(*) TITLE, the title of the problem chosen.
c
      implicit none

      integer iprob
      integer lenc
      integer lenchr
      integer mprob
      integer nprob
      character*(*) title

10    continue

      write ( *, * ) ' '
      write ( *, * ) 'PROBLEM_CHOICE:'
      write ( *, * ) '  There are ', mprob, ' problems available.'
      write ( *, * ) ' '
c
c  Print the problem titles.
c
      do iprob = 1, mprob
        call p00_title ( iprob, title )
        lenc = lenchr ( title )
        write ( *, '(i2, '': '', a)' ) iprob, title(1:lenc)
      end do
c
c  The user chooses a problem by number.
c
      write ( *, * ) ' '
      write ( *, * ) 'Which one would you like to work on?'
      read ( *, *, end = 20, err = 20 ) nprob

      if ( 1 .le. nprob .and. nprob .le. mprob ) then
        call p00_title ( nprob, title )
        return
      else
        write ( *, * ) ' '
        write ( *, * ) 'PROBLEM_CHOICE - Warning!'
        write ( *, * ) '  That was not a valid problem number.'
        write ( *, * ) '  Please choose again.'
        go to 10
      end if
 
20    continue
 
      write ( *, * ) ' '
      write ( *, * ) 'PROBLEM_CHOICE - Fatal error!'
      write ( *, * ) '  Illegal input.'
      stop

      end
      subroutine reject ( abserr, fnorm0, fnorm1, ihate, nstep, snorm0, 
     &  snorm1 )

c*********************************************************************72
c
cc REJECT decides whether the iteration should be halted.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision ABSERR, the absolute error tolerance.
c
      implicit none

      double precision abserr
      double precision fnorm0
      double precision fnorm1
      integer ihate
      integer nstep
      double precision snorm0
      double precision snorm1

      ihate = 0
 
      if ( nstep .eq. 1 ) then
 
        if ( fnorm1 .gt. fnorm0 + abserr ) then
          ihate = 1
        end if
 
      else
 
        if ( fnorm1 .gt. fnorm0 + abserr ) then
          ihate = 1
        end if
 
        if ( snorm1 .gt. snorm0 + abserr ) then
          ihate = 1
        end if
 
      end if
 
      return
      end
      subroutine seterr ( inorm, neqn, work, xnew, xtrue, error )

c*********************************************************************72
c
cc SETERR computes the norm of ( X - XTRUE ).
c
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer neqn

      double precision error
      integer i
      integer inorm
      double precision work(neqn)
      double precision xnew(neqn)
      double precision xtrue(neqn)

      do i = 1, neqn
        work(i) = xtrue(i) - xnew(i)
      end do
 
      call norm ( inorm, neqn, work, error )
 
      return
      end
      subroutine setmth ( method )

c*********************************************************************72
c
cc SETMTH allows the user to choose the iteration method.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      character*1 isay
      logical leqi
      integer method

      write ( *, * ) ' '
      write ( *, * ) 'Three methods are available:'
      write ( *, * ) ' '
      write ( *, * ) 'B  Broyden''s method.'
      write ( *, * ) 'F  Finite difference Newton method.'
      write ( *, * ) 'N  Newton method.'
      write ( *, * ) ' '
      write ( *, * ) 'Choose a method by typing B, F, or N.'
      read ( *, '(a)' ) isay
 
      if ( leqi ( isay, 'F' ) ) then
 
        write ( *, * ) ' '
        write ( *, * ) 'Please choose the type of differencing:'
        write ( *, * ) ' '
        write ( *, * ) 'B  Backward differencing, fixed h.'
        write ( *, * ) 'C  Central differencing, fixed h.'
        write ( *, * ) 'F  Forward differencing, fixed h.'
        read ( *, '(a)' ) isay
 
        if ( leqi ( isay, 'B' ) ) then
          method = -1
        else if ( leqi ( isay, 'C' ) ) then
          method = 0
        else if ( leqi ( isay, 'F' ) ) then
          method = 1
        else
          write ( *, * ) 'Your choice was not recognized.'
        endif
 
      else if ( leqi ( isay, 'N' ) ) then
 
        method = 2
 
      else if ( leqi ( isay, 'B' ) ) then
 
        write ( *, * ) ' '
        write ( *, * ) 'Please choose how Broyden''s method will start:'
        write ( *, * ) ' '
        write ( *, * ) 'I  Initial iteration matrix is identity.'
        write ( *, * ) 'J  Initial iteration matrix is jacobian.'
        read ( *, '(a)' ) isay
 
        if ( leqi ( isay, 'I' ) ) then
          method = 3
        else if ( leqi ( isay, 'J' ) ) then
          method = 4
        else
          write ( *, * ) 'Your choice was not recognized.'
        end if
 
      else
 
        write ( *, * ) 'Your choice was not recognized.'
 
      end if
 
      return
      end
      subroutine setter ( abserr, difjac, idamp, inorm, iprint, maxstp, 
     &  newmat, pause, relerr )

c*********************************************************************72
c
cc SETTER allows the user to set various parameters.
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, double precision ABSERR, the absolute error tolerance.
c
c    Input/output, double precision DIFJAC, difference parameter for jacobians.
c
      implicit none

      double precision abserr
      double precision difjac
      integer idamp
      integer inorm
      integer iprint
      character*1 isay
      logical leqi
      integer maxstp
      character*10 name
      integer newmat
      logical pause
      double precision relerr

10    continue
 
      write ( *, * ) 
     &  'Enter name of variable to set (H = help, Q = quit)'
      read ( *, '(a)' ) name
 
      if ( leqi ( name, 'abserr' ) ) then
 
        write ( *, * ) 'Enter absolute error tolerance.'
        read ( *, * ) abserr
        write ( *, * ) 'Absolute error tolerance set to ', abserr
 
      else if ( leqi ( name, 'h' ) ) then
 
        write ( *, * ) ' '
        write ( *, * ) 'Here are the variables you can set:'
        write ( *, * ) ' '
        write ( *, * ) 'abserr  Absolute error tolerance.'
        write ( *, * ) 'difjac  Difference parameter for jacobians.'
        write ( *, * ) 'idamp   The damping option.'
        write ( *, * ) 'inorm   The vector norm used.'
        write ( *, * ) 'iprint  Amount of output.'
        write ( *, * ) 'maxstp  The maximum number of iterations.'
        write ( *, * ) 'newmat  Frequency of iteration matrix updates.'
        write ( *, * ) 'pause   Determines whether iterations pause.'
        write ( *, * ) 'relerr  Relative error tolerance.'
        write ( *, * ) ' '
        write ( *, * ) 'q       Means you are done setting values.'
        write ( *, * ) ' '
 
      else if ( leqi ( name, 'idamp' ) ) then
 
        write ( *, * ) 'Set the damping option.'
        write ( *, * ) '0 = no damping, '
        write ( *, * ) '1 = repeated halving of Newton step'
        read ( *, * ) idamp
        write ( *, * ) 'Damping option chosen was ', idamp
 
      else if ( leqi ( name, 'difjac' ) ) then
 
        write ( *, * ) 'Enter a small, positive number for use for'
        write ( *, * ) 'approximating jacobians by finite differences.'
        read ( *, * ) difjac
        write ( *, * ) 'Difference parameter for jacobians=', difjac
 
      else if ( leqi ( name, 'inorm' ) ) then
 
        write ( *, * ) 'Choose the vector norm to use:'
        write ( *, * ) ' '
        write ( *, * ) '0  Maximum entry norm (L-infinity)'
        write ( *, * ) '1  Sum of absolute values (L-1)'
        write ( *, * ) '2  Square root of sum of squares (L-2)'
        read ( *, * ) inorm
        if ( inorm .lt. 0 .or. inorm .gt. 2 ) inorm = 0
        write ( *, * ) 'Vector norm option chose was ', inorm
 
      else if ( leqi ( name, 'iprint' ) ) then
 
        write ( *, * ) ' '
        write ( *, * ) '0 = print out only result of iteration.'
        write ( *, * ) '1 = print norms of x, f(x) and delta x.'
        write ( *, * ) '2 = print x, f(x) and delta x'
        read ( *, * ) iprint
        write ( *, * ) 'Printout level set to ', iprint
 
      else if ( leqi ( name, 'maxstp' ) ) then
 
        write ( *, * ) 'Enter limit for number of iterations.'
        read ( *, * ) maxstp
        write ( *, * ) 'Number of iterations set to ', maxstp
 
      else if ( leqi ( name, 'newmat' ) ) then
 
        write ( *, * ) 
     &    'How many steps before updating iteration matrix?'
        read ( *, * ) newmat
        write ( *, * ) 'Number of steps before update set to ', newmat
 
      else if ( leqi ( name, 'pause' ) ) then
 
        write ( *, * ) 'Type T if rejected steps should pause.'
        write ( *, * ) 'Type F otherwise.'
        read ( *, '(a1)' ) isay
 
        if ( leqi ( isay, 'T' ) ) then
          pause = .true.
        else
          pause = .false.
        end if
 
        if ( pause ) then
          write ( *, * ) 'Pausing requested.'
        else
          write ( *, * ) 'Pausing turned off.'
        end if
 
      else if ( leqi ( name, 'q' ) .or. name .eq. ' ' ) then
 
        return
 
      else if ( leqi ( name, 'relerr' ) ) then
 
        write ( *, * ) 'Enter relative error tolerance.'
        read ( *, * ) relerr
        write ( *, * ) 'New relativer error tolerance=', relerr
 
      else
 
        write ( *, * ) 'That was NOT the name of a variable to change!'
        write ( *, * ) 'Perhaps you mistyped the name?'
 
      end if
 
      go to 10
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
      subroutine upmat ( delx, fprime, fx, neqn )

c*********************************************************************72
c
cc UPMAT carries out the Broyden update of the iteration matrix.
c
c  Discussion:
c
c    B(k+1) = B(k) + f(x(k+1)) * transpose(delx(k+1))
c      / transpose(delx(k+1)) * delx(k+1)
c
c  Modified:
c
c    12 December 1998
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision DELX(NEQN), ???
c
c    Input/output, double precision FPRIME(NROW,NEQN), the iteration matrix.
c    On output, the Broyden update has been applied.
c
c    Input, double precision X(NEQN), the function value at the current iterate X>
c
c    Input, integer NEQN, the number of equations.
c
c    Input, integer NROW, the leading dimension of FPRIME, which
c    must be at least NEQN.
c
      implicit none

      integer neqn

      double precision deldotdel
      double precision delx(neqn)
      double precision fprime(neqn,neqn)
      double precision fx(neqn)
      integer i
      integer j

      deldotdel = 0.0D+00
      do i = 1, neqn
        deldotdel = deldotdel + delx(i)**2
      end do
 
      do i = 1, neqn
        do j = 1, neqn
          fprime(i,j) = fprime(i,j) + fx(i) * delx(j) / deldotdel
        end do
      end do
 
      return
      end
