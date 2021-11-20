 
c umfpack demo program.
c
c factor and solve a 5-by-5 system, ax=b, using default parameters,
c except with complete printing of all arguments on input and output,
c where
c     [ 2  3  0  0  0 ]      [  8 ]                  [ 1 ]
c     [ 3  0  4  0  6 ]      [ 45 ]                  [ 2 ]
c a = [ 0 -1 -3  2  0 ], b = [ -3 ]. solution is x = [ 3 ].
c     [ 0  0  1  0  0 ]      [  3 ]                  [ 4 ]
c     [ 0  4  2  0  1 ]      [ 19 ]                  [ 5 ]
c solve a'x=b, with solution:
c       x = [  1.8158  1.4561 1.5000 -24.8509 10.2632 ]'
c using the factors of a.   modify one entry (a (5,2) = 1.0e-6) and
c refactorize.  solve ax=b both without and with iterative refinement,
c with true solution (rounded to 4 digits past the decimal point):
c       x = [-15.0000 12.6667 3.0000   9.3333 13.0000 ]'
 
        program main

cc MAIN is the main program for UMFPACK_PRB.
c
        integer nmax, nemax, lvalue, lindex
        parameter (nmax=20, nemax=100, lvalue=300, lindex=300)
        integer keep (20), index (lindex), info (40),
     $     i, icntl (20), n, ne, ai (2*nemax)
        real b (nmax), x (nmax), w (4*nmax), value (lvalue),
     $     cntl (10), rinfo (20), ax (nemax)
c
c read input matrix and right-hand side.  
c
c  keep a copy of the triplet form in ai and ax.
c
        read (5, *) n, ne
        read (5, *) (ai (i), ai (ne+i), i = 1,ne)
        read (5, *) (ax (i), i = 1,ne)
        read (5, *) (b (i), i = 1,n)

        do i = 1, ne
           index (i) = ai (i)
           index (ne+i) = ai (ne+i)
           value (i) = ax (i)
        end do
 
c initialize controls, and change default printing control.  note that
c this change from the default should only be used for test cases.  it
c can generate a lot of output for large matrices.
 
        call ums2in (icntl, cntl, keep)
        icntl (3) = 4
 
c factorize a, and print the factors.  input matrix is not preserved.
 
        call ums2fa (n, ne, 0, .false., lvalue, lindex, value, index,
     $               keep, cntl, icntl, info, rinfo)
        if (info (1) .lt. 0) stop
 
c reset default printing control (ums2in could be called instead)
        icntl (3) = 2
c
c solve ax = b and print solution.
c
        call ums2so (n, 0, .false., lvalue, lindex, value, index,
     $               keep, b, x, w, cntl, icntl, info, rinfo)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Solution of A * x = b:'
      write ( *, '(a)' ) ' '
      write (6, 30) (x (i), i = 1, n)
        if (info (1) .lt. 0) stop
 
c solve a'x = b and print solution.
 
        call ums2so (n, 0, .true., lvalue, lindex,  value, index,
     $               keep, b, x, w, cntl, icntl, info, rinfo)


      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Solution of A'' * x = b:'
      write ( *, '(a)' ) ' '
        write (6, 30) (x (i), i = 1, n)
        if (info (1) .lt. 0) stop
 
c modify one entry of a, and refactorize using ums2rf.
 
        do i = 1, ne
           index (i) = ai (i)
           index (ne+i) = ai (ne+i)
           value (i) = ax (i)
        end do
c
c       a (5,2) happens to be (paq)_22, the second pivot entry:
c
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Modify one entry of A, call it A#:'
        write ( *, '(a)' ) ' '

        value (10) = 1.0e-6
 
        call ums2rf (n, ne, 1, .false., lvalue, lindex, value, index,
     $               keep, cntl, icntl, info, rinfo)
        if (info (1) .lt. 0) stop
c
c solve ax = b without iterative refinement, and print solution.
c this will be very inaccurate due to the tiny second pivot entry.
 
        call ums2so (n, 0, .false., lvalue, lindex,  value, index,
     $               keep, b, x, w, cntl, icntl, info, rinfo)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Solution of A# * x = b:'
      write ( *, '(a)' ) ' '
        write (6, 30) (x (i), i = 1, n)
        if (info (1) .lt. 0) stop
c
c solve ax = b with iterative refinement, and print solution.
c this is much more accurate.
 
        icntl (8) = 10
        call ums2so (n, 0, .false., lvalue, lindex,  value, index,
     $               keep, b, x, w, cntl, icntl, info, rinfo)
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Solution of A# * x = b'
      write ( *, '(a)' ) '  via iterative refinement.'
      write ( *, '(a)' ) ' '
        write (6, 30) (x (i), i = 1, n)
        if (info (1) .lt. 0) stop
        stop
30      format ('solution: ', 5(/,f20.16))
        end
