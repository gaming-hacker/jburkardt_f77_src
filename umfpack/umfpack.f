         subroutine ums2co (n, nz, transa, xx, xsize, info, icntl,
     $          ii, isize, w, wp, who)
        integer isize, ii (isize), n, nz, w (n), wp (n+1), info (40),
     $          icntl (20), xsize, who
        real
     $          xx (xsize)
        logical transa
 
c=== ums2co ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  convert input matrix (ii,xx,n,nz) into from triplet form to column-
c  oriented form.  remove invalid entries and duplicate entries.
 
c=======================================================================
c  input:
c=======================================================================
c
c       n:              size of the matrix
c       nz:             number of nonzeros in the input matrix
c       transa:         if true then transpose input matrix
c       xx (1..nz):     values of triplet form
c       xsize:          size of xx, must be >= 2*nz
c       ii (1..2*nz):   row and col indices of triplet form
c       isize:          size of ii, must be >= max (2*nz,n+1) + nz
c       icntl:          integer control parameters
c       who:            who called ums2co, 1: ums2fa, 2: ums2rf
c
c       ii must be at least of size (nz + max (2*nz, n+1))
c       xx must be at least of size (nz + max (  nz, n+1))
c
c       input triplet matrix:
c          if (transa) is false:
c               ii (p)          row index, for p = 1..nz
c               ii (nz+p)       col index
c               xx (p)          value
c          if (transa) is true:
c               ii (p)          col index, for p = 1..nz
c               ii (nz+p)       row index
c               xx (p)          value
 
c=======================================================================
c  workspace:
c=======================================================================
c
c       w (1..n)
 
c=======================================================================
c  output:
c=======================================================================
c
c       nz:             number of nonzeros in the output matrix,
c                       after removing invalid entries, and summing up
c                       duplicate entries
c       ii (n+2..nz+n+1): row indices in column-form
c       xx (1..nz):     values in column-form.
c       info (1):       error flag
c       info (3):       invalid entries
c       info (2):       duplicate entries
c       info (5):       remaining valid entries
c       info (6):       remaining valid entries
c       info (7):       0
c       wp (1..n+1)     column pointers for column form
c       ii (1..n+1)     column pointers for column form
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutines:  ums2fa, ums2rf
c       subroutines called:     ums2er, ums2p2
c       functions called:       max
        intrinsic max
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer row, col, pdest, p, nz1, pcol, ip, xp, io, prl, ninvld,
     $          ndupl, i
        logical pr3
        real
     $          zero
        parameter (zero = 0.0)
 
c  row:     row index
c  col:     column index
c  pdest:   location of an entry in the column-form, for dupl. removal
c  p:       pointer
c  nz1:     number of entries after removing invalid or duplic. entries
c  pcol:    column col starts here after duplicates removed
c  ip:      column-form copy of matrix placed in ii (ip...ip+nz-1)
c  xp:      column-form copy of matrix placed in xx (xp...xp+nz-1)
c  ninvld:  number of invalid entries
c  ndupl:   number of duplicate entries
c  i:       a row index if transa is true, a column index otherwise
c  io:      i/o unit for warning messages (for invalid or dupl. entries)
c  prl:     printing level
c  pr3:     true if printing invalid and duplicate entries
 
c=======================================================================
c  executable statements:
c=======================================================================
 
c-----------------------------------------------------------------------
c  get arguments and check memory sizes
c-----------------------------------------------------------------------
 
        io = icntl (2)
        prl = icntl (3)
        pr3 = prl .ge. 3 .and. io .ge. 0
 
c-----------------------------------------------------------------------
c  count nonzeros in columns and check for invalid entries
c-----------------------------------------------------------------------
 
        ninvld = 0
        ndupl = 0
        do 10 col = 1, n
           w (col) = 0
10      continue
        nz1 = nz
        do 20 p = nz, 1, -1
           row = ii (p)
           col = ii (nz+p)
           if (row.lt.1.or.row.gt.n.or.col.lt.1.or.col.gt.n) then
c             this entry is invalid - delete it
              if (pr3) then
c                print the offending entry on the diagnostic i/o unit
                 call ums2p2 (who, 99, row, col, xx(p), io)
              endif
              ii (p)    = ii (nz1)
              ii (nz+p) = ii (nz+nz1)
              xx (p)    = xx (nz1)
              nz1 = nz1 - 1
           else
              if (transa) then
c                factorizing a transpose
                 w (row) = w (row) + 1
              else
c                factorizing a
                 w (col) = w (col) + 1
              endif
           endif
20      continue
        ninvld = nz - nz1
        if (ninvld .ne. 0) then
c          invalid entries found - set warning flag and continue
           call ums2er (who, icntl, info, 1, ninvld)
        endif
 
c-----------------------------------------------------------------------
c  convert triplet form to column-form
c-----------------------------------------------------------------------
 
        wp (1) = 1
        do 30 i = 1, n
           wp (i+1) = wp (i) + w (i)
30      continue
        do 40 i = 1, n
           w (i) = wp (i)
40      continue
 
c       ----------------------------------------------------------------
c       construct column-form in ii (2*nz+1..3*nz) and xx (nz+1..2*nz)
c       ----------------------------------------------------------------
 
        ip = max (2*nz, n+1)
        xp = nz
        if (transa) then
           do 50 p = 1, nz1
              row = ii (p)
              col = ii (nz+p)
              ii (ip + w (row)) = col
              xx (xp + w (row)) = xx (p)
              w (row) = w (row) + 1
50         continue
        else
           do 60 p = 1, nz1
              row = ii (p)
              col = ii (nz+p)
              ii (ip + w (col)) = row
              xx (xp + w (col)) = xx (p)
              w (col) = w (col) + 1
60         continue
        endif
 
c       ----------------------------------------------------------------
c       shift the matrix back to ii (n+2..nz+n+1) and xx (n+2..nz+n+1)
c       ----------------------------------------------------------------
 
        nz = nz1
cfpp$ nodepchk l
        do 70 p = 1, nz
           ii (n+1+p) = ii (ip+p)
           xx (p) = xx (xp+p)
70      continue
 
c-----------------------------------------------------------------------
c  remove duplicate entries by adding them up
c-----------------------------------------------------------------------
 
        do 80 row = 1, n
           w (row) = 0
80      continue
        pdest = 1
        do 100 col = 1, n
           pcol = pdest
           do 90 p = wp (col), wp (col+1)-1
              row = ii (n+1+p)
              if (w (row) .ge. pcol) then
c                this is a duplicate entry
                 xx (w (row)) = xx (w (row)) + xx (p)
                 if (pr3) then
c                   print the duplicate entry on the diagnostic i/o
c                   unit.  the row and column indices printed reflect
c                   the input matrix.
                    if (transa) then
                       call ums2p2 (who, 98, col, row, xx (p), io)
                    else
                       call ums2p2 (who, 98, row, col, xx (p), io)
                    endif
                 endif
              else
c                this is a new entry, store and record where it is
                 w (row) = pdest
                 if (pdest .ne. p) then
                    ii (n+1+pdest) = row
                    xx (pdest) = xx (p)
                 endif
                 pdest = pdest + 1
              endif
90         continue
           wp (col) = pcol
100     continue
        wp (n+1) = pdest
        nz1 = pdest - 1
        ndupl = nz - nz1
        if (ndupl .ne. 0) then
c          duplicate entries found - set warning flag and continue
           call ums2er (who, icntl, info, 2, ndupl)
        endif
        nz = nz1
 
c-----------------------------------------------------------------------
c  save column pointers in ii (1..n+1)
c-----------------------------------------------------------------------
 
        do 110 col = 1, n+1
           ii (col) = wp (col)
110     continue
 
        info (2) = ndupl
        info (3) = ninvld
        info (5) = nz
        info (6) = nz
        info (7) = 0
        if (nz .eq. 0) then
c          set error flag if all entries are invalid
           call ums2er (who, icntl, info, -2, -1)
        endif
        return
        end
 
        subroutine ums2er (who, icntl, info, error, s)
        integer who, icntl (20), info (40), error, s
 
c=== ums2er ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  print error and warning messages, and set error flags.
 
c=======================================================================
c  input:
c=======================================================================
c
c       who             which user-callable routine called:
c                       1: ums2fa, 2: ums2rf, 3: ums2so
c       icntl (1):      i/o unit for error and warning messages
c       icntl (3):      printing level
c       info (1):       the error/warning status
c       error:          the applicable error (<0) or warning (>0).
c                       see ums2p2 for a description.
c       s:              the relevant offending value
 
c=======================================================================
c  output:
c=======================================================================
c
c       info (1):       the error/warning status
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutines:  ums2co, ums2fa, ums2f0, ums2f1, ums2f2,
c                               ums2rf, ums2r0, ums2r2, ums2so, ums2s2
c       subroutines called:     ums2p2
c       functions called:       mod
        intrinsic mod
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        logical both
        real
     $          zero
        parameter (zero = 0.0)
        integer ioerr, prl
 
c  ioerr:   i/o unit for error and warning messages
c  prl:     printing level
c  both:    if true, then combine errors -3 and -4 into error -5
 
c=======================================================================
c  executable statements:
c=======================================================================
 
        ioerr = icntl (1)
        prl = icntl (3)
        if (error .lt. 0) then
c          this is an error message
           both = (info (1) .eq. -3 .and. error .eq. -4) .or.
     $            (info (1) .eq. -4 .and. error .eq. -3)
           if (both) then
c             combine error -3 (out of integer memory) and error -4
c             (out of real memory)
              info (1) = -5
           else
              info (1) = error
           endif
           if (prl .ge. 1) then
              call ums2p2 (who, error, s, 0, zero, ioerr)
           endif
        else if (error .gt. 0) then
c          this is a warning message
           if (info (1) .ge. 0) then
c             do not override a prior error setting, sum up warnings
              if (mod (info (1) / error, 2) .eq. 0) then
                 info (1) = info (1) + error
              endif
           endif
           if (prl .ge. 2) then
              call ums2p2 (who, error, s, 0, zero, ioerr)
           endif
        endif
        return
        end
 
        subroutine ums2f0 (n, nz, cp, xx, xsize, ii, isize, xtail,
     $          itail, iuse, xuse, nzoff, nblks, icntl, cntl, info,
     $          rinfo, presrv, ap, ai, ax, an, anz, keep, ne)
        integer n, nz, isize, ii (isize), icntl (20), info (40),
     $          cp (n+1), xsize, xtail, itail, iuse, xuse, an, anz,
     $          ap (an+1), ai (anz), keep (20), nzoff, nblks, ne
        logical presrv
        real
     $          xx (xsize), cntl (10), rinfo (20), ax (anz)
 
c=== ums2f0 ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  factorize an unsymmetric sparse matrix in column-form, optionally
c  permuting the matrix to upper block triangular form and factorizing
c  each diagonal block.
 
c=======================================================================
c  input:
c=======================================================================
c
c       n:              order of matrix
c       nz:             entries in matrix, after removing duplicates
c                       and invalid entries.
c       ne:             number of triplets, unchanged from ums2fa
c       cp (1..n+1):    column pointers of input matrix
c       presrv:         if true then preserve original matrix
c       xsize:          size of xx
c       isize:          size of ii
c       iuse:           memory usage in index on input
c       xuse:           memory usage in value on input
c       icntl:          integer control parameters, see ums2in
c       cntl:           real control parameters, see ums2in
c       keep (6..8):    integer control parameters, see ums2in
c
c       if presrv is true:
c           an:                 = n, order of preserved matrix
c           anz:                = anz, order of preserved matrix
c           ap (1..an+1):       column pointers of preserved matrix
c           ai (1..nz):         row indices of preserved matrix
c           ax (1..nz):         values of preserved matrix
c           ii:                 unused on input
c           xx:                 unused on input
c       else
c           an:                 1
c           anz:                1
c           ap:                 unused
c           ai:                 unused
c           ax:                 unused
c           ii (1..nz):         row indices of input matrix
c           xx (1..nz):         values of input matrix
 
c=======================================================================
c  output:
c=======================================================================
c
c       xx (xtail ... xsize), xtail:
c
c                       lu factors are located in xx (xtail ... xsize),
c                       including values in off-diagonal part if matrix
c                       was permuted to block triangular form.
c
c       ii (itail ... isize), itail:
c
c                       lu factors are located in ii (itail ... isize),
c                       including pattern, row and column permutations,
c                       block triangular information, etc.  see umf2fa
c                       for more information.
c
c       info:           integer informational output, see ums2fa
c       rinfo:          real informational output, see ums2fa
c
c       iuse:           memory usage in index on output
c       xuse:           memory usage in value on output
c
c       nzoff:          entries in off-diagonal part (0 if btf not used)
c       nblks:          number of diagonal blocks (1 if btf not used)
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2fa
c       subroutines called:     ums2er, ums2fb, ums2f1, ums2of
c       functions called:       max
        intrinsic max
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer kn, nzdia, blkpp, lublpp, p, offip, xhead, row,
     $          offxp, offpp, ihead, k1, k2, blk, prp, p2, cpermp,
     $          rpermp, nsgltn, npiv, mnz, nsym, k, col, rmax, cmax,
     $          totnlu, xrmax, xruse
        logical trybtf, iout, xout
        real
     $          zero, one, a
        parameter (zero = 0.0, one = 1.0)
 
c  allocated array pointers:
c  -------------------------
c  blkpp:   blkp (1..nblks+1) array located in ii (blkpp..blkp+nblks)
c  lublpp:  lublkp (1..nblks) array loc. in ii (lublpp..lublpp+nblks-1)
c  offip:   offi (1..nzoff) array located in ii (offip..offip+nzoff-1)
c  offxp:   offx (1..nzoff) array located in xx (offxp..offxp+nzoff-1)
c  offpp:   offp (1..n+1) array located in ii (offpp..offpp+n)
c  cpermp:  cperm (1..n) array located in ii (cpermp..cpermp+n-1)
c  rpermp:  rperm (1..n) array located in ii (rpermp..rpermp+n-1)
c  prp:     pr (1..n) work array located in ii (prp..prp+n-1)
c
c  btf information:
c  ----------------
c  k1:      starting index of diagonal block being factorized
c  k2:      ending index of diagonal block being factorized
c  kn:      the order of the diagonal block being factorized
c  blk:     block number of diagonal block being factorized
c  trybtf:  true if btf is to be attempted (= icntl (4) .eq. 1)
c  nzdia:   number of entries in diagonal blocks (= nz if btf not used)
c  nsgltn:  number of 1-by-1 diagonal blocks ("singletons")
c  npiv:    number of numerically valid singletons
c  a:       numerical value of a singleton
c  mnz:     nzoff
c
c  memory usage:
c  -------------
c  xhead:   xx (1..xhead-1) is in use, xx (xhead..xtail-1) is free
c  ihead:   ii (1..ihead-1) is in use, ii (ihead..itail-1) is free
c  iout:    true if ums2f1 ran out of integer memory, but did not
c           set error flag
c  xout:    true if ums2f2 ran out of integer memory, but did not
c           set error flag
c
c  estimated memory for ums2rf:
c  ----------------------------
c  rmax:    largest contribution block is cmax-by-rmax
c  cmax:       "         "         "    "   "   "  "
c  totnlu:  total number of lu arrowheads in all diagonal blocks
c  xrmax:   estimated maximum real memory usage for ums2rf
c  xruse:   estimated current real memory usage for ums2rf
c
c  other:
c  ------
c  k:       loop index (kth pivot)
c  row:     row index
c  col:     column index
c  p:       pointer
c  p2:      pointer
c  nsym:    number of symmetric pivots chosen
 
c=======================================================================
c  executable statements:
c=======================================================================
 
c-----------------------------------------------------------------------
c  get input parameters and initialize
c-----------------------------------------------------------------------
 
        nblks = 1
        nzoff = 0
        nzdia = nz
        nsgltn = 0
        npiv = 0
        rmax = 1
        cmax = 1
        totnlu = 0
        if (presrv) then
c          original matrix is not in cp/ii/xx, but in ap/ai/ax:
           ihead = 1
           xhead = 1
        else
           ihead = nz + 1
           xhead = nz + 1
           endif
        itail = isize + 1
        xtail = xsize + 1
 
c-----------------------------------------------------------------------
c  allocate permutation arrays: cperm (1..n) and rperm (1..n), and
c  seven scalars:  transa, nzoff, nblks, presrv, nz, n, ne
c  (in that order) at tail of ii (in lu factors)
c-----------------------------------------------------------------------
 
        itail = itail - (2*n+7)
        iuse = iuse + (2*n+7)
        info (18) = max (info (18), iuse)
        info (19) = info (18)
        cpermp = itail
        rpermp = cpermp + n
        if (ihead .gt. itail) then
c          error return, if not enough integer memory:
           go to 9000
           endif
 
c-----------------------------------------------------------------------
c  find permutations to block upper triangular form, if requested.
c-----------------------------------------------------------------------
 
        trybtf = icntl (4) .eq. 1
        if (trybtf) then
 
c          -------------------------------------------------------------
c          get workspace at tail of ii of size 6n+2
c          -------------------------------------------------------------
 
           itail = itail - (n+1)
           offpp = itail
           itail = itail - (5*n+1)
           p = itail
           iuse = iuse + (6*n+2)
           info (18) = max (info (18), iuse)
           info (19) = info (18)
 
c          -------------------------------------------------------------
           if (presrv) then
c          find permutation, but do not convert to btf form
c          -------------------------------------------------------------
 
              if (ihead .gt. itail) then
c                error return, if not enough integer memory:
                 go to 9000
                 endif
              call ums2fb (ax, anz, ai, anz, n, nz, nzdia, nzoff,
     $           nblks, cp, ii (cpermp), ii (rpermp), ii(p), ii(p+n),
     $           ii (p+2*n), ii (p+3*n), ii (p+4*n), ii (offpp),
     $           presrv, icntl)
 
c          -------------------------------------------------------------
           else
c          find permutation, convert to btf form, and discard original
c          -------------------------------------------------------------
 
c             use additional size nz temporary workspace in ii and xx
              ihead = ihead + nz
              xhead = xhead + nz
              iuse = iuse + nz
              xuse = xuse + nz
              info (18) = max (info (18), iuse)
              info (20) = max (info (20), xuse)
              info (19) = info (18)
              info (21) = info (20)
              if (ihead .gt. itail .or. xhead .gt. xtail) then
c                error return, if not enough integer and/or real memory:
                 go to 9000
                 endif
              call ums2fb (xx, 2*nz, ii, 2*nz, n, nz, nzdia, nzoff,
     $              nblks, cp, ii (cpermp), ii (rpermp), ii(p), ii(p+n),
     $              ii (p+2*n), ii (p+3*n), ii (p+4*n), ii (offpp),
     $              presrv, icntl)
c             deallocate extra workspace in ii and xx
              ihead = ihead - nz
              xhead = xhead - nz
              iuse = iuse - nz
              xuse = xuse - nz
              endif
 
c          -------------------------------------------------------------
c          deallocate workspace, and allocate btf arrays if required
c          -------------------------------------------------------------
 
           if (nblks .gt. 1) then
c             replace (6*n+2) workspace at tail of ii with
c             blkp (1..nblks+1) and lublkp (1..nblks), offp (1..n+1)
              blkpp = offpp - (nblks+1)
              lublpp = blkpp - (nblks)
              itail = lublpp
              iuse = iuse - (6*n+2) + (2*nblks+n+2)
           else
c             the matrix is irreducible.  there is only one block.
c             remove everything at tail of ii, except
c             for the 2*n permutation vectors and the 7 scalars.
c             (transa, nzoff, nblks, presrv, nz, n, ne).
              itail = (isize + 1) - (2*n+7)
              iuse = iuse - (6*n+2)
              endif
 
           endif
 
c-----------------------------------------------------------------------
c current memory usage:
c-----------------------------------------------------------------------
 
c       if .not. presrv then
c               input matrix is now in ii (1..nz) and xx (1..nz)
c               off-diagonal part: in ii/xx (1..nzoff)
c                       col pattern: ii (offp (col) ... offp (col+1))
c                       col values:  xx (offp (col) ... offp (col+1))
c               diagonal blocks: in ii/xx (nzoff+1..nz)
c                       col pattern: ii (cp (col) ... cp (col+1))
c                       col values:  xx (cp (col) ... cp (col+1))
c               total: nz+n+1 integers, nz reals
c       else
c               input matrix is now in ai (1..nz) and ax (1..nz),
c               in original (non-btf) order:
c                       col pattern: ai (ap (col) ... ap (col+1))
c                       col values:  ax (ap (col) ... ap (col+1))
c               cp is a size n+1 integer workspace
c               total: nz+2*(n+1) integers, nz reals
c
c       if (nblks > 1) then
c               at tail of ii (in order): 2*nblks+n+2
c                       lublkp (1..nblks)
c                       blkp (1..nblks+1)
c                       offp (1..n+1)
c               total: (2*nblks+n+2) integers
c
c       remainder at tail of ii:
c               cperm (1..n)
c               rperm (1..n)
c               seven scalars: transa, nzoff, nblks, presrv, nz, n, ne
c
c   grand total current memory usage (including ii,xx,cp,ai,ap,ax):
c
c       presrv  nblks>1         integers, iuse =
c       f       f               nz+  (n+1)+(2*n+7)
c       f       t               nz+  (n+1)+(2*n+7)+(2*nblks+n+2)
c       t       f               nz+2*(n+1)+(2*n+7)
c       t       t               nz+2*(n+1)+(2*n+7)+(2*nblks+n+2)
c
c   real usage is xuse = nz
 
c       ----------------------------------------------------------------
c       get memory usage for next call to ums2rf
c       ----------------------------------------------------------------
 
        xrmax = 2*ne
        xruse = nz
 
c-----------------------------------------------------------------------
c factorization
c-----------------------------------------------------------------------
 
        if (nblks .eq. 1) then
 
c          -------------------------------------------------------------
c          factorize the matrix as a single block
c          -------------------------------------------------------------
 
           call ums2f1 (cp, n, ii (cpermp), ii (rpermp), nzoff,
     $          itail, xtail, xx, xsize, xuse, ii, itail-1, iuse,
     $          icntl, cntl, info, rinfo, nblks,
     $          ap, ai, ax, presrv, 1, an, anz, ii, keep,
     $          rmax, cmax, totnlu, xrmax, xruse, iout, xout)
           if (iout .or. xout) then
c             error return, if not enough integer and/or real memory:
              go to 9000
              endif
           if (info (1) .lt. 0) then
c             error return, if error in ums2f2:
              go to 9010
              endif
c          original matrix has been deallocated
           ihead = 1
           xhead = 1
 
c          -------------------------------------------------------------
c          make the index of the block relative to start of lu factors
c          -------------------------------------------------------------
 
           ii (itail) = 1
 
        else
 
c          -------------------------------------------------------------
c          factorize the block-upper-triangular form of the matrix
c          -------------------------------------------------------------
 
           prp = offpp
           if (presrv) then
c             count the off-diagonal entries during factorization
              nzoff = 0
c             compute temp inverse permutation in ii (prp..prp+n-1)
cfpp$ nodepchk l
              do 10 k = 1, n
                 ii (prp + ii (rpermp+k-1) - 1) = k
10            continue
           endif
 
           do 30 blk = nblks, 1, -1
 
c             ----------------------------------------------------------
c             factorize the kn-by-kn block, a (k1..k2, k1..k2)
c             ----------------------------------------------------------
 
c             get k1 and k2, the start and end of this block
              k1 = ii (blkpp+blk-1)
              k2 = ii (blkpp+blk) - 1
              kn = k2-k1+1
              if (.not. presrv) then
                 p = cp (k1)
                 cp (k2+1) = ihead
              endif
 
              if (kn .gt. 1) then
 
c                -------------------------------------------------------
c                factor the block (the block is not a singleton)
c                -------------------------------------------------------
 
                 call ums2f1 (cp (k1), kn,
     $              ii (cpermp+k1-1), ii (rpermp+k1-1), nzoff,
     $              itail, xtail, xx, xtail-1, xuse, ii, itail-1,
     $              iuse, icntl, cntl, info, rinfo, nblks,
     $              ap, ai, ax, presrv, k1, an, anz, ii (prp), keep,
     $              rmax, cmax, totnlu, xrmax, xruse, iout, xout)
                 if (iout .or. xout) then
c                   error return, if not enough int. and/or real memory:
                    go to 9000
                 endif
                 if (info (1) .lt. 0) then
c                   error return, if error in ums2f2:
                    go to 9010
                 endif
                 if (presrv) then
                    ihead = 1
                    xhead = 1
                 else
                    ihead = p
                    xhead = p
                 endif
 
c                -------------------------------------------------------
c                save the location of the lu factors in lubkp (blk)
c                -------------------------------------------------------
 
                 ii (lublpp+blk-1) = itail
 
              else
 
c                -------------------------------------------------------
c                get the value of singleton at a (k1,k1), if it exists
c                -------------------------------------------------------
 
                 a = zero
                 if (presrv) then
c                   find the diagonal entry in the unpermuted matrix
                    col = ii (cpermp + k1 - 1)
                    do 20 p2 = ap (col), ap (col + 1) - 1
                       row = ii (prp + ai (p2) - 1)
                       if (row .lt. k1) then
c                         entry in off-diagonal blocks
                          nzoff = nzoff + 1
                       else
                          a = ax (p2)
                       endif
20                  continue
                    ihead = 1
                    xhead = 1
                 else if (p .ne. ihead) then
                    a = xx (p)
                    ihead = p
                    xhead = p
                    iuse = iuse - 1
                    xuse = xuse - 1
                    xruse = xruse - 1
                 endif
 
c                -------------------------------------------------------
c                store the 1-by-1 lu factors of a singleton
c                -------------------------------------------------------
 
                 nsgltn = nsgltn + 1
                 if (a .eq. zero) then
c                   the diagonal entry is either not present, or present
c                   but numerically zero.  this is a singular matrix,
c                   replace with 1-by-1 identity matrix.
                    a = one
                 else
c                   increment pivot count
                    npiv = npiv + 1
                 endif
                 xtail = xtail - 1
c                note: if the matrix is not preserved and nonsingular
c                then we will not run out of memory at this point.
                 xuse = xuse + 1
                 xruse = xruse + 1
                 xrmax = max (xrmax, xruse)
                 info (20) = max (info (20), xuse)
                 info (21) = max (info (21), xuse)
c                error return, if not enough real memory:
                 if (xhead .gt. xtail) then
                    go to 9000
                 endif
                 ii (lublpp+blk-1) = -xtail
                 xx (xtail) = a
 
              endif
 
30         continue
 
c          -------------------------------------------------------------
c          make the index of each block relative to start of lu factors
c          -------------------------------------------------------------
 
cfpp$ nodepchk l
           do 40 p = lublpp, lublpp + nblks - 1
              if (ii (p) .gt. 0) then
                 ii (ii (p)) = ii (ii (p)) - xtail + 1
                 ii (p) = ii (p) - itail + 1
              else
c                this is a singleton
                 ii (p) = (-ii (p)) - xtail + 1
              endif
40         continue
 
c          -------------------------------------------------------------
c          allocate temporary workspace for pr (1..n) at head of ii
c          -------------------------------------------------------------
 
           prp = ihead
           ihead = ihead + n
           iuse = iuse + n
 
c          -------------------------------------------------------------
c          allocate a single entry in case the lu factors are empty
c          -------------------------------------------------------------
 
           if (nblks .eq. n) then
c             otherwise, arrays in ums2rf and ums2so would have
c             zero size, which can cause an address fault later on
              itail = itail - 1
              iuse = iuse + 1
              p2 = itail
           endif
 
c          -------------------------------------------------------------
c          allocate permanent copy of off-diagonal blocks
c          -------------------------------------------------------------
 
           itail = itail - nzoff
           offip = itail
           xtail = xtail - nzoff
           offxp = xtail
           iuse = iuse + nzoff
           xuse = xuse + nzoff
           xruse = xruse + nzoff
           xrmax = max (xrmax, xruse)
           info (18) = max (info (18), iuse)
           info (19) = max (info (19), iuse)
           info (20) = max (info (20), xuse)
           info (21) = max (info (21), xuse)
           if (ihead .gt. itail .or. xhead .gt. xtail) then
c             error return, if not enough integer and/or real memory:
              go to 9000
           endif
 
c          -------------------------------------------------------------
c          re-order the off-diagonal blocks according to pivot perm
c          -------------------------------------------------------------
 
c          use cp as temporary work array:
           mnz = nzoff
           if (presrv) then
              call ums2of (cp, n, ii (rpermp), ii (cpermp), nzoff,
     $          ii (offpp), ii (offip), xx (offxp), ii (prp),
     $          icntl, ap, ai, ax, an, anz, presrv, nblks, ii (blkpp),
     $          mnz, 1, info, p)
           else
              call ums2of (cp, n, ii (rpermp), ii (cpermp), nzoff,
     $          ii (offpp), ii (offip), xx (offxp), ii (prp),
     $          icntl, 0, ii, xx, 0, mnz, presrv, 0, 0,
     $          mnz, 1, info, p)
           endif
           if (nblks .eq. n) then
c             zero the only entry in the integer part of the lu factors
              ii (p2) = 0
           endif
 
c          -------------------------------------------------------------
c          deallocate pr (1..n), and ii/xx (1..nzoff) if present
c          -------------------------------------------------------------
 
           ihead = 1
           xhead = 1
           iuse = iuse - n
           if (.not. presrv) then
              iuse = iuse - nzoff
              xuse = xuse - nzoff
           endif
 
        endif
 
c-----------------------------------------------------------------------
c  normal and error return
c-----------------------------------------------------------------------
 
c       error return label:
9000    continue
        if (iout .or. ihead .gt. itail) then
c          set error flag if not enough integer memory
           call ums2er (1, icntl, info, -3, info (19))
        endif
        if (xout .or. xhead .gt. xtail) then
c          set error flag if not enough real memory
           call ums2er (1, icntl, info, -4, info (21))
        endif
 
c       error return label, for error from ums2f2:
9010    continue
 
        info (4) = 0
        nzdia = nz - nzoff
        info (5) = nz
        info (6) = nzdia
        info (7) = nzoff
        info (8) = nsgltn
        info (9) = nblks
        info (12) = info (10) + info (11) + n + info (7)
 
c       count the number of symmetric pivots chosen.  note that some of
c       these may have been numerically unacceptable.
        nsym = 0
        if (info (1) .ge. 0) then
           do 50 k = 1, n
              if (ii (cpermp+k-1) .eq. ii (rpermp+k-1)) then
c                this kth pivot came from the diagonal of a
                 nsym = nsym + 1
              endif
50         continue
        endif
        info (16) = nsym
 
        info (17) = info (17) + npiv
        rinfo (1) = rinfo (4) + rinfo (5) + rinfo (6)
 
        if (info (1) .ge. 0 .and. info (17) .lt. n) then
c          set warning flag if matrix is singular
           call ums2er (1, icntl, info, 4, info (17))
        endif
 
c       ----------------------------------------------------------------
c       determine an upper bound on the amount of integer memory needed
c       (lindex) for a subsequent call to ums2rf.  if block-upper-
c       triangular-form is not in use (info (9) is 1), then
c       this bound is exact.  if ne is higher in the call to ums2rf
c       than in the call to ums2fa, then add 3 integers for each
c       additional entry (including the 2 integers required for the
c       row and column indices of the additional triplet itself).
c       this estimate assumes that job and transa are the same in
c       ums2fa and ums2rf.
c       ----------------------------------------------------------------
 
c       (keep (5) - keep (4) + 1), is added to info (22)
c       in ums2fa, to complete the computation of the estimate.
 
        if (presrv) then
           info (22) = max (3*ne+2*n+1, ne+3*n+2,
     $                           2*nz+4*n+10+rmax+3*cmax+4*totnlu)
        else
           info (22) = max (3*ne+2*n+1, ne+3*n+2, 2*nz+3*n+2,
     $                             nz+3*n+ 9+rmax+3*cmax+4*totnlu)
        endif
 
c       ----------------------------------------------------------------
c       approximate the amount of real memory needed (lvalue) for a
c       subsequent call to ums2rf.  the approximation is an upper bound
c       on the bare minimum amount needed.  some garbage collection may
c       occur, but ums2rf is guaranteed to finish if given an lvalue of
c       size info (23) and if the pattern is the same.  if ne is
c       higher in the call to ums2rf than in the call to ums2fa, then
c       add 2 reals for each additional entry (including the 1 real
c       required for the value of the additional triplet itself).
c       this estimate assumes that job and transa are the same in
c       ums2fa and ums2rf.
c       ----------------------------------------------------------------
 
        info (23) = xrmax
        return
        end
 
        subroutine ums2f1 (cp, n, cperm, rperm, nzoff,
     $          itail, xtail, xx, xsize, xuse, ii, isize, iuse,
     $          icntl, cntl, info, rinfo, nblks,
     $          ap, ai, ax, presrv, k1, an, anz, pr, keep,
     $          rmax, cmax, totnlu, xrmax, xruse, iout, xout)
        integer xsize, isize, n, icntl (20), info (40), xuse, iuse,
     $          itail, xtail, ii (isize), cp (n+1), cperm (n), nzoff,
     $          an, anz, rperm (n), ai (anz), ap (an+1), k1, pr (an),
     $          nblks, keep (20), rmax, cmax, totnlu, xrmax, xruse
        logical presrv, iout, xout
        real
     $          xx (xsize), cntl (10), rinfo (20), ax (anz)
 
c=== ums2f1 ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  ums2f1 factorizes the n-by-n column-form matrix at the head of ii/xx
c  or in ap/ai/ax, and places its lu factors at the tail of ii/xx.  the
c  input matrix overwritten if it is located in ii/xx on input.  if
c  block-triangular-form (btf) is in use, this routine factorizes a
c  single diagonal block.
 
c=======================================================================
c  input:
c=======================================================================
c
c       n:              order of matrix (or order of diagonal block
c                       if btf is in use).
c       cp (1..n+1):    column pointers for input matrix
c       nblks:          number of diagonal blocks in btf form
c       isize:          size of ii
c       xsize:          size of xx
c       k1:             first index of this matrix (1 if btf not used)
c       icntl:          integer control parameters, see ums2in
c       cntl:           real control parameters, see ums2in
c       keep (6..8):    integer control parameters, see ums2in
c       iuse:           memory usage in index
c       xuse:           memory usage in value
c       rmax:           maximum ludegr seen so far (see ums2f2 for info)
c       cmax:           maximum ludegc seen so far (see ums2f2 for info)
c       totnlu:         total number of lu arrowheads constructed so far
c
c       if nblks>1 then:
c          cperm (1..n):        col permutation to btf
c          rperm (1..n):        row permutation to btf
c       else
c          cperm (1..n):        undefined on input
c          rperm (1..n):        undefined on input
c
c
c       presrv:         if true then input matrix is preserved
c
c       if presrv is true then:
c           an:                 order of preserved matrix (all blocks)
c           anz:                entries in preserved matrix
c           ap (1..an+1):       column pointers for preserved matrix
c           ai (1..anz):        row indices of preserved matrix
c           ax (1..anz):        values of preserved matrix
c                               the preserved matrix is not in btf form;
c                               it is in the orginal order.
c           if nblks > 1:
c               pr (1..n):      inverse row permutations to btf form
c               nzoff           entries in off-diagonal blocks
c                               seen so far
c           else
c               pr (1..n):      undefined on input
c
c           ii (1..isize):      undefined on input
c           xx (1..xsize):      undefined on input
c           cp (1..n+1):        undefined on input
c
c       else, if presrv is false:
c           an:                         1
c           anz:                        1
c           ii (1..cp (1) - 1):         unused
c           ii (cp (1) ... cp (n+1)-1): row indices of matrix to factor,
c                                       will be overwritten on output
c           ii (cp (n+1) ... isize):    unused on input
c
c           xx (1..cp (1) - 1):         unused
c           xx (cp (1) ... cp (n+1)-1): values of matrix to factorize,
c                                       will be overwritten on output
c           xx (cp (n+1) ... xsize):    unused on input
c                       if btf is in use, then ii and xx contain a
c                       single diagonal block.
 
c=======================================================================
c  output:
c=======================================================================
c
c       xx (xtail ... xsize), xtail,  ii (itail ... isize), itail:
c
c                       the lu factors of a single diagonal block.
c                       see ums2f2 for a description.
c
c       ii (cp1 ... itail-1):   undefined on output
c       xx (cp1 ... xtail-1):   undefined on output,
c                       where cp1 is equal to the value of cp (1)
c                       if presrv is false, or cp1 = 1 if presrv is
c                       true.
c
c       info:           integer informational output, see ums2fa
c       rinfo:          real informational output, see ums2fa
c       cperm (1..n):   the final col permutations, including btf
c       rperm (1..n):   the final row permutations, including btf
c
c       iuse:           memory usage in index
c       xuse:           memory usage in value
c       rmax:           maximum ludegr seen so far (see ums2f2 for info)
c       cmax:           maximum ludegc seen so far (see ums2f2 for info)
c       totnlu:         total number of lu arrowheads constructed so far
c
c       if nblks>1 and presrv:
c           nzoff       entries in off-diagonal blocks seen so far
c
c       iout:           true if ran out of integer memory in ums2f1
c       xout:           true if ran out of real memory in ums2f1
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2f0
c       subroutines called:     ums2f2
c       functions called:       max, sqrt
        intrinsic max, sqrt
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer cp1, pc, pend, pcol, cdeg, col, csiz, nz, xp, ip, is, p,
     $          dn, dsiz, wrksiz, i, clen, d1, d2, n2, row, cscal
        parameter (cscal = 9)
        real
     $          xn
 
c  original and expanded column-form:
c  ----------------------------------
c  cp1:     = value of cp (1) on input
c  pc:      pointer to integer part of expanded column-form matrix
c  pend:    column col ends here in the input column-form matrix
c  pcol:    column col starts here in the input column-form matrix
c  cdeg:    degree (number of entries) in a column
c  clen:    number of original entries in a column (= degree, here,
c           but decreases in ums2f2)
c  csiz:    size of the integer part of an expanded column (cdeg+cscal)
c  cscal:   = 9, the number of scalars in column data structure
c  nz:      number of entries in the diagonal block being factorized
c  xp:      pointer to real part of the expanded column
c  ip:      pointer to integer part of the expanded column
c
c  memory usage:
c  -------------
c  wrksiz:  size of integer workspace needed by ums2f2
c
c  "dense" columns: (converted to a prior, or artificial, frontal mat.):
c  ----------------
c  d1:      = keep (7), dense column control
c  d2:      = keep (8), dense column control
c  dn:      number of "dense" columns
c  dsiz:    a column is "dense" if it has more than dsiz entries
c  xn:      = sqrt (real (n))
c  n2:      = int (sqrt (real (n)))
c
c  other:
c  ------
c  row:     row index
c  col:     a column index
c  i:       loop index
c  p:       pointer
 
c=======================================================================
c  executable statements:
c=======================================================================
 
        iout = .false.
        xout = .false.
 
c-----------------------------------------------------------------------
c  count "dense" columns (they are treated as a priori frontal matrices)
c-----------------------------------------------------------------------
 
c       a column is "dense" if it has more than dsiz entries
        d1 = keep (7)
        d2 = keep (8)
        xn = n
        xn = sqrt (xn)
        n2 = xn
        dsiz = max (0, d1, d2 * n2)
        dn = 0
        if (presrv) then
           if (nblks .eq. 1) then
              do 10 col = 1, n
                 if (ap (col+1) - ap (col) .gt. dsiz) then
c                   this is a "dense" column
                    dn = dn + 1
                    endif
10            continue
           else
              do 40 col = 1, n
c                if col might be dense, check more carefully:
                 cdeg = ap (cperm (col) + 1)- ap (cperm (col))
                 if (cdeg .gt. dsiz) then
                    cdeg = 0
                    do 20 p = ap (cperm (col)), ap (cperm (col) + 1) -1
                       row = pr (ai (p))
                       if (row .ge. k1) then
                          cdeg = cdeg + 1
                          if (cdeg .gt. dsiz) then
c                            this is a "dense" column, exit out of loop
                             dn = dn + 1
                             go to 30
                          endif
                       endif
20                  continue
c                   loop exit label:
30                  continue
                 endif
40            continue
           endif
        else
           do 50 col = 1, n
              if (cp (col+1) - cp (col) .gt. dsiz) then
c                this is a "dense" column
                 dn = dn + 1
              endif
50         continue
        endif
 
c-----------------------------------------------------------------------
c  get size of workspaces to allocate from ii
c-----------------------------------------------------------------------
 
c       workspaces: wir (n), wic (n), wpr (n), wpc (n),
c       wm (n), head (n), rp (n+dn), wc (n+dn), wr (n+dn), wj (n)
        if (nblks .eq. 1) then
c          rperm (1..n) is used as wir (1..n), and
c          cperm (1..n) is used as wic (1..n) in ums2f2
           wrksiz = 8*n + 3*dn
        else
           wrksiz = 10*n + 3*dn
        endif
 
c-----------------------------------------------------------------------
c  construct the expanded column-form of the matrix or the diag. block
c-----------------------------------------------------------------------
 
        if (presrv) then
 
c          -------------------------------------------------------------
c          allocate space for wrksiz workspace and nz+cscal*n
c          integers and nz reals for the expanded column-form matrix.
c          -------------------------------------------------------------
 
           cp1 = 1
           xp = 1
           ip = 1 + wrksiz
           if (nblks .eq. 1) then
 
c             ----------------------------------------------------------
c             construct copy of entire matrix
c             ----------------------------------------------------------
 
              nz = anz
              is = nz + wrksiz + cscal*n
              iuse = iuse + is
              xuse = xuse + nz
              info (18) = max (info (18), iuse)
              info (19) = max (info (19), iuse)
              info (20) = max (info (20), xuse)
              info (21) = max (info (21), xuse)
              iout = is .gt. isize
              xout = nz .gt. xsize
              if (iout .or. xout) then
c                error return, if not enough integer and/or real memory:
                 go to 9000
              endif
 
              pc = ip
              do 70 col = 1, n
                 cp (col) = pc - wrksiz
                 cdeg = ap (col+1) - ap (col)
                 clen = cdeg
                 csiz = cdeg + cscal
                 ii (pc) = csiz
                 ii (pc+1) = cdeg
                 ii (pc+5) = 0
                 ii (pc+6) = clen
                 ii (pc+7) = 0
                 ii (pc+8) = 0
                 ii (pc+2) = xp
                 xp = xp + cdeg
                 pc = pc + cscal
                 p = ap (col)
                 do 60 i = 0, cdeg - 1
                    ii (pc + i) = ai (p + i)
60               continue
                 pc = pc + cdeg
70            continue
              do 80 p = 1, nz
                 xx (p) = ax (p)
80            continue
 
           else
 
c             ----------------------------------------------------------
c             construct copy of a single block in btf form
c             ----------------------------------------------------------
 
c             check for memory usage during construction of block
              do 100 col = 1, n
                 pc = ip
                 cp (col) = pc - wrksiz
                 ip = ip + cscal
                 iout = ip .gt. isize
                 if (iout) then
c                   error return, if not enough integer memory:
                    go to 9000
                 endif
                 ii (pc+2) = xp
                 cdeg = ip
                 do 90 p = ap (cperm (col)), ap (cperm (col)+1)-1
                    row = pr (ai (p))
                    if (row .ge. k1) then
                       iout = ip .gt. isize
                       xout = xp .gt. xsize
                       if (iout .or. xout) then
c                         error return, if not enough memory
                          go to 9000
                       endif
                       ii (ip) = row - k1 + 1
                       xx (xp) = ax (p)
                       ip = ip + 1
                       xp = xp + 1
                    else
c                      entry in off-diagonal part
                       nzoff = nzoff + 1
                    endif
90               continue
                 cdeg = ip - cdeg
                 clen = cdeg
                 csiz = cdeg + cscal
                 ii (pc) = csiz
                 ii (pc+1) = cdeg
                 ii (pc+5) = 0
                 ii (pc+6) = clen
                 ii (pc+7) = 0
                 ii (pc+8) = 0
100           continue
 
              nz = xp - 1
              is = nz + wrksiz + cscal*n
              iuse = iuse + is
              xuse = xuse + nz
              info (18) = max (info (18), iuse)
              info (19) = max (info (19), iuse)
              info (20) = max (info (20), xuse)
              info (21) = max (info (21), xuse)
 
           endif
 
c          -------------------------------------------------------------
c          get memory usage for next call to ums2rf
c          -------------------------------------------------------------
 
           xruse = xruse + nz
           xrmax = max (xrmax, xruse)
 
        else
 
c          -------------------------------------------------------------
c          allocate space for wrksiz workspace and additional cscal*n
c          space for the expanded column-form of the matrix.
c          -------------------------------------------------------------
 
           cp1 = cp (1)
           nz = cp (n+1) - cp1
           pc = cp1 + wrksiz + (nz+cscal*n)
           iuse = iuse + wrksiz + cscal*n
           info (18) = max (info (18), iuse)
           info (19) = max (info (19), iuse)
           iout = pc .gt. isize+1
           if (iout) then
c             error return, if not enough integer memory:
              go to 9000
           endif
 
c          -------------------------------------------------------------
c          expand the column form in place and make space for workspace
c          -------------------------------------------------------------
 
           xp = nz + 1
           ip = nz + cscal*n + 1
           pend = cp (n+1)
           do 120 col = n, 1, -1
              pcol = cp (col)
              do 110 p = pend-1, pcol, -1
                 pc = pc - 1
                 ii (pc) = ii (p)
110           continue
              pc = pc - cscal
              cdeg = pend - pcol
              clen = cdeg
              pend = pcol
              csiz = cdeg + cscal
              ip = ip - csiz
              cp (col) = ip
              ii (pc) = csiz
              ii (pc+1) = cdeg
              ii (pc+5) = 0
              ii (pc+6) = clen
              ii (pc+7) = 0
              ii (pc+8) = 0
              xp = xp - cdeg
              ii (pc+2) = xp
120        continue
        endif
 
c-----------------------------------------------------------------------
c  factorize the expanded column-form, with allocated workspaces
c-----------------------------------------------------------------------
 
        xp = cp1
        ip = cp1 + wrksiz
 
        if (nblks .eq. 1) then
 
c          pass rperm and cperm as the wir and wic arrays in ums2f2:
           call ums2f2 (cp, nz, n, 1, 1, 1, itail, xtail,
     $          xx (xp), xsize-xp+1, ii (ip), isize-ip+1, icntl, cntl,
     $          info, rinfo, .false., iuse, xuse,
     $          rperm, cperm, ii (cp1), ii (cp1+n),
     $          ii (cp1+2*n), ii (cp1+3*n), ii (cp1+4*n), ii (cp1+5*n),
     $          ii (cp1+6*n+dn), ii (cp1+7*n+2*dn),
     $          dn, dsiz, keep, rmax, cmax, totnlu, xrmax, xruse)
 
        else
 
c          pass cperm, rperm, wic and wir as separate arrays, and
c          change cperm and rperm from the btf permutations to the
c          final permutations (including btf and numerical pivoting).
           call ums2f2 (cp, nz, n, n, cperm, rperm, itail, xtail,
     $          xx (xp), xsize-xp+1, ii (ip), isize-ip+1, icntl, cntl,
     $          info, rinfo, .true., iuse, xuse,
     $          ii (cp1), ii (cp1+n), ii (cp1+2*n), ii (cp1+3*n),
     $          ii (cp1+4*n), ii (cp1+5*n), ii (cp1+6*n), ii (cp1+7*n),
     $          ii (cp1+8*n+dn), ii (cp1+9*n+2*dn),
     $          dn, dsiz, keep, rmax, cmax, totnlu, xrmax, xruse)
 
        endif
 
        if (info (1) .lt. 0) then
c          error return, if error occured in ums2f2:
           return
        endif
 
c-----------------------------------------------------------------------
c  adjust tail pointers, and save pointer to numerical part of lu
c-----------------------------------------------------------------------
 
c       head = cp1
        iuse = iuse - wrksiz
        itail = itail + ip - 1
        xtail = xtail + xp - 1
        ii (itail) = xtail
        return
 
c=======================================================================
c  error return
c=======================================================================
 
c       error return label:
9000    continue
        return
        end
 
        subroutine ums2f2 (cp, nz, n, pn, cperm, rperm, itail, xtail,
     $          xx, xsize, ii, isize, icntl, cntl, info, rinfo, pgiven,
     $          iuse, xuse, wir, wic, wpr, wpc, wm, head,
     $          wj, rp, wc, wr, dn, dsiz, keep,
     $          rmax, cmax, totnlu, xrmax, xruse)
        integer xsize, isize, icntl (20), info (40), pn,
     $          itail, xtail, nz, n, ii (isize), cp (n+1), dn, dsiz,
     $          rperm (pn), cperm (pn), wir (n), wic (n), wpr (n),
     $          wpc (n), wm (n), head (n), rp (n+dn), wc (n+dn),
     $          wr (n+dn), iuse, xuse, wj (n), keep (20),
     $          rmax, cmax, totnlu, xrmax, xruse
        logical pgiven
        real
     $          xx (xsize), cntl (10), rinfo (20)
 
c=== ums2f2 ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  ums2f2 factorizes the n-by-n input matrix at the head of ii/xx
c  (in expanded column-form) and places its lu factors at the tail of
c  ii/xx.  the input matrix is overwritten.   no btf information is
c  used in this routine, except that the btf permutation arrays are
c  modified to include the final permutations.
 
c=======================================================================
c  input:
c=======================================================================
c
c       cp (1..n+1):    column pointers of expanded column-form,
c                       undefined on output
c       n:              order of input matrix
c       nz:             entries in input matrix
c       isize:          size of ii
c       xsize:          size of xx
c       iuse:           memory usage in index
c       xuse:           memory usage in value
c       icntl:          integer control parameters, see ums2in
c       cntl:           real control parameters, see ums2in
c       keep (6)        integer control parameter, see ums2in
c       dn:             number of dense columns
c       dsiz:           entries required for col to be treated as dense
c       rmax:           maximum ludegr seen so far (see below)
c       cmax:           maximum ludegc seen so far (see below)
c       totnlu:         total number of lu arrowheads constructed so far
c       xrmax:          maximum real memory usage for ums2rf
c       xruse:          current real memory usage for ums2rf
c
c       pgiven:         true if cperm and rperm are defined on input
c       if pgiven then:
c          cperm (1..pn):       col permutation to btf, n = pn
c          rperm (1..pn):       row permutation to btf
c       else
c          cperm (1..pn):       unaccessed pn = 1
c          rperm (1..pn):       unaccessed
c
c       ii (1..nz+cscal*n):             expanded column-form, see below
c       ii (nz+cscal*n+1..isize):       undefined on input
c       xx (1..nz):                     expanded column-form, see below
c       xx (nz+1..xsize):               undefined on input
 
c=======================================================================
c  workspace:
c=======================================================================
c
c       wir (1..n)
c       wic (1..n)
c       wpr (1..n)
c       wpc (1..n)
c       wm (1..n)
c       head (n)
c       rp (1..n+dn)
c       wr (1..n+dn)
c       wc (1..n+dn)
 
c=======================================================================
c  output:
c=======================================================================
c
c       ii (1..itail-1):        undefined on output
c       ii (itail..isize):      lu factors of this matrix, see below
c       xx (1..xtail-1):        undefined on output
c       xx (xtail..xsize):      lu factors of this matrix, see below
c
c       info:           integer informational output, see ums2fa
c       rinfo:          real informational output, see ums2fa
c       if pgiven:
c          cperm (1..n): the final col permutations, including btf
c          rperm (1..n): the final row permutations, including btf
c
c       wic (1..n):     row permutations, not including btf
c       wir (1..n):     column permutations, not including btf
c
c       iuse:           memory usage in index
c       xuse:           memory usage in value
c       rmax:           maximum ludegr seen so far (see below)
c       cmax:           maximum ludegc seen so far (see below)
c       totnlu:         total number of lu arrowheads constructed so far
c       xrmax:          maximum real memory usage for ums2rf
c       xruse:          current real memory usage for ums2rf
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2f1
c       subroutines called:     ums2er, ums2fg, sgemv, sgemm
c       functions called:       isamax, abs, max, min
        integer isamax
        intrinsic abs, max, min
 
c=======================================================================
c  description of data structures:
c=======================================================================
 
c-----------------------------------------------------------------------
c  column/element/arrowhead pointers:
c-----------------------------------------------------------------------
c
c  the cp (1..n) array contains information about non-pivotal columns
c
c       p = cp (j)
c       if (p = 0) then j is a pivotal column
c       else i is a non-pivotal column
c
c  the rp (1..n) array contains information about non-pivotal rows,
c  unassembled frontal matrices (elements), and the lu arrowheads
c
c       p = rp (i)
c       if (i > n) then
c          i is an artificial frontal matrix (a dense column)
c          if (p = 0) then i is assembled, else unassembled
c       else if (p = 0) then i is pivotal but not element/arrowhead
c       else if (wc (i) >= 0 and wc (i) <= n) then
c          i is a non-pivotal row
c       else if (wc (i) = -(n+dn+2)) then
c          i is a pivotal row, an assembled element, and an lu arrowhead
c       else i an unassembled element
 
c-----------------------------------------------------------------------
c  matrix being factorized:
c-----------------------------------------------------------------------
c
c    each column is stored in ii and xx:
c    -----------------------------------
c
c       if j is a non-pivotal column, pc = cp (j):
c
c       csiz = ii (pc) size of the integer data structure for col j,
c                        including the cscal scalars
c       cdeg = ii (pc+1) degree of column j
c       cxp  = ii (pc+2) pointer into xx for numerical values
c       next = ii (pc+3) pointer to next block of memory in xx
c       prev = ii (pc+4) pointer to previous block of memory in xx
c       celn = ii (pc+5) number of elements in column j element list
c       clen = ii (pc+6) number of original entries in column j
c       cnxt = ii (pc+7) next column with same degree as col j
c       cprv = ii (pc+8) previous column with same degree as col j
c       cep = (pc+9) pointer to start of the element list
c       ii (cep ... cep + 2*celn - 1)
c                       element list (e,f) for the column
c       ii (cep + 2*celn ... pc + csiz - clen - 1)
c                       empty
c       ii (pc + csiz - clen ... pc + csiz - 1)
c                       row indices of original nonzeros in the column
c       xx (xp ... xp + clen - 1)
c                       numerical values of original nonzeros in the col
c
c       if cdeg = ii (pc+1) = -(n+2), then this is a singular column
c       if cdeg = -1, then this column is deallocated
c
c    each row is stored in ii only:
c    ------------------------------
c
c       if i is a non-pivotal row, pr = rp (i)
c
c       rsiz = ii (pr) size of the integer data structure for row i,
c                        including the rscal scalars
c       rdeg = ii (pr+1) degree of row i
c       reln = wr (i) number of elements in row i element list
c       rlen = wc (i) number of original entries in row i
c       rep  = (pr+2) pointer to start of the element list
c       ii (rep ... rep + 2*reln - 1)
c                       element list (e,f) for the row
c       ii (rep + 2*reln ... pr + rsiz - rlen - 1)
c                       empty
c       ii (pr + rsiz - rlen ... pr + rsiz - 1)
c                       column indices of original nonzeros in the row
c
c       if rdeg = -1, then this row is deallocated
 
c-----------------------------------------------------------------------
c  frontal matrices
c-----------------------------------------------------------------------
c
c   each unassembled frontal matrix (element) is stored as follows:
c       total size: fscal integers, (fdimr*fdimc) reals
c
c       if e is an unassembled element, ep = rp (e), and e is also
c       the first pivot row in the frontal matrix.
c
c       fluip  = ii (ep)        pointer to lu arrowhead in ii
c       fdimc  = ii (ep+1)      column dimension of contribution block
c       fxp    = ii (ep+2)      pointer to contribution block in xx
c       next   = ii (ep+3)      pointer to next block in xx
c       prev   = ii (ep+4)      pointer to previous block in xx
c       fleftr = ii (ep+5)      number of unassembled rows
c       fleftc = ii (ep+6)      number of unassembled columns
c       fextr = wr (e) - w0     external row degree of the frontal mtx
c       fextc = wc (e) - w0     external col degree of the frontal mtx
c       xx (fxp ... )
c               a 2-dimensional array, c (1..fdimc, 1..fdimr).
c               note that fdimr is not kept (it is not needed,
c               except for the current frontal).  if this is not the
c               current frontal matrix, then luip points to the
c               corresponding lu arrowhead, and the contribution block
c               is stored in c (1..ludegc, 1..ludegr) in the
c               c (1..fdimc, ...) array.
c
c               if memory is limited, garbage collection will occur.
c               in this case, the c (1..fdimc, 1..fdimr) array is
c               compressed to be just large enough to hold the
c               unassembled contribution block,
c               c (1..ludegc, 1..ludegr).
 
c-----------------------------------------------------------------------
c  artificial frontal matrices
c-----------------------------------------------------------------------
c
c   an artificial frontal matrix is an original column that is treated
c   as a c-by-1 frontal matrix, where c is the number of original
c   nonzeros in the column.  dense columns (c > dsiz) are treated this
c   way.  an artificial frontal matrix is just the same as a frontal
c   matrix created by the elimination of one or more pivots, except
c   that there is no corresponding lu arrowhead.  the row and column
c   patterns are stored in:
c
c       ep = rp (e), where e = n+1 .. n+dn, where there are dn
c                    artificial frontal matrices.
c
c       lucp = (ep+9)   pointer to row pattern (just one column index)
c       lurp = (ep+8) pointer to column pattern (fdimc row indices)
 
c-----------------------------------------------------------------------
c  current frontal matrix
c-----------------------------------------------------------------------
c
c  ffxp points to current frontal matrix (contribution block and lu
c  factors).  for example, if fflefc = 4, fflefr = 6, k = 3, and
c  gro = 2.0, then "x" is a term in the contribution block, "l" in l1,
c  "u" in u1, "l" in l2, "u" in u2, and "." is unused.  xx (fxp) is "x".
c  the first 3 pivot values (diagonal entries in u1) are 1,2, and 3.
c  for this frontal matrix, ffdimr = 12 (the number of columns), and
c  ffdimc = 8 (the number of rows).  the frontal matrix is
c  ffdimc-by-ffdimr
c
c                             |----------- col 1 of l1 and l2, etc.
c                             v
c       x x x x x x . . . l l l
c       x x x x x x . . . l l l
c       x x x x x x . . . l l l
c       x x x x x x . . . l l l
c       . . . . . . . . . . . .
c       u u u u u u . . . 3 l l         <- row 3 of u1 and u2
c       u u u u u u . . . u 2 l         <- row 2 of u1 and u2
c       u u u u u u . . . u u 1         <- row 1 of u1 and u2
 
c-----------------------------------------------------------------------
c  lu factors
c-----------------------------------------------------------------------
c
c   the lu factors are placed at the tail of ii and xx.  if this routine
c   is factorizing a single block, then this decription is for the
c   factors of the single block:
c
c       ii (itail):             xtail = start of lu factors in xx
c       ii (itail+1):           nlu = number of lu arrowheads
c       ii (itail+2):           npiv = number of pivots
c       ii (itail+3):           maximum number of rows in any
c                               contribution block (max ludegc)
c       ii (itail+4):           maximum number of columns in any
c                               contribution block (max ludegr)
c       ii (itail+5..itail+nlu+4): lup (1..nlu) array, pointers to each
c                               lu arrowhead, in order of their
c                               factorization
c       ii (itail+nlu+5...isize):integer info. for lu factors
c       xx (xtail..xsize):      real values in lu factors
c
c   each lu arrowhead is stored as follows:
c   ---------------------------------------
c
c       total size: (7 + ludegc + ludegr + nsons) integers,
c                   (luk**2 + ludegc*luk + luk*ludegc) reals
c
c       if e is an lu arrowhead, then luip = rp (e), and luip >= itail.
c       when ums2f2 returns, then luip is given by luip =
c       ii (itail+s+1), where s = 1..nlu is the position of the lu
c       arrowhead in the lu factors (s=1,2,.. refers to the first,
c       second,.. lu arrowhead)
c
c       luxp   = ii (luip) pointer to numerical lu arrowhead
c       luk    = ii (luip+1) number of pivots in lu arrowhead
c       ludegr = ii (luip+2) degree of last row of u (excl. diag)
c       ludegc = ii (luip+3) degree of last col of l (excl. diag)
c       nsons  = ii (luip+4) number of children in assembly dag
c       ludimr = ii (luip+5)
c       ludimc = ii (luip+5) max front size is ludimr-by-ludimc,
c                       or zero if this lu arrowhead factorized within
c                       the frontal matrix of a prior lu arrowhead.
c       lucp   = (luip + 7)
c                       pointer to pattern of column of l
c       lurp   = lucp + ludegc
c                       pointer to patter of row of u
c       lusonp = lurp + ludegr
c                       pointer to list of sons in the assembly dag
c       ii (lucp ... lucp + ludegc - 1)
c                       row indices of column of l
c       ii (lurp ... lurp + ludegr - 1)
c                       column indices of row of u
c       ii (lusonp ... lusonp + nsons - 1)
c                       list of sons
c       xx (luxp...luxp + luk**2 + ludegc*luk + luk*ludegr - 1)
c                       pivot block (luk-by-luk) and the l block
c                       (ludegc-by-luk) in a single (luk+ludegc)-by-luk
c                       array, followed by the u block in a
c                       luk-by-ludegr array.
c
c   pivot column/row pattern (also columns/rows in contribution block):
c       if the column/row index is negated, the column/row has been
c       assembled out of the frontal matrix into a subsequent frontal
c       matrix.  after factorization, the negative flags are removed,
c       and the row/col indices are replaced with their corresponding
c       index in the permuted lu factors.
c
c   list of sons:
c       1 <= son <= n:           son an luson
c       n+1 <= son <= 2n:        son-n is an uson
c       2n+n <= son <= 3n:       son-2n is a lson
c       during factorzation, a son is referred to by its first
c       pivot column.  after factorization, they are numbered according
c       to their order in the lu factors.
 
c-----------------------------------------------------------------------
c  workspaces:
c-----------------------------------------------------------------------
c
c   wir (e):  link list of sons of the current element
c       wir (e) = -1 means that e is not in the list.
c       wir (e) = next+n+2 means that "next" is the element after e.
c       the end of the list is marked with wir (e) = -(n+2).
c       sonlst points to the first element in the list, or 0 if
c       the sonlst is empty.
c
c   wir (row), wic (col):  used for pivot row/col offsets:
c
c       if wir (row) >= 0 then the row is in the current
c       column pattern.  similarly for wic (col).
c
c       if wir (row) is set to "empty" (<= -1), then
c       the row is not in the current pivot column pattern.
c
c       similarly, if wic (col) is set to -2, then the column is
c       not in the current pivot row pattern.
c
c       if wic (col) = -1 then col is pivotal
c
c       after factorization, wir/c holds the pivot permutations.
c
c   wpr/c (1..n):  the first part is used for the current frontal
c           matrix pattern.  during factorization, the last part holds
c           a stack of the row and column permutations (wpr/c (n-k+1)
c           is the k-th pivot row/column).
c
c   head (1..n):        degree lists for columns.  head (d) is the
c                       first column in list d with degree d.
c                       the cnxt and cprv pointers are stored in the
c                       column data structure itself.
c                       mindeg is the least non-empty list
c
c   wm (1..n):          various uses
c   wj (1..degc) or wj (1..fdegc):      offset in pattern of a son
 
c-----------------------------------------------------------------------
c  memory allocation in ii and xx:
c-----------------------------------------------------------------------
c
c   ii (1..ihead):      rows and columns of active submatrix, and
c                       integer information for frontal matrices.
c   xx (1..xhead):      values of original entries in columns of
c                       matrix, values of contribution blocks, followed
c                       by the current frontal matrix.
c
c   mhead:              a pointer to the first block in the head of
c                       xx.  each block (a column or frontal matrix)
c                       contains a next and prev pointer for this list.
c                       if the list is traversed starting at mhead,
c                       then the pointers to the reals (cxp or fxp)
c                       will appear in strictly increasing order.
c                       note that the next, prev, and real pointers
c                       are in ii.  next and prev point to the next
c                       and previous block in ii, and the real pointer
c                       points to the real part in xx.
c
c   mtail:              the end of the memory list.
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer swpcol, swprow, fdimc, k0, colpos, rowpos, row2, rdeg2,
     $          p, i, j, ffrow, pivrow, pivcol, ludegr, ludegc, e1,
     $          fxp, lurp, lucp, ip, next, fflefr, pc, mnext, mprev,
     $          fflefc, fedegr, fedegc, k, xudp, xdp, xsp, xlp, s, col2,
     $          bestco, col, e, row, cost, srched, pr, f1, rscan, rep,
     $          kleft1, ffsize, ffxp, w0, ffdimr, ffdimc, kleft, xldp,
     $          ep, scan1, scan2, scan3, scan4, nzl, nzu, degc, cep
        integer mindeg, nsrch, npiv, eson, luip1, dnz, iworst, wxp,
     $          nb, lupp, nlu, nsons, ineed, xneed, ldimc, lxp, rlen2,
     $          rsiz, lsons, sonlst, xhead, ihead, deln, dlen,
     $          slist, xp, luip, rdeg, cdeg1, pfree, xfree, cdeg2,
     $          f, cdeg, mtail, mhead, rsiz2, csiz2, ip2, maxdr, maxdc,
     $          xs, is, luxp, fsp, flp, fdp, jj, usons, ndn, p2,
     $          csiz, celn, clen, reln, rlen, uxp, pc2, pr2
        integer cnxt, cprv, cxp, fluip, lusonp, fleftr, fleftc, maxint,
     $          fmaxr, fmaxc, slots, limit, rscal, cscal, fscal, extra,
     $          fmax, w0big, minmem, dummy1, dummy2, dummy3, dummy4
        logical symsrc, pfound, movelu, okcol, okrow, better
        real
     $          toler, maxval, relpt, gro, one, zero, x
        parameter (one = 1.0, zero = 0.0,
     $          rscal = 2, cscal = 9, fscal = 7,
     $          minmem = 24)
 
c  current element and working array, c:
c  -------------------------------------
c  ffxp:    current working array is in xx (ffxp ... ffxp+ffsize-1)
c  ffsize:  size of current working array in xx
c  ffdimr:  row degree (number of columns) of current working array
c  ffdimc:  column degree (number of rows) of current working array
c  fflefr:  row degree (number of columns) of current contribution block
c  fflefc:  column degree (number of rows) of current contribution block
c  fmaxr:   max row degree (maximum front size is fmaxr-by-fmaxc)
c  fmaxc:   max col degree (maximum front size is fmaxr-by-fmaxc)
c  fedegr:  extended row degree
c  fedegc:  extended column degree
c  ffrow:   current element being factorized (a pivot row index)
c  pivrow:  current pivot row index
c  pivcol:  current pivot column index
c  e1:      first pivot row in the frontal matrix
c  gro:     frontal matrix amalgamation growth factor
c  usons:   pointer to a link list of usons, in wc, assembled this scan3
c  lsons:   pointer to a link list of lsons, in wr, assembled this scan4
c  sonlst:  pointer to a link list of sons, in wir, of current element
c  swpcol:  the non-pivotal column to be swapped with pivot column
c  swprow:  the non-pivotal row to be swapped with pivot row
c  colpos:  position in wpr of the pivot column
c  rowpos:  position in wpc of the pivot row
c  k:       current pivot is kth pivot of current element
c  k0:      contribution block, c, has been updated with pivots 1..k0
c
c  lu arrowhead (a factorized element):
c  ------------------------------------
c  movelu:  true if a new lu arrowhead is to be created
c  luip:    current element is in ii (luip ...)
c  luip1:   first element from current frontal matrix in ii (luip1...)
c  ludegc:  degree of pivot column (excluding pivots themselves)
c  ludegr:  degree of pivot row (excluding pivots themselves)
c  lucp:    pattern of col(s) of current element in ii (lucp...)
c  lurp:    pattern of row(s) of current element in ii (lurp...)
c  lusonp:  list of sons of current element is in ii (lusonp...)
c  nsons:   number of sons of current element
c  ldimc:   column dimension (number of rows) of [l1\u1 l2] block
c  luxp:    numerical values of lu arrowhead stored in xx (luxp ...)
c  lxp:     l2 block is stored in xx (lxp ...) when computed
c  uxp:     u2 block is stored in xx (uxp ...) when computed
c  nzu:     nonzeros above diagonal in u in current lu arrowhead
c  nzl:     nonzeros below diagonal in l in current lu arrowhead
c
c  son, or element other than current element:
c  -------------------------------------------
c  e:       an element
c  eson:    an element
c  s:       a renumbered element (1..nlu) for ums2so and ums2rf
c  ep:      frontal matrix integer data struct. in ii (ep...ep+fscal-1)
c  fscal:   = 7, size of frontal matrix data structure
c  fluip:   lu arrowhead of e is in ii (fluip ...)
c  fxp:     contribution block of son is in xx (fxp ...)
c  fdimc:   leading dimension of contribution block of e
c  lucp:    pattern of col(s) of e in ii (lucp...)
c  lurp:    pattern of row(s) of e in ii (lurp...)
c  ludegr:  row degree of contribution block of e
c  ludegr:  column degree of contribution block of e
c  maxdr:   maximum ludegr for any lu arrowhead, for ums2rf
c  maxdc:   maximum ludegc for any lu arrowhead, for ums2rf
c  degc:    compressed column offset vector of son is in wj/wm (1..degc)
c  fleftr:  remaining row degree (number of columns) of a contrib. block
c  fleftc:  remaining column degree (number of rows) of a contrib. block
c  xudp:    pointer to a column of a prior contribution block
c  xldp:    pointer to a row of a prior contribution block
c
c  memory allocation:
c  ------------------
c  mhead:   head pointer for link list of blocks in xx
c  mtail:   tail pointer for link list of blocks in xx
c  mprev:   previous block, ii (p+4), of the block located at p
c  mnext:   next block, ii (p+3), of the block located at p
c  pfree:   ii (pfree+2) is the largest known free block in xx
c  xfree:   size of largest known free block in xx
c  xhead:   xx (1..xhead-1) is in use, xx (xhead ..xtail-1) is free
c  xtail:   xx (xtail..xsize) is in use, xx (xhead ..xtail-1) is free
c  xneed:   bare minimum memory currently needed in xx
c  ihead:   ii (1..ihead-1) is in use, ii (ihead ..itail-1) is free
c  itail:   ii (itail..isize) is in use, ii (ihead ..itail-1) is free
c  ineed:   bare minimum memory currently needed in ii
c  iworst:  worst possible current integer memory required
c  xs:      size of a block of memory in xx
c  is:      size of a block of memory in ii
c  wxp:     pointer to a temporary workspace in xx (wxp ... )
c  slots:   number of slots added to element lists during garbage coll.
c  minmem:  smallest isize allowed
c
c  wr and wc flag arrays:
c  ----------------------
c  w0:      marker value for wr (1..n) and wc (1..n) arrays
c  w0big:   largest permissible value of w0 (w0+n must not overflow)
c  fmax:    largest row/col degree of an element seen so far
c
c  a column:
c  ---------
c  pc:      pointer to a column, in ii (pc...)
c  pc2:     pointer to a column, in ii (pc2...)
c  csiz:    size of integer data structure of a column
c  csiz2:   size of integer data structure of a column
c  cscal:   = 9, number of scalars in data structure of a column
c  cdeg:    degree of a column
c  cdeg1:   degree of a column
c  cdeg2:   degree of a column
c  celn:    number of elements in the element list of a column
c  clen:    number of original entries that remain in a column
c  cnxt:    next column with same degree as this column
c  cprv:    previous column with same degree as this column
c  cep:     pointer to the element list of a column
c  cxp:     pointer to the numerical values in a column
c  limit:   maximum size for row/col data structure (excl. scalars)
c
c  dense columns:
c  --------------
c  dnz:     number of original entries that reside in "dense" columns
c  dn:      number of "dense" columns
c  ndn:     n + dn
c  extra:   number of extra slots to add to reconstructed "dense" cols
c
c  a row:
c  ------
c  pr:      pointer to a row, in ii (pr...)
c  pr2:     pointer to a row, in ii (pr2...)
c  rsiz:    size of integer data structure of a row
c  rsiz2:   size of integer data structure of a row
c  rscal:   = 2, number of scalars in data structure of a row
c  rdeg:    degree of a row
c  rdeg2:   degree of a row
c  reln:    number of elements in the element list of a row
c  rlen:    number of original entries that remain in a row
c  rlen2:   number of original entries that remain in a row
c  rep:     pointer to the element list of a row
c
c  pivot search:
c  -------------
c  cost:    approximate markowitz-cost of the current candidate pivot
c  bestco:  best approximate markowitz-cost seen so far
c  srched:  number of non-singular candidates searched so far
c  mindeg:  minimum degree of columns in active submatrix
c  nsrch:   maximum number of columns to search
c  slist:   pointer to a link list of searched columns, in ii
c  symsrc:  true if attempting to preserve symmetry
c  pfound:  true if pivot found during local search
c  okcol:   true if candidate pivot column is acceptable, so far
c  okrow:   true if candidate pivot row is acceptable, so far
c  toler:   pivot tolerance; abs(pivot) must be >= toler
c  maxval:  maximum absolute value in a candidate pivot column
c  relpt:   relative pivot tolerance (cntl (1))
c  npiv:    number of pivots factorized so far, incl. current element
c  kleft:   number of rows/columns remaining in active submatrix
c  kleft1:  kleft - 1
c  better:  if true, then candidate is better than the prior candidate
c
c  assembly:
c  ---------
c  f1:      degree prior to assembly next item
c  f:       offset into an element
c  rscan:   skip row assembly if more than rscan original entries
c  scan1:   start scan1 at wpc (scan1 ... fflefc)
c  scan2:   start scan2 at wpr (scan2 ... fflefr)
c  scan3:   start scan3 at wpr (scan3 ... fflefr)
c  scan4:   start scan4 at wpc (scan4 ... fflefc)
c  deln:    number of (e,f) tuples to delete from an element list
c  dlen:    number of original entries to delete from a row/col
c
c  allocated arrays:
c  -----------------
c  lupp:    lup (1..nlu) array located in ii (lupp...lupp+nlu-1)
c  nlu:     number of lu arrowheads
c
c  other:
c  ------
c  xdp:     destination pointer, into xx
c  xsp:     source pointer, into xx
c  xlp:     pointer into xx of location of last row/col in c
c  xp:      pointer into xx
c  ip:      pointer into ii
c  ip2:     pointer into ii
c  p2:      pointer into ii
c  fsp:     source pointer, into xx
c  fsp:     destination pointer, into xx
c  flp:     last row/column in current contribution is in xx (flp...)
c  col,col2: a column index
c  row,row2: a row index
c  nb:      block size for tradeoff between level-2 and level-3 blas
c  p, i, j, k, x:  various uses
c  jj:      loop index
c  maxint:  largest representable positive integer
c  next:    next pointer, for a link list
c  dummy1:  dummy loop index for main factorization loop
c  dummy2:  dummy loop index for global pivot search loop
c  dummy3:  dummy loop index for outer frontal matrix factorization loop
c  dummy4:  dummy loop index for inner frontal matrix factorization loop
 
c=======================================================================
c  executable statements:
c=======================================================================
 
c       ----------------------------------------------------------------
c       get control parameters and initialize various scalars
c       ----------------------------------------------------------------
 
        nsrch = max (1, icntl (5))
        symsrc = icntl (6) .ne. 0
        nb = max (1, icntl (7))
        relpt = max (zero, min (cntl (1), one))
        gro = max (one, cntl (2))
        maxint = keep (6)
        ndn = n + dn
        w0big = maxint - n
        w0 = ndn + 2
c       currently: w0 = n+dn+2 < 2n+2 < w0big = maxint - n
c       2n+2 < maxint - n must hold, so n < (maxint - 2) / 3 is the
c       largest that n can be.  this condition is checked in ums2fa.
        kleft = n
        npiv = 0
        nlu = 0
        mindeg = 1
        fmax = 1
        ihead = nz + cscal*n + 1
        xhead = nz + 1
        itail = isize + 1
        xtail = xsize + 1
c       cp (1) must equal 1, the first block
        xfree = -1
        pfree = 0
c       make sure integer space is at least of size minmem (simplifies
c       link list management and memory management)
        info (19) = max (info (19), iuse+minmem)
        if (ihead.gt.itail.or.isize.lt.minmem.or.xhead.gt.xtail) then
c          error return, if not enough integer and/or real memory:
           go to 9000
        endif
        bestco = 0
        limit = n + 2*ndn
        lsons = ndn + 1
        usons = ndn + 1
 
c       ----------------------------------------------------------------
c       initialize workspaces
c       ----------------------------------------------------------------
 
        do 10 i = 1, n
           wir (i) = -1
           wic (i) = -2
           head (i) = 0
           wc (i) = 0
           wr (i) = 0
10      continue
 
c       ----------------------------------------------------------------
c       initialize the link list for keeping track of real memory usage
c       ----------------------------------------------------------------
 
        mhead = 0
        mtail = 0
        do 20 col = 1, n
           pc = cp (col)
           clen = ii (pc+6)
           if (clen .gt. 0) then
c             place the column in the link list of blocks in xx
              if (mhead .eq. 0) then
                 mhead = pc
              endif
              ii (pc+4) = mtail
              ii (pc+3) = 0
              if (mtail .ne. 0) then
                 ii (mtail+3) = pc
              endif
              mtail = pc
           else
              ii (pc+2) = 0
              ii (pc+4) = 0
              ii (pc+3) = 0
           endif
20      continue
 
c       ----------------------------------------------------------------
c       convert dense columns to a-priori contribution blocks and
c       get the count of nonzeros in each row
c       ----------------------------------------------------------------
 
        e = n
        dnz = 0
        do 50 col = 1, n
           pc = cp (col)
           clen = ii (pc+6)
           cep = (pc+9)
           if (clen .gt. dsiz) then
c             this is a dense column - add to element list length
              dnz = dnz + clen
              do 30 ip = cep, cep + clen - 1
                 row = ii (ip)
                 wr (row) = wr (row) + 1
30            continue
c             convert dense column (in place) into a frontal matrix
              e = e + 1
              ep = pc
              rp (e) = ep
              fdimc = clen
              fleftc = clen
              fleftr = 1
              ii (ep+1) = fdimc
              ii (ep+5) = fleftr
              ii (ep+6) = fleftc
              wr (e) = w0-1
              wc (e) = w0-1
              lurp = (ep+8)
              ii (lurp) = col
              fmax = max (fmax, fleftc)
           else
c             this is a sparse column - add to orig entry length
              do 40 ip = cep, cep + clen - 1
                 row = ii (ip)
                 wc (row) = wc (row) + 1
40            continue
           endif
50      continue
 
c       ----------------------------------------------------------------
c       get memory for row-oriented form, and dense column element lists
c       ----------------------------------------------------------------
 
        pr = ihead
        csiz = cscal + 2
        is = (nz + rscal*n + dnz) + (dn * csiz)
        ihead = ihead + is
        iuse = iuse + is
        ineed = iuse
        xneed = xuse
        info (18) = max (info (18), iuse)
        info (19) = max (info (19), ineed)
        if (ihead .gt. itail) then
c          error return, if not enough integer memory:
           go to 9000
        endif
 
c       ----------------------------------------------------------------
c       if memory is available, add up to dsiz+6 extra slots in the
c       reconstructed dense columns to allow for element list growth
c       ----------------------------------------------------------------
 
        if (dn .gt. 0) then
           extra = min ((itail - ihead) / dn, dsiz + 6)
           csiz = csiz + extra
           is = dn * extra
           ihead = ihead + is
           iuse = iuse + is
           info (18) = max (info (18), iuse)
        endif
 
c       ----------------------------------------------------------------
c       construct row pointers
c       ----------------------------------------------------------------
 
        do 60 row = 1, n
           rp (row) = pr
           rep  = (pr+2)
           reln = wr (row)
           rlen = wc (row)
           rsiz = 2*reln + rlen + rscal
           ii (pr) = rsiz
           rdeg = reln + rlen
           ii (pr+1) = rdeg
           wm (row) = rep
           pr = pr + rsiz
60      continue
 
c       ----------------------------------------------------------------
c       construct row element lists for dense columns
c       ----------------------------------------------------------------
 
        pc = pr
        do 80 e = n+1, n+dn
           ep = rp (e)
           lucp = (ep+9)
           fdimc = ii (ep+1)
cfpp$ nodepchk l
           do 70 f = 0, fdimc - 1
              row = ii (lucp+f)
              ii (wm (row)    ) = e
              ii (wm (row) + 1) = f
              wm (row) = wm (row) + 2
70         continue
c          re-construct dense columns as just an element list,
c          containing a single element tuple (e,f), where f = 0
           lurp = (ep+8)
           col = ii (lurp)
           cp (col) = pc
           ii (pc) = csiz
           cdeg = fdimc
           ii (pc+1) = cdeg
           ii (pc+2) = 0
           ii (pc+4) = 0
           ii (pc+3) = 0
           ii (pc+5) = 1
           ii (pc+6) = 0
           ii (pc+7) = 0
           ii (pc+8) = 0
c          store the (e,0) tuple:
           cep = (pc+9)
           ii (cep  ) = e
           ii (cep+1) = 0
           pc = pc + csiz
80      continue
 
c       ----------------------------------------------------------------
c       construct the nonzero pattern of the row-oriented form
c       ----------------------------------------------------------------
 
        do 100 col = 1, n
           pc = cp (col)
           cep = (pc+9)
           clen = ii (pc+6)
cfpp$ nodepchk l
           do 90 p = cep, cep + clen - 1
              row = ii (p)
              ii (wm (row)) = col
              wm (row) = wm (row) + 1
90         continue
100     continue
 
c       count the numerical assembly of the original matrix
        rinfo (2) = rinfo (2) + nz
 
c       ----------------------------------------------------------------
c       initialize the degree lists
c       ----------------------------------------------------------------
 
c       do so in reverse order to try to improve pivot tie-breaking
        do 110 col = n, 1, -1
           pc = cp (col)
           cdeg = ii (pc+1)
           if (cdeg .le. 0) then
c             empty column - remove from pivot search
              cdeg = -(n+2)
              ii (pc+1) = cdeg
           else
              cnxt = head (cdeg)
              ii (pc+7) = cnxt
              ii (pc+8) = 0
              if (cnxt .ne. 0) then
                 ii (cp (cnxt)+8) = col
              endif
              head (cdeg) = col
           endif
110     continue
 
c=======================================================================
c=======================================================================
c  main factorization loop [
c=======================================================================
c=======================================================================
 
        do 1540 dummy1 = 1, n
c       (this loop is not indented due to its length)
 
c       ----------------------------------------------------------------
c       factorization is done if n pivots have been found
c       ----------------------------------------------------------------
 
        if (npiv .ge. n) then
           go to 2000
        endif
 
c=======================================================================
c  global pivot search, and initialization of a new frontal matrix [
c=======================================================================
 
        if (mtail .ne. 0 .and. ii (mtail+6) .eq. 0) then
c          tail block is free, delete it
           xp = ii (mtail+2)
           xuse = xuse - (xhead - xp)
           xhead = xp
           if (mtail .eq. pfree) then
              pfree = 0
              xfree = -1
           endif
           mtail = ii (mtail+4)
           if (mtail .ne. 0) then
              ii (mtail+3) = 0
           else
c             singular matrix.  no columns or contribution blocks left.
              mhead = 0
           endif
        endif
 
c=======================================================================
c  global pivot search:  find pivot row and column
c=======================================================================
 
        nsons = 0
        sonlst = 0
        srched = 0
        pivcol = 0
        slist = 0
 
        do 255 dummy2 = 1, n
 
c          -------------------------------------------------------------
c          get col from column upper-bound degree list
c          -------------------------------------------------------------
 
           col = 0
           do 140 cdeg = mindeg, n
              col = head (cdeg)
              if (col .ne. 0) then
c                exit out of loop if column found:
                 go to 150
              endif
140        continue
           if (col .eq. 0) then
c             exit out of loop if column not found (singular matrix):
              go to 260
           endif
c          loop exit label:
150        continue
           pc = cp (col)
           cnxt = ii (pc+7)
           if (cnxt .ne. 0) then
              ii (cp (cnxt)+8) = 0
           endif
           head (cdeg) = cnxt
           mindeg = cdeg
 
c          -------------------------------------------------------------
c          construct candidate column in wm and xx (wxp..wxp+cdeg-1)
c          -------------------------------------------------------------
 
           xs = cdeg
c          use wm (1..cdeg) for pattern [
c          use xx (wxp..wxp+xs-1) as workspace for values [
 
           if (xs .gt. xtail-xhead) then
 
              info (15) = info (15) + 1
              call ums2fg (xx, xsize, xhead, xtail, xuse,
     $                     ii, isize, ihead, itail, iuse,
     $                     cp, rp, dn, n, icntl, wir, wic, wr, wc,
     $                     0, 0, 0, 0, .false.,
     $                     pfree, xfree, mhead, mtail, slots)
c             at this point, iuse = ineed and xuse = xneed
              pc = cp (col)
           endif
 
           wxp = xhead
           xhead = xhead + xs
           xuse = xuse + xs
           xneed = xneed + xs
           info (20) = max (info (20), xuse)
           info (21) = max (info (21), xneed)
           if (xhead .gt. xtail) then
c             error return, if not enough real memory:
              go to 9000
           endif
 
c          -------------------------------------------------------------
c          assemble the elements in the element list
c          -------------------------------------------------------------
 
           cdeg = 0
           cep = (pc+9)
           celn = ii (pc+5)
           do 190 ip = cep, cep + 2*celn - 2, 2
              e = ii (ip)
              f = ii (ip+1)
              ep = rp (e)
              fdimc = ii (ep+1)
              fxp = ii (ep+2)
              if (e .le. n) then
                 fluip = ii (ep)
                 ludegc = ii (fluip+3)
                 lucp = (fluip + 7)
              else
                 ludegc = fdimc
                 lucp = (ep+9)
              endif
              xp = fxp + f * fdimc
c             split into 3 loops so that they all vectorize on a cray
              cdeg1 = cdeg
              do 160 p = lucp, lucp + ludegc - 1
                 row = ii (p)
                 if (row .gt. 0) then
                    if (wir (row) .le. 0) then
                       cdeg = cdeg + 1
                       wm (cdeg) = row
                    endif
                 endif
160           continue
              do 170 i = cdeg1+1, cdeg
                 row = wm (i)
                 wir (row) = i
                 xx (wxp+i-1) = zero
170           continue
cfpp$ nodepchk l
              do 180 j = 0, ludegc - 1
                 row = ii (lucp+j)
                 if (row .gt. 0) then
                    xx (wxp + wir (row) - 1) =
     $              xx (wxp + wir (row) - 1) + xx (xp+j)
                 endif
180           continue
190        continue
 
c          -------------------------------------------------------------
c          assemble the original entries in the column
c          -------------------------------------------------------------
 
           cdeg1 = cdeg
           clen = ii (pc+6)
           csiz = ii (pc)
           ip = pc + csiz - clen
           cxp = ii (pc+2)
cfpp$ nodepchk l
           do 200 i = 0, clen - 1
              row = ii (ip+i)
              wm (cdeg+1+i) = row
              xx (wxp+cdeg+i) = xx (cxp+i)
200        continue
           cdeg = cdeg + clen
 
c          -------------------------------------------------------------
c          update the degree of this column (exact, not upper bound)
c          -------------------------------------------------------------
 
           ii (pc+1) = cdeg
 
c          wm (1..cdeg) holds the pattern of col being searched.
c          xx (wxp..wxp+cdeg-1) holds the numerical values of col being
c          searched.  wir (wm (1..cdeg1)) is 1..cdeg1.
 
c          -------------------------------------------------------------
c          find the maximum absolute value in the column
c          -------------------------------------------------------------
 
           maxval = abs (xx (wxp - 1 + isamax (cdeg, xx (wxp), 1)))
           rinfo (3) = rinfo (3) + cdeg
           toler = relpt * maxval
           rdeg = n+1
 
c          -------------------------------------------------------------
c          look for the best possible pivot row in this column
c          -------------------------------------------------------------
 
           if (cdeg .ne. 0 .and. maxval .gt. zero) then
              if (symsrc) then
c                prefer symmetric pivots, if numerically acceptable
                 row = col
                 rowpos = wir (row)
                 if (rowpos .le. 0) then
c                   diagonal may be in original entries
                    do 210 i = cdeg1 + 1, cdeg1 + clen
                       if (wm (i) .eq. row) then
                          rowpos = i
c                         exit out of loop if symmetric pivot found:
                          go to 220
                       endif
210                 continue
c                   loop exit label:
220                 continue
                 endif
                 if (rowpos .gt. 0) then
c                   diagonal entry exists in the column pattern
                    x = abs (xx (wxp-1+rowpos))
                    if (x .ge. toler .and. x .gt. zero) then
c                      diagonal entry is numerically acceptable
                       pr = rp (row)
                       rdeg = ii (pr+1)
                    endif
                 endif
              endif
              if (rdeg .eq. n+1) then
c                continue searching - no diagonal found or sought for.
c                minimize row degree subject to abs(value) constraints.
                 row = n+1
                 do 230 i = 1, cdeg
                    row2 = wm (i)
                    pr = rp (row2)
                    rdeg2 = ii (pr+1)
c                   among those numerically acceptable rows of least
c                   (upper bound) degree, select the row with the
c                   lowest row index
                    better = rdeg2 .lt. rdeg .or.
     $                      (rdeg2 .eq. rdeg .and. row2 .lt. row)
                    if (better) then
                       x = abs (xx (wxp-1+i))
                       if (x.ge.toler .and. x.gt.zero) then
                          row = row2
                          rdeg = rdeg2
                          rowpos = i
                       endif
                    endif
230              continue
              endif
           endif
 
c          -------------------------------------------------------------
c          deallocate workspace
c          -------------------------------------------------------------
 
           xhead = xhead - xs
           xuse = xuse - xs
           xneed = xneed - xs
c          done using xx (wxp...wxp+xs-1) ]
 
c          -------------------------------------------------------------
c          reset work vector
c          -------------------------------------------------------------
 
           do 240 i = 1, cdeg1
              wir (wm (i)) = -1
240        continue
 
c          -------------------------------------------------------------
c          check to see if a pivot column was found
c          -------------------------------------------------------------
 
           if (rdeg .eq. n+1) then
 
c             ----------------------------------------------------------
c             no pivot found, column is zero
c             ----------------------------------------------------------
 
c             remove this singular column from any further pivot search
              cdeg = -(n+2)
              ii (pc+1) = cdeg
 
           else
 
c             ----------------------------------------------------------
c             save a list of the columns searched (with nonzero degrees)
c             ----------------------------------------------------------
 
              srched = srched + 1
              ii (pc+7) = slist
              slist = col
 
c             ----------------------------------------------------------
c             check if this is the best pivot seen so far
c             ----------------------------------------------------------
 
c             compute the true markowitz cost without scanning the row
c             wm (1..cdeg) holds pivot column, including pivot row index
c             wm (rowpos) contains the candidate pivot row index
              cost = (cdeg - 1) * (rdeg - 1)
              if (pivcol .eq. 0 .or. cost .lt. bestco) then
                 fflefc = cdeg
                 do 250 i = 1, fflefc-1
                    wpc (i) = wm (i)
250              continue
c                remove the pivot row index from pivot column pattern
                 wpc (rowpos) = wm (fflefc)
                 pivcol = col
                 pivrow = row
                 bestco = cost
              endif
           endif
 
c          done using wm (1..cdeg) for pattern ]
c          wpc (1..fflefc-1) holds pivot column (excl. pivot row index)
 
c          -------------------------------------------------------------
c          exit global pivot search if nsrch pivots have been searched
c          -------------------------------------------------------------
 
           if (srched .ge. nsrch) then
              go to 260
           endif
 
255     continue
c       exit label for loop 255:
260     continue
 
c=======================================================================
c  quit early if no pivot found (singular matrix detected)
c=======================================================================
 
        if (pivcol .eq. 0) then
c          complete the column permutation vector in
c          wpc (n-npiv+1 ... n) in reverse order
           k = n - npiv + 1
           do 270 col = 1, n
              if (cp (col) .ne. 0) then
c                this is a non-pivotal column
                 k = k - 1
                 wpc (k) = col
                 cp (col) = 0
              endif
270        continue
c          complete the row permutation vector in
c          wpr (n-npiv+1 ... n) in reverse order
           k = n - npiv + 1
           do 280 row = 1, ndn
              if (row .gt. n) then
c                this is an artificial frontal matrix
                 e = row
                 rp (e) = 0
              else if (rp (row) .ne. 0) then
                 rlen = wc (row)
                 if (rlen .ge. 0 .and. rlen .le. n) then
c                   this is a non-pivotal row
                    k = k - 1
                    wpr (k) = row
                    rp (row) = 0
                 else if (rlen .ne. -(ndn+2)) then
c                   this is an unassembled element: convert to lu
                    e = row
                    ep = rp (row)
                    wr (e) = -(ndn+2)
                    wc (e) = -(ndn+2)
                    fluip = ii (ep)
                    rp (e) = fluip
                 endif
              endif
280        continue
c          factorization is done, exit the main factorization loop:
           go to 2000
        endif
 
c=======================================================================
c  place the non-pivotal columns searched back in degree lists
c=======================================================================
 
        do 300 i = 1, srched
           col = slist
           pc = cp (col)
           slist = ii (pc+7)
           if (col .ne. pivcol) then
              cdeg = ii (pc+1)
              cnxt = head (cdeg)
              ii (pc+7) = cnxt
              ii (pc+8) = 0
              if (cnxt .ne. 0) then
                 ii (cp (cnxt)+8) = col
              endif
              head (cdeg) = col
              mindeg = min (mindeg, cdeg)
           endif
300     continue
 
c=======================================================================
c  construct pivot row pattern
c=======================================================================
 
c       at this point, wir (1..n) = -1 and wic (1..n) is -2 for
c       nonpivotal columns and -1 for pivotal columns.
c       wic (wpr (1..fflefr+1)) is set to zero in the code below.  it
c       will be set to the proper offsets in do 775, once ffdimc is
c       known (offsets are dependent on ffdimc, which is dependent on
c       fflefr calculated below, and the memory allocation).
 
c       ----------------------------------------------------------------
c       assemble the elements in the element list
c       ----------------------------------------------------------------
 
        pr = rp (pivrow)
        fflefr = 0
        rep = (pr+2)
        reln = wr (pivrow)
        do 330 ip = rep, rep + 2*reln - 2, 2
           e = ii (ip)
           ep = rp (e)
           if (e .le. n) then
              fluip = ii (ep)
              lucp = (fluip + 7)
              ludegr = ii (fluip+2)
              ludegc = ii (fluip+3)
              lurp = lucp + ludegc
c             split into two loops so that they both vectorize on a cray
              f1 = fflefr
              do 310 p = lurp, lurp + ludegr - 1
                 col = ii (p)
                 if (col .gt. 0) then
                    if (wic (col) .eq. -2) then
                       fflefr = fflefr + 1
                       wpr (fflefr) = col
                    endif
                 endif
310           continue
              do 320 i = f1+1, fflefr
                 wic (wpr (i)) = 0
320           continue
           else
c             this is an artifical element (a dense column)
              lurp = (ep+8)
              col = ii (lurp)
              if (wic (col) .eq. -2) then
                 fflefr = fflefr + 1
                 wpr (fflefr) = col
                 wic (col) = 0
              endif
           endif
330     continue
 
c       ----------------------------------------------------------------
c       assemble the original entries in the pivot row
c       ----------------------------------------------------------------
 
        rsiz = ii (pr)
        rlen = wc (pivrow)
        do 340 p = pr + rsiz - rlen, pr + rsiz - 1
           col = ii (p)
           if (wic (col) .eq. -2) then
              fflefr = fflefr + 1
              wpr (fflefr) = col
           endif
340     continue
c       the exact degree of the pivot row is fflefr
 
c=======================================================================
c  initialize the new frontal matrix
c=======================================================================
 
c       ffrow is the name of current frontal matrix
        ffrow = pivrow
        e1 = pivrow
        k = 1
        k0 = 0
        ffdimr = min (kleft, int (gro * fflefr))
        ffdimc = min (kleft, int (gro * fflefc))
        fmaxr = fflefr
        fmaxc = fflefc
        ffsize = ffdimc * ffdimr
        rscan = max (dsiz, ffdimr)
 
c       ----------------------------------------------------------------
c       compute the offsets for rows in the pivot column
c       and the offsets for columns in the pivot row
c       ----------------------------------------------------------------
 
        do 350 i = 1, fflefc - 1
           wir (wpc (i)) = i - 1
350     continue
        do 360 i = 1, fflefr
           wic (wpr (i)) = (i - 1) * ffdimc
360     continue
 
c       ----------------------------------------------------------------
c       remove the pivot column index from the pivot row pattern
c       ----------------------------------------------------------------
 
        col = wpr (fflefr)
        colpos = (wic (pivcol)/ffdimc)+1
        wpr (colpos) = col
        wic (col) = wic (pivcol)
        wic (pivcol) = (ffdimr - 1) * ffdimc
        wir (pivrow) = ffdimc - 1
 
c       ----------------------------------------------------------------
c       remove the pivot row/col from the nonzero count
c       ----------------------------------------------------------------
 
        fflefr = fflefr - 1
        fflefc = fflefc - 1
 
c       ----------------------------------------------------------------
c       allocate the working array, doing garbage collection if needed
c       also allocate space for a work vector of size ffdimc
c       ----------------------------------------------------------------
 
        if (ffsize + ffdimc .gt. xtail-xhead) then
           info (15) = info (15) + 1
           call ums2fg (xx, xsize, xhead, xtail, xuse,
     $                  ii, isize, ihead, itail, iuse,
     $                  cp, rp, dn, n, icntl, wir, wic, wr, wc,
     $                  0, 0, 0, 0, .false.,
     $                  pfree, xfree, mhead, mtail, slots)
c          at this point, iuse = ineed and xuse = xneed
        endif
 
        ffxp = xhead
        xhead = xhead + ffsize
        wxp = xhead
        xhead = xhead + ffdimc
        xuse = xuse + ffsize + ffdimc
        xneed = xneed + ffsize + ffdimc
        info (20) = max (info (20), xuse)
        info (21) = max (info (21), xneed)
        if (xhead .gt. xtail) then
c          error return, if not enough real memory:
           go to 9000
        endif
 
c       ----------------------------------------------------------------
c       get memory usage for next call to ums2rf
c       ----------------------------------------------------------------
 
        xruse = xruse + ffsize
        xrmax = max (xrmax, xruse)
 
c       ----------------------------------------------------------------
c       zero the working array
c       ----------------------------------------------------------------
 
c       zero the contribution block:
        do 380 j = 0, fflefr - 1
           do 370 i = 0, fflefc - 1
              xx (ffxp + j*ffdimc + i) = zero
370        continue
380     continue
 
c       zero the pivot row:
        do 390 j = 0, fflefr - 1
           xx (ffxp + j*ffdimc + ffdimc-1) = zero
390     continue
 
c       zero the pivot column:
        do 400 i = 0, fflefc - 1
           xx (ffxp + (ffdimr-1)*ffdimc + i) = zero
400     continue
 
c       zero the pivot entry itself:
        xx (ffxp + (ffdimr-1)*ffdimc + ffdimc-1) = zero
 
c       ----------------------------------------------------------------
c       current workspace usage:
c       ----------------------------------------------------------------
 
c       wpc (1..fflefc):        holds the pivot column pattern
c                               (excluding the pivot row index)
c       wpc (fflefc+1 .. n-npiv):       unused
c       wpc (n-npiv+1 .. n):            pivot columns in reverse order
c
c       wpr (1..fflefr):        holds the pivot row pattern
c                               (excluding the pivot column index)
c       wpr (fflefr+1 .. n-npiv):       unused
c       wpr (n-npiv+1 .. n):            pivot rows in reverse order
c
c       c (1..ffdimr, 1..ffdimc):  space for the new frontal matrix.
c
c       c (i,j) is located at xx (ffxp+((i)-1)+((j)-1)*ffdimc)
c
c       wir (row) >= 0 for each row in pivot column pattern.
c               offset into pattern is given by:
c               wir (row) == offset - 1
c               also, wir (pivrow) is ffdimc-1, the offset in c of
c               the pivot row itself.
c               otherwise, wir (1..n) is -1
c
c       wic (col) >= 0 for each col in pivot row pattern.
c               wic (col) == (offset - 1) * ffdimc
c               also, wic (pivcol) is (ffdimr-1)*ffdimc,
c               the offset in c of the pivot column itself.
c               otherwise, wic (1..n) is -2 for nonpivotal columns,
c               and -1 for pivotal columns
 
c       ----------------------------------------------------------------
c       remove the columns affected by this element from degree lists
c       ----------------------------------------------------------------
 
        do 410 j = 1, fflefr
           pc = cp (wpr (j))
           cdeg = ii (pc+1)
           if (cdeg .gt. 0) then
              cnxt = ii (pc+7)
              cprv = ii (pc+8)
              if (cnxt .ne. 0) then
                 ii (cp (cnxt)+8) = cprv
              endif
              if (cprv .ne. 0) then
                 ii (cp (cprv)+7) = cnxt
              else
                 head (cdeg) = cnxt
              endif
           endif
410     continue
 
c=======================================================================
c  initialization of new frontal matrix is complete ]
c=======================================================================
 
c=======================================================================
c  assemble and factorize the current frontal matrix [
c=======================================================================
 
c       for first pivot in frontal matrix, do all scans
        scan1 = 0
        scan2 = 0
        scan3 = 0
        scan4 = 0
 
        do 1395 dummy3 = 1, n
c       (this loop is not indented due to its length)
 
c=======================================================================
c  degree update and numerical assembly [
c=======================================================================
 
        kleft1 = kleft - 1
 
c       ----------------------------------------------------------------
c       scan1:  scan the element lists of each row in the pivot col
c               and compute the external column degree for each frontal
c       ----------------------------------------------------------------
 
        row = pivrow
        do 440 j = scan1, fflefc
           if (j .ne. 0) then
c             get a row;  otherwise, scan the pivot row if j is zero.
              row = wpc (j)
           endif
           pr = rp (row)
           rep = (pr+2)
           reln = wr (row)
cfpp$ nodepchk l
           do 430 p = rep, rep + 2*reln - 2, 2
              e = ii (p)
              if (wc (e) .lt. w0) then
c                this is the first time seen in either scan 1 or 2:
                 ep = rp (e)
                 fleftr = ii (ep+5)
                 fleftc = ii (ep+6)
                 wr (e) = fleftr + w0
                 wc (e) = fleftc + w0
              endif
              wc (e) = wc (e) - 1
430        continue
440     continue
 
c       ----------------------------------------------------------------
c       scan2:  scan the element lists of each col in the pivot row
c               and compute the external row degree for each frontal
c       ----------------------------------------------------------------
 
        col = pivcol
        do 460 j = scan2, fflefr
           if (j .ne. 0) then
c             get a col;  otherwise, scan the pivot col if j is zero.
              col = wpr (j)
           endif
           pc = cp (col)
           celn = ii (pc+5)
           cep = (pc+9)
cfpp$ nodepchk l
           do 450 p = cep, cep + 2*celn - 2, 2
              e = ii (p)
              if (wr (e) .lt. w0) then
c                this is the first time seen in either scan 1 or 2:
                 ep = rp (e)
                 fleftr = ii (ep+5)
                 fleftc = ii (ep+6)
                 wr (e) = fleftr + w0
                 wc (e) = fleftc + w0
              endif
              wr (e) = wr (e) - 1
450        continue
460     continue
 
c       ----------------------------------------------------------------
c       scan3:  scan the element lists of each column in pivot row
c               do degree update for the columns
c               assemble effective usons and lu-sons
c       ----------------------------------------------------------------
 
c       flag usons in wc (e) as scanned (all now unflagged) [
c       uses wc (e) for the link list.  wc (e) <= 0
c       means that e is in the list, the external column
c       degree is zero, and -(wc (e)) is the next element in
c       the uson list.
 
        col = pivcol
        do 700 jj = scan3, fflefr
 
c          -------------------------------------------------------------
c          assemble and update the degree of a column
c          -------------------------------------------------------------
 
           if (jj .ne. 0) then
c             get a col;  otherwise, scan the pivot col if jj is zero
              col = wpr (jj)
           endif
 
c          -------------------------------------------------------------
c          compute the degree, and partition the element list into
c          two parts.  the first part are not lusons or usons, and
c          are not assembled.  the second part is assembled.
c          -------------------------------------------------------------
 
           cdeg = 0
           deln = 0
           pc = cp (col)
           cep = (pc+9)
           celn = ii (pc+5)
           ip2 = cep + 2*celn - 2
           xudp = ffxp + wic (col)
cfpp$ nodepchk l
           do 470 ip = cep, ip2, 2
              e = ii (ip)
              if (wc (e) .gt. w0) then
c                this element cannot be assembled
                    cdeg = cdeg + (wc (e) - w0)
              else
c                delete this tuple from the element list
                 deln = deln + 1
                 wm (deln) = ip
              endif
470       continue
 
          if (deln .ne. 0) then
 
c             ----------------------------------------------------------
c             move the deleted tuples to the end of the element list
c             ----------------------------------------------------------
 
              p2 = ip2
              do 480 i = deln, 1, -1
                 e = ii (wm (i)  )
                 f = ii (wm (i)+1)
                 ii (wm (i)  ) = ii (p2  )
                 ii (wm (i)+1) = ii (p2+1)
                 ii (p2  ) = e
                 ii (p2+1) = f
                 p2 = p2 - 2
480           continue
 
c             ----------------------------------------------------------
c             assemble from lusons and usons (the deleted tuples)
c             ----------------------------------------------------------
 
              do 670 ip = p2 + 2, ip2, 2
 
c                -------------------------------------------------------
c                this is an luson or uson.  if fextc < 0 then this has
c                already been assembled.
c                -------------------------------------------------------
 
                 e = ii (ip)
                 if (wc (e) .lt. w0) then
c                   go to next iteration if already assembled
                    goto 670
                 endif
 
c                -------------------------------------------------------
c                get scalar info, add son to list if not already there
c                -------------------------------------------------------
 
                 ep = rp (e)
                 fdimc = ii (ep+1)
                 fxp = ii (ep+2)
                 fleftr = ii (ep+5)
                 fleftc = ii (ep+6)
                 if (e .le. n) then
                    fluip = ii (ep)
                    ludegr = ii (fluip+2)
                    ludegc = ii (fluip+3)
                    lucp = (fluip + 7)
                    lurp = lucp + ludegc
                    if (wir (e) .eq. -1) then
                       wir (e) = sonlst - n - 2
                       sonlst = e
                       nsons = nsons + 1
                    endif
                 else
c                   an artificial frontal matrix
                    ludegr = 1
                    ludegc = fdimc
                    lucp = (ep+9)
                    lurp = (ep+8)
                 endif
 
c                -------------------------------------------------------
                 if (wr (e) .eq. w0) then
c                this is an luson - assemble an entire frontal matrix
c                -------------------------------------------------------
 
c                   ----------------------------------------------------
                    if (ludegc .eq. fleftc) then
c                   no rows assembled out of this frontal yet
c                   ----------------------------------------------------
 
c                      compute the compressed column offset vector
c                      use wm (1..ludegc for offsets) [
                       do 490 i = 0, ludegc-1
                          row2 = ii (lucp+i)
                          wm (i+1) = wir (row2)
490                    continue
 
c                      -------------------------------------------------
                       if (ludegr .eq. fleftr) then
c                      no rows or cols assembled out of frontal yet
c                      -------------------------------------------------
 
                          do 510 j = 0, ludegr-1
                             col2 = ii (lurp+j)
                             xdp = ffxp + wic (col2)
cfpp$ nodepchk l
                             do 500 i = 0, ludegc-1
                                xx (xdp + wm (i+1)) =
     $                          xx (xdp + wm (i+1)) +
     $                          xx (fxp + j*fdimc + i)
500                          continue
510                       continue
 
c                      -------------------------------------------------
                       else
c                      only cols have been assembled out of frontal
c                      -------------------------------------------------
 
                          do 530 j = 0, ludegr-1
                             col2 = ii (lurp+j)
                             if (col2 .gt. 0) then
                                xdp = ffxp + wic (col2)
cfpp$ nodepchk l
                                do 520 i = 0, ludegc-1
                                   xx (xdp + wm (i+1)) =
     $                             xx (xdp + wm (i+1)) +
     $                             xx (fxp + j*fdimc + i)
520                             continue
                             endif
530                       continue
                       endif
c                      done using wm (1..ludegc for offsets) ]
 
c                   ----------------------------------------------------
                    else
c                   some rows have been assembled out of this frontal
c                   ----------------------------------------------------
 
c                      compute the compressed column offset vector
c                      use wm (1..ludegc for offsets) [
                       degc = 0
                       do 540 i = 0, ludegc-1
                          row2 = ii (lucp+i)
                          if (row2 .gt. 0) then
                             degc = degc + 1
                             wj (degc) = i
                             wm (degc) = wir (row2)
                          endif
540                    continue
 
c                      -------------------------------------------------
                       if (ludegr .eq. fleftr) then
c                      only rows assembled out of this frontal
c                      -------------------------------------------------
 
                          do 560 j = 0, ludegr-1
                             col2 = ii (lurp+j)
                             xdp = ffxp + wic (col2)
cfpp$ nodepchk l
                             do 550 i = 1, degc
                                xx (xdp + wm (i)) =
     $                          xx (xdp + wm (i)) +
     $                          xx (fxp + j*fdimc + wj (i))
550                          continue
560                       continue
 
c                      -------------------------------------------------
                       else
c                      both rows and columns assembled out of frontal
c                      -------------------------------------------------
 
                          do 580 j = 0, ludegr-1
                             col2 = ii (lurp+j)
                             if (col2 .gt. 0) then
                                xdp = ffxp + wic (col2)
cfpp$ nodepchk l
                                do 570 i = 1, degc
                                   xx (xdp + wm (i)) =
     $                             xx (xdp + wm (i)) +
     $                             xx (fxp + j*fdimc + wj (i))
570                             continue
                             endif
580                       continue
                       endif
c                      done using wm (1..ludegc for offsets) ]
                    endif
 
c                   ----------------------------------------------------
c                   deallocate the luson frontal matrix
c                   ----------------------------------------------------
 
                    wr (e) = -(ndn+2)
                    wc (e) = -(ndn+2)
                    if (e .le. n) then
                       rp (e) = fluip
                       ii (ep) = fscal
                       ineed = ineed - fscal
                    else
                       rp (e) = 0
                       ii (ep) = fdimc + cscal
                       ineed = ineed - (fdimc + cscal)
                    endif
                    ii (ep+1) = -1
                    ii (ep+6) = 0
                    mprev = ii (ep+4)
                    mnext = ii (ep+3)
                    xneed = xneed - ludegr*ludegc
                    if (mnext .ne. 0 .and. ii (mnext+6) .eq. 0) then
c                      next block is free - delete it
                       mnext = ii (mnext+3)
                       ii (ep+3) = mnext
                       if (mnext .ne. 0) then
                          ii (mnext+4) = ep
                       else
                          mtail = ep
                       endif
                    endif
                    if (mprev .ne. 0 .and. ii (mprev+6) .eq. 0) then
c                      previous block is free - delete it
                       ii (ep+2) = ii (mprev+2)
                       mprev = ii (mprev+4)
                       ii (ep+4) = mprev
                       if (mprev .ne. 0) then
                          ii (mprev+3) = ep
                       else
                          mhead = ep
                       endif
                    endif
c                   get the size of the freed block
                    if (mnext .ne. 0) then
                       xs = ii (mnext+2) - ii (ep+2)
                    else
                       xs = ffxp - ii (ep+2)
                    endif
                    if (xs .gt. xfree) then
c                      keep track of the largest free block
                       xfree = xs
                       pfree = ep
                    endif
 
c                   ----------------------------------------------------
c                   get memory usage for next call to ums2rf
c                   ----------------------------------------------------
 
                    xruse = xruse - ludegr*ludegc
 
c                -------------------------------------------------------
                 else if (wr (e) - w0 .le. fleftr/2) then
c                this is a uson - assemble all possible columns
c                -------------------------------------------------------
 
c                   ----------------------------------------------------
c                   add to uson list - to be cleared just after scan 3
c                   ----------------------------------------------------
 
                    wc (e) = -usons
                    usons = e
 
c                   ----------------------------------------------------
                    if (ludegc .eq. fleftc) then
c                   no rows assembled out of this uson frontal yet
c                   ----------------------------------------------------
 
c                      compute the compressed column offset vector
c                      use wm (1..ludegc for offsets)
                       do 590 i = 0, ludegc-1
                          row2 = ii (lucp+i)
                          wm (i+1) = wir (row2)
590                    continue
 
                       do 610 j = 0, ludegr-1
                          col2 = ii (lurp+j)
                          if (col2 .gt. 0) then
                             if (wic (col2) .ge. 0) then
                                xdp = ffxp + wic (col2)
cfpp$ nodepchk l
                                do 600 i = 0, ludegc-1
                                   xx (xdp + wm (i+1)) =
     $                             xx (xdp + wm (i+1)) +
     $                             xx (fxp + j*fdimc + i)
600                             continue
c                               flag this column as assembled from uson
                                ii (lurp+j) = -col2
                             endif
                          endif
610                    continue
 
c                   ----------------------------------------------------
                    else
c                   some rows already assembled out of this uson frontal
c                   ----------------------------------------------------
 
c                      compute the compressed column offset vector
c                      use wm (1..ludegc for offsets)
                       degc = 0
                       do 620 i = 0, ludegc-1
                          row2 = ii (lucp+i)
                          if (row2 .gt. 0) then
                             degc = degc + 1
                             wj (degc) = i
                             wm (degc) = wir (row2)
                          endif
620                    continue
 
                       do 640 j = 0, ludegr-1
                          col2 = ii (lurp+j)
                          if (col2 .gt. 0) then
                             if (wic (col2) .ge. 0) then
                                xdp = ffxp + wic (col2)
cfpp$ nodepchk l
                                do 630 i = 1, degc
                                   xx (xdp + wm (i)) =
     $                             xx (xdp + wm (i)) +
     $                             xx (fxp + j*fdimc + wj (i))
630                             continue
c                               flag this column as assembled from uson
                                ii (lurp+j) = -col2
                             endif
                          endif
640                    continue
 
                    endif
 
                    fleftr = wr (e) - w0
                    ii (ep+5) = fleftr
 
c                -------------------------------------------------------
                 else
c                this is a uson - assemble just one column
c                -------------------------------------------------------
 
c                   get the offset, f, from the (e,f) tuple
                    f = ii (ip+1)
 
c                   ----------------------------------------------------
                    if (ludegc .eq. fleftc) then
c                   no rows assembled out of this uson yet
c                   ----------------------------------------------------
 
cfpp$ nodepchk l
                       do 650 i = 0, ludegc-1
                          row2 = ii (lucp+i)
                          xx (xudp + wir (row2)) =
     $                    xx (xudp + wir (row2)) +
     $                    xx (fxp + f*fdimc + i)
650                    continue
 
c                   ----------------------------------------------------
                    else
c                   some rows already assembled out of this uson
c                   ----------------------------------------------------
 
cfpp$ nodepchk l
                       do 660 i = 0, ludegc-1
                          row2 = ii (lucp+i)
                          if (row2 .gt. 0) then
                             xx (xudp + wir (row2)) =
     $                       xx (xudp + wir (row2)) +
     $                       xx (fxp + f*fdimc + i)
                          endif
660                    continue
                    endif
 
c                   ----------------------------------------------------
c                   decrement count of unassembled cols in frontal
c                   ----------------------------------------------------
 
                    ii (ep+5) = fleftr - 1
c                   flag the column as assembled from the uson
                    ii (lurp+f) = -col
                 endif
 
670           continue
 
c             ----------------------------------------------------------
c             update the count of (e,f) tuples in the element list
c             ----------------------------------------------------------
 
              ii (pc+5) = ii (pc+5) - deln
              ineed = ineed - 2*deln
           endif
 
c          -------------------------------------------------------------
c          assemble the original column and update count of entries
c          -------------------------------------------------------------
 
           clen = ii (pc+6)
           if (clen .gt. 0) then
              csiz = ii (pc)
              ip = pc + csiz - clen
              dlen = 0
cfpp$ nodepchk l
              do 680 i = 0, clen - 1
                 row = ii (ip+i)
                 if (wir (row) .ge. 0) then
c                   this entry can be assembled and deleted
                    dlen = dlen + 1
                    wm (dlen) = i
                 endif
680           continue
              if (dlen .ne. 0) then
                 cxp = ii (pc+2)
                 do 690 j = 1, dlen
                    i = wm (j)
                    row = ii (ip+i)
c                   assemble the entry
                    xx (xudp + wir (row)) =
     $              xx (xudp + wir (row)) + xx (cxp+i)
c                   and delete the entry
                    ii (ip +i) = ii (ip +j-1)
                    xx (cxp+i) = xx (cxp+j-1)
690              continue
                 clen = clen - dlen
                 cxp = cxp + dlen
                 ineed = ineed - dlen
                 xneed = xneed - dlen
                 ii (pc+6) = clen
                 if (clen .ne. 0) then
                    ii (pc+2) = cxp
                 else
c                   deallocate the real portion of the column:
                    mprev = ii (pc+4)
                    mnext = ii (pc+3)
                    if (mnext .ne. 0 .and. ii (mnext+6) .eq. 0) then
c                      next block is free - delete it
                       mnext = ii (mnext+3)
                       ii (pc+3) = mnext
                       if (mnext .ne. 0) then
                          ii (mnext+4) = pc
                       else
                          mtail = pc
                       endif
                    endif
                    if (mprev .ne. 0 .and. ii (mprev+6) .eq. 0) then
c                      previous block is free - delete it
                       ii (pc+2) = ii (mprev+2)
                       mprev = ii (mprev+4)
                       ii (pc+4) = mprev
                       if (mprev .ne. 0) then
                          ii (mprev+3) = pc
                       else
                          mhead = pc
                       endif
                    endif
                    if (pc .eq. mhead) then
c                      adjust the start of the block if this is head
                       ii (pc+2) = 1
                    endif
c                   get the size of the freed block
                    if (mnext .ne. 0) then
                       xs = ii (mnext+2) - ii (pc+2)
                    else
                       xs = ffxp - ii (pc+2)
                    endif
                    if (xs .gt. xfree) then
c                      keep track of the largest free block
                       xfree = xs
                       pfree = pc
                    endif
                 endif
              endif
              cdeg = cdeg + clen
           endif
 
c          -------------------------------------------------------------
c          compute the upper bound degree - excluding current front
c          -------------------------------------------------------------
 
           cdeg2 = ii (pc+1)
           cdeg = min (kleft1 - fflefc, cdeg2, cdeg)
           ii (pc+1) = cdeg
 
700     continue
 
c       ----------------------------------------------------------------
c       scan-3 wrap-up:  remove flags from assembled usons
c       ----------------------------------------------------------------
 
c       while (usons .ne. ndn+1) do
710     continue
        if (usons .ne. ndn+1) then
           next = -wc (usons)
           wc (usons) = w0
           usons = next
c       end while:
        goto 710
        endif
c       done un-flagging usons, all now unflagged in wc (e) ]
 
c       ----------------------------------------------------------------
c       scan4:  scan element lists of each row in the pivot column
c               do degree update for the rows
c               assemble effective lsons
c       ----------------------------------------------------------------
 
c       flag lsons in wr (e) (all are now unflagged) [
c       uses wr (e) for the link list.  wr (e) <= 0 means
c       that e is in the list, the external row degree is zero, and
c       -(wr (e)) is the next element in the lson list.
 
        row = pivrow
        do 840 jj = scan4, fflefc
 
c          -------------------------------------------------------------
c          assemble and update the degree of a row
c          -------------------------------------------------------------
 
           if (jj .ne. 0) then
c             get a row;  otherwise, scan the pivot row if jj is zero
              row = wpc (jj)
           endif
 
c          -------------------------------------------------------------
c          compute the degree, and partition the element list into
c          two parts.  the first part are not lusons or lsons, and
c          are not assembled.  the second part is assembled.
c          -------------------------------------------------------------
 
           rdeg = 0
           deln = 0
           pr = rp (row)
           rep = (pr+2)
           reln = wr (row)
           ip2 = rep + 2*reln - 2
cfpp$ nodepchk l
           do 720 ip = rep, ip2, 2
              e = ii (ip)
              if (wr (e) .gt. w0) then
                 rdeg = rdeg + (wr (e) - w0)
              else
                 deln = deln + 1
                 wm (deln) = ip
              endif
720        continue
 
           if (deln .ne. 0) then
 
c             ----------------------------------------------------------
c             move the deleted tuples to the end of the element list
c             ----------------------------------------------------------
 
              p2 = ip2
              do 730 i = deln, 1, -1
                 e = ii (wm (i)  )
                 f = ii (wm (i)+1)
                 ii (wm (i)  ) = ii (p2  )
                 ii (wm (i)+1) = ii (p2+1)
                 ii (p2  ) = e
                 ii (p2+1) = f
                 p2 = p2 - 2
730           continue
 
c             ----------------------------------------------------------
c             assemble from lsons (the deleted tuples)
c             ----------------------------------------------------------
 
              do 810 ip = p2 + 2, ip2, 2
 
c                -------------------------------------------------------
c                this is an luson or lson.  if fextr < 0 then this has
c                already been assembled.  all lusons have already been
c                assembled (in scan3, above).
c                -------------------------------------------------------
 
                 e = ii (ip)
                 if (wr (e) .lt. w0) then
c                   go to next iteration if already assembled
                    goto 810
                 endif
 
c                -------------------------------------------------------
c                get scalar info, add to son list if not already there
c                -------------------------------------------------------
 
                 ep = rp (e)
                 fdimc = ii (ep+1)
                 fxp = ii (ep+2)
                 fleftr = ii (ep+5)
                 fleftc = ii (ep+6)
                 if (e .le. n) then
                    fluip = ii (ep)
                    ludegr = ii (fluip+2)
                    ludegc = ii (fluip+3)
                    lucp = (fluip + 7)
                    lurp = lucp + ludegc
                    if (wir (e) .eq. -1) then
                       wir (e) = sonlst - n - 2
                       sonlst = e
                       nsons = nsons + 1
                    endif
                 else
c                   an artificial frontal matrix
                    ludegr = 1
                    ludegc = fdimc
                    lucp = (ep+9)
                    lurp = (ep+8)
                 endif
 
c                -------------------------------------------------------
                 if (wc (e) - w0 .le. fleftc/2) then
c                this is an lson - assemble all possible rows
c                -------------------------------------------------------
 
c                   ----------------------------------------------------
c                   add to lson list - to be cleared just after scan 4
c                   ----------------------------------------------------
 
                    wr (e) = -lsons
                    lsons = e
 
c                   compute the compressed column offset vector
c                   use wm (1..ludegc for offsets) [
                    degc = 0
                    do 740 i = 0, ludegc-1
                       row2 = ii (lucp+i)
                       if (row2 .gt. 0) then
                          if (wir (row2) .ge. 0) then
c                            this row will be assembled in loop below
                             degc = degc + 1
                             wj (degc) = i
                             wm (degc) = wir (row2)
c                            flag the row as assembled from the lson
                             ii (lucp+i) = -row2
                          endif
                       endif
740                 continue
 
c                   ----------------------------------------------------
                    if (ludegr .eq. fleftr) then
c                   no columns assembled out this lson yet
c                   ----------------------------------------------------
 
                       do 760 j = 0, ludegr-1
                          col2 = ii (lurp+j)
                          xdp = ffxp + wic (col2)
cfpp$ nodepchk l
                          do 750 i = 1, degc
                             xx (xdp + wm (i)) =
     $                       xx (xdp + wm (i)) +
     $                       xx (fxp + j*fdimc + wj (i))
750                       continue
760                    continue
 
c                   ----------------------------------------------------
                    else
c                   some columns already assembled out of this lson
c                   ----------------------------------------------------
 
                       do 780 j = 0, ludegr-1
                          col2 = ii (lurp+j)
                          if (col2 .gt. 0) then
                             xdp = ffxp + wic (col2)
cfpp$ nodepchk l
                             do 770 i = 1, degc
                                xx (xdp + wm (i)) =
     $                          xx (xdp + wm (i)) +
     $                          xx (fxp + j*fdimc + wj (i))
770                          continue
                          endif
780                    continue
                    endif
 
c                   done using wm (1..ludegc for offsets) ]
                    fleftc = wc (e) - w0
                    ii (ep+6) = fleftc
 
c                -------------------------------------------------------
                 else
c                this is an lson - assemble just one row
c                -------------------------------------------------------
 
                    xldp = ffxp + wir (row)
c                   get the offset, f, from the (e,f) tuple
                    f = ii (ip+1)
 
c                   ----------------------------------------------------
                    if (ludegr .eq. fleftr) then
c                   no columns assembled out this lson yet
c                   ----------------------------------------------------
 
cfpp$ nodepchk l
                       do 790 j = 0, ludegr-1
                          col2 = ii (lurp+j)
                          xx (xldp + wic (col2)) =
     $                    xx (xldp + wic (col2)) +
     $                    xx (fxp + j*fdimc + f)
790                    continue
 
c                   ----------------------------------------------------
                    else
c                   some columns already assembled out of this lson
c                   ----------------------------------------------------
 
cfpp$ nodepchk l
                       do 800 j = 0, ludegr-1
                          col2 = ii (lurp+j)
                          if (col2 .gt. 0) then
                             xx (xldp + wic (col2)) =
     $                       xx (xldp + wic (col2)) +
     $                       xx (fxp + j*fdimc + f)
                          endif
800                    continue
                    endif
 
                    ii (ep+6) = fleftc - 1
c                   flag the row as assembled from the lson
                    ii (lucp+f) = -row
                 endif
 
810           continue
 
c             ----------------------------------------------------------
c             update the count of (e,f) tuples in the element list
c             ----------------------------------------------------------
 
              wr (row) = wr (row) - deln
              ineed = ineed - 2*deln
           endif
 
c          -------------------------------------------------------------
c          assemble the original row and update count of entries
c          -------------------------------------------------------------
 
           rlen = wc (row)
           if (rlen .gt. 0) then
c             do not scan a very long row:
              if (rlen .le. rscan) then
                 rsiz = ii (pr)
                 ip = pr + rsiz - rlen
                 dlen = 0
cfpp$ nodepchk l
                 do 820 p = ip, ip + rlen - 1
                    col = ii (p)
                    if (wic (col) .ne. -2) then
c                      this entry can be assembled and deleted
c                      if wic (col) = -1, it is an older pivot col,
c                      otherwise (>=0) it is in the current element
                       dlen = dlen + 1
                       wm (dlen) = p
                    endif
820              continue
                 if (dlen .ne. 0) then
                    do 830 j = 1, dlen
c                      delete the entry
                       ii (wm (j)) = ii (ip+j-1)
830                 continue
                    rlen = rlen - dlen
                    ineed = ineed - dlen
                    wc (row) = rlen
                 endif
              endif
              rdeg = rdeg + rlen
           endif
 
c          -------------------------------------------------------------
c          compute the upper bound degree - excluding current front
c          -------------------------------------------------------------
 
           rdeg2 = ii (pr+1)
           rdeg = min (kleft1 - fflefr, rdeg2, rdeg)
           ii (pr+1) = rdeg
 
840     continue
 
c       ----------------------------------------------------------------
c       scan-4 wrap-up:  remove flags from assembled lsons
c       ----------------------------------------------------------------
 
c       while (lsons .ne. ndn+1) do
850     continue
        if (lsons .ne. ndn+1) then
           next = -wr (lsons)
           wr (lsons) = w0
           lsons = next
c       end while:
        goto 850
        endif
c       done un-flagging lsons, all now unflagged in wr (e) ]
 
c=======================================================================
c  degree update and numerical assemble is complete ]
c=======================================================================
 
c=======================================================================
c  factorize frontal matrix until next pivot extends it [
c=======================================================================
 
        do 1324 dummy4 = 1, n
c       (this loop is not indented due to its length)
 
c       ----------------------------------------------------------------
c       wc (e) = fextc+w0, where fextc is the external column
c               degree for each element (ep = rp (e)) appearing in
c               the element lists for each row in the pivot column.
c               if wc (e) < w0, then fextc is defined as ii (ep+6)
c
c       wr (e) = fextr+w0, where fextr is the external row
c               degree for each element (ep = rp (e)) appearing in
c               the element lists for each column in the pivot row
c               if wr (e) < w0, then fextr is defined as ii (ep+5)
c
c       wir (row) >= 0 for each row in pivot column pattern.
c               offset into pattern is given by:
c               wir (row) == offset - 1
c               wir (pivrow) is the offset of the latest pivot row
c
c       wic (col) >= 0 for each col in pivot row pattern.
c               wic (col) == (offset - 1) * ffdimc
c               wic (pivcol) is the offset of the latest pivot column
c
c       wpr (1..fflefr) is the pivot row pattern (excl pivot cols)
c       wpc (1..fflefc) is the pivot col pattern (excl pivot rows)
c       ----------------------------------------------------------------
 
c=======================================================================
c  divide pivot column by pivot
c=======================================================================
 
c       k-th pivot in frontal matrix located in c(ffdimc-k+1,ffdimr-k+1)
        xdp = ffxp + (ffdimr - k) * ffdimc
        x = xx (xdp + ffdimc - k)
 
c       divide c(1:fflefc,ffdimr-k+1) by pivot value
        x = one / x
        do 870 p = xdp, xdp + fflefc-1
           xx (p) = xx (p) * x
870     continue
c       count this as a call to the level-1 blas:
        rinfo (4) = rinfo (4) + fflefc
 
c=======================================================================
c  a pivot step is complete
c=======================================================================
 
        kleft = kleft - 1
        npiv = npiv + 1
        info (17) = info (17) + 1
 
c       ----------------------------------------------------------------
c       the pivot column is fully assembled and scaled, and is now the
c       (npiv)-th column of l. the pivot row is the (npiv)-th row of u.
c       ----------------------------------------------------------------
 
        wpr (n-npiv+1) = pivrow
        wpc (n-npiv+1) = pivcol
        wir (pivrow) = -1
        wic (pivcol) = -1
 
c       ----------------------------------------------------------------
c       deallocate the pivot row and pivot column
c       ----------------------------------------------------------------
 
        rlen = wc (pivrow)
        ineed = ineed - cscal - rscal - rlen
        pr = rp (pivrow)
        pc = cp (pivcol)
        ii (pr+1) = -1
        ii (pc+1) = -1
        rp (pivrow) = 0
        cp (pivcol) = 0
 
c=======================================================================
c  local search for next pivot within current frontal matrix [
c=======================================================================
 
        fedegc = fflefc
        fedegr = fflefr
        pfound = .false.
        okcol = fflefc .gt. 0
        okrow = .false.
 
c       ----------------------------------------------------------------
c       find column of minimum degree in current frontal row pattern
c       ----------------------------------------------------------------
 
c       among those columns of least (upper bound) degree, select the
c       column with the lowest column index
        if (okcol) then
           colpos = 0
           pivcol = n+1
           cdeg = n+1
c          can this be vectorized?  this is the most intensive
c          non-vector loop.
           do 880 j = 1, fflefr
              col = wpr (j)
              pc = cp (col)
              cdeg2 = ii (pc+1)
              better = cdeg2 .ge. 0 .and.
     $                (cdeg2 .lt. cdeg .or.
     $                (cdeg2 .eq. cdeg .and. col .lt. pivcol))
              if (better) then
                 cdeg = cdeg2
                 colpos = j
                 pivcol = col
              endif
880        continue
           okcol = colpos .ne. 0
        endif
 
c=======================================================================
c  assemble candidate pivot column in temporary workspace
c=======================================================================
 
        if (okcol) then
           pc = cp (pivcol)
           clen = ii (pc+6)
           okcol = fedegc + clen .le. ffdimc
        endif
 
        if (okcol) then
 
c          -------------------------------------------------------------
c          copy candidate column from current frontal matrix into
c          work vector xx (wxp ... wxp+ffdimc-1) [
c          -------------------------------------------------------------
 
           p = ffxp + (colpos - 1) * ffdimc - 1
cfpp$ nodepchk l
           do 890 i = 1, fflefc
              xx (wxp-1+i) = xx (p+i)
890        continue
 
c          -------------------------------------------------------------
c          update candidate column with previous pivots in this front
c          -------------------------------------------------------------
 
           if (k-k0 .gt. 0 .and. fflefc .ne. 0) then
              call sgemv ('n', fflefc, k-k0,
     $          -one, xx (ffxp + (ffdimr - k) * ffdimc)        ,ffdimc,
     $                xx (ffxp + (colpos - 1) * ffdimc + ffdimc - k), 1,
     $           one, xx (wxp)                                      , 1)
              rinfo (3) = rinfo (3) + 2*fflefc*(k-k0)
           endif
 
c          -------------------------------------------------------------
c          compute extended pivot column in xx (wxp..wxp-1+fedegc).
c          pattern of pivot column is placed in wpc (1..fedegc)
c          -------------------------------------------------------------
 
c          assemble the elements in the element list
           cep = (pc+9)
           celn = ii (pc+5)
           do 930 ip = cep, cep + 2*celn - 2, 2
              e = ii (ip)
              f = ii (ip+1)
              ep = rp (e)
              fleftc = ii (ep+6)
              fdimc = ii (ep+1)
              fxp = ii (ep+2)
              if (e .le. n) then
                 fluip = ii (ep)
                 lucp = (fluip + 7)
                 ludegc = ii (fluip+3)
              else
                 lucp = (ep+9)
                 ludegc = fdimc
              endif
              xp = fxp + f * fdimc
c             split into 3 loops so that they all vectorize on a cray
              f1 = fedegc
              do 900 p = lucp, lucp + ludegc - 1
                 row = ii (p)
                 if (row .gt. 0) then
                    if (wir (row) .lt. 0) then
                       f1 = f1 + 1
                       wpc (f1) = row
                    endif
                 endif
900           continue
              okcol = f1 + clen .le. ffdimc
              if (.not. okcol) then
c                exit out of loop if column too long:
                 go to 940
              endif
              do 910 i = fedegc+1, f1
                 row = wpc (i)
                 wir (row) = i - 1
                 xx (wxp-1+i) = zero
910           continue
              fedegc = f1
cfpp$ nodepchk l
              do 920 j = 0, ludegc - 1
                 row = ii (lucp+j)
                 if (row .gt. 0) then
                    xx (wxp + wir (row)) =
     $              xx (wxp + wir (row)) + xx (xp+j)
                 endif
920           continue
930        continue
c          loop exit label:
940        continue
        endif
 
c=======================================================================
c  find candidate pivot row - unless candidate pivot column is too long
c=======================================================================
 
        if (okcol) then
 
c          -------------------------------------------------------------
c          assemble the original entries in the column
c          -------------------------------------------------------------
 
           csiz = ii (pc)
           ip = pc + csiz - clen
           cxp = ii (pc+2)
cfpp$ nodepchk l
           do 950 i = 0, clen - 1
              row = ii (ip+i)
              wir (row) = fedegc + i
              wpc (fedegc+1+i) = row
              xx  (wxp+fedegc+i) = xx (cxp+i)
950        continue
           fedegc = fedegc + clen
 
c          -------------------------------------------------------------
c          update degree of candidate column - excluding current front
c          -------------------------------------------------------------
 
           cdeg = fedegc - fflefc
           ii (pc+1) = cdeg
 
c          -------------------------------------------------------------
c          find the maximum absolute value in the column
c          -------------------------------------------------------------
 
           maxval = abs (xx (wxp-1 + isamax (fedegc, xx (wxp), 1)))
           rinfo (3) = rinfo (3) + fedegc
           toler = relpt * maxval
           rdeg = n+1
 
c          -------------------------------------------------------------
c          look for the best possible pivot row in this column
c          -------------------------------------------------------------
 
           if (maxval .gt. zero) then
              if (symsrc) then
c                prefer symmetric pivots, if numerically acceptable
                 pivrow = pivcol
                 rowpos = wir (pivrow) + 1
                 if (rowpos .gt. 0 .and. rowpos .le. fflefc) then
c                   diagonal entry exists in the column pattern
c                   also within the current frontal matrix
                    x = abs (xx (wxp-1+rowpos))
                    if (x.ge.toler .and. x.gt.zero) then
c                      diagonal entry is numerically acceptable
                       pr = rp (pivrow)
                       rdeg = ii (pr+1)
                    endif
                 endif
              endif
              if (rdeg .eq. n+1) then
c                continue searching - no diagonal found or sought for.
c                minimize row degree subject to abs(value) constraints.
                 pivrow = n+1
                 do 960 i = 1, fflefc
                    row2 = wpc (i)
                    pr = rp (row2)
                    rdeg2 = ii (pr+1)
c                   among those numerically acceptable rows of least
c                   (upper bound) degree, select the row with the
c                   lowest row index
                    better = rdeg2 .lt. rdeg .or.
     $                      (rdeg2 .eq. rdeg .and. row2 .lt. pivrow)
                    if (better) then
                       x = abs (xx (wxp-1+i))
                       if (x.ge.toler .and. x.gt.zero) then
                          pivrow = row2
                          rdeg = rdeg2
                          rowpos = i
                       endif
                    endif
960              continue
              endif
           else
c             remove this column from any further pivot search
              cdeg = -(n+2)
              ii (pc+1) = cdeg
           endif
           okrow = rdeg .ne. n+1
        endif
 
c       done using xx (wxp...wxp+ffdimc-1) ]
 
c=======================================================================
c  if found, construct candidate pivot row pattern
c=======================================================================
 
        if (okrow) then
 
c          -------------------------------------------------------------
c          assemble the elements in the element list
c          -------------------------------------------------------------
 
           pr = rp (pivrow)
           rep = (pr+2)
           reln = wr (pivrow)
           do 990 ip = rep, rep + 2*reln - 2, 2
              e = ii (ip)
              ep = rp (e)
              if (e .le. n) then
                 fluip = ii (ep)
                 lucp = (fluip + 7)
                 ludegr = ii (fluip+2)
                 ludegc = ii (fluip+3)
                 lurp = lucp + ludegc
                 fleftr = ii (ep+5)
                 okrow = fleftr .le. ffdimr
                 if (.not. okrow) then
c                   exit out of loop if row too long:
                    go to 1000
                 endif
c                split into two loops so that both vectorize on a cray
                 f1 = fedegr
                 do 970 p = lurp, lurp + ludegr - 1
                    col = ii (p)
                    if (col .gt. 0) then
                       if (wic (col) .eq. -2) then
                          f1 = f1 + 1
                          wpr (f1) = col
                       endif
                    endif
970              continue
                 okrow = f1 .le. ffdimr
                 if (.not. okrow) then
c                   exit out of loop if row too long:
                    go to 1000
                 endif
                 do 980 i = fedegr+1, f1
                    wic (wpr (i)) = (i - 1) * ffdimc
980              continue
                 fedegr = f1
              else
c                this is an artificial element (a dense column)
                 lurp = (ep+8)
                 col = ii (lurp)
                 if (wic (col) .eq. -2) then
                    wic (col) = fedegr * ffdimc
                    fedegr = fedegr + 1
                    wpr (fedegr) = col
                    okrow = fedegr .le. ffdimr
                    if (.not. okrow) then
c                      exit out of loop if row too long:
                       go to 1000
                    endif
                 endif
              endif
990        continue
c          loop exit label:
1000       continue
        endif
 
        if (okrow) then
 
c          -------------------------------------------------------------
c          assemble the original entries in the row
c          -------------------------------------------------------------
 
           rlen = wc (pivrow)
           if (rlen .gt. 0) then
              f1 = fedegr
              rsiz = ii (pr)
              p2 = pr + rsiz
c             split into two loops so that they both vectorize on a cray
              do 1010 p = p2 - rlen, p2 - 1
                 col = ii (p)
                 if (wic (col) .eq. -2) then
c                   this entry cannot be assembled, do not delete
                    f1 = f1 + 1
                    wpr (f1) = col
                 endif
1010          continue
              rlen2 = f1 - fedegr
              if (rlen2 .lt. rlen) then
c                delete one or more entries in the row
                 do 1020 i = fedegr+1, f1
                    ii (p2 - f1 + i - 1) = wpr (i)
1020             continue
                 ineed = ineed - (rlen - rlen2)
                 wc (pivrow) = rlen2
              endif
 
c             ----------------------------------------------------------
c             update the candidate row degree - excluding current front
c             ----------------------------------------------------------
 
              rdeg = f1 - fflefr
              ii (pr+1) = rdeg
 
c             ----------------------------------------------------------
c             pivot is found if candidate pivot row is not too long
c             ----------------------------------------------------------
 
              okrow = f1 .le. ffdimr
              if (okrow) then
                 do 1030 i = fedegr+1, f1
                    wic (wpr (i)) = (i - 1) * ffdimc
1030             continue
                 fedegr = f1
              endif
 
           else
 
c             ----------------------------------------------------------
c             update the candidate row degree - excluding current front
c             ----------------------------------------------------------
 
              rdeg = fedegr - fflefr
              ii (pr+1) = rdeg
           endif
        endif
 
c       ----------------------------------------------------------------
c       if pivot not found: clear wir and wic
c       ----------------------------------------------------------------
 
        pfound = okrow .and. okcol
        if (.not. pfound) then
           movelu = k .gt. 0
           do 1040 i = fflefr+1, fedegr
              wic (wpr (i)) = -2
1040       continue
           fedegr = fflefr
           do 1050 i = fflefc+1, fedegc
              wir (wpc (i)) = -1
1050       continue
           fedegc = fflefc
        else
           movelu = fedegc .gt. ffdimc - k .or. fedegr .gt. ffdimr - k
        endif
 
c       ----------------------------------------------------------------
c       wpr (1..fflefr)                 unextended pivot row pattern
c       wpr (fflefr+1 .. fedegr)        extended pattern, if pfound
c       wpr (fedegr+1 .. n-npiv)        empty space
c       wpr (n-npiv+1 .. n)             pivot row order
c
c       wpc (1..fflefc)                 unextended pivot column pattern
c       wpc (fflefc+1 .. fedegc)        extended pattern, if pfound
c       wpc (fedegc+1 .. n-npiv)        empty space
c       wpc (n-npiv+1 .. n)             pivot column order
c       ----------------------------------------------------------------
 
c=======================================================================
c  local pivot search complete ]
c=======================================================================
 
c=======================================================================
c  update contribution block: rank-nb, or if lu arrowhead to be moved
c=======================================================================
 
        if (k-k0 .ge. nb .or. movelu) then
           call sgemm ('n', 'n', fflefc, fflefr, k-k0,
     $          -one, xx (ffxp + (ffdimr - k) * ffdimc), ffdimc,
     $                xx (ffxp +  ffdimc - k)          , ffdimc,
     $           one, xx (ffxp)                        , ffdimc)
           rinfo (6) = rinfo (6) + 2*fflefc*fflefr*(k-k0)
           k0 = k
        endif
 
c=======================================================================
c  move the lu arrowhead if no pivot found, or pivot needs room
c=======================================================================
 
        if (movelu) then
 
c          allocate permanent space for the lu arrowhead
           ludegr = fflefr
           ludegc = fflefc
           xs = k*ludegc + k*ludegr + k*k
           is = 7 + ludegc + ludegr + nsons
           if (is .gt. itail-ihead .or. xs .gt. xtail-xhead) then
              if (is .gt. itail-ihead) then
c                garbage collection because we ran out of integer mem
                 info (14) = info (14) + 1
              endif
              if (xs .gt. xtail-xhead) then
c                garbage collection because we ran out of real mem
                 info (15) = info (15) + 1
              endif
              call ums2fg (xx, xsize, xhead, xtail, xuse,
     $                     ii, isize, ihead, itail, iuse,
     $                     cp, rp, dn, n, icntl, wir, wic, wr, wc,
     $                     ffxp, ffsize, wxp, ffdimc, .false.,
     $                     pfree, xfree, mhead, mtail, slots)
c             at this point, iuse = ineed and xuse = xneed
           endif
 
           itail = itail - is
           luip = itail
           iuse = iuse + is
           ineed = ineed + is
           xtail = xtail - xs
           luxp = xtail
           xuse = xuse + xs
           xneed = xneed + xs
           info (18) = max (info (18), iuse)
           info (19) = max (info (19), ineed)
           info (20) = max (info (20), xuse)
           info (21) = max (info (21), xneed)
           if (ihead .gt. itail .or. xhead .gt. xtail) then
c             error return, if not enough integer and/or real memory:
              go to 9000
           endif
 
c          -------------------------------------------------------------
c          get memory usage for next call to ums2rf
c          -------------------------------------------------------------
 
           xruse = xruse + xs
           xrmax = max (xrmax, xruse)
 
c          -------------------------------------------------------------
c          save the new lu arrowhead
c          -------------------------------------------------------------
 
c          save the scalar data of the lu arrowhead
           ii (luip) = luxp
           ii (luip+1) = k
           ii (luip+2) = ludegr
           ii (luip+3) = ludegc
           ii (luip+4) = nsons
           ii (luip+5) = 0
           ii (luip+6) = 0
           e = ffrow
           if (e .eq. e1) then
c             this is the first lu arrowhead from this global pivot
              luip1 = luip
           endif
           wr (e) = -(ndn+2)
           wc (e) = -(ndn+2)
 
c          save column pattern
           lucp = (luip + 7)
           do 1060 i = 0, ludegc-1
              ii (lucp+i) = wpc (i+1)
1060       continue
 
c          save row pattern
           lurp = lucp + ludegc
           do 1070 i = 0, ludegr-1
              ii (lurp+i) = wpr (i+1)
1070       continue
 
c          add list of sons after the end of the frontal matrix pattern
c          this list of sons is for the refactorization (ums2rf) only.
           lusonp = lurp + ludegr
           ip = lusonp
           e = sonlst
c          while (e > 0) do
1080       continue
           if (e .gt. 0) then
              ep = rp (e)
              if (wc (e) .eq. -(ndn+2)) then
c                luson
                 ii (ip) = e
              else if (wc (e) .eq. w0) then
c                uson
                 ii (ip) = e + n
              else if (wr (e) .eq. w0) then
c                lson
                 ii (ip) = e + 2*n
              endif
              next = wir (e) + n + 2
              wir (e) = -1
              e = next
              ip = ip + 1
c          end while:
           goto 1080
           endif
           nsons = 0
           sonlst = 0
 
c          move the l1,u1 matrix, compressing the dimension from
c          ffdimc to ldimc.  the lu arrowhead grows on top of stack.
           ldimc = k + ludegc
           xp = ffxp + (ffdimr-1)*ffdimc + ffdimc-1
           do 1100 j = 0, k-1
cfpp$ nodepchk l
              do 1090 i = 0, k-1
                 xx (luxp + j*ldimc + i) = xx (xp - j*ffdimc - i)
1090          continue
1100       continue
 
c          move l2 matrix, compressing dimension from ffdimc to ludegc+k
           if (ludegc .ne. 0) then
              lxp = luxp + k
              xp = ffxp + (ffdimr-1)*ffdimc
              do 1120 j = 0, k-1
cfpp$ nodepchk l
                 do 1110 i = 0, ludegc-1
                    xx (lxp + j*ldimc + i) = xx (xp - j*ffdimc + i)
1110             continue
1120          continue
           endif
 
c          move the u2 block.
           if (ludegr .ne. 0) then
              uxp = luxp + k * ldimc
              xp = ffxp + ffdimc-1
              do 1140 j = 0, ludegr-1
cfpp$ nodepchk l
                 do 1130 i = 0, k-1
                    xx (uxp + j*k + i) = xx (xp + j*ffdimc - i)
1130             continue
1140          continue
           endif
 
c          one more lu arrowhead has been created
           nlu = nlu + 1
           nzu = (k*(k-1)/2) + k*ludegc
           nzl = (k*(k-1)/2) + k*ludegr
           info (10) = info (10) + nzl
           info (11) = info (11) + nzu
 
c          no more rows of u or columns of l in current frontal array
           k = 0
           k0 = 0
 
           if (pfound) then
 
c             ----------------------------------------------------------
c             place the old frontal matrix as the only item in the son
c             list, since the next "implied" frontal matrix will have
c             this as its son.
c             ----------------------------------------------------------
 
              nsons = 1
              e = ffrow
              wir (e) = - n - 2
              sonlst = e
 
c             ----------------------------------------------------------
c             the contribution block of the old frontal matrix is still
c             stored in the current frontal matrix, and continues (in a
c             unifrontal sense) as a "new" frontal matrix (same array
c             but with a new name, and the lu arrowhead is removed and
c             placed in the lu factors).  old name is "ffrow", new name
c             is "pivrow".
c             ----------------------------------------------------------
 
              rp (e) = luip
              ffrow = pivrow
           endif
        endif
 
c=======================================================================
c  stop the factorization of this frontal matrix if no pivot found
c=======================================================================
 
c       (this is the only way out of loop 1395)
        if (.not. pfound) then
c          exit out of loop 1395 if pivot not found:
           go to 1400
        endif
 
c=======================================================================
c  update the pivot column, and move into position as (k+1)-st col of l
c=======================================================================
 
        xsp = (colpos - 1) * ffdimc
        xdp = (ffdimr - k - 1) * ffdimc
        fsp = ffxp + xsp
        fdp = ffxp + xdp
 
        if (k-k0 .gt. 0 .and. fflefc .ne. 0) then
           call sgemv ('n', fflefc, k-k0,
     $          -one, xx (fdp + ffdimc    ), ffdimc,
     $                xx (fsp + ffdimc - k), 1,
     $           one, xx (fsp             ), 1)
           rinfo (5) = rinfo (5) + 2*fflefc*(k-k0)
        endif
 
        if (fflefr .lt. ffdimr - k) then
 
           xlp = (fflefr - 1) * ffdimc
           if (fflefr .eq. colpos) then
 
c             ----------------------------------------------------------
c             move c(:,colpos) => c(:,ffdimr-k)
c             ----------------------------------------------------------
 
              if (ffdimc .le. 64) then
c                copy the gap (useless work but quicker if gap is small)
cdir$ shortloop
cfpp$ nodepchk l
                 do 1150 i = 0, ffdimc - 1
                    xx (fdp+i) = xx (fsp+i)
1150             continue
              else
c                copy only what needs to be copied
c                column of the contribution block:
cfpp$ nodepchk l
                 do 1160 i = 0, fflefc - 1
                    xx (fdp+i) = xx (fsp+i)
1160             continue
c                column of the u2 block
cfpp$ nodepchk l
                 do 1170 i = ffdimc - k, ffdimc - 1
                    xx (fdp+i) = xx (fsp+i)
1170             continue
              endif
 
           else
 
c             ----------------------------------------------------------
c             move c(:,colpos) => c(:,ffdimr-k)
c             move c(:,fflefr) => c(:,colpos)
c             ----------------------------------------------------------
 
              flp = ffxp + xlp
              if (ffdimc .le. 64) then
c                copy the gap (useless work but quicker if gap is small)
cdir$ shortloop
cfpp$ nodepchk l
                 do 1180 i = 0, ffdimc - 1
                    xx (fdp+i) = xx (fsp+i)
                    xx (fsp+i) = xx (flp+i)
1180             continue
              else
 
c                copy only what needs to be copied
c                columns of the contribution block:
cfpp$ nodepchk l
                 do 1190 i = 0, fflefc - 1
                    xx (fdp+i) = xx (fsp+i)
                    xx (fsp+i) = xx (flp+i)
1190             continue
c                columns of the u2 block:
cfpp$ nodepchk l
                 do 1200 i = ffdimc - k, ffdimc - 1
                    xx (fdp+i) = xx (fsp+i)
                    xx (fsp+i) = xx (flp+i)
1200             continue
              endif
 
              swpcol = wpr (fflefr)
              wpr (colpos) = swpcol
              wic (swpcol) = xsp
           endif
 
           if (fedegr .ne. fflefr) then
c             move column fedegr to column fflefr (pattern only)
              swpcol = wpr (fedegr)
              wpr (fflefr) = swpcol
              wic (swpcol) = xlp
           endif
 
        else if (colpos .ne. ffdimr - k) then
 
c          -------------------------------------------------------------
c          swap c(:,colpos) <=> c (:,ffdimr-k)
c          -------------------------------------------------------------
 
           if (ffdimc .le. 64) then
c             swap the gap (useless work but quicker if gap is small)
cdir$ shortloop
cfpp$ nodepchk l
cfpp$ nolstval l
              do 1210 i = 0, ffdimc - 1
                 x = xx (fdp+i)
                 xx (fdp+i) = xx (fsp+i)
                 xx (fsp+i) = x
1210          continue
           else
c             swap only what needs to be swapped
c             columns of the contribution block:
cfpp$ nodepchk l
cfpp$ nolstval l
              do 1220 i = 0, fflefc - 1
                 x = xx (fdp+i)
                 xx (fdp+i) = xx (fsp+i)
                 xx (fsp+i) = x
1220          continue
c             columns of the u2 block:
cfpp$ nodepchk l
cfpp$ nolstval l
              do 1230 i = ffdimc - k, ffdimc - 1
                 x = xx (fdp+i)
                 xx (fdp+i) = xx (fsp+i)
                 xx (fsp+i) = x
1230          continue
           endif
           swpcol = wpr (ffdimr - k)
           wpr (colpos) = swpcol
           wic (swpcol) = xsp
        endif
 
        wic (pivcol) = xdp
        fedegr = fedegr - 1
        scan2 = fflefr
        fflefr = fflefr - 1
 
c=======================================================================
c  move pivot row into position as (k+1)-st row of u, and update
c=======================================================================
 
        xsp = rowpos - 1
        xdp = ffdimc - k - 1
        fsp = ffxp + xsp
        fdp = ffxp + xdp
 
        if (fflefc .lt. ffdimc - k) then
 
           xlp = fflefc - 1
           if (fflefc .eq. rowpos) then
 
c             ----------------------------------------------------------
c             move c(rowpos,:) => c(ffdimc-k,:)
c             ----------------------------------------------------------
 
              if (ffdimr .le. 64) then
c                copy the gap (useless work but quicker if gap is small)
cdir$ shortloop
cfpp$ nodepchk l
                 do 1240 j = 0, (ffdimr - 1) * ffdimc, ffdimc
                    xx (fdp+j) = xx (fsp+j)
1240             continue
              else
c                copy only what needs to be copied
c                row of the contribution block:
cfpp$ nodepchk l
                 do 1250 j = 0, (fflefr - 1) * ffdimc, ffdimc
                    xx (fdp+j) = xx (fsp+j)
1250             continue
c                row of the l2 block:
cfpp$ nodepchk l
                 do 1260 j = (ffdimr - k - 1) * ffdimc,
     $                       (ffdimr - 1) * ffdimc, ffdimc
                    xx (fdp+j) = xx (fsp+j)
1260             continue
              endif
 
           else
 
c             ----------------------------------------------------------
c             move c(rowpos,:) => c(ffdimc-k,:)
c             move c(fflefc,:) => c(rowpos,:)
c             ----------------------------------------------------------
 
              flp = ffxp + xlp
              if (ffdimr .le. 64) then
c                copy the gap (useless work but quicker if gap is small)
cdir$ shortloop
cfpp$ nodepchk l
                 do 1270 j = 0, (ffdimr - 1) * ffdimc, ffdimc
                    xx (fdp+j) = xx (fsp+j)
                    xx (fsp+j) = xx (flp+j)
1270             continue
              else
c                copy only what needs to be copied
c                rows of the contribution block:
cfpp$ nodepchk l
                 do 1280 j = 0, (fflefr - 1) * ffdimc, ffdimc
                    xx (fdp+j) = xx (fsp+j)
                    xx (fsp+j) = xx (flp+j)
1280             continue
c                rows of the l2 block:
cfpp$ nodepchk l
                 do 1290 j = (ffdimr - k - 1) * ffdimc,
     $                       (ffdimr - 1) * ffdimc, ffdimc
                    xx (fdp+j) = xx (fsp+j)
                    xx (fsp+j) = xx (flp+j)
1290             continue
              endif
              swprow = wpc (fflefc)
              wpc (rowpos) = swprow
              wir (swprow) = xsp
           endif
 
           if (fedegc .ne. fflefc) then
c             move row fedegc to row fflefc (pattern only)
              swprow = wpc (fedegc)
              wpc (fflefc) = swprow
              wir (swprow) = xlp
           endif
 
        else if (rowpos .ne. ffdimc - k) then
 
c          -------------------------------------------------------------
c          swap c(rowpos,:) <=> c (ffdimc-k,:)
c          -------------------------------------------------------------
 
           if (ffdimr .le. 64) then
c             swap the gap (useless work but quicker if gap is small)
cdir$ shortloop
cfpp$ nodepchk l
cfpp$ nolstval l
              do 1300 j = 0, (ffdimr - 1) * ffdimc, ffdimc
                 x = xx (fdp+j)
                 xx (fdp+j) = xx (fsp+j)
                 xx (fsp+j) = x
1300          continue
           else
c             swap only what needs to be swapped
c             rows of the contribution block:
cfpp$ nodepchk l
cfpp$ nolstval l
              do 1310 j = 0, (fflefr - 1) * ffdimc, ffdimc
                 x = xx (fdp+j)
                 xx (fdp+j) = xx (fsp+j)
                 xx (fsp+j) = x
1310          continue
c             rows of the l2 block:
cfpp$ nodepchk l
cfpp$ nolstval l
              do 1320 j = (ffdimr - k - 1) * ffdimc,
     $                    (ffdimr - 1) * ffdimc, ffdimc
                 x = xx (fdp+j)
                 xx (fdp+j) = xx (fsp+j)
                 xx (fsp+j) = x
 
1320          continue
           endif
           swprow = wpc (ffdimc - k)
           wpc (rowpos) = swprow
           wir (swprow) = xsp
        endif
 
        wir (pivrow) = xdp
        fedegc = fedegc - 1
        scan1 = fflefc
        fflefc = fflefc - 1
 
        if (k-k0 .gt. 0 .and. fflefr .gt. 0) then
           call sgemv ('t', k-k0, fflefr,
     $       -one, xx (fdp + 1)                    , ffdimc,
     $             xx (fdp + (ffdimr - k) * ffdimc), ffdimc,
     $        one, xx (fdp)                        , ffdimc)
           rinfo (5) = rinfo (5) + 2*(k-k0)*fflefr
        endif
 
c=======================================================================
c  prepare for degree update and next local pivot search
c=======================================================================
 
c       ----------------------------------------------------------------
c       if only column pattern has been extended:
c               scan1:  new rows only
c               scan2:  no columns scanned
c               scan3:  all columns
c               scan4:  new rows only
c
c       if only row pattern has been extended:
c               scan1:  no rows scanned
c               scan2:  new columns only
c               scan3:  new columns only
c               scan4:  all rows
c
c       if both row and column pattern have been extended:
c               scan1:  new rows only
c               scan2:  new columns only
c               scan3:  all columns
c               scan4:  all rows
c
c       if no patterns have been extended:
c               scan1-4: none
c       ----------------------------------------------------------------
 
        if (fedegc .eq. fflefc) then
c          column pattern has not been extended
           scan3 = fflefr + 1
        else
c          column pattern has been extended.
           scan3 = 0
        endif
 
        if (fedegr .eq. fflefr) then
c          row pattern has not been extended
           scan4 = fflefc + 1
        else
c          row pattern has been extended
           scan4 = 0
        endif
 
c=======================================================================
c  finished with step k (except for assembly and scaling of pivot col)
c=======================================================================
 
        k = k + 1
 
c       ----------------------------------------------------------------
c       exit loop if frontal matrix has been extended
c       ----------------------------------------------------------------
 
        if (fedegr .ne. fflefr .or. fedegc .ne. fflefc) then
           go to 1325
        endif
 
1324    continue
c       exit label for loop 1324:
1325    continue
 
c=======================================================================
c  finished factorizing while frontal matrix is not extended ]
c=======================================================================
 
c=======================================================================
c  extend the frontal matrix [
c=======================================================================
 
c       ----------------------------------------------------------------
c       zero the newly extended frontal matrix
c       ----------------------------------------------------------------
 
c       fill-in due to amalgamation caused by this step is
c       k*(fedegr-fflefr+fedegc-fflefc)
 
        do 1350 j = fflefr, fedegr - 1
c          zero the new columns in the contribution block:
           do 1330 i = 0, fedegc - 1
              xx (ffxp + j*ffdimc + i) = zero
1330       continue
c          zero the new columns in u block:
           do 1340 i = ffdimc - k, ffdimc - 1
              xx (ffxp + j*ffdimc + i) = zero
1340       continue
1350    continue
 
cfpp$ nodepchk l
        do 1380 i = fflefc, fedegc - 1
c          zero the new rows in the contribution block:
cfpp$ nodepchk l
           do 1360 j = 0, fflefr - 1
              xx (ffxp + j*ffdimc + i) = zero
1360       continue
c          zero the new rows in l block:
cfpp$ nodepchk l
           do 1370 j = ffdimr - k, ffdimr - 1
              xx (ffxp + j*ffdimc + i) = zero
1370       continue
1380    continue
 
c       ----------------------------------------------------------------
c       remove the new columns from the degree lists
c       ----------------------------------------------------------------
 
        do 1390 j = fflefr+1, fedegr
           pc = cp (wpr (j))
           cdeg = ii (pc+1)
           if (cdeg .gt. 0) then
              cnxt = ii (pc+7)
              cprv = ii (pc+8)
              if (cnxt .ne. 0) then
                 ii (cp (cnxt)+8) = cprv
              endif
              if (cprv .ne. 0) then
                 ii (cp (cprv)+7) = cnxt
              else
                 head (cdeg) = cnxt
              endif
           endif
1390    continue
 
c       ----------------------------------------------------------------
c       finalize extended row and column pattern of the frontal matrix
c       ----------------------------------------------------------------
 
        fflefc = fedegc
        fflefr = fedegr
        fmaxr = max (fmaxr, fflefr + k)
        fmaxc = max (fmaxc, fflefc + k)
 
c=======================================================================
c  done extending the current frontal matrix ]
c=======================================================================
 
1395    continue
c       exit label for loop 1395:
1400    continue
 
c=======================================================================
c  done assembling and factorizing the current frontal matrix ]
c=======================================================================
 
c=======================================================================
c  wrap-up:  complete the current frontal matrix [
c=======================================================================
 
c       ----------------------------------------------------------------
c       store the maximum front size in the first lu arrowhead
c       ----------------------------------------------------------------
 
        ii (luip1+5) = fmaxr
        ii (luip1+6) = fmaxc
 
c       one more frontal matrix is finished
        info (13) = info (13) + 1
 
c       ----------------------------------------------------------------
c       add the current frontal matrix to the degrees of each column,
c       and place the modified columns back in the degree lists
c       ----------------------------------------------------------------
 
c       do so in reverse order to try to improve pivot tie-breaking
        do 1410 j = fflefr, 1, -1
           col = wpr (j)
           pc = cp (col)
c          add the current frontal matrix to the degree
           cdeg = ii (pc+1)
           cdeg = min (kleft, cdeg + fflefc)
           if (cdeg .gt. 0) then
              ii (pc+1) = cdeg
              cnxt = head (cdeg)
              ii (pc+7) = cnxt
              ii (pc+8) = 0
              if (cnxt .ne. 0) then
                 ii (cp (cnxt)+8) = col
              endif
              head (cdeg) = col
              mindeg = min (mindeg, cdeg)
           endif
1410    continue
 
c       ----------------------------------------------------------------
c       add the current frontal matrix to the degrees of each row
c       ----------------------------------------------------------------
 
cfpp$ nodepchk l
        do 1420 i = 1, fflefc
           row = wpc (i)
           pr = rp (row)
           rdeg = ii (pr+1)
           rdeg = min (kleft, rdeg + fflefr)
           ii (pr+1) = rdeg
1420    continue
 
c       ----------------------------------------------------------------
c       reset w0 so that wr (1..n) < w0 and wc (1..n) < w0.
c       also ensure that w0 + n would not cause integer overflow
c       ----------------------------------------------------------------
 
        w0 = w0 + fmax + 1
        if (w0 .ge. w0big) then
           w0 = ndn+2
           do 1430 e = 1, n+dn
              if (wr (e) .gt. ndn) then
c                this is a frontal matrix
                 wr (e) = w0-1
                 wc (e) = w0-1
              endif
1430       continue
        endif
 
c       ----------------------------------------------------------------
c       deallocate work vector
c       ----------------------------------------------------------------
 
        xuse = xuse - ffdimc
        xneed = xneed - ffdimc
        xhead = xhead - ffdimc
 
c       ----------------------------------------------------------------
c       get the name of this new frontal matrix, and size of
c       contribution block
c       ----------------------------------------------------------------
 
        e = ffrow
        xs = fflefr * fflefc
        fmax = max (fmax, fflefr, fflefc)
 
c       ----------------------------------------------------------------
c       get memory usage for next call to ums2rf
c       ----------------------------------------------------------------
 
        xruse = xruse - ffsize + xs
 
c       ----------------------------------------------------------------
c       if contribution block empty, deallocate and continue next step
c       ----------------------------------------------------------------
 
        if (fflefr .le. 0 .or. fflefc .le. 0) then
           rp (e) = luip
           xuse = xuse - ffsize
           xneed = xneed - ffsize
           xhead = ffxp
           do 1440 i = 1, fflefr
              wic (wpr (i)) = -2
1440       continue
           do 1450 i = 1, fflefc
              wir (wpc (i)) = -1
1450       continue
c          next iteration of main factorization loop 1540:
           goto 1540
        endif
 
c       ----------------------------------------------------------------
c       prepare the contribution block for later assembly
c       ----------------------------------------------------------------
 
        if (fscal .gt. itail-ihead) then
           info (14) = info (14) + 1
           call ums2fg (xx, xsize, xhead, xtail, xuse,
     $                  ii, isize, ihead, itail, iuse,
     $                  cp, rp, dn, n, icntl, wir, wic, wr, wc,
     $                  ffxp, ffsize, 0, 0, .false.,
     $                  pfree, xfree, mhead, mtail, slots)
c          at this point, iuse = ineed and xuse = xneed
        endif
 
        ep = ihead
        ihead = ihead + fscal
        iuse = iuse + fscal
        ineed = ineed + fscal
        info (18) = max (info (18), iuse)
        info (19) = max (info (19), ineed)
        if (ihead .gt. itail) then
c          error return, if not enough integer memory:
c          (highly unlikely to run out of memory at this point)
           go to 9000
        endif
 
        rp (e) = ep
        ii (ep) = luip
        ii (ep+5) = fflefr
        ii (ep+6) = fflefc
        wr (e) = w0-1
        wc (e) = w0-1
 
c       count the numerical assembly
        rinfo (2) = rinfo (2) + xs
 
        if (xs .le. xfree) then
 
c          -------------------------------------------------------------
c          compress and store the contribution block in a freed block
c          -------------------------------------------------------------
 
c          place the new block in the list in front of the free block
           xdp = ii (pfree+2)
           ii (pfree+2) = ii (pfree+2) + xs
           xfree = xfree - xs
           mprev = ii (pfree+4)
           if (xfree .eq. 0) then
c             delete the free block if its size is zero
              mnext = ii (pfree+3)
              pfree = 0
              xfree = -1
           else
              mnext = pfree
           endif
           if (mnext .ne. 0) then
              ii (mnext+4) = ep
           else
              mtail = ep
           endif
           if (mprev .ne. 0) then
              ii (mprev+3) = ep
           else
              mhead = ep
           endif
           do 1470 j = 0, fflefr - 1
cfpp$ nodepchk l
              do 1460 i = 0, fflefc - 1
                 xx (xdp + j*fflefc + i) = xx (ffxp + j*ffdimc + i)
1460          continue
1470       continue
           xhead = ffxp
           xuse = xuse - ffsize
           xneed = xneed - ffsize + xs
           ffdimc = fflefc
           ii (ep+1) = ffdimc
           ii (ep+2) = xdp
           ii (ep+3) = mnext
           ii (ep+4) = mprev
 
        else
 
c          -------------------------------------------------------------
c          deallocate part of the unused portion of the frontal matrix
c          -------------------------------------------------------------
 
c          leave the contribution block c (1..fflefc, 1..fflefr) at the
c          head of xx, with column dimension of ffdimc and in space
c          of size (fflefr-1)*ffdimc for the first fflefr columns, and
c          fflefc for the last column.
           xneed = xneed - ffsize + xs
           xs = ffsize - (fflefc + (fflefr-1)*ffdimc)
           xhead = xhead - xs
           xuse = xuse - xs
           ii (ep+1) = ffdimc
           ii (ep+2) = ffxp
           ii (ep+3) = 0
           ii (ep+4) = mtail
           if (mtail .eq. 0) then
              mhead = ep
           else
              ii (mtail+3) = ep
           endif
           mtail = ep
        endif
 
c       ----------------------------------------------------------------
c       add tuples to the amount of integer space needed - and add
c       limit+cscal to maximum need to account for worst-case possible
c       reallocation of rows/columns.  required integer memory usage
c       is guaranteed not to exceed iworst during the placement of (e,f)
c       tuples in the two loops below.
c       ----------------------------------------------------------------
 
        ineed = ineed + 2*(fflefr+fflefc)
        iworst = ineed + limit + cscal
        info (19) = max (info (19), iworst)
        info (18) = max (info (18), iworst)
 
c       ----------------------------------------------------------------
c       place (e,f) in the element list of each column
c       ----------------------------------------------------------------
 
        do 1500 i = 1, fflefr
           col = wpr (i)
           pc = cp (col)
           celn = ii (pc+5)
           csiz = ii (pc)
           clen = ii (pc+6)
c          clear the column offset
           wic (col) = -2
 
c          -------------------------------------------------------------
c          make sure an empty slot exists - if not, create one
c          -------------------------------------------------------------
 
           if (2*(celn+1) + clen + cscal .gt. csiz) then
 
c             ----------------------------------------------------------
c             no room exists - reallocate elsewhere
c             ----------------------------------------------------------
 
c             at least this much space is needed:
              is = 2 * (celn + 1) + clen
c             add some slots for growth: at least 8 tuples,
c             or double the size - whichever is larger (but with a total
c             size not larger than limit+cscal)
              is = min (is + max (16, is), limit)
              csiz2 = is + cscal
 
c             ----------------------------------------------------------
c             make sure enough room exists: garbage collection if needed
c             ----------------------------------------------------------
 
              if (csiz2 .gt. itail-ihead) then
c                garbage collection:
                 info (14) = info (14) + 1
                 call ums2fg (xx, xsize, xhead, xtail, xuse,
     $                        ii, isize, ihead, itail, iuse,
     $                        cp, rp, dn, n, icntl, wir, wic, wr, wc,
     $                        0, 0, 0, 0, .true.,
     $                        pfree, xfree, mhead, mtail, slots)
c                at this point, iuse+csiz2 <= iworst and xuse = xneed
                 pc = cp (col)
                 csiz = ii (pc)
              endif
 
c             ----------------------------------------------------------
c             get space for the new copy
c             ----------------------------------------------------------
 
              pc2 = ihead
              ihead = ihead + csiz2
              iuse = iuse + csiz2
              info (18) = max (info (18), iuse)
              if (ihead .gt. itail) then
c                error return, if not enough integer memory:
                 go to 9000
              endif
 
c             ----------------------------------------------------------
c             make the copy, leaving hole in middle for element list
c             ----------------------------------------------------------
 
c             copy the cscal scalars, and the element list
cfpp$ nodepchk l
              do 1480 j = 0, cscal + 2*celn - 1
                 ii (pc2+j) = ii (pc+j)
1480          continue
 
c             copy column indices of original entries (xx is unchanged)
cfpp$ nodepchk l
              do 1490 j = 0, clen - 1
                 ii (pc2+csiz2-clen+j) = ii (pc+csiz-clen+j)
1490          continue
 
              if (clen .gt. 0) then
c                place the new block in the memory-list
                 mnext = ii (pc2+3)
                 mprev = ii (pc2+4)
                 if (mnext .ne. 0) then
                    ii (mnext+4) = pc2
                 else
                    mtail = pc2
                 endif
                 if (mprev .ne. 0) then
                    ii (mprev+3) = pc2
                 else
                    mhead = pc2
                 endif
              endif
 
              cp (col) = pc2
              ii (pc2) = csiz2
 
c             ----------------------------------------------------------
c             deallocate the old copy of the column in ii (not in xx)
c             ----------------------------------------------------------
 
              ii (pc+1) = -1
              ii (pc+6) = 0
              pc = pc2
           endif
 
c          -------------------------------------------------------------
c          place the new (e,f) tuple in the element list of the column
c          -------------------------------------------------------------
 
           cep = (pc+9)
           ii (cep + 2*celn  ) = e
           ii (cep + 2*celn+1) = i - 1
           ii (pc+5) = celn + 1
1500    continue
 
c       ----------------------------------------------------------------
c       place (e,f) in the element list of each row
c       ----------------------------------------------------------------
 
        do 1530 i = 1, fflefc
           row = wpc (i)
           pr = rp (row)
           rsiz = ii (pr)
           reln = wr (row)
           rlen = wc (row)
c          clear the row offset
           wir (row) = -1
 
c          -------------------------------------------------------------
c          make sure an empty slot exists - if not, create one
c          -------------------------------------------------------------
 
           if (2*(reln+1) + rlen + rscal .gt. rsiz) then
 
c             ----------------------------------------------------------
c             no room exists - reallocate elsewhere
c             ----------------------------------------------------------
 
c             at least this much space is needed:
              is = 2 * (reln + 1) + rlen
c             add some extra slots for growth - for at least 8
c             tuples, or double the size (but with a total size not
c             larger than limit+rscal)
              is = min (is + max (16, is), limit)
              rsiz2 = is + rscal
 
c             ----------------------------------------------------------
c             make sure enough room exists: garbage collection if needed
c             ----------------------------------------------------------
 
              if (rsiz2 .gt. itail-ihead) then
c                garbage collection:
                 info (14) = info (14) + 1
                 call ums2fg (xx, xsize, xhead, xtail, xuse,
     $                        ii, isize, ihead, itail, iuse,
     $                        cp, rp, dn, n, icntl, wir, wic, wr, wc,
     $                        0, 0, 0, 0, .true.,
     $                        pfree, xfree, mhead, mtail, slots)
c                at this point, iuse+rsiz2 <= iworst and xuse = xneed
                 pr = rp (row)
                 rsiz = ii (pr)
              endif
 
c             ----------------------------------------------------------
c             get space for the new copy
c             ----------------------------------------------------------
 
              pr2 = ihead
              ihead = ihead + rsiz2
              iuse = iuse + rsiz2
              info (18) = max (info (18), iuse)
              if (ihead .gt. itail) then
c                error return, if not enough integer memory:
                 go to 9000
              endif
 
c             ----------------------------------------------------------
c             make the copy, leaving hole in middle for element list
c             ----------------------------------------------------------
 
c             copy the rscal scalars, and the element list
cfpp$ nodepchk l
              do 1510 j = 0, rscal + 2*reln - 1
                 ii (pr2+j) = ii (pr+j)
1510          continue
 
c             copy the original entries
cfpp$ nodepchk l
              do 1520 j = 0, rlen - 1
                 ii (pr2+rsiz2-rlen+j) = ii (pr+rsiz-rlen+j)
1520          continue
 
              rp (row) = pr2
              ii (pr2) = rsiz2
 
c             ----------------------------------------------------------
c             deallocate the old copy of the row
c             ----------------------------------------------------------
 
              ii (pr+1) = -1
              pr = pr2
           endif
 
c          -------------------------------------------------------------
c          place the new (e,f) tuple in the element list of the row
c          -------------------------------------------------------------
 
           rep = (pr+2)
           ii (rep + 2*reln  ) = e
           ii (rep + 2*reln+1) = i - 1
           wr (row) = reln + 1
1530    continue
 
c=======================================================================
c  wrap-up of factorized frontal matrix is complete ]
c=======================================================================
 
1540    continue
c       exit label for loop 1540:
2000    continue
 
c=======================================================================
c=======================================================================
c  end of main factorization loop ]
c=======================================================================
c=======================================================================
 
c=======================================================================
c  wrap-up:  store lu factors in their final form [
c=======================================================================
 
c       ----------------------------------------------------------------
c       deallocate all remaining columns, rows, and frontal matrices
c       ----------------------------------------------------------------
 
        iuse = iuse - (ihead - 1)
        xuse = xuse - (xhead - 1)
        ineed = iuse
        xneed = xuse
        ihead = 1
        xhead = 1
 
        if (nlu .eq. 0) then
c          lu factors are completely empty (a = 0).
c          add one integer and one real, to simplify rest of code.
c          otherwise, some arrays in ums2rf or ums2so would have
c          zero size, which can cause an address fault.
           itail = isize
           xtail = xsize
           iuse = iuse + 1
           xuse = xuse + 1
           ineed = iuse
           xneed = xuse
           ip = itail
           xp = xtail
        endif
 
c       ----------------------------------------------------------------
c       compute permutation and inverse permutation vectors.
c       use wir/c for the row/col permutation, and wpr/c for the
c       inverse row/col permutation.
c       ----------------------------------------------------------------
 
        do 2010 k = 1, n
c          the kth pivot row and column:
           row = wpr (n-k+1)
           col = wpc (n-k+1)
           wir (k) = row
           wic (k) = col
2010    continue
c       replace wpr/c with the inversion permutations:
        do 2020 k = 1, n
           row = wir (k)
           col = wic (k)
           wpr (row) = k
           wpc (col) = k
2020    continue
 
        if (pgiven) then
c          the input matrix had been permuted from the original ordering
c          according to rperm and cperm.  combine the initial
c          permutations (now in rperm and cperm) and the pivoting
c          permutations, and place them back into rperm and cperm.
           do 2030 row = 1, n
              wm (wpr (row)) = rperm (row)
2030       continue
           do 2040 row = 1, n
              rperm (row) = wm (row)
2040       continue
           do 2050 col = 1, n
              wm (wpc (col)) = cperm (col)
2050       continue
           do 2060 col = 1, n
              cperm (col) = wm (col)
2060       continue
c       else
c          the input matrix was not permuted on input.  rperm and cperm
c          in ums2f1 have been passed to this routine as wir and wic,
c          which now contain the row and column permutations.  rperm and
c          cperm in this routine (ums2f2) are not defined.
        endif
 
c       ----------------------------------------------------------------
c       allocate nlu+3 integers for xtail, nlu, npiv and lup (1..nlu)
c       ----------------------------------------------------------------
 
        is = nlu + 5
        luip1 = itail
        itail = itail - is
        iuse = iuse + is
        ineed = iuse
        info (18) = max (info (18), iuse)
        info (19) = max (info (19), ineed)
        if (ihead .le. itail) then
 
c          -------------------------------------------------------------
c          sufficient memory exist to finish the factorization
c          -------------------------------------------------------------
 
           ii (itail+1) = nlu
           ii (itail+2) = npiv
           lupp = itail+5
           if (nlu .eq. 0) then
c             zero the dummy entries, if lu factors are empty
              ii (ip) = 0
              xx (xp) = zero
           endif
 
c          -------------------------------------------------------------
c          convert the lu factors into the new pivot order
c          -------------------------------------------------------------
 
           s = 0
           maxdr = 1
           maxdc = 1
           do 2100 k = 1, n
              e = wir (k)
              luip = rp (e)
              if (luip .gt. 0) then
c                this is an lu arrowhead - save a pointer in lup:
                 s = s + 1
c                update pointers to lu arrowhead relative to start of lu
                 ii (lupp+s-1) = luip - luip1 + 1
                 luxp = ii (luip)
                 ii (luip) = luxp - xtail + 1
c                convert the row and column indices to their final order
c                pattern of a column of l:
                 p = (luip + 7)
                 ludegc = ii (luip+3)
                 maxdc = max (maxdc, ludegc)
                 do 2070 j = 1, ludegc
                    ii (p) = wpr (abs (ii (p)))
                    p = p + 1
2070             continue
c                pattern of a row of u:
                 ludegr = ii (luip+2)
                 maxdr = max (maxdr, ludegr)
                 do 2080 j = 1, ludegr
                    ii (p) = wpc (abs (ii (p)))
                    p = p + 1
2080             continue
c                convert the lusons, usons, and lsons:
                 nsons = ii (luip+4)
                 do 2090 j = 1, nsons
                    eson = ii (p)
                    if (eson .le. n) then
c                      an luson
                       ii (p) = wm (eson)
                    else if (eson .le. 2*n) then
c                      a uson
                       ii (p) = wm (eson-n) + n
                    else
c                      an lson
                       ii (p) = wm (eson-2*n) + 2*n
                    endif
                    p = p + 1
2090             continue
c                renumber this lu arrowhead
                 wm (e) = s
              endif
2100       continue
 
           cmax = max (cmax, maxdc)
           rmax = max (rmax, maxdr)
           totnlu = totnlu + nlu
 
           ii (itail+3) = maxdc
           ii (itail+4) = maxdr
 
c          -------------------------------------------------------------
c          get memory usage for next call to ums2rf
c          -------------------------------------------------------------
 
           xruse = xruse - nz
           return
        endif
 
c=======================================================================
c  lu factors are now stored in their final form ]
c=======================================================================
 
c=======================================================================
c  error conditions
c=======================================================================
 
c       error return label:
9000    continue
        if (ihead .gt. itail .or. isize .lt. minmem) then
c          error return if out of integer memory
           call ums2er (1, icntl, info, -3, info (19))
        endif
        if (xhead .gt. xtail) then
c          error return if out of real memory
           call ums2er (1, icntl, info, -4, info (21))
        endif
        return
        end
 
        subroutine ums2fa (n, ne, job, transa, lvalue, lindex, value,
     $          index, keep, cntl, icntl, info, rinfo)
        integer n, ne, job, lvalue, lindex, index (lindex), keep (20),
     $          icntl (20), info (40)
        real
     $          value (lvalue), cntl (10), rinfo (20)
        logical transa
 
c=== ums2fa ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  given a sparse matrix a, find a sparsity-preserving and numerically-
c  acceptable pivot order and compute the lu factors, paq = lu.  the
c  matrix is optionally preordered into a block upper triangular form
c  (btf).  pivoting is performed within each diagonal block to maintain
c  sparsity and numerical stability.  the method used to factorize the
c  matrix is an unsymmetric-pattern variant of the multifrontal method.
c  most of the floating-point work is done in the level-3 blas (dense
c  matrix multiply).  in addition, approximate degrees are used in the
c  markowitz-style pivot search to reduce the symbolic overhead.  for
c  best performance, be sure to use an optimized blas library.
c
c  this routine is normally preceded by a call to ums2in, to
c  initialize the default control parameters.  ums2in need only be
c  called once.  a call to ums2fa can be followed by any number of
c  calls to ums2so, which solves a linear system using the lu factors
c  computed by this routine.  a call to ums2fa can also be followed by
c  any number of calls to ums2rf, which factorizes another matrix with
c  the same nonzero pattern as the matrix factorized by ums2fa (but with
c  different numerical values).
c
c  for more information, see t. a. davis and i. s. duff, "an
c  unsymmetric-pattern multifrontal method for sparse lu factorization",
c  siam j. matrix analysis and applications (to appear), also
c  technical report tr-94-038, cise dept., univ. of florida,
c  p.o. box 116120, gainesville, fl 32611-6120, usa.  the method used
c  here is a modification of that method, described in t. a. davis,
c  "a combined unifrontal/multifrontal method for unsymmetric sparse
c  matrices," tr-94-005.  (technical reports are available via www at
c  http://www.cis.ufl.edu/).  the appoximate degree update algorithm
c  used here has been incorporated into an approximate minimum degree
c  ordering algorithm, desribed in p. amestoy, t. a. davis, and i. s.
c  duff, "an approximate minimum degree ordering algorithm", siam j.
c  matrix analysis and applications (to appear, also tr-94-039).  the
c  approximate minimum degree ordering algorithm is implemented as mc47
c  in the harwell subroutine library (mc47 is not called by umfpack).
 
c=======================================================================
c  installation note:
c=======================================================================
c
c  requires the blas (basic linear algebra subprograms) and two routines
c  from the harwell subroutine library.  ideally, you should use
c  vendor-optimized blas for your computer.  if you do not have them,
c  you may obtain the fortran blas from 1.  send email to
c  netlib@ornl.gov with the two-line message:
c               send index from blas
c               send blas.shar from blas
c
c  to obtain the two harwell subroutine library (hsl) routines, send
c  email to netlib@ornl.gov with the message:
c               send mc21b.f mc13e.f from harwell
c  these two routines hsl contain additional licensing restrictions.
c  if you want to run umfpack without them, see the "installation
c  note:" comment in ums2fb.
c
c  to permamently disable any diagnostic and/or error printing, see
c  the "installation note:" comments in ums2p1 and ums2p2.
c
c  to change the default control parameters, see the
c  "installation note:" comments in ums2in.
 
c=======================================================================
c  arguments:
c=======================================================================
 
c           ------------------------------------------------------------
c  n:       an integer variable.
c           must be set by caller on input (not modified).
c           order of the matrix.  restriction:  1 <= n <= (maxint-5)/3,
c           where maxint is the largest representable positive integer.
 
c           ------------------------------------------------------------
c  ne:      an integer variable.
c           must be set by caller on input (not modified).
c           number of entries in input matrix.  restriction:  ne => 1.
 
c           ------------------------------------------------------------
c  job:     an integer variable.
c           must be set by caller on input (not modified).
c           if job=1, then a column-oriented form of the input matrix
c           is preserved, otherwise, the input matrix is overwritten
c           with its lu factors.  if iterative refinement is to done
c           in ums2so, (icntl (8) > 0), then job must be set to 1.
 
c           ------------------------------------------------------------
c  transa:  an integer variable.
c           must be set by caller on input (not modified).
c           if false then a is factorized: paq = lu.  otherwise, a
c           transpose is factorized:  pa'q = lu.
 
c           ------------------------------------------------------------
c  lvalue:  an integer variable.
c           must be set by caller on input (not modified).
c           size of the value array.  restriction:  lvalue >= 2*ne
c           is required to convert the input form of the matrix into
c           the internal represenation.  lvalue >= ne + axcopy is
c           required to start the factorization, where axcopy = ne if
c           job = 1, or axcopy = 0 otherwise.  during factorization,
c           additional memory is required to hold the frontal matrices.
c           the internal representation of the matrix is overwritten
c           with the lu factors, of size (keep (2) - keep (1) + 1
c           + axcopy), on output.
 
c           ------------------------------------------------------------
c  lindex:  an integer variable.
c           must be set by caller on input (not modified).
c           size of the index array.  restriction: lindex >= 3*ne+2*n+1,
c           is required to convert the input form of the matrix into
c           its internal representation.  lindex >= wlen + alen + acopy
c           is required to start the factorization, where
c           wlen <= 11*n + 3*dn + 8 is the size of the workspaces,
c           dn <= n is the number of columns with more than d
c           entries (d = max (64, sqrt (n)) is the default),
c           alen <= 2*ne + 11*n + 11*dn + dne is the size of the
c           internal representation of the matrix, dne <= ne is the
c           number of entries in such columns with more than d entries,
c           and acopy = ne+n+1 if job = 1, or acopy = 0 otherwize.
c           during factorization, the internal representation of size
c           alen is overwritten with the lu factors, of size
c           luilen = (keep (5) - keep (3) + 1 - acopy) on output.
c           additional memory is also required to hold the unsymmetric
c           quotient graph, but this also overwrites the input matrix.
c           usually about 7*n additional space is adequate for this
c           purpose.  just prior to the end of factorization,
c           lindex >= wlen + luilen + acopy is required.
 
c           ------------------------------------------------------------
c  value:   a real array of size lvalue.
c           must be set by caller on input.  modified on output.  on
c           input, value (1..ne) holds the original matrix in triplet
c           form.  on output, value holds the lu factors, and
c           (optionally) a column-oriented form of the original matrix
c           - otherwise the input matrix is overwritten with the lu
c           factors.
 
c           ------------------------------------------------------------
c  index:   an integer array of size lindex.
c           must be set by caller on input.  modified on output.  on
c           input, index (1..2*ne) holds the original matrix in triplet
c           form.  on output, index holds the lu factors, and
c           (optionally) a column-oriented form of the original matrix
c           - otherwise the input matrix is overwritten with the lu
c           factors.
c
c           on input the kth triplet (for k = 1...ne) is stored as:
c                       a (row,col) = value (k)
c                       row         = index (k)
c                       col         = index (k+ne)
c           if there is more than one entry for a particular position,
c           the values are accumulated, and the number of such duplicate
c           entries is returned in info (2), and a warning flag is
c           set.  however, applications such as finite element methods
c           naturally generate duplicate entries which are then
c           assembled (added) together.  if this is the case, then
c           ignore the warning message.
c
c           on output, the lu factors and the column-oriented form
c           of a (if preserved) are stored in:
c               value (keep (1)...keep (2))
c               index (keep (3)...keep (5))
c           where keep (2) = lvalue, and keep (5) = lindex.
 
c           ------------------------------------------------------------
c  keep:    an integer array of size 20.
c
c           keep (1 ... 5):  need not be set by caller on input.
c               modified on output.
c               keep (1): lu factors start here in value
c               keep (2) = lvalue: lu factors end here in value
c               keep (3): lu factors start here in index
c               keep (4): lu factors needed for ums2rf start here
c                             in index
c               keep (5) = lindex: lu factors end here in index
c
c           keep (6 ... 8):  must be set by caller on input (not
c               modified).
c               integer control arguments not normally modified by the
c               user.  see ums2in for details, which sets the defaults.
c               keep (6) is the largest representable positive
c               integer.  keep (7) and keep (8) determine the
c               size of d, where columns with more than d original
c               entries are treated as a priori frontal matrices.
c
c           keep (9 ... 20): unused.  reserved for future releases.
 
c           ------------------------------------------------------------
c  cntl:    a real array of size 10.
c           must be set by caller on input (not modified).
c           real control arguments, see ums2in for a description,
c           which sets the defaults. ums2fa uses cntl (1) and cntl (2).
 
c           ------------------------------------------------------------
c  icntl:   an integer array of size 20.
c           must be set by caller on input (not modified).
c           integer control arguments, see ums2in for a description,
c           which sets the defaults.  ums2fa uses icntl (1..7).
 
c           ------------------------------------------------------------
c  info:    an integer array of size 40.
c           need not be set by caller on input.  modified on output.
c           it contains information about the execution of ums2fa.
c
c           info (1): zero if no error occurred, negative if
c               an error occurred and the factorization was not
c               completed, positive if a warning occurred (the
c               factorization was completed).
c
c               these errors cause the factorization to terminate:
c
c               error   description
c               -1      n < 1 or n > maximum value
c               -2      ne < 1 or ne > maximum value
c               -3      lindex too small
c               -4      lvalue too small
c               -5      both lindex and lvalue are too small
c
c               with these warnings the factorization was able to
c               complete:
c
c               error   description
c               1       invalid entries
c               2       duplicate entries
c               3       invalid and duplicate entries
c               4       singular matrix
c               5       invalid entries, singular matrix
c               6       duplicate entries, singular matrix
c               7       invalid and duplicate entries, singular matrix
c
c               subsequent calls to ums2rf and ums2so can only be made
c               if info (1) is zero or positive.  if info (1)
c               is negative, then some or all of the remaining
c               info and rinfo arrays may not be valid.
c
c           info (2): duplicate entries in a.  a warning is set
c               if info (2) > 0.  however, the duplicate entries
c               are summed and the factorization continues.  duplicate
c               entries are sometimes intentional - for finite element
c               codes, for example.
c
c           info (3): invalid entries in a, indices not in 1..n.
c               these entries are ignored and a warning is set
c               in info (1).
c
c           info (4): zero.  used by ums2rf only.
c
c           info (5): entries in a after adding duplicates and
c               removing invalid entries.
c
c           info (6): entries in diagonal blocks of a.
c
c           info (7): entries in off-diagonal blocks of a.  zero
c               if info (9) = 1.
c
c           info (8): 1-by-1 diagonal blocks.
c
c           info (9): blocks in block-triangular form.
c
c           info (10): entries below diagonal in l.
c
c           info (11): entries below diagonal in u.
c
c           info (12): entries in l+u+offdiagonal part.
c
c           info (13): frontal matrices.
c
c           info (14): garbage collections performed on index, when
c               memory is exhausted.  garbage collections are performed
c               to remove external fragmentation.  if info (14) is
c               excessively high, performance can be degraded.  try
c               increasing lindex if that occurs.  note that external
c               fragmentation in *both* index and value is removed when
c               either is exhausted.
c
c           info (15): garbage collections performed on value.
c
c           info (16): diagonal pivots chosen.
c
c           info (17): numerically acceptable pivots found in a.
c               if less than n, then a is singular (or nearly so).
c               the factorization still proceeds, and ums2so can still
c               be called.  the zero-rank active submatrix of order
c               n - info (17) is replaced with the identity matrix
c               (assuming btf is not in use).  if btf is in use, then
c               one or more of the diagonal blocks are singular.
c
c           info (18): memory used in index.
c
c           info (19): minimum memory needed in index
c               (or minimum recommended).  if lindex is set to
c               info (19) on a subsequent call, then a moderate
c               number of garbage collections (info (14)) will
c               occur.
c
c           info (20): memory used in value.
c
c           info (21): minimum memory needed in value
c               (or minimum recommended).  if lvalue is set to
c               info (21) on a subsequent call, then a moderate
c               number of garbage collections (info (15)) will
c               occur.
c
c           info (22): memory needed in index for the next call to
c               ums2rf.
c
c           info (23): memory needed in value for the next call to
c               ums2rf.
c
c           info (24): zero.  used by ums2so only.
c
c           info (25 ... 40): reserved for future releases
 
c           ------------------------------------------------------------
c  rinfo:   a real array of size 20.
c           need not be set by caller on input.  modified on output.
c           it contains information about the execution of ums2fa.
c
c           rinfo (1): total flop count in the blas
c
c           rinfo (2): total assembly flop count
c
c           rinfo (3): total flops during pivot search
c
c           rinfo (4): level-1 blas flops
c
c           rinfo (5): level-2 blas flops
c
c           rinfo (6): level-3 blas flops
c
c           rinfo (7): zero.  used by ums2so only.
c
c           rinfo (8): zero.  used by ums2so only.
c
c           rinfo (9 ... 20): reserved for future releases
 
c=======================================================================
c  to be preserved between calls to ums2fa, ums2rf, ums2so:
c=======================================================================
c
c  when calling ums2so to solve a linear system using the factors
c  computed by ums2fa or ums2rf, the following must be preserved:
c
c       n
c       value (keep (1)...keep (2))
c       index (keep (3)...keep (5))
c       keep (1 ... 20)
c
c  when calling ums2rf to factorize a subsequent matrix with a pattern
c  similar to that factorized by ums2fa, the following must be
c  preserved:
c
c       n
c       index (keep (4)...keep (5))
c       keep (4 ... 20)
c
c  note that the user may move the lu factors to a different position
c  in value and/or index, as long as keep (1 ... 5) are modified
c  correspondingly.
 
c## end of user documentation ##########################################
 
c=======================================================================
c  coding conventions:
c=======================================================================
c
c  this package is written in ansi fortran 77.  to make the code more
c  understandable, the following coding conventions are followed for all
c  routines in this package:
c
c  1) large code blocks are delimited with [...] comments.
c
c  2) goto usage:
c       a) goto's used to return if an error condition is found are
c          written as "go to 9000" or "go to 9010".
c       b) goto's used to exit loops prematurely are written as "go to",
c          and have a target label of 2000 or less.
c       c) goto's used to jump to the next iteration of a do loop or
c          while loop (or to implement a while loop) are written as
c          "goto".
c       no other goto's are used in this package.
c
c  this package uses the following cray compiler directives to help
c  in the vectorization of loops.  each of them operate on the
c  do-loop immediately following the directive.  other compilers
c  normally treat these directives as ordinary comments.
c
c       cfpp$ nodepchk l        disables data dependency check, and
c                               asserts that no recursion exists.
c       cfpp$ nolstval l        disables the saving of last values of
c                               transformed scalars (indexes or promoted
c                               scalars, especially those in array
c                               subscripts).  asserts that values do not
c                               need to be the same as in the scalar
c                               version (for later use of the scalars).
c       cdir$ shortloop         asserts that the loop count is always
c                               64 or less.
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   user routine
c       subroutines called:     ums2er, ums2p1, ums2co, ums2f0
c       functions called:       max, min
        intrinsic max, min
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer i, nz, lux1, lui1, iuse, xuse, luir1, nzoff, nblks,
     $          maxint, nmax, io, prl
        logical presrv
        real
     $          zero
        parameter (zero = 0.0)
 
c  printing control:
c  -----------------
c  io:      i/o unit for diagnostic messages
c  prl:     printing level
c
c  location of lu factors:
c  -----------------------
c  lux1:    real part of lu factors placed in value (lux1 ... lvalue)
c  lui1:    integer part of lu factors placed in index (lui1 ... lindex)
c  luir1:   index (luir1 ... lindex) must be preserved for ums2rf
c
c  memory usage:
c  -------------
c  iuse:    current memory usage in index
c  xuse:    current memory usage in value
c
c  matrix to factorize:
c  --------------------
c  nblks:   number of diagonal blocks (1 if btf not used)
c  nzoff:   entries in off-diagonal part (0 if btf not used)
c  nz:      entries in matrix after removing invalid/duplicate entries
c
c  other:
c  ------
c  maxint:  largest representable positive integer
c  nmax:    largest permissible value of n
c  i:       general loop index
c  presrv:  true if original matrix to be preserved
 
c=======================================================================
c  executable statements:
c=======================================================================
 
        io = icntl (2)
        prl = icntl (3)
 
c-----------------------------------------------------------------------
c  clear informational output, and keep array (except keep (6..8)):
c-----------------------------------------------------------------------
 
        do 10 i = 1, 40
           info (i) = 0
10      continue
        do 20 i = 1, 20
           rinfo (i) = zero
20      continue
        keep (1) = 0
        keep (2) = 0
        keep (3) = 0
        keep (4) = 0
        keep (5) = 0
 
c-----------------------------------------------------------------------
c  print input arguments if requested
c-----------------------------------------------------------------------
 
        call ums2p1 (1, 1,
     $          n, ne, job, transa, lvalue, lindex, value,
     $          index, keep, cntl, icntl, info, rinfo,
     $          zero, zero, 1, zero, 1)
 
c-----------------------------------------------------------------------
c  initialize and check inputs
c-----------------------------------------------------------------------
 
        iuse = 0
        xuse = 0
        info (5) = ne
        info (6) = ne
        maxint = keep (6)
        nmax = (maxint - 2) / 3
        if (n .lt. 1) then
c          n is too small
           call ums2er (1, icntl, info, -1, -1)
           go to 9000
        endif
        if (n .gt. nmax) then
c          n is too big
           call ums2er (1, icntl, info, -1, nmax)
           go to 9000
        endif
        if (ne .lt. 1) then
c          ne is too small
           call ums2er (1, icntl, info, -2, -1)
           go to 9000
        endif
 
c-----------------------------------------------------------------------
c  get memory for conversion to column form
c-----------------------------------------------------------------------
 
        nz = ne
        iuse = 2*n+1 + max (2*nz, n+1) + nz
        xuse = 2*nz
        info (18) = iuse
        info (20) = xuse
        info (19) = iuse
        info (21) = xuse
        if (lindex .lt. iuse) then
c          set error flag if out of integer memory:
           call ums2er (1, icntl, info, -3, iuse)
        endif
        if (lvalue .lt. xuse) then
c          set error flag if out of real memory:
           call ums2er (1, icntl, info, -4, xuse)
        endif
        if (info (1) .lt. 0) then
c          error return, if not enough integer and/or real memory:
           go to 9000
        endif
 
c-----------------------------------------------------------------------
c  convert to column-oriented form and remove duplicates
c-----------------------------------------------------------------------
 
        call ums2co (n, nz, transa, value, lvalue, info, icntl,
     $     index, lindex-(2*n+1), index(lindex-2*n), index(lindex-n), 1)
        if (info (1) .lt. 0) then
c          error return, if all entries invalid (nz is now 0):
           go to 9000
        endif
 
c-----------------------------------------------------------------------
c  current memory usage:
c-----------------------------------------------------------------------
 
c       index (1..n+1): column pointers.  input matrix is now in
c       index (1..nz+n+1) and value (1..nz)
c       col pattern: index (n+1+ index (col) ... n+1+ index (col+1))
c       col values:  value (     index (col) ...      index (col+1))
c       at this point, nz <= ne (nz = ne if there are no invalid or
c       duplicate entries; nz < ne otherwise).
 
        iuse = nz + (n+1)
        xuse = nz
 
c-----------------------------------------------------------------------
c  factorize
c-----------------------------------------------------------------------
 
        presrv = job .eq. 1
        if (presrv) then
 
c          -------------------------------------------------------------
c          keep a copy of the original matrix in column-oriented form
c          -------------------------------------------------------------
 
c          copy column pointers (cp (1..n+1) = ap (1..n+1))
           iuse = iuse + (n+1)
cfpp$ nodepchk l
           do 30 i = 1, n+1
              index (nz+n+1+i) = index (i)
30         continue
 
           call ums2f0 (n, nz, index (nz+n+2),
     $          value (nz+1), lvalue-nz,
     $          index (nz+2*n+3), lindex-(nz+2*n+2),
     $          lux1, lui1, iuse, xuse, nzoff, nblks,
     $          icntl, cntl, info, rinfo,
     $          presrv, index, index (n+2), value, n, nz, keep, ne)
           if (info (1) .lt. 0) then
c             error return, if ums2f0 fails
              go to 9000
           endif
c          adjust pointers to reflect index/value, not ii/xx:
           lux1 = lux1 + nz
           lui1 = lui1 + (nz+2*n+2)
 
c          move preserved copy of a to permanent place
           lux1 = lux1 - nz
           lui1 = lui1 - (nz+n+1)
           do 40 i = nz+n+1, 1, -1
              index (lui1+i-1) = index (i)
40         continue
           do 50 i = nz, 1, -1
              value (lux1+i-1) = value (i)
50         continue
 
        else
 
c          -------------------------------------------------------------
c          do not preserve the original matrix
c          -------------------------------------------------------------
 
           call ums2f0 (n, nz, index,
     $          value, lvalue,
     $          index (n+2), lindex-(n+1),
     $          lux1, lui1, iuse, xuse, nzoff, nblks,
     $          icntl, cntl, info, rinfo,
     $          presrv, 1, 1, zero, 0, 1, keep, ne)
           if (info (1) .lt. 0) then
c             error return, if ums2f0 fails
              go to 9000
           endif
c          adjust pointers to reflect index/value, not ii/xx:
           lui1 = lui1 + (n+1)
        endif
 
c-----------------------------------------------------------------------
c  wrap-up
c-----------------------------------------------------------------------
 
        if (transa) then
           index (lindex-6) = 1
        else
           index (lindex-6) = 0
        endif
        index (lindex-5) = nzoff
        index (lindex-4) = nblks
        if (presrv) then
           index (lindex-3) = 1
        else
           index (lindex-3) = 0
        endif
        index (lindex-2) = nz
        index (lindex-1) = n
        index (lindex) = ne
 
c       do not need preserved matrix (n+1+nz), or off-diagonal entries
c       (nzoff) for ums2rf:
        luir1 = lui1
        if (presrv) then
c          do not need preserved matrix for ums2rf
           luir1 = luir1 + n+1 + nz
        endif
        if (nblks .gt. 1) then
c          do not need off-diagonal part for ums2rf
           luir1 = luir1 + nzoff
        endif
 
c       save location of lu factors
        keep (1) = lux1
        keep (2) = lvalue
        keep (3) = lui1
        keep (4) = luir1
        keep (5) = lindex
 
c       update memory usage information
        iuse = lindex - lui1 + 1
        xuse = lvalue - lux1 + 1
        info (22) = info (22) + (lindex - luir1 + 1)
 
c-----------------------------------------------------------------------
c  print the output arguments if requested, and return
c-----------------------------------------------------------------------
 
c       error return label:
9000    continue
        if (info (1) .lt. 0) then
           keep (1) = 0
           keep (2) = 0
           keep (3) = 0
           keep (4) = 0
           keep (5) = 0
        endif
 
        info (18) = min (lindex, max (info (18), iuse))
        info (20) = min (lvalue, max (info (20), xuse))
 
        call ums2p1 (1, 2,
     $          n, ne, job, transa, lvalue, lindex, value,
     $          index, keep, cntl, icntl, info, rinfo,
     $          zero, zero, 1, zero, 1)
        return
        end
 
        subroutine ums2fb (xx, xsize, ii, isize, n, nz, nzdia, nzoff,
     $          nblks, cp, cperm, rperm, pr, pc,
     $          w, zperm, bp, offp,
     $          presrv, icntl)
        integer n, nz, isize, ii (isize), nzdia, nzoff, nblks, cp (n+1),
     $          cperm (n), rperm (n), pr (n), pc (n), w (n), zperm (n),
     $          bp (n+1), offp (n+1), icntl (20), xsize
        logical presrv
        real
     $          xx (xsize)
 
c=== ums2fb ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  find permutations to block triangular form:
c       1) permute the matrix so that it has a zero-free diagonal.
c       2) find strongly-connected components of the corresponding
c          graph.  each diagonal block corresponds to exactly one
c          strongly-connected component.
c       3) convert the matrix to block triangular form, unless it is
c          to be preserved in its original form.
 
c  calls harwell ma28 routines mc21b and mc13e, which can be obtained
c  separately from netlib.  send email to netlib@ornl.gov with the
c  message:
c       send mc13e.f mc21b.f from harwell
 
c=======================================================================
c  installation note:
c=======================================================================
c
c  if the ma28 harwell subroutine library routines mc21b and mc13e
c  (which perform the permutation to block-triangular-form) are not
c  available, then you may comment out all executable code in this
c  routine, or place a "return" statement as the first executable
c  statement (see below).  if you do make this modification, please do
c  not delete any original code.  add a comment and date to your
c  modifications.
 
c  sept. 14, 1995:  changed non-ansi "60 enddo" statement to
c       "60 continue".
 
c=======================================================================
c  input:
c=======================================================================
c
c       presrv:         true if original matrix is to be preserved
c       n:              order of matrix
c       nz:             entries in matrix
c       isize:          size of ii
c       xsize:          size of xx
c       cp (1..n+1):    column pointers
c       xx (1..nz):     values
c       ii (1..nz):     row indices
c       icntl:          integer control arguments
c
c          input matrix in column form is in:
c          xx (1..nz), ii (1..nz), n, nz, cp (1..n+1), where
c               ii (cp(col) ... cp(col+1)-1): row indices
c               xx (cp(col) ... cp(col+1)-1): values
c          if presrv is false then xsize and isize must be >= 2*nz
c          otherwise, xsize and isize must be >= nz
 
c=======================================================================
c  workspace:
c=======================================================================
c
c       pr (1..n), pc (1..n), w (1..n), zperm (1..n)
 
c======================================================================
c  output:
c=======================================================================
c
c       nblks: number of blocks
c       if (nblks > 1):
c
c           cperm (1..n), rperm (1..n): permutation to block form:
c               rperm (newrow) = oldrow
c               cperm (newcol) = oldcol
c
c           bp (n-nblks+1...n+1) holds the start/end of blocks 1..nblks
c
c           if (presrv is false) then
c
c              input matrix is converted to block-upper-tri. form,
c              using ii/xx (nz+1..2*nz) as workspace.
c              nzdia: nonzeros in diagonal blocks
c              nzoff: nonzeros in off-diagonal blocks
c              (nz = nzdia + nzoff)
c
c              off-diagonal column-oriented form in xx/ii (1..nzoff)
c              col is located in
c              xx/ii (offp (col) ... offp (col+1)-1)
c
c              diagonal blocks now in xx/ii (nzoff+1 .. nzoff+nzdia)
c              col is located in
c              xx/ii (cp (col) ... cp (col+1)-1)
c
c       else, nblks=1: and no other output is generated.
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2f0
c       subroutines called:     mc21b, mc13e (in ma28 hsl package)
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer col, ndiag, i, po, pb, blk, p, row, k1, k
 
c  ndiag:   number of entries on the diagonal
c  po:      pointer into off-diagonal part
c  pb:      pointer into diagonal blocks
c  blk:     block number for current diagonal block
c  k1:      column col is in diagonal block a (k1.., k1...)
c  k:       kth row/col in btf form is rperm(k)/cperm(k) in input matrix
c  p:       pointer
c  row:     row index
c  col:     column index
c  i:       general loop index
 
c=======================================================================
c  executable statements:
c       if (mc21b and mc13e not available during installation) return
c=======================================================================
 
        nzdia = nz
        nzoff = 0
 
c-----------------------------------------------------------------------
c compute the length of each column
c-----------------------------------------------------------------------
 
        do 10 col = 1, n
           w (col) = cp (col+1) - cp (col)
10      continue
 
c-----------------------------------------------------------------------
c find a column permutation for a zero-free diagonal
c-----------------------------------------------------------------------
 
        call mc21b (n, ii, nz, cp, w, zperm, ndiag, offp, cperm, pr, pc)
c          mc21b calling interface:
c          input:       n, ii (1..nz), nz, cp (n), w (n):
c                       n-by-n matrix, col is of length w (col),
c                       and its pattern is located in
c                       ii (cp (col) ... cp (col)+w(col)-1)
c          output:      zperm (n), the permutation, such that
c                       colold = zperm (col), and ndiag (number of
c                       structural nonzeros on the diagonal.
c                       matrix is structurally singular if ndiag < n
c          workspace:   offp, cperm, pr, pc
 
c-----------------------------------------------------------------------
c  permute the columns of the temporary matrix to get zero-free diagonal
c-----------------------------------------------------------------------
 
        do 20 col = 1, n
           offp (col) = cp (zperm (col))
           w (col) = cp (zperm (col)+1) - cp (zperm (col))
20      continue
 
c-----------------------------------------------------------------------
c  find a symmetric permutation into upper block triangular form
c  (that is, find the strongly-connected components in the graph).
c-----------------------------------------------------------------------
 
        call mc13e (n, ii, nz, offp, w, rperm, bp, nblks, cperm, pr, pc)
c          mc13e calling interface:
c          input:       n, ii (1..nz), nz, offp (n), w (n)
c                       n-by-n matrix, col of length w(col),
c                       in ii (offp(col) ... offp(col)+w(col)-1), where
c                       this permuted matrix has a zero-free diagonal
c                       (unless the matrix is structurally singular).
c          output:      rperm (n), bp (n+1), nblks
c                       old = rperm (new) is the symmetric permutation,
c                       there are nblks diagonal blocks, bp (i) is
c                       the position in new order of the ith block.
c          workspace:   cperm, pr, pc
 
c-----------------------------------------------------------------------
c  if more than one block, get permutations and block pointers,
c  and convert to block-upper-triangular form (unless matrix preserved)
c-----------------------------------------------------------------------
 
        if (nblks .ne. 1) then
 
c          -------------------------------------------------------------
c          find the composite column permutation vector (cperm):
c          -------------------------------------------------------------
 
           do 30 col = 1, n
              cperm (col) = zperm (rperm (col))
30         continue
 
c          -------------------------------------------------------------
c          convert to block-upper-triangular form, if not preserved
c          -------------------------------------------------------------
 
           if (.not. presrv) then
 
c             ----------------------------------------------------------
c             find the inverse permutation vectors, pr and pc
c             ----------------------------------------------------------
 
              do 40 k = 1, n
                 pc (cperm (k)) = k
                 pr (rperm (k)) = k
40            continue
 
c             ----------------------------------------------------------
c             construct flag array to determine if entry in block or not
c             ----------------------------------------------------------
 
              bp (nblks+1) = n+1
              do 60 blk = 1, nblks
                 do 50 i = bp (blk), bp (blk+1)-1
                    w (i) = bp (blk)
50               continue
60            continue
 
c             ----------------------------------------------------------
c             construct block-diagonal form in xx/ii (nz+1..nz+nzdia)
c             ----------------------------------------------------------
 
c             these blocks are in a permuted order (according to rperm
c             and cperm).  the row indices in each block range from 1
c             to the size of the block.
 
              pb = nz + 1
              do 80 col = 1, n
                 zperm (col) = pb
                 k1 = w (col)
cfpp$ nodepchk l
                 do 70 p = cp (cperm (col)), cp (cperm (col)+1)-1
                    row = pr (ii (p))
                    if (w (row) .eq. k1) then
c                      entry is in the diagonal block:
                       ii (pb) = row - k1 + 1
                       xx (pb) = xx (p)
                       pb = pb + 1
                    endif
70               continue
80            continue
c             zperm (n+1) == pb  ( but zperm (n+1) does not exist )
              nzdia = pb - (nz + 1)
              nzoff = nz - nzdia
 
c             diagonal blocks now in xx/ii (nz+1..nz+nzdia)
c             col is located in xx/ii (zperm (col) ... zperm (col+1)-1)
 
c             ----------------------------------------------------------
c             compress original matrix to off-diagonal part, in place
c             ----------------------------------------------------------
 
c             the rows/cols of off-diagonal form correspond to rows/cols
c             in the original, unpermuted matrix.  they are permuted to
c             the final pivot order and stored in a row-oriented form,
c             after the factorization is complete (by ums2of).
 
              po = 1
              do 100 col = 1, n
                 offp (col) = po
                 k1 = w (pc (col))
cfpp$ nodepchk l
                 do 90 p = cp (col), cp (col+1)-1
                    row = pr (ii (p))
                    if (w (row) .ne. k1) then
c                      offdiagonal entry
                       ii (po) = ii (p)
                       xx (po) = xx (p)
                       po = po + 1
                    endif
90               continue
100           continue
              offp (n+1) = po
 
c             off-diagonal form now in xx/ii (1..nzoff)
c             col is located in xx/ii(offp(col)..offp(col+1)-1)
 
c             ----------------------------------------------------------
c             move block-diagonal part into place
c             ----------------------------------------------------------
 
              pb = nz + 1
cfpp$ nodepchk l
              do 110 i = 0, nzdia - 1
                 ii (po+i) = ii (pb+i)
                 xx (po+i) = xx (pb+i)
110           continue
              do 120 col = 1, n
                 cp (col) = zperm (col) - nzdia
120           continue
c             cp (n+1) == nz+1  ( this is unchanged )
 
c             diagonal blocks now in xx/ii (nzoff+1 .. nzoff+nzdia)
c             col is located in xx/ii (cp (col) ... cp (col+1)-1)
 
           endif
 
c          -------------------------------------------------------------
c          shift bp (1 .. nblks+1) down to bp (1+n-nblks .. n+1), which
c          then becomes the blkp (1 .. nblks+1) array.
c          -------------------------------------------------------------
 
           bp (nblks+1) = n+1
cfpp$ nodepchk l
           do 130 blk = nblks + 1, 1, -1
              bp (blk + (n-nblks)) = bp (blk)
130        continue
        endif
 
        return
        end
 
        subroutine ums2fg (xx, xsize, xhead, xtail, xuse,
     $          ii, isize, ihead, itail, iuse,
     $          cp, rp, dn, n, icntl, wir, wic, wr, wc,
     $          ffxp, ffsize, wxp, ffdimc, doslot,
     $          pfree, xfree, mhead, mtail, slots)
        integer n, dn, isize, ii (isize), ihead, itail, rp (n+dn),
     $          cp (n+1), icntl (20), wir (n), wic (n), xsize, xuse,
     $          iuse, xhead, xtail, ffxp, ffsize, wxp,
     $          ffdimc, wr (n+dn), wc (n+dn), pfree, xfree, mhead,
     $          mtail, slots
        logical doslot
        real
     $          xx (xsize)
 
c=== ums2fg ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  garbage collection for ums2f2.
 
c=======================================================================
c  input:
c=======================================================================
c
c       ii/xx:          integer/real workspace, containing matrix being
c                       factorized and partially-computed lu factors
c       isize:          size of ii
c       xsize:          size of xx
c       xhead:          xx (1..xhead) is in use (matrix, frontal mtc's)
c       xtail:          xx (xtail..xsize) is in use (lu factors)
c       xuse:           memory usage in value
c       ihead:          ii (1..ihead) is in use (matrix, frontal mtc's)
c       itail:          ii (itail..isize) is in use (lu factors)
c       iuse:           memory usage in index
c       cp (1..n+1):    pointers to columns
c       rp (1..n+dn):   pointers to rows, frontal matrices, and lu
c                       arrowheads
c       dn:             number of dense columns
c       n:              order of matrix
c       icntl:          integer control parameters, see ums2in
c       wr (1..n):      see ums2f2
c       wc (1..n):      see ums2f2
c       ffxp:           pointer to current contribution block
c       ffsize:         size of current contribution block
c       mhead:          pointer to first block in memory list
c       mtail:          pointer to last block in memory list
c       doslot:         true if adding slots
c       if doslot:
c           wir (1..n)  if wir (row) >= 0 then add (or keep) an extra
c                       slot in the row's element list
c           wic (1..n)  if wir (col) >= 0 then add (or keep) an extra
c                       slot in the col's element list
 
c=======================================================================
c  output:
c=======================================================================
c
c       ii/xx:          external fragmentation is removed at head
c       xhead:          xx (1..xhead) is in use, reduced in size
c       xuse:           memory usage in value, reduced
c       ihead:          ii (1..ihead) is in use, reduced in size
c       iuse:           memory usage in index, reduced
c       pfree:          pointer to free block in memory list, set to 0
c       xfree:          size of free block in xx, set to -1
c       mhead:          pointer to first block in memory list
c       mtail:          pointer to last block in memory list
c       ffxp            current working array has been shifted
c       wxp             current work vector has been shifted
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2f2
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer what, fsiz, row, col, p, idp, xdp, i, e, ep, fdimc,
     $          ludegr, ludegc, j, pc, celn, clen, reln, rlen,
     $          csiz1, csiz2, rsiz1, rsiz2, fluip, cxp, fxp, rdeg,
     $          cdeg, cscal, rscal, fscal
        parameter (cscal = 9, rscal = 2, fscal = 7)
        logical slot
 
c  compression:
c  ------------
c  what:    what this block of memory is (a row, column, etc.)
c  idp:     int. destination pointer, current block moved to ii (idp...)
c  xdp:     real destination pointer, current block moved to xx (xdp...)
c  slot:    true if adding, or keeping, a size-2 hole in an element list
c
c  columns:
c  --------
c  cscal:   = 9, the number of scalars in column data structure
c  celn:    number of (e,f) tuples in element list of a column
c  clen:    number of unassembled original entries in a column
c  cdeg:    degree of a column (number of entries, including fill-in)
c  cxp:     a column is in xx (cxp...) prior to compression
c  pc:      column is in ii (pc ...) prior to compression
c  csiz1:   size of a column in ii, prior to compression
c  csiz2:   size of a column in ii, after compression
c  col:     column index
c
c  rows:
c  -----
c  rscal:   = 2, the number of scalars in row data structure
c  reln:    number of (e,f) tuples in element list of a row
c  rlen:    number of unassembled original entries in a row
c  rsiz1:   size of a row in ii, prior to compression
c  rsiz2:   size of a row in ii, after compression
c  rdeg:    degree of a row (number of entries, including fill-in)
c  row:     row index
c
c  frontal matrices:
c  -----------------
c  fscal:   = 7, the number of scalars in element data structure
c  fluip:   element is in ii (fluip...) prior to compression
c  fxp:     a frontal matrix is in xx (fxp...) prior to compression
c  e:       an element
c  fdimc:   column dimension (number of rows) of a frontal matrix
c  ludegr:  row degree (number of columns) of a contribution block
c  ludegc:  column degree (number of rows) of a contribution block
c  fsiz:    size of an artificial frontal matrix
c  ep:      an artificial frontal matrix is in ii (ep ...) prior to comp
c
c  other:
c  ------
c  p:       pointer
c  i:       general loop index
c  j:       general loop index
 
c=======================================================================
c  executable statments:
c=======================================================================
 
        slots = 0
 
c-----------------------------------------------------------------------
c   prepare the non-pivotal rows/cols and unassembled elements
c-----------------------------------------------------------------------
 
c       place the size of each block of memory at the beginning,
c       and mark the 2nd entry in each block with what it is
 
c       ----------------------------------------------------------------
c       mark the columns
c       ----------------------------------------------------------------
 
cfpp$ nodepchk l
        do 10 col = 1, n
           pc = cp (col)
           if (pc .ne. 0) then
c             this is a non-pivotal, non-null column
              cdeg = ii (pc+1)
              cp (col) = cdeg
              ii (pc+1) = col+n
           endif
10      continue
 
c       ----------------------------------------------------------------
c       mark the rows and frontal matrices
c       ----------------------------------------------------------------
 
cfpp$ nodepchk l
        do 20 row = 1, n
           p = rp (row)
           rlen = wc (row)
           if (p .eq. 0) then
c             a pivotal row
              continue
           else if (rlen .ge. 0 .and. rlen .le. n) then
c             this is a non-pivotal, non-null row
              rdeg = ii (p+1)
              rp (row) = rdeg
              ii (p+1) = row+2*n
           else if (wr (row) .eq. -(n+dn+2)) then
c             a pivotal row, and an assembled element
              continue
           else
c             this is an unassembled frontal matrix
c             the size is implicitly fscal
              fdimc = ii (p+1)
              rp (row) = fdimc
              ii (p+1) = row
           endif
20      continue
 
c       ----------------------------------------------------------------
c       mark the artificial frontal matrices
c       ----------------------------------------------------------------
 
cfpp$ nodepchk l
        do 30 e = n+1, n+dn
           ep = rp (e)
           if (ep .ne. 0) then
c             this is an unassembled artificial frontal matrix
c             the size is ii (ep+1) + cscal
              fdimc = ii (ep+1)
              rp (e) = fdimc
              ii (ep+1) = e+2*n
           endif
30      continue
 
c-----------------------------------------------------------------------
c  scan the link list and compress the reals
c-----------------------------------------------------------------------
 
        xdp = 1
        p = mhead
c       while (p .ne. 0) do
40      continue
        if (p .ne. 0) then
 
           what = ii (p+1)
 
c          -------------------------------------------------------------
           if (what .gt. 3*n) then
c          -------------------------------------------------------------
 
c             this is an unassembled artificial frontal matrix
              e = what - 2*n
              fxp = ii (p+2)
              ii (p+2) = xdp
cfpp$ nodepchk l
              do 50 j = 0, rp (e) - 1
                 xx (xdp+j) = xx (fxp+j)
50            continue
              xdp = xdp + rp (e)
 
c          -------------------------------------------------------------
           else if (what .eq. -1 .or. ii (p+6) .eq. 0) then
c          -------------------------------------------------------------
 
c             this is a real hole - delete it from the link list
              if (ii (p+4) .ne. 0) then
                 ii (ii (p+4)+3) = ii (p+3)
              else
                 mhead = ii (p+3)
              endif
              if (ii (p+3) .ne. 0) then
                 ii (ii (p+3)+4) = ii (p+4)
              else
                 mtail = ii (p+4)
              endif
 
c          -------------------------------------------------------------
           else if (what .le. n) then
c          -------------------------------------------------------------
 
c             this is an unassembled frontal matrix
              e = what
              fxp = ii (p+2)
              ii (p+2) = xdp
              fluip = ii (p)
              ludegr = ii (fluip+2)
              ludegc = ii (fluip+3)
              fdimc = rp (e)
              if (fdimc .eq. ludegc) then
c                contribution block is already compressed
cfpp$ nodepchk l
                 do 60 i = 0, (ludegr * ludegc) - 1
                    xx (xdp+i) = xx (fxp+i)
60               continue
              else
c                contribution block is not compressed
c                compress xx (fxp..) to xx (xdp..xdp+(ludegr*ludegc)-1)
                 do 80 j = 0, ludegr - 1
cfpp$ nodepchk l
                    do 70 i = 0, ludegc - 1
                       xx (xdp + j*ludegc + i) = xx (fxp + j*fdimc + i)
70                  continue
80               continue
                 rp (e) = ludegc
              endif
              xdp = xdp + ludegr*ludegc
 
c          -------------------------------------------------------------
           else if (what .le. 2*n) then
c          -------------------------------------------------------------
 
c             this is a column
              cxp = ii (p+2)
              ii (p+2) = xdp
              clen = ii (p+6)
cfpp$ nodepchk l
              do 90 j = 0, clen - 1
                 xx (xdp+j) = xx (cxp+j)
90            continue
              xdp = xdp + clen
 
c          -------------------------------------------------------------
           endif
c          -------------------------------------------------------------
 
c          -------------------------------------------------------------
c          get the next item in the link list
c          -------------------------------------------------------------
 
           p = ii (p+3)
 
c       end while:
        goto 40
        endif
 
        pfree = 0
        xfree = -1
 
c       ----------------------------------------------------------------
c       shift the current working array (if it exists)
c       ----------------------------------------------------------------
 
        if (ffxp .ne. 0) then
cfpp$ nodepchk l
           do 100 i = 0, ffsize - 1
              xx (xdp+i) = xx (ffxp+i)
100        continue
           ffxp = xdp
           xdp = xdp + ffsize
        endif
 
c       ----------------------------------------------------------------
c       shift the current work vector (if it exists)
c       ----------------------------------------------------------------
 
        if (wxp .ne. 0) then
           wxp = xdp
           xdp = xdp + ffdimc
        endif
 
c-----------------------------------------------------------------------
c  scan from the top of integer memory (1) to bottom (ihead) and
c  compress the integers
c-----------------------------------------------------------------------
 
        p = 1
        idp = p
c       while (p .lt. ihead) do:
110     continue
        if (p .lt. ihead) then
 
           what = ii (p+1)
 
c          -------------------------------------------------------------
           if (what .gt. 3*n) then
c          -------------------------------------------------------------
 
c             this is an unassembled artificial frontal matrix
              e = what - 2*n
              fsiz = rp (e) + cscal
              ii (p+1) = rp (e)
              rp (e) = idp
cfpp$ nodepchk l
              do 120 i = 0, fsiz - 1
                 ii (idp+i) = ii (p+i)
120           continue
c             shift pointers in the link list
              if (ii (idp+4) .ne. 0) then
                 ii (ii (idp+4)+3) = idp
              else
                 mhead = idp
              endif
              if (ii (idp+3) .ne. 0) then
                 ii (ii (idp+3)+4) = idp
              else
                 mtail = idp
              endif
              p = p + fsiz
              idp = idp + fsiz
 
c          -------------------------------------------------------------
           else if (what .eq. -1) then
c          -------------------------------------------------------------
 
c             this is a integer hole
              p = p + ii (p)
 
c          -------------------------------------------------------------
           else if (what .ge. 1 .and. what .le. n) then
c          -------------------------------------------------------------
 
c             this is an unassembled frontal matrix (fscal integers)
              e = what
              fdimc = rp (e)
              ii (p+1) = fdimc
              rp (e) = idp
cfpp$ nodepchk l
              do 130 i = 0, fscal - 1
                 ii (idp+i) = ii (p+i)
130           continue
c             shift pointers in the link list
              if (ii (idp+4) .ne. 0) then
                 ii (ii (idp+4)+3) = idp
              else
                 mhead = idp
              endif
              if (ii (idp+3) .ne. 0) then
                 ii (ii (idp+3)+4) = idp
              else
                 mtail = idp
              endif
              p = p + fscal
              idp = idp + fscal
 
c          -------------------------------------------------------------
           else if (what .le. 2*n) then
c          -------------------------------------------------------------
 
c             this is a non-pivotal column
              csiz1 = ii (p)
              col = what - n
              celn = ii (p+5)
              clen = ii (p+6)
              csiz2 = 2*celn + clen + cscal
              slot = doslot .and. wic (col) .ge. 0 .and. p .ge. idp+2
              if (slot) then
c                keep (or make) one extra slot for element list growth
                 csiz2 = csiz2 + 2
                 slots = slots + 2
              endif
              cdeg = cp (col)
              ii (p+1) = cdeg
              cp (col) = idp
              ii (p) = csiz2
c             copy the cscal scalars and the celn (e,f) tuples
cfpp$ nodepchk l
              do 140 i = 0, cscal + 2*celn - 1
                 ii (idp+i) = ii (p+i)
140           continue
              if (clen .gt. 0) then
c                shift pointers in the link list
                 if (ii (idp+4) .ne. 0) then
                    ii (ii (idp+4)+3) = idp
                 else
                    mhead = idp
                 endif
                 if (ii (idp+3) .ne. 0) then
                    ii (ii (idp+3)+4) = idp
                 else
                    mtail = idp
                 endif
              endif
              p = p + csiz1 - clen
              idp = idp + cscal + 2*celn
              if (slot) then
c                skip past the slot
                 idp = idp + 2
              endif
c             copy the clen original row indices
cfpp$ nodepchk l
              do 150 i = 0, clen - 1
                 ii (idp+i) = ii (p+i)
150           continue
              p = p + clen
              idp = idp + clen
 
c          -------------------------------------------------------------
           else
c          -------------------------------------------------------------
 
c             this is a non-pivotal row
              rsiz1 = ii (p)
              row = what - 2*n
              reln = wr (row)
              rlen = wc (row)
              rsiz2 = 2*reln + rlen + rscal
              slot = doslot .and. wir (row) .ge. 0 .and. p .ge. idp+2
              if (slot) then
c                keep (or make) one extra slot for element list growth
                 rsiz2 = rsiz2 + 2
                 slots = slots + 2
              endif
              rdeg = rp (row)
              ii (p+1) = rdeg
              rp (row) = idp
              ii (p) = rsiz2
c             copy the rscal scalars, and the reln (e,f) tuples
cfpp$ nodepchk l
              do 160 i = 0, rscal + 2*reln - 1
                 ii (idp+i) = ii (p+i)
160           continue
              p = p + rsiz1 - rlen
              idp = idp + rscal + 2*reln
              if (slot) then
c                skip past the slot
                 idp = idp + 2
              endif
c             copy the rlen original column indices
cfpp$ nodepchk l
              do 170 i = 0, rlen - 1
                 ii (idp+i) = ii (p+i)
170           continue
              p = p + rlen
              idp = idp + rlen
 
c          -------------------------------------------------------------
           endif
c          -------------------------------------------------------------
 
c          -------------------------------------------------------------
c          move to the next block
c          -------------------------------------------------------------
 
c       end while:
        goto 110
        endif
 
c-----------------------------------------------------------------------
c  deallocate the unused space
c-----------------------------------------------------------------------
 
        iuse = iuse - (ihead - idp)
        ihead = idp
        xuse = xuse - (xhead - xdp)
        xhead = xdp
        return
        end
 
        subroutine ums2in (icntl, cntl, keep)
        integer icntl (20), keep (20)
        real
     $          cntl (10)
 
c=== ums2in ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  initialize user-controllable parameters to default values, and
c  non-user controllable parameters.  this routine is normally
c  called once prior to any call to ums2fa.
c
c  this routine sets the default control parameters.  we recommend
c  changing these defaults under certain circumstances:
c
c  (1) if you know that your matrix has nearly symmetric nonzero
c       pattern, then we recommend setting icntl (6) to 1 so that
c       diagonal pivoting is preferred.  this can have a significant
c       impact on the performance for matrices that are essentially
c       symmetric in pattern.
c
c   (2) if you know that your matrix is not reducible to block
c       triangular form, then we recommend setting icntl (4) to 0
c       so that umfpack does not try to permute the matrix to block
c       triangular form (it will not do any useful work and will
c       leave the matrix in its irreducible form).  the work saved
c       is typically small, however.
c
c   the other control parameters typically have less effect on overall
c   performance.
 
c=======================================================================
c  installation note:
c=======================================================================
c
c  this routine can be modified on installation to reflect the computer
c  or environment in which this package is installed (printing control,
c  maximum integer, block size, and machine epsilon in particular).  if
c  you, the installer, modify this routine, please comment out the
c  original code, and add comments (with date) to describe the
c  installation.  do not delete any original code.
 
c=======================================================================
c  arguments:
c=======================================================================
 
c               --------------------------------------------------------
c  icntl:       an integer array of size 20.  need not be set by
c               caller on input.  on output, it contains default
c               integer control parameters.
c
c  icntl (1):   fortran output unit for error messages.
c               default: 6
c
c  icntl (2):   fortran output unit for diagnostic messages.
c               default: 6
c
c  icntl (3):   printing-level.
c               0 or less: no output
c               1: error messages only
c               2: error messages and terse diagnostics
c               3: as 2, and print first few entries of all input and
c                       output arguments.  invalid and duplicate entries
c                       are printed.
c               4 or more: as 2, and print all entries of all input and
c                       output arguments.  invalid and duplicate entries
c                       are printed.
c               default: 2
c
c  icntl (4):   whether or not to attempt a permutation to block
c               triangular form.  if nonzero, then attempt the
c               permutation.  if you know the matrix is not reducible
c               to block triangular form, then setting icntl (4) to
c               zero can save a small amount of computing time.
c               default: 1 (attempt the permutation)
c
c  icntl (5):   the number of columns to examine during the global
c               pivot search.  a value less than one is treated as one.
c               default: 4
c
c  icntl (6):   if not equal to zero, then pivots from the diagonal
c               of a (or the diagonal of the block-triangular form) are
c               preferred.  if the nonzero pattern of the matrix is
c               basically symmetric, we recommend that you change this
c               default value to 1 so that pivots on the diagonal
c               are preferred.
c               default: 0 (do not prefer the diagonal)
c
c  icntl (7):   block size for the blas, controlling the tradeoff
c               between the level-2 and level-3 blas.  values less than
c               one are treated as one.
c               default: 16, which is suitable for the cray ymp.
c
c  icntl (8):   number of steps of iterative refinement to perform.
c               values less than zero are treated as zero.  the matrix
c               must be preserved for iterative refinement to be done
c               (job=1 in ums2fa or ums2rf).
c               default: 0  (no iterative refinement)
c
c  icntl (9 ... 20):  set to zero.  reserved for future releases.
 
c               --------------------------------------------------------
c  cntl:        a real array of size 10.
c               need not be set by caller on input.  on output, contains
c               default real control parameters.
c
c  cntl (1):    pivoting tradeoff between sparsity-preservation
c               and numerical stability.  an entry a(k,k) is numerically
c               acceptable if:
c                  abs (a(k,k)) >= cntl (1) * max (abs (a(*,k)))
c               values less than zero are treated as zero (no numerical
c               constraints).  values greater than one are treated as
c               one (partial pivoting with row interchanges).
c               default: 0.1
c
c  cntl (2):    amalgamation parameter.  if the first pivot in a
c               frontal matrix has row degree r and column degree c,
c               then a working array of size
c                  (cntl (2) * c) - by - (cntl (2) * r)
c               is allocated for the frontal matrix.  subsequent pivots
c               within the same frontal matrix must fit within this
c               working array, or they are not selected for this frontal
c               matrix.  values less than one are treated as one (no
c               fill-in due to amalgamation).  some fill-in due to
c               amalgamation is necessary for efficient use of the blas
c               and to reduce the assembly operations required.
c               default: 2.0
c
c  cntl (3):    normally not modified by the user.
c               defines the smallest positive number,
c               epsilon = cntl (3), such that fl (1.0 + epsilon)
c               is greater than 1.0 (fl (x) is the floating-point
c               representation of x).  if the floating-point mantissa
c               is binary, then cntl (3) is 2 ** (-b+1), where b
c               is the number of bits in the mantissa (including the
c               implied bit, if applicable).
c
c               typical defaults:
c               for ieee double precision, cntl (3) = 2 ** (-53+1)
c               for ieee single precision, cntl (3) = 2 ** (-24+1)
c               for cray double precision, cntl (3) = 2 ** (-96+1)
c               for cray single precision, cntl (3) = 2 ** (-48+1)
c
c               a value of cntl (3) less than or equal to zero
c               or greater than 2 ** (-15) is treated as 2 ** (-15),
c               which assumes that any floating point representation
c               has at least a 16-bit mantissa.  cntl (3) is only
c               used in ums2s2 to compute the sparse backward error
c               estimates, rinfo (7) and rinfo (8), when
c               icntl (8) > 0 (the default is icntl (8) = 0,
c               so by default, cntl (3) is not used).
c
c  cntl (4 ... 10):  set to zero.  reserved for future releases.
 
c               --------------------------------------------------------
c  keep:        an integer array of size 20.
c               need not be set by the caller.  on output, contains
c               integer control parameters that are (normally) non-user
c               controllable (but can of course be modified by the
c               "expert" user or library installer).
c
c  keep (1 ... 5):  unmodified (see ums2fa or ums2rf for a description).
c
c  keep (6):    largest representable positive integer.  set to
c               2^31 - 1 = 2147483647 for 32-bit machines with 2's
c               complement arithmetic (the usual case).
c               default: 2147483647
c
c  keep (7) and keep (8): a column is treated as "dense" if
c               it has more than
c               max (0, keep(7), keep(8)*int(sqrt(float(n))))
c               original entries.  "dense" columns are treated
c               differently that "sparse" rows and columns.  dense
c               columns are transformed into a priori contribution
c               blocks of dimension cdeg-by-1, where cdeg is the number
c               of original entries in the column.  modifying these two
c               parameters can change the pivot order.
c               default:  keep (7) = 64
c               default:  keep (8) = 1
c
c  keep (9 ... 20):  set to zero.  reserved for future releases.
 
c## end of user documentation ##########################################
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   user routine
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer i
        real
     $          zero, tenth, two
        parameter (tenth = 0.1, two = 2.0, zero = 0.0)
 
c  i:       loop index
 
c=======================================================================
c  executable statments:
c=======================================================================
 
c       ----------------------------------------------------------------
c       integer control parameters:
c       ----------------------------------------------------------------
 
        icntl (1) = 6
        icntl (2) = 6
        icntl (3) = 2
        icntl (4) = 1
        icntl (5) = 4
        icntl (6) = 0
        icntl (7) = 16
        icntl (8) = 0
 
c       icntl (9 ... 20) is reserved for future releases:
        do 10 i = 9, 20
           icntl (i) = 0
10      continue
 
c       ----------------------------------------------------------------
c       real control parameters:
c       ----------------------------------------------------------------
 
        cntl (1) = tenth
        cntl (2) = two
 
c       ieee single precision:  epsilon = 2 ** (-24)
        cntl (3) = two ** (-23)
 
c       cntl (4 ... 10) is reserved for future releases:
        do 30 i = 4, 10
           cntl (i) = zero
30      continue
 
c       ----------------------------------------------------------------
c       integer control parameters in keep:
c       ----------------------------------------------------------------
 
        keep (6) = 2147483647
        keep (7) = 64
        keep (8) = 1
 
c       keep (9 ... 20) is reserved for future releases:
        do 20 i = 9, 20
           keep (i) = 0
20      continue
 
        return
        end
 
        subroutine ums2lt (nlu, npiv, n, lup, lui, lux, x, w)
        integer nlu, npiv, n, lup (nlu), lui (*)
        real
     $          lux (*), x (n), w (n)
 
c=== ums2lt ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  solves l'x = b, where l is the lower triangular factor of a matrix
c  (if btf not used) or a single diagonal block (if btf is used).
c  b is overwritten with the solution x.
 
c=======================================================================
c  input:
c=======================================================================
c
c       nlu:            number of lu arrowheads in the lu factors
c       npiv:           number of pivots found (normally n)
c       n:              order of matrix
c       lup (1..nlu):   pointer to lu arrowheads in lui
c       lui ( ... ):    integer values of lu arrowheads
c       lux ( ... ):    real values of lu arroheads
c       x (1..n):       the right-hand-side
 
c=======================================================================
c  workspace:
c=======================================================================
c
c       w (1..n):
 
c=======================================================================
c  output:
c=======================================================================
c
c       x (1..n):       the solution to l'x=b
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2s2
c       subroutines called:     strsv, sgemv
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer j, k, s, luip, luxp, luk, ludegc, lucp, lxp, col
        real
     $          one
        parameter (one = 1.0)
 
c  s:       an element, or lu arrowhead
c  k:       kth pivot
c  j:       jth column in l2' array in element s
c  luip:    s is in lui (luip...)
c  luxp:    real part of s is in lux (luxp...)
c  luk:     number of pivots in s
c  ludegc:  column degree of non-pivotal part of s
c  lucp:    pattern of column of s in lui (lucp...lucp+ludegc-1)
c  lxp:     the ludegc-by-luk l2 block of s is in lux (lxp...)
c  col:     column index
 
c=======================================================================
c  executable statments:
c=======================================================================
 
        k = npiv
        do 30 s = nlu, 1, -1
 
c          -------------------------------------------------------------
c          get s-th lu arrowhead (s = nlu..1, in reverse pivotal order)
c          -------------------------------------------------------------
 
           luip   = lup (s)
           luxp   = lui (luip)
           luk    = lui (luip+1)
           ludegc = lui (luip+3)
           lucp   = (luip + 7)
           lxp    = luxp + luk
 
           if (luk .eq. 1) then
 
c             ----------------------------------------------------------
c             only one pivot, stride-1 sparse dot product
c             ----------------------------------------------------------
 
cfpp$ nodepchk l
              do 10 j = 1, ludegc
                 col = lui (lucp+j-1)
c                row: k, u (row,col): lux (lxp+j-1)
                 x (k) = x (k) - lux (lxp+j-1) * x (col)
10            continue
c             l (k,k) is one
              k = k - 1
 
           else
 
c             ----------------------------------------------------------
c             more than one pivot
c             ----------------------------------------------------------
 
              k = k - luk
              do 20 j = 1, ludegc
                 col = lui (lucp+j-1)
                 w (j) = x (col)
20            continue
              call sgemv ('t', ludegc, luk, -one,
     $           lux (lxp), ludegc + luk, w, 1, one, x (k+1), 1)
              call strsv ('l', 't', 'u', luk,
     $           lux (luxp), ludegc + luk, x (k+1), 1)
 
           endif
 
30      continue
        return
        end
 
        subroutine ums2of (w, n, rperm, cperm, nzoff,
     $          offp, offi, offx, pr,
     $          icntl, mp, mi, mx, mn, mnz, presrv, nblks, blkp,
     $          onz, who, info, nbelow)
        integer n, nzoff, w (n+1), rperm (n), cperm (n), onz,
     $          offp (n+1), offi (onz), pr (n), icntl (20), mn, mnz,
     $          mp (mn+1), mi (mnz), nblks, blkp (nblks+1), who, nbelow,
     $          info (40)
        logical presrv
        real
     $          offx (onz), mx (mnz)
 
c=== ums2of ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  permute the off-diagonal blocks according to final pivot permutation.
c  this routine is called only if the block-triangular-form (btf) is
c  used.
 
c=======================================================================
c  input:
c=======================================================================
c
c       n:              order of matrix
c       rperm (1..n):   the final row permutations, including btf
c                       if i is the k-th pivot row, then rperm (k) = i
c       cperm (1..n):   the final column permutations, including btf
c                       if j is the k-th pivot col, then cperm (k) = j
c       icntl:          integer control parameters, see ums2in
c       info:           integer informational parameters
c       who:            who called (1: ums2fa, 2: ums2rf)
c
c       if presrv is true then
c           mn:                 order of preserved matrix
c           mnz:                number of entries in preserved matrix
c           mp (1..mn+1):       column pointers of preserved matrix
c           mi (1..mnz):        row indices of preserved matrix
c           mx (1..mnz):        values of preserved matrix
c           blkp (1..nblks+1):  the index range of the blocks
c           nblks:              the number of diagonal blocks
c       else
c           mn:                 0
c           mnz:                nzoff
c           mp:                 unaccessed
c           offp (1..n+1):      column pointers for off-diagonal entries
c                               in original order
c           mi (1..mnz):        the row indices of off-diagonal entries,
c                               in original order
c           mx (1..mnz):        the values of off-diagonal entries,
c                               in original order
c           nblks:              0
c           blkp (1..nblks+1):  unaccessed
c           nzoff:              number of entries in off-diagonal blocks
 
c=======================================================================
c  workspace:
c=======================================================================
c
c       w (1..n)
 
c=======================================================================
c  output:
c=======================================================================
c
c       offp (1..n+1):          row pointers for off-diagonal part
c       offi (1..nzoff):        column indices in off-diagonal part
c       offx (1..nzoff):        values in off-diagonal part
c       nzoff:                  number of entries in off-diagonal blocks
c       pr (1..n):              inverse row permutation
c       nbelow:                 entries that are below the diagonal
c                               blocks (can only occur if who = 2)
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2f0, ums2ra, ums2r0
c       subroutines called:     ums2p2
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer row, col, p, blk, k, k1, k2, io, prl
        logical pr3
 
c  row:     row index
c  col:     column index
c  p:       pointer
c  blk:     current diagonal block
c  k:       kth pivot
c  k1,k2:   current diaogonal block is a (k1..k2, k1..k2)
c  io:      i/o unit for diagnostic messages
c  prl:     printing level
c  pr3:     true if printing entries below diagonal blocks (ums2rf)
 
c=======================================================================
c  executable statments:
c=======================================================================
 
        io = icntl (2)
        prl = icntl (3)
        pr3 = prl .ge. 3 .and. io .ge. 0
 
c-----------------------------------------------------------------------
c  compute inverse row permutation
c-----------------------------------------------------------------------
 
c       if original row i is the kth pivot row, then
c               rperm (k) = i
c               pr (i) = k
c       if original col j is the kth pivot col, then
c               cperm (k) = j
cfpp$ nodepchk l
        do 10 k = 1, n
           pr (rperm (k)) = k
10      continue
 
c-----------------------------------------------------------------------
c  construct row-oriented pointers for permuted row-form
c-----------------------------------------------------------------------
 
        w (1) = 1
        do 20 row = 2, n
           w (row) = 0
20      continue
        nbelow = 0
        if (presrv) then
           do 50 blk = 1, nblks
              k1 = blkp (blk)
              k2 = blkp (blk+1) - 1
              do 40 col = k1, k2
cfpp$ nodepchk l
                 do 30 p = mp (cperm (col)), mp (cperm (col)+1)-1
                    row = pr (mi (p))
                    if (row .lt. k1) then
c                      offdiagonal entry
                       w (row) = w (row) + 1
                    else if (row .gt. k2 .and. who .eq. 2) then
c                      this entry is below the diagonal block - invalid.
c                      this can only occur if who = 2 (ums2rf).
                       if (pr3) then
c                         print the original row and column indices:
                          call ums2p2 (2, 96, mi(p), col,mx(p),io)
                       endif
                       nbelow = nbelow + 1
                    endif
30               continue
40            continue
50         continue
        else
           do 70 col = 1, n
cfpp$ nodepchk l
              do 60 p = offp (col), offp (col+1) - 1
                 row = pr (mi (p))
                 w (row) = w (row) + 1
60            continue
70         continue
        endif
        do 80 row = 2, n
           w (row) = w (row) + w (row-1)
80      continue
        w (n+1) = w (n)
c       w (row) now points just past end of row in offi/x
 
c-----------------------------------------------------------------------
c  construct the row-oriented form of the off-diagonal values,
c  in the final pivot order.  the column indices in each row
c  are placed in ascending order (the access of offi/offx later on
c  does not require this, but it makes access more efficient).
c-----------------------------------------------------------------------
 
        if (presrv) then
           do 110 blk = nblks, 1, -1
              k1 = blkp (blk)
              k2 = blkp (blk+1) - 1
              do 100 col = k2, k1, - 1
cfpp$ nodepchk l
                 do 90 p = mp (cperm (col)), mp (cperm (col)+1)-1
                    row = pr (mi (p))
                    if (row .lt. k1) then
c                      offdiagonal entry
                       w (row) = w (row) - 1
                       offi (w (row)) = col
                       offx (w (row)) = mx (p)
                    endif
90               continue
100           continue
110        continue
        else
           do 130 col = n, 1, -1
cfpp$ nodepchk l
              do 120 p = offp (cperm (col)), offp (cperm (col) + 1) - 1
                 row = pr (mi (p))
                 w (row) = w (row) - 1
                 offi (w (row)) = col
                 offx (w (row)) = mx (p)
120           continue
130        continue
        endif
 
c-----------------------------------------------------------------------
c  save the new row pointers
c-----------------------------------------------------------------------
 
        do 140 row = 1, n+1
           offp (row) = w (row)
140     continue
 
        nzoff = offp (n+1) - 1
 
        return
        end
 
        subroutine ums2p1 (who, where,
     $          n, ne, job, trans, lvalue, lindex, value,
     $          index, keep, cntl, icntl, info, rinfo,
     $          b, x, lx, w, lw)
        integer who, where, n, ne, job, lvalue, lindex, index (lindex),
     $          keep (20), icntl (20), info (40), lx, lw
        real
     $          value (lvalue), cntl (10), rinfo (20), b (lx),
     $          x (lx), w (lw)
        logical trans
 
c=== ums2p1 ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  print input/output arguments for ums2fa, ums2rf, and ums2so
 
c=======================================================================
c  installation note:
c=======================================================================
c
c  this routine can be deleted on installation (replaced with a dummy
c  routine that just returns without printing) in order to completely
c  disable the printing of all input/output parameters.  to completely
c  disable all i/o, you can also replace the ums2p2 routine with a
c  dummy subroutine.  if you make this modification, please do
c  not delete any original code - just comment it out instead.  add a
c  comment and date to your modifications.
 
c=======================================================================
c  input:
c=======================================================================
c
c       who:            what routine called ums2p1:
c                       1: ums2fa, 2: ums2rf, 3: ums2so
c       where:          called from where:
c                       1: entry of routine, else exit of routine
c       icntl (3):      if < 3 then print nothing, if 3 then print
c                       terse output, if >= 4 print everything
c       icntl (2):      i/o unit on which to print.  no printing
c                       occurs if < 0.
c
c       parameters to print, see ums2fa, ums2rf, or ums2so for
c       descriptions:
c
c           n, ne, job, trans, lvalue, lindex, value, index, keep,
c           icntl, info, rinfo, b, x, lx, w, lw
 
c=======================================================================
c  output:
c=======================================================================
c
c       on icntl (2) i/o unit only
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutines:  ums2fa, ums2rf, ums2so
c       functions called:       min
        intrinsic min
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        logical transa, transc, prlu, badlu, sglton, presrv, symbol
        integer io, prl, prn, k, lui1, lui2, lux1, lux2, row, col,
     $          facne, facn, nz, facjob, nblks, nzoff, factra, cpermp,
     $          rpermp, app, axp, aip, offip, offxp, lublpp, offpp,
     $          blkpp, p1, p2, p, blk, k1, k2, kn, luiip, luxxp, npiv,
     $          nlu, e, luk, lupp, luip, luxp, ludegr, ludegc, lunson,
     $          lusonp, lucp, lurp, i, j, nzcol, nzrow, uxp, son,
     $          prmax, ludimr, ludimc, maxdr, maxdc, luir1, ip1, ip2,
     $          xp1
        parameter (prmax = 10)
 
c  printing control:
c  -----------------
c  io:      i/o unit for diagnostic messages
c  prl:     printing level
c  prn:     number of entries printed so far
c  prmax:   maximum number of entries to print if prl = 3
c  prlu:    true if printing lu factors
c
c  location and status of lu factors:
c  ----------------------------------
c  transc:  transc argument in ums2so
c  transa:  transa argument in ums2fa or ums2rf when matrix factorized
c  badlu:   true if lu factors uncomputed or corrupted
c  presrv:  true if original matrix was preserved when factorized
c  symbol:  true if only symbolic part of lu factors needed on input
c  lui1:    integer part of lu factors start in index (lui1...)
c  luir1:   index (luir1 ... lui2) is needed for a call to ums2rf
c  lui2:    integer part of lu factors end in index (..lui2)
c  lux1:    real part of lu factors start in value (lux1...)
c  lux2:    real part of lu factors end in value (...lux1)
c  ip1:     pointer into leading part of lu factors in index
c  ip2:     pointer into trailing part of lu factors in index
c  xp1:     pointer into leading part of lu factors in value
c
c  arrays and scalars allocated in lu factors (in order):
c  ------------------------------------------------------
c  app:     ap (1..n+1) array located in index (app...app+n)
c  axp:     ax (1..nz) array located in value (axp...axp+nz-1)
c  aip:     ai (1..nz) array located in index (aip...aip+nz-1)
c  offip:   offi (1..nzoff) array loc. in index (offip...offip+nzoff-1)
c  offxp:   offx (1..nzoff) array loc. in value (offxp...offxp+nzoff-1)
c  ...      lu factors of each diagonal block located here
c  lublpp:  lublkp (1..nblks) array in index (lublpp..lublpp+nblks-1)
c  blkpp:   blkp (1..nblks+1) array loc. in index (blkpp...blkpp+nblks)
c  offpp:   offp (1..n+1) array located in index (offpp...offpp+n)
c  cpermp:  cperm (1..n) array located in index (cpermp...cpermp+n-1)
c  rpermp:  rperm (1..n) array located in index (rpermp...rpermp+n-1)
c  ...      seven scalars in index (lui2-6...lui2):
c  factra:  0/1 if transa argument was false/true in ums2fa or ums2rf
c  nzoff:   number of entries in off-diagonal part
c  nblks:   number of diagonal blocks
c  facjob:  job argument in ums2fa or ums2rf when matrix factorized
c  nz:      entries in a
c  facn:    n argument in ums2fa or ums2rf when matrix factorized
c  facne:   ne argument in ums2fa or ums2rf when matrix factorized
c
c  a single diagonal block and its lu factors:
c  -------------------------------------------
c  blk:     current diagonal block
c  k1,k2:   current diagonal is a (k1..k2, k1..k2)
c  kn:      order of current diagonal block (= k2-k1+1)
c  sglton:  true if current diagonal block is 1-by-1 (a singleton)
c  luiip:   lu factors of a diagonal block start in index (luiip...)
c  luxxp:   lu factors of a diagonal block start in value (luxxp...)
c  npiv:    number of pivots in a diagonal block (0 <= npiv <= kn)
c  nlu:     number of elements in a diagonal block
c  lupp:    lup (1..nlu) array located in index (lupp...lupp+nlu-1)
c
c  an element in the lu factors of a single diagonal block:
c  --------------------------------------------------------
c  e:       element
c  luk:     number of pivots in element e
c  luip:    integer part of element is in index (luip...)
c  luxp:    real part of element e is in value (luxp...)
c  ludegr:  row degree (number of columns) of u2 block in element e
c  ludegc:  column degree (number of rows) of l2 block in element e
c  lunson:  number of sons of element e in the assembly dag
c  lusonp:  list of sons of element e in index(lusonp...lusonp+lunson-1)
c  lucp:    column pattern (row indices) of l2 block in index (lucp..)
c  lurp:    row pattern (column indices) of u2 block in index (lurp..)
c  nzcol:   entries in a column of l, including unit diagonal
c  nzrow:   entries in a row of u, including non-unit diagonal
c  uxp:     a row of the u2 block located in value (uxp...)
c  son:     a son of the element e
c  ludimr:  row dimension (number of columns) in frontal matrix
c  ludimc:  column dimension (number of rows) in frontal matrix
c  maxdr:   largest ludimr for this block
c  maxdc:   largest ludimc for this block
c
c  other:
c  ------
c  row:     row index
c  col:     column index
c  k:       kth pivot, and general loop index
c  i, j:    loop indices
c  p:       pointer
c  p1:      column of a starts ai/ax (p1...), or row offi/x (p1...)
c  p2:      column of a ends in ai/ax (...p2), or row offi/x (...p2)
 
c=======================================================================
c  executable statements:
c       if (printing disabled on installation) return
c=======================================================================
 
c-----------------------------------------------------------------------
c  get printing control parameters
c-----------------------------------------------------------------------
 
        io = icntl (2)
        prl = icntl (3)
        if (prl .lt. 3 .or. io .lt. 0) then
c          printing has not been requested
           return
        endif
 
c-----------------------------------------------------------------------
c  who is this, and where.  determine if lu factors are to be printed
c-----------------------------------------------------------------------
 
        if (who .eq. 1) then
           if (where .eq. 1) then
              write (io, 200) 'ums2fa input:       '
              prlu = .false.
           else
              write (io, 200) 'ums2fa output:      '
              prlu = .true.
           endif
        else if (who .eq. 2) then
           if (where .eq. 1) then
              write (io, 200) 'ums2rf input:       '
              prlu = .true.
           else
              write (io, 200) 'ums2rf output:      '
              prlu = .true.
           endif
        else if (who .eq. 3) then
           if (where .eq. 1) then
              write (io, 200) 'ums2so input:       '
              prlu = .true.
           else
              write (io, 200) 'ums2so output:      '
              prlu = .false.
           endif
        endif
 
c-----------------------------------------------------------------------
c  print scalar input arguments: n, ne, job, trans, lvalue, lindex
c-----------------------------------------------------------------------
 
        if (where .eq. 1) then
           write (io, *)  '   scalar arguments:'
           write (io, *)  '      n:      ', n, ' : order of matrix a'
           if (who .eq. 3) then
c             ums2so:
c             was a or a^t factorized?
              lui2 = keep (5)
              transa = .false.
              if (lui2-6 .ge. 1 .and. lui2-6 .le. lindex) then
                 transa = index (lui2-6) .ne. 0
              endif
              transc = trans
              if (.not. transc) then
                 if (job .eq. 1) then
                    write (io, *)'      job:    ',job,' : solve p''lx=b'
                 else if (job .eq. 2) then
                    write (io, *)'      job:    ',job,' : solve uq''x=b'
                 else if (.not. transa) then
                    write (io, *)'      job:    ',job,' : solve ax=b',
     $                           ' (paq=lu was factorized)'
                 else
                    write (io, *)'      job:    ',job,' : solve a''x=b',
     $                           ' (pa''q=lu was factorized)'
                 endif
              else
                 if (job .eq. 1) then
                    write (io, *)'      job:    ',job,' : solve l''px=b'
                 else if (job .eq. 2) then
                    write (io, *)'      job:    ',job,' : solve qu''x=b'
                 else if (.not. transa) then
                    write (io, *)'      job:    ',job,' : solve a''x=b',
     $                           ' (paq=lu was factorized)'
                 else
                    write (io, *)'      job:    ',job,' : solve ax=b',
     $                           ' (pa''q=lu was factorized)'
                 endif
              endif
              if (transc) then
                 write (io, *)
     $                  '      transc:   .true. : see job above '
              else
                 write (io, *)
     $                  '      transc:   .false. : see job above '
              endif
           else
c             ums2fa or ums2rf:
              write (io, *)'      ne:     ', ne,' : entries in matrix a'
              if (job .eq. 1) then
                 write (io, *)
     $                  '      job:    ',job,' : matrix a preserved'
              else
                 write (io, *)
     $                  '      job:    ',job,' : matrix a not preserved'
              endif
              transa = trans
              if (transa) then
                 write (io, *)
     $                  '      transa:   .true. : factorize a transpose'
              else
                 write (io, *)
     $                  '      transa:   .false. : factorize a'
              endif
           endif
           write (io, *)
     $  '      lvalue: ',lvalue,' : size of value array'
           write (io, *)
     $  '      lindex: ',lindex,' : size of index array'
        endif
 
c-----------------------------------------------------------------------
c  print control parameters:  icntl, cntl, and keep (6..8)
c-----------------------------------------------------------------------
 
        if (where .eq. 1) then
           write (io, *)
     $  '   control parameters, normally initialized by ums2in:'
           write (io, *)
     $  '      icntl (1...8): integer control parameters'
           write (io, *)
     $  '      icntl (1): ',icntl (1),' : i/o unit for error and',
     $                    ' warning messages'
           write (io, *)
     $  '      icntl (2): ',io,' : i/o unit for diagnostics'
           write (io, *)
     $  '      icntl (3): ',prl,' : printing control'
           if (who .eq. 1) then
              if (icntl (4) .eq. 1) then
                 write (io, *)
     $  '      icntl (4): ',icntl (4),' : use block triangular',
     $                    ' form (btf)'
              else
                 write (io, *)
     $  '      icntl (4): ',icntl (4),' : do not permute to block',
     $                    ' triangular form (btf)'
              endif
              write (io, *)
     $  '      icntl (5): ',icntl (5),' : columns examined during',
     $                    ' pivot search'
              if (icntl (6) .ne. 0) then
                 write (io, *)
     $  '      icntl (6): ',icntl (6),' : preserve symmetry'
              else
                 write (io, *)
     $  '      icntl (6): ',icntl (6),' : do not preserve symmetry'
              endif
           endif
           if (who .ne. 3) then
              write (io, *)
     $  '      icntl (7): ',icntl (7),' : block size for dense matrix',
     $                    ' multiply'
           else
              write (io, *)
     $  '      icntl (8): ',icntl (8),' : maximum number',
     $                    ' of iterative refinement steps'
           endif
           if (who .eq. 1) then
              write (io, *)
     $  '      cntl (1...3): real control parameters'
              write (io, *)
     $  '      cntl (1):  ',cntl (1),' : relative pivot tolerance'
              write (io, *)
     $  '      cntl (2):  ',cntl (2),' : frontal matrix',
     $                    ' growth factor'
              write (io, *)
     $  '      keep (6...8): integer control parameters',
     $                          ' not normally modified by user'
              write (io, *)
     $  '      keep (6):  ',keep(6),' : largest positive integer'
              write (io, *)
     $  '      keep (7):  ',keep(7),' : dense row/col control, d1'
              write (io, *)
     $  '      keep (8):  ',keep(8),' : dense row/col control, d2'
           else if (who .eq. 3) then
              write (io, *)
     $  '      cntl (1...3): real control parameters'
              write (io, *)
     $  '      cntl (3):  ',cntl(3),' : machine epsilon'
           endif
        endif
 
c-----------------------------------------------------------------------
c  print the informational output
c-----------------------------------------------------------------------
 
        if (where .ne. 1) then
           write (io, *)
     $  '   output information:'
           write (io, *)
     $  '      info (1...24): integer output information'
           if (info (1) .lt. 0) then
              write (io, *)
     $  '      info (1):  ',info (1),' : error occurred!'
           else if (info (1) .gt. 0) then
              write (io, *)
     $  '      info (1):  ',info (1),' : warning occurred'
           else
              write (io, *)
     $  '      info (1):  ',info (1),' : no error or warning',
     $                    ' occurred'
           endif
           if (who .ne. 3) then
              write (io, *)
     $  '      info (2):  ',info (2),' : duplicate entries in a'
              write (io, *)
     $  '      info (3):  ',info (3),' : invalid entries in a',
     $                          ' (indices not in 1..n)'
              write (io, *)
     $  '      info (4):  ',info (4),' : invalid entries in a',
     $                          ' (not in prior pattern)'
              write (io, *)
     $  '      info (5):  ',info (5),' : entries in a after adding'
              write (io, *)
     $  '                       duplicates and removing invalid entries'
              write (io, *)
     $  '      info (6):  ',info (6),' : entries in diagonal',
     $                    ' blocks of a'
              write (io, *)
     $  '      info (7):  ',info (7),' : entries in off-diagonal',
     $                    ' blocks of a'
              write (io, *)
     $  '      info (8):  ',info (8),' : 1-by-1 diagonal blocks',
     $                    ' in a'
              write (io, *)
     $  '      info (9):  ',info (9),' : diagonal blocks in a',
     $                    ' (>1 only if btf used)'
              write (io, *)
     $  '      info (10): ',info (10),' : entries below diagonal in l'
              write (io, *)
     $  '      info (11): ',info (11),' : entries above diagonal in u'
              write (io, *)
     $  '      info (12): ',info (12),' : entries in l + u +',
     $                    ' offdiagonal blocks of a'
              write (io, *)
     $  '      info (13): ',info (13),' : frontal matrices'
              write (io, *)
     $  '      info (14): ',info (14),' : integer garbage',
     $                    ' collections'
              write (io, *)
     $  '      info (15): ',info (15),' : real garbage collections'
              write (io, *)
     $  '      info (16): ',info (16),' : diagonal pivots chosen'
              write (io, *)
     $  '      info (17): ',info (17),' : numerically valid pivots',
     $                    ' found in a'
              write (io, *)
     $  '      info (18): ',info (18),' : memory used in index'
              write (io, *)
     $  '      info (19): ',info (19),' : minimum memory needed in',
     $                    ' index'
              write (io, *)
     $  '      info (20): ',info (20),' : memory used in value'
              write (io, *)
     $  '      info (21): ',info (21),' : minimum memory needed in',
     $                    ' value'
              write (io, *)
     $  '      info (22): ',info (22),' : memory needed in',
     $                    ' index for next call to ums2rf'
              write (io, *)
     $  '      info (23): ',info (23),' : memory needed in',
     $                    ' value for next call to ums2rf'
           else
              write (io, *)
     $  '      info (24): ',info (24),' : steps of iterative',
     $                    ' refinement taken'
           endif
           if (who .ne. 3) then
              write (io, *)
     $  '      rinfo (1...8): real output information'
              write (io, *)
     $  '      rinfo (1): ',rinfo (1),' : total blas flop count'
              write (io, *)
     $  '      rinfo (2): ',rinfo (2),' : assembly flop count'
              write (io, *)
     $  '      rinfo (3): ',rinfo (3),' : pivot search flop count'
              write (io, *)
     $  '      rinfo (4): ',rinfo (4),' : level-1 blas flop count'
              write (io, *)
     $  '      rinfo (5): ',rinfo (5),' : level-2 blas flop count'
              write (io, *)
     $  '      rinfo (6): ',rinfo (6),' : level-3 blas flop count'
           else if (lw .eq. 4*n) then
              write (io, *)
     $  '      rinfo (1...8): real output information'
              write (io, *)
     $  '      rinfo (7): ',rinfo (7),' : sparse error estimate',
     $                    ' omega1'
              write (io, *)
     $  '      rinfo (8): ',rinfo (8),' : sparse error estimate',
     $                    ' omega2'
           endif
        endif
 
c-----------------------------------------------------------------------
c  print input matrix a, in triplet form, for ums2fa and ums2rf
c-----------------------------------------------------------------------
 
        if (where .eq. 1 .and. who .ne. 3) then
 
           if (transa) then
              write (io, *) '   the input matrix a transpose:'
              write (io, *)
     $  '      value (1 ... ',ne,' ): numerical values'
              write (io, *)
     $  '      index (1 ... ',ne,' ): column indices'
              write (io, *)
     $  '      index (',ne+1,' ... ',2*ne,' ): row indices'
              write (io, *) '   entries in the matrix a transpose',
     $                      ' (entry number: row, column, value):'
           else
              write (io, *) '   the input matrix a:'
              write (io, *)
     $  '      value (1 ... ',ne,' ): numerical values'
              write (io, *)
     $  '      index (1 ... ',ne,' ): row indices'
              write (io, *)
     $  '      index (',ne+1,' ... ',2*ne,' ): column indices'
              write (io, *) '   entries in the matrix a',
     $                      ' (entry number: row, column, value):'
           endif
 
           prn = min (prmax, ne)
           if (prl .ge. 4) then
              prn = ne
           endif
           do 10 k = 1, prn
              if (transa) then
                 row = index (k+ne)
                 col = index (k)
              else
                 row = index (k)
                 col = index (k+ne)
              endif
              write (io, *) '      ', k, ': ',row,' ',col,' ', value (k)
10         continue
           if (prn .lt. ne) then
              write (io, 220)
           endif
        endif
 
c-----------------------------------------------------------------------
c  print the lu factors:  ums2fa output, ums2rf input/output,
c                         and ums2so input
c-----------------------------------------------------------------------
 
        if (prlu .and. info (1) .lt. 0) then
           write (io, *) '   lu factors not printed because of error',
     $                   ' flag, info (1) = ', info (1)
           prlu = .false.
        endif
 
        if (prlu) then
 
c          -------------------------------------------------------------
c          description of what must be preserved between calls
c          -------------------------------------------------------------
 
           lux1 = keep (1)
           lux2 = keep (2)
           lui1 = keep (3)
           luir1 = keep (4)
           lui2 = keep (5)
 
           xp1 = lux1
           ip1 = lui1
           ip2 = lui2
 
c          -------------------------------------------------------------
c          on input to ums2rf, only the symbol information is used
c          -------------------------------------------------------------
 
           symbol = who .eq. 2 .and. where .eq. 1
 
           if (symbol) then
              write (io, *)
     $  '   keep (4...5) gives the location of lu factors'
              write (io, *)
     $  '      which must be preserved for calls to ums2rf: '
           else
              write (io, *)
     $  '   keep (1...5) gives the location of lu factors'
              write (io, *)
     $  '      which must be preserved for calls to ums2so: '
              write (io, *)
     $  '         value ( keep (1): ', lux1,' ... keep (2): ', lux2,' )'
              write (io, *)
     $  '         index ( keep (3): ', lui1,' ... keep (5): ', lui2,' )'
              write (io, *)
     $  '      and for calls to ums2rf: '
           endif
           write (io, *)
     $  '         index ( keep (4): ',luir1,' ... keep (5): ', lui2,' )'
 
           badlu = luir1 .le. 0 .or. lui2-6 .lt. luir1 .or.
     $        lui2 .gt. lindex
           if (.not. symbol) then
              badlu = badlu .or. lux1 .le. 0 .or.
     $        lux1 .gt. lux2 .or. lux2 .gt. lvalue .or. lui1 .le. 0 .or.
     $        luir1 .lt. lui1 .or. luir1 .gt. lui2
           endif
 
c          -------------------------------------------------------------
c          get the 7 scalars, and location of permutation vectors
c          -------------------------------------------------------------
 
           if (badlu) then
c             pointers are bad, so these values cannot be obtained
              facne  = 0
              facn   = 0
              nz     = 0
              facjob = 0
              nblks  = 0
              nzoff  = 0
              factra = 0
           else
              facne  = index (lui2)
              facn   = index (lui2-1)
              nz     = index (lui2-2)
              facjob = index (lui2-3)
              nblks  = index (lui2-4)
              nzoff  = index (lui2-5)
              factra = index (lui2-6)
           endif
 
           presrv = facjob .ne. 0
           transa = factra .ne. 0
           rpermp = (lui2-6) - (facn)
           cpermp = rpermp - (facn)
           ip2 = cpermp - 1
 
           if (symbol) then
              write (io, *)'   layout of lu factors in index:'
           else
              write (io, *)'   layout of lu factors in value and index:'
           endif
 
c          -------------------------------------------------------------
c          print location of preserved input matrix
c          -------------------------------------------------------------
 
           if (presrv) then
c             preserved column-form of original matrix
              app = ip1
              aip = app + (facn+1)
              ip1 = aip + (nz)
              axp = xp1
              xp1 = xp1 + (nz)
              if (.not. symbol) then
                 write (io, *)'      preserved copy of original matrix:'
                 write (io, *)
     $  '         index ( ',app,' ... ', aip-1,' ): column pointers'
                 write (io, *)
     $  '         index ( ',aip,' ... ',ip1-1,' ): row indices'
                 write (io, *)
     $  '         value ( ',axp,' ... ',xp1-1,' ): numerical values'
              endif
           else
              if (.not. symbol) then
                 write (io, *) '      original matrix not preserved.'
              endif
           endif
 
           badlu = badlu .or.
     $          n .ne. facn .or. nz .le. 0 .or. luir1 .gt. ip2 .or.
     $          nblks .le. 0 .or. nblks .gt. n
           if (.not. symbol) then
              badlu = badlu .or. xp1 .gt. lux2 .or. nzoff .lt. 0
           endif
           if (badlu) then
              nblks = 0
           endif
 
           if (nblks .le. 1) then
 
c             ----------------------------------------------------------
c             single block (or block triangular form not used),
c             or lu factors are corrupted
c             ----------------------------------------------------------
 
              write (io, *)
     $  '      collection of elements in lu factors:'
              write (io, *)
     $  '         (an "element" contains one or columns of l and'
              write (io, *)
     $  '         rows of u with similar nonzero pattern)'
              write (io, *)
     $  '         index ( ',luir1,' ... ', ip2,
     $                                      ' ): integer data'
              if (.not. symbol) then
                 write (io, *)
     $  '         value ( ',xp1,' ... ', lux2,' ): numerical values'
              endif
 
           else
 
c             ----------------------------------------------------------
c             block triangular form with more than one block
c             ----------------------------------------------------------
 
              offip = ip1
              ip1 = ip1 + (nzoff)
              offxp = xp1
              xp1 = xp1 + (nzoff)
              offpp = cpermp - (n+1)
              blkpp = offpp - (nblks+1)
              lublpp = blkpp - (nblks)
              ip2 = lublpp - 1
              badlu = badlu .or. luir1 .gt. ip2
              if (.not. symbol) then
                 badlu = badlu .or. ip1 .gt. ip2 .or.
     $           xp1 .gt. lux2 .or. luir1 .ne. ip1
              endif
              write (io, *)
     $  '      matrix permuted to upper block triangular form.'
              if (nzoff .ne. 0 .and. .not. symbol) then
                 write (io, *) '      entries not in diagonal blocks:'
                 write (io, *)
     $  '         index ( ',offip,' ... ',luir1-1,' ): row indices'
                 write (io, *)
     $  '         value ( ',offxp,' ... ',xp1-1,' ): numerical values'
              endif
              write (io, *)
     $  '      collection of elements in lu factors of diagonal blocks:'
              write (io, *)
     $  '         (an "element" contains one or columns of l and'
              write (io, *)
     $  '         rows of u with similar nonzero pattern)'
              if (luir1 .le. lublpp-1) then
                 write (io, *)
     $  '         index ( ',luir1,' ... ', ip2,
     $                                         ' ): integer data'
              endif
              if (xp1 .le. lux2 .and. .not. symbol) then
                 write (io, *)
     $  '         value ( ',xp1,' ... ', lux2,' ): numerical values'
              endif
              write (io, *) '      other block triangular data:'
              write (io, *)
     $  '         index ( ',lublpp,' ... ',blkpp-1,' ):',
     $                          ' pointers to block factors'
              write (io, *)
     $  '         index ( ', blkpp,' ... ',offpp-1,' ):',
     $                          ' index range of blocks'
              if (.not. symbol) then
                 write (io, *)
     $  '         index ( ', offpp,' ... ',   lui2-7,' ):',
     $          ' row pointers for off-diagonal part'
              endif
           endif
 
c          -------------------------------------------------------------
c          print location of permutation vectors and 7 scalars at tail
c          -------------------------------------------------------------
 
           write (io, *)
     $  '      permutation vectors (start at keep(4)-2*n-6):'
           write (io, *)
     $  '         index ( ', cpermp,' ... ',rpermp-1,' ):',
     $                          ' column permutations'
           write (io, *)
     $  '         index ( ', rpermp,' ... ',lui2-7,' ):',
     $                          ' row permutations'
           write (io, *) '      other data in index: '
           write (io, *)
     $  '         index ( ',lui2-6,' ): ', factra,' :',
     $                                  ' transa ums2fa/ums2rf argument'
           write (io, *)
     $  '         index ( ',lui2-5,' ): ', nzoff,' :',
     $                                  ' entries in off-diagonal part'
           write (io, *)
     $  '         index ( ',lui2-4,' ): ', nblks,' :',
     $                                  ' number of diagonal blocks'
           write (io, *)
     $  '         index ( ',lui2-3,' ): ', facjob,' :',
     $                                  ' job ums2fa/ums2rf argument'
           write (io, *)
     $  '         index ( ',lui2-2,' ): ', nz,' :',
     $                                  ' entries in original matrix'
           write (io, *)
     $  '         index ( ',lui2-1,' ): ', facn,' :',
     $                                  ' n ums2fa/ums2rf argument'
           write (io, *)
     $  '         index ( ',lui2  ,' ): ', facne,' :',
     $                                  ' ne ums2fa/ums2rf argument'
 
           if (.not. symbol) then
              badlu = badlu .or. ip1 .ne. luir1
           endif
           ip1 = luir1
           if (badlu) then
              write (io, *) '   lu factors uncomputed or corrupted!'
              presrv = .false.
              nblks = 0
           endif
 
c          -------------------------------------------------------------
c          copy of original matrix in column-oriented form
c          -------------------------------------------------------------
 
           if (presrv .and. .not. symbol) then
              write (io, 230)
              write (io, *) '   preserved copy of original matrix:'
              do 20 col = 1, n
                 p1 = index (app-1 + col)
                 p2 = index (app-1 + col+1) - 1
                 write (io, *) '      col: ', col, ' nz: ', p2-p1+1
                 if (prl .eq. 3) then
                    p2 = min (prmax, p2)
                 endif
                 write (io, *) (index (aip-1 + p), p = p1, p2)
                 write (io, 210) (value (axp-1 + p), p = p1, p2)
                 if (prl .eq. 3 .and. p2 .ge. prmax) then
c                   exit out of loop if done printing:
                    go to 30
                 endif
20            continue
c             loop exit label:
30            continue
              if (prl .eq. 3 .and. nz .gt. prmax) then
                 write (io, 220)
              endif
           endif
 
c          -------------------------------------------------------------
c          entries in off-diagonal blocks, in row-oriented form
c          -------------------------------------------------------------
 
           if (nblks .gt. 1 .and. .not. symbol) then
              write (io, 230)
              write (io, *)'   entries not in diagonal blocks:'
              if (nzoff .eq. 0) then
                 write (io, *)'      (none)'
              endif
              do 40 row = 1, n
                 p1 = index (offpp-1 + row)
                 p2 = index (offpp-1 + row+1) - 1
                 if (p2 .ge. p1) then
                    write (io, *) '      row: ', row, ' nz: ',p2-p1+1
                    if (prl .eq. 3) then
                       p2 = min (prmax, p2)
                    endif
                    write (io, *) (index (offip-1 + p), p = p1, p2)
                    write (io, 210) (value (offxp-1 + p), p = p1, p2)
                 endif
                 if (prl .eq. 3 .and. p2 .ge. prmax) then
c                   exit out of loop if done printing:
                    go to 50
                 endif
40            continue
c             loop exit label:
50            continue
              if (prl .eq. 3 .and. nz .gt. prmax) then
                 write (io, 220)
              endif
           endif
 
c          -------------------------------------------------------------
c          lu factors of each diagonal block
c          -------------------------------------------------------------
 
           write (io, 230)
           if (nblks .gt. 0) then
              write (io, *) '   lu factors of each diagonal block:'
           endif
           prn = 0
           do 140 blk = 1, nblks
 
c             ----------------------------------------------------------
c             print the factors of a single diagonal block
c             ----------------------------------------------------------
 
              if (nblks .gt. 1) then
                 k1 = index (blkpp-1 + blk)
                 k2 = index (blkpp-1 + blk+1) - 1
                 kn = k2-k1+1
                 sglton = kn .eq. 1
                 if (sglton) then
c                   this is a singleton
                    luxxp = xp1-1 + index (lublpp-1 + blk)
                 else
                    luiip = ip1-1 + index (lublpp-1 + blk)
                 endif
              else
                 sglton = .false.
                 k1 = 1
                 k2 = n
                 kn = n
                 luiip = ip1
              endif
 
              write (io, 240)
              if (sglton) then
 
c                -------------------------------------------------------
c                this is a singleton
c                -------------------------------------------------------
 
                 write (io, *)'   singleton block: ', blk,
     $                        ' at index : ', k1
                 if (.not. symbol) then
                    write (io, *)
     $           '   located in value ( ', luxxp,' ): ', value (luxxp)
                 endif
                 if (prl .eq. 3 .and. prn .gt. prmax) then
c                   exit out of loop if done printing:
                    go to 150
                 endif
                 prn = prn + 1
 
              else
 
c                -------------------------------------------------------
c                this block is larger than 1-by-1
c                -------------------------------------------------------
 
                 luxxp = xp1-1 + index (luiip)
                 nlu = index (luiip+1)
                 npiv = index (luiip+2)
                 maxdc = index (luiip+3)
                 maxdr = index (luiip+4)
                 lupp = luiip+5
                 write (io, *) '   block: ',blk,' first index: ',k1,
     $                         ' last index: ',k2, '   order: ', kn
                 write (io, *) '   elements: ', nlu, '   pivots: ', npiv
                 write (io, *) '   largest contribution block: ',
     $                         maxdc, ' by ', maxdr
                 write (io, *) '   located in index ( ',luiip,' ... )'
                 if (.not. symbol) then
                    write (io, *) '   and in value ( ',luxxp,' ... )'
                 endif
                 luiip = lupp + nlu
 
c                note: the indices of the lu factors of the block range
c                from 1 to kn, even though the kn-by-kn block resides in
c                a (k1 ... k2, k1 ... k2).
                 k = 0
 
                 do 130 e = 1, nlu
 
c                   ----------------------------------------------------
c                   print a single element
c                   ----------------------------------------------------
 
                    luip = luiip-1 + index (lupp-1 + e)
                    luxp = luxxp-1 + index (luip)
                    luk  = index (luip+1)
                    ludegr = index (luip+2)
                    ludegc = index (luip+3)
                    lunson = index (luip+4)
                    ludimr = index (luip+5)
                    ludimc = index (luip+6)
                    lucp = luip + 7
                    lurp = lucp + ludegc
                    lusonp = lurp + ludegr
                    write (io, *) '      e: ',e, ' pivots: ', luk,
     $                  ' children in dag: ', lunson,
     $                  ' frontal matrix: ', ludimr, ' by ', ludimc
 
c                   ----------------------------------------------------
c                   print the columns of l
c                   ----------------------------------------------------
 
                    p = luxp
                    do 80 j = 1, luk
                       col = k+j
                       nzcol = luk-j+1+ludegc
                       write (io, *) '         col: ',col,' nz: ',nzcol
c                      l is unit diagonal
                       prn = prn + 1
                       row = col
                       if (symbol) then
                          write (io, *) '            ', row
                       else
                          write (io, *) '            ', row, '  1.0'
                       endif
                       p = p + 1
c                      pivot block
                       do 60 i = j+1, luk
                          if (prl.eq.3 .and. prn.gt.prmax) then
c                            exit out of loop if done printing:
                             go to 150
                          endif
                          prn = prn + 1
                          row = k+i
                          if (symbol) then
                             write (io, *)'            ', row
                          else
                             write (io, *)'            ', row, value (p)
                          endif
                          p = p + 1
60                     continue
c                      l block
                       do 70 i = 1, ludegc
                          if (prl.eq.3 .and. prn.gt.prmax) then
c                            exit out of loop if done printing:
                             go to 150
                          endif
                          prn = prn + 1
                          row = index (lucp-1+i)
                          if (symbol) then
                             write (io, *)'            ', row
                          else
                             write (io, *)'            ', row, value (p)
                          endif
                          p = p + 1
70                     continue
                       p = p + j
80                  continue
 
c                   ----------------------------------------------------
c                   print the rows of u
c                   ----------------------------------------------------
 
                    uxp = luxp + luk*(ludegc+luk)
                    do 110 i = 1, luk
                       row = k+i
                       nzrow = luk-i+1+ludegr
                       write (io, *) '         row: ',row,' nz: ',nzrow
                       p = luxp + (i-1) + (i-1) * (ludegc+luk)
c                      pivot block
                       do 90 j = i, luk
                          if (prl.eq.3 .and. prn.gt.prmax) then
c                            exit out of loop if done printing:
                             go to 150
                          endif
                          prn = prn + 1
                          col = k+j
                          if (symbol) then
                             write (io, *)'            ', col
                          else
                             write (io, *)'            ', col, value (p)
                          endif
                          p = p + (ludegc+luk)
90                     continue
                       p = uxp
c                      u block
                       do 100 j = 1, ludegr
                          if (prl.eq.3 .and. prn.gt.prmax) then
c                            exit out of loop if done printing:
                             go to 150
                          endif
                          prn = prn + 1
                          col = index (lurp-1+j)
                          if (symbol) then
                             write (io, *)'            ', col
                          else
                             write (io, *)'            ', col, value (p)
                          endif
                          p = p + luk
100                    continue
                       uxp = uxp + 1
110                 continue
 
c                   ----------------------------------------------------
c                   print the sons of the element in the assembly dag
c                   ----------------------------------------------------
 
                    do 120 i = 1, lunson
                       prn = prn + 1
                       son = index (lusonp-1+i)
                       if (son .le. kn) then
c                         an luson
                          write (io, *) '         luson: ', son
                       else if (son .le. 2*kn) then
c                         a uson
                          write (io, *) '         uson:  ', son-kn
                       else
c                         an lson
                          write (io, *) '         lson:  ', son-2*kn
                       endif
120                 continue
 
c                   ----------------------------------------------------
c                   increment count of pivots within this block
c                   ----------------------------------------------------
 
                    k = k + luk
130              continue
              endif
140        continue
c          if loop was not exited prematurely, do not print "..." :
           go to 160
c          loop exit label:
150        continue
           write (io, 220)
160        continue
 
c          -------------------------------------------------------------
c          row and column permutations
c          -------------------------------------------------------------
 
           if (.not. badlu) then
              write (io, 230)
              write (io, *) '      column permutations'
              if (prl .ge. 4 .or. n .le. prmax) then
                 write (io, *) (index (cpermp+i-1), i = 1, n)
              else
                 write (io, *) (index (cpermp+i-1), i = 1, prmax)
                 write (io, 220)
              endif
 
              write (io, 230)
              write (io, *) '      row permutations'
              if (prl .ge. 4 .or. n .le. prmax) then
                 write (io, *) (index (rpermp+i-1), i = 1, n)
              else
                 write (io, *) (index (rpermp+i-1), i = 1, prmax)
                 write (io, 220)
              endif
           endif
 
        endif
 
c-----------------------------------------------------------------------
c  print b (on input) or w and x (on output) for ums2so
c-----------------------------------------------------------------------
 
        if (who .eq. 3) then
           write (io, 230)
           prn = min (prmax, n)
           if (prl .ge. 4) then
c             print all of b, or w and x
              prn = n
           endif
           if (where .eq. 1) then
              write (io, *) '   w (1 ... ',lw,' ), work vector:',
     $                      ' not printed'
              write (io, *) '   b (1 ... ',n,' ), right-hand side: '
              do 170 i = 1, prn
                 write (io, *) '      ', i, ': ', b (i)
170           continue
              if (prn .lt. n) then
                 write (io, 220)
              endif
           else
              if (info (1) .lt. 0) then
                 write (io, *) '   w (1 ... ',lw,' ), work vector, and'
                 write (io, *) '   x (1 ... ',n,' ), solution,'
                 write (io, *) '      not printed because of error',
     $                         ' flag, info (1) = ', info (1)
              else
                 if (lw .eq. 4*n) then
c                   ums2so did iterative refinement
                    write (io, *) '   w (1 ... ',n,' ), residual: '
                    do 180 i = 1, prn
                       write (io, *) '      ', i, ': ', w (i)
180                 continue
                    if (prn .lt. n) then
                       write (io, 220)
                    endif
                    write (io, *) '   w (',n+1,' ... ',lw,' )',
     $                            ', work vector: not printed'
                 else
c                   no iterative refinement
                    write (io, *) '   w (1 ... ',lw,' ),',
     $                            ' work vector: not printed'
                 endif
                 write (io, *) '   x (1 ... ',n,' ), solution: '
                 do 190 i = 1, prn
                    write (io, *) '      ', i, ': ', x (i)
190              continue
                 if (prn .lt. n) then
                    write (io, 220)
                 endif
              endif
           endif
        endif
 
c-----------------------------------------------------------------------
c  who is this, and where:
c-----------------------------------------------------------------------
 
        if (who .eq. 1) then
           if (where .eq. 1) then
              write (io, 200) 'end of ums2fa input '
           else
              write (io, 200) 'end of ums2fa output'
           endif
        else if (who .eq. 2) then
           if (where .eq. 1) then
              write (io, 200) 'end of ums2rf input '
           else
              write (io, 200) 'end of ums2rf output'
           endif
        else if (who .eq. 3) then
           if (where .eq. 1) then
              write (io, 200) 'end of ums2so input '
           else
              write (io, 200) 'end of ums2so output'
           endif
        endif
 
        return
 
c-----------------------------------------------------------------------
c  format statments
c-----------------------------------------------------------------------
 
200     format (60('='), a20)
210     format (5e16.8)
220     format ('        ...')
230     format ('   ', 77 ('-'))
240     format ('   ', 77 ('.'))
        end
 
        subroutine ums2p2 (who, error, i, j, x, io)
        integer who, error, i, j, io
        real
     $          x
 
c=== ums2p2 ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable
 
c=======================================================================
c  description:
c=======================================================================
c
c  print error and warning messages for for ums2fa, ums2rf, and ums2so.
 
c=======================================================================
c  installation note:
c=======================================================================
c
c  this routine can be deleted on installation (replaced with a dummy
c  routine that just returns without printing) in order to completely
c  disable the printing of all error and warning messages.  the error
c  and warning return flag (info (1)) will not be affected.  to
c  completely disable all i/o, you can also replace the ums2p1 routine
c  with a dummy subroutine.  if you make this modification, please do
c  not delete any original code - just comment it out instead.  add a
c  comment and date to your modifications.
 
c=======================================================================
c  input:
c=======================================================================
c
c       who:            what user-callable routine called ums2p2:
c                       1: ums2fa, 2: ums2rf, 3: ums2so
c       i, j, x:        the relevant offending value(s)
c       io:             i/o unit on which to print.  no printing
c                       occurs if < 0.
c       error:          the applicable error (<0) or warning (>0)
c                       errors (<0) cause the factorization/solve to
c                       be terminated.  if an error occurs, a prior
c                       warning status is overwritten with the error
c                       status.
c
c  the following error codes are returned in info (1) by ums2er.
c  these errors cause the factorization or solve to terminate:
c
c  where**      error   description
c
c  fa rf  -     -1      n < 1 or n > maximum value
c  fa rf  -     -2      ne < 1 or ne > maximum value
c  fa rf  -     -3      lindex too small
c  fa rf  -     -4      lvalue too small
c  fa rf  -     -5      both lindex and lvalue are too small
c   - rf  -     -6      prior pivot ordering no longer acceptable
c   - rf so     -7      lu factors are uncomputed, or are corrupted
c
c  the following warning codes are returned in info (1) by ums2er.
c  the factorization or solve was able to complete:
c
c  fa rf  -     1       invalid entries
c  fa rf  -     2       duplicate entries
c  fa rf  -     3       invalid and duplicate entries
c  fa rf  -     4       singular matrix
c  fa rf  -     5       invalid entries, singular matrix
c  fa rf  -     6       duplicate entries, singular matrix
c  fa rf  -     7       invalid and duplicate entries, singular matrix
c   -  - so     8       iterative refinement cannot be done
c
c  the following are internal error codes (not returned in info (1))
c  for printing specific invalid or duplicate entries.  these codes are
c  for ums2co, ums2of, and ums2r2.  invalid and duplicate entries are
c  ignored during factorization.  warning levels (1..7) will be set
c  later by ums2er, above.
c
c  fa rf  -     99      invalid entry, out of range 1..n
c  fa rf  -     98      duplicate entry
c   - rf  -     97      invalid entry:  within a diagonal block, but not
c                       in the pattern of the lu factors of that block.
c   - rf  -     96      invalid entry:  below the diagonal blocks.  can
c                       only occur if the matrix has been ordered into
c                       block-upper-triangular form.
c   - rf  -     95      invalid entry:  matrix is singular.  the
c                       remaining rank 0 submatrix yet to be factorized
c                       is replaced with the identity matrix in the lu
c                       factors.  any entry that remains is ignored.
 
c ** fa: ums2fa, rf: ums2rf, so: ums2so
 
c=======================================================================
c  output:
c=======================================================================
c
c  error or warning message printed on i/o unit
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutines:  ums2er, ums2co, ums2r0, ums2r2
 
c=======================================================================
c  executable statements:
c       if (printing disabled on installation) return
c=======================================================================
 
        if (io .lt. 0) then
c          printing of error / warning messages has not been requested
           return
        endif
 
        if (who .eq. 1) then
 
c          -------------------------------------------------------------
c          ums2fa error messages
c          -------------------------------------------------------------
 
           if (error .eq. -1) then
              if (i .lt. 0) then
                 write (io, *)'ums2fa: n less than one!'
              else
                 write (io, *)'ums2fa: n too large, must be <= ', i,'!'
              endif
           else if (error .eq. -2) then
              write (io, *)'ums2fa: ne less than one!'
           else if (error .eq. -3) then
              write (io, *)'ums2fa: insufficient integer workspace! ',
     $                     ' lindex must be >= ', i, '.'
           else if (error .eq. -4) then
              write (io, *)'ums2fa: insufficient real workspace!    ',
     $                     ' lvalue must be >= ', i, '.'
 
c          -------------------------------------------------------------
c          ums2fa cumulative warning messages
c          -------------------------------------------------------------
 
           else if (error .eq. 1) then
              write (io, *)'ums2fa: ', i,' invalid entries ignored',
     $                     ' (out of range 1..n).'
           else if (error .eq. 2) then
              write (io, *)'ums2fa: ', i,' duplicate entries summed.'
           else if (error .eq. 4) then
              write (io, *)'ums2fa: matrix is singular.  only ', i,
     $                     ' pivots found.'
 
c          -------------------------------------------------------------
c          ums2fa non-cumulative warning messages (internal error codes)
c          -------------------------------------------------------------
 
           else if (error .eq. 99) then
              write (io, *)'ums2fa: invalid entry (out of range 1..n):'
              write (io, *)'        row: ',i,' col: ',j,' ',x
           else if (error .eq. 98) then
              write (io, *)'ums2fa: duplicate entry summed:'
              write (io, *)'        row: ',i,' col: ',j,' ',x
           endif
 
        else if (who .eq. 2) then
 
c          -------------------------------------------------------------
c          ums2rf error messages
c          -------------------------------------------------------------
 
           if (error .eq. -1) then
              write (io, *)'ums2rf: n less than one!'
           else if (error .eq. -2) then
              if (i .lt. 0) then
                 write (io, *)'ums2rf: ne less than one!'
              else
                 write (io, *)'ums2rf: ne too large, must be <= ',i, '!'
              endif
           else if (error .eq. -3) then
              write (io, *)'ums2rf: insufficient integer workspace! ',
     $                       ' lindex must be >= ', i, '.'
           else if (error .eq. -4) then
              write (io, *)'ums2rf: insufficient real workspace!    ',
     $                       ' lvalue must be >= ', i, '.'
           else if (error .eq. -6) then
              write (io, *)'ums2rf: pivot order from ums2fa failed!'
           else if (error .eq. -7) then
              write (io, *)'ums2rf: lu factors uncomputed,',
     $                     ' or corrupted!'
 
c          -------------------------------------------------------------
c          ums2rf cumulative warning messages
c          -------------------------------------------------------------
 
           else if (error .eq. 1) then
              if (i .gt. 0) then
                 write (io, *)'ums2rf: ', i,' invalid entries ignored',
     $                        ' (out of range 1..n).'
              else
                 write (io, *)'ums2rf: ',-i,' invalid entries ignored',
     $                        ' (not in prior pattern).'
              endif
           else if (error .eq. 2) then
              write (io, *)'ums2rf: ', i,' duplicate entries summed.'
           else if (error .eq. 4) then
              write (io, *)'ums2rf: matrix is singular.  only ', i,
     $                     ' pivots found.'
 
c          -------------------------------------------------------------
c          ums2rf non-cumulative warning messages (internal error codes)
c          -------------------------------------------------------------
 
           else if (error .eq. 99) then
              write (io, *)'ums2rf: invalid entry (out of range 1..n):'
              write (io, *)'        row: ',i,' col: ',j,' ',x
           else if (error .eq. 98) then
              write (io, *)'ums2rf: duplicate entry summed:'
              write (io, *)'        row: ',i,' col: ',j,' ',x
           else if (error .eq. 97) then
              write (io, *)'ums2rf: invalid entry (not in pattern of',
     $                     ' lu factor(s) of diagonal block(s)):'
              write (io, *)'        row: ',i,' col: ',j,' ',x
           else if (error .eq. 96) then
              write (io, *)'ums2rf: invalid entry (below diagonal',
     $                     ' blocks):'
              write (io, *)'        row: ',i,' col: ',j,' ',x
           else if (error .eq. 95) then
              write (io, *)'ums2rf: invalid entry (because',
     $                     ' matrix factorized by ums2fa was singular):'
              write (io, *)'        row: ',i,' col: ',j,' ',x
           endif
 
        else if (who .eq. 3) then
 
c          -------------------------------------------------------------
c          ums2so error messages
c          -------------------------------------------------------------
 
           if (error .eq. -7) then
              write (io, *)'ums2so: lu factors uncomputed,',
     $                     ' or corrupted!'
 
c          -------------------------------------------------------------
c          ums2so non-cumulative warning messages
c          -------------------------------------------------------------
 
           else if (error .eq. 8) then
              if (i .eq. 0) then
                 write (io, *)'ums2so: iterative refinement requested',
     $           ' but original matrix not preserved.'
              else
                 write (io, *)'ums2so: iterative refinement requested',
     $           ' but only available for ax=b or a''x=b.'
              endif
           endif
 
        endif
        return
        end
 
        subroutine ums2r0 (n, nz, cp, xx, xsize, ii, isize, xtail,
     $          itail, iuse, xuse, nzoff, nblks, icntl, cntl, info,
     $          rinfo, presrv, ap, ai, ax, an, anz, lui, luisiz,
     $          lublkp, blkp, offp, on, cperm, rperm, ne)
        integer n, nz, isize, ii (isize), icntl (20), info (40),
     $          cp (n+1), xsize, xtail, itail, iuse, xuse, an, anz,
     $          ap (an+1), ai (anz), luisiz, lui (luisiz), nblks,
     $          lublkp (nblks), blkp (nblks+1), on, offp (on+1),
     $          cperm (n), rperm (n), nzoff, ne
        logical presrv
        real
     $          xx (xsize), cntl (10), rinfo (20), ax (anz)
 
c=== ums2r0 ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  refactorize an unsymmetric sparse matrix in column-form, optionally
c  permuting the matrix to upper block triangular form and factorizing
c  each diagonal block.
 
c=======================================================================
c  input:
c=======================================================================
c
c       n:              order of matrix
c       nz:             entries in matrix
c       cp (1..n+1):    column pointers of input matrix
c       presrv:         if true then preserve original matrix
c       xsize:          size of xx
c       isize:          size of ii
c       iuse:           memory usage in index on input
c       xuse:           memory usage in value on input
c       icntl:          integer control parameters, see ums2in
c       cntl:           real control parameters, see ums2in
c
c       if presrv is true:
c           an:                 = n, order of preserved matrix
c           anz:                = anz, order of preserved matrix
c           ap (1..an+1):       column pointers of preserved matrix
c           ai (1..nz):         row indices of preserved matrix
c           ax (1..nz):         values of preserved matrix
c           ii:                 unused on input
c           xx:                 unused on input
c       else
c           an:                 0
c           anz:                1
c           ap:                 unused
c           ai:                 unused
c           ax:                 unused
c           ii (1..nz):         row indices of input matrix
c           xx (1..nz):         values of input matrix
c
c       information from prior lu factorization:
c
c       luisiz:                 size of lui
c       lui (1..luisiz):        patterns of lu factors, excluding
c                               prior preserved matrix (if it existed)
c                               and prior off-diagonal part (if it
c                               existed)
c       cperm (1..n):           column permutations
c       rperm (1..n):           row permutations
c       nblks:                  number of diagonal blocks for btf
c       if nblks > 1:
c           lublkp (1..nblks):  pointers to each diagonal lu factors
c           blkp (1..nblks+1):  index range of diagonal blocks
 
c=======================================================================
c  output:
c=======================================================================
c
c       xx (xtail ... xsize), xtail:
c
c                       lu factors are located in xx (xtail ... xsize),
c                       including values in off-diagonal part if matrix
c                       was permuted to block triangular form.
c
c       ii (itail ... isize), itail:
c
c                       the off-diagonal nonzeros, if nblks > 1
c
c       offp (1..n+1):  row pointers for off-diagonal part, if nblks > 1
c       info:           integer informational output, see ums2fa
c       rinfo:          real informational output, see ums2fa
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2rf
c       subroutines called:     ums2er, ums2r2, ums2p2, ums2ra, ums2of
c       functions called:       max
        intrinsic max
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer i, nzdia, p, ihead, nsgltn, nsym, wp, arip, arxp, npiv,
     $          wrksiz, nlu, prp, mc, mr, dummy1, dummy2, nz2, k, blk,
     $          k1, k2, kn, nzblk, col, row, prl, io, luip, mnz, arnz,
     $          xhead, offip, offxp, noutsd, nbelow, nzorig, xrmax
        real
     $          zero, one, a
        parameter (zero = 0.0, one = 1.0)
 
c  printing control:
c  -----------------
c  io:      i/o unit for diagnostic messages
c  prl:     printing level
c
c  allocated array pointers:
c  -------------------------
c  wp:      w (1..n+1), or w (1..kn+1), work array located in ii (wp...)
c  prp:     pr (1..n) work array located in ii (prp..prp+n-1)
c  arip:    ari (1..nz) array located in ii (arip..arip+nz-1)
c  arxp:    arx (1..nz) array located in xx (arxp..arxp+nz-1)
c  offip:   offi (1..nzoff) array located in ii (offip..offip+nzoff-1)
c  offxp:   offx (1..nzoff) array located in xx (offxp..offip+nzoff-1)
c
c  arrowhead-form matrix:
c  ----------------------
c  nz2:     number of entries in arrowhead matrix
c  nzblk:   number of entries in arrowhead matrix of a single block
c  arnz:    arrowhead form of blocks located in ii/xx (1..arnz)
c
c  btf information:
c  ----------------
c  k1:      starting index of diagonal block being factorized
c  k2:      ending index of diagonal block being factorized
c  kn:      the order of the diagonal block being factorized
c  blk:     block number of diagonal block being factorized
c  nsgltn:  number of 1-by-1 diagonal blocks ("singletons")
c  a:       numerical value of a singleton
c  mnz:     nzoff
c  noutsd:  entries in diagonal blocks, but not in lu (invalid)
c  nbelow:  entries below diagonal blocks (invalid)
c  nzoff:   entries above diagonal blocks (valid)
c  nzdia:   entries in diagonal blocks (valid)
c  nzorig:  total number of original entries
c
c  memory usage:
c  -------------
c  xhead:   xx (1..xhead-1) is in use, xx (xhead..xtail-1) is free
c  ihead:   ii (1..ihead-1) is in use, ii (ihead..itail-1) is free
c  wrksiz:  total size of work arrays need in ii for call to ums2r2
c  xrmax:   memory needed in value for next call to ums2rf
c
c  symbolic information and pattern of prior lu factors:
c  -----------------------------------------------------
c  nlu:     number of elements in a diagonal block
c  luip:    integer part of lu factors located in lui (luip...)
c  mr,mc:   largest frontal matrix for this diagonal block is mc-by-mr
c
c  other:
c  ------
c  k:       loop index (kth pivot)
c  i:       loop index
c  row:     row index
c  col:     column index
c  p:       pointer
c  nsym:    number of symmetric pivots chosen
c  dummy1:  argument returned by ums2ra, but not needed
c  dummy2:  argument returned by ums2ra, but not needed
 
c=======================================================================
c  executable statements:
c=======================================================================
 
        io = icntl (2)
        prl = icntl (3)
        nzorig = nz
 
        if (presrv) then
c          original matrix is not in cp/ii/xx, but in ap/ai/ax:
           ihead = 1
           xhead = 1
        else
           ihead = nz + 1
           xhead = nz + 1
        endif
 
        nzoff = 0
        nzdia = nz
        nsgltn = 0
        npiv = 0
        noutsd = 0
        nbelow = 0
        itail = isize + 1
        xtail = xsize + 1
 
c-----------------------------------------------------------------------
c current memory usage:
c-----------------------------------------------------------------------
 
c       if .not. presrv then
c               input matrix is now in ii (1..nz) and xx (1..nz)
c                       col pattern: ii (cp (col) ... cp (col+1))
c                       col values:  xx (cp (col) ... cp (col+1))
c               total: nz+n+1 integers, nz reals
c       else
c               input matrix is now in ai (1..nz) and ax (1..nz)
c                       col pattern: ai (ap (col) ... ap (col+1))
c                       col values:  ax (ap (col) ... ap (col+1))
c               cp is a size n+1 integer workspace
c               total: nz+2*(n+1) integers, nz reals
c
c       if (nblks > 1) then
c                       lublkp (1..nblks)
c                       blkp (1..nblks+1)
c                       offp (1..n+1)
c               total: (2*nblks+n+2) integers
c
c       cperm (1..n) and rperm (1..n)
c               total: 2*n integers
c
c   grand total current memory usage (including ii,xx,cp,ai,ap,ax
c       and lui):
c
c       presrv  nblks>1 integers, iuse =
c       f       f       luisiz + nz+  (n+1)+(2*n+7)
c       f       t       luisiz + nz+  (n+1)+(2*n+7)+(2*nblks+n+2)
c       t       f       luisiz + nz+2*(n+1)+(2*n+7)
c       t       t       luisiz + nz+2*(n+1)+(2*n+7)+(2*nblks+n+2)
c
c   real usage is xuse = nz
 
c-----------------------------------------------------------------------
c  get memory usage estimate for next call to ums2rf
c-----------------------------------------------------------------------
 
        xrmax = 2*ne
 
c-----------------------------------------------------------------------
c  convert matrix into arrowhead format (unless btf and preserved)
c-----------------------------------------------------------------------
 
        if (nblks .gt. 1 .and. presrv) then
 
c          -------------------------------------------------------------
c          btf is to be used, and original matrix is to be preserved.
c          it is converted and factorized on a block-by-block basis,
c          using the inverse row permutation (computed and stored in
c          offp (1..n)
c          -------------------------------------------------------------
 
           do 10 k = 1, n
              offp (rperm (k)) = k
10         continue
 
        else
 
c          -------------------------------------------------------------
c          convert the entire input matrix to arrowhead form
c          -------------------------------------------------------------
 
c          -------------------------------------------------------------
c          allocate workspace: w (n+1), pr (n), ari (nz), arx (nz)
c          -------------------------------------------------------------
 
           itail = itail - (2*n+1)
           iuse = iuse + 2*n+1
           prp = itail
           wp = prp + n
           iuse = iuse + nz
           xuse = xuse + nz
           arxp = xhead
           arip = ihead
           ihead = ihead + nz
           xhead = xhead + nz
           info (18) = max (info (18), iuse)
           info (20) = max (info (20), xuse)
           info (21) = max (info (21), xuse)
           if (ihead .gt. itail .or. xhead .gt. xtail) then
c             error return, if not enough integer and/or real memory:
              go to 9000
           endif
 
c          -------------------------------------------------------------
c          convert
c          -------------------------------------------------------------
 
           if (nblks .eq. 1) then
              if (presrv) then
                 call ums2ra (presrv, n, nz, cperm, rperm, ii (prp),
     $              ii (wp), nblks, xx (arxp), ii (arip), nzoff, nzdia,
     $              icntl, ap, blkp, ai, ax, info, offp, on, nz,
     $              0, n, nz2, i)
              else
                 call ums2ra (presrv, n, nz, cperm, rperm, ii (prp),
     $              ii (wp), nblks, xx (arxp), ii (arip), nzoff, nzdia,
     $              icntl, cp, blkp, ii, xx, info, offp, on, nz,
     $              0, n, nz2, i)
              endif
           else
c             note that presrv is false in this case
              call ums2ra (presrv, n, nz, cperm, rperm, ii (prp),
     $           ii (wp), nblks, xx (arxp), ii (arip), nzoff, nzdia,
     $           icntl, cp, blkp, ii, xx, info, offp, on, nz,
     $           0, n, nz2, nbelow)
           endif
 
c          -------------------------------------------------------------
c          copy the arrowhead pointers from w (1..n+1) to cp (1..n+1)
c          -------------------------------------------------------------
 
           do 20 i = 1, n+1
              cp (i) = ii (wp+i-1)
20         continue
 
c          -------------------------------------------------------------
c          deallocate w and pr.  if not presrv deallocate ari and arx
c          -------------------------------------------------------------
 
           iuse = iuse - (2*n+1)
           if (.not. presrv) then
c             ari and arx have been deallocated.
              xuse = xuse - nz
              iuse = iuse - nz
           endif
           itail = isize + 1
           xtail = xsize + 1
           nz = nz2
           ihead = nz + 1
           xhead = nz + 1
 
        endif
 
        info (5) = nz
        info (6) = nzdia
        info (7) = nzoff
        info (4) = nbelow
 
c-----------------------------------------------------------------------
c  refactorization
c-----------------------------------------------------------------------
 
c       ----------------------------------------------------------------
c       if nblks=1
c          arrowhead form is now stored in ii (1..nz) and xx (1..nz)
c          in reverse pivotal order (arrowhead n, n-1, ..., 2, 1).
c          the arrowhead form will be overwritten.
c       else if not presrv
c          off-diagonal part is in ii (1..nzoff), xx (1..nzoff),
c          (with row pointers offp (1..n+1)) followed by each diagonal
c          block (block 1, 2, ... nblks) in ii/xx (nzoff+1...nz).
c          each diagonal block is in arrowhead form, and in
c          reverse pivotal order (arrowhead k2, k2-1, ..., k1-1, k1).
c          the arrowhead form will be overwritten.
c       else (nblks > 1 and presrv)
c          ii and xx are still empty.  original matrix is in ap, ai,
c          and ax.  inverse row permutation (pr) is in offp (1..n).
c          the arrowhead form is not yet computed.
c       ----------------------------------------------------------------
 
        if (nblks .eq. 1) then
 
c          -------------------------------------------------------------
c          refactorize the matrix as a single block
c          -------------------------------------------------------------
 
           nlu = lui (2)
           mc = lui (4)
           mr = lui (5)
           wrksiz = 2*n + mr + 3*mc + 4*(nlu+2)
           itail = itail - wrksiz
           iuse = iuse + wrksiz
           p = itail
           info (18) = max (info (18), iuse)
           if (ihead .gt. itail) then
c             error return, if not enough integer memory:
              go to 9000
           endif
 
           call ums2r2 (cp, nz, n, xtail,
     $          xx, xsize, xuse, ii, cperm, rperm,
     $          icntl, cntl, info, rinfo, mc, mr,
     $          ii (p), ii (p+n), ii (p+2*n), ii (p+2*n+mr),
     $          ii (p+2*n+mr+mc), ii (p+2*n+mr+2*mc),
     $          ii (p+2*n+mr+3*mc), ii (p+2*n+mr+3*mc+(nlu+2)),
     $          ii (p+2*n+mr+3*mc+2*(nlu+2)),
     $          ii (p+2*n+mr+3*mc+3*(nlu+2)),
     $          nlu, lui (6), lui (nlu+6), noutsd,
     $          xrmax)
 
           if (info (1) .lt. 0) then
c             error return, if not enough real memory or bad pivot found
              go to 9010
           endif
 
c          -------------------------------------------------------------
c          deallocate workspace and original matrix (reals already done)
c          -------------------------------------------------------------
 
           iuse = iuse - wrksiz - nz
           itail = itail + wrksiz
           lui (1) = 1
           ihead = 1
           xhead = 1
 
        else
 
c          -------------------------------------------------------------
c          refactorize the block-upper-triangular form of the matrix
c          -------------------------------------------------------------
 
           if (presrv) then
c             count the entries in off-diagonal part
              nzoff = 0
           endif
 
           do 70 blk = nblks, 1, -1
 
c             ----------------------------------------------------------
c             factorize the kn-by-kn block, a (k1..k2, k1..k2)
c             ----------------------------------------------------------
 
c             get k1 and k2, the start and end of this block
              k1 = blkp (blk)
              k2 = blkp (blk+1) - 1
              kn = k2-k1+1
              a = zero
 
c             ----------------------------------------------------------
c             get pointers to, or place the block in, arrowhead form
c             ----------------------------------------------------------
 
              if (presrv) then
 
                 if (kn .gt. 1) then
 
c                   ----------------------------------------------------
c                   convert a single block to arrowhead format, using
c                   the inverse row permutation stored in offp
c                   ----------------------------------------------------
 
c                   ----------------------------------------------------
c                   compute nzblk, allocate ii/xx (1..nzblk), w(1..kn+1)
c                   ----------------------------------------------------
 
                    nzblk = 0
                    do 40 k = k1, k2
                       col = cperm (k)
cfpp$ nodepchk l
                       do 30 p = ap (col), ap (col+1) - 1
                          row = offp (ai (p))
                          if (row .lt. k1) then
c                            entry in off-diagonal part
                             nzoff = nzoff + 1
                          else if (row .le. k2) then
c                            entry in diagonal block
                             nzblk = nzblk + 1
                          endif
30                     continue
40                  continue
 
                    itail = itail - (kn+1)
                    wp = itail
                    ihead = nzblk + 1
                    xhead = nzblk + 1
                    iuse = iuse + nzblk + kn+1
                    xuse = xuse + nzblk
                    xrmax = max (xrmax, xuse)
                    info (18) = max (info (18), iuse)
                    info (20) = max (info (20), xuse)
                    info (21) = max (info (21), xuse)
                    if (ihead .gt. itail .or. xhead .gt. xtail) then
c                      error return, if not enough integer
c                      and/or real memory:
                       go to 9000
                    endif
 
c                   ----------------------------------------------------
c                   convert blk from column-form in ai/ax to arrowhead
c                   form in ii/xx (1..nzblk)
c                   ----------------------------------------------------
 
                    call ums2ra (presrv, n, nz, cperm, rperm, offp,
     $                 ii (wp), nblks, xx, ii, dummy1, dummy2,
     $                 icntl, ap, blkp, ai, ax, info, 0, 0, nzblk,
     $                 blk, kn, nz2, i)
 
c                   ----------------------------------------------------
c                   copy the arrowhead pointers from w (1..kn+1)
c                   to cp (k1 ... k2+1)
c                   ----------------------------------------------------
 
                    do 50 i = 0, kn
                       cp (k1+i) = ii (wp+i)
50                  continue
 
c                   cp (k1) is nzblk + 1 and cp (k2+1) is 1
 
c                   ----------------------------------------------------
c                   deallocate w (1..kn+1)
c                   ----------------------------------------------------
 
                    iuse = iuse - (kn+1)
                    itail = itail + (kn+1)
 
                 else
 
c                   ----------------------------------------------------
c                   get the value of singleton at a (k1,k1) if it exists
c                   ----------------------------------------------------
 
c                   find the diagonal entry in the unpermuted matrix,
c                   and count the entries in the diagonal and
c                   off-diagonal blocks.
                    col = cperm (k1)
                    do 60 p = ap (col), ap (col + 1) - 1
c                      inverse row permutation is stored in offp
                       row = offp (ai (p))
                       if (row .lt. k1) then
                          nzoff = nzoff + 1
                       else if (row .eq. k1) then
                          a = ax (p)
c                      else
c                         this is an invalid entry, below the diagonal
c                         block.  it will be detected (and optionally
c                         printed) in the call to ums2of below.
                       endif
60                  continue
 
                    ihead = 1
                    xhead = 1
                 endif
 
              else
 
c                -------------------------------------------------------
c                the block is located in ii/xx (cp (k2+1) ... cp (k1)-1)
c                and has already been converted to arrowhead form
c                -------------------------------------------------------
 
                 if (blk .eq. 1) then
c                   this is the last block to factorize
                    cp (k2+1) = nzoff + 1
                 else
                    cp (k2+1) = cp (blkp (blk-1))
                 endif
 
                 ihead = cp (k1)
                 xhead = ihead
 
                 if (kn .eq. 1) then
c                   singleton block in ii/xx (cp (k1+1) ... cp (k1)-1)
                    if (cp (k1) .gt. cp (k1+1)) then
                       a = xx (cp (k1) - 1)
                       ihead = ihead - 1
                       xhead = xhead - 1
                       iuse = iuse - 1
                       xuse = xuse - 1
                    endif
                 endif
 
              endif
 
c             ----------------------------------------------------------
c             factor the block
c             ----------------------------------------------------------
 
              if (kn .gt. 1) then
 
c                -------------------------------------------------------
c                the a (k1..k2, k1..k2) block is not a singleton.
c                block is now in ii/xx (cp (k2+1) ... cp (k1)-1), in
c                arrowhead form, and is to be overwritten with lu
c                -------------------------------------------------------
 
                 arnz = cp (k1) - 1
 
c                if (presrv) then
c                   ii/xx (1..arnz) holds just the current block, blk
c                else
c                   ii/xx (1..arnz) holds the off-diagonal part, and
c                   blocks 1..blk, in that order.
c                endif
 
                 luip = lublkp (blk)
c                luxp = lui (luip), not needed for refactorization
                 nlu = lui (luip+1)
c                npiv = lui (luip+2), not needed for refactorization
                 mc = lui (luip+3)
                 mr = lui (luip+4)
                 wrksiz = 2*kn + mr + 3*mc + 4*(nlu+2)
                 itail = itail - wrksiz
                 iuse = iuse + wrksiz
                 p = itail
                 info (18) = max (info (18), iuse)
                 if (ihead .gt. itail) then
c                   error return, if not enough integer memory:
                    go to 9000
                 endif
 
                 call ums2r2 (cp (k1), arnz, kn, xtail,
     $                xx, xtail-1, xuse, ii, cperm (k1), rperm (k1),
     $                icntl, cntl, info, rinfo, mc, mr,
     $                ii (p), ii (p+kn), ii (p+2*kn), ii (p+2*kn+mr),
     $                ii (p+2*kn+mr+mc), ii (p+2*kn+mr+2*mc),
     $                ii (p+2*kn+mr+3*mc), ii (p+2*kn+mr+3*mc+(nlu+2)),
     $                ii (p+2*kn+mr+3*mc+2*(nlu+2)),
     $                ii (p+2*kn+mr+3*mc+3*(nlu+2)),
     $                nlu, lui (luip+5), lui (luip+nlu+5), noutsd,
     $                xrmax)
 
                 if (info (1) .lt. 0) then
c                   error return, if not enough real memory or bad pivot
                    go to 9010
                 endif
 
c                -------------------------------------------------------
c                deallocate workspace and original matrix (reals
c                already deallocated in ums2r2)
c                -------------------------------------------------------
 
                 iuse = iuse - wrksiz
                 itail = itail + wrksiz
                 lui (luip) = xtail
                 iuse = iuse - (ihead - cp (k2+1))
                 ihead = cp (k2+1)
                 xhead = ihead
 
              else
 
c                -------------------------------------------------------
c                factor the singleton a (k1,k1) block, in a
c                -------------------------------------------------------
 
                 nsgltn = nsgltn + 1
                 if (a .eq. zero) then
c                   this is a singular matrix, replace with 1-by-1
c                   identity matrix.
                    a = one
                 else
c                   increment pivot count
                    npiv = npiv + 1
                 endif
                 xtail = xtail - 1
                 xuse = xuse + 1
                 xrmax = max (xrmax, xuse)
                 info (20) = max (info (20), xuse)
                 info (21) = max (info (21), xuse)
c                note: if the matrix is not preserved and nonsingular
c                then we will not run out of memory
                 if (xhead .gt. xtail) then
c                   error return, if not enough real memory:
                    go to 9000
                 endif
 
c                -------------------------------------------------------
c                store the 1-by-1 lu factors
c                -------------------------------------------------------
 
                 xx (xtail) = a
                 lublkp (blk) = -xtail
 
              endif
70         continue
 
c          -------------------------------------------------------------
c          make the index of each block relative to start of lu factors
c          -------------------------------------------------------------
cfpp$ nodepchk l
           do 80 blk = 1, nblks
              if (lublkp (blk) .gt. 0) then
                 lui (lublkp (blk)) = lui (lublkp (blk)) - xtail + 1
              else
c                this is a singleton
                 lublkp (blk) = (-lublkp (blk)) - xtail + 1
              endif
80         continue
 
c          -------------------------------------------------------------
c          store the off-diagonal blocks
c          -------------------------------------------------------------
 
           if (presrv) then
 
c             ----------------------------------------------------------
c             allocate temporary workspace for pr (1..n) at head of ii
c             ----------------------------------------------------------
 
              prp = ihead
              ihead = ihead + n
              iuse = iuse + n
 
c             ----------------------------------------------------------
c             allocate permanent copy of off-diagonal blocks
c             ----------------------------------------------------------
 
              itail = itail - nzoff
              offip = itail
              xtail = xtail - nzoff
              offxp = xtail
              iuse = iuse + nzoff
              xuse = xuse + nzoff
              xrmax = max (xrmax, xuse)
              info (18) = max (info (18), iuse)
              info (20) = max (info (20), xuse)
              info (21) = max (info (21), xuse)
              if (ihead .gt. itail .or. xhead .gt. xtail) then
c                error return, if not enough integer and/or real memory:
                 go to 9000
              endif
 
c             ----------------------------------------------------------
c             re-order the off-diagonal blocks according to pivot perm
c             ----------------------------------------------------------
 
c             use cp as temporary work array:
              mnz = nzoff
              if (nzoff .eq. 0) then
c                offi and offx are not accessed in ums2of.  set offip
c                and offxp to 1 (since offip = itail = isize+1, which
c                can generate an address fault, otherwise).
                 offip = 1
                 offxp = 1
              endif
              call ums2of (cp, n, rperm, cperm, nzoff,
     $             offp, ii (offip), xx (offxp), ii (prp),
     $             icntl, ap, ai, ax, an, anz, presrv, nblks, blkp,
     $             mnz, 2, info, nbelow)
 
c             ----------------------------------------------------------
c             deallocate pr (1..n)
c             ----------------------------------------------------------
 
              ihead = 1
              xhead = 1
              iuse = iuse - n
 
           else
 
c             off-diagonal entries are in ii/xx (1..nzoff); shift down
c             to ii/xx ( ... itail/xtail).  no extra memory needed.
              do 90 i = nzoff, 1, -1
                 ii (itail+i-nzoff-1) = ii (i)
                 xx (xtail+i-nzoff-1) = xx (i)
90            continue
              ihead = 1
              xhead = 1
              itail = itail - nzoff
              xtail = xtail - nzoff
           endif
 
        endif
 
c       ----------------------------------------------------------------
c       clear the flags (negated row/col indices, and negated ludegr/c)
c       ----------------------------------------------------------------
 
        do 100 i = 1, luisiz
           lui (i) = abs (lui (i))
100     continue
 
c-----------------------------------------------------------------------
c  normal and error return
c-----------------------------------------------------------------------
 
c       error return label:
9000    continue
        if (ihead .gt. itail) then
c          set error flag if not enough integer memory
           call ums2er (2, icntl, info, -3, info (18))
        endif
        if (xhead .gt. xtail) then
c          set error flag if not enough real memory
           call ums2er (2, icntl, info, -4, info (21))
        endif
 
c       error return label, for error return from ums2r2:
9010    continue
 
c       cp can now be deallocated in ums2rf:
        iuse = iuse - (n+1)
 
        info (4) = noutsd + nbelow
        nzdia = nzorig - nzoff - noutsd - nbelow
        info (5) = nzoff + nzdia
        info (6) = nzdia
        info (7) = nzoff
        info (8) = nsgltn
        info (9) = nblks
        info (12) = info (10) + info (11) + n + info (7)
 
c       count the number of symmetric pivots chosen.  note that some
c       of these may have been numerically unacceptable.
        nsym = 0
        do 110 k = 1, n
           if (cperm (k) .eq. rperm (k)) then
c             this kth pivot came from the diagonal of a
              nsym = nsym + 1
           endif
110     continue
        info (16) = nsym
 
        info (17) = info (17) + npiv
        rinfo (1) = rinfo (4) + rinfo (5) + rinfo (6)
 
c       set warning flag if entries outside prior pattern are present
        if (info (4) .gt. 0) then
           call ums2er (2, icntl, info, 1, -info (4))
        endif
 
c       set warning flag if matrix is singular
        if (info (1) .ge. 0 .and. info (17) .lt. n) then
           call ums2er (2, icntl, info, 4, info (17))
        endif
 
c       ----------------------------------------------------------------
c       return memory usage estimate for next call to ums2rf
c       ----------------------------------------------------------------
 
        info (23) = xrmax
 
        return
        end
 
        subroutine ums2r2 (cp, nz, n, xtail, xx, xsize, xuse, ari,
     $          cperm, rperm, icntl, cntl, info, rinfo, mc, mr,
     $          wir, wic, wpr, wpc, wm, wj, frdimc, frxp, frnext,
     $          frprev, nlu, lup, lui, noutsd, xrmax)
        integer xsize, icntl (20), info (40), cperm (n), rperm (n),
     $          xtail, nz, n, ari (nz), cp (n+1), mr, mc, noutsd,
     $          wir (n), wic (n), wpr (mr), xrmax, wpc (mc), wm (mc),
     $          nlu, frdimc (nlu+2), frxp (nlu+2), xuse, wj (mc),
     $          frnext (nlu+2), frprev (nlu+2), lup (nlu), lui (*)
        real
     $          xx (xsize), cntl (10), rinfo (20)
 
c=== ums2r2 ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  ums2r2 refactorizes the n-by-n input matrix at the head of xx
c  (in arrowhead form) and places its lu factors at the tail of
c  xx.  the input matrix is overwritten.   no btf information is
c  used in this routine.
 
c=======================================================================
c  input:
c=======================================================================
c
c       cp (1..n+1):    column pointers of arrowhead form
c       n:              order of input matrix
c       nz:             entries in input matrix
c       xsize:          size of xx
c       icntl:          integer control parameters, see ums2in
c       cntl:           real control parameters, see ums2in
c
c       ari (1..nz):            arrowhead format of a
c       xx (1..nz):             arrowhead format of a, see below
c       xx (nz+1..xsize):       undefined on input, used as workspace
c
c       nlu:            number of lu arrowheads
c       lup (1..nlu):   pointers to lu arrowheads in lui
c       lui (1.. ):     lu arrowheads
c
c       xuse:           memory usage in value
c
c       noutsd:         entries not in prior lu pattern
c
c       cperm (1..n):   column permutation
c       rperm (1..n):   row permutation
 
c=======================================================================
c  workspace:
c=======================================================================
c
c       wir (1..n)
c       wic (1..n)
c
c       wpr (1.. max ludegr)
c       wpc (1.. max ludegc)
c       wm  (1.. max ludegc)
c       wj  (1.. max ludegc)
c
c       frdimc (1..nlu+2)
c       frxp   (1..nlu+2)
c       frnext (1..nlu+2)
c       frprev (1..nlu+2)
 
c=======================================================================
c  output:
c=======================================================================
c
c       lui (1..):              lu arrowheads, modified luxp pointers
c       xx (1..xtail-1):        undefined on output
c       xx (xtail..xsize):      lu factors of this matrix, see below
c
c       info:           integer informational output, see ums2fa
c       rinfo:          real informational output, see ums2fa
c
c       xuse:           memory usage in value
c
c       noutsd:         entries not in prior lu pattern, incremented
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2r0
c       subroutines called:     ums2er, ums2p2, ums2rg, sgemv,
c                               sgemm, strsv, strsm
c       functions called:       abs, max
        intrinsic abs, max
 
c=======================================================================
c  description of data structures:
c=======================================================================
 
c-----------------------------------------------------------------------
c  matrix being factorized:
c-----------------------------------------------------------------------
c
c  the input matrix is held in an arrowhead format.  for the kth pivot,
c  the nonzeros in the pivot row (a (k, k...n)) and pivot column
c  (a (k...n, k)) are stored in the kth arrowhead.  the kth arrowhead
c  is located in:
c       ari (cp (k+1) ... cp (k)-1):    pattern
c       xx  (cp (k+1) ... cp (k)-1):    values
c
c  suppose p is in the range cp (k+1) to cp (k)-1.  if ari (p) is
c  greater than zero, then the entry is in row ari (p), column k,
c  with value xx (p).  if ari (p) is less than zero, then the entry is
c  in row k, column -ari (p), with value xx (p).  the arrowheads are
c  stored in reverse order (arrowhead n, n-1, ... 2, 1) in ari and xx.
c  note that cp (n+1) = 1 unless btf is in use and the original matrix
c  is not preserved.   in all cases, the real part of the arrowhead
c  format (xx (cp (n+1) ... cp (1)-1)) is overwritten with the lu
c  factors.  the integer part (ari (cp (n+1) ... cp (1)-1)) is not
c  overwritten, since ums2r2 does not require dynamic allocation of
c  integer memory.
 
c-----------------------------------------------------------------------
c  frontal matrices
c-----------------------------------------------------------------------
c
c   each unassembled frontal matrix (element) is stored as follows:
c       total size: fscal integers, (fdimr*fdimc) reals
c
c       if e is an unassembled element, and not the current frontal
c       matrix:
c
c       fluip = lup (e) pointer to lu arrowhead in ii
c       fdimc = frdimc (e)      column dimension of contribution block
c       fxp   = frxp (e)        pointer to contribution block in xx
c       next  = frnext (e)      pointer to next block in xx
c       prev  = frprev (e)      pointer to previous block in xx
c       fdegr = abs (lui (fluip+2))
c       fdegc = abs (lui (fluip+2))
c       xx (fxp ... )
c               a 2-dimensional array, c (1..fdimc, 1..fdimr), where
c               fdimr = fdegr if the contribution block is compressed,
c               or fdimr = lui (fluip+5) if not.  note, however, that
c               fdimr is not needed.  the contribution block is stored
c               in c (1..fdegc, 1..fdegr) in the c (1..fdimc,...) array.
c
c               if memory is limited, garbage collection will occur.
c               in this case, the c (1..fdimc, 1..fdimr) array is
c               compressed to be just large enough to hold the
c               unassembled contribution block,
c               c (1..fdegc, 1..fdegr).
 
c-----------------------------------------------------------------------
c  current frontal matrix
c-----------------------------------------------------------------------
c
c  ffxp points to current frontal matrix (contribution block and lu
c  factors).  for example, if fflefc = 4, fflefr = 6, luk = 3,
c  ffdimc = 8, ffdimr = 12, then "x" is a term in the contribution
c  block, "l" in l1, "u" in u1, "l" in l2, "u" in u2, and "." is unused.
c  xx (fxp) is "x". the first 3 pivot values (diagonal entries in u1)
c  are labelled 1, 2, and 3.  the frontal matrix is ffdimc-by-ffdimr.
c
c                   |----------- col 1 of l1 and l2, etc.
c                   v
c       x x x x x x l l l . . .
c       x x x x x x l l l . . .
c       x x x x x x l l l . . .
c       x x x x x x l l l . . .
c       u u u u u u 3 l l . . .         <- row 3 of u1 and u2
c       u u u u u u u 2 l . . .         <- row 2 of u1 and u2
c       u u u u u u u u 1 . . .         <- row 1 of u1 and u2
c       . . . . . . . . . . . .
 
c-----------------------------------------------------------------------
c  lu factors
c-----------------------------------------------------------------------
c
c   the lu factors are placed at the tail of xx.  if this routine
c   is factorizing a single block, then this description is for the
c   factors of the single block:
c
c       lui (1..):      integer info. for lu factors
c       xx (xtail..xsize):      real values in lu factors
c
c   each lu arrowhead (or factorized element) is stored as follows:
c   ---------------------------------------------------------------
c
c       total size: (7 + ludegc + ludegr + lunson) integers,
c                   (luk**2 + ludegc*luk + luk*ludegc) reals
c
c       if e is an lu arrowhead, then luip = lup (e).
c
c       luxp   = lui (luip) pointer to numerical lu arrowhead
c       luk    = lui (luip+1) number of pivots in lu arrowhead
c       ludegr = lui (luip+2) degree of last row of u (excl. diag)
c       ludegc = lui (luip+3) degree of last col of l (excl. diag)
c       lunson = lui (luip+4) number of children in assembly dag
c       ffdimr = lui (luip+5)
c       ffdimc = lui (luip+6)
c                       max front size for this lu arrowhead is
c                       ffdimr-by-ffdimc, or zero if this lu arrowhead
c                       factorized within the frontal matrix of a prior
c                       lu arrowhead.
c       lucp   = (luip + 7)
c                       pointer to pattern of column of l
c       lurp   = lucp + ludegc
c                       pointer to patter of row of u
c       lusonp = lurp + ludegr
c                       pointer to list of sons in the assembly dag
c       lui (lucp ... lucp + ludegc - 1)
c                       row indices of column of l
c       lui (lurp ... lurp + ludegr - 1)
c                       column indices of row of u
c       lui (lusonp ... lusonp + lunson - 1)
c                       list of sons
c       xx (luxp...luxp + luk**2 + ludegc*luk + luk*ludegr - 1)
c                       pivot block (luk-by-luk) and the l block
c                       (ludegc-by-luk) in a single (luk+ludegc)-by-luk
c                       array, followed by the u block in a
c                       luk-by-ludegr array.
c
c   pivot column/row pattern (also columns/rows in contribution block):
c       if the column/row index is negated, the column/row has been
c       assembled out of the frontal matrix into a subsequent frontal
c       matrix.  after factorization, the negative flags are removed.
c
c   list of sons:
c       1 <= son <= n:           son an luson
c       n+1 <= son <= 2n:        son-n is an uson
c       2n+n <= son <= 3n:       son-2n is a lson
 
c-----------------------------------------------------------------------
c  workspaces:
c-----------------------------------------------------------------------
c
c  wpc (1..ludegr):     holds the pivot column pattern
c                       (excluding the pivot row indices)
c
c  wpr (1..ludegr):     holds the pivot row pattern
c                       (excluding the pivot column indices)
c
c  wir (row) >= 0 for each row in pivot column pattern.
c               offset into pattern is given by:
c               wir (row) == offset - 1
c               otherwise, wir (1..n) is < 0
c
c  wic (col) >= 0 for each col in pivot row pattern.
c               wic (col) == (offset - 1) * ffdimc
c               otherwise, wic (1..n) is < 0
c
c  wm (1..degc) or wm (1..fdegc):       a gathered copy of wir
c  wj (1..degc) or wj (1..fdegc):       offset in pattern of a son
 
c-----------------------------------------------------------------------
c  memory allocation in xx:
c-----------------------------------------------------------------------
c
c   xx (1..xhead):      values of original entries in arrowheads of
c                       matrix, values of contribution blocks, followed
c                       by the current frontal matrix.
c
c   mtail = nlu+2
c   mhead = nlu+1:      frnext (mhead) points to the first contribution
c                       block in the head of xx.  the frnext and frprev
c                       arrays form a doubly-linked list.  traversing
c                       the list from mhead to mtail gives the
c                       contribution blocks in ascending ordering of
c                       address (frxp).  a block is free if frdimc <= 0.
c                       the largest known free block in xx is pfree,
c                       located in
c                       xx (frxp (pfree) ... frxp (pfree) + xfree -1),
c                       unless pfree = 0, in which case no largest free
c                       block is known.
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer swpcol, swprow, fdimc, k0, colpos, rowpos, pivot, ffpp,
     $          p, i, j, ludegr, ludegc, kpos, sp, ffrp, ffcp, type,
     $          fxp, lurp, lucp, next, fflefr, prev, xhead, fdegr,
     $          fflefc, k, xcdp, xdp, xsp, s, fdegc, flurp, flucp,
     $          col, e, row, mhead, mtail, uxp, luk, io, fluip, lusonp,
     $          ffsize, ffxp, ffdimr, ffdimc, xrdp, npiv, nb, lunson,
     $          xneed, ldimr, ldimc, lxp, prl, xp, luip, pfree, xfree,
     $          xs, luxp, fsp, flp, fdp, degc, nzu, nzl, xruse
        logical pr3, allcol, allrow
        real
     $          one, zero, x
        parameter (one = 1.0, zero = 0.0)
 
c  printing control:
c  -----------------
c  prl:     invalid entries printed if prl >= 3
c  io:      i/o unit for warning messages (printing invalid entries)
c  pr3:     true if invalid entries are to be printed when found
c
c  current working array:
c  ----------------------
c  ffxp:    current working array is in xx (ffxp ... ffxp+ffsize-1)
c  ffsize:  size of current working array in xx
c  ffdimr:  row degree (number of columns) of current working array
c  ffdimc:  column degree (number of rows) of current working array
c  fflefr:  row degree (number of columns) of current contribution block
c  fflefc:  column degree (number of rows) of current contribution block
c  ffrp:     u2 block is in xx (ffrp ...)
c  ffcp:     l2 block is in xx (ffcp ...)
c  ffpp:     location in xx of the current pivot value
c
c  current element:
c  ----------------
c  s:       current element being factorized
c  luip:    current element is in lui (luip ...)
c  luk:     number of pivots in current element
c  ludegc:  degree of pivot column (excluding pivots themselves)
c  ludegr:  degree of pivot row (excluding pivots themselves)
c  ldimr:   row degree (number of columns) of current element
c  ldimc:   column degree (number of row) of current element
c  lucp:    pattern of col(s) of current element in lui (lucp...)
c  lurp:    pattern of row(s) of current element in lui (lurp...)
c  lusonp:  list of sons of current element is in lui (lusonp...)
c  lunson:  number of sons of current element
c  sp:      pointer into list of sons of current element
c  luxp:    numerical values of lu arrowhead stored in xx (luxp ...)
c  lxp:     l2 block is stored in xx (lxp ...) when computed
c  uxp:     u2 block is stored in xx (uxp ...) when computed
c  nzu:     nonzeros above diagonal in u in current lu arrowhead
c  nzl:     nonzeros below diagonal in l in current lu arrowhead
c  swpcol:  the non-pivotal column to be swapped with pivot column
c  swprow:  the non-pivotal row to be swapped with pivot row
c  colpos:  position in wpr of the pivot column
c  rowpos:  position in wpc of the pivot row
c  kpos:    position in c to place pivot row/column
c  k:       current pivot is kth pivot of current element, k=1..luk
c  k0:      contribution block, c, has been updated with pivots 1..k0
c  npiv:    number of pivots factorized so far, excl. current element
c  pivot:   current pivot entry is a (pivot, pivot)
c  xcdp:    current pivot column is in xx (xcdp ...)
c  xrdp:    current pivot row is in xx (xrdp ...)
c
c  son, or element other than current element:
c  -------------------------------------------
c  e:       an element other than s (a son of s, for example)
c  fluip:   lu arrowhead of e is in lui (fluip ...)
c  fxp:     contribution block of son is in xx (fxp ...)
c  fdimc:   leading dimension of contribution block of a son
c  fdegr:   row degree of contribution block of son (number of columns)
c  fdegc:   column degree of contribution block of son (number of rows)
c  allcol:  true if all columns are present in son
c  allrow:  true if all rows are present in son
c  flucp:   pattern of col(s) of son in lui (flucp...)
c  flurp:   pattern of row(s) of son in lui (flurp...)
c  type:    an luson (type = 1), uson (type = 2) or lson (type = 3)
c  degc:    compressed column offset vector of son is in wj/wm (1..degc)
c
c  memory allocation:
c  ------------------
c  mhead:   nlu+1, head pointer for contribution block link list
c  mtail:   nlu+2, tail pointer for contribution block link list
c  prev:    frprev (e) of the element e
c  next:    frnext (e) of the element e
c  pfree:   frxp (pfree) is the largest known free block in xx
c  xfree:   size of largest known free block in xx
c  xneed:   bare minimum memory currently needed in xx
c  xhead:   xx (1..xhead-1) is in use, xx (xhead ..) is free
c  xruse:   estimated memory needed in xx for next call to ums2rf,
c           assuming a modest number of garbage collections
c  xs:      size of a block of memory in xx
c
c  other:
c  ------
c  xdp:     destination pointer, into xx
c  xsp:     source pointer, into xx
c  xp:      a pointer into xx
c  fsp:     source pointer, into xx
c  fsp:     destination pointer, into xx
c  flp:     last row/column in current contribution is in xx (flp...)
c  col:     a column index
c  row:     a row index
c  nb:      block size for tradeoff between level-2 and level-3 blas
c  p, i, j, x:  various uses
 
c=======================================================================
c  executable statements:
c=======================================================================
 
c       ----------------------------------------------------------------
c       get control parameters and initialize various scalars
c       ----------------------------------------------------------------
 
        io = icntl (2)
        prl = icntl (3)
        nb = max (1, icntl (7))
        npiv = 0
        xhead = cp (1)
        xtail = xsize + 1
        xneed = xuse
        xruse = xuse
        xrmax = max (xrmax, xruse)
        mhead = nlu+1
        mtail = nlu+2
        xfree = -1
        pfree = 0
        pr3 = prl .ge. 3 .and. io .ge. 0
 
c       ----------------------------------------------------------------
c       initialize workspaces
c       ----------------------------------------------------------------
 
        do 10 i = 1, n
           wir (i) = -1
           wic (i) = -1
10      continue
 
        do 20 e = 1, nlu+2
           frdimc (e) = 0
           frxp (e) = 0
           frnext (e) = 0
           frprev (e) = 0
20      continue
        frnext (mhead) = mtail
        frprev (mtail) = mhead
        frxp (mhead) = xhead
        frxp (mtail) = xhead
 
c       count the numerical assembly of the original matrix
        rinfo (2) = rinfo (2) + nz
 
c       current working array is empty:
        fflefr = 0
        fflefc = 0
        ffsize = 0
        ffxp = xhead
 
c=======================================================================
c  factorization [
c=======================================================================
 
        do 600 s = 1, nlu
 
c=======================================================================
c  get the next element to factorize
c=======================================================================
 
           luip = lup (s)
           luk = lui (luip+1)
           ludegc = lui (luip+3)
           ludegr = lui (luip+2)
           lunson = lui (luip+4)
           lucp = (luip + 7)
           lurp = lucp + ludegc
           lusonp = lurp + ludegr
           ldimc = luk + ludegc
           ldimr = luk + ludegr
 
c=======================================================================
c  start new frontal matrix or merge with prior contribution block [
c=======================================================================
 
c          =============================================================
           if (lui (luip+6) .ne. 0) then
c          start new contribution block
c          =============================================================
 
c             ----------------------------------------------------------
c             clear the prior offsets
c             ----------------------------------------------------------
 
              do 30 i = 1, fflefr
                 wic (wpr (i)) = -1
30            continue
              do 40 i = 1, fflefc
                 wir (wpc (i)) = -1
40            continue
 
c             ----------------------------------------------------------
c             save prior contribution block (s-1), if it exists
c             ----------------------------------------------------------
 
              xs = fflefr * fflefc
              if (ffsize .ne. 0) then
c                one more frontal matrix is finished
                 xneed = xneed - (ffsize - xs)
                 xruse = xruse - (ffsize - xs)
                 info (13) = info (13) + 1
c             else
c                prior contribution block does not exist
              endif
 
              if (fflefr .le. 0 .or. fflefc .le. 0) then
 
c                -------------------------------------------------------
c                if prior contribution block nonexistent or empty
c                -------------------------------------------------------
 
                 xuse = xuse - (xhead - frxp (mtail))
                 xhead = frxp (mtail)
 
              else
 
c                -------------------------------------------------------
c                prepare the prior contribution block for later assembly
c                -------------------------------------------------------
 
                 e = s - 1
 
c                count the numerical assembly
                 rinfo (2) = rinfo (2) + xs
 
                 if (xs .le. xfree) then
 
c                   ----------------------------------------------------
c                   compress and store in a freed block
c                   ----------------------------------------------------
 
c                   place the new block in the list
                    xfree = xfree - xs
                    if (pfree .eq. mtail) then
c                      place the new block at start of tail block
                       prev = frprev (mtail)
                       next = mtail
                       xdp = frxp (mtail)
                       frxp (mtail) = xdp + xs
                    else
c                      place the new block at end of block
                       prev = pfree
                       next = frnext (pfree)
                       xdp = frxp (next) - xs
                       if (xfree .eq. 0 .and. pfree .ne. mhead) then
c                         delete the free block if its size is zero
                          prev = frprev (prev)
                          pfree = 0
                          xfree = -1
                       endif
                    endif
                    do 60 j = 0, fflefr - 1
cfpp$ nodepchk l
                       do 50 i = 0, fflefc - 1
                          xx (xdp+j*fflefc+i) = xx (ffxp+j*ffdimc+i)
50                     continue
60                  continue
                    xuse = xuse - (xhead - frxp (mtail))
                    xhead = frxp (mtail)
                    frxp (e) = xdp
                    frdimc (e) = fflefc
 
                 else
 
c                   ----------------------------------------------------
c                   deallocate part of unused portion of frontal matrix
c                   ----------------------------------------------------
 
c                   leave the contribution block c (1:fflefc, 1:fflefr)
c                   at head of xx, with column dimension of ffdimc and
c                   space of size (fflefr-1)*ffdimc for the first
c                   fflefr columns, and fflefc for the last column.
                    xs = ffsize - (fflefc + (fflefr-1)*ffdimc)
                    xhead = xhead - xs
                    xuse = xuse - xs
                    prev = frprev (mtail)
                    next = mtail
                    frxp (mtail) = xhead
                    frxp (e) = ffxp
                    frdimc (e) = ffdimc
                 endif
 
                 frnext (prev) = e
                 frprev (next) = e
                 frnext (e) = next
                 frprev (e) = prev
 
              endif
 
              if (pfree .eq. mtail) then
                 pfree = 0
                 xfree = -1
              endif
 
c             ----------------------------------------------------------
c             allocate a new ffdimr-by-ffdimc frontal matrix
c             ----------------------------------------------------------
 
              ffdimc = lui (luip+6)
              ffdimr = lui (luip+5)
              ffsize = ffdimr * ffdimc
              ffxp = 0
 
c             ----------------------------------------------------------
c             allocate and zero the space, garbage collection if needed
c             ----------------------------------------------------------
 
              if (ffsize .gt. xtail-xhead) then
                 info (15) = info (15) + 1
                 call ums2rg (xx, xsize, xhead, xtail, xuse,
     $              lui, frdimc, frxp, frnext, frprev, nlu, lup,
     $              icntl, ffxp, ffsize, pfree, xfree)
              endif
 
              ffxp = xhead
              xhead = xhead + ffsize
              xuse = xuse + ffsize
              xneed = xneed + ffsize
              xruse = xruse + ffsize
              xrmax = max (xrmax, xruse)
              info (20) = max (info (20), xuse)
              info (21) = max (info (21), xneed)
              if (xhead .gt. xtail) then
c                error return, if not enough real memory:
                 go to 9000
              endif
 
c             ----------------------------------------------------------
c             zero the frontal matrix
c             ----------------------------------------------------------
 
              do 70 p = ffxp, ffxp + ffsize - 1
                 xx (p) = zero
70            continue
 
c             ----------------------------------------------------------
c             place pivot rows and columns in correct position
c             ----------------------------------------------------------
 
              do 80 k = 1, luk
                 wic (npiv + k) = (ldimr - k) * ffdimc
                 wir (npiv + k) =  ldimc - k
80            continue
 
c             ----------------------------------------------------------
c             get the pivot row pattern of the new lu arrowhead
c             ----------------------------------------------------------
 
              do 90 i = 0, ludegr - 1
                 col = lui (lurp+i)
                 wic (col) = i * ffdimc
                 wpr (i+1) = col
90            continue
 
c             ----------------------------------------------------------
c             get the pivot column pattern of the new lu arrowhead
c             ----------------------------------------------------------
 
              do 100 i = 0, ludegc - 1
                 row = lui (lucp+i)
                 wir (row) = i
                 wpc (i+1) = row
100           continue
 
c          =============================================================
           else
c          merge with prior contribution block
c          =============================================================
 
c             ----------------------------------------------------------
c             prior block is located at xx (ffxp ... ffxp + ffsize - 1).
c             it holds a working array c (1..ffdimc, 1..ffdimr), with a
c             prior contribution block in c (1..fflefc, 1..fflefr).
c             the last pivot column pattern is wpc (1..fflefc), and
c             the last pivot row pattern is wpr (1..fflefr).  the
c             offsets wir and wic are:
c             wir (wpc (i)) = i-1, for i = 1..fflefc, and -1 otherwise.
c             wic (wpr (i)) = (i-1)*ffdimc, for i = 1..fflefr, else -1.
c             the prior lu arrowhead is an implicit luson of the current
c             element (and is implicitly assembled into the same
c             frontal matrix).
c             ----------------------------------------------------------
 
c             ----------------------------------------------------------
c             zero the newly extended frontal matrix
c             ----------------------------------------------------------
 
c             zero the new columns in the contribution and lu blocks
c             c (1..ldimc, fflefr+1..ldimr) = 0
              do 120 j = fflefr, ldimr - 1
                 do 110 i = 0, ldimc - 1
                    xx (ffxp + j*ffdimc + i) = zero
110              continue
120           continue
 
c             c (fflefc+1..ldimc, 1..fflefr) = 0
c             zero the new rows in the contribution and u blocks
              do 140 i = fflefc, ldimc - 1
cfpp$ nodepchk l
                 do 130 j = 0, fflefr - 1
                    xx (ffxp + j*ffdimc + i) = zero
130              continue
140           continue
 
c             ----------------------------------------------------------
c             move pivot rows and columns into correct position
c             ----------------------------------------------------------
 
              do 220 k = 1, luk
 
c                -------------------------------------------------------
c                kth pivot of frontal matrix, (npiv+k)th pivot of lu
c                -------------------------------------------------------
 
                 pivot = npiv + k
 
c                -------------------------------------------------------
c                move the kth pivot column into position
c                -------------------------------------------------------
 
                 xsp = wic (pivot)
                 kpos = ldimr - k + 1
                 xdp = (kpos - 1) * ffdimc
                 wic (pivot) = xdp
 
                 if (xsp .ge. 0) then
c                   pivot column is already in current frontal matrix,
c                   shift into proper position
                    colpos = (xsp / ffdimc) + 1
                    fsp = ffxp + xsp
                    fdp = ffxp + xdp
 
                    if (fflefr .lt. kpos) then
 
                       if (fflefr .eq. colpos) then
 
c                         ----------------------------------------------
c                         move c(:,colpos) => c (:,kpos)
c                         c (:,colpos) = 0
c                         ----------------------------------------------
cfpp$ nodepchk l
                          do 150 i = 0, ldimc - 1
                             xx (fdp+i) = xx (fsp+i)
                             xx (fsp+i) = zero
150                       continue
 
                       else
 
c                         ----------------------------------------------
c                         move c(:,colpos) => c (:,kpos)
c                         move c(:,fflefr) => c (:,colpos)
c                         c (:,fflefr) = 0
c                         ----------------------------------------------
 
                          flp = ffxp + (fflefr - 1) * ffdimc
cfpp$ nodepchk l
                          do 160 i = 0, ldimc - 1
                             xx (fdp+i) = xx (fsp+i)
                             xx (fsp+i) = xx (flp+i)
                             xx (flp+i) = zero
160                       continue
 
                          swpcol = wpr (fflefr)
                          wpr (colpos) = swpcol
                          wic (swpcol) = xsp
                       endif
 
                    else if (colpos .ne. kpos) then
 
c                      -------------------------------------------------
c                      swap c (:,colpos) <=> c (:,kpos)
c                      -------------------------------------------------
cfpp$ nodepchk l
                       do 180 i = 0, ldimc - 1
                          x = xx (fdp+i)
                          xx (fdp+i) = xx (fsp+i)
                          xx (fsp+i) = x
180                    continue
 
                       swpcol = wpr (kpos)
                       wpr (colpos) = swpcol
                       wic (swpcol) = xsp
                    endif
 
                    fflefr = fflefr - 1
                 endif
 
c                -------------------------------------------------------
c                move the kth pivot row into position
c                -------------------------------------------------------
 
                 xsp = wir (pivot)
                 kpos = ldimc - k + 1
                 xdp = (kpos - 1)
                 wir (pivot) = xdp
 
                 if (xsp .ge. 0) then
c                   pivot row is already in current frontal matrix,
c                   shift into proper position
                    rowpos = xsp + 1
                    fsp = ffxp + xsp
                    fdp = ffxp + xdp
 
                    if (fflefc .lt. kpos) then
 
                       if (fflefc .eq. rowpos) then
 
c                         ----------------------------------------------
c                         move c(rowpos,:) => c (kpos,:)
c                         c (rowpos,:) = 0
c                         ----------------------------------------------
cfpp$ nodepchk l
                          do 190 j = 0, (ldimr - 1) * ffdimc, ffdimc
                             xx (fdp+j) = xx (fsp+j)
                             xx (fsp+j) = zero
190                       continue
 
                       else
 
c                         ----------------------------------------------
c                         move c(rowpos,:) => c (kpos,:)
c                         move c(fflefc,:) => c (rowpos,:)
c                         c (fflefc,:) = 0
c                         ----------------------------------------------
 
                          flp = ffxp + (fflefc - 1)
cfpp$ nodepchk l
                          do 200 j = 0, (ldimr - 1) * ffdimc, ffdimc
                             xx (fdp+j) = xx (fsp+j)
                             xx (fsp+j) = xx (flp+j)
                             xx (flp+j) = zero
200                       continue
 
                          swprow = wpc (fflefc)
                          wpc (rowpos) = swprow
                          wir (swprow) = xsp
                       endif
 
                    else if (rowpos .ne. kpos) then
 
c                      -------------------------------------------------
c                      swap c (rowpos,:) <=> c (kpos,:)
c                      -------------------------------------------------
cfpp$ nodepchk l
                       do 210 j = 0, (ldimr - 1) * ffdimc, ffdimc
                          x = xx (fdp+j)
                          xx (fdp+j) = xx (fsp+j)
                          xx (fsp+j) = x
210                    continue
 
                       swprow = wpc (kpos)
                       wpc (rowpos) = swprow
                       wir (swprow) = xsp
                    endif
 
                    fflefc = fflefc - 1
                 endif
 
220           continue
 
c             ----------------------------------------------------------
c             merge with pivot row pattern of new lu arrowhead
c             ----------------------------------------------------------
 
              i = fflefr
              do 230 p = lurp, lurp + ludegr - 1
                 col = lui (p)
                 if (wic (col) .lt. 0) then
                    wic (col) = i * ffdimc
                    i = i + 1
                    wpr (i) = col
                 endif
230           continue
 
c             ----------------------------------------------------------
c             merge with pivot column pattern of new lu arrowhead
c             ----------------------------------------------------------
 
              i = fflefc
              do 240 p = lucp, lucp + ludegc - 1
                 row = lui (p)
                 if (wir (row) .lt. 0) then
                    wir (row) = i
                    i = i + 1
                    wpc (i) = row
                 endif
240           continue
 
           endif
 
c=======================================================================
c  done initializing frontal matrix ]
c=======================================================================
 
c=======================================================================
c  assemble original arrowheads into the frontal matrix, and deallocate
c=======================================================================
 
c          -------------------------------------------------------------
c          current workspace usage:
c          -------------------------------------------------------------
 
c          wpc (1..ludegr):     holds the pivot column pattern
c                               (excluding the pivot row indices)
c
c          wpr (1..ludegr):     holds the pivot row pattern
c                               (excluding the pivot column indices)
c
c          c (1..ffdimr, 1..ffdimc):  space for the frontal matrix,
c               in xx (ffxp ... ffxp + ffsize - 1)
c
c          c (i,j) is located at xx (ffxp+((i)-1)+((j)-1)*ffdimc)
c
c          c (1..ludegc, 1..ludegr):            contribution block
c          c (ludegc+1..ludegc+luk, 1..ludegr):             u2 block
c          c (1..ludegc, ludegr+1..ludegr+luk):             l2 block
c          c (ludegc+1..ludegc+luk, ludegr+1..ludegr+luk):  l1\u1 block
c
c          wir (row) >= 0 for each row in pivot column pattern.
c               offset into pattern is given by:
c               wir (row) == offset - 1
c               also, wir (npiv+1 ... npiv+luk) is
c               ludegc+luk-1 ... ludegc, the offsets of the pivot rows.
c
c               otherwise, wir (1..n) is < 0
c
c          wic (col) >= 0 for each col in pivot row pattern.
c               wic (col) == (offset - 1) * ffdimc
c               also, wic (npiv+1 ... npiv+luk) is
c               ludegr+luk-1 ... ludegr, the offsets of the pivot rows.
c
c               otherwise, wic (1..n) is < 0
 
           do 260 k = 1, luk
              i = npiv + k
              xcdp = ffxp + wic (i)
              xrdp = ffxp + wir (i)
              do 250 p = cp (i+1), cp (i) - 1
                 j = ari (p)
                 if (j .gt. 0) then
c                   a diagonal entry, or lower triangular entry
c                   row = j, col = i
                    xp = xcdp + wir (j)
                    if (xp .lt. xcdp) then
c                      invalid entry - not in prior lu pattern
                       noutsd = noutsd + 1
                       if (pr3) then
c                         get original row and column index and print it
                          row = rperm (j)
                          col = cperm (i)
                          call ums2p2 (2, 97, row, col, xx (p), io)
                       endif
                    else
                       xx (xp) = xx (xp) + xx (p)
                    endif
                 else
c                   an upper triangular entry
c                   row = i, col = -j
                    xp = xrdp + wic (-j)
                    if (xp .lt. xrdp) then
c                      invalid entry - not in prior lu pattern
                       noutsd = noutsd + 1
                       if (pr3) then
c                         get original row and column index and print it
                          row = rperm (i)
                          col = cperm (-j)
                          call ums2p2 (2, 97, row, col, xx (p), io)
                       endif
                    else
                       xx (xp) = xx (xp) + xx (p)
                    endif
                 endif
250           continue
260        continue
 
c          deallocate the original arrowheads
           p = cp (npiv + luk + 1)
           xs = cp (npiv + 1) - p
           frxp (mhead) = p
           xneed = xneed - xs
           if (xs .gt. xfree) then
              xfree = xs
              pfree = mhead
           endif
 
c=======================================================================
c  assemble lusons, usons, and lsons into the frontal matrix [
c=======================================================================
 
           do 480 sp = lusonp, lusonp + lunson - 1
 
c             ----------------------------------------------------------
c             get the son and determine its type (luson, uson, or lson)
c             ----------------------------------------------------------
 
              e = lui (sp)
              if (e .le. n) then
c                luson
                 type = 1
              else if (e .le. 2*n) then
c                uson
                 e = e - n
                 type = 2
              else
c                lson
                 e = e - 2*n
                 type = 3
              endif
 
c             ----------------------------------------------------------
c             if fdimc=0 this is the implicit luson (already assembled)
c             ----------------------------------------------------------
 
              fdimc = frdimc (e)
              if (fdimc .ne. 0) then
 
c                -------------------------------------------------------
c                get scalar info of the son (it needs assembling)
c                -------------------------------------------------------
 
                 fxp = frxp (e)
                 fluip = lup (e)
                 fdegr = lui (fluip+2)
                 fdegc = lui (fluip+3)
                 allcol = fdegr .gt. 0
                 allrow = fdegc .gt. 0
                 fdegr = abs (fdegr)
                 fdegc = abs (fdegc)
                 flucp = (fluip + 7)
                 flurp = flucp + fdegc
 
c                use wm (1..fdegc) for offsets:
 
c                -------------------------------------------------------
                 if (type .eq. 1) then
c                this is an luson - assemble an entire frontal matrix
c                -------------------------------------------------------
 
c                   ----------------------------------------------------
                    if (allrow) then
c                   no rows assembled out of this luson yet
c                   ----------------------------------------------------
 
c                      compute the compressed column offset vector
                       do 270 i = 0, fdegc-1
                          row = lui (flucp+i)
                          wm (i+1) = wir (row)
270                    continue
 
c                      -------------------------------------------------
                       if (allcol) then
c                      no rows or cols assembled out of luson yet
c                      -------------------------------------------------
 
                          do 290 j = 0, fdegr-1
                             col = lui (flurp+j)
                             xdp = ffxp + wic (col)
cfpp$ nodepchk l
                             do 280 i = 0, fdegc-1
                                xx (xdp + wm (i+1)) =
     $                          xx (xdp + wm (i+1)) +
     $                          xx (fxp + j*fdimc + i)
280                          continue
290                       continue
 
c                      -------------------------------------------------
                       else
c                      some columns already assembled out of luson
c                      -------------------------------------------------
 
                          do 310 j = 0, fdegr-1
                             col = lui (flurp+j)
                             if (col .gt. 0) then
                                xdp = ffxp + wic (col)
cfpp$ nodepchk l
                                do 300 i = 0, fdegc-1
                                   xx (xdp + wm (i+1)) =
     $                             xx (xdp + wm (i+1)) +
     $                             xx (fxp + j*fdimc + i)
300                             continue
                             endif
310                       continue
 
                       endif
 
c                   ----------------------------------------------------
                    else
c                   some rows already assembled out of luson
c                   ----------------------------------------------------
 
c                      compute the compressed column offset vector
                       degc = 0
                       do 320 i = 0, fdegc-1
                          row = lui (flucp+i)
                          if (row .gt. 0) then
                             degc = degc + 1
                             wj (degc) = i
                             wm (degc) = wir (row)
                          endif
320                    continue
 
c                      -------------------------------------------------
                       if (allcol) then
c                      some rows already assembled out of luson
c                      -------------------------------------------------
 
                          do 340 j = 0, fdegr-1
                             col = lui (flurp+j)
                             xdp = ffxp + wic (col)
cfpp$ nodepchk l
                             do 330 i = 1, degc
                                xx (xdp + wm (i)) =
     $                          xx (xdp + wm (i)) +
     $                          xx (fxp + j*fdimc + wj (i))
330                          continue
340                       continue
 
c                      -------------------------------------------------
                       else
c                      rows and columns already assembled out of luson
c                      -------------------------------------------------
 
                          do 360 j = 0, fdegr-1
                             col = lui (flurp+j)
                             if (col .gt. 0) then
                                xdp = ffxp + wic (col)
cfpp$ nodepchk l
                                do 350 i = 1, degc
                                   xx (xdp + wm (i)) =
     $                             xx (xdp + wm (i)) +
     $                             xx (fxp + j*fdimc + wj (i))
350                             continue
                             endif
360                       continue
 
                       endif
                    endif
 
c                   ----------------------------------------------------
c                   deallocate the luson frontal matrix
c                   ----------------------------------------------------
 
                    frdimc (e) = 0
                    prev = frprev (e)
                    next = frnext (e)
                    xneed = xneed - fdegr*fdegc
                    xruse = xruse - fdegr*fdegc
 
                    if (frdimc (prev) .le. 0) then
c                      previous block is free - delete this block
                       frnext (prev) = next
                       frprev (next) = prev
                       e = prev
                       prev = frprev (e)
                    endif
 
                    if (frdimc (next) .le. 0) then
c                      next block is free - delete this block
                       frxp (next) = frxp (e)
                       if (e .le. nlu) then
                          frnext (prev) = next
                          frprev (next) = prev
                       endif
                       e = next
                       next = frnext (e)
                       if (frnext (mhead) .eq. mtail) then
c                         no blocks left except mhead and mtail
                          frxp (mtail) = frxp (mhead)
                       endif
                    endif
 
c                   get the size of the freed block
                    if (next .eq. 0) then
c                      this is the mtail block
                       xs = ffxp - frxp (e)
                    else
                       xs = frxp (next) - frxp (e)
                    endif
                    if (xs .gt. xfree) then
c                      keep track of the largest free block
                       xfree = xs
                       pfree = e
                    endif
 
c                -------------------------------------------------------
                 else if (type .eq. 2) then
c                uson:  assemble all possible columns
c                -------------------------------------------------------
 
c                   ----------------------------------------------------
                    if (allrow) then
c                   no rows assembled out of this uson yet
c                   ----------------------------------------------------
 
c                      compute the compressed column offset vector
                       do 370 i = 0, fdegc-1
                          row = lui (flucp+i)
                          wm (i+1) = wir (row)
370                    continue
 
                       do 390 j = 0, fdegr-1
                          col = lui (flurp+j)
                          if (col .gt. 0) then
                             if (wic (col) .ge. 0) then
                                xdp = ffxp + wic (col)
cfpp$ nodepchk l
                                do 380 i = 0, fdegc-1
                                   xx (xdp + wm (i+1)) =
     $                             xx (xdp + wm (i+1)) +
     $                             xx (fxp + j*fdimc + i)
380                             continue
c                               flag this column as assembled
                                lui (flurp+j) = -col
                             endif
                          endif
390                    continue
 
c                   ----------------------------------------------------
                    else
c                   some rows already assembled out of this uson
c                   ----------------------------------------------------
 
c                      compute the compressed column offset vector
                       degc = 0
                       do 400 i = 0, fdegc-1
                          row = lui (flucp+i)
                          if (row .gt. 0) then
                             degc = degc + 1
                             wj (degc) = i
                             wm (degc) = wir (row)
                          endif
400                    continue
 
                       do 420 j = 0, fdegr-1
                          col = lui (flurp+j)
                          if (col .gt. 0) then
                             if (wic (col) .ge. 0) then
                                xdp = ffxp + wic (col)
cfpp$ nodepchk l
                                do 410 i = 1, degc
                                   xx (xdp + wm (i)) =
     $                             xx (xdp + wm (i)) +
     $                             xx (fxp + j*fdimc + wj (i))
410                             continue
c                               flag this column as assembled
                                lui (flurp+j) = -col
                             endif
                          endif
420                    continue
 
                    endif
 
c                   flag this element as missing some columns
                    lui (fluip+2) = -fdegr
 
c                -------------------------------------------------------
                 else
c                lson:  assemble all possible rows
c                -------------------------------------------------------
 
c                   compute the compressed column offset vector
                    degc = 0
                    do 430 i = 0, fdegc-1
                       row = lui (flucp+i)
                       if (row .gt. 0) then
                          if (wir (row) .ge. 0) then
c                            this row will be assembled in loop below
                             degc = degc + 1
                             wj (degc) = i
                             wm (degc) = wir (row)
c                            flag this row as assembled
                             lui (flucp+i) = -row
                          endif
                       endif
430                 continue
 
c                   ----------------------------------------------------
                    if (allcol) then
c                   no columns assembled out of this lson yet
c                   ----------------------------------------------------
 
                       do 450 j = 0, fdegr-1
                          col = lui (flurp+j)
                          xdp = ffxp + wic (col)
cfpp$ nodepchk l
                          do 440 i = 1, degc
                             xx (xdp + wm (i)) =
     $                       xx (xdp + wm (i)) +
     $                       xx (fxp + j*fdimc + wj (i))
440                       continue
450                    continue
 
c                   ----------------------------------------------------
                    else
c                   some columns already assembled out of this lson
c                   ----------------------------------------------------
 
                       do 470 j = 0, fdegr-1
                          col = lui (flurp+j)
                          if (col .gt. 0) then
                             xdp = ffxp + wic (col)
cfpp$ nodepchk l
                             do 460 i = 1, degc
                                xx (xdp + wm (i)) =
     $                          xx (xdp + wm (i)) +
     $                          xx (fxp + j*fdimc + wj (i))
460                          continue
                          endif
470                    continue
 
                    endif
 
c                   flag this element as missing some rows
                    lui (fluip+3) = -fdegc
 
                 endif
 
              endif
 
480        continue
 
c=======================================================================
c  done assemblying sons into the frontal matrix ]
c=======================================================================
 
c=======================================================================
c  factorize the frontal matrix [
c=======================================================================
 
           k0 = 0
           fflefr = ldimr
           fflefc = ldimc
           ffcp = ffxp + fflefr * ffdimc
           ffrp = ffxp + fflefc
           ffpp = ffxp + fflefc + fflefr * ffdimc
 
           do 500 k = 1, luk
 
c             ----------------------------------------------------------
c             compute kth column of u1, and update pivot column
c             ----------------------------------------------------------
 
              if (k-k0-2 .gt. 0) then
c                u1 = l1 \ u1.  note that l1 transpose is stored, and
c                that u1 is stored with rows in reverse order.
                 call strsv ('u', 'n', 'u', k-k0-1,
     $                         xx (ffpp         ), ffdimc,
     $                         xx (ffpp - ffdimc), 1)
                 rinfo (5) = rinfo (5) + (k-k0-2)*(k-k0-1)
              endif
              if (k-k0-1 .gt. 0) then
c                l1 = l1 - l2*u1
                 call sgemv ('n', fflefc, k-k0-1,
     $                   -one, xx (ffcp         ), ffdimc,
     $                         xx (ffpp - ffdimc), 1,
     $                    one, xx (ffcp - ffdimc), 1)
                 rinfo (5) = rinfo (5) + 2*fflefc*(k-k0-1)
              endif
 
              ffcp = ffcp - ffdimc
              ffrp = ffrp - 1
              ffpp = ffpp - ffdimc - 1
              fflefr = fflefr - 1
              fflefc = fflefc - 1
 
c             ----------------------------------------------------------
c             divide pivot column by pivot
c             ----------------------------------------------------------
 
c             k-th pivot in frontal matrix located in xx (ffpp)
              x = xx (ffpp)
              if (abs (x) .eq. zero) then
c                error return, if pivot order from ums2fa not acceptable
                 go to 9010
              endif
              x = one / x
              do 490 p = ffcp, ffcp + fflefc - 1
                 xx (p) = xx (p) * x
490           continue
c             count this as a call to the level-1 blas:
              rinfo (4) = rinfo (4) + fflefc
              info (17) = info (17) + 1
 
c             ----------------------------------------------------------
c             compute u1 (k0+1..k, k..ldimc) and
c             update contribution block: rank-nb, or if last pivot
c             ----------------------------------------------------------
 
              if (k-k0 .ge. nb .or. k .eq. luk) then
                 call strsm ('l', 'u', 'n', 'u', k-k0, fflefr, one,
     $                      xx (ffpp), ffdimc,
     $                      xx (ffrp), ffdimc)
                 call sgemm ('n', 'n', fflefc, fflefr, k-k0,
     $                -one, xx (ffcp ), ffdimc,
     $                      xx (ffrp ), ffdimc,
     $                 one, xx (ffxp), ffdimc)
                 rinfo (6) = rinfo (6) + fflefr*(k-k0-1)*(k-k0)
     $                                         + 2*fflefc*fflefr*(k-k0)
                 k0 = k
              endif
 
500        continue
 
c=======================================================================
c  done factorizing the frontal matrix ]
c=======================================================================
 
c=======================================================================
c  save the new lu arrowhead [
c=======================================================================
 
c          allocate permanent space for the lu arrowhead
           xs = luk*ludegc + luk*ludegr + luk*luk
 
           if (xs .gt. xtail-xhead) then
              info (15) = info (15) + 1
              call ums2rg (xx, xsize, xhead, xtail, xuse,
     $              lui, frdimc, frxp, frnext, frprev, nlu, lup,
     $              icntl, ffxp, ffsize, pfree, xfree)
           endif
 
           xtail = xtail - xs
           luxp = xtail
           xuse = xuse + xs
           xneed = xneed + xs
           xruse = xruse + xs
           xrmax = max (xrmax, xruse)
           info (20) = max (info (20), xuse)
           info (21) = max (info (21), xneed)
           if (xhead .gt. xtail) then
c             error return, if not enough real memory:
              go to 9000
           endif
 
c          save the scalar data of the lu arrowhead
           lui (luip) = luxp
 
c          save column pattern (it may have been rearranged)
           do 510 i = 0, ludegc-1
              lui (lucp+i) = wpc (i+1)
510        continue
 
c          save row pattern (it may have been rearranged)
           do 520 i = 0, ludegr-1
              lui (lurp+i) = wpr (i+1)
520        continue
 
c          move the l1,u1 matrix, compressing the dimension from
c          ffdimc to ldimc.  the lu arrowhead grows on top of stack.
           xp = ffxp + (ldimr-1)*ffdimc + ldimc-1
           do 540 j = 0, luk-1
cfpp$ nodepchk l
              do 530 i = 0, luk-1
                 xx (luxp + j*ldimc + i) = xx (xp - j*ffdimc - i)
530           continue
540        continue
 
c          move l2 matrix, compressing dimension from ffdimc to ldimc
           if (ludegc .ne. 0) then
              lxp = luxp + luk
              xp = ffxp + (ldimr-1)*ffdimc
              do 560 j = 0, luk-1
cfpp$ nodepchk l
                 do 550 i = 0, ludegc-1
                    xx (lxp + j*ldimc + i) = xx (xp - j*ffdimc + i)
550              continue
560           continue
           endif
 
c          move the u2 block.
           if (ludegr .ne. 0) then
              uxp = luxp + luk * ldimc
              xp = ffxp + ldimc-1
              do 580 j = 0, ludegr-1
cfpp$ nodepchk l
                 do 570 i = 0, luk-1
                    xx (uxp + j*luk + i) = xx (xp + j*ffdimc - i)
570              continue
580           continue
           endif
 
c          one more lu arrowhead has been refactorized
           nzu = (luk*(luk-1)/2) + luk*ludegc
           nzl = (luk*(luk-1)/2) + luk*ludegr
           info (10) = info (10) + nzl
           info (11) = info (11) + nzu
 
c          -------------------------------------------------------------
c          clear the pivot row and column offsets
c          -------------------------------------------------------------
 
           do 590 pivot = npiv + 1, npiv + luk
              wir (pivot) = -1
              wic (pivot) = -1
590        continue
           npiv = npiv + luk
 
c=======================================================================
c  done saving the new lu arrowhead ]
c=======================================================================
 
600     continue
 
c=======================================================================
c  factorization complete ]
c=======================================================================
 
c=======================================================================
c  wrap-up:  store lu factors in their final form
c=======================================================================
 
c       ----------------------------------------------------------------
c       flag remaining arrowheads as invalid entries, if prior matrix
c       was singular.  print them if requested.
c       ----------------------------------------------------------------
 
        if (npiv .lt. n) then
           if (pr3) then
              do 620 i = npiv+1, n
                 do 610 p = cp (i+1), cp (i) - 1
                    j = ari (p)
                    if (j .gt. 0) then
c                      a diagonal entry, or lower triangular entry
c                      get original row and column index
                       row = rperm (j)
                       col = cperm (i)
                    else
c                      an upper triangular entry
c                      get original row and column index
                       row = rperm (i)
                       col = cperm (-j)
                    endif
                    call ums2p2 (2, 95, row, col, xx(p), io)
610              continue
620           continue
           endif
           noutsd = noutsd + (cp (npiv+1) - cp (n+1))
        endif
 
c       ----------------------------------------------------------------
c       deallocate all remaining input arrowheads and frontal matrices
c       ----------------------------------------------------------------
 
        if (ffsize .ne. 0) then
           info (13) = info (13) + 1
        endif
        xuse = xuse - (xhead - cp (n+1))
        xneed = xuse
        xhead = cp (n+1)
 
        if (nlu .eq. 0) then
c          lu factors are completely empty (a = 0).
c          add one real, to simplify rest of code.
c          otherwise, some arrays in ums2rf or ums2so would have
c          zero size, which can cause an address fault.
           xtail = xsize
           xuse = xuse + 1
           xruse = xuse
           xneed = xuse
           info (20) = max (info (20), xuse)
           info (21) = max (info (21), xneed)
        endif
 
        if (xhead .le. xtail) then
 
c          -------------------------------------------------------------
c          sufficient memory to complete the factorization
c          -------------------------------------------------------------
 
           if (nlu .eq. 0) then
c             zero the dummy entry, although it won't be accessed:
              xx (xtail) = zero
           endif
 
c          -------------------------------------------------------------
c          update pointers in lu factors
c          -------------------------------------------------------------
 
           do 630 s = 1, nlu
              luip = lup (s)
              luxp = lui (luip)
              lui (luip) = luxp - xtail + 1
630        continue
 
c          -------------------------------------------------------------
c          get memory usage estimate for next call to ums2rf
c          -------------------------------------------------------------
 
           xruse = xuse
           xrmax = max (xrmax, xruse)
           return
 
        endif
 
c=======================================================================
c  error conditions
c=======================================================================
 
c       error return label:
9000    continue
c       out of real memory
        call ums2er (2, icntl, info, -4, info (21))
        return
 
c       error return label:
9010    continue
c       original pivot order computed by ums2fa is no longer acceptable
        call ums2er (2, icntl, info, -6, 0)
        return
        end
 
        subroutine ums2ra (presrv, n, nz, cperm, rperm, pr,
     $          w, nblks, arx, ari, nzoff, nzdia,
     $          icntl, mp, blkp, mi, mx, info, offp, on, nzblk,
     $          cblk, kn, nz2, nbelow)
        integer n, nz, cperm (n), rperm (n), pr (n), kn, w (kn+1),
     $          nblks, nzblk, ari (nzblk), nzoff, nzdia, mp (n+1),
     $          mi (nz), on, icntl (20), blkp (nblks+1), nz2,
     $          info (40), offp (on+1), cblk, nbelow
        logical presrv
        real
     $          arx (nzblk), mx (nz)
 
c=== ums2ra ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  convert a column-oriented matrix into an arrowhead format.
 
c=======================================================================
c  input:
c=======================================================================
c
c       n               size of entire matrix
c       mi (1..nz):     row indices of column form of entire matrix
c       mx (1..nz):     values of column form of entire matrix
c       mp (1..n+1)     column pointers for entire matrix
c       cperm (1..n):   column permutations
c       rperm (1..n):   row permutations
c
c       if nblks > 1 and presrv
c           cblk:               the block to convert
c           kn:                 the size of the block to convert
c       else
c           cblk:               0
c           kn                  n, size of input matrix
 
c=======================================================================
c  output:
c=======================================================================
c
c       if nblks = 1 and not presrv
c
c           nzoff               0
c           nzdia               nz - (entries below in diagonal blocks)
c           nz2                 nzdia
c
c           mi (1..nz2)         arrowheads for the diagonal block
c           mx (1..nz2)
c           ari, arx            used as workspace
c           w (1..n+1)          pointer to each arrowhead in mi/mx
c
c           offp                not accessed
c
c       if nblks = 1 and presrv
c
c           nzoff               0
c           nzdia               nz - (entries below in diagonal blocks)
c           nz2                 nzdia
c
c           mi, mx              not modified
c           ari (1..nz2)        arrowheads for the diagonal block
c           arx (1..nz2)
c           w (1..n+1)          pointer to each arrowhead in ari/arx
c
c           offp                not accessed
c
c       else if nblks > 1 and not presrv
c
c           nzoff               number of entries in off-diagonal part
c           nzdia               number of entries in diagonal blocks
c                               (nz = nzoff + nzdia + entries below
c                               diagonal blocks)
c           nz2                 nzoff + nzdia
c
c           mi (nzoff+1..nz2)   arrowheads for each diagonal block
c           mx (nzoff+1..nz2)
c           ari, arx            used as workspace
c           w (1..n+1)          pointer to each arrowhead in mi/mx
c
c           offp (1..n+1)       row pointers for off-diagonal part
c           mi (1..nzoff)       col indices for off-diagonal part
c           mx (1..nzoff)       values for off-diagonal part
c
c       else (nblks > 1 and presrv)
c
c           nzoff               0
c           nzdia               nonzeros in the diagonal block, cblk
c           nz2                 nzdia
c
c           mi, mx              not modified
c           ari (1..nz2)        arrowheads for the diagonal block, cblk
c           arx (1..nz2)
c           w (1..kn+1)         pointer to each arrowhead in ari/arx
c
c           offp                not accessed
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2r0
c       subroutines called:     ums2of
c       functions called:       min
        intrinsic min
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer i, p, row, col, blk, base, k1, k2, k, b1, b2, k0
 
c  i:       loop index, arrowhead index
c  p:       pointer into column-form input matrix
c  row:     row index
c  col:     column index
c  blk:     current diagonal block
c  base:    where to start the construction of the arrowhead form
c  k1,k2:   current diagonal block is a (k1..k2, k1..k2)
c  k:       loop index, kth pivot
c  b1,b2:   convert blocks b1...b2 from column-form to arrowhead form
c  k0:      convert a (k0+1..., k0+1...) to arrowhead form
 
c=======================================================================
c  executable statements:
c=======================================================================
 
c-----------------------------------------------------------------------
c  if entire matrix is to be converted, then create the off-diagonal
c  part in row-oriented form in ari (1..nzoff) and arx (1..nzoff) and
c  compute inverse row permutation.  otherwise, the inverse row
c  permutation has already been computed.
c-----------------------------------------------------------------------
 
        nzoff = 0
        nbelow = 0
        if (nblks .eq. 1) then
           do 10 k = 1, n
              pr (rperm (k)) = k
10         continue
        else if (nblks .gt. 1 .and. .not. presrv) then
           call ums2of (w, n, rperm, cperm, nzoff,
     $        offp, ari, arx, pr,
     $        icntl, mp, mi, mx, n, nz, .true., nblks, blkp,
     $        nz, 2, info, nbelow)
        endif
 
c-----------------------------------------------------------------------
c  construct the arrowhead form for the diagonal block(s)
c-----------------------------------------------------------------------
 
        do 20 i = 1, kn+1
           w (i) = 0
20      continue
 
        base = nzoff + 1
 
        if (cblk .ne. 0) then
c          convert just cblk
           k0 = blkp (cblk) - 1
           b1 = cblk
           b2 = cblk
        else
c          convert all the block(s)
           k0 = 0
           b1 = 1
           b2 = nblks
        endif
 
        do 80 blk = b1, b2
 
c          -------------------------------------------------------------
c          get the starting and ending indices of this diagonal block
c          -------------------------------------------------------------
 
           if (nblks .gt. 1) then
              k1 = blkp (blk)
              k2 = blkp (blk+1) - 1
           else
              k1 = 1
              k2 = n
           endif
 
c          -------------------------------------------------------------
c          count the number of entries in each arrowhead
c          -------------------------------------------------------------
 
           do 40 col = k1, k2
              do 30 p = mp (cperm (col)), mp (cperm (col) + 1) - 1
                 row = pr (mi (p))
                 if (row .ge. k1 .and. row .le. k2) then
c                   this is in a diagonal block, arrowhead i
                    i = min (row, col) - k0
                    w (i) = w (i) + 1
                 endif
30            continue
40         continue
 
c          -------------------------------------------------------------
c          set pointers to point just past end of each arrowhead
c          -------------------------------------------------------------
 
           w (k2-k0+1) = w (k2-k0) + base
           do 50 i = k2-k0, k1-k0+1, -1
              w (i) = w (i+1) + w (i-1)
50         continue
           w (k1-k0) = w (k1-k0+1)
c          w (i+1-k0) points just past end of arrowhead i in ari/arx
 
c          -------------------------------------------------------------
c          construct arrowhead form, leaving pointers in final state
c          -------------------------------------------------------------
 
           do 70 col = k1, k2
              do 60 p = mp (cperm (col)), mp (cperm (col) + 1) - 1
                 row = pr (mi (p))
                 if (row .ge. k1 .and. row .le. k2) then
                    if (row .ge. col) then
c                      diagonal, or lower triangular part
                       i = col - k0 + 1
                       w (i) = w (i) - 1
                       ari (w (i)) = row - k1 + 1
                       arx (w (i)) = mx (p)
                    else
c                      upper triangular part, flag by negating col
                       i = row - k0 + 1
                       w (i) = w (i) - 1
                       ari (w (i)) = -(col - k1 + 1)
                       arx (w (i)) = mx (p)
                    endif
                 endif
60            continue
70         continue
 
           base = w (k1-k0)
           w (k2-k0+1) = 0
80      continue
 
        w (kn+1) = nzoff + 1
        nzdia = base - nzoff - 1
        nz2 = nzoff + nzdia
 
c       ----------------------------------------------------------------
c       if cblk = 0, the entire matrix has been converted:
c
c          w (i) now points just past end of arrowhead i in ari/arx
c          arrowhead i is located in ari/arx (w (i+1) ... w (i)-1),
c          except for the k2-th arrowhead in each block.  those are
c          located in ari/arx (base ... w (k2) - 1), where base is
c          w (blkp (blk-1)) if blk>1 or w (n+1) = nzoff + 1 otherwise.
c
c       otherwise, just one block has been converted:
c
c          w (i) now points just past end of arrowhead i in ari/arx,
c          where i = 1 is the first arrowhead of this block (not the
c          first arrowhead of the entire matrix).  arrowhead i is
c          located in ari/arx (w (i+1) ... w (i)-1).
c          this option is used only if nblks>1 and presrv is true.
c       ----------------------------------------------------------------
 
c-----------------------------------------------------------------------
c  if not preserved, overwrite column-form with arrowhead form
c-----------------------------------------------------------------------
 
        if (.not. presrv) then
           do 90 i = 1, nz
              mi (i) = ari (i)
              mx (i) = arx (i)
90         continue
        endif
 
        return
        end
 
        subroutine ums2rf (n, ne, job, transa, lvalue, lindex, value,
     $          index, keep, cntl, icntl, info, rinfo)
        integer n, ne, job, lvalue, lindex, index (lindex), keep (20),
     $          icntl (20), info (40)
        real
     $          value (lvalue), cntl (10), rinfo (20)
        logical transa
 
c=== ums2rf ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  given a sparse matrix a, and a sparsity-preserving and numerically-
c  acceptable pivot order and symbolic factorization, compute the lu
c  factors, paq = lu.  uses the sparsity pattern and permutations from
c  a prior factorization by ums2fa or ums2rf.  the matrix a should have
c  the same nonzero pattern as the matrix factorized by ums2fa or
c  ums2rf.  the matrix can have different numerical values.  no
c  variations are made in the pivot order computed by ums2fa.  if a
c  zero pivot is encountered, an error flag is set and the
c  factorization terminates.
c
c  this routine can actually handle any matrix a such that (paq)_ij can
c  be nonzero only if (lu)_ij is be nonzero, where l and u are the lu
c  factors of the matrix factorized by ums2fa.  if btf (block triangular
c  form) is used, entries above the diagonal blocks of (paq)_ij can have
c  an arbitrary sparsity pattern.  entries for which (lu)_ij is not
c  present, or those below the diagonal blocks are invalid and ignored
c  (a warning flag is set and the factorization proceeds without the
c  invalid entries).  a listing of the invalid entries can be printed.
c
c  this routine must be preceded by a call to ums2fa or ums2rf.
c  a call to ums2rf can be followed by any number of calls to ums2so,
c  which solves a linear system using the lu factors computed by this
c  routine or by ums2fa.  a call to ums2rf can also be followed by any
c  number of calls to ums2rf.
 
c=======================================================================
c  arguments:
c=======================================================================
 
c           ------------------------------------------------------------
c  n:       an integer variable.
c           must be set by caller on input (not modified).
c           order of the matrix.  must be identical to the value of n
c           in the last call to ums2fa.
 
c           ------------------------------------------------------------
c  ne:      an integer variable.
c           must be set by caller on input (not modified).
c           number of entries in input matrix.  normally not modified
c           since the last call to ums2fa.
c           restriction:  1 <= ne < (keep (4)) / 2
 
c           ------------------------------------------------------------
c  job:     an integer variable.
c           must be set by caller on input (not modified).
c           if job=1, then a column-oriented form of the input matrix
c           is preserved, otherwise, the input matrix is overwritten
c           with its lu factors.  if iterative refinement is to done
c           (icntl (8) > 0), then job must be set to 1.  can be
c           the same, or different, as the last call to ums2fa.
 
c           ------------------------------------------------------------
c  transa:  a logical variable.
c           must be set by caller on input (not modified).
c           if false then a is factorized: paq = lu.  otherwise, a
c           transpose is factorized:  pa'q = lu.  normally the same as
c           the last call to ums2fa.
 
c           ------------------------------------------------------------
c  lvalue:  an integer variable.
c           must be set by caller on input (not modified).
c           size of the value array.  restriction:  lvalue >= 2*ne,
c           although a larger will typically be required to complete
c           the factorization.  the exact value required is computed
c           by the last call to ums2fa or ums2rf (info (23)).
c           this value assumes that the ne, job, and transa parameters
c           are the same as the last call.  some garbage collection may
c           occur if lvalue is set to info (23), but usually not
c           much.  we recommend lvalue => 1.2 * info (23).  the
c           lvalue parameter is usually the same as in the last call to
c           ums2fa, however.
 
c           ------------------------------------------------------------
c  lindex:  an integer variable.
c           must be set by caller on input (not modified).
c           size of the index array.  restriction:
c           lindex >= 3*ne+2*n+1 + (keep (5) - keep (4) + 1),
c           although a larger will typically be required to complete
c           the factorization.  the exact value required is computed
c           by the last call to ums2fa or ums2rf (info (22)).
c           this value assumes that the ne, job, and transa parameters
c           are the same as the last call.  no garbage collection ever
c           occurs in the index array, since ums2rf does not create
c           external fragmentation in index.  the lindex parameter is
c           usually the same as in the last call to ums2fa, however.
c           note that lindex >= keep (5) is also required, since
c           the pattern of the prior lu factors reside in
c           index (keep (4) ... keep (5)).
 
c           ------------------------------------------------------------
c  value:   a real array of size lvalue.
c           must be set by caller on input (normally from the last call
c           to ums2fa or ums2rf).  modified on output.  on input,
c           value (1..ne) holds the original matrix in triplet form.
c           on output, value holds the lu factors, and (optionally) a
c           column-oriented form of the original matrix - otherwise
c           the input matrix is overwritten with the lu factors.
 
c           ------------------------------------------------------------
c  index:   an integer array of size lindex.
c           must be set by caller on input (normally from the last call
c           to ums2fa or ums2rf).  modified on output.  on input,
c           index (1..2*ne) holds the original matrix in triplet form,
c           and index (keep (4) ... keep (5)) holds the pattern
c           of the prior lu factors.  on output, index holds the lu
c           factors, and (optionally) a column-oriented form of the
c           original matrix - otherwise the input matrix is overwritten
c           with the lu factors.
c
c           on input the kth triplet (for k = 1...ne) is stored as:
c                       a (row,col) = value (k)
c                       row         = index (k)
c                       col         = index (k+ne)
c           if there is more than one entry for a particular position,
c           the values are accumulated, and the number of such duplicate
c           entries is returned in info (2), and a warning flag is
c           set.  however, applications such as finite element methods
c           naturally generate duplicate entries which are then
c           assembled (added) together.  if this is the case, then
c           ignore the warning message.
c
c           on input, and the pattern of the prior lu factors is in
c               index (keep (4) ... keep (5))
c
c           on output, the lu factors and the column-oriented form
c           of a (if preserved) are stored in:
c               value (keep (1)...keep (2))
c               index (keep (3)...keep (5))
c           where keep (2) = lvalue, and keep (5) = lindex.
 
c           ------------------------------------------------------------
c  keep:    an integer array of size 20.
c
c           keep (1 ... 3):  need not be set by caller on input.
c               modified on output.
c               keep (1): new lu factors start here in value
c               keep (2) = lvalue: new lu factors end here in value
c               keep (3): new lu factors start here in index
c
c           keep (4 ... 5): must be set by caller on input (normally
c               from the last call to ums2fa or ums2rf). modified on
c               output.
c               keep (4):  on input, the prior lu factors start here
c               in index, not including the prior (optionally) preserved
c               input matrix, nor the off-diagonal pattern (if btf was
c               used in the last call to ums2fa).  on output, the new
c               lu factors needed for ums2rf start here in index.
c               keep (5):  on input, the prior lu factors end here in
c               index.  on output, keep (5) is set to lindex, which
c               is where the new lu factors end in index
c
c           keep (6 ... 8):  unused.  these are used by ums2fa only.
c               future releases may make use of them, however.
c
c           keep (9 ... 20): unused.  reserved for future releases.
 
c           ------------------------------------------------------------
c  cntl:    a real array of size 10.
c           must be set by caller on input (not modified).
c           real control arguments, see ums2in for a
c           description, which sets the default values.  the current
c           version of ums2rf does not actually use cntl.  it is
c           included to make the argument list of ums2rf the same as
c           ums2fa.  ums2rf may use cntl in future releases.
 
c           ------------------------------------------------------------
c  icntl:   an integer array of size 20.
c           must be set by caller on input (not modified).
c           integer control arguments, see ums2in for a description,
c           which sets the default values.  ums2rf uses icntl (1),
c           icntl (2), icntl (3), and icntl (7).
 
c           ------------------------------------------------------------
c  info:    an integer array of size 40.
c           need not be set by caller on input.  modified on output.
c           it contains information about the execution of ums2rf.
c
c           info (1): zero if no error occurred, negative if
c               an error occurred and the factorization was not
c               completed, positive if a warning occurred (the
c               factorization was completed).
c
c               these errors cause the factorization to terminate:
c
c               error   description
c               -1      n < 1 or n > maximum value
c               -2      ne < 1 or ne > maximum value
c               -3      lindex too small
c               -4      lvalue too small
c               -5      both lindex and lvalue are too small
c               -6      prior pivot ordering no longer acceptable
c               -7      lu factors are uncomputed, or are corrupted
c
c               with these warnings the factorization was able to
c               complete:
c
c               error   description
c               1       invalid entries
c               2       duplicate entries
c               3       invalid and duplicate entries
c               4       singular matrix
c               5       invalid entries, singular matrix
c               6       duplicate entries, singular matrix
c               7       invalid and duplicate entries, singular matrix
c
c               subsequent calls to ums2rf and ums2so can only be made
c               if info (1) is zero or positive.  if info (1)
c               is negative, then some or all of the remaining
c               info and rinfo arrays may not be valid.
c
c           info (2): duplicate entries in a.  a warning is set
c               if info (2) > 0.  however, the duplicate entries
c               are summed and the factorization continues.  duplicate
c               entries are sometimes intentional - for finite element
c               codes, for example.
c
c           info (3): invalid entries in a, indices not in 1..n.
c               these entries are ignored and a warning is set in
c               info (1).
c
c           info (4): invalid entries in a, not in prior lu
c               factors.  these entries are ignored and a warning is
c               set in info (1).
c
c           info (5): entries in a after adding duplicates and
c               removing invalid entries.
c
c           info (6): entries in diagonal blocks of a.
c
c           info (7): entries in off-diagonal blocks of a.  zero
c               if info (9) = 1.
c
c           info (8): 1-by-1 diagonal blocks.
c
c           info (9): blocks in block-triangular form.
c
c           info (10): entries below diagonal in l.
c
c           info (11): entries below diagonal in u.
c
c           info (12): entries in l+u+offdiagonal part.
c
c           info (13): frontal matrices.
c
c           info (14): zero.  used by ums2fa only.
c
c           info (15): garbage collections performed on value.
c
c           info (16): diagonal pivots chosen.
c
c           info (17): numerically acceptable pivots found in a.
c               if less than n, then a is singular (or nearly so).
c               the factorization still proceeds, and ums2so can still
c               be called.  the zero-rank active submatrix of order
c               n - info (17) is replaced with the identity matrix
c               (assuming btf is not in use).  if btf is in use, then
c               one or more of the diagonal blocks are singular.
c               ums2rf can be called if the value of info (17)
c               returned by ums2fa was less than n, but the order
c               (n - info (17)) active submatrix is still replaced
c               with the identity matrix.  entries residing in this
c               submatrix are ignored, their number is included in
c               info (4), and a warning is set in info (1).
c
c           info (18): memory used in index.
c
c           info (19): memory needed in index (same as info (18)).
c
c           info (20): memory used in value.
c
c           info (21): minimum memory needed in value
c               (or minimum recommended).  if lvalue is set to
c               info (21) on a subsequent call, then a moderate
c               number of garbage collections (info (15)) will
c               occur.
c
c           info (22): memory needed in index for the next call to
c               ums2rf.
c
c           info (23): memory needed in value for the next call to
c               ums2rf.
c
c           info (24): zero.  used by ums2so only.
c
c           info (25 ... 40): reserved for future releases
 
c           ------------------------------------------------------------
c  rinfo:   a real array of size 20.
c           need not be set by caller on input.  modified on output.
c           it contains information about the execution of ums2rf.
c
c           rinfo (1): total flop count in the blas
c
c           rinfo (2): total assembly flop count
c
c           rinfo (3): zero.  used by ums2fa only.
c
c           rinfo (4): level-1 blas flops
c
c           rinfo (5): level-2 blas flops
c
c           rinfo (6): level-3 blas flops
c
c           rinfo (7): zero.  used by ums2so only.
c
c           rinfo (8): zero.  used by ums2so only.
c
c           rinfo (9 ... 20): reserved for future releases
 
c=======================================================================
c  to be preserved between calls to ums2fa, ums2rf, ums2so:
c=======================================================================
c
c  when calling ums2so to solve a linear system using the factors
c  computed by ums2fa or ums2rf, the following must be preserved:
c
c       n
c       value (keep (1)...keep (2))
c       index (keep (3)...keep (5))
c       keep (1 ... 20)
c
c  when calling ums2rf to factorize a subsequent matrix with a pattern
c  similar to that factorized by ums2fa, the following must be
c  preserved:
c
c       n
c       index (keep (4)...keep (5))
c       keep (4 ... 20)
c
c  note that the user may move the lu factors to a different position
c  in value and/or index, as long as keep (1 ... 5) are modified
c  correspondingly.
 
c## end of user documentation ##########################################
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   user routine
c       subroutines called:     ums2er, ums2p1, ums2co, ums2r0
c       functions called:       max, min
        intrinsic max, min
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer i, nz, lux1, lui1, iuse, xuse, n1, nz1, nblks,
     $          lind2, luir1, lusiz, lui2, rpermp, cpermp,
     $          offpp, lublpp, blkpp, on, nzoff, ip2, io, prl
        logical presrv, badlu
        real
     $          zero
        parameter (zero = 0.0)
 
c  printing control:
c  -----------------
c  io:      i/o unit for diagnostic messages
c  prl:     printing level
c
c  matrix to factorize:
c  --------------------
c  nz:      number of entries, after removing invalid/duplicate entries
c  presrv:  true if original matrix to be preserved
c
c  memory usage:
c  -------------
c  iuse:    current memory usage in index
c  xuse:    current memory usage in value
c  lind2:   allocatable part of index is (1..lind2)
c
c  location and status of lu factors:
c  ----------------------------------
c  lui1:    integer part of lu factors start in index (lui1...)
c  luir1:   index (luir1 ... lui2) is needed for this call to ums2rf
c  lusiz:   size of index (luir1..lui2), needed from prior lu factors
c  lui2:    integer part of lu factors end in index (..lui2)
c  lux1:    real part of lu factors in value (lux1...lvalue)
c  ip2:     pointer into trailing part of lu factors in index
c  badlu:   if true, then lu factors are corrupted or not computed
c
c  arrays and scalars allocated in lu factors (in order):
c  ------------------------------------------------------
c  ...      lu factors of each diagonal block located here
c  lublpp:  lublkp (1..nblks) array in index (lublpp..lublpp+nblks-1)
c  blkpp:   blkp (1..nblks+1) array loc. in index (blkpp...blkpp+nblks)
c  offpp:   offp (1..n+1) array located in index (offpp...offpp+n)
c  on:      size of offp array
c  cpermp:  cperm (1..n) array located in index (cpermp...cpermp+n-1)
c  rpermp:  rperm (1..n) array located in index (rpermp...rpermp+n-1)
c  nblks:   number of diagonal blocks
c  nz1:     number of entries when prior matrix factorize
c  n1:      n argument in ums2fa or ums2rf when prior matrix factorized
c
c  other:
c  ------
c  i:       loop index
 
c=======================================================================
c  executable statements:
c=======================================================================
 
        io = icntl (2)
        prl = icntl (3)
 
c-----------------------------------------------------------------------
c  clear informational output, and the unneeded part of the keep array
c-----------------------------------------------------------------------
 
        do 10 i = 1, 40
           info (i) = 0
10      continue
        do 20 i = 1, 20
           rinfo (i) = zero
20      continue
        keep (1) = 0
        keep (2) = 0
        keep (3) = 0
 
c-----------------------------------------------------------------------
c  print input arguments if requested
c-----------------------------------------------------------------------
 
        call ums2p1 (2, 1,
     $          n, ne, job, transa, lvalue, lindex, value,
     $          index, keep, cntl, icntl, info, rinfo,
     $          zero, zero, 1, zero, 1)
 
c-----------------------------------------------------------------------
c  check input arguments
c-----------------------------------------------------------------------
 
        iuse = 0
        xuse = 0
        info (5) = ne
        info (6) = ne
        if (n .lt. 1) then
c          n is too small
           call ums2er (2, icntl, info, -1, -1)
           go to 9000
        endif
        if (ne .lt. 1) then
c          ne is too small
           call ums2er (2, icntl, info, -2, -1)
           go to 9000
        endif
 
c-----------------------------------------------------------------------
c  get pointers to integer part of prior lu factors
c-----------------------------------------------------------------------
 
        luir1 = keep (4)
        lui2 = keep (5)
        lusiz = lui2 - luir1 + 1
 
        badlu = luir1 .le. 0 .or. lui2-6 .lt. luir1 .or. lui2.gt.lindex
        if (badlu) then
           call ums2er (2, icntl, info, -7, 0)
c          error return, lu factors are corrupted:
           go to 9000
        endif
        if (2*ne .gt. luir1) then
           call ums2er (2, icntl, info, -2, luir1/2)
c          error return, ne is too large:
           go to 9000
        endif
 
c-----------------------------------------------------------------------
c  shift the prior lu factors down to the end of index.  if keep and
c  lindex are unmodified from the prior call to ums2fa, then
c  keep (5) = lindex, and this shift is not performed.
c-----------------------------------------------------------------------
 
        if (lui2 .lt. lindex) then
           do 30 i = lindex, lindex - lusiz + 1, -1
              index (i) = index (i - lindex + lui2)
30         continue
           luir1 = lindex - lusiz + 1
           keep (5) = lindex
           keep (4) = luir1
        endif
 
c-----------------------------------------------------------------------
c  get seven scalars (transa, nzoff, nblks, presrv, nz, n, ne) from lu
c-----------------------------------------------------------------------
 
c       ne1 = index (lindex), not required for ums2rf
        n1 = index (lindex-1)
        nz1 = index (lindex-2)
c       presr1 = index (lindex-3) .ne. 0, not required for ums2rf
        nblks = index (lindex-4)
c       nzoff1 = index (lindex-5), not required for ums2rf
c       trans1 = index (lindex-6) .ne. 0, not required for ums2rf
 
c-----------------------------------------------------------------------
c  get pointers to permutation vectors
c-----------------------------------------------------------------------
 
        rpermp = (lindex-6) - n
        cpermp = rpermp - n
        ip2 = cpermp - 1
 
c-----------------------------------------------------------------------
c  get pointers to block-triangular information, if btf was used
c-----------------------------------------------------------------------
 
        if (nblks .gt. 1) then
 
c          -------------------------------------------------------------
c          get pointers to btf arrays
c          -------------------------------------------------------------
 
           offpp = cpermp - (n+1)
           blkpp = offpp - (nblks+1)
           lublpp = blkpp - (nblks)
           ip2 = lublpp - 1
           on = n
 
        else
 
c          -------------------------------------------------------------
c          matrix was factorized as a single block, pass dummy arg.
c          -------------------------------------------------------------
 
           offpp = 1
           blkpp = 1
           lublpp = 1
           on = 0
 
        endif
 
        badlu = n .ne. n1 .or. nz1 .le. 0 .or. luir1 .gt. ip2 .or.
     $          nblks .le. 0 .or. nblks .gt. n
        if (badlu) then
           call ums2er (2, icntl, info, -7, 0)
c          error return, lu factors are corrupted:
           go to 9000
        endif
 
c-----------------------------------------------------------------------
c  get memory for conversion to column form
c-----------------------------------------------------------------------
 
        nz = ne
        iuse = 2*n+1 + max (2*nz,n+1) + nz + lusiz
        xuse = 2*nz
        info (18) = iuse
        info (20) = xuse
        info (19) = iuse
        info (21) = xuse
        info (23) = xuse
        lind2 = luir1 - 1
        if (lindex .lt. iuse) then
c          set error flag if out of integer memory
           call ums2er (2, icntl, info, -3, iuse)
        endif
        if (lvalue .lt. xuse) then
c          set error flag if out of real memory
           call ums2er (2, icntl, info, -4, xuse)
        endif
        if (info (1) .lt. 0) then
c          error return, if not enough integer and/or real memory
           go to 9000
        endif
 
c-----------------------------------------------------------------------
c  convert to column-oriented form and remove duplicates
c-----------------------------------------------------------------------
 
        call ums2co (n, nz, transa, value, lvalue, info, icntl,
     $     index, lind2-(2*n+1), index (lind2-2*n), index (lind2-n), 2)
        if (info (1) .lt. 0) then
c          error return, if all entries are invalid (nz is now 0)
           go to 9000
        endif
 
c-----------------------------------------------------------------------
c  current memory usage:
c-----------------------------------------------------------------------
 
c       index (1..n+1): column pointers.  input matrix is now in
c       index (1..nz+n+1) and value (1..nz)
c       col pattern: index (n+1+ index (col) ... n+1+ index (col+1))
c       col values:  value (     index (col) ...      index (col+1))
c       at this point, nz <= ne (nz = ne if there are no invalid or
c       duplicate entries; nz < ne otherwise).
c       pattern of prior lu factors and btf arrays are in
c       index (keep (4) ... keep (5))
 
        iuse = nz + (n+1) + lusiz
        xuse = nz
 
c-----------------------------------------------------------------------
c  refactorize
c-----------------------------------------------------------------------
 
        presrv = job .eq. 1
        if (presrv) then
 
c          -------------------------------------------------------------
c          keep a copy of the original matrix in column-oriented form
c          -------------------------------------------------------------
 
c          copy column pointers (cp (1..n+1) = ap (1..n+1))
           iuse = iuse + (n+1)
cfpp$ nodepchk l
           do 40 i = 1, n+1
              index (nz+n+1+i) = index (i)
40         continue
 
           call ums2r0 (n, nz, index (nz+n+2),
     $          value (nz+1), lvalue-nz,
     $          index (nz+2*n+3), lind2-(nz+2*n+2),
     $          lux1, lui1, iuse, xuse, nzoff, nblks,
     $          icntl, cntl, info, rinfo,
     $          presrv, index, index (n+2), value, n, nz,
     $          index (luir1), ip2 - luir1 + 1,
     $          index (lublpp), index (blkpp), index (offpp), on,
     $          index (cpermp), index (rpermp), ne)
           if (info (1) .lt. 0) then
c             error return, if ums2r0 fails
              go to 9000
           endif
c          adjust pointers to reflect index/value, not ii/xx:
           lux1 = lux1 + nz
           lui1 = lui1 + (nz+2*n+2)
 
c          move preserved copy of a to permanent place
           lux1 = lux1 - (nz)
           lui1 = lui1 - (nz+n+1)
           do 50 i = nz+n+1, 1, -1
              index (lui1+i-1) = index (i)
50         continue
           do 60 i = nz, 1, -1
              value (lux1+i-1) = value (i)
60         continue
 
        else
 
c          -------------------------------------------------------------
c          do not preserve the original matrix
c          -------------------------------------------------------------
 
           call ums2r0 (n, nz, index,
     $          value, lvalue,
     $          index (n+2), lind2-(n+1),
     $          lux1, lui1, iuse, xuse, nzoff, nblks,
     $          icntl, cntl, info, rinfo,
     $          presrv, 1, 1, zero, 0, 1,
     $          index (luir1), ip2 - luir1 + 1,
     $          index (lublpp), index (blkpp), index (offpp), on,
     $          index (cpermp), index (rpermp), ne)
           if (info (1) .lt. 0) then
c             error return, if ums2r0 fails
              go to 9000
           endif
c          adjust pointers to reflect index/value, not ii/xx:
           lui1 = lui1 + (n+1)
 
        endif
 
c-----------------------------------------------------------------------
c  wrap-up
c-----------------------------------------------------------------------
 
        if (transa) then
           index (lindex-6) = 1
        else
           index (lindex-6) = 0
        endif
        index (lindex-5) = nzoff
        index (lindex-4) = nblks
        if (presrv) then
           index (lindex-3) = 1
        else
           index (lindex-3) = 0
        endif
        index (lindex-2) = nz
        index (lindex-1) = n
        index (lindex) = ne
 
c       save location of lu factors
        keep (1) = lux1
        keep (2) = lvalue
        keep (3) = lui1
        keep (4) = luir1
        keep (5) = lindex
 
c       update memory usage information
        iuse = lindex - lui1 + 1
        xuse = lvalue - lux1 + 1
 
c-----------------------------------------------------------------------
c  print the output arguments if requested, and return
c-----------------------------------------------------------------------
 
c       error return label:
9000    continue
        if (info (1) .lt. 0) then
           keep (1) = 0
           keep (2) = 0
           keep (3) = 0
           keep (4) = 0
           keep (5) = 0
        endif
 
        info (18) = min (lindex, max (info (18), iuse))
        info (19) = info (18)
        info (22) = info (19)
        info (20) = min (lvalue, max (info (20), xuse))
 
        call ums2p1 (2, 2,
     $          n, ne, job, transa, lvalue, lindex, value,
     $          index, keep, cntl, icntl, info, rinfo,
     $          zero, zero, 1, zero, 1)
        return
        end
 
        subroutine ums2rg (xx, xsize, xhead, xtail, xuse,
     $          lui, frdimc, frxp, frnext, frprev, nlu, lup,
     $          icntl, ffxp, ffsize, pfree, xfree)
        integer lui (*), nlu, frdimc (nlu+2), frxp (nlu+2),
     $          frnext (nlu+2), frprev (nlu+2), lup (nlu),
     $          icntl (20), xsize, xuse, xhead, xtail, ffxp, ffsize,
     $          pfree, xfree
        real
     $          xx (xsize)
 
c=== ums2rg ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  garbage collection for ums2r2.
 
c=======================================================================
c  input:
c=======================================================================
c
c       xx:             real workspace, containing matrix being
c                       factorized and partially-computed lu factors
c       xsize:          size of xx
c       xhead:          xx (1..xhead) is in use (matrix, frontal mtc's)
c       xtail:          xx (xtail..xsize) is in use (lu factors)
c       xuse:           memory usage in value
c       icntl:          integer control parameters, see ums2in
c       ffxp:           pointer to current contribution block
c       ffsize:         size of current contribution block
c       nlu:            number of lu arrowheads
c
c       frdimc (1..nlu+2)       leading dimension of frontal matrices
c       frxp (1..nlu+2)         pointer to frontal matrices in xx
c       frnext (1..nlu+2)       pointer to next block in xx
c       frprev (1..nlu+2)       pointer to previous block in xx
c       lup (1..nlu)            pointer to lu arrowhead patters in lui
c       lui (*)                 pattern of lu factors
 
c=======================================================================
c  output:
c=======================================================================
c
c       xx:             external fragmentation is removed at head
c       xhead:          xx (1..xhead) is in use, reduced in size
c       xuse:           memory usage in value, reduced
c       pfree:          pointer to free block in memory list, set to 0
c       xfree:          size of free block in xx, set to -1
c       frdimc          arrays for frontal matrices are compressed
c       frxp            frontal matrices have been shifted
c       ffxp            current working array has been shifted
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2r2
c       functions called:       abs
        intrinsic abs
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer xdp, i, e, fdimc, ludegr, ludegc, j, fluip, fxp,
     $          mhead, mtail
 
c  xdp:     real destination pointer, current block moved to xx (xdp...)
c  e:       an element
c  fdimc:   column dimension (number of rows) of a frontal matrix
c  ludegr:  row degree (number of columns) of a contribution block
c  ludegc:  column degree (number of rows) of a contribution block
c  fluip:   element is in lui (fluip...)
c  fxp:     element is in xx (fxp...) prior to compression
c  mhead:   nlu+1, head pointer for contribution block link list
c  mtail:   nlu+2, tail pointer for contribution block link list
c  i:       general loop index
c  j:       general loop index
 
c=======================================================================
c  executable statments:
c=======================================================================
 
c-----------------------------------------------------------------------
c  scan the link list and compress the reals
c-----------------------------------------------------------------------
 
        mhead = nlu+1
        mtail = nlu+2
        xdp = frxp (mhead)
        e = frnext (mhead)
 
c       while (e .ne. mtail) do
10      continue
        if (e .ne. mtail) then
 
           fdimc = frdimc (e)
 
c          -------------------------------------------------------------
           if (fdimc .eq. 0) then
c          -------------------------------------------------------------
 
c             this is a real hole - delete it from the link list
 
              frnext (frprev (e)) = frnext (e)
              frprev (frnext (e)) = frprev (e)
 
c          -------------------------------------------------------------
           else
c          -------------------------------------------------------------
 
c             this is an unassembled frontal matrix
              fxp = frxp (e)
              frxp (e) = xdp
              fluip = lup (e)
              ludegr = abs (lui (fluip+2))
              ludegc = abs (lui (fluip+3))
              if (fdimc .eq. ludegc) then
c                contribution block is already compressed
cfpp$ nodepchk l
                 do 20 i = 0, (ludegr * ludegc) - 1
                    xx (xdp+i) = xx (fxp+i)
20               continue
              else
c                contribution block is not compressed
c                compress xx (fxp..) to xx (xdp..xdp+(ludegr*ludegc)-1)
                 do 40 j = 0, ludegr - 1
cfpp$ nodepchk l
                    do 30 i = 0, ludegc - 1
                       xx (xdp + j*ludegc + i) = xx (fxp + j*fdimc + i)
30                  continue
40               continue
                 frdimc (e) = ludegc
              endif
              xdp = xdp + ludegr*ludegc
 
           endif
 
c          -------------------------------------------------------------
c          get the next item in the link list
c          -------------------------------------------------------------
 
           e = frnext (e)
 
c       end while:
        goto 10
        endif
 
        frxp (mtail) = xdp
        pfree = 0
        xfree = -1
 
c       ----------------------------------------------------------------
c       shift the current working array (if it exists)
c       ----------------------------------------------------------------
 
        if (ffxp .ne. 0) then
cfpp$ nodepchk l
           do 50 i = 0, ffsize - 1
              xx (xdp+i) = xx (ffxp+i)
50         continue
           ffxp = xdp
           xdp = xdp + ffsize
        endif
 
c-----------------------------------------------------------------------
c  deallocate the unused space
c-----------------------------------------------------------------------
 
        xuse = xuse - (xhead - xdp)
        xhead = xdp
        return
        end
 
        subroutine ums2s2 (n, job, transc, luxsiz, lux,
     $          luisiz, lui, b, x, r, z, ly, y, s, cntl, icntl, info,
     $          rinfo, cperm, rperm, presrv, an, anz, ap, ai, ax, on,
     $          nzoff, offp, offi, offx, nblks, lublkp, blkp, irstep)
        integer n, job, luxsiz, luisiz, lui (luisiz), ly, irstep,
     $          icntl (20), info (40), cperm (n), rperm (n), an,
     $          anz, ap (an+1), ai (anz), on, nzoff, offp (on+1),
     $          offi (nzoff), nblks, lublkp (nblks), blkp (nblks+1)
        logical transc, presrv
        real
     $          lux (luxsiz), b (n), x (n), r (n), z (n), y (ly),
     $          s (ly), cntl (10), rinfo (20), ax (anz),
     $          offx (nzoff)
 
c=== ums2s2 ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  solve a system, given lu factors, permutation arrays, original
c  matrix (if preserved), and off-diagonal blocks (if btf was used).
 
c=======================================================================
c  input:
c=======================================================================
c
c       n:              order of matrix
c       job:            0: solve ax=b, 1: solve lx=b, 2: solve ux=b
c       transc:         if true, solve with transposed factors instead
c       luxsiz:         size of lux
c       lux (1..luxsiz) real values in lu factors for each block
c       luisiz:         size of lui
c       lui (1..luisiz) integers in lu factors for each block
c       b (1..n):       right-hand-side
c       ly:             size of y and s, ly=n if y and s are used
c       cntl:           real control parameters, see ums2in
c       icntl:          integer control parameters, see ums2in
c       cperm (1..n):   q, column permutation array
c       rperm (1..n):   p, row permutation array
c       presrv:         if true, then original matrix was preserved
c       nblks:          number of diagonoal blocks (1 if no btf)
c       irstep:         maximum number of steps of iterative refinement
c
c       if presrv then
c           an:                 order of preserved matrix, n
c           anz:                number of entries in preserved matrix
c           ap (1..an+1):       column pointers of preserved matrix
c           ai (1..anz):        row indices of preserved matrix
c           ax (1..anz):        values of preserved matrix
c           an, anz, ap, ai, ax:        not accessed
c
c       if nblks > 1 then
c           on:                 n
c           nzoff:              number of off-diagonoal entries
c           offp (1..n+1)       row pointers for off-diagonal part
c           offi (1..nzoff):    column indices for off-diagonal part
c           offx (1..nzoff):    values of off-diagonal part
c           lublkp (1..nblks):  pointers to lu factors of each block
c           blkp (1..nblks+1):  index range of each block
c       else
c           on, nzoff, offp, offi, offx, lublkp, blkp:  not accessed
 
c=======================================================================
c  workspace:
c=======================================================================
c
c       r (1..n), z (1..n)
c       y (1..ly), s (1..ly):   unaccessed if no iterative refinement
 
c=======================================================================
c  output:
c=======================================================================
c
c       x (1..n):       solution
c       info:           integer informational output, see ums2in
c       rinfo:          real informational output, see ums2in
c
c       if irsteps > 0 and presrv is true then
c           w (1..n):           residual
c           rinfo (7):  sparse error estimate, omega1
c           rinfo (8):  sparse error estimate, omega2
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2so
c       subroutines called:     ums2er, ums2sl, ums2lt, ums2su, ums2ut
c       functions called:       isamax, abs, max
        intrinsic abs, max
        integer isamax
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer nlu, i, blk, k1, k2, kn, p, step, npiv, j
        real
     $          zero, one, xnorm, tau, nctau, omega1, omega2, d1,
     $          d2, omega, omlast, om1lst, om2lst, two, eps, maxeps,
     $          thosnd, a, axx, r2, x2, y2, z2
        parameter (zero = 0.0, one = 1.0, two = 2.0,
     $          maxeps = two ** (-15), thosnd = 1000.0)
 
c  lu factors:
c  -----------
c  blk:     current diagonal block
c  k1,k2:   current diagonal block is a (k1..k2, k1..k2)
c  kn:      size of diagonal block (= k2-k1+1)
c  nlu:     number of elements in the lu factors of a single diag block
c  npiv:    number of pivots in the lu factors of a single diag block
c
c  iterative refinement and sparse backward error:
c  -----------------------------------------------
c  step:    number of steps of iterative refinement taken
c  xnorm:   ||x|| maxnorm of solution vector, x
c  tau:     threshold for selecting which estimate to use (1 or 2)
c  nctau:   1000*n*eps
c  eps:     largest positive value such that fl (1.0 + eps) = 1.0
c  omega1:  current sparse backward error estimate 1
c  omega2:  current sparse backward error estimate 2
c  d1:      divisor for omega1
c  d2:      divisor for omega2
c  omega:   omega1 + omega2
c  omlast:  value of omega from previous step
c  om1lst:  value of omega1 from previous step
c  om2lst:  value of omega2 from previous step
c  maxeps:  2**(-16), maximum value that eps is allowed to be
c  a:       value of an entry in a, a_ij
c  axx:     a_ij * x_j
c
c  other:
c  ------
c  i,j:     loop indices
c  p:       pointer
c  r2:      r (i)
c  x2:      x (i)
c  y2:      y (i)
c  z2:      z (i)
 
c=======================================================================
c  executable statements:
c=======================================================================
 
c-----------------------------------------------------------------------
c  initializations for sparse backward error
c-----------------------------------------------------------------------
 
        omega = zero
        omega1 = zero
        omega2 = zero
        eps = cntl (3)
        if (eps .le. zero .or. eps .gt. maxeps) then
c          eps is too small or too big: set to a large default value
           eps = maxeps
        endif
        nctau = thosnd * n * eps
 
c-----------------------------------------------------------------------
c  get information on lu factorization if btf was not used
c-----------------------------------------------------------------------
 
        if (nblks .eq. 1) then
c          p is 1, and lui (p) is 1
           nlu = lui (2)
           npiv = lui (3)
        endif
 
c-----------------------------------------------------------------------
        if (job .eq. 1) then
c-----------------------------------------------------------------------
 
c          -------------------------------------------------------------
           if (.not. transc) then
c          -------------------------------------------------------------
 
c             ----------------------------------------------------------
c             solve p'lx=b:  x = l \ pb
c             ----------------------------------------------------------
 
              do 10 i = 1, n
                 x (i) = b (rperm (i))
10            continue
              if (nblks .eq. 1) then
                 call ums2sl (nlu, npiv, n, lui(6), lui(6+nlu), lux,x,z)
              else
                 do 20 blk = 1, nblks
                    k1 = blkp (blk)
                    k2 = blkp (blk+1) - 1
                    kn = k2-k1+1
                    if (kn .gt. 1) then
                       p = lublkp (blk)
                       nlu = lui (p+1)
                       npiv = lui (p+2)
                       call ums2sl (nlu, npiv, kn, lui (p+5),
     $                    lui (p+5+nlu), lux (lui (p)), x (k1), z)
                    endif
20               continue
              endif
 
c          -------------------------------------------------------------
           else
c          -------------------------------------------------------------
 
c             ----------------------------------------------------------
c             solve l'px=b:  x = p' (l' \ b)
c             ----------------------------------------------------------
 
              do 30 i = 1, n
                 r (i) = b (i)
30            continue
              if (nblks .eq. 1) then
                 call ums2lt (nlu, npiv, n, lui(6), lui(6+nlu), lux,r,z)
              else
                 do 40 blk = 1, nblks
                    k1 = blkp (blk)
                    k2 = blkp (blk+1) - 1
                    kn = k2-k1+1
                    if (kn .gt. 1) then
                       p = lublkp (blk)
                       nlu = lui (p+1)
                       npiv = lui (p+2)
                       call ums2lt (nlu, npiv, kn, lui (p+5),
     $                    lui (p+5+nlu), lux (lui (p)), r (k1), z)
                    endif
40               continue
              endif
              do 50 i = 1, n
                 x (rperm (i)) = r (i)
50            continue
 
c          -------------------------------------------------------------
           endif
c          -------------------------------------------------------------
 
c-----------------------------------------------------------------------
        else if (job .eq. 2) then
c-----------------------------------------------------------------------
 
c          -------------------------------------------------------------
           if (transc) then
c          -------------------------------------------------------------
 
c             ----------------------------------------------------------
c             solve qu'x=b:  x = u' \ q'b
c             ----------------------------------------------------------
 
              do 60 i = 1, n
                 x (i) = b (cperm (i))
60            continue
              if (nblks .eq. 1) then
                 call ums2ut (nlu, npiv, n, lui(6), lui(6+nlu), lux,x,z)
              else
                 do 100 blk = 1, nblks
                    k1 = blkp (blk)
                    k2 = blkp (blk+1) - 1
                    kn = k2-k1+1
                    if (kn .eq. 1) then
                       x (k1) = x (k1) / lux (lublkp (blk))
                       r (k1) = x (k1)
                    else
                       p = lublkp (blk)
                       nlu = lui (p+1)
                       npiv = lui (p+2)
                       call ums2ut (nlu, npiv, kn, lui (p+5),
     $                    lui (p+5+nlu), lux (lui (p)), x (k1), z)
                       do 70 i = k1, k2
                          r (i) = x (i)
70                     continue
                       call ums2lt (nlu, npiv, kn, lui (p+5),
     $                    lui (p+5+nlu), lux (lui (p)), r (k1), z)
                    endif
                    do 90 i = k1, k2
                       r2 = r (i)
                       do 80 p = offp (i), offp (i+1)-1
                          x (offi (p)) = x (offi (p)) - offx (p) * r2
80                     continue
90                  continue
100              continue
              endif
 
c          -------------------------------------------------------------
           else
c          -------------------------------------------------------------
 
c             ----------------------------------------------------------
c             solve uq'x=b:  x = q (u \ b)
c             ----------------------------------------------------------
 
              if (nblks .eq. 1) then
                 do 110 i = 1, n
                    r (i) = b (i)
110              continue
                 call ums2su (nlu, npiv, n, lui(6), lui(6+nlu), lux,r,z)
              else
                 do 150 blk = nblks, 1, -1
                    k1 = blkp (blk)
                    k2 = blkp (blk+1) - 1
                    kn = k2-k1+1
                    do 130 i = k1, k2
                       x2 = zero
                       do 120 p = offp (i), offp (i+1)-1
                          x2 = x2 + offx (p) * r (offi (p))
120                    continue
                       x (i) = x2
130                 continue
                    if (kn .eq. 1) then
                       r (k1) = (b (k1) - x (k1)) / lux (lublkp (blk))
                    else
                       p = lublkp (blk)
                       nlu = lui (p+1)
                       npiv = lui (p+2)
                       call ums2sl (nlu, npiv, kn, lui (p+5),
     $                    lui (p+5+nlu), lux (lui (p)), x (k1), z)
                       do 140 i = k1, k2
                          r (i) = b (i) - x (i)
140                    continue
                       call ums2su (nlu, npiv, kn, lui (p+5),
     $                    lui (p+5+nlu), lux (lui (p)), r (k1), z)
                    endif
150              continue
              endif
              do 160 i = 1, n
                 x (cperm (i)) = r (i)
160           continue
 
c          -------------------------------------------------------------
           endif
c          -------------------------------------------------------------
 
c-----------------------------------------------------------------------
        else
c-----------------------------------------------------------------------
 
           do 450 step = 0, irstep
 
c             ----------------------------------------------------------
c             if transa was true in ums2fa or ums2rf, then c = a'.
c             otherwise c = a.  in both cases, the factorization is
c             pcq = lu, and c is stored in column-form in ai,ax,ap if
c             it is preserved.
c             ----------------------------------------------------------
 
c             ----------------------------------------------------------
              if (.not. transc) then
c             ----------------------------------------------------------
 
c                -------------------------------------------------------
c                solve cx=b (step 0):
c                   x = q (u \ l \ pb)
c                and then perform iterative refinement (step > 0):
c                   x = x + q (u \ l \ p (b-cx))
c                -------------------------------------------------------
 
                 if (step .eq. 0) then
                    do 170 i = 1, n
                       r (i) = b (rperm (i))
170                 continue
                 else
                    do 180 i = 1, n
                       z (i) = b (i)
180                 continue
                    do 200 i = 1, n
                       x2 = x (i)
                       do 190 p = ap (i), ap (i+1) - 1
                          z (ai (p)) = z (ai (p)) - ax (p) * x2
190                    continue
200                 continue
                    do 210 i = 1, n
                       r (i) = z (rperm (i))
210                 continue
                 endif
                 if (nblks .eq. 1) then
                    call ums2sl (nlu, npiv, n,lui(6),lui(6+nlu),lux,r,z)
                    call ums2su (nlu, npiv, n,lui(6),lui(6+nlu),lux,r,z)
                 else
                    do 240 blk = nblks, 1, -1
                       k1 = blkp (blk)
                       k2 = blkp (blk+1) - 1
                       kn = k2-k1+1
                       do 230 i = k1, k2
                          r2 = r (i)
                          do 220 p = offp (i), offp (i+1)-1
                             r2 = r2 - offx (p) * r (offi (p))
220                       continue
                          r (i) = r2
230                    continue
                       if (kn .eq. 1) then
                          r (k1) = r (k1) / lux (lublkp (blk))
                       else
                          p = lublkp (blk)
                          nlu = lui (p+1)
                          npiv = lui (p+2)
                          call ums2sl (nlu, npiv, kn, lui (p+5),
     $                       lui (p+5+nlu), lux (lui (p)), r (k1), z)
                          call ums2su (nlu, npiv, kn, lui (p+5),
     $                       lui (p+5+nlu), lux (lui (p)), r (k1), z)
                       endif
240                 continue
                 endif
                 if (step .eq. 0) then
                    do 250 i = 1, n
                       x (cperm (i)) = r (i)
250                 continue
                 else
                    do 260 i = 1, n
                       x (cperm (i)) = x (cperm (i)) + r (i)
260                 continue
                 endif
 
c             ----------------------------------------------------------
              else
c             ----------------------------------------------------------
 
c                -------------------------------------------------------
c                solve c'x=b (step 0):
c                   x = p' (l' \ u' \ q'b)
c                and then perform iterative refinement (step > 0):
c                   x = x + p' (l' \ u' \ q' (b-c'x))
c                -------------------------------------------------------
 
                 if (step .eq. 0) then
                    do 270 i = 1, n
                       r (i) = b (cperm (i))
270                 continue
                 else
                    do 280 i = 1, n
                       z (i) = b (i)
280                 continue
                    do 300 i = 1, n
                       z2 = z (i)
                       do 290 p = ap (i), ap (i+1) - 1
                          z2 = z2 - ax (p) * x (ai (p))
290                    continue
                       z (i) = z2
300                 continue
                    do 310 i = 1, n
                       r (i) = z (cperm (i))
310                 continue
                 endif
                 if (nblks .eq. 1) then
                    call ums2ut (nlu, npiv, n,lui(6),lui(6+nlu),lux,r,z)
                    call ums2lt (nlu, npiv, n,lui(6),lui(6+nlu),lux,r,z)
                 else
                    do 340 blk = 1, nblks
                       k1 = blkp (blk)
                       k2 = blkp (blk+1) - 1
                       kn = k2-k1+1
                       if (kn .eq. 1) then
                          r (k1) = r (k1) / lux (lublkp (blk))
                       else
                          p = lublkp (blk)
                          nlu = lui (p+1)
                          npiv = lui (p+2)
                          call ums2ut (nlu, npiv, kn, lui (p+5),
     $                       lui (p+5+nlu), lux (lui (p)), r (k1), z)
                          call ums2lt (nlu, npiv, kn, lui (p+5),
     $                       lui (p+5+nlu), lux (lui (p)), r (k1), z)
                       endif
                       do 330 i = k1, k2
                          r2 = r (i)
                          do 320 p = offp (i), offp (i+1)-1
                             r (offi (p)) = r (offi (p)) - offx (p) * r2
320                       continue
330                    continue
340                 continue
                 endif
                 if (step .eq. 0) then
                    do 350 i = 1, n
                       x (rperm (i)) = r (i)
350                 continue
                 else
                    do 360 i = 1, n
                       x (rperm (i)) = x (rperm (i)) + r (i)
360                 continue
                 endif
 
c             ----------------------------------------------------------
              endif
c             ----------------------------------------------------------
 
c             ----------------------------------------------------------
c             sparse backward error estimate
c             ----------------------------------------------------------
 
              if (irstep .gt. 0) then
 
c                xnorm = ||x|| maxnorm
                 xnorm = abs (x (isamax (n, x, 1)))
 
c                r (i) = (b-ax)_i, residual (or a')
c                z (i) = (|a||x|)_i
c                y (i) = ||a_i||, maxnorm of row i of a (or a')
                 do 370 i = 1, n
                    r (i) = b (i)
                    z (i) = zero
                    y (i) = zero
370              continue
 
                 if (.not. transc) then
 
c                   ----------------------------------------------------
c                   sparse backward error for cx=b, c stored by column
c                   ----------------------------------------------------
 
                    do 390 j = 1, n
                       x2 = x (j)
cfpp$ nodepchk l
                       do 380 p = ap (j), ap (j+1) - 1
                          i = ai (p)
                          a = ax (p)
                          axx = a * x2
                          r (i) = r (i) -     (axx)
                          z (i) = z (i) + abs (axx)
                          y (i) = y (i) + abs (a)
380                    continue
390                 continue
 
                 else
 
c                   ----------------------------------------------------
c                   sparse backward error for c'x=b, c' stored by row
c                   ----------------------------------------------------
 
                    do 410 i = 1, n
                       r2 = r (i)
                       z2 = z (i)
                       y2 = y (i)
cfpp$ nodepchk l
                       do 400 p = ap (i), ap (i+1) - 1
                          j = ai (p)
                          a = ax (p)
                          axx = a * x (j)
                          r2 = r2 -     (axx)
                          z2 = z2 + abs (axx)
                          y2 = y2 + abs (a)
400                    continue
                       r (i) = r2
                       z (i) = z2
                       y (i) = y2
410                 continue
 
                 endif
 
c                -------------------------------------------------------
c                save the last iteration in case we need to reinstate it
c                -------------------------------------------------------
 
                 omlast = omega
                 om1lst = omega1
                 om2lst = omega2
 
c                -------------------------------------------------------
c                compute sparse backward errors: omega1 and omega2
c                -------------------------------------------------------
 
                 omega1 = zero
                 omega2 = zero
                 do 420 i = 1, n
                    tau = (y (i) * xnorm + abs (b (i))) * nctau
                    d1 = z (i) + abs (b (i))
                    if (d1 .gt. tau) then
                       omega1 = max (omega1, abs (r (i)) / d1)
                    else if (tau .gt. zero) then
                       d2 = z (i) + y (i) * xnorm
                       omega2 = max (omega2, abs (r (i)) / d2)
                    endif
420              continue
                 omega = omega1 + omega2
                 rinfo (7) = omega1
                 rinfo (8) = omega2
 
c                -------------------------------------------------------
c                stop the iterations if the backward error is small
c                -------------------------------------------------------
 
                 info (24) = step
                 if (one + omega .le. one) then
c                   further iterative refinement will no longer improve
c                   the solution
                    return
                 endif
 
c                -------------------------------------------------------
c                stop if insufficient decrease in omega
c                -------------------------------------------------------
 
                 if (step .gt. 0 .and. omega .gt. omlast / two) then
                    if (omega .gt. omlast) then
c                      last iteration better than this one, reinstate it
                       do 430 i = 1, n
                          x (i) = s (i)
                          rinfo (7) = om1lst
                          rinfo (8) = om2lst
430                    continue
                    endif
                    info (24) = step - 1
                    return
                 endif
 
c                -------------------------------------------------------
c                save current solution in case we need to reinstate
c                -------------------------------------------------------
 
                 do 440 i = 1, n
                    s (i) = x (i)
440              continue
 
              endif
 
450        continue
 
c-----------------------------------------------------------------------
        endif
c-----------------------------------------------------------------------
 
        return
        end
 
        subroutine ums2sl (nlu, npiv, n, lup, lui, lux, x, w)
        integer nlu, npiv, n, lup (nlu), lui (*)
        real
     $          lux (*), x (n), w (n)
 
c=== ums2sl ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  solves lx = b, where l is the lower triangular factor of a matrix
c  (if btf not used) or a single diagonal block (if btf is used).
c  b is overwritten with the solution x.
 
c=======================================================================
c  input:
c=======================================================================
c
c       nlu:            number of lu arrowheads in the lu factors
c       npiv:           number of pivots found (normally n)
c       n:              order of matrix
c       lup (1..nlu):   pointer to lu arrowheads in lui
c       lui ( ... ):    integer values of lu arrowheads
c       lux ( ... ):    real values of lu arroheads
c       x (1..n):       the right-hand-side
 
c=======================================================================
c  workspace:
c=======================================================================
c
c       w (1..n)
 
c=======================================================================
c  output:
c=======================================================================
c
c       x (1..n):       the solution to lx=b
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2s2
c       subroutines called:     strsv, sgemv
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer i, k, s, luip, luxp, luk, ludegc, lucp, lxp, row
        real
     $          one
        parameter (one = 1.0)
 
c  s:       an element, or lu arrowhead
c  k:       kth pivot
c  i:       ith row in l2 array in element s
c  luip:    integer part of s is in lui (luip...)
c  luxp:    real part of s is in lux (luxp...)
c  luk:     number of pivots in s
c  ludegc:  column degree of non-pivotal part of s
c  lucp:    pattern of column of s in lui (lucp...lucp+ludegc-1)
c  lxp:     the ludegc-by-luk l2 block of s is in lux (lxp...)
c  row:     row index
 
c=======================================================================
c  executable statments:
c=======================================================================
 
        k = 0
        do 40 s = 1, nlu
 
c          -------------------------------------------------------------
c          get the s-th lu arrowhead (s = 1..nlu, in pivotal order)
c          -------------------------------------------------------------
 
           luip   = lup (s)
           luxp   = lui (luip)
           luk    = lui (luip+1)
           ludegc = lui (luip+3)
           lucp   = (luip + 7)
           lxp    = luxp + luk
 
           if (luk .eq. 1) then
 
c             ----------------------------------------------------------
c             only one pivot, stride-1 sparse saxpy
c             ----------------------------------------------------------
 
              k = k + 1
c             l (k,k) is one
cfpp$ nodepchk l
              do 10 i = 1, ludegc
                 row = lui (lucp+i-1)
c                col: k, l (row,col): lux (lxp+i-1)
                 x (row) = x (row) - lux (lxp+i-1) * x (k)
10            continue
 
           else
 
c             ----------------------------------------------------------
c             more than one pivot
c             ----------------------------------------------------------
 
              call strsv ('l', 'n', 'u', luk,
     $           lux (luxp), ludegc + luk, x (k+1), 1)
              do 20 i = 1, ludegc
                 row = lui (lucp+i-1)
                 w (i) = x (row)
20            continue
              call sgemv ('n', ludegc, luk, -one,
     $           lux (lxp), ludegc + luk, x (k+1), 1, one, w, 1)
              do 30 i = 1, ludegc
                 row = lui (lucp+i-1)
                 x (row) = w (i)
30            continue
              k = k + luk
           endif
40      continue
        return
        end
 
        subroutine ums2so (n, job, transc, lvalue, lindex, value,
     $          index, keep, b, x, w, cntl, icntl, info, rinfo)
        integer n, job, lvalue, lindex, index (lindex), keep (20),
     $          icntl (20), info (40)
        real
     $          value (lvalue), b (n), x (n), w (*), cntl (10),
     $          rinfo (20)
        logical transc
 
c=== ums2so ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  given lu factors computed by ums2fa or ums2rf, and the
c  right-hand-side, b, solve a linear system for the solution x.
c
c  this routine handles all permutations, so that b and x are in terms
c  of the original order of the matrix, a, and not in terms of the
c  permuted matrix.
c
c  if iterative refinement is done, then the residual is returned in w,
c  and the sparse backward error estimates are returned in
c  rinfo (7) and rinfo (8).  the computed solution x is the
c  exact solution of the equation (a + da)x = (b + db), where
c    da (i,j)  <= max (rinfo (7), rinfo (8)) * abs (a(i,j))
c  and
c    db (i) <= max (rinfo (7) * abs (b (i)),
c                   rinfo (8) * maxnorm (a) * maxnorm (x computed))
c  note that da has the same sparsity pattern as a.
c  the method used to compute the sparse backward error estimate is
c  described in m. arioli, j. w. demmel, and i. s. duff, "solving
c  sparse linear systems with sparse backward error," siam j. matrix
c  analysis and applications, vol 10, 1989, pp. 165-190.
 
c=======================================================================
c  arguments:
c=======================================================================
 
c           ------------------------------------------------------------
c  n:       an integer variable.
c           must be set by caller on input (not modified).
c           must be the same as passed to ums2fa or ums2rf.
 
c           ------------------------------------------------------------
c  job:     an integer variable.
c           must be set by caller on input (not modified).
c           what system to solve (see the transc argument below).
c           iterative refinement is only performed if job = 0,
c           icntl (8) > 0, and only if the original matrix was
c           preserved (job = 1 in ums2fa or ums2rf).
 
c           ------------------------------------------------------------
c  transc:  an integer variable.
c           must be set by caller on input (not modified).
c           solve with l and u factors or with l' and u', where
c           transa was passed to ums2fa or ums2rf.
c
c           if transa = false, then paq = lu was performed,
c           and the following systems are solved:
c
c                               transc = false          transc = true
c                               ----------------        ----------------
c                  job = 0      solve ax = b            solve a'x = b
c                  job = 1      solve p'lx = b          solve l'px = b
c                  job = 2      solve uq'x = b          solve qu'x = b
c
c           if transa = true, then a was transformed prior to lu
c           factorization, and p(a')q = lu
c
c                               transc = false          transc = true
c                               ----------------        ----------------
c                  job = 0      solve a'x = b           solve ax = b
c                  job = 1      solve p'lx = b          solve l'px = b
c                  job = 2      solve uq'x = b          solve qu'x = b
c
c           other values of job are treated as zero.  iterative
c           refinement can be done only when solving ax=b or a'x=b.
c
c           the comments below use matlab notation, where
c           x = l \ b means x = (l^(-1)) * b, premultiplication by
c           the inverse of l.
 
c           ------------------------------------------------------------
c  lvalue:  an integer variable.
c           must be set by caller on input (not modified).
c           the size of value.
 
c           ------------------------------------------------------------
c  lindex:  an integer variable.
c           must be set by caller on input (not modified).
c           the size of index.
 
c           ------------------------------------------------------------
c  value:   a real array of size lvalue.
c           must be set by caller on input (normally from last call to
c           ums2fa or ums2rf) (not modified).
c           the lu factors, in value (keep (1) ... keep (2)).
c           the entries in value (1 ... keep (1) - 1) and in
c           value (keep (2) + 1 ... lvalue) are not accessed.
 
c           ------------------------------------------------------------
c  index:   an integer array of size lindex.
c           must be set by caller on input (normally from last call to
c           ums2fa or ums2rf) (not modified).
c           the lu factors, in index (keep (3) ... keep (5)).
c           the entries in index (1 ... keep (3) - 1) and in
c           index (keep (5) + 1 ... lindex) are not accessed.
 
c           ------------------------------------------------------------
c  keep:    an integer array of size 20.
c
c           keep (1..5): must be set by caller on input (normally from
c               last call to ums2fa or ums2rf) (not modified).
c               layout of the lu factors in value and index
 
c           ------------------------------------------------------------
c  b:       a real array of size n.
c           must be set by caller on input (not modified).
c           the right hand side, b, of the system to solve.
 
c           ------------------------------------------------------------
c  w:       a real array of size 2*n or 4*n.
c           need not be set by caller on input.  modified on output.
c           workspace of size w (1..2*n) if icntl (8) = 0, which
c           is the default value.  if iterative refinement is
c           performed, and w must be of size w (1..4*n) and the
c           residual b-ax (or b-a'x) is returned in w (1..n).
 
c           ------------------------------------------------------------
c  x:       a real array of size n.
c           need not be set by caller on input.  modified on output.
c           the solution, x, of the system that was solved.  valid only
c           if info (1) is greater than or equal to 0.
 
c           ------------------------------------------------------------
c  cntl:    a real array of size 10.
c           must be set by caller on input (not modified).
c           real control parameters, see ums2in for a description,
c           which sets the defaults.
 
c           ------------------------------------------------------------
c  icntl:   an integer array of size 20.
c           must be set by caller on input (not modified).
c           integer control parameters, see ums2in for a description,
c           which sets the defaults.  in particular, icntl (8) is
c           the maximum number of steps of iterative refinement to be
c           performed.
 
c           ------------------------------------------------------------
c  info:    an integer array of size 40.
c           need not be set by caller on input.  modified on output.
c           it contains information about the execution of ums2so.
c
c           info (1) is the error flag.  if info (1) is -7, then
c           the lu factors are uncomputed, or have been corrupted since
c           the last call to ums2fa or ums2rf.  no system is solved,
c           and x (1..n) is not valid on output.  if info (1) is 8,
c           then iterative refinement was requested but cannot be done.
c           to perform iterative refinement, the original matrix must be
c           preserved (job = 1 in ums2fa or ums2rf) and ax=b or a'x=b
c           must be solved (job = 0 in ums2so).  info (24) is the
c           steps of iterative refinement actually taken.
 
c           ------------------------------------------------------------
c  rinfo:   a real array of size 20.
c           need not be set by caller on input.  modified on output.
c           it contains information about the execution of ums2so.
c
c           if iterative refinement was performed then
c           rinfo (7) is the sparse error estimate, omega1, and
c           rinfo (8) is the sparse error estimate, omega2.
 
c=======================================================================
c  to be preserved between calls to ums2fa, ums2rf, ums2so:
c=======================================================================
c
c  the following must be unchanged since the call to ums2fa or ums2rf
c  that computed the lu factors:
c
c       n
c       value (keep (1) ... keep (2))
c       index (keep (3) ... keep (5))
c       keep (1 ... 20)
 
c## end of user documentation ##########################################
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   user routine
c       subroutines called:     ums2er, ums2p1, ums2s2
c       functions called:       max
        intrinsic max
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer nblks, offip, offxp, n1, nz, ne, offpp, blkpp, lublpp,
     $          app, an, anz, on, lui1, lui2, lux1, lux2, aip, axp,
     $          cpermp, rpermp, nzoff, irstep, yp, ly, lw, sp, ip1, ip2,
     $          xp1, luir1, io, prl
        logical presrv, badlu
        real
     $          zero
        parameter (zero = 0.0)
 
c  printing control:
c  -----------------
c  io:      i/o unit for diagnostic messages
c  prl:     printing level
c
c  location and status of lu factors:
c  ----------------------------------
c  lui1:    integer part of lu factors start in index (lui1...)
c  luir1:   index (luir1 ... lui2) is needed for a call to ums2rf
c  lui2:    integer part of lu factors end in index (..lui2)
c  lux1:    real part of lu factors start in value (lux1...)
c  lux2:    real part of lu factors end in value (...lux1)
c  ip1:     pointer into leading part of lu factors in index
c  ip2:     pointer into trailing part of lu factors in index
c  xp1:     pointer into leading part of lu factors in value
c  badlu:   if true, then lu factors are corrupted or not computed
c
c  arrays and scalars allocated in lu factors (in order):
c  ------------------------------------------------------
c  app:     ap (1..n+1) array located in index (app...app+n)
c  axp:     ax (1..nz) array located in value (axp...axp+nz-1)
c  aip:     ai (1..nz) array located in index (aip...aip+nz-1)
c  an:      n if a is preserved, 1 otherwise
c  anz:     nz if a is preserved, 1 otherwise
c  offip:   offi (1..nzoff) array loc. in index (offip...offip+nzoff-1)
c  offxp:   offx (1..nzoff) array loc. in value (offxp...offxp+nzoff-1)
c  ...      lu factors of each diagonal block located here
c  lublpp:  lublkp (1..nblks) array in index (lublpp..lublpp+nblks-1)
c  blkpp:   blkp (1..nblks+1) array loc. in index (blkpp...blkpp+nblks)
c  offpp:   offp (1..n+1) array located in index (offpp...offpp+n)
c  on:      size of offp (1..n+1):  n if nblks > 1, 1 otherwise
c  cpermp:  cperm (1..n) array located in index (cpermp...cpermp+n-1)
c  rpermp:  rperm (1..n) array located in index (rpermp...rpermp+n-1)
c  ...      seven scalars in index (lui2-6...lui2):
c  nzoff:   number of entries in off-diagonal part
c  nblks:   number of diagonal blocks
c  presrv:  true if original matrix was preserved when factorized
c  nz:      entries in a
c  n1:      n argument in ums2fa or ums2rf when matrix factorized
c  ne:      ne argument in ums2fa or ums2rf when matrix factorized
c
c  arrays allocated from w work array:
c  -----------------------------------
c  lw:      size of w
c  yp:      y (1..n) located in w (yp...yp+n-1)
c  sp:      s (1..n) located in w (sp...sp+n-1)
c  ly:      size of y and s
c
c  other:
c  ------
c  irstep:  maximum number of iterative refinement steps to take
 
c=======================================================================
c  executable statements:
c=======================================================================
 
        io = icntl (2)
        prl = icntl (3)
 
c-----------------------------------------------------------------------
c  clear informational output
c-----------------------------------------------------------------------
 
        info (1) = 0
        info (24) = 0
        rinfo (7) = zero
        rinfo (8) = zero
 
c-----------------------------------------------------------------------
c  print input arguments if requested
c-----------------------------------------------------------------------
 
        irstep = max (0, icntl (8))
        if (irstep .eq. 0) then
           lw = 2*n
        else
           lw = 4*n
        endif
        call ums2p1 (3, 1,
     $          n, ne, job, transc, lvalue, lindex, value,
     $          index, keep, cntl, icntl, info, rinfo,
     $          b, x, n, w, lw)
 
c-----------------------------------------------------------------------
c  get pointers to lu factors
c-----------------------------------------------------------------------
 
        lux1 = keep (1)
        lux2 = keep (2)
        lui1 = keep (3)
        luir1 = keep (4)
        lui2 = keep (5)
        badlu = luir1 .le. 0 .or. lui2-6 .lt. luir1
     $     .or. lui2 .gt. lindex
     $     .or. lux1 .le. 0 .or. lux1 .gt. lux2 .or. lux2 .gt. lvalue
     $     .or. lui1 .le. 0 .or. luir1 .lt. lui1 .or. luir1 .gt. lui2
        if (badlu) then
           call ums2er (3, icntl, info, -7, 0)
c          error return, lu factors are corrupted:
           go to 9000
        endif
 
c-----------------------------------------------------------------------
c  get seven scalars (transa, nzoff, nblks, presrv, nz, n, ne) from lu
c-----------------------------------------------------------------------
 
        ne = index (lui2)
        n1 = index (lui2-1)
        nz = index (lui2-2)
        presrv = index (lui2-3) .ne. 0
        nblks = index (lui2-4)
        nzoff = index (lui2-5)
c       transa = index (lui2-6) .ne. 0, we don't actually need this here
 
c-----------------------------------------------------------------------
c  get pointers to permutation vectors
c-----------------------------------------------------------------------
 
        rpermp = (lui2-6) - n
        cpermp = rpermp - n
        ip2 = cpermp - 1
        xp1 = lux1
        ip1 = lui1
 
c-----------------------------------------------------------------------
c  get pointers to preserved column-oriented copy of input matrix
c-----------------------------------------------------------------------
 
        if (presrv) then
 
c          -------------------------------------------------------------
c          original matrix preserved in index (lui1..lui1+nz+n) and
c          value (lux1..lux1+nz-1)
c          -------------------------------------------------------------
 
           app = ip1
           aip = app + n+1
           ip1 = aip + nz
           axp = xp1
           xp1 = axp + nz
           an = n
           anz = nz
 
        else
 
c          -------------------------------------------------------------
c          original matrix not preserved, pass dummy argument to ums2s2
c          -------------------------------------------------------------
 
           app = 1
           aip = 1
           axp = 1
           an = 1
           anz = 1
 
        endif
 
c-----------------------------------------------------------------------
c  get pointers to block-triangular information, if btf was used
c-----------------------------------------------------------------------
 
        if (nblks .gt. 1) then
 
c          -------------------------------------------------------------
c          get pointers to off-diagonal nonzeros, and btf arrays
c          -------------------------------------------------------------
 
           offip = ip1
           ip1 = ip1 + nzoff
           offxp = xp1
           xp1 = xp1 + nzoff
           offpp = cpermp - (n+1)
           blkpp = offpp - (nblks+1)
           lublpp = blkpp - (nblks)
           ip2 = lublpp - 1
           on = n
 
        else
 
c          -------------------------------------------------------------
c          matrix was factorized as a single block, pass dummy arg.
c          -------------------------------------------------------------
 
           offip = 1
           offxp = 1
           offpp = 1
           blkpp = 1
           lublpp = 1
           on = 1
 
        endif
 
        badlu = n .ne. n1 .or. nz .le. 0 .or. luir1 .gt. ip2 .or.
     $     nblks .le. 0 .or. nblks .gt. n .or.
     $     xp1 .gt. lux2 .or. nzoff .lt. 0 .or. ip1 .ne. luir1
        if (badlu) then
           call ums2er (3, icntl, info, -7, 0)
c          error return, lu factors are corrupted:
           go to 9000
        endif
 
c-----------------------------------------------------------------------
c  get the number of steps of iterative refinement
c-----------------------------------------------------------------------
 
        if (irstep .gt. 0 .and. .not. presrv) then
c          original matrix not preserved (ums2fa/ums2rf job .ne. 1)
           call ums2er (3, icntl, info, 8, 0)
           irstep = 0
        endif
        if (irstep .gt. 0 .and. (job .eq. 1 .or. job .eq. 2)) then
c          iterative refinement for ax=b and a'x=b only (job = 0)
           call ums2er (3, icntl, info, 8, 1)
           irstep = 0
        endif
        if (irstep .eq. 0) then
c          pass a dummy argument as y, which is not accessed in ums2s2
           yp = 1
           ly = 1
           sp = 1
           lw = 2*n
        else
c          pass w (yp ... yp+n-1) as y (1..n) to ums2s2
           yp = 2*n+1
           ly = n
           sp = 3*n+1
           lw = 4*n
        endif
 
c-----------------------------------------------------------------------
c  solve; optional iterative refinement and sparse backward error
c-----------------------------------------------------------------------
 
        call ums2s2 (n, job, transc, lux2-xp1+1, value (xp1),
     $     ip2-luir1+1, index (luir1), b, x,
     $     w, w (n+1), ly, w (yp), w (sp),
     $     cntl, icntl, info, rinfo, index (cpermp), index (rpermp),
     $     presrv, an, anz, index (app), index (aip), value (axp),
     $     on, max (1, nzoff), index (offpp), index (offip),
     $     value (offxp), nblks, index (lublpp), index (blkpp), irstep)
 
c-----------------------------------------------------------------------
c  print output arguments if requested
c-----------------------------------------------------------------------
 
c       error return label:
9000    continue
        call ums2p1 (3, 2,
     $          n, ne, job, transc, lvalue, lindex, value,
     $          index, keep, cntl, icntl, info, rinfo,
     $          b, x, n, w, lw)
        return
        end
 
        subroutine ums2su (nlu, npiv, n, lup, lui, lux, x, w)
        integer nlu, npiv, n, lup (nlu), lui (*)
        real
     $          lux (*), x (n), w (n)
 
c=== ums2su ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  solves ux = b, where u is the upper triangular factor of a matrix
c  (if btf not used) or a single diagonal block (if btf is used).
c  b is overwritten with the solution x.
 
c=======================================================================
c  input:
c=======================================================================
c
c       nlu:            number of lu arrowheads in the lu factors
c       npiv:           number of pivots found (normally n)
c       n:              order of matrix
c       lup (1..nlu):   pointer to lu arrowheads in lui
c       lui ( ... ):    integer values of lu arrowheads
c       lux ( ... ):    real values of lu arroheads
c       x (1..n):       the right-hand-side
 
c=======================================================================
c  workspace:
c=======================================================================
c
c       w (1..n)
 
c=======================================================================
c  output:
c=======================================================================
c
c       x (1..n):       the solution to ux=b
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2s2
c       subroutines called:     strsv, sgemv
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer j, k, s, luip, luxp, luk, ludegr, ludegc, lurp, uxp,
     $          lucp, col
        real
     $          one
        parameter (one = 1.0)
 
c  s:       an element, or lu arrowhead
c  k:       kth pivot
c  j:       jth column in u2 array in element s
c  luip:    s is in lui (luip...)
c  luxp:    real part of s is in lux (luxp...)
c  luk:     number of pivots in s
c  ludegc:  column degree of non-pivotal part of s
c  ludegr:  row degree of non-pivotal part of s
c  lucp:    pattern of column of s in lui (lucp...lucp+ludegc-1)
c  lurp:    pattern of row of s in lui (lurp...lurp+ludegr-1)
c  uxp:     the luk-by-ludegr u2 block of s is in lux (uxp...)
c  col:     column index
 
c=======================================================================
c  executable statments:
c=======================================================================
 
        k = npiv
        do 30 s = nlu, 1, -1
 
c          -------------------------------------------------------------
c          get s-th lu arrowhead (s = nlu..1, in reverse pivotal order)
c          -------------------------------------------------------------
 
           luip   = lup (s)
           luxp   = lui (luip)
           luk    = lui (luip+1)
           ludegr = lui (luip+2)
           ludegc = lui (luip+3)
           lucp   = (luip + 7)
           lurp   = lucp + ludegc
           uxp    = luxp + luk * (ludegc + luk)
 
           if (luk .eq. 1) then
 
c             ----------------------------------------------------------
c             only one pivot, stride-1 sparse dot product
c             ----------------------------------------------------------
 
cfpp$ nodepchk l
              do 10 j = 1, ludegr
                 col = lui (lurp+j-1)
c                row: k, u (row,col): lux (uxp+j-1)
                 x (k) = x (k) - lux (uxp+j-1) * x (col)
10            continue
c             divide by pivot, u (k,k): lux (luxp)
              x (k) = x (k) / lux (luxp)
              k = k - 1
 
           else
 
c             ----------------------------------------------------------
c             more than one pivot
c             ----------------------------------------------------------
 
              k = k - luk
              do 20 j = 1, ludegr
                 col = lui (lurp+j-1)
                 w (j) = x (col)
20            continue
              call sgemv ('n', luk, ludegr, -one,
     $           lux (uxp), luk, w, 1, one, x (k+1), 1)
              call strsv ('u', 'n', 'n', luk,
     $           lux (luxp), ludegc + luk, x (k+1), 1)
 
           endif
 
30      continue
        return
        end
 
        subroutine ums2ut (nlu, npiv, n, lup, lui, lux, x, w)
        integer nlu, npiv, n, lup (nlu), lui (*)
        real
     $          lux (*), x (n), w (n)
 
c=== ums2ut ============================================================
c
c  unsymmetric-pattern multifrontal package (umfpack). version 2.0s.
c  copyright (c) 1995, timothy a. davis, university of florida, usa.
c  joint work with iain s. duff, rutherford appleton laboratory, uk.
c  september 1995. work supported by the national science foundation
c  (dms-9223088 and dms-9504974) and the state of florida; and by cray
c  research inc. through the allocation of supercomputing resources.
 
c***********************************************************************
c* notice:  "the umfpack package may be used solely for educational,   *
c* research, and benchmarking purposes by non-profit organizations and *
c* the u.s. government.  commericial and other organizations may make  *
c* use of umfpack solely for benchmarking purposes only.  umfpack may  *
c* be modified by or on behalf of the user for such use but at no time *
c* shall umfpack or any such modified version of umfpack become the    *
c* property of the user.  umfpack is provided without warranty of any  *
c* kind, either expressed or implied.  neither the authors nor their   *
c* employers shall be liable for any direct or consequential loss or   *
c* damage whatsoever arising out of the use or misuse of umfpack by    *
c* the user.  umfpack must not be sold.  you may make copies of        *
c* umfpack, but this notice and the copyright notice must appear in    *
c* all copies.  any other use of umfpack requires written permission.  *
c* your use of umfpack is an implicit agreement to these conditions."  *
c*                                                                     *
c* the ma38 package in the harwell subroutine library (hsl) has        *
c* equivalent functionality (and identical calling interface) as       *
c* umfpack.  it is available for commercial use.   technical reports,  *
c* information on hsl, and matrices are available via the world wide   *
c* web at http://www.cis.rl.ac.uk/struct/arcd/num.html, or by          *
c* anonymous ftp at seamus.cc.rl.ac.uk/pub.  also contact john         *
c* harding, harwell subroutine library, b 552, aea technology,         *
c* harwell, didcot, oxon ox11 0ra, england.                            *
c* telephone (44) 1235 434573, fax (44) 1235 434340,                   *
c* email john.harding@aeat.co.uk, who will provide details of price    *
c* and conditions of use.                                              *
c***********************************************************************
 
c=======================================================================
c  not user-callable.
 
c=======================================================================
c  description:
c=======================================================================
c
c  solves u'x = b, where u is the upper triangular factor of a matrix
c  (if btf not used) or a single diagonal block (if btf is used).
c  b is overwritten with the solution x.
 
c=======================================================================
c  input:
c=======================================================================
c
c       nlu:            number of lu arrowheads in the lu factors
c       npiv:           number of pivots found (normally n)
c       n:              order of matrix
c       lup (1..nlu):   pointer to lu arrowheads in lui
c       lui ( ... ):    integer values of lu arrowheads
c       lux ( ... ):    real values of lu arroheads
c       x (1..n):       the right-hand-side
 
c=======================================================================
c  workspace:
c=======================================================================
c
c       w (1..n)
 
c=======================================================================
c  output:
c=======================================================================
c
c       x (1..n):       the solution to u'x=b
 
c=======================================================================
c  subroutines and functions called / called by:
c=======================================================================
c
c       called by subroutine:   ums2s2
c       subroutines called:     strsv, sgemv
 
c=======================================================================
c  local scalars:
c=======================================================================
 
        integer i, k, s, luip, luxp, luk, ludegr, ludegc, lurp, uxp,
     $          lucp, row
        real
     $          one
        parameter (one = 1.0)
 
c  s:       an element, or lu arrowhead
c  k:       kth pivot
c  i:       ith column in u2' array in element s
c  luip:    s is in lui (luip...)
c  luxp:    real part of s is in lux (luxp...)
c  luk:     number of pivots in s
c  ludegc:  column degree of non-pivotal part of s
c  ludegr:  row degree of non-pivotal part of s
c  lucp:    pattern of column of s in lui (lucp...lucp+ludegc-1)
c  lurp:    pattern of row of s in lui (lurp...lurp+ludegr-1)
c  uxp:     the luk-by-ludegr u2 block of s is in lux (uxp...)
c  row:     row index
 
c=======================================================================
c  executable statments:
c=======================================================================
 
        k = 0
        do 40 s = 1, nlu
 
c          -------------------------------------------------------------
c          get s-th lu arrowhead (s = 1..nlu, in pivotal order)
c          -------------------------------------------------------------
 
           luip   = lup (s)
           luxp   = lui (luip)
           luk    = lui (luip+1)
           ludegr = lui (luip+2)
           ludegc = lui (luip+3)
           lucp   = (luip + 7)
           lurp   = lucp + ludegc
 
           if (luk .eq. 1) then
 
c             ----------------------------------------------------------
c             only one pivot, stride-1 sparse saxpy
c             ----------------------------------------------------------
 
              k = k + 1
c             divide by pivot, u (k,k): lux (luxp)
              x (k) = x (k) / lux (luxp)
              uxp = luxp + ludegc + 1
cfpp$ nodepchk l
              do 10 i = 1, ludegr
                 row = lui (lurp+i-1)
c                col: k, u (row,col): lux (uxp+i-1)
                 x (row) = x (row) - lux (uxp+i-1) * x (k)
10            continue
 
           else
 
c             ----------------------------------------------------------
c             more than one pivot
c             ----------------------------------------------------------
 
              uxp = luxp + luk * (ludegc + luk)
              call strsv ('u', 't', 'n', luk,
     $           lux (luxp), ludegc + luk, x (k+1), 1)
              do 20 i = 1, ludegr
                 row = lui (lurp+i-1)
                 w (i) = x (row)
20            continue
              call sgemv ('t', luk, ludegr, -one,
     $           lux (uxp), luk, x (k+1), 1, one, w, 1)
              do 30 i = 1, ludegr
                 row = lui (lurp+i-1)
                 x (row) = w (i)
30            continue
              k = k + luk
 
           endif
 
40      continue
        return
        end
      integer function isamax(n,sx,incx)
c
c     finds the index of element having max. absolute value.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      real sx(*),smax
      integer i,incx,ix,n
c
      isamax = 0
      if( n.lt.1 .or. incx.le.0 ) return
      isamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      smax = abs(sx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(abs(sx(ix)).le.smax) go to 5
         isamax = i
         smax = abs(sx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 smax = abs(sx(1))
      do 30 i = 2,n
         if(abs(sx(i)).le.smax) go to 30
         isamax = i
         smax = abs(sx(i))
   30 continue
      return
      end
      subroutine sgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb,
     $                   beta, c, ldc )
*     .. scalar arguments ..
      character*1        transa, transb
      integer            m, n, k, lda, ldb, ldc
      real               alpha, beta
*     .. array arguments ..
      real               a( lda, * ), b( ldb, * ), c( ldc, * )
*     ..
*
*  purpose
*  =======
*
*  sgemm  performs one of the matrix-matrix operations
*
*     c := alpha*op( a )*op( b ) + beta*c,
*
*  where  op( x ) is one of
*
*     op( x ) = x   or   op( x ) = x',
*
*  alpha and beta are scalars, and a, b and c are matrices, with op( a )
*  an m by k matrix,  op( b )  a  k by n matrix and  c an m by n matrix.
*
*  parameters
*  ==========
*
*  transa - character*1.
*           on entry, transa specifies the form of op( a ) to be used in
*           the matrix multiplication as follows:
*
*              transa = 'n' or 'n',  op( a ) = a.
*
*              transa = 't' or 't',  op( a ) = a'.
*
*              transa = 'c' or 'c',  op( a ) = a'.
*
*           unchanged on exit.
*
*  transb - character*1.
*           on entry, transb specifies the form of op( b ) to be used in
*           the matrix multiplication as follows:
*
*              transb = 'n' or 'n',  op( b ) = b.
*
*              transb = 't' or 't',  op( b ) = b'.
*
*              transb = 'c' or 'c',  op( b ) = b'.
*
*           unchanged on exit.
*
*  m      - integer.
*           on entry,  m  specifies  the number  of rows  of the  matrix
*           op( a )  and of the  matrix  c.  m  must  be at least  zero.
*           unchanged on exit.
*
*  n      - integer.
*           on entry,  n  specifies the number  of columns of the matrix
*           op( b ) and the number of columns of the matrix c. n must be
*           at least zero.
*           unchanged on exit.
*
*  k      - integer.
*           on entry,  k  specifies  the number of columns of the matrix
*           op( a ) and the number of rows of the matrix op( b ). k must
*           be at least  zero.
*           unchanged on exit.
*
*  alpha  - real            .
*           on entry, alpha specifies the scalar alpha.
*           unchanged on exit.
*
*  a      - real             array of dimension ( lda, ka ), where ka is
*           k  when  transa = 'n' or 'n',  and is  m  otherwise.
*           before entry with  transa = 'n' or 'n',  the leading  m by k
*           part of the array  a  must contain the matrix  a,  otherwise
*           the leading  k by m  part of the array  a  must contain  the
*           matrix a.
*           unchanged on exit.
*
*  lda    - integer.
*           on entry, lda specifies the first dimension of a as declared
*           in the calling (sub) program. when  transa = 'n' or 'n' then
*           lda must be at least  max( 1, m ), otherwise  lda must be at
*           least  max( 1, k ).
*           unchanged on exit.
*
*  b      - real             array of dimension ( ldb, kb ), where kb is
*           n  when  transb = 'n' or 'n',  and is  k  otherwise.
*           before entry with  transb = 'n' or 'n',  the leading  k by n
*           part of the array  b  must contain the matrix  b,  otherwise
*           the leading  n by k  part of the array  b  must contain  the
*           matrix b.
*           unchanged on exit.
*
*  ldb    - integer.
*           on entry, ldb specifies the first dimension of b as declared
*           in the calling (sub) program. when  transb = 'n' or 'n' then
*           ldb must be at least  max( 1, k ), otherwise  ldb must be at
*           least  max( 1, n ).
*           unchanged on exit.
*
*  beta   - real            .
*           on entry,  beta  specifies the scalar  beta.  when  beta  is
*           supplied as zero then c need not be set on input.
*           unchanged on exit.
*
*  c      - real             array of dimension ( ldc, n ).
*           before entry, the leading  m by n  part of the array  c must
*           contain the matrix  c,  except when  beta  is zero, in which
*           case c need not be set on entry.
*           on exit, the array  c  is overwritten by the  m by n  matrix
*           ( alpha*op( a )*op( b ) + beta*c ).
*
*  ldc    - integer.
*           on entry, ldc specifies the first dimension of c as declared
*           in  the  calling  (sub)  program.   ldc  must  be  at  least
*           max( 1, m ).
*           unchanged on exit.
*
*
*  level 3 blas routine.
*
*  -- written on 8-february-1989.
*     jack dongarra, argonne national laboratory.
*     iain duff, aere harwell.
*     jeremy du croz, numerical algorithms group ltd.
*     sven hammarling, numerical algorithms group ltd.
*
*
*     .. external functions ..
      logical            lsame
      external           lsame
*     .. external subroutines ..
      external           xerbla
*     .. intrinsic functions ..
      intrinsic          max
*     .. local scalars ..
      logical            nota, notb
      integer            i, info, j, l, ncola, nrowa, nrowb
      real               temp
*     .. parameters ..
      real               one         , zero
      parameter        ( one = 1.0e+0, zero = 0.0e+0 )
*     ..
*     .. executable statements ..
*
*     set  nota  and  notb  as  true if  a  and  b  respectively are not
*     transposed and set  nrowa, ncola and  nrowb  as the number of rows
*     and  columns of  a  and the  number of  rows  of  b  respectively.
*
      nota  = lsame( transa, 'n' )
      notb  = lsame( transb, 'n' )
      if( nota )then
         nrowa = m
         ncola = k
      else
         nrowa = k
         ncola = m
      end if
      if( notb )then
         nrowb = k
      else
         nrowb = n
      end if
*
*     test the input parameters.
*
      info = 0
      if(      ( .not.nota                 ).and.
     $         ( .not.lsame( transa, 'c' ) ).and.
     $         ( .not.lsame( transa, 't' ) )      )then
         info = 1
      else if( ( .not.notb                 ).and.
     $         ( .not.lsame( transb, 'c' ) ).and.
     $         ( .not.lsame( transb, 't' ) )      )then
         info = 2
      else if( m  .lt.0               )then
         info = 3
      else if( n  .lt.0               )then
         info = 4
      else if( k  .lt.0               )then
         info = 5
      else if( lda.lt.max( 1, nrowa ) )then
         info = 8
      else if( ldb.lt.max( 1, nrowb ) )then
         info = 10
      else if( ldc.lt.max( 1, m     ) )then
         info = 13
      end if
      if( info.ne.0 )then
         call xerbla( 'sgemm ', info )
         return
      end if
*
*     quick return if possible.
*
      if( ( m.eq.0 ).or.( n.eq.0 ).or.
     $    ( ( ( alpha.eq.zero ).or.( k.eq.0 ) ).and.( beta.eq.one ) ) )
     $   return
*
*     and if  alpha.eq.zero.
*
      if( alpha.eq.zero )then
         if( beta.eq.zero )then
            do 20, j = 1, n
               do 10, i = 1, m
                  c( i, j ) = zero
   10          continue
   20       continue
         else
            do 40, j = 1, n
               do 30, i = 1, m
                  c( i, j ) = beta*c( i, j )
   30          continue
   40       continue
         end if
         return
      end if
*
*     start the operations.
*
      if( notb )then
         if( nota )then
*
*           form  c := alpha*a*b + beta*c.
*
            do 90, j = 1, n
               if( beta.eq.zero )then
                  do 50, i = 1, m
                     c( i, j ) = zero
   50             continue
               else if( beta.ne.one )then
                  do 60, i = 1, m
                     c( i, j ) = beta*c( i, j )
   60             continue
               end if
               do 80, l = 1, k
                  if( b( l, j ).ne.zero )then
                     temp = alpha*b( l, j )
                     do 70, i = 1, m
                        c( i, j ) = c( i, j ) + temp*a( i, l )
   70                continue
                  end if
   80          continue
   90       continue
         else
*
*           form  c := alpha*a'*b + beta*c
*
            do 120, j = 1, n
               do 110, i = 1, m
                  temp = zero
                  do 100, l = 1, k
                     temp = temp + a( l, i )*b( l, j )
  100             continue
                  if( beta.eq.zero )then
                     c( i, j ) = alpha*temp
                  else
                     c( i, j ) = alpha*temp + beta*c( i, j )
                  end if
  110          continue
  120       continue
         end if
      else
         if( nota )then
*
*           form  c := alpha*a*b' + beta*c
*
            do 170, j = 1, n
               if( beta.eq.zero )then
                  do 130, i = 1, m
                     c( i, j ) = zero
  130             continue
               else if( beta.ne.one )then
                  do 140, i = 1, m
                     c( i, j ) = beta*c( i, j )
  140             continue
               end if
               do 160, l = 1, k
                  if( b( j, l ).ne.zero )then
                     temp = alpha*b( j, l )
                     do 150, i = 1, m
                        c( i, j ) = c( i, j ) + temp*a( i, l )
  150                continue
                  end if
  160          continue
  170       continue
         else
*
*           form  c := alpha*a'*b' + beta*c
*
            do 200, j = 1, n
               do 190, i = 1, m
                  temp = zero
                  do 180, l = 1, k
                     temp = temp + a( l, i )*b( j, l )
  180             continue
                  if( beta.eq.zero )then
                     c( i, j ) = alpha*temp
                  else
                     c( i, j ) = alpha*temp + beta*c( i, j )
                  end if
  190          continue
  200       continue
         end if
      end if
*
      return
*
*     end of sgemm .
*
      end
      subroutine sgemv ( trans, m, n, alpha, a, lda, x, incx,
     $                   beta, y, incy )
*     .. scalar arguments ..
      real               alpha, beta
      integer            incx, incy, lda, m, n
      character*1        trans
*     .. array arguments ..
      real               a( lda, * ), x( * ), y( * )
*     ..
*
*  purpose
*  =======
*
*  sgemv  performs one of the matrix-vector operations
*
*     y := alpha*a*x + beta*y,   or   y := alpha*a'*x + beta*y,
*
*  where alpha and beta are scalars, x and y are vectors and a is an
*  m by n matrix.
*
*  parameters
*  ==========
*
*  trans  - character*1.
*           on entry, trans specifies the operation to be performed as
*           follows:
*
*              trans = 'n' or 'n'   y := alpha*a*x + beta*y.
*
*              trans = 't' or 't'   y := alpha*a'*x + beta*y.
*
*              trans = 'c' or 'c'   y := alpha*a'*x + beta*y.
*
*           unchanged on exit.
*
*  m      - integer.
*           on entry, m specifies the number of rows of the matrix a.
*           m must be at least zero.
*           unchanged on exit.
*
*  n      - integer.
*           on entry, n specifies the number of columns of the matrix a.
*           n must be at least zero.
*           unchanged on exit.
*
*  alpha  - real            .
*           on entry, alpha specifies the scalar alpha.
*           unchanged on exit.
*
*  a      - real             array of dimension ( lda, n ).
*           before entry, the leading m by n part of the array a must
*           contain the matrix of coefficients.
*           unchanged on exit.
*
*  lda    - integer.
*           on entry, lda specifies the first dimension of a as declared
*           in the calling (sub) program. lda must be at least
*           max( 1, m ).
*           unchanged on exit.
*
*  x      - real             array of dimension at least
*           ( 1 + ( n - 1 )*abs( incx ) ) when trans = 'n' or 'n'
*           and at least
*           ( 1 + ( m - 1 )*abs( incx ) ) otherwise.
*           before entry, the incremented array x must contain the
*           vector x.
*           unchanged on exit.
*
*  incx   - integer.
*           on entry, incx specifies the increment for the elements of
*           x. incx must not be zero.
*           unchanged on exit.
*
*  beta   - real            .
*           on entry, beta specifies the scalar beta. when beta is
*           supplied as zero then y need not be set on input.
*           unchanged on exit.
*
*  y      - real             array of dimension at least
*           ( 1 + ( m - 1 )*abs( incy ) ) when trans = 'n' or 'n'
*           and at least
*           ( 1 + ( n - 1 )*abs( incy ) ) otherwise.
*           before entry with beta non-zero, the incremented array y
*           must contain the vector y. on exit, y is overwritten by the
*           updated vector y.
*
*  incy   - integer.
*           on entry, incy specifies the increment for the elements of
*           y. incy must not be zero.
*           unchanged on exit.
*
*
*  level 2 blas routine.
*
*  -- written on 22-october-1986.
*     jack dongarra, argonne national lab.
*     jeremy du croz, nag central office.
*     sven hammarling, nag central office.
*     richard hanson, sandia national labs.
*
*
*     .. parameters ..
      real               one         , zero
      parameter        ( one = 1.0e+0, zero = 0.0e+0 )
*     .. local scalars ..
      real               temp
      integer            i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
*     .. external functions ..
      logical            lsame
      external           lsame
*     .. external subroutines ..
      external           xerbla
*     .. intrinsic functions ..
      intrinsic          max
*     ..
*     .. executable statements ..
*
*     test the input parameters.
*
      info = 0
      if     ( .not.lsame( trans, 'n' ).and.
     $         .not.lsame( trans, 't' ).and.
     $         .not.lsame( trans, 'c' )      )then
         info = 1
      else if( m.lt.0 )then
         info = 2
      else if( n.lt.0 )then
         info = 3
      else if( lda.lt.max( 1, m ) )then
         info = 6
      else if( incx.eq.0 )then
         info = 8
      else if( incy.eq.0 )then
         info = 11
      end if
      if( info.ne.0 )then
         call xerbla( 'sgemv ', info )
         return
      end if
*
*     quick return if possible.
*
      if( ( m.eq.0 ).or.( n.eq.0 ).or.
     $    ( ( alpha.eq.zero ).and.( beta.eq.one ) ) )
     $   return
*
*     set  lenx  and  leny, the lengths of the vectors x and y, and set
*     up the start points in  x  and  y.
*
      if( lsame( trans, 'n' ) )then
         lenx = n
         leny = m
      else
         lenx = m
         leny = n
      end if
      if( incx.gt.0 )then
         kx = 1
      else
         kx = 1 - ( lenx - 1 )*incx
      end if
      if( incy.gt.0 )then
         ky = 1
      else
         ky = 1 - ( leny - 1 )*incy
      end if
*
*     start the operations. in this version the elements of a are
*     accessed sequentially with one pass through a.
*
*     first form  y := beta*y.
*
      if( beta.ne.one )then
         if( incy.eq.1 )then
            if( beta.eq.zero )then
               do 10, i = 1, leny
                  y( i ) = zero
   10          continue
            else
               do 20, i = 1, leny
                  y( i ) = beta*y( i )
   20          continue
            end if
         else
            iy = ky
            if( beta.eq.zero )then
               do 30, i = 1, leny
                  y( iy ) = zero
                  iy      = iy   + incy
   30          continue
            else
               do 40, i = 1, leny
                  y( iy ) = beta*y( iy )
                  iy      = iy           + incy
   40          continue
            end if
         end if
      end if
      if( alpha.eq.zero )
     $   return
      if( lsame( trans, 'n' ) )then
*
*        form  y := alpha*a*x + y.
*
         jx = kx
         if( incy.eq.1 )then
            do 60, j = 1, n
               if( x( jx ).ne.zero )then
                  temp = alpha*x( jx )
                  do 50, i = 1, m
                     y( i ) = y( i ) + temp*a( i, j )
   50             continue
               end if
               jx = jx + incx
   60       continue
         else
            do 80, j = 1, n
               if( x( jx ).ne.zero )then
                  temp = alpha*x( jx )
                  iy   = ky
                  do 70, i = 1, m
                     y( iy ) = y( iy ) + temp*a( i, j )
                     iy      = iy      + incy
   70             continue
               end if
               jx = jx + incx
   80       continue
         end if
      else
*
*        form  y := alpha*a'*x + y.
*
         jy = ky
         if( incx.eq.1 )then
            do 100, j = 1, n
               temp = zero
               do 90, i = 1, m
                  temp = temp + a( i, j )*x( i )
   90          continue
               y( jy ) = y( jy ) + alpha*temp
               jy      = jy      + incy
  100       continue
         else
            do 120, j = 1, n
               temp = zero
               ix   = kx
               do 110, i = 1, m
                  temp = temp + a( i, j )*x( ix )
                  ix   = ix   + incx
  110          continue
               y( jy ) = y( jy ) + alpha*temp
               jy      = jy      + incy
  120       continue
         end if
      end if
*
      return
*
*     end of sgemv .
*
      end
      subroutine strsm ( side, uplo, transa, diag, m, n, alpha, a, lda,
     $                   b, ldb )
*     .. scalar arguments ..
      character*1        side, uplo, transa, diag
      integer            m, n, lda, ldb
      real               alpha
*     .. array arguments ..
      real               a( lda, * ), b( ldb, * )
*     ..
*
*  purpose
*  =======
*
*  strsm  solves one of the matrix equations
*
*     op( a )*x = alpha*b,   or   x*op( a ) = alpha*b,
*
*  where alpha is a scalar, x and b are m by n matrices, a is a unit, or
*  non-unit,  upper or lower triangular matrix  and  op( a )  is one  of
*
*     op( a ) = a   or   op( a ) = a'.
*
*  the matrix x is overwritten on b.
*
*  parameters
*  ==========
*
*  side   - character*1.
*           on entry, side specifies whether op( a ) appears on the left
*           or right of x as follows:
*
*              side = 'l' or 'l'   op( a )*x = alpha*b.
*
*              side = 'r' or 'r'   x*op( a ) = alpha*b.
*
*           unchanged on exit.
*
*  uplo   - character*1.
*           on entry, uplo specifies whether the matrix a is an upper or
*           lower triangular matrix as follows:
*
*              uplo = 'u' or 'u'   a is an upper triangular matrix.
*
*              uplo = 'l' or 'l'   a is a lower triangular matrix.
*
*           unchanged on exit.
*
*  transa - character*1.
*           on entry, transa specifies the form of op( a ) to be used in
*           the matrix multiplication as follows:
*
*              transa = 'n' or 'n'   op( a ) = a.
*
*              transa = 't' or 't'   op( a ) = a'.
*
*              transa = 'c' or 'c'   op( a ) = a'.
*
*           unchanged on exit.
*
*  diag   - character*1.
*           on entry, diag specifies whether or not a is unit triangular
*           as follows:
*
*              diag = 'u' or 'u'   a is assumed to be unit triangular.
*
*              diag = 'n' or 'n'   a is not assumed to be unit
*                                  triangular.
*
*           unchanged on exit.
*
*  m      - integer.
*           on entry, m specifies the number of rows of b. m must be at
*           least zero.
*           unchanged on exit.
*
*  n      - integer.
*           on entry, n specifies the number of columns of b.  n must be
*           at least zero.
*           unchanged on exit.
*
*  alpha  - real            .
*           on entry,  alpha specifies the scalar  alpha. when  alpha is
*           zero then  a is not referenced and  b need not be set before
*           entry.
*           unchanged on exit.
*
*  a      - real             array of dimension ( lda, k ), where k is m
*           when  side = 'l' or 'l'  and is  n  when  side = 'r' or 'r'.
*           before entry  with  uplo = 'u' or 'u',  the  leading  k by k
*           upper triangular part of the array  a must contain the upper
*           triangular matrix  and the strictly lower triangular part of
*           a is not referenced.
*           before entry  with  uplo = 'l' or 'l',  the  leading  k by k
*           lower triangular part of the array  a must contain the lower
*           triangular matrix  and the strictly upper triangular part of
*           a is not referenced.
*           note that when  diag = 'u' or 'u',  the diagonal elements of
*           a  are not referenced either,  but are assumed to be  unity.
*           unchanged on exit.
*
*  lda    - integer.
*           on entry, lda specifies the first dimension of a as declared
*           in the calling (sub) program.  when  side = 'l' or 'l'  then
*           lda  must be at least  max( 1, m ),  when  side = 'r' or 'r'
*           then lda must be at least max( 1, n ).
*           unchanged on exit.
*
*  b      - real             array of dimension ( ldb, n ).
*           before entry,  the leading  m by n part of the array  b must
*           contain  the  right-hand  side  matrix  b,  and  on exit  is
*           overwritten by the solution matrix  x.
*
*  ldb    - integer.
*           on entry, ldb specifies the first dimension of b as declared
*           in  the  calling  (sub)  program.   ldb  must  be  at  least
*           max( 1, m ).
*           unchanged on exit.
*
*
*  level 3 blas routine.
*
*
*  -- written on 8-february-1989.
*     jack dongarra, argonne national laboratory.
*     iain duff, aere harwell.
*     jeremy du croz, numerical algorithms group ltd.
*     sven hammarling, numerical algorithms group ltd.
*
*
*     .. external functions ..
      logical            lsame
      external           lsame
*     .. external subroutines ..
      external           xerbla
*     .. intrinsic functions ..
      intrinsic          max
*     .. local scalars ..
      logical            lside, nounit, upper
      integer            i, info, j, k, nrowa
      real               temp
*     .. parameters ..
      real               one         , zero
      parameter        ( one = 1.0e+0, zero = 0.0e+0 )
*     ..
*     .. executable statements ..
*
*     test the input parameters.
*
      lside  = lsame( side  , 'l' )
      if( lside )then
         nrowa = m
      else
         nrowa = n
      end if
      nounit = lsame( diag  , 'n' )
      upper  = lsame( uplo  , 'u' )
*
      info   = 0
      if(      ( .not.lside                ).and.
     $         ( .not.lsame( side  , 'r' ) )      )then
         info = 1
      else if( ( .not.upper                ).and.
     $         ( .not.lsame( uplo  , 'l' ) )      )then
         info = 2
      else if( ( .not.lsame( transa, 'n' ) ).and.
     $         ( .not.lsame( transa, 't' ) ).and.
     $         ( .not.lsame( transa, 'c' ) )      )then
         info = 3
      else if( ( .not.lsame( diag  , 'u' ) ).and.
     $         ( .not.lsame( diag  , 'n' ) )      )then
         info = 4
      else if( m  .lt.0               )then
         info = 5
      else if( n  .lt.0               )then
         info = 6
      else if( lda.lt.max( 1, nrowa ) )then
         info = 9
      else if( ldb.lt.max( 1, m     ) )then
         info = 11
      end if
      if( info.ne.0 )then
         call xerbla( 'strsm ', info )
         return
      end if
*
*     quick return if possible.
*
      if( n.eq.0 )
     $   return
*
*     and when  alpha.eq.zero.
*
      if( alpha.eq.zero )then
         do 20, j = 1, n
            do 10, i = 1, m
               b( i, j ) = zero
   10       continue
   20    continue
         return
      end if
*
*     start the operations.
*
      if( lside )then
         if( lsame( transa, 'n' ) )then
*
*           form  b := alpha*inv( a )*b.
*
            if( upper )then
               do 60, j = 1, n
                  if( alpha.ne.one )then
                     do 30, i = 1, m
                        b( i, j ) = alpha*b( i, j )
   30                continue
                  end if
                  do 50, k = m, 1, -1
                     if( b( k, j ).ne.zero )then
                        if( nounit )
     $                     b( k, j ) = b( k, j )/a( k, k )
                        do 40, i = 1, k - 1
                           b( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   40                   continue
                     end if
   50             continue
   60          continue
            else
               do 100, j = 1, n
                  if( alpha.ne.one )then
                     do 70, i = 1, m
                        b( i, j ) = alpha*b( i, j )
   70                continue
                  end if
                  do 90 k = 1, m
                     if( b( k, j ).ne.zero )then
                        if( nounit )
     $                     b( k, j ) = b( k, j )/a( k, k )
                        do 80, i = k + 1, m
                           b( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   80                   continue
                     end if
   90             continue
  100          continue
            end if
         else
*
*           form  b := alpha*inv( a' )*b.
*
            if( upper )then
               do 130, j = 1, n
                  do 120, i = 1, m
                     temp = alpha*b( i, j )
                     do 110, k = 1, i - 1
                        temp = temp - a( k, i )*b( k, j )
  110                continue
                     if( nounit )
     $                  temp = temp/a( i, i )
                     b( i, j ) = temp
  120             continue
  130          continue
            else
               do 160, j = 1, n
                  do 150, i = m, 1, -1
                     temp = alpha*b( i, j )
                     do 140, k = i + 1, m
                        temp = temp - a( k, i )*b( k, j )
  140                continue
                     if( nounit )
     $                  temp = temp/a( i, i )
                     b( i, j ) = temp
  150             continue
  160          continue
            end if
         end if
      else
         if( lsame( transa, 'n' ) )then
*
*           form  b := alpha*b*inv( a ).
*
            if( upper )then
               do 210, j = 1, n
                  if( alpha.ne.one )then
                     do 170, i = 1, m
                        b( i, j ) = alpha*b( i, j )
  170                continue
                  end if
                  do 190, k = 1, j - 1
                     if( a( k, j ).ne.zero )then
                        do 180, i = 1, m
                           b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  180                   continue
                     end if
  190             continue
                  if( nounit )then
                     temp = one/a( j, j )
                     do 200, i = 1, m
                        b( i, j ) = temp*b( i, j )
  200                continue
                  end if
  210          continue
            else
               do 260, j = n, 1, -1
                  if( alpha.ne.one )then
                     do 220, i = 1, m
                        b( i, j ) = alpha*b( i, j )
  220                continue
                  end if
                  do 240, k = j + 1, n
                     if( a( k, j ).ne.zero )then
                        do 230, i = 1, m
                           b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  230                   continue
                     end if
  240             continue
                  if( nounit )then
                     temp = one/a( j, j )
                     do 250, i = 1, m
                       b( i, j ) = temp*b( i, j )
  250                continue
                  end if
  260          continue
            end if
         else
*
*           form  b := alpha*b*inv( a' ).
*
            if( upper )then
               do 310, k = n, 1, -1
                  if( nounit )then
                     temp = one/a( k, k )
                     do 270, i = 1, m
                        b( i, k ) = temp*b( i, k )
  270                continue
                  end if
                  do 290, j = 1, k - 1
                     if( a( j, k ).ne.zero )then
                        temp = a( j, k )
                        do 280, i = 1, m
                           b( i, j ) = b( i, j ) - temp*b( i, k )
  280                   continue
                     end if
  290             continue
                  if( alpha.ne.one )then
                     do 300, i = 1, m
                        b( i, k ) = alpha*b( i, k )
  300                continue
                  end if
  310          continue
            else
               do 360, k = 1, n
                  if( nounit )then
                     temp = one/a( k, k )
                     do 320, i = 1, m
                        b( i, k ) = temp*b( i, k )
  320                continue
                  end if
                  do 340, j = k + 1, n
                     if( a( j, k ).ne.zero )then
                        temp = a( j, k )
                        do 330, i = 1, m
                           b( i, j ) = b( i, j ) - temp*b( i, k )
  330                   continue
                     end if
  340             continue
                  if( alpha.ne.one )then
                     do 350, i = 1, m
                        b( i, k ) = alpha*b( i, k )
  350                continue
                  end if
  360          continue
            end if
         end if
      end if
*
      return
*
*     end of strsm .
*
      end
      subroutine strsv ( uplo, trans, diag, n, a, lda, x, incx )
*     .. scalar arguments ..
      integer            incx, lda, n
      character*1        diag, trans, uplo
*     .. array arguments ..
      real               a( lda, * ), x( * )
*     ..
*
*  purpose
*  =======
*
*  strsv  solves one of the systems of equations
*
*     a*x = b,   or   a'*x = b,
*
*  where b and x are n element vectors and a is an n by n unit, or
*  non-unit, upper or lower triangular matrix.
*
*  no test for singularity or near-singularity is included in this
*  routine. such tests must be performed before calling this routine.
*
*  parameters
*  ==========
*
*  uplo   - character*1.
*           on entry, uplo specifies whether the matrix is an upper or
*           lower triangular matrix as follows:
*
*              uplo = 'u' or 'u'   a is an upper triangular matrix.
*
*              uplo = 'l' or 'l'   a is a lower triangular matrix.
*
*           unchanged on exit.
*
*  trans  - character*1.
*           on entry, trans specifies the equations to be solved as
*           follows:
*
*              trans = 'n' or 'n'   a*x = b.
*
*              trans = 't' or 't'   a'*x = b.
*
*              trans = 'c' or 'c'   a'*x = b.
*
*           unchanged on exit.
*
*  diag   - character*1.
*           on entry, diag specifies whether or not a is unit
*           triangular as follows:
*
*              diag = 'u' or 'u'   a is assumed to be unit triangular.
*
*              diag = 'n' or 'n'   a is not assumed to be unit
*                                  triangular.
*
*           unchanged on exit.
*
*  n      - integer.
*           on entry, n specifies the order of the matrix a.
*           n must be at least zero.
*           unchanged on exit.
*
*  a      - real             array of dimension ( lda, n ).
*           before entry with  uplo = 'u' or 'u', the leading n by n
*           upper triangular part of the array a must contain the upper
*           triangular matrix and the strictly lower triangular part of
*           a is not referenced.
*           before entry with uplo = 'l' or 'l', the leading n by n
*           lower triangular part of the array a must contain the lower
*           triangular matrix and the strictly upper triangular part of
*           a is not referenced.
*           note that when  diag = 'u' or 'u', the diagonal elements of
*           a are not referenced either, but are assumed to be unity.
*           unchanged on exit.
*
*  lda    - integer.
*           on entry, lda specifies the first dimension of a as declared
*           in the calling (sub) program. lda must be at least
*           max( 1, n ).
*           unchanged on exit.
*
*  x      - real             array of dimension at least
*           ( 1 + ( n - 1 )*abs( incx ) ).
*           before entry, the incremented array x must contain the n
*           element right-hand side vector b. on exit, x is overwritten
*           with the solution vector x.
*
*  incx   - integer.
*           on entry, incx specifies the increment for the elements of
*           x. incx must not be zero.
*           unchanged on exit.
*
*
*  level 2 blas routine.
*
*  -- written on 22-october-1986.
*     jack dongarra, argonne national lab.
*     jeremy du croz, nag central office.
*     sven hammarling, nag central office.
*     richard hanson, sandia national labs.
*
*
*     .. parameters ..
      real               zero
      parameter        ( zero = 0.0e+0 )
*     .. local scalars ..
      real               temp
      integer            i, info, ix, j, jx, kx
      logical            nounit
*     .. external functions ..
      logical            lsame
      external           lsame
*     .. external subroutines ..
      external           xerbla
*     .. intrinsic functions ..
      intrinsic          max
*     ..
*     .. executable statements ..
*
*     test the input parameters.
*
      info = 0
      if     ( .not.lsame( uplo , 'u' ).and.
     $         .not.lsame( uplo , 'l' )      )then
         info = 1
      else if( .not.lsame( trans, 'n' ).and.
     $         .not.lsame( trans, 't' ).and.
     $         .not.lsame( trans, 'c' )      )then
         info = 2
      else if( .not.lsame( diag , 'u' ).and.
     $         .not.lsame( diag , 'n' )      )then
         info = 3
      else if( n.lt.0 )then
         info = 4
      else if( lda.lt.max( 1, n ) )then
         info = 6
      else if( incx.eq.0 )then
         info = 8
      end if
      if( info.ne.0 )then
         call xerbla( 'strsv ', info )
         return
      end if
*
*     quick return if possible.
*
      if( n.eq.0 )
     $   return
*
      nounit = lsame( diag, 'n' )
*
*     set up the start point in x if the increment is not unity. this
*     will be  ( n - 1 )*incx  too small for descending loops.
*
      if( incx.le.0 )then
         kx = 1 - ( n - 1 )*incx
      else if( incx.ne.1 )then
         kx = 1
      end if
*
*     start the operations. in this version the elements of a are
*     accessed sequentially with one pass through a.
*
      if( lsame( trans, 'n' ) )then
*
*        form  x := inv( a )*x.
*
         if( lsame( uplo, 'u' ) )then
            if( incx.eq.1 )then
               do 20, j = n, 1, -1
                  if( x( j ).ne.zero )then
                     if( nounit )
     $                  x( j ) = x( j )/a( j, j )
                     temp = x( j )
                     do 10, i = j - 1, 1, -1
                        x( i ) = x( i ) - temp*a( i, j )
   10                continue
                  end if
   20          continue
            else
               jx = kx + ( n - 1 )*incx
               do 40, j = n, 1, -1
                  if( x( jx ).ne.zero )then
                     if( nounit )
     $                  x( jx ) = x( jx )/a( j, j )
                     temp = x( jx )
                     ix   = jx
                     do 30, i = j - 1, 1, -1
                        ix      = ix      - incx
                        x( ix ) = x( ix ) - temp*a( i, j )
   30                continue
                  end if
                  jx = jx - incx
   40          continue
            end if
         else
            if( incx.eq.1 )then
               do 60, j = 1, n
                  if( x( j ).ne.zero )then
                     if( nounit )
     $                  x( j ) = x( j )/a( j, j )
                     temp = x( j )
                     do 50, i = j + 1, n
                        x( i ) = x( i ) - temp*a( i, j )
   50                continue
                  end if
   60          continue
            else
               jx = kx
               do 80, j = 1, n
                  if( x( jx ).ne.zero )then
                     if( nounit )
     $                  x( jx ) = x( jx )/a( j, j )
                     temp = x( jx )
                     ix   = jx
                     do 70, i = j + 1, n
                        ix      = ix      + incx
                        x( ix ) = x( ix ) - temp*a( i, j )
   70                continue
                  end if
                  jx = jx + incx
   80          continue
            end if
         end if
      else
*
*        form  x := inv( a' )*x.
*
         if( lsame( uplo, 'u' ) )then
            if( incx.eq.1 )then
               do 100, j = 1, n
                  temp = x( j )
                  do 90, i = 1, j - 1
                     temp = temp - a( i, j )*x( i )
   90             continue
                  if( nounit )
     $               temp = temp/a( j, j )
                  x( j ) = temp
  100          continue
            else
               jx = kx
               do 120, j = 1, n
                  temp = x( jx )
                  ix   = kx
                  do 110, i = 1, j - 1
                     temp = temp - a( i, j )*x( ix )
                     ix   = ix   + incx
  110             continue
                  if( nounit )
     $               temp = temp/a( j, j )
                  x( jx ) = temp
                  jx      = jx   + incx
  120          continue
            end if
         else
            if( incx.eq.1 )then
               do 140, j = n, 1, -1
                  temp = x( j )
                  do 130, i = n, j + 1, -1
                     temp = temp - a( i, j )*x( i )
  130             continue
                  if( nounit )
     $               temp = temp/a( j, j )
                  x( j ) = temp
  140          continue
            else
               kx = kx + ( n - 1 )*incx
               jx = kx
               do 160, j = n, 1, -1
                  temp = x( jx )
                  ix   = kx
                  do 150, i = n, j + 1, -1
                     temp = temp - a( i, j )*x( ix )
                     ix   = ix   - incx
  150             continue
                  if( nounit )
     $               temp = temp/a( j, j )
                  x( jx ) = temp
                  jx      = jx   - incx
  160          continue
            end if
         end if
      end if
*
      return
*
*     end of strsv .
*
      end
      LOGICAL          FUNCTION LSAME( CA, CB )
*
*  -- LAPACK auxiliary routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     January 31, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          CA, CB
*     ..
*
*  Purpose
*  =======
*
*  LSAME returns .TRUE. if CA is the same letter as CB regardless of
*  case.
*
*  Arguments
*  =========
*
*  CA      (input) CHARACTER*1
*  CB      (input) CHARACTER*1
*          CA and CB specify the single characters to be compared.
*
* =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
*     ..
*     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
*     ..
*     .. Executable Statements ..
*
*     Test if the characters are equal
*
      LSAME = CA.EQ.CB
      IF( LSAME )
     $   RETURN
*
*     Now test for equivalence if both characters are alphabetic.
*
      ZCODE = ICHAR( 'Z' )
*
*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
*     machines, on which ICHAR returns a value with bit 8 set.
*     ICHAR('A') on Prime machines returns 193 which is the same as
*     ICHAR('A') on an EBCDIC machine.
*
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
*
      IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
*
*        ASCII is assumed - ZCODE is the ASCII code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
         IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
*
      ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
*
*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.
     $       INTA.GE.145 .AND. INTA.LE.153 .OR.
     $       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.
     $       INTB.GE.145 .AND. INTB.LE.153 .OR.
     $       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
*
      ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
*
*        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
*        plus 128 of either lower or upper case 'Z'.
*
         IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
      END IF
      LSAME = INTA.EQ.INTB
*
*     RETURN
*
*     End of LSAME
*
      END
      subroutine mc13e(n,icn,licn,ip,lenr,arp,ib,num,lowl,numb,prev)
      integer stp,dummy
      integer ip(n)
c
c arp(i) is one less than the number of unsearched edges leaving
c     node i.  at the end of the algorithm it is set to a
c     permutation which puts the matrix in block lower
c     triangular form.
c ib(i) is the position in the ordering of the start of the ith
c     block.  ib(n+1-i) holds the node number of the ith node
c     on the stack.
c lowl(i) is the smallest stack position of any node to which a path
c     from node i has been found.  it is set to n+1 when node i
c     is removed from the stack.
c numb(i) is the position of node i in the stack if it is on
c     it, is the permuted order of node i for those nodes
c     whose final position has been found and is otherwise zero.
c prev(i) is the node at the end of the path when node i was
c     placed on the stack.
      integer icn(licn),lenr(n),arp(n),ib(n),lowl(n),numb(n),
     1prev(n)
c
c
c   icnt is the number of nodes whose positions in final ordering have
c     been found.
      icnt=0
c num is the number of blocks that have been found.
      num=0
      nnm1=n+n-1
c
c initialization of arrays.
      do 20 j=1,n
      numb(j)=0
      arp(j)=lenr(j)-1
   20 continue
c
c
      do 120 isn=1,n
c look for a starting node
      if (numb(isn).ne.0) go to 120
      iv=isn
c ist is the number of nodes on the stack ... it is the stack pointer.
      ist=1
c put node iv at beginning of stack.
      lowl(iv)=1
      numb(iv)=1
      ib(n)=iv
c
c the body of this loop puts a new node on the stack or backtracks.
      do 110 dummy=1,nnm1
      i1=arp(iv)
c have all edges leaving node iv been searched.
      if (i1.lt.0) go to 60
      i2=ip(iv)+lenr(iv)-1
      i1=i2-i1
c
c look at edges leaving node iv until one enters a new node or
c     all edges are exhausted.
      do 50 ii=i1,i2
      iw=icn(ii)
c has node iw been on stack already.
      if (numb(iw).eq.0) go to 100
c update value of lowl(iv) if necessary.
  50  lowl(iv)=min0(lowl(iv),lowl(iw))
c
c there are no more edges leaving node iv.
      arp(iv)=-1
c is node iv the root of a block.
   60 if (lowl(iv).lt.numb(iv)) go to 90
c
c order nodes in a block.
      num=num+1
      ist1=n+1-ist
      lcnt=icnt+1
c peel block off the top of the stack starting at the top and
c     working down to the root of the block.
      do 70 stp=ist1,n
      iw=ib(stp)
      lowl(iw)=n+1
      icnt=icnt+1
      numb(iw)=icnt
      if (iw.eq.iv) go to 80
   70 continue
   80 ist=n-stp
      ib(num)=lcnt
c are there any nodes left on the stack.
      if (ist.ne.0) go to 90
c have all the nodes been ordered.
      if (icnt.lt.n) go to 120
      go to 130
c
c backtrack to previous node on path.
   90 iw=iv
      iv=prev(iv)
c update value of lowl(iv) if necessary.
      lowl(iv)=min0(lowl(iv),lowl(iw))
      go to 110
c
c put new node on the stack.
 100  arp(iv)=i2-ii-1
      prev(iw)=iv
      iv=iw
      ist=ist+1
      lowl(iv)=ist
      numb(iv)=ist
      k=n+1-ist
      ib(k)=iv
  110 continue
c
  120 continue
c
c
c put permutation in the required form.
  130 do 140 i=1,n
      ii=numb(i)
 140  arp(ii)=i
      return
      end
      subroutine mc21b(n,icn,licn,ip,lenr,iperm,numnz,pr,arp,cv,out)
      integer ip(n)
c   pr(i) is the previous row to i in the depth first search.
c it is used as a work array in the sorting algorithm.
c   elements (iperm(i),i) i=1, ... n  are non-zero at the end of the
c algorithm unless n assignments have not been made.  in which case
c (iperm(i),i) will be zero for n-numnz entries.
c   cv(i) is the most recent row extension at which column i
c was visited.
c   arp(i) is one less than the number of non-zeros in row i
c which have not been scanned when looking for a cheap assignment.
c   out(i) is one less than the number of non-zeros in row i
c which have not been scanned during one pass through the main loop.
      integer icn(licn),lenr(n),iperm(n),pr(n),cv(n),
     1arp(n),out(n)
c
c   initialization of arrays.
      do 10 i=1,n
      arp(i)=lenr(i)-1
      cv(i)=0
   10 iperm(i)=0
      numnz=0
c
c
c   main loop.
c   each pass round this loop either results in a new assignment
c or gives a row with no assignment.
      do 130 jord=1,n
      j=jord
      pr(j)=-1
      do 100 k=1,jord
c look for a cheap assignment
      in1=arp(j)
      if (in1.lt.0) go to 60
      in2=ip(j)+lenr(j)-1
      in1=in2-in1
      do 50 ii=in1,in2
      i=icn(ii)
      if (iperm(i).eq.0) go to 110
   50 continue
c   no cheap assignment in row.
      arp(j)=-1
c   begin looking for assignment chain starting with row j.
   60 out(j)=lenr(j)-1
c inner loop.  extends chain by one or backtracks.
      do 90 kk=1,jord
      in1=out(j)
      if (in1.lt.0) go to 80
      in2=ip(j)+lenr(j)-1
      in1=in2-in1
c forward scan.
      do 70 ii=in1,in2
      i=icn(ii)
      if (cv(i).eq.jord) go to 70
c   column i has not yet been accessed during this pass.
      j1=j
      j=iperm(i)
      cv(i)=jord
      pr(j)=j1
      out(j1)=in2-ii-1
      go to 100
   70 continue
c
c   backtracking step.
   80 j=pr(j)
      if (j.eq.-1) go to 130
   90 continue
c
  100 continue
c
c   new assignment is made.
  110 iperm(i)=j
      arp(j)=in2-ii-1
      numnz=numnz+1
      do 120 k=1,jord
      j=pr(j)
      if (j.eq.-1) go to 130
      ii=ip(j)+lenr(j)-out(j)-2
      i=icn(ii)
      iperm(i)=j
  120 continue
c
  130 continue
c
c   if matrix is structurally singular, we now complete the
c permutation iperm.
      if (numnz.eq.n) return
      do 140 i=1,n
  140 arp(i)=0
      k=0
      do 160 i=1,n
      if (iperm(i).ne.0) go to 150
      k=k+1
      out(k)=i
      go to 160
  150 j=iperm(i)
      arp(j)=i
  160 continue
      k=0
      do 170 i=1,n
      if (arp(i).ne.0) go to 170
      k=k+1
      ioutk=out(k)
      iperm(ioutk)=i
  170 continue
      return
      end
      SUBROUTINE XERBLA( SRNAME, INFO )
*
*  -- LAPACK auxiliary routine (preliminary version) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            INFO
*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Arguments
*  =========
*
*  SRNAME  (input) CHARACTER*6
*          The name of the routine which called XERBLA.
*
*  INFO    (input) INTEGER
*          The position of the invalid parameter in the parameter list
*          of the calling routine.
*
*
      WRITE( *, FMT = 9999 )SRNAME, INFO
*
      STOP
*
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
*
*     End of XERBLA
*
      END

