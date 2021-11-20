        program main

c*********************************************************************72
c
cc MAIN is the main program for QUAD2_MPI.
c
c  Discussion:
c
c    Process 0 reads the name of a quadrature rule file, 
c    reads the R, W and X files, and sends a subset of the W and X data
c    to each process (including itselfc).
c
c    Process I evaluates F(X) at the assigned points and returns the
c    weighted sum to Process 0.
c
c    Process 0 adds up the contributions and reports the integral estimate.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none
c
c  For computation with a true MPI library:
c
      include 'mpif.h'
c
c  For computation with a simulated MPI library for one processor:
c
c     include 'mpi_stubs_f77.h'

      integer dim_max
      parameter ( dim_max = 2 )
      integer point_max
      parameter ( point_max = 60000 )

      integer arg_num
      integer dim
      integer dim_num
      integer dim_num2
      integer error_flag
      double precision f_proc(point_max)
      integer i
      integer i4_div_rounded
      integer iarg
      integer iargc
      integer ierror
      double precision integral
      integer ios
      logical more
      integer point
      integer point_num
      integer point_num_proc
      integer point_num2
      integer problem
      integer process
      integer process_id
      integer process_master
      parameter ( process_master = 0 )
      integer process_number
      integer process_remain
      double precision quad
      double precision quad_err
      character * ( 80 ) quad_filename
      double precision quad_proc
      character * ( 80 ) quad_r_filename
      character * ( 80 ) quad_w_filename
      character * ( 80 ) quad_x_filename
      double precision r(dim_max,2)
      integer source
      integer status(MPI_Status_size)
      character * ( 80 ) string
      integer tag
      integer target
      integer task
      integer task_hi
      integer task_lo
      integer task_proc
      integer task_remain
      double precision volume1
      double precision w(point_max)
      double precision w_proc(point_max)
      double precision wtime_diff
      double precision wtime_start
      double precision wtime_stop
      double precision x(dim_max,point_max)
      double precision x_proc(dim_max,point_max)
c
c  INITIALIZE PARALLEL PROCESSING.
c
      call MPI_Init ( error_flag )

      call MPI_Comm_size ( MPI_COMM_WORLD, process_number, error_flag )

      call MPI_Comm_rank ( MPI_COMM_WORLD, process_id, error_flag )
c
c  Process 0 reads in the quadrature rule, and parcels out the
c  evaluation points among the processes.
c
      if ( process_id .eq. process_master ) then

        wtime_start = MPI_Wtime ( )

        call timestamp ( )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'QUAD2_MPI'
        write ( *, '(a)' ) '  FORTRAN77/MPI version'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Estimate the integral of a function F(X)'
        write ( *, '(a)' ) '  defined over a multidimensional domain'
        write ( *, '(a)' ) 
     &    '  using a quadrature rule stored in 3 files.'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Use MPI to divide the computation among'
        write ( *, '(a)' ) '  multiple processes.'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  The parallel processing is carried out as follows,'
        write ( *, '(a)' ) 
     &    '  with process 0 playing the role of "master":'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Process 0:'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  * Reads 3 files defining a quadrature rule.'
        write ( *, '(a)' ) 
     &    '  * Divides the quadrature rule into PROC_NUM'
        write ( *, '(a)' ) 
     &  '    portions, sending one portion to each processor'
        write ( *, '(a)' ) '    (including itself).'
        write ( *, '(a)' ) 
     &    '  * Carries out its portion of the computation.'
        write ( *, '(a)' ) 
     &    '  * Collects and sums the contributions from'
        write ( *, '(a)' ) '    other processes.'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Process I:'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  * Receives one portion of the quadrature rule;'
        write ( *, '(a)' ) 
     &    '  * Carries out its portion of the computation.'
        write ( *, '(a)' ) '  * Sends its contribution to process 0.'
c
c  Get the number of command line arguments.
c
        arg_num = iargc ( )
c
c  Get the quadrature file root name:
c
        if ( 1 .le. arg_num ) then

          iarg = 1
          call getarg ( iarg, quad_filename )

        else

          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'QUAD2_MPI:'
          write ( *, '(a)' ) 
     &      '  Enter the "root" name of the quadrature files.'

          read ( *, '(a)' ) quad_filename

        end if
c
c  Create the names of:
c    the quadrature X file;
c    the quadrature W file;
c    the quadrature R file.
c
        quad_x_filename = trim ( quad_filename ) // '_x.txt'
        quad_w_filename = trim ( quad_filename ) // '_w.txt'
        quad_r_filename = trim ( quad_filename ) // '_r.txt'
c
c  Summarize the input.
c
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'QUAD2_MPI: User input:'
        write ( *, '(a,a)' ) '  Quadrature X file = ', quad_x_filename
        write ( *, '(a,a)' ) '  Quadrature W file = ', quad_w_filename
        write ( *, '(a,a)' ) '  Quadrature R file = ', quad_r_filename
c
c  Read the X file.
c
        call dtable_header_read ( quad_x_filename, dim_num, point_num )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Spatial dimension = ', dim_num
        write ( *, '(a,i8)' ) '  Number of points  = ', point_num
        write ( *, '(a)' ) ' '

        call dtable_data_read ( quad_x_filename, dim_num, point_num, 
     &    x )
c
c  Read the W file.
c
        call dtable_header_read ( quad_w_filename, dim_num2, 
     &    point_num2 )

        if ( dim_num2 .ne. 1 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'QUAD2_MPI - Fatal errorc'
          write ( *, '(a)' ) '  The quadrature weight file should have'
          write ( *, '(a)' ) '  exactly one value on each line.'
          stop
        end if

        if ( point_num2 .ne. point_num ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'QUAD2_MPI - Fatal errorc'
          write ( *, '(a)' ) '  The quadrature weight file should have'
          write ( *, '(a)' ) 
     &      '  the same number of lines as the abscissa file.'
          stop
        end if

        call dtable_data_read ( quad_w_filename, 1, point_num, w )
c
c  Read the R file.
c
        call dtable_header_read ( quad_r_filename, dim_num2, 
     &    point_num2 )

        if ( dim_num2 .ne. dim_num ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'QUAD2_MPI - Fatal errorc'
          write ( *, '(a)' ) '  The quadrature region file should have'
          write ( *, '(a)' ) '  the same number of values on each line'
          write ( *, '(a)' ) '  as the abscissa file.'
          stop
        end if

        if ( point_num2 .ne. 2 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'QUAD2_MPI - Fatal errorc'
          write ( *, '(a)' ) 
     &      '  The quadrature region file should have two lines.'
          stop
        end if

        call dtable_data_read ( quad_r_filename, dim_num, 2, r )
c
c  Rescale the weights, and translate the abscissas.
c
        volume1 = 1.0
        do dim = 1, dim_num
          volume1 = volume1 * abs ( r(dim,2) - r(dim,1) )
        end do

        problem = 28
c
c  Set problem data to default values.
c
        call p28_exact ( dim_num, integral )
c
c  Map the abscissas to the [0,1] hypercube.
c
        do dim = 1, dim_num
          do point = 1, point_num
            x(dim,point) = ( x(dim,point) - r(dim,1) ) 
     &                   / ( r(dim,2)     - r(dim,1) )
          end do
        end do

      end if
c
c  We have POINT_NUM tasks to carry out, and PROCESS_NUMBER processors.
c
c  We assign tasks TASK_LO to TASK_HI to processor PROCESS.
c
c  The master process BROADCASTS to all processes:
c
c    DIM_NUM
c
c  The master process SENDS to PROCESS:
c
c    TASK_PROC = TASK_HI + 1 - TASK_LO;
c    W(TASK_LO:TASK_HI)
c    X(1:DIM_NUM,TASK_LO:TASK:HI)
c
c  Process PROCESS RECEIVES the data, evaluates
c
c    QUAD_PROC = sum ( TASK_LO <= TASK <= TASK_HI ) W(TASK) * F(X(:,TASK) )
c
c  and SENDS QUAD_PROC back to the master process.
c
c  While PROCESS 0 also does its share of the work, we don't allow it
c  to SEND or RECEIVE messages to itself.
c
c  PROCESS 0 sums up the values of QUAD_PROC to produce QUAD, 
c  the estimate for the integral.
c
      source = process_master
      call MPI_Bcast ( dim_num, 1, MPI_INTEGER, source, MPI_COMM_WORLD, 
     &  ierror )

      if ( process_id .eq. process_master ) then

        task_hi = 0

        task_remain = point_num
        process_remain = process_number

        do process = 0, process_number - 1

          task_proc = i4_div_rounded ( task_remain, process_remain )

          process_remain = process_remain - 1
          task_remain = task_remain - task_proc

          task_lo = task_hi + 1
          task_hi = task_hi + task_proc

          if ( process .eq. process_master ) then

            point_num_proc = task_proc
            
            do task = task_lo, task_hi
              w_proc(task+1-task_lo) = w(task)
            end do

            do dim = 1, dim_num
              do task = task_lo, task_hi
                x_proc(dim,task+1-task_lo) = x(dim,task)
              end do
            end do

          else

            target = process
            tag = 1
            call MPI_Send ( task_proc, 1, 
     &        MPI_INTEGER, target, tag, MPI_COMM_WORLD, ierror )

            target = process
            tag = 2
            call MPI_Send ( w(task_lo), task_proc, 
     &        MPI_DOUBLE_PRECISION, target, tag, MPI_COMM_WORLD, 
     &        ierror )

            target = process
            tag = 3
            call MPI_Send ( x(1,task_lo), dim_num * task_proc, 
     &        MPI_DOUBLE_PRECISION, target, tag, MPI_COMM_WORLD, 
     &        ierror )

          end if

        end do

        quad = 0.0D+00

      else

        source = process_master
        tag = 1

        call MPI_Recv ( point_num_proc, 1, 
     &    MPI_INTEGER, source, tag, MPI_COMM_WORLD, status, ierror )

        source = process_master
        tag = 2

        call MPI_Recv ( w_proc, point_num_proc, 
     &    MPI_DOUBLE_PRECISION, source, tag, MPI_COMM_WORLD, status, 
     &    ierror )

        source = process_master
        tag = 3

        call MPI_Recv ( x_proc, dim_num * point_num_proc, 
     &    MPI_DOUBLE_PRECISION, source, tag, MPI_COMM_WORLD, status, 
     &   ierror )

      end if
c
c  Each process evaluates the function at the abscissas.
c
      call p28_f ( dim_num, point_num_proc, x_proc, f_proc )
c
c  Each process weights their points.
c
      quad_proc = 0.0D+00
      do point = 1, point_num_proc
        quad_proc = quad_proc + w_proc(point) * f_proc(point)
      end do

      write ( *, '(a,i8,a,g14.6)' ) 
     &  '  Process ', process_id, ' contributes QUAD_PROC = ', 
     &  quad_proc
c
c  Each process sends its value of QUAD_PROC to the master process, to
c  be summed in QUAD.
c
      call MPI_Reduce ( quad_proc, quad, 1, MPI_DOUBLE_PRECISION, 
     &  MPI_SUM, process_master, MPI_COMM_WORLD, ierror )
c
c  Compute the weighted estimate.
c
      if ( process_id .eq. process_master ) then

        quad = quad / volume1

        quad_err = abs ( quad - integral )

        write ( *, '(a)' ) ' '
        write ( *, '(a,a)' ) 
     &    '  Prob   Dim      Points       Approx',
     &    '          Exact           Error'
        write ( *, '(a)' ) ' '
        write ( *, '(2x,i4,2x,i4,2x,i10,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    problem, dim_num, point_num, quad, integral, quad_err

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'QUAD2_MPI:'
        write ( *, '(a)' ) '  Normal end of execution.'
        write ( *, '(a)' ) ' '
        call timestamp ( )

        wtime_stop = MPI_Wtime ( )

        wtime_diff = wtime_stop - wtime_start

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'QUAD2_MPI:'
        write ( *, '(a,g14.6)' ) 
     &    '  MPI Wallclock elapsed seconds = ', wtime_diff

      end if
c
c  TERMINATE PARALLEL PROCESSING.
c
      call MPI_Finalize ( error_flag )

      stop
      end
      subroutine ch_cap ( ch )

c*********************************************************************72
c
cc CH_CAP capitalizes a single character.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, character CH, the character to capitalize.
c
      implicit none

      character ch
      integer itemp

      itemp = ichar ( ch )

      if ( 97 .le. itemp .and. itemp .le. 122 ) then
        ch = char ( itemp - 32 )
      end if

      return
      end
      function ch_eqi ( c1, c2 )

c*********************************************************************72
c
cc CH_EQI is a case insensitive comparison of two characters for equality.
c
c  Example:
c
c    CH_EQI ( 'A', 'a' ) is TRUE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character C1, C2, the characters to compare.
c
c    Output, logical CH_EQI, the result of the comparison.
c
      implicit none

      character c1
      character c1_cap
      character c2
      character c2_cap
      logical ch_eqi

      c1_cap = c1
      c2_cap = c2

      call ch_cap ( c1_cap )
      call ch_cap ( c2_cap )

      if ( c1_cap == c2_cap ) then
        ch_eqi = .true.
      else
        ch_eqi = .false.
      end if

      return
      end
      subroutine ch_to_digit ( c, digit )

c*********************************************************************72
c
cc CH_TO_DIGIT returns the integer value of a base 10 digit.
c
c  Example:
c
c     C   DIGIT
c    ---  -----
c    '0'    0
c    '1'    1
c    ...  ...
c    '9'    9
c    ' '    0
c    'X'   -1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 1999
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character C, the decimal digit, '0' through '9' or blank
c    are legal.
c
c    Output, integer DIGIT, the corresponding integer value.  If C was
c    'illegal', then DIGIT is -1.
c
      implicit none

      character c
      integer digit

      if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then

        digit = ichar ( c ) - 48

      else if ( c .eq. ' ' ) then

        digit = 0

      else

        digit = -1

      end if

      return
      end
      subroutine dtable_data_read ( input_file_name, m, n, table )

c*********************************************************************72
c
cc DTABLE_DATA_READ reads data from a DTABLE file.
c
c  Discussion:
c
c    The file may contain more than N points, but this routine will
c    return after reading N of them.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILE_NAME, the name of the input file.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Output, double precision TABLE(M,N), the table data.
c
      implicit none

      integer m
      integer  n

      integer i
      integer ierror
      character * ( * ) input_file_name
      integer input_status
      integer input_unit
      integer j
      character * ( 255 ) line
      integer s_len_trim
      double precision table(m,n)
      double precision x(m)

      ierror = 0

      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_file_name, 
     &  status = 'old' )

      j = 0

10    continue

      if ( j .lt. n ) then

        read ( input_unit, '(a)' ) line

        if ( line(1:1) == '#' .or. s_len_trim ( line ) .eq. 0 ) then
          go to 10
        end if

        call s_to_r8vec ( line, m, x, ierror )

        if ( ierror .ne. 0 ) then
          go to 10
        end if

        j = j + 1

        do i = 1, m
          table(i,j) = x(i)
        end do

        go to 10

      end if

      close ( unit = input_unit )

      return
      end
      subroutine dtable_header_read ( input_file_name, m, n )

c*********************************************************************72
c
cc DTABLE_HEADER_READ reads the header from a DTABLE file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILE_NAME, the name of the input file.
c
c    Output, integer M, spatial dimension.
c
c    Output, integer N, the number of points.
c
      implicit none

      character * ( * ) input_file_name
      integer m
      integer n

      call file_column_count ( input_file_name, m )

      if ( m .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DTABLE_HEADER_READ - Fatal errorc'
        write ( *, '(a)' ) '  There was an I/O problem while trying'
        write ( *, '(a)' ) '  to count the number of data columns in'
        write ( *, '(a,a,a)' ) '  the file "', input_file_name, '".'
        stop
      end if

      call file_row_count ( input_file_name, n )

      if ( n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DTABLE_HEADER_READ - Fatal errorc'
        write ( *, '(a)' ) '  There was an I/O problem while trying'
        write ( *, '(a)' ) '  to count the number of data rows in'
        write ( *, '(a,a,a)' ) '  the file "', input_file_name, '".'
        stop
      end if

      return
      end
      subroutine file_column_count ( input_file_name, column_num )

c*********************************************************************72
c
cc FILE_COLUMN_COUNT counts the number of columns in the first line of a file.
c
c  Discussion:
c
c    The file is assumed to be a simple text file.
c
c    Most lines of the file is presumed to consist of COLUMN_NUM words,
c    separated by spaces.  There may also be some blank lines, and some
c    comment lines,
c    which have a "#" in column 1.
c
c    The routine tries to find the first non-comment non-blank line and
c    counts the number of words in that line.
c
c    If all lines are blanks or comments, it goes back and tries to analyze
c    a comment line.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILE_NAME, the name of the file.
c
c    Output, integer COLUMN_NUM, the number of columns in the file.
c
      implicit none

      integer column_num
      logical got_one
      character * ( * ) input_file_name
      integer input_unit
      character * ( 256 ) line
      integer s_len_trim
c
c  Open the file.
c
      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_file_name, 
     &  status = 'old',  form = 'formatted', access = 'sequential' )
c
c  Read one line, but skip blank lines and comment lines.
c
      got_one = .false.

10    continue

        read ( input_unit, '(a)', err = 20 ) line

        if ( s_len_trim ( line ) .eq. 0 ) then
          go to 10
        end if

        if ( line(1:1) .eq. '#' ) then
          go to 10
        end if

        got_one = .true.
        go to 20

      go to 10

20    continue

      if ( .not. got_one ) then

        rewind ( input_unit )

30      continue

          read ( input_unit, '(a)', err = 40 ) line

          if ( s_len_trim ( line ) .eq. 0 ) then
            go to 30
          end if

          got_one = .true.
          go to 40

        go to 30

40    continue

      end if

      close ( unit = input_unit )

      if ( .not. got_one ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILE_COLUMN_COUNT - Warning.'
        write ( *, '(a)' ) '  The file does not contain any data.'
        column_num = -1
        return
      end if

      call s_word_count ( line, column_num )

      return
      end
      subroutine file_row_count ( input_file_name, row_num )

c*********************************************************************72
c
cc FILE_ROW_COUNT counts the number of row records in a file.
c
c  Discussion:
c
c    It does not count lines that are blank, or that begin with a
c    comment symbol '#'.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) INPUT_FILE_NAME, the name of the input file.
c
c    Output, integer ROW_NUM, the number of rows found.
c
      implicit none

      integer bad_num
      integer comment_num
      integer ierror
      character * ( * ) input_file_name
      integer input_status
      integer input_unit
      character * ( 100 ) line
      integer record_num
      integer row_num
      integer s_len_trim

      call get_unit ( input_unit )

      open ( unit = input_unit, file = input_file_name, 
     &  status = 'old' )

      comment_num = 0
      row_num = 0
      record_num = 0
      bad_num = 0

10    continue

        read ( input_unit, '(a)', err = 20, end = 20 ) line

        record_num = record_num + 1

        if ( line(1:1) .eq. '#' ) then
          comment_num = comment_num + 1
          go to 10
        end if

        if ( s_len_trim ( line ) .eq. 0 ) then
          comment_num = comment_num + 1
          go to 10
        end if

        row_num = row_num + 1

      go to 10

20    continue

      close ( unit = input_unit )

      return
      end
      subroutine get_unit ( unit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is an integer between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If UNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, UNIT is an integer between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer UNIT, the free unit number.
c
      implicit none

      integer i
      integer unit

      unit = 0

      do i = 1, 99

        if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

          open ( unit = i, err = 10 )
          close ( unit = i )

          unit = i

          return
        end if

10      continue

      end do

      return
      end
      function i4_div_rounded ( a, b )

c*********************************************************************72
c
cc I4_DIV_ROUNDED computes the rounded result of I4 division.
c
c  Discussion:
c
c    This routine computes C = A / B, where A, B and C are integers
c    and C is the closest integer value to the exact real result.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 October 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer A, B, the number to be divided,
c    and the divisor.
c
c    Output, integer I4_DIV_ROUNDED, the rounded result
c    of the division.
c
      implicit none

      integer a
      integer a_abs
      integer b
      integer b_abs
      integer c
      integer c_abs
      integer c_s
      integer i4_div_rounded
      integer i4_huge
      integer i4_sign

      if ( a .eq. 0 ) then

        c_abs = i4_huge ( )
        c_s = i4_sign ( b )

      else

        a_abs = abs ( a )
        b_abs = abs ( b )
        c_s = i4_sign ( a ) * i4_sign ( b )

        c_abs = a_abs / b_abs

        if ( ( 2 * c_abs + 1 ) * b_abs .lt. 2 * a_abs ) then
          c_abs = c_abs + 1
        end if

      end if

      c = c_s * c_abs

      i4_div_rounded = c

      return
      end
      function i4_huge ( )

c*********************************************************************72
c
cc I4_HUGE returns a "huge" I4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer I4_HUGE, a huge number.
c
      implicit none

      integer i4_huge

      i4_huge = 2147483647

      return
      end
      function i4_sign ( x )

c*********************************************************************72
c
cc I4_SIGN evaluates the sign of an I4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X, the number whose sign is desired.
c
c    Output, integer I4_SIGN, the sign of X:
c
      implicit none

      integer i4_sign
      integer x

      if ( x .lt. 0 ) then
        i4_sign = -1
      else
        i4_sign = +1
      end if

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

        if ( s(i:i) .ne. ' ' ) then
          s_len_trim = i
          return
        end if

      end do

      s_len_trim = 0

      return
      end
      subroutine s_to_i4 ( s, ival, ierror, length )

c*********************************************************************72
c
cc S_TO_I4 reads an I4 from a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, a string to be examined.
c
c    Output, integer IVAL, the integer value read from the string.
c    If the string is blank, then IVAL will be returned 0.
c
c    Output, integer IERROR, an error flag.
c    0, no error.
c    1, an error occurred.
c
c    Output, integer LENGTH, the number of characters of S
c    used to make IVAL.
c
      implicit none

      character c
      integer i
      integer ierror
      integer isgn
      integer istate
      integer ival
      integer length
      character * ( * ) s
      integer s_len_trim

      ierror = 0
      istate = 0
      isgn = 1
      ival = 0

      do i = 1, s_len_trim ( s )

        c = s(i:i)
c
c  Haven't read anything.
c
        if ( istate .eq. 0 ) then

          if ( c .eq. ' ' ) then

          else if ( c .eq. '-' ) then
            istate = 1
            isgn = -1
          else if ( c .eq. '+' ) then
            istate = 1
            isgn = + 1
          else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            istate = 2
            ival = ichar ( c ) - ichar ( '0' )
          else
            ierror = 1
            return
          end if
c
c  Have read the sign, expecting digits.
c
        else if ( istate .eq. 1 ) then

          if ( c .eq. ' ' ) then

          else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            istate = 2
            ival = ichar ( c ) - ichar ( '0' )
          else
            ierror = 1
            return
          end if
c
c  Have read at least one digit, expecting more.
c
        else if ( istate .eq. 2 ) then

          if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
            ival = 10 * ival + ichar ( c ) - ichar ( '0' )
          else
            ival = isgn * ival
            length = i - 1
            return
          end if

        end if

      end do
c
c  If we read all the characters in the string, see if we're OK.
c
      if ( istate .eq. 2 ) then
        ival = isgn * ival
        length = s_len_trim ( s )
      else
        ierror = 1
        length = 0
      end if

      return
      end
      subroutine s_to_r8 ( s, dval, ierror, length )

c*********************************************************************72
c
cc S_TO_R8 reads an R8 from a string.
c
c  Discussion:
c
c    The routine will read as many characters as possible until it reaches
c    the end of the string, or encounters a character which cannot be
c    part of the number.
c
c    Legal input is:
c
c       1 blanks,
c       2 '+' or '-' sign,
c       2.5 blanks
c       3 integer part,
c       4 decimal point,
c       5 fraction part,
c       6 'E' or 'e' or 'D' or 'd', exponent marker,
c       7 exponent sign,
c       8 exponent integer part,
c       9 exponent decimal point,
c      10 exponent fraction part,
c      11 blanks,
c      12 final comma or semicolon,
c
c    with most quantities optional.
c
c  Example:
c
c    S                 DVAL
c
c    '1'               1.0
c    '     1   '       1.0
c    '1A'              1.0
c    '12,34,56'        12.0
c    '  34 7'          34.0
c    '-1E2ABCD'        -100.0
c    '-1X2ABCD'        -1.0
c    ' 2E-1'           0.2
c    '23.45'           23.45
c    '-4.2E+2'         -420.0
c    '17d2'            1700.0
c    '-14e-2'         -0.14
c    'e2'              100.0
c    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string containing the
c    data to be read.  Reading will begin at position 1 and
c    terminate at the end of the string, or when no more
c    characters can be read to form a legal real.  Blanks,
c    commas, or other nonnumeric data will, in particular,
c    cause the conversion to halt.
c
c    Output, double precision DVAL, the value read from the string.
c
c    Output, integer IERROR, error flag.
c    0, no errors occurred.
c    1, 2, 6 or 7, the input number was garbled.  The
c    value of IERROR is the last type of input successfully
c    read.  For instance, 1 means initial blanks, 2 means
c    a plus or minus sign, and so on.
c
c    Output, integer LENGTH, the number of characters read
c    to form the number, including any terminating
c    characters such as a trailing comma or blanks.
c
      implicit none

      logical ch_eqi
      character c
      double precision dval
      integer ierror
      integer ihave
      integer isgn
      integer iterm
      integer jbot
      integer jsgn
      integer jtop
      integer length
      integer nchar
      integer ndig
      double precision rbot
      double precision rexp
      double precision rtop
      character * ( * ) s
      integer s_len_trim

      nchar = s_len_trim ( s )

      ierror = 0
      dval = 0.0D+00
      length = -1
      isgn = 1
      rtop = 0
      rbot = 1
      jsgn = 1
      jtop = 0
      jbot = 1
      ihave = 1
      iterm = 0

10    continue

        length = length + 1

        if ( nchar .lt. length+1 ) then
          go to 20
        end if

        c = s(length+1:length+1)
c
c  Blank character.
c
        if ( c .eq. ' ' ) then

          if ( ihave .eq. 2 ) then

          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            iterm = 1
          else if ( 1 .lt. ihave ) then
            ihave = 11
          end if
c
c  Comma.
c
        else if ( c .eq. ',' .or. c .eq. ';' ) then

          if ( ihave .ne. 1 ) then
            iterm = 1
            ihave = 12
            length = length + 1
          end if
c
c  Minus sign.
c
        else if ( c .eq. '-' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
            isgn = -1
          else if ( ihave .eq. 6 ) then
            ihave = 7
            jsgn = -1
          else
            iterm = 1
          end if
c
c  Plus sign.
c
        else if ( c .eq. '+' ) then

          if ( ihave .eq. 1 ) then
            ihave = 2
          else if ( ihave .eq. 6 ) then
            ihave = 7
          else
            iterm = 1
          end if
c
c  Decimal point.
c
        else if ( c .eq. '.' ) then

          if ( ihave .lt. 4 ) then
            ihave = 4
          else if ( 6 .le. ihave .and. ihave .le. 8 ) then
            ihave = 9
          else
            iterm = 1
          end if
c
c  Scientific notation exponent marker.
c
        else if ( ch_eqi ( c, 'E' ) .or. ch_eqi ( c, 'D' ) ) then

          if ( ihave .lt. 6 ) then
            ihave = 6
          else
            iterm = 1
          end if
c
c  Digit.
c
        else if ( ihave .lt. 11 .and. lle ( '0', c ) 
     &    .and. lle ( c, '9' ) ) then

          if ( ihave .le. 2 ) then
            ihave = 3
          else if ( ihave .eq. 4 ) then
            ihave = 5
          else if ( ihave .eq. 6 .or. ihave .eq. 7 ) then
            ihave = 8
          else if ( ihave .eq. 9 ) then
            ihave = 10
          end if

          call ch_to_digit ( c, ndig )

          if ( ihave .eq. 3 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
          else if ( ihave .eq. 5 ) then
            rtop = 10.0D+00 * rtop + dble ( ndig )
            rbot = 10.0D+00 * rbot
          else if ( ihave .eq. 8 ) then
            jtop = 10 * jtop + ndig
          else if ( ihave .eq. 10 ) then
            jtop = 10 * jtop + ndig
            jbot = 10 * jbot
          end if
c
c  Anything else is regarded as a terminator.
c
        else
          iterm = 1
        end if
c
c  If we haven't seen a terminator, and we haven't examined the
c  entire string, go get the next character.
c
        if ( iterm .eq. 1 ) then
          go to 20
        end if

        go to 10

20    continue
c
c  If we haven't seen a terminator, and we have examined the
c  entire string, then we're done, and LENGTH is equal to NCHAR.
c
      if ( iterm .ne. 1 .and. length+1 .eq. nchar ) then
        length = nchar
      end if
c
c  Number seems to have terminated.  Have we got a legal number?
c  Not if we terminated in states 1, 2, 6 or 7.
c
      if ( ihave .eq. 1 .or. ihave .eq. 2 .or. 
     &     ihave .eq. 6 .or. ihave .eq. 7 ) then
        ierror = ihave
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_TO_R8 - Serious error!'
        write ( *, '(a)' ) '  Illegal or nonnumeric input:'
        write ( *, '(a,a)' ) '    ', s
        return
      end if
c
c  Number seems OK.  Form it.
c
      if ( jtop .eq. 0 ) then
        rexp = 1.0D+00
      else
        if ( jbot .eq. 1 ) then
          rexp = 10.0D+00 ** ( jsgn * jtop )
        else
          rexp = 10.0D+00 ** ( dble ( jsgn * jtop ) / dble ( jbot ) )
        end if
      end if

      dval = dble ( isgn ) * rexp * rtop / rbot

      return
      end
      subroutine s_to_r8vec ( s, n, rvec, ierror )

c*********************************************************************72
c
cc S_TO_R8VEC reads an R8VEC from a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string to be read.
c
c    Input, integer N, the number of values expected.
c
c    Output, double precision RVEC(N), the values read from the string.
c
c    Output, integer IERROR, error flag.
c    0, no errors occurred.
c    -K, could not read data for entries -K through N.
c
      implicit none

      integer  n

      integer i
      integer ierror
      integer ilo
      integer lchar
      double precision rvec(n)
      character * ( * ) s

      i = 0
      ierror = 0
      ilo = 1

10    continue

      if ( i .lt. n ) then

        i = i + 1

        call s_to_r8 ( s(ilo:), rvec(i), ierror, lchar )

        if ( ierror .ne. 0 ) then
          ierror = -i
          go to 20
        end if

        ilo = ilo + lchar

        go to 10

      end if

20    continue

      return
      end
      subroutine s_word_count ( s, nword )

c*********************************************************************72
c
cc S_WORD_COUNT counts the number of "words" in a string.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) S, the string to be examined.
c
c    Output, integer NWORD, the number of "words" in the string.
c    Words are presumed to be separated by one or more blanks.
c
      implicit none

      logical blank
      integer i
      integer lens
      integer nword
      character * ( * ) s

      nword = 0
      lens = len ( s )

      if ( lens .le. 0 ) then
        return
      end if

      blank = .true.

      do i = 1, lens

        if ( s(i:i) .eq. ' ' ) then
          blank = .true.
        else if ( blank ) then
          nword = nword + 1
          blank = .false.
        end if

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
      subroutine p28_exact ( dim_num, exact )

c*********************************************************************72
c
cc P28_EXACT returns the exact integral for problem 28.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Output, double precision EXACT, the exact value of the integral.
c
      implicit none

      integer dim_num

      double precision exact

      exact = ( 2.0D+00 * atan ( 0.5D+00 ) )**dim_num

      return
      end
      subroutine p28_f ( dim_num, point_num, x, value )

c*********************************************************************72
c
cc P28_F evaluates the integrand for problem 28.
c
c  Dimension:
c
c    N arbitrary.
c
c  Region:
c
c    0 <= X(1:DIM_NUM) <= 1
c
c  Integrand:
c
c    1 / product ( C(1:DIM_NUM)**2 + ( X(1:DIM_NUM) - Z(1:DIM_NUM) )**2 )
c
c  Exact Integral:
c
c    product ( (   arctan ( ( 1 - Z(1:DIM_NUM) ) / C(1:DIM_NUM) )
c                + arctan (       Z(1:DIM_NUM)   / C(1:DIM_NUM) ) 
c              ) / C(1:DIM_NUM)
c            )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Alan Genz,
c    [Integral #2]
c    A Package for Testing Multiple Integration Subroutines,
c    in Numerical Integration: Recent Developments, Software
c    and Applications,
c    edited by Patrick Keast and Graeme Fairweather,
c    D Reidel, 1987, pages 337-340,
c    LC: QA299.3.N38.
c
c    Thomas Patterson,
c    [Integral #6],
c    On the Construction of a Practical Ermakov-Zolotukhin 
c    Multiple Integrator,
c    in Numerical Integration: Recent Developments, Software
c    and Applications,
c    edited by Patrick Keast and Graeme Fairweather,
c    D. Reidel, 1987, pages 269-290,
c    LC: QA299.3.N38.
c
c  Parameters:
c
c    Input, integer DIM_NUM, the dimension of the argument.
c
c    Input, integer POINT_NUM, the number of points.
c
c    Input, double precision X(DIM_NUM,POINT_NUM), the evaluation points.
c
c    Output, double precision VALUE(POINT_NUM), the function values.
c
      implicit none

      integer dim_num
      integer point_num

      integer dim
      integer point
      double precision value(point_num)
      double precision x(dim_num,point_num)

      do point = 1, point_num
        value(point) = 1.0D+00 
        do dim = 1, dim_num
          value(point) = value(point) 
     &      / ( 1.0 + ( x(dim,point) - 0.5D+00 )**2 )
        end do
      end do

      return
      end
