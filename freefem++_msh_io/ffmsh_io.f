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

      if ( c1_cap .eq. c2_cap ) then
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
      subroutine ffmsh_2d_data_example ( v_num, e_num, t_num, v_xy, v_l,
     &  e_v, e_l, t_v, t_l )

c*********************************************************************72
c
cc FFMSH_2D_DATA_EXAMPLE returns example FFMSH data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer V_NUM, the number of vertices.
c
c    Input, integer E_NUM, the number of boundary edges.
c
c    Input,  T_NUM, the number of triangles.
c
c    Output, double precision V_XY(2,V_NUM), vertex coordinates.
c
c    Output, integer V_L(V_NUM), vertex labels.
c
c    Output, integer E_V(2,E_NUM), edge vertices.
c
c    Output, integer E_L(E_NUM), vertex labels.
c
c    Output, integer T_V(3,T_NUM), triangle vertices.
c
c    Output, integer T_L(T_NUM), triangle labels.
c
      implicit none

      integer e_num
      integer t_num
      integer v_num

      integer e_l(e_num)
      integer e_l_save(10)
      integer e_v(2,e_num)
      integer e_v_save(2,10)
      integer t_l(t_num)
      integer t_l_save(18)
      integer t_v(3,t_num)
      integer t_v_save(3,18)
      integer v_l(v_num)
      integer v_l_save(15)
      double precision v_xy(2,v_num)
      double precision v_xy_save(2,15)

      save e_l_save
      save e_v_save
      save t_l_save
      save t_v_save
      save v_l_save
      save v_xy_save

      data e_l_save /
     &  1, 1, 1, 1, 1, 
     &  1, 1, 1, 1, 1 /
      data e_v_save /
     & 11,  6, 
     &  6,  4, 
     &  4,  1, 
     &  1,  2, 
     &  2,  5, 
     &  5,  9, 
     &  9, 13, 
     & 13, 15, 
     & 15, 14, 
     & 14, 11 /
      data t_l_save /
     &  0, 0, 0, 0, 0, 
     &  0, 0, 0, 0, 0, 
     &  0, 0, 0, 0, 0, 
     &  0, 0, 0 /
      data t_v_save /
     &  1,  3,  4, 
     &  7,  2,  5, 
     &  9,  7,  5, 
     &  8,  6,  4, 
     & 12,  8,  7, 
     & 12, 11,  8, 
     &  3,  1,  2, 
     &  7,  3,  2, 
     &  7,  8,  3, 
     &  4,  3,  8, 
     &  6,  8, 11, 
     & 12,  7, 10, 
     & 11, 12, 14, 
     & 10,  9, 13, 
     & 12, 10, 13, 
     &  7,  9, 10, 
     & 12, 13, 15, 
     & 14, 12, 15 /
      data v_l_save /
     &  1, 1, 0, 1, 1, 
     &  1, 0, 0, 1, 0, 
     &  1, 0, 1, 1, 1 /
      data v_xy_save /
     & -0.309016994375D+00,  0.951056516295D+00, 
     & -0.809016994375D+00,  0.587785252292D+00, 
     & -0.321175165867D+00,  0.475528256720D+00, 
     &  0.309016994375D+00,  0.951056516295D+00, 
     & -1.000000000000D+00,  0.000000000000D+00, 
     &  0.809016994375D+00,  0.587785252292D+00, 
     & -0.333333334358D+00,  0.000000000000D+00, 
     &  0.237841829972D+00,  0.293892623813D+00, 
     & -0.809016994375D+00, -0.587785252292D+00, 
     & -0.321175165867D+00, -0.475528259963D+00, 
     &  1.000000000000D+00,  0.000000000000D+00, 
     &  0.206011327827D+00, -0.391856835534D+00, 
     & -0.309016994375D+00, -0.951056516295D+00, 
     &  0.809016994375D+00, -0.587785252292D+00, 
     &  0.309016994375D+00, -0.951056516295D+00 /

      call i4vec_copy (    v_num, v_l_save,  v_l )
      call r8mat_copy ( 2, v_num, v_xy_save, v_xy )
      call i4vec_copy (    e_num, e_l_save,  e_l )
      call i4mat_copy ( 2, e_num, e_v_save,  e_v )
      call i4vec_copy (    t_num, t_l_save,  t_l )
      call i4mat_copy ( 3, t_num, t_v_save,  t_v )

      return
      end
      subroutine ffmsh_2d_data_print ( title, v_num, e_num, t_num, v_xy,
     &  v_l, e_v, e_l, t_v, t_l )

c*********************************************************************72
c
cc FFMSH_2D_DATA_PRINT prints FFMSH data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) TITLE, a title.
c
c    Input, integer V_NUM, the number of vertices.
c
c    Input, integer E_NUM, the number of boundary edges.
c
c    Input, integer T_NUM, the number of triangles.
c
c    Input, double precision V_XY(2,V_NUM), vertex coordinates.
c
c    Input, integer V_L(V_NUM), vertex labels.
c
c    Input, integer E_V(2,E_NUM), edge vertices.
c
c    Input, integer E_L(E_NUM), vertex labels.
c
c    Input, integer T_V(3,T_NUM), triangle vertices.
c
c    Input, integer T_L(T_NUM), triangle labels.
c
      implicit none

      integer e_num
      integer t_num
      integer v_num

      integer e_l(e_num)
      integer e_v(2,e_num)
      integer t_l(t_num)
      integer t_v(3,t_num)
      character * ( * ) title
      integer v_l(v_num)
      double precision v_xy(2,v_num)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) trim ( title )

      call i4vec_print (              v_num, v_l,  '  Vertex labels:' )
      call r8mat_transpose_print ( 2, v_num, v_xy, 
     &  '  Vertex coordinates:' )
      call i4vec_print (              e_num, e_l,  '  Edge labels:' )
      call i4mat_transpose_print ( 2, e_num, e_v,  '  Edge vertices:' )
      call i4vec_print (              t_num, t_l,  
     &  '  Triangle labels:' )
      call i4mat_transpose_print ( 3, t_num, t_v,  
     &  '  Triangle vertices:' )

      return
      end
      subroutine ffmsh_2d_data_read ( ffmsh_filename, v_num, e_num, 
     &  t_num, v_xy, v_l, e_v, e_l, t_v, t_l )

c*********************************************************************72
c
cc FFMSH_2D_DATA_READ reads data from an FFMSH file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 December 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FFMSH_FILENAME, the FFMSH filename.
c
c    Input, integer V_NUM, the number of vertices.
c
c    Input, integer E_NUM, the number of boundary edges.
c
c    Input, integer T_NUM, the number of triangles.
c
c    Output, double precision V_XY(2,V_NUM), vertex coordinates.
c
c    Output, integer V_L(V_NUM), vertex labels.
c
c    Output, integer E_V(2,E_NUM), edge vertices.
c
c    Output, integer E_L(E_NUM), vertex labels.
c
c    Output, integer T_V(3,T_NUM), triangle vertices.
c
c    Output, integer T_L(T_NUM), triangle labels.
c
      implicit none

      integer e_num
      integer t_num
      integer v_num

      integer e_l(e_num)
      integer e_num2
      integer e_v(2,e_num)
      character * ( * ) ffmsh_filename
      integer ffmsh_stat
      integer ffmsh_unit
      integer i1
      integer i2
      integer i3
      integer i4
      integer ierror
      integer j
      integer length
      double precision r1
      double precision r2
      integer t_l(t_num)
      integer t_num2
      integer t_v(3,t_num)
      character * ( 255 ) text
      integer v_num2
      integer v_l(v_num)
      double precision v_xy(2,v_num)

      call get_unit ( ffmsh_unit )

      open ( unit = ffmsh_unit, file = ffmsh_filename, status = 'old',  
     &   iostat = ffmsh_stat )

      if ( ffmsh_stat .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'FFMSH_2D_DATA_READ - Fatal errorc'
        write ( *, '(a)' ) '  Could not open input file "' 
     &    // trim ( ffmsh_filename ) // '"'
        stop 1
      end if
c
c  Read the sizes (again).
c
      read ( ffmsh_unit, '(a)', iostat = ffmsh_stat ) text

      call s_to_i4 ( text, v_num2, ierror, length )
      text = text(length+1:)
      call s_to_i4 ( text, t_num2, ierror, length )
      text = text(length+1:)
      call s_to_i4 ( text, e_num2, ierror, length )
c
c  Read Vertex X, Y, Label
c
      do j = 1, v_num
        read ( ffmsh_unit, '(a)', iostat = ffmsh_stat ) text
        call s_to_r8 ( text, r1, ierror, length )
        text = text(length+1:)
        call s_to_r8 ( text, r2, ierror, length )
        text = text(length+1:)
        call s_to_i4 ( text, i1, ierror, length )
        v_xy(1,j) = r1
        v_xy(2,j) = r2
        v_l(j) = i1
      end do
c
c  Read Triangle V1, V2, V3, Label
c
      do j = 1, t_num
        read ( ffmsh_unit, '(a)', iostat = ffmsh_stat ) text
        call s_to_i4 ( text, i1, ierror, length )
        text = text(length+1:)
        call s_to_i4 ( text, i2, ierror, length )
        text = text(length+1:)
        call s_to_i4 ( text, i3, ierror, length )
        text = text(length+1:)
        call s_to_i4 ( text, i4, ierror, length )
        t_v(1,j) = i1
        t_v(2,j) = i2
        t_v(3,j) = i3
        t_l(j) = i4
      end do
c
c  Read Edge V1, V2, Label
c
      do j = 1, e_num
        read ( ffmsh_unit, '(a)', iostat = ffmsh_stat ) text
        call s_to_i4 ( text, i1, ierror, length )
        text = text(length+1:)
        call s_to_i4 ( text, i2, ierror, length )
        text = text(length+1:)
        call s_to_i4 ( text, i3, ierror, length )
        e_v(1,j) = i1
        e_v(2,j) = i2
        e_l(j) = i3
      end do

      close ( unit = ffmsh_unit )

      return
      end
      subroutine ffmsh_2d_size_example ( v_num, e_num, t_num )

c*********************************************************************72
c
cc FFMSH_2D_SIZE_EXAMPLE returns sizes for the 2D example.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer V_NUM, the number of vertices.
c
c    Output, integer E_NUM, the number of boundary edges.
c
c    Output, integer T_NUM, the number of triangles.
c
      implicit none

      integer e_num
      integer t_num
      integer v_num

      e_num = 10
      t_num = 18
      v_num = 15

      return
      end
      subroutine ffmsh_2d_size_print ( title, v_num, e_num, t_num )

c*********************************************************************72
c
cc FFMSH_2D_SIZE_PRINT prints the sizes of an FFMSH.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) TITLE, a title.
c
c    Input, integer V_NUM, the number of vertices.
c
c    Input, integer E_NUM, the number of boundary edges.
c
c    Input, integer T_NUM, the number of triangles.
c
      implicit none

      integer e_num
      integer t_num
      character * ( * ) title
      integer v_num

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ''
      write ( *, '(a,i6)' ) '  Number of vertices = ', v_num
      write ( *, '(a,i6)' ) '  Number of boundary edges = ', e_num
      write ( *, '(a,i6)' ) '  Number of triangles = ', t_num

      return
      end
      subroutine ffmsh_2d_size_read ( ffmsh_filename, v_num, e_num, 
     &  t_num )

c*********************************************************************72
c
cc FFMSH_2D_SIZE_READ reads sizes from a FFMSH file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FFMSH_FILENAME, the FFMSH filename.
c
c    Output, integer V_NUM, the number of vertices.
c
c    Output, integer E_NUM, the number of boundary edges.
c
c    Output, integer T_NUM, the number of triangles.
c
      implicit none

      integer e_num
      character * ( * ) ffmsh_filename
      integer ffmsh_stat
      integer ffmsh_unit
      integer ierror
      integer length
      integer t_num
      character * ( 255 ) text
      integer v_num

      call get_unit ( ffmsh_unit )

      open ( unit = ffmsh_unit, file = ffmsh_filename, status = 'old',  
     &  iostat = ffmsh_stat )

      if ( ffmsh_stat .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'FFMSH_SIZE_READ - Fatal errorc'
        write ( *, '(a)' ) '  Could not open the input file "' 
     &    // trim ( ffmsh_filename ) // '"'
        stop 1
      end if

      read ( ffmsh_unit, '(a)', iostat = ffmsh_stat ) text

      call s_to_i4 ( text, v_num, ierror, length )
      text = text(length+1:)
      call s_to_i4 ( text, t_num, ierror, length )
      text = text(length+1:)
      call s_to_i4 ( text, e_num, ierror, length )

      close ( unit = ffmsh_unit )

      return
      end
      subroutine ffmsh_2d_write ( ffmsh_filename, v_num, e_num, t_num, 
     &  v_xy, v_l, e_v, e_l, t_v, t_l )

c*********************************************************************72
c
cc FFMSH_2D_WRITE writes FFMSH data to a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) FFMSH_FILENAME, the name of the file.
c
c    Input, integer V_NUM, the number of vertices.
c
c    Input, integer E_NUM, the number of boundary edges.
c
c    Input, integer T_NUM, the number of triangles.
c
c    Input, double precision V_XY(2,V_NUM), vertex coordinates.
c
c    Input, integer V_L(V_NUM), vertex labels.
c
c    Input, integer E_V(2,E_NUM), edge vertices.
c
c    Input, integer E_L(E_NUM), vertex labels.
c
c    Input, integer T_V(3,T_NUM), triangle vertices.
c
c    Input, integer T_L(T_NUM), triangle labels.
c
      implicit none

      integer e_num
      integer t_num
      integer v_num

      integer e_l(e_num)
      integer e_v(2,e_num)
      character * ( * ) ffmsh_filename
      integer ffmsh_unit
      integer j
      integer t_l(t_num)
      integer t_v(3,t_num)
      integer v_l(v_num)
      double precision v_xy(2,v_num)
c
c  Open the file.
c
      call get_unit ( ffmsh_unit )

      open ( unit = ffmsh_unit, file = ffmsh_filename, 
     &  status = 'replace' )
c
c  Write the data.
c
      write ( ffmsh_unit, '(2x,i6,2x,i6,2x,i6)' ) v_num, t_num, e_num

      do j = 1, v_num
        write ( ffmsh_unit, '(2x,g14.6,2x,g14.6,2x,i6)' ) 
     &    v_xy(1:2,j), v_l(j)
      end do

      do j = 1, t_num
        write ( ffmsh_unit, '(2x,i6,2x,i6,2x,i6,2x,i6)' ) 
     &    t_v(1:3,j), t_l(j)
      end do

      do j = 1, e_num
        write ( ffmsh_unit, '(2x,i6,2x,i6,2x,i6)' ) e_v(1:2,j), e_l(j)
      end do

      close ( unit = ffmsh_unit )

      return
      end
      subroutine i4mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc I4MAT_COPY copies an I4MAT.
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
c    04 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, integer A1(M,N), the matrix to copy.
c
c    Output, integer A2(M,N), the copy.
c
      implicit none

      integer m
      integer n

      integer a1(m,n)
      integer a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
      end do

      return
      end
      subroutine i4mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc I4MAT_TRANSPOSE_PRINT prints an I4MAT, transposed.
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
c    39 October 2007
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
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      character * ( * ) title

      call i4mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine i4mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &  jhi, title )

c*********************************************************************72
c
cc I4MAT_TRANSPOSE_PRINT_SOME prints some of the transpose of an I4MAT.
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
c    30 October 2007
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
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 10 )
      integer m
      integer n

      integer a(m,n)
      character*8 ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer  j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' )  title

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8)' ) i
        end do

        write ( *, '(''  Row '',10a8)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Col'
        write ( *, '(a)' ) ' '

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc

            i = i2lo - 1 + i2

            write ( ctemp(i2), '(i8)' ) a(i,j)

          end do

          write ( *, '(i5,a,10a8)' ) j, ':', ( ctemp(i), i = 1, inc )

        end do

      end do

      return
      end
      subroutine i4vec_copy ( n, a1, a2 )

c*********************************************************************72
c
cc I4VEC_COPY copies an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, integer A1(N), the vector to be copied.
c
c    Output, integer A2(N), a copy of A1.
c
      implicit none

      integer n

      integer a1(n)
      integer a2(n)
      integer i

      do i = 1, n
        a2(i) = a1(i)
      end do

      return
      end
      subroutine i4vec_print ( n, a, title )

c*********************************************************************72
c
cc I4VEC_PRINT prints an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of integer values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, integer A(N), the vector to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer n

      integer a(n)
      integer i
      character*(*) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,i12)' ) i, ':', a(i)
      end do

      return
      end
      subroutine mesh_base_one ( node_num, element_order, element_num, 
     &  element_node )

c*********************************************************************72
c
cc MESH_BASE_ONE ensures that the element definition is one-based.
c
c  Discussion:
c
c    The ELEMENT_NODE array contains nodes indices that form elements.
c    The convention for node indexing might start at 0 or at 1.
c    Since a FORTRAN90 program will naturally assume a 1-based indexing, it is
c    necessary to check a given element definition and, if it is actually
c    0-based, to convert it.
c
c    This function attempts to detect 0-based node indexing and correct it.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 October 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, int NODE_NUM, the number of nodes.
c
c    Input, int ELEMENT_ORDER, the order of the elements.
c
c    Input, int ELEMENT_NUM, the number of elements.
c
c    Input/output, int ELEMENT_NODE(ELEMENT_ORDER,ELEMENT_NUM), the element
c    definitions.
c
      implicit none

      integer element_num
      integer element_order

      integer element
      integer element_node(element_order,element_num)
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer i4mat_max
      integer i4mat_min
      integer j
      integer node
      integer node_max
      integer node_min
      integer node_num
      integer order

      node_min = + i4_huge
      node_max = - i4_huge
      do j = 1, element_num
        do i = 1, element_order
          node_min = min ( node_min, element_node(i,j) )
          node_max = max ( node_max, element_node(i,j) )
        end do
      end do

      if ( node_min .eq. 0 .and. node_max .eq. node_num - 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ONE:'
        write ( *, '(a)' )
     &    '  The element indexing appears to be 0-based!'
        write ( *, '(a)' ) '  This will be converted to 1-based.'
        do j = 1, element_num
          do i = 1, element_order
            element_node(i,j) = element_node(i,j) + 1
          end do
        end do
      else if ( node_min .eq. 1 .and. node_max .eq. node_num ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ONE:'
        write ( *, '(a)' ) 
     &    '  The element indexing appears to be 1-based!'
        write ( *, '(a)' ) '  No conversion is necessary.'
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MESH_BASE_ONE - Warning!'
        write ( *, '(a)' ) 
     &    '  The element indexing is not of a recognized type.'
        write ( *, '(a,i8)' ) '  NODE_MIN = ', node_min
        write ( *, '(a,i8)' ) '  NODE_MAX = ', node_max
        write ( *, '(a,i8)' ) '  NODE_NUM = ', node_num
      end if

      return
      end
      subroutine r8mat_copy ( m, n, a1, a2 )

c*********************************************************************72
c
cc R8MAT_COPY copies an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the order of the matrix.
c
c    Input, double precision A1(M,N), the matrix to be copied.
c
c    Output, double precision A2(M,N), a copy of the matrix.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      integer i
      integer j

      do j = 1, n
        do i = 1, m
          a2(i,j) = a1(i,j)
        end do
      end do

      return
      end
      subroutine r8mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
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
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character*(*) title

      call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &  jhi, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT transposed.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
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
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title
      integer title_len

      title_len = len_trim ( title )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) title(1:title_len)

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8,6x)') i
        end do

        write ( *, '(''       Row'',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '       Col'

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc
            i = i2lo - 1 + i2
            write ( ctemp(i2), '(g14.6)' ) a(i,j)
          end do

          write ( *, '(2x,i8,5a14)' ) j, ( ctemp(i), i = 1, inc )

        end do

      end do

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
      integer s_len

      ierror = 0
      istate = 0
      isgn = 1
      ival = 0
      s_len = len_trim ( s )

      do i = 1, s_len

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
        length = len_trim ( s )
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
c    '-12.73e-9.23'   -12.73 * 10.0^(-9.23)
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

      nchar = len_trim ( s )

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

        if ( nchar .lt. length + 1 ) then
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
