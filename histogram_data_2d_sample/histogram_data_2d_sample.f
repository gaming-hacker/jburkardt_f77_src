      program main

c*********************************************************************72
c
cc MAIN is the main program for HISTOGRAM_DATA_2D_SAMPLE.
c
c  Discussion:
c
c    This program is an example of how discrete sample or density data
c    can be used to define a PDF (probability density function).
c
c    In this function and the functions it calls, we assume that we have
c    data for an array of 20 by 20 square subcells of the unit square.
c    We wish to derive a PDF that will allow us to sample an arbitrary
c    number of points from this region.
c
c    In particular, we intend to use the discrete data to generate a PDF
c    which we will then use to generate sample points.
c
c    Roughly speaking, we have kept track of how many fish we caught in
c    each part of a lake, and now we want to simulate catching N fish
c    under the same conditions.
c
c    The statistics for each simulation should be governed by the discrete
c    PDF, but with random variation.  In other words, the actual number
c    of points taken from each subregion is random, and the actual location of
c    each point in a subregion is random, but over many simulations, the
c    statistics of the sample points should reproduce the statistics of
c    the original discrete sample that defined the PDF.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Local, integer N, the number of sample points to be generated.
c
      implicit none

      integer n
      integer n1
      integer n2

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HISTOGRAM_DATA_2D_SAMPLE:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Generate sample data using a discrete PDF.'

      n = 1000
      call get_discrete_pdf_size1 ( n1, n2 )
      call test01 ( n, n1, n2 )

      n = 1000
      call get_discrete_pdf_size2 ( n1, n2 )
      call test02 ( n, n1, n2 )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HISTOGRAM_DATA_2D_SAMPLE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( n,  n1, n2 )

c*********************************************************************72
c
cc TEST01 looks at a 20x20 region.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of sample points to be generated.
c
      implicit none

      integer n
      integer n1
      integer n2

      double precision cdf(n1,n2)
      character * ( 255 ) filename
      double precision pdf(n1,n2)
      integer seed
      double precision u(n)
      double precision xy(2,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Consider data skewed toward the upper left corner'
      write ( *, '(a)' ) '  of the unit square.'
      write ( *, '(a,i4,a)' ) '  Generate ', n, ' samples'
c
c  Get the dimensions of the PDF data.
c
      write ( *, '(a,i4,a,i4,a)' ) 
     &  '  PDF data is on a ', n1, ' by ', n2, ' grid.'
c
c  Construct a PDF from the data.
c
      call get_discrete_pdf_data1 ( n1, n2, pdf )
c
c  "Integrate" the data over rows and columns of the region to get the CDF.
c
      call set_discrete_cdf ( n1, n2, pdf, cdf )
c
c  Choose N CDF values at random.
c
      seed = 123456789
 
      call r8vec_uniform_01 ( n, seed, u )
c
c  Find the cell corresponding to each CDF value,
c  and choose a random point in that cell.
c
      call discrete_cdf_to_xy ( n1, n2, cdf, n, u, seed, xy )
c
c  Write data to a file for examination, plotting, or analysis.
c
      filename = 'test01.txt'
      call r8mat_write ( filename, 2, n, xy )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Wrote sample data to file "' // 
     &  trim ( filename ) // '".'

      return
      end
      subroutine test02 ( n, n1, n2 )

c*********************************************************************72
c
cc TEST02 looks at a 12x8 region.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of sample points to be generated.
c
      implicit none

      integer n
      integer n1
      integer n2

      double precision cdf(n1,n2)
      character * ( 255 ) filename
      double precision pdf(n1,n2)
      integer seed
      double precision u(n)
      double precision xy(2,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Consider data suggested by the shape and density of Iowa.'
      write ( *, '(a,i4,a)' ) '  Generate ', n, ' samples'

      write ( *, '(a,i4,a,i4,a)' ) 
     &  '  PDF data is on a ', n1, ' by ', n2, ' grid.'
c
c  Construct a PDF from the data.
c
      call get_discrete_pdf_data2 ( n1, n2, pdf )
c
c  "Integrate" the data over rows and columns of the region to get the CDF.
c
      call set_discrete_cdf ( n1, n2, pdf, cdf )
c
c  Choose N CDF values at random.
c
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, u )
c
c  Find the cell corresponding to each CDF value,
c  and choose a random point in that cell.
c
      call discrete_cdf_to_xy ( n1, n2, cdf, n, u, seed, xy )
c
c  Write data to a file for examination, plotting, or analysis.
c
      filename = 'test02.txt'
      call r8mat_write ( filename, 2, n, xy )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Wrote sample data to file "' // 
     &  trim ( filename ) // '".'

      return
      end
      subroutine discrete_cdf_to_xy ( n1, n2, cdf, n, u, seed, xy )

c*********************************************************************72
c
cc DISCRETE_CDF_TO_XY finds XY points corresponding to discrete CDF values.
c
c  Discussion:
c
c    This program is given a discrete CDF function and a set of N random
c    values U.  Each value of U corresponds to a particular (I,J) subregion
c    whose CDF value just exceeds the value of U.  Inside that subregion,
c    we pick a point at random - this is equivalent to assuming the PDF
c    is constant over the subregion.
c
c    This function is part of an example program, for which various
c    assumptions have been made.  In particular, the region is the unit
c    square, and the subregions are formed by an N1 by N2 grid of rectangles.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, the number of rows and columns
c    of PDF data.
c
c    Input, double precision CDF(N1,N2), the CDF values associated with each
c    subcell.  A particular ordering has been given to the subcells so that the
c    CDF is a monotonoe function when the subcells are listed in that order.
c
c    Input, integer N, the number of sample points.
c
c    Input, double precision U(N), N random values.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision XY(2,N), the sample points in the unit square.
c
      implicit none

      integer n
      integer n1
      integer n2

      double precision cdf(n1,n2)
      double precision high
      integer i
      integer j
      integer k
      double precision low
      double precision r(2)
      integer seed
      double precision u(n)
      double precision xy(2,n)

      do j = 1, n
        do i = 1, 2
          xy(i,j) = 0.0D+00
        end do
      end do

      low = 0.0D+00
      do j = 1, n2
        do i = 1, n1
          high = cdf(i,j)
          do k = 1, n
            if ( low .le. u(k) .and. u(k) .le. high ) then
              call r8vec_uniform_01 ( 2, seed, r )
              xy(1,k) = ( dble ( i - 1 ) + r(1) ) / dble ( n1 )
              xy(2,k) = ( dble ( j - 1 ) + r(2) ) / dble ( n2 )
            end if
          end do
          low = high
        end do
      end do

      return
      end
      subroutine get_discrete_pdf_data1 ( n1, n2, pdf )

c*********************************************************************72
c
cc GET_DISCRETE_PDF_DATA1 returns the discrete PDF data array.
c
c  Discussion:
c
c    Cell (I,J) extends from
c
c      (I-1) * H < Y < I * H
c      (J-1) * H < X < J * H
c
c    We have data for each cell, representing the integral of some PDF
c    over that cell.  The function pdf(x,y) must be nonnegative.  However,
c    we don't impose any other conditions on it.
c
c    The array PDF(:,:) contains the integral of pdf(x,y) over each cell,
c    or, almost as good, simply a sample or average value.
c
c    We load the array PDF, and then we normalize it so that the sum of
c    all the entries is 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, the number of rows and columns
c    of PDF data.
c
c    Output, double precision PDF(N1,N2).  PDF(I,J) is the discrete PDF
c    for the cell (I,J), normalized so that the sum over all cells is 1.
c
      implicit none

      integer n1
      integer n2

      integer i
      integer j
      double precision pdf(n1,n2)
      double precision pdf_save(20,20)
      double precision r8mat_sum
      double precision total

      save pdf_save

      data pdf_save /
     &  0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,
     &  0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,
     &  0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,
     &  0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,
     &  0.0000D+00, 0.0000D+00, 0.0001D+00, 0.0001D+00, 0.0002D+00,
     &  0.0002D+00, 0.0002D+00, 0.0003D+00, 0.0003D+00, 0.0003D+00,
     &  0.0003D+00, 0.0003D+00, 0.0002D+00, 0.0002D+00, 0.0002D+00,
     &  0.0002D+00, 0.0001D+00, 0.0001D+00, 0.0001D+00, 0.0000D+00,
     &  0.0000D+00, 0.0001D+00, 0.0002D+00, 0.0003D+00, 0.0004D+00,
     &  0.0004D+00, 0.0005D+00, 0.0006D+00, 0.0006D+00, 0.0006D+00,
     &  0.0006D+00, 0.0006D+00, 0.0005D+00, 0.0005D+00, 0.0004D+00,
     &  0.0003D+00, 0.0003D+00, 0.0002D+00, 0.0001D+00, 0.0000D+00,
     &  0.0000D+00, 0.0002D+00, 0.0003D+00, 0.0005D+00, 0.0006D+00,
     &  0.0008D+00, 0.0009D+00, 0.0009D+00, 0.0010D+00, 0.0010D+00,
     &  0.0010D+00, 0.0009D+00, 0.0008D+00, 0.0008D+00, 0.0007D+00,
     &  0.0006D+00, 0.0004D+00, 0.0003D+00, 0.0002D+00, 0.0000D+00,
     &  0.0000D+00, 0.0003D+00, 0.0005D+00, 0.0008D+00, 0.0010D+00,     
     &  0.0012D+00, 0.0014D+00, 0.0015D+00, 0.0015D+00, 0.0015D+00,
     &  0.0015D+00, 0.0014D+00, 0.0013D+00, 0.0011D+00, 0.0010D+00,
     &  0.0008D+00, 0.0006D+00, 0.0005D+00, 0.0003D+00, 0.0000D+00,
     &  0.0000D+00, 0.0004D+00, 0.0009D+00, 0.0013D+00, 0.0016D+00,
     &  0.0019D+00, 0.0021D+00, 0.0023D+00, 0.0023D+00, 0.0023D+00,
     &  0.0021D+00, 0.0020D+00, 0.0018D+00, 0.0016D+00, 0.0013D+00,
     &  0.0011D+00, 0.0009D+00, 0.0007D+00, 0.0004D+00, 0.0000D+00,
     &  0.0000D+00, 0.0007D+00, 0.0014D+00, 0.0020D+00, 0.0025D+00,
     &  0.0030D+00, 0.0033D+00, 0.0034D+00, 0.0034D+00, 0.0033D+00,
     &  0.0031D+00, 0.0028D+00, 0.0025D+00, 0.0022D+00, 0.0018D+00,
     &  0.0015D+00, 0.0012D+00, 0.0009D+00, 0.0006D+00, 0.0000D+00,
     &  0.0000D+00, 0.0011D+00, 0.0021D+00, 0.0031D+00, 0.0039D+00,
     &  0.0045D+00, 0.0049D+00, 0.0051D+00, 0.0050D+00, 0.0047D+00,
     &  0.0043D+00, 0.0039D+00, 0.0034D+00, 0.0029D+00, 0.0024D+00,
     &  0.0019D+00, 0.0015D+00, 0.0011D+00, 0.0007D+00, 0.0000D+00,
     &  0.0000D+00, 0.0017D+00, 0.0033D+00, 0.0048D+00, 0.0060D+00,     
     &  0.0069D+00, 0.0074D+00, 0.0074D+00, 0.0072D+00, 0.0066D+00,
     &  0.0059D+00, 0.0052D+00, 0.0045D+00, 0.0037D+00, 0.0031D+00,
     &  0.0025D+00, 0.0019D+00, 0.0014D+00, 0.0009D+00, 0.0000D+00,
     &  0.0000D+00, 0.0025D+00, 0.0050D+00, 0.0073D+00, 0.0091D+00,
     &  0.0104D+00, 0.0109D+00, 0.0107D+00, 0.0101D+00, 0.0091D+00,
     &  0.0080D+00, 0.0068D+00, 0.0057D+00, 0.0047D+00, 0.0038D+00,
     &  0.0030D+00, 0.0023D+00, 0.0017D+00, 0.0011D+00, 0.0000D+00,
     &  0.0000D+00, 0.0038D+00, 0.0075D+00, 0.0110D+00, 0.0136D+00,
     &  0.0153D+00, 0.0157D+00, 0.0151D+00, 0.0138D+00, 0.0121D+00,
     &  0.0104D+00, 0.0087D+00, 0.0071D+00, 0.0058D+00, 0.0046D+00,
     &  0.0036D+00, 0.0027D+00, 0.0019D+00, 0.0012D+00, 0.0000D+00,
     &  0.0000D+00, 0.0055D+00, 0.0110D+00, 0.0160D+00, 0.0198D+00,
     &  0.0218D+00, 0.0219D+00, 0.0205D+00, 0.0182D+00, 0.0155D+00,
     &  0.0129D+00, 0.0106D+00, 0.0085D+00, 0.0068D+00, 0.0053D+00,
     &  0.0041D+00, 0.0031D+00, 0.0022D+00, 0.0014D+00, 0.0000D+00,
     &  0.0000D+00, 0.0077D+00, 0.0154D+00, 0.0224D+00, 0.0276D+00,     
     &  0.0299D+00, 0.0293D+00, 0.0266D+00, 0.0229D+00, 0.0190D+00,
     &  0.0154D+00, 0.0123D+00, 0.0098D+00, 0.0077D+00, 0.0059D+00,
     &  0.0045D+00, 0.0034D+00, 0.0024D+00, 0.0015D+00, 0.0000D+00,
     &  0.0000D+00, 0.0100D+00, 0.0202D+00, 0.0295D+00, 0.0362D+00,
     &  0.0385D+00, 0.0368D+00, 0.0324D+00, 0.0271D+00, 0.0219D+00,
     &  0.0174D+00, 0.0137D+00, 0.0107D+00, 0.0082D+00, 0.0063D+00,
     &  0.0048D+00, 0.0035D+00, 0.0025D+00, 0.0016D+00, 0.0000D+00,
     &  0.0000D+00, 0.0120D+00, 0.0244D+00, 0.0356D+00, 0.0432D+00,
     &  0.0455D+00, 0.0426D+00, 0.0366D+00, 0.0298D+00, 0.0236D+00,
     &  0.0184D+00, 0.0143D+00, 0.0110D+00, 0.0084D+00, 0.0064D+00,
     &  0.0048D+00, 0.0035D+00, 0.0025D+00, 0.0016D+00, 0.0000D+00,
     &  0.0000D+00, 0.0134D+00, 0.0266D+00, 0.0382D+00, 0.0461D+00,
     &  0.0480D+00, 0.0445D+00, 0.0376D+00, 0.0301D+00, 0.0235D+00,
     &  0.0181D+00, 0.0139D+00, 0.0106D+00, 0.0081D+00, 0.0061D+00,
     &  0.0046D+00, 0.0033D+00, 0.0023D+00, 0.0015D+00, 0.0000D+00,
     &  0.0000D+00, 0.0151D+00, 0.0261D+00, 0.0362D+00, 0.0436D+00,
     &  0.0447D+00, 0.0412D+00, 0.0347D+00, 0.0276D+00, 0.0214D+00,
     &  0.0164D+00, 0.0125D+00, 0.0095D+00, 0.0072D+00, 0.0054D+00,
     &  0.0041D+00, 0.0029D+00, 0.0021D+00, 0.0013D+00, 0.0000D+00,
     &  0.0000D+00, 0.0174D+00, 0.0220D+00, 0.0295D+00, 0.0349D+00,
     &  0.0361D+00, 0.0333D+00, 0.0281D+00, 0.0225D+00, 0.0175D+00,
     &  0.0134D+00, 0.0102D+00, 0.0078D+00, 0.0059D+00, 0.0044D+00,
     &  0.0033D+00, 0.0024D+00, 0.0017D+00, 0.0010D+00, 0.0000D+00,
     &  0.0000D+00, 0.0097D+00, 0.0152D+00, 0.0200D+00, 0.0235D+00,
     &  0.0244D+00, 0.0227D+00, 0.0193D+00, 0.0156D+00, 0.0122D+00,
     &  0.0094D+00, 0.0072D+00, 0.0055D+00, 0.0041D+00, 0.0031D+00,
     &  0.0023D+00, 0.0017D+00, 0.0012D+00, 0.0007D+00, 0.0000D+00,
     &  0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,
     &  0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,
     &  0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00,
     &  0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00 /

      do j = 1, n2
        do i = 1, n1
          pdf(i,j) = pdf_save(i,j)
        end do
      end do
c
c  Normalize to get an integral of 1.
c
      total = r8mat_sum ( n1, n2, pdf )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF data sums to ', total

      do j = 1, n2
        do i = 1, n1
          pdf(i,j) = pdf(i,j) / total
        end do
      end do

      return
      end
      subroutine get_discrete_pdf_data2 ( n1, n2, pdf )

c*********************************************************************72
c
cc GET_DISCRETE_PDF_DATA2 returns the discrete PDF data array.
c
c  Discussion:
c
c    Cell (I,J) extends from
c
c      (I-1) * H < Y < I * H
c      (J-1) * H < X < J * H
c
c    We have data for each cell, representing the integral of some PDF
c    over that cell.  The function pdf(x,y) must be nonnegative.  However,
c    we don't impose any other conditions on it.
c
c    The array PDF(:,:) contains the integral of pdf(x,y) over each cell,
c    or, almost as good, simply a sample or average value.
c
c    We load the array PDF, and then we normalize it so that the sum of
c    all the entries is 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, the number of rows and columns
c    of PDF data.
c
c    Output, double precision PDF(N1,N2).  PDF(I,J) is the discrete PDF
c    for the cell (I,J), normalized so that the sum over all cells is 1.
c
      implicit none

      integer n1
      integer n2

      integer i
      integer j
      double precision pdf(n1,n2)
      double precision pdf_save(12,8)
      double precision r8mat_sum
      double precision total

      save pdf_save

      data pdf_save /
     &  10.0,  20.0,  10.0,  10.0,  20.0,  10.0,
     &  30.0,  10.0,  10.0,  10.0,  10.0,  50.0,
     &  25.0,  30.0,  10.0,  25.0,  30.0,  40.0,    
     &  30.0,  20.0,  10.0,  20.0,  30.0,  40.0,
     &  25.0,  30.0,  20.0,  10.0,  40.0, 200.0,
     &  50.0,  40.0,  10.0,  30.0,  60.0,  40.0,
     &  20.0,  30.0,  40.0,  10.0,  75.0, 100.0,
     & 100.0,  30.0,  25.0,  25.0,  90.0,  30.0,
     &  75.0,  15.0,  20.0,  10.0,  75.0,  50.0,
     &  40.0,  10.0, 100.0,  25.0,  25.0,  80.0,
     &  25.0,  50.0,  50.0,  10.0,  25.0,  25.0,
     &  15.0,  10.0,  25.0,  25.0,  10.0,  10.0,
     & 100.0,  50.0,  50.0,  10.0,  10.0,  10.0,      
     &   5.0, 100.0,  50.0,  50.0,  10.0,  10.0,
     &  10.0,  10.0,  25.0,  50.0,  10.0,  50.0,
     &  10.0,  50.0,  25.0,  25.0,  25.0,  10.0 /

      do j = 1, n2
        do i = 1, n1
          pdf(i,j) = pdf_save(i,j)
        end do
      end do
c
c  Normalize to get an integral of 1.
c
      total = r8mat_sum ( n1, n2, pdf )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  PDF data sums to ', total

      do j = 1, n2
        do i = 1, n1
          pdf(i,j) = pdf(i,j) / total
        end do
      end do

      return
      end
      subroutine get_discrete_pdf_size1 ( n1, n2 )

c*********************************************************************72
c
cc GET_DISCRETE_PDF_SIZE1 returns the dimension of the discrete PDF data
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer N1, N2, the number of rows and columns
c    of data.
c
      implicit none

      integer n1
      integer n2

      n1 = 20
      n2 = 20

      return
      end
      subroutine get_discrete_pdf_size2 ( n1, n2 )

c*********************************************************************72
c
cc GET_DISCRETE_PDF_SIZE2 returns the dimension of the discrete PDF data
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer N1, N2, the number of rows and columns
c    of data.
c
      implicit none

      integer n1
      integer n2

      n1 = 12
      n2 = 8

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
      function r8mat_sum ( m, n, a )

c*********************************************************************72
c
cc R8MAT_SUM sums the entries of an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 January 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), the array.
c
c    Output, double precision R8MAT_SUM, the sum of the entries.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      integer i
      integer j
      double precision r8mat_sum
      double precision value

      value = 0.0D+00
      do j = 1, n
        do i = 1, m
          value = value + a(i,j)
        end do
      end do

      r8mat_sum = value

      return
      end
      subroutine r8mat_write ( output_filename, m, n, table )

c*********************************************************************72
c
cc R8MAT_WRITE writes a R8MAT file.
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
c    22 October 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) OUTPUT_FILENAME, the output file name.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision TABLE(M,N), the data.
c
      implicit none

      integer m
      integer n

      integer j
      character * ( * ) output_filename
      integer output_unit
      character * ( 30 ) string
      double precision table(m,n)
c
c  Open the file.
c
      call get_unit ( output_unit )

      open ( unit = output_unit, file = output_filename,
     &  status = 'replace' )
c
c  Create the format string.
c
      if ( 0 .lt. m .and. 0 .lt. n ) then

        write ( string, '(a1,i8,a1,i8,a1,i8,a1)' )
     &    '(', m, 'g', 24, '.', 16, ')'
c
c  Write the data.
c
        do j = 1, n
          write ( output_unit, string ) table(1:m,j)
        end do

      end if
c
c  Close the file.
c
      close ( unit = output_unit )

      return
      end
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
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
c    17 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

      end do

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
      subroutine set_discrete_cdf ( n1, n2, pdf, cdf )

c*********************************************************************72
c
cc SET_DISCRETE_CDF sets a CDF from a discrete PDF.
c
c  Discussion:
c
c    Here, we proceed from cell (1,1) to (2,1) to
c    (N1,1), (1,2), (2,2)...(N1,N2).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, the number of rows and columns
c    of PDF data.
c
c    Input, double precision PDF(N1,N2), the discrete PDF for the cell (I,J),
c    normalized so that the sum over all cells is 1.
c
c    Output, double precision CDF(N1,N2), the discrete CDF for the cell (I,J).
c    CDF(N1,N2) should be 1.
c
      implicit none

      integer n1
      integer n2

      double precision cdf(n1,n2)
      integer i
      integer j
      double precision pdf(n1,n2)
      double precision total

      total = 0.0D+00
      do j = 1, n2
        do i = 1, n1
          total = total + pdf(i,j)
          cdf(i,j) = total
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
