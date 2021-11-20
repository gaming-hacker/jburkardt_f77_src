#! /bin/bash
#
gfortran -c pdflib_test.f
if [ $? -ne 0 ]; then
  echo "Compile errors."
  exit
fi
#
gfortran pdflib_test.o -L$HOME/libf77 -lpdflib -lrnglib
if [ $? -ne 0 ]; then
  echo "Load errors."
  exit
fi
rm pdflib_test.o
#
mv a.out pdflib_test
./pdflib_test > pdflib_test.txt
if [ $? -ne 0 ]; then
  echo "Run errors."
  exit
fi
rm pdflib_test
#
echo "Normal end of execution."
