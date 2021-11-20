gfortran -c rfmm2dpart_dr.f
gfortran -c ../src/rfmm2dpart.f
gfortran -c ../src/cfmm2dpart.f
gfortran -c ../src/lfmm2drouts.f
gfortran -c ../src/d2tstrcr_omp.f
gfortran -c ../src/d2mtreeplot.f
gfortran -c ../src/l2dterms.f
gfortran -c ../src/laprouts2d.f
gfortran -c ../src/prini.f
gfortran -c hkrand.f
gfortran -c dlaran.f

gfortran *.o

mv a.out rfmm2dpart_dr

