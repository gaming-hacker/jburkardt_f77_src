include ${PETSC_DIR}/bmake/common/base

hello: hello.o
	-${FLINKER} -o hello hello.o ${PETSC_FORTRAN_LIB} ${PETSC_LIB}
	${RM} hello.o
ex1: ex1.o
	-${FLINKER} -o ex1 ex1.o ${PETSC_FORTRAN_LIB} ${PETSC_LIB}
	${RM} ex1.o
ex22: ex22.o
	-${FLINKER} -o ex22 ex22.o ${PETSC_FORTRAN_LIB} ${PETSC_LIB}
	${RM} ex22.o

