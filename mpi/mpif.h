c  mpif.h  19 October 2018
c
c  user include file for MPI programs, with no dependencies 
c
c  Return codes 
c
      integer, parameter :: MPI_SUCCESS = 0
      integer, parameter :: MPI_ERR_BUFFER = 1
      integer, parameter :: MPI_ERR_COUNT = 2
      integer, parameter :: MPI_ERR_TYPE = 3
      integer, parameter :: MPI_ERR_TAG = 4
      integer, parameter :: MPI_ERR_COMM = 5
      integer, parameter :: MPI_ERR_RANK = 6
      integer, parameter :: MPI_ERR_ROOT = 7
      integer, parameter :: MPI_ERR_GROUP = 8
      integer, parameter :: MPI_ERR_OP = 9
      integer, parameter :: MPI_ERR_TOPOLOGY = 10
      integer, parameter :: MPI_ERR_DIMS = 11
      integer, parameter :: MPI_ERR_ARG = 12
      integer, parameter :: MPI_ERR_UNKNOWN = 13
      integer, parameter :: MPI_ERR_TRUNCATE = 14
      integer, parameter :: MPI_ERR_OTHER = 15
      integer, parameter :: MPI_ERR_INTERN = 16
      integer, parameter :: MPI_ERR_IN_STATUS = 17
      integer, parameter :: MPI_ERR_PENDING = 18
      integer, parameter :: MPI_ERR_REQUEST = 19
      integer, parameter :: MPI_ERR_LASTCODE = 4114

      integer, parameter :: MPI_UNDEFINED = -32766

      integer, parameter :: MPI_GRAPH = 1
      integer, parameter :: MPI_CART = 2
      integer, parameter :: MPI_PROC_NULL = -1

      integer, parameter :: MPI_BSEND_OVERHEAD = 512

      integer, parameter :: MPI_SOURCE = 2
      integer, parameter :: MPI_TAG = 3
      integer, parameter :: MPI_ERROR = 4
      integer, parameter :: MPI_STATUS_SIZE = 4

      integer, parameter :: MPI_MAX_PROCESSOR_NAME = 256
      integer, parameter :: MPI_MAX_ERROR_STRING = 512
      integer, parameter :: MPI_MAX_NAME_STRING = 63

      integer, parameter :: MPI_COMM_NULL = 0

      integer, parameter :: MPI_DATATYPE_NULL = 0
      
      integer, parameter :: MPI_ERRHANDLER_NULL = 0
      
      integer, parameter :: MPI_GROUP_NULL = 0
      
      integer, parameter :: MPI_KEYVAL_INVALID = 0
      
      integer, parameter :: MPI_REQUEST_NULL = 0

      integer, parameter :: MPI_IDENT = 0
      integer, parameter :: MPI_CONGRUENT = 1
      integer, parameter :: MPI_SIMILAR = 2
      integer, parameter :: MPI_UNEQUAL = 3
c
c  MPI_BOTTOM needs to be a known address; here we put it at the
c  beginning of the common block.  The point-to-point and collective
c  routines know about MPI_BOTTOM, but MPI_TYPE_STRUCT as yet does not.
c
c  The types MPI_INTEGER1,2,4 and MPI_REAL4,8 are OPTIONAL.
c  Their values are zero if they are not available.  Note that
c  using these reduces the portability of code (though may enhance
c  portability between Crays and other systems)
c
      integer, parameter :: MPI_ERRORS_ARE_FATAL = 119
      integer, parameter :: MPI_ERRORS_RETURN = 120
      integer, parameter :: MPI_COMPLEX = 23
      integer, parameter :: MPI_DOUBLE_COMPLEX = 24
      integer, parameter :: MPI_LOGICAL = 25
      integer, parameter :: MPI_REAL = 26
      integer, parameter :: MPI_DOUBLE_PRECISION = 27
      integer, parameter :: MPI_INTEGER = 28
      integer, parameter :: MPI_2INTEGER = 29
      integer, parameter :: MPI_2COMPLEX = 30
      integer, parameter :: MPI_2DOUBLE_COMPLEX = 31
      integer, parameter :: MPI_2REAL = 32
      integer, parameter :: MPI_2DOUBLE_PRECISION = 33
      integer, parameter :: MPI_CHARACTER = 1
      integer, parameter :: MPI_BYTE = 3
      integer, parameter :: MPI_UB = 16
      integer, parameter :: MPI_LB = 15
      integer, parameter :: MPI_PACKED = 14

      integer, parameter :: MPI_INTEGER1 = 0
      integer, parameter :: MPI_INTEGER2 = 0
      integer, parameter :: MPI_INTEGER4 = 0
      integer, parameter :: MPI_REAL4 = 0
      integer, parameter :: MPI_REAL8 = 0

      integer MPI_BOTTOM
      common /MPIPRIV/ MPI_BOTTOM
c
c  Without this save, some Fortran implementations may make the common dynamicc
c
      save /MPIPRIV/

      integer, parameter :: MPI_MAX = 100
      integer, parameter :: MPI_MIN = 101
      integer, parameter :: MPI_SUM = 102
      integer, parameter :: MPI_PROD = 103
      integer, parameter :: MPI_LAND = 104
      integer, parameter :: MPI_BAND = 105
      integer, parameter :: MPI_LOR = 106
      integer, parameter :: MPI_BOR = 107
      integer, parameter :: MPI_LXOR = 108
      integer, parameter :: MPI_BXOR = 109
      integer, parameter :: MPI_MINLOC = 110
      integer, parameter :: MPI_MAXLOC = 111
      integer, parameter :: MPI_OP_NULL = 0

      integer, parameter :: MPI_GROUP_EMPTY = 90
      integer, parameter :: MPI_COMM_WORLD = 91
      integer, parameter :: MPI_COMM_SELF = 92
      integer, parameter :: MPI_TAG_UB = 80
      integer, parameter :: MPI_HOST = 82
      integer, parameter :: MPI_IO = 84
      integer, parameter :: MPI_WTIME_IS_GLOBAL = 86

      integer, parameter :: MPI_ANY_SOURCE = -2
      integer, parameter :: MPI_ANY_TAG = -1

      integer, parameter :: MPI_VERSION = 1
      integer, parameter :: MPI_SUBVERSION = 1
c
c  All other MPI routines are subroutines.
c  This may cause some Fortran compilers to complain about defined and
c  not used.  Such compilers should be improved.
c
      double precision MPI_WTIME
      double precision MPI_WTICK
      external MPI_WTIME 
      external MPI_WTICK
c
c  The attribute copy/delete subroutines are symbols that can be passed
c  to MPI routines.
c
      external MPI_NULL_COPY_FN
      external MPI_NULL_DELETE_FN
      external MPI_DUP_FN
