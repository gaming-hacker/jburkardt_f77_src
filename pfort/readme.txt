pfort can be compiled with the command run.
If the .o files already exist, the lod file can be used.
In each case, the .f files from the pfort tape
(or the corresponding .o files) without the last
line on the tape (which started with a dot in column one)
are compiled and loaded. In addition, the files pack.f
and unpack.f, written by me, are included in the compilation.
These pack and unpack routines are machine dependent
and must be written in assembly language on machines
that do not provide for byte manipulation in their
fortran. For the VAX, f77 programs were adequate for the job.
See readpfort2 for runcoms for making and testing
the pack and unpack programs.
      See readpfort1 for how the tape was read.
The undefined: L15 recurs when run is used, and one gets
a huge PFORT file with 206760 bytes. (This has happened to
me before when something was undefined ore missing.)
However, a subsequent use of the lod command works and does not
complain about anything missing. It produced the PFORT file in this
directory which is slightly smaller than the OPFORT file
made previously. The latter is identical to /usr/lbin/PFORT
which I have executed on various files.
       The compilation was done with the command
run > outt 2>outt2
The error messages in outt2 are all warnings. These warnings
may explain why I could not use the options to suppress printout.
the lod command produces output even though it complains about L15. !!!


The pfort tape was read in using dd with a conversion from ebcdic
to ascii. The last eight characters on each line were removed.
Then all trailing blanks were removed. (These operations
were performed with the stream editor.)
Although it was not necessary to do so, pfort was then split
using fsplit into its subroutines and functions.
        The subroutines s5pack and s5unpk were then written
following A.D.Hall's specifications. Note that he expects
the array variables to be integers. We found it expedient,
however, to use character arrays internally to the subroutines.
This worked because fortran passes locations on a subroutine
call. These subroutines, located in files pack.f and unpack.f
were tested using tstpak.f.
        An f77 run command was placed in the file run. The command
run > out 2>tt2out
was then given. The file tt2 was constructed from tt2out by deleting all
lines containing the word warning. It was then seen that all files
were successfully compiled. However, the binary output file
PFORT was not executable. An examination of the file out
gave the message:
   L15 undefined
A grep of L15 on all .f files drew a blank. Is L15
some internal variable that would have been defined if I used
an appropriate library?

               mel lax
P.S. This file is /usr/lsrc/pfort/readme, and all files
referred to reside in /usr/lsrc/pfort
P.P.S. mktst is a makefile for tstpak packc and unpakc.
