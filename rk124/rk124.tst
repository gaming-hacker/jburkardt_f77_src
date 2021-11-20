N
This data file is used to verify that UPJODE is working properly.
.
D upjode.trn
N
First let's make sure all the help stuff is in place.
.
H
?
Commands


V
T
N
Now let's define a new problem.
.
TINIT=1.0
NEQN=1
Y1=1.0
Y1'=2*T
X1=T*T
METHOD=3
TSTOP=5.0
NSTEPS=8
IPRINT=1
T
N
Now let's write the problem data to disk.
.
W upjode.out
N
Now let's integrate the ODE.
.
C
METHOD=1
C
N
Now let's write the solution data to disk.
.
P upjode.plt
D
QUIT
YES
