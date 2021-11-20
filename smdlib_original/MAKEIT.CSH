#!/bin/csh
#
f77 -o smdfont smdfont.f
smdfont < smdfonts.list
rm smdfont
mv smdfonts.dat ~/bin/smdfonts.dat
