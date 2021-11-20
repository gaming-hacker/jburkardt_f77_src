#!/bin/csh
#
F77 -c -g handler_report_filter.f >& compiler.txt
if ( $status != 0 ) then
  echo "Errors compiling handler_report_filter.f"
  exit
endif
rm compiler.txt
#
F77 handler_report_filter.o
if ( $status != 0 ) then
  echo "Errors linking and loading handler_report_filter.o"
  exit
endif
rm handler_report_filter.o
#
chmod ugo+x a.out
mv a.out ~/binf77/$ARCH/handler_report_filter
#
echo "Executable installed as ~/binf77/$ARCH/handler_report_filter"
