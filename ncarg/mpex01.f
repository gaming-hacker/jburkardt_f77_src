      PROGRAM MPEX01
C
C The program MPEX01 produces a map of the U.S., using a Lambert
C conformal conic.
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=21, IWKID=1)
C
C Define the label for the top of the map.
C
      CHARACTER*37 PLBL
C
      DATA PLBL / 'THE U.S. ON A LAMBERT CONFORMAL CONIC' /
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Set the outline-dataset parameter.
C
C     CALL MAPSTC ( 'OU', 'US' )
      CALL MAPSTC ( 'OU', 'PO' )
C
C Set the projection-type parameters.
C
      CALL MAPROJ ('LC',30.,-100.,45.)
C
C Set the limits parameters.
C
C     CALL MAPSET ('CO',22.6,-120.,46.9,-64.2)
      CALL MAPSET ('CO', 50.0, -12.0, 56.0, -6.0 )
C
C Draw the map.
C
      CALL MAPDRW
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PWRIT (.5,.925,PLBL,37,2,0,0)
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
C Done.
C
      STOP
C
      END
      SUBROUTINE BNDARY
C
C Routine to draw the plotter-frame edge.
C
      CALL PLOTIT (    0,    0,0)
      CALL PLOTIT (32767,    0,1)
      CALL PLOTIT (32767,32767,1)
      CALL PLOTIT (    0,32767,1)
      CALL PLOTIT (    0,    0,1)
      RETURN
      END
