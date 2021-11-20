/* purpose:  x-windows driver stubbs

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <math.h>

extern void xwinit_();
extern void xreset_();
extern void xtext_();
extern void xpause_();
extern void xgtcmd_();
extern void xfile_();
extern void cshex_();
extern void xmpisw_();

extern void xginit_();
extern void grinit_();
extern void xgdisp_();

/* internal routines */
extern void f2cstring();
extern void c2fstring();

/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xwinit_( int *ncmd, char ctable[], char logo[] )
{

}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xreset_(char list[], int *num, char typ[], char sval[],
        int mark[], char table[], int nptr[], char labels[],
        char values[], int *icmd )
{

}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xfile_(char list[],  char sval[], char tval[], int *icmd )
{

}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xgtcmd_(char list[])
{

}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xpause_()
{

}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xtext_(char line[])
{

}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xginit_(int *ncolor, int rgb[], int *id, int *width, int *height ) 
    {
}

/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void  xgdisp_( int *nx, int *ny,  int *ishift,  int myimage[] )
{
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void grinit_(int *num)
{
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void cshex_( char fstring[] )
{
        char            cstring[81];
 
/*      convert f -> c string */
        f2cstring(&cstring[0], &fstring[0] , 80);
 
/*      sh < cstring */
        system (&cstring[0]);
 
}

/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xmpi_( int *mpisw )
{
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void c2fstring (char*  cstring,  char* fstring, int flen)
{
        int i, clen, start;

        clen = strlen(cstring);
        start = 0;
        for (i= 0; i< clen; i++){
            if(cstring[i] == '\n') start= i+ 1;
            if(cstring[i] == '\r') start= i+ 1;
            if(cstring[i] == '\b') start= i+ 1;
        }
        for (i=0; i<flen; i++) {
                fstring[i] = ' ';
        }
        if ( start + flen < clen ) clen = start + flen;
        for (i= start; i<clen; i++) {
                fstring[i-start] = cstring[i];
        }
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void f2cstring (char*  cstring,  char* fstring, int flen)
{
        int i, indx;
        indx = 0;
        for (i=flen-1; i>=0; i--) {
                if (fstring[i] != ' ') {
                        indx = i+1;
                        break;
                }
        }
        for (i=0; i<indx; i++) {
                cstring[i] = fstring[i];
        }
        cstring[indx] = '\0';
}












