#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <rpc/rpc.h>

static  XDR xdrs[10];
static  FILE *myFile[10];
static  char filename[256];
static  int istack[10], irw[10], maxid = 10, ifirst = 1, next = 0;

extern void xdrutl_();
extern void xdrint_();
extern void xdrflt_();
extern void xdrdbl_();
extern void xdrstr_();
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xdrutl_( int *id, char fname[],  char *mode ,  int *iflag )
{
        int i;

        *iflag =0 ;
        if (ifirst == 1) {
            for ( i = 0; i < maxid; i++) {
                irw[i] =0;
                istack[i] = i+1;
            }
            istack[maxid -1] = -1;
            next = 0;
            ifirst = 0;
        }
  
/*      clean up     */
  
        if( *mode ==  'c'){
            if ( *id < 0 || *id >= maxid ) {
                *iflag = 4;
                return;
            }
            if ( irw[*id] == 0 ) {
                *iflag = 5;
                return;
            }
            irw[*id] = 0;
            istack[*id] = next;
            next = *id;
            xdr_destroy( &xdrs[*id] );
            fclose(myFile[*id]);
            return;
        }
  
        if( next >= 0 ) {
            *id = next;
            next = istack[*id];
        } else {
            *iflag = 3;
            return;
        }

/*      convert fortan string to c string     */  
  
        filename[80] = '\0';
        for (i=0; i<80; i++) {    
            filename[i] = fname[i];
            if(fname[i] == ' ') {
                filename[i] = '\0';
                break;
            }
        }
  
/*      initialize for writing   */
  
        if( *mode ==  'w'){
            myFile[*id]  = fopen(filename,"w");
            if( myFile[*id]  == NULL) { 
                *iflag = 1;
                irw [*id] =0;
                istack[*id] = next;
                next = *id;
            } else {
                xdrstdio_create( &xdrs[*id], myFile[*id],  XDR_ENCODE );
                irw[*id] = 1;
            }

/*      initialize for reading    */
      
        } else if ( *mode == 'r'){
            myFile[*id]  = fopen(filename,"r");
            if( myFile[*id]  == NULL) { 
                *iflag = 1;
                irw [*id] =0;
                istack[*id] = next;
                next = *id;
            } else {
                xdrstdio_create( &xdrs[*id], myFile[*id],  XDR_DECODE );
                irw[*id] = -1;
            }
        } else {
            *iflag = 2;
        }
        return;
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xdrint_( int *id, int *ival , uint *len , int *iflag )
{
        int i;
  
/*      process an array of integers   */
  
        *iflag = 0;
        if ( *id < 0 || *id >= maxid ) {
            *iflag = 4;
            return;
        }
        if ( irw[*id] == 0 ) {
            *iflag = 5;
            return;
        }

        i= xdr_vector(&xdrs[*id], (char *)ival, *len, sizeof(int),  
                (xdrproc_t) xdr_int );

        if( i != 1 ) {
            if ( irw[*id] < 0 ) {
                *iflag = 6;
            } else {
                *iflag = 7;
            }
        }
        return;
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xdrflt_( int *id, float *rval , uint *len , int *iflag )
{
        int i;

/*      process an array of floats  */

        *iflag = 0;
        if ( *id < 0 || *id >= maxid ) {
            *iflag = 4;
            return;
        }
        if ( irw[*id] == 0 ) {
            *iflag = 5;
            return;
        }
  
        i= xdr_vector(&xdrs[*id], (char *)rval, *len, sizeof(float), 
                (xdrproc_t) xdr_float );

        if( i != 1 ) {
            if ( irw[*id] < 0 ) {
                *iflag = 6;
            } else {
                *iflag = 7;
            }
        }
        return;
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xdrstr_( int *id, char cval[] , uint *len , int *iflag )
{
        int i;
        uint ii;
/*
        process an array of characters
*/
        *iflag = 0;
        if ( *id < 0 || *id >= maxid ) {
            *iflag = 4;
            return;
        }
        if ( irw[*id] == 0 ) {
            *iflag = 5;
            return;
        }

        ii = *len;
        i = xdr_bytes(&xdrs[*id] , (char **)&cval, &ii, *len);

        if( i != 1 ) {
            if ( irw[*id] < 0 ) {
                *iflag = 6;
            } else {
                *iflag = 7;
            }
        }
    return;
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xdrdbl_( int *id, double *rval , uint *len , int *iflag )
{
        int i;

/*      process an array of doubles  */

        *iflag = 0;
        if ( *id < 0 || *id >= maxid ) {
            *iflag = 4;
            return;
        }
        if ( irw[*id] == 0 ) {
            *iflag = 5;
            return;
        }
  
        i= xdr_vector(&xdrs[*id], (char *)rval, *len, sizeof(double), 
                (xdrproc_t) xdr_double );

        if( i != 1 ) {
            if ( irw[*id] < 0 ) {
                *iflag = 6;
            } else {
                *iflag = 7;
            }
        }
        return;
}
