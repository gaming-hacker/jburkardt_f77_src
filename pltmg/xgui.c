/* purpose:  a general x-windows driver for numerical codes */
/* authors:  randy bank and michael holst */
/***************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>  
#include <Xm/Form.h>  
#include <Xm/PushB.h>  
#include <Xm/Label.h> 
#include <Xm/TextF.h> 
#include <Xm/Text.h> 
#include <Xm/DialogS.h>
#include <Xm/Command.h>
#include <Xm/Separator.h>
#include <Xm/FileSB.h>  
#include <Xm/DrawingA.h>  
#include <Xm/ToggleB.h>  
#include <Xm/RowColumn.h>  

static Display         *myDisplay;        
static XtAppContext    myAppCon;          
static Pixmap          trianglePixmap;
static Screen          *myScreen;
static Widget          topLevel, mainForm, commandField;
static Widget          myButton[20], outputFile, myPopup;
static Widget          mainFormSep[2], mpiLabel;
static Widget          varNameBox[50], varValueBox[50]; 
static Widget          varToggle[100], varRadioBox[50], varCheckBox[50];

char                   bgclr[80];
char                   btnbg[80];
/*
static String          bgclr="grey80";
static String          btnbg="gray53";
*/
static Pixel           myForeground, myBackground;
static Pixel           myButtonForeground, myButtonBackground;
static Pixel           myTopShadow, myBottomShadow, mySelect;

static int             ifirst = 1;
static int             varRadioMark[50];
static int             mpiLabelWidth, mpiLabelHeight, mpiBorder;
static int             mainFormWidth, mainFormHeight;
static int             popupWidth, popupFontHeight;
static int             minWidth, minHeight;
static int             maxWidth = 900, maxHeight = 600;
static int             buttonWidth, buttonHeight;
static int             commandFieldWidth, commandFieldHeight;
static int             outputFileWidth, outputFileHeight;
static int             myHorizSep, myVertSep, fontHeight;
static int             defaultSep, defaultBorder, scrollBarWidth;
static XmFontList      myFont, popupFont;

static char            buttonLabels[360],  commandBuffer[81];
static char            radioLabels[8000],  radioValues[8000], radioTable[250];
static int             radioPtr[100];
static int             lastNcmd = 2, overallScale = 50;
static int             commandSwitch = 0;
static int             cancelSwitch = 0;
static int             filterSwitch[24];

static XmTextPosition  myPosition=0;
static String          myFilter[24];

static int             maxgraph = 4;
static Widget          graphicsPopup[10], graphicsArea[10];
static int             graphicsAreaWidth[10], graphicsAreaHeight[10];
static Position        graphicsPositionX[10], graphicsPositionY[10];
static XImage          *graphicsImage[10];
static unsigned long   ColorMapPixels[2560];
static int             ColorStatus[2560];
static int             lastNcolor[10] = {0,0,0,0,0,0,0,0,0,0};
static int             curId, shift;

/* external interface routines */
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
extern void makelogo();
extern void makecolors();
extern void makegeom();
extern void resizeEventHandle();
extern void cmdCallback();
extern void cmdEventHandle();
extern void radioEventHandle();
extern void contCallback();
extern void btnCallback();
extern void cnclCallback();
extern void xradio();
extern void radioCallback();

extern void popgr();
extern void mkgr();
extern void expCallback();
extern void rszCallback();

/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xwinit_( int *ncmd, char ctable[], char logo[] )
{
        int             argc = 0, i, counter, across;
        char            myString[100];
        String          argv;
        XmString        myLabel;
        Widget          tempw, temps, tempv, temph;
        Arg             args[20];
       
        if( *ncmd <= 0) {
            XtDestroyWidget(topLevel);
            XtDestroyApplicationContext(myAppCon); 
            return;
        }
  
/*      initialize */

        argc = 1;
        f2cstring(&myString, &logo[0] , 80);
        argv=(String)myString;
        topLevel = XtVaAppInitialize( &myAppCon, "XPltmg",   
            NULL, 0, &argc, &argv, NULL, NULL);
        myDisplay = XtDisplay(topLevel);
        myScreen = XtScreen(topLevel);

/*      set colors , make triangle logo, compute geometry, make fonts */

        f2cstring(&bgclr, &logo[80] , 80);
        f2cstring(&btnbg, &logo[160] , 80);
        makecolors();
        makelogo();   

        XtVaSetValues(topLevel, 
            XtNiconPixmap,     trianglePixmap, 
            XmNtitle,          myString,
            XmNminAspectX,     3,
            XmNminAspectY,     2,
            XmNmaxAspectX,     3,
            XmNmaxAspectY,     2,
            NULL);   

        lastNcmd = *ncmd;
        for (i = 0; i < lastNcmd; i++) {
            filterSwitch[i] = 0;
        }
        for (i = 0; i < 15 * lastNcmd; i++) {
            buttonLabels[i] = ctable[i];
        }

/*      the main form  */

        mainForm = XtVaCreateManagedWidget("mainForm", 
            xmFormWidgetClass,  topLevel,
            NULL);
        XtAddEventHandler(mainForm, StructureNotifyMask, (int) 0, 
            (XtEventHandler)  resizeEventHandle, (XtPointer) 0);
        XmChangeColor (mainForm,  myBackground);

/*      the command buttons   */

        counter = 0;
        across = 8;
        for (i=0; i< lastNcmd; i++) {

            if (counter == across) counter = 0;

            f2cstring(myString, &buttonLabels[15 * i], 8);
            myLabel = XmStringCreateLocalized(myString );

            myButton[i] = XtVaCreateManagedWidget(myString,
                xmPushButtonWidgetClass, mainForm,
                XmNlabelString,     myLabel,
                XmNtopAttachment,   
                        (i < across) ? XmATTACH_FORM : XmATTACH_WIDGET,
                XmNtopWidget,       
                        (i < across) ? NULL : myButton[i-across],
                XmNleftAttachment,  
                        (counter == 0) ? XmATTACH_FORM : XmATTACH_WIDGET,
                XmNleftWidget,      
                        (counter == 0) ? NULL : myButton[i-1],
                XmNrightAttachment,  ( counter == across) ?
                        XmATTACH_FORM : 0 ,
                XmNrecomputeSize,    FALSE,
                NULL);                 

            XmStringFree(myLabel);
            XmChangeColor (myButton[i],  myButtonBackground);

            XtAddEventHandler(myButton[i], ButtonPressMask, (int) 0, 
                (XtEventHandler)  cmdEventHandle, (XtPointer) i);
            counter++;
        }

/*      separator   */

        mainFormSep[0] = XtVaCreateManagedWidget("sep0",
            xmSeparatorWidgetClass, mainForm,               
            XmNorientation,         XmHORIZONTAL,
            XmNtopAttachment,       XmATTACH_WIDGET,
            XmNtopWidget,           myButton[lastNcmd-1],
            XmNleftAttachment,      XmATTACH_FORM,
            XmNrightAttachment,     XmATTACH_FORM,
            NULL); 
        XmChangeColor (mainFormSep[0],  myBackground);

/*      command text   */

        myLabel = XmStringCreateLocalized("Command:");
        commandField = XtVaCreateManagedWidget("commandField",
            xmCommandWidgetClass, mainForm,                
            XmNtopAttachment,   XmATTACH_WIDGET,
            XmNtopWidget,       mainFormSep[0],
            XmNleftAttachment,  XmATTACH_FORM,
            XmNpromptString,    myLabel,
            XmNleftWidget,      NULL,
            XmNresizePolicy,    XmRESIZE_NONE,
            NULL); 

        XmStringFree(myLabel);
        XmChangeColor(commandField,  myBackground);
        tempw = XtNameToWidget(commandField,"Text");
        XmChangeColor(tempw,  myButtonBackground);
        XtVaSetValues(tempw, XmNmaxLength, 80, NULL );
        tempw = XtNameToWidget(commandField,"ItemsListSW.ItemsList");
        tempv=XtParent(tempw);
        XtUnmanageChild(tempv);

        XtAddCallback (commandField, XmNcommandEnteredCallback, 
            (XtCallbackProc)cmdCallback,(XtPointer)0 );

/*    mpi label    */

        myLabel = XmStringCreateLocalized("   ");
        mpiLabel = XtVaCreateManagedWidget("mpiLabel",
            xmLabelWidgetClass, mainForm,               
            XmNtopAttachment,   XmATTACH_WIDGET,
            XmNtopWidget,       mainFormSep[0],
            XmNleftAttachment,  XmATTACH_WIDGET,
            XmNleftWidget,      commandField,
            XmNrightAttachment,  XmATTACH_FORM,
            XmNlabelType,        XmSTRING,
            XmNlabelString,      myLabel,
            XmNalignment,        XmALIGNMENT_CENTER,
            XmNrecomputeSize,    FALSE,
            NULL); 

        XmChangeColor (mpiLabel,  myBackground);
        XtVaSetValues(mpiLabel, XmNborderColor, myBackground, NULL );
        XmStringFree(myLabel);

/*      separator  */

        mainFormSep[1] = XtVaCreateManagedWidget("sep1",
            xmSeparatorWidgetClass, mainForm,               
            XmNorientation,         XmHORIZONTAL,
            XmNtopAttachment,       XmATTACH_WIDGET,
            XmNtopWidget,           commandField,
            XmNleftAttachment,      XmATTACH_FORM,
            XmNrightAttachment,     XmATTACH_FORM,
            NULL); 
        XmChangeColor (mainFormSep[1],  myBackground);

/*      the output file  */

        i = 0;
        XtSetArg(args[i], XmNeditable,               False); i++;
        XtSetArg(args[i], XmNeditMode,               XmMULTI_LINE_EDIT); i++;
        XtSetArg(args[i], XmNcursorPositionVisible,  False); i++;
        XtSetArg(args[i], XmNtopAttachment,          XmATTACH_WIDGET); i++;
        XtSetArg(args[i], XmNtopWidget,              mainFormSep[1]); i++;
        XtSetArg(args[i], XmNleftAttachment,         XmATTACH_FORM); i++;
        XtSetArg(args[i], XmNrightAttachment,        XmATTACH_FORM); i++;
        XtSetArg(args[i], XmNbottomAttachment,       XmATTACH_FORM); i++;
        XtSetArg(args[i], XmNscrollBarDisplayPolicy, XmAS_NEEDED); i++;
        XtSetArg(args[i], XmNvalue,                  "  " ); i++;
        XtSetArg(args[i], XmNwordWrap,               True); i++;
/*
        XtSetArg(args[i], XmNrows,                   20); i++;
        XtSetArg(args[i], XmNcolumns,                80); i++;
        XtSetArg(args[i], XmNscrollVertical,         True); i++;
        XtSetArg(args[i], XmNscrollHorizontal,       True); i++;
*/                 
        outputFile = XmCreateScrolledText (mainForm, "outputFile", args, i);

        XmChangeColor (outputFile,  myButtonBackground);

/*      scrollbar stuff */

        tempv=XtParent(outputFile);
        XtVaGetValues(tempv, XmNverticalScrollBar ,&temps ,NULL );
        XtVaGetValues(tempv, XmNhorizontalScrollBar ,&temph ,NULL );

        XtVaSetValues(tempv, XmNscrollBarPlacement, XmBOTTOM_RIGHT, NULL );
        XmChangeColor(tempv,  myBackground);
        XmChangeColor(temps,  myBackground);
        XmChangeColor(temph,  myBackground);
        XtVaSetValues(temps, XmNtroughColor, myButtonBackground, NULL);
        XtVaSetValues(temph, XmNtroughColor, myButtonBackground, NULL);
  
        XtManageChild (outputFile);
  
/*      set geometry and realize main window  */

        makegeom();      
        XtRealizeWidget (topLevel);       
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
        XEvent         myEvent;
        char           cString[100];
        String         myString;
        int            i, counter, across, stack, numS, numV;
        int            myPopupWidth , myPopupHeight, jcmd;
        int            labelButtonWidth, labelButtonHeight;
        int            labelHorizSep, labelVertSep, myPopupFontHeight;
        int            stringButtonWidth, lrMargin, tbMargin;
        int            buttonVertSep, buttonHorizSep, myBorder;
        char           varname[10], varvalue[81];
        Arg            args[20];
        Widget         mySeparator, myOkButton, myCancelButton;
        XmString       myLabel;
        XmFontList     myPopupFont;

/*      create display for integer, real  and string parameters */
  
        if( popupWidth > mainFormWidth) {
            myPopupWidth = popupWidth;
            myPopupFont = popupFont;
            myPopupFontHeight =popupFontHeight;
        } else {
            myPopupWidth = mainFormWidth;
            myPopupFont =  myFont;
            myPopupFontHeight =fontHeight;
        }
        numS= 0;
        for (i=0; i< *num  ; i++) {
           radioPtr[i] = nptr[i] - 1;
           if ( typ[i] == 'i' ) numS = numS + 1;
           if ( typ[i] == 'r' ) numS = numS + 1;
           if ( typ[i] == 's' ) numS = numS + 1;
        }
        numV = *num ; 
        radioPtr[numV] = nptr[numV] - 1;
        for (i=0; i< 80 * radioPtr[ numV ]; i++) {
           radioLabels[i] = labels[i];
           radioValues[i] = values[i];
        }
        for (i=0; i< 9 * numV  ; i++) {
           radioTable[i] = table[i];
        }

        counter = 0;
        across = 4;
        stack = 1 + numV - numS + (( numS > 0 ) ? 
                      (numS - 1)/across +1 : 0 );

        labelButtonWidth  = myPopupWidth *2/17 ;   
        labelButtonHeight = labelButtonWidth * 5/12;   
        if( labelButtonHeight <  5 * myPopupFontHeight /2 )
            labelButtonHeight = 5 * myPopupFontHeight /2 ;

        labelHorizSep = (myPopupWidth - 2 * across * labelButtonWidth) 
                / ( 2 * across + 1);
        labelVertSep  = labelHorizSep ;

        stringButtonWidth = (2 * across - 1 ) * labelButtonWidth 
            + ( 2 * across - 2 ) * labelHorizSep;
        myPopupHeight = (stack + 1) * labelButtonHeight + 
            (stack + 2) * labelVertSep;

        myBorder = (labelButtonHeight - myPopupFontHeight) / 2;
        if(myBorder > 2 ) myBorder = 2;
        lrMargin= labelButtonWidth/20;
        if( lrMargin >  5 ) lrMargin = 5;
        tbMargin= (labelButtonHeight - 4 - 2* myBorder - myPopupFontHeight) / 2;
        if( tbMargin <  0 ) tbMargin = 0;
        buttonHorizSep = ( myPopupWidth - 2 * labelButtonWidth )/3;
        buttonVertSep = labelButtonHeight/3;
        jcmd = *icmd -1;

/*      setup reset display */

        if(numV > 0) {  
            f2cstring(varvalue, &buttonLabels[15 * (*icmd -1)], 6);

            i = 0;
            XtSetArg(args[i], XmNwidth,    myPopupWidth); i++;
            XtSetArg(args[i], XmNheight,  myPopupHeight); i++;

            myPopup = XmCreateFormDialog (outputFile,
                varvalue, args, i);
            XmChangeColor (myPopup,  myBackground);

            for (i=0; i< numV ; i++) {

                if( i == numS ){
                    counter=0;
                    across=1;
                }
                if (counter == across) counter = 0;

                f2cstring(&varname[0], &table[9*i], 9);
                varNameBox[i] = XtVaCreateManagedWidget(varname,
                    xmLabelWidgetClass, myPopup,                
                    XmNwidth,  (radioPtr[i+1] > radioPtr[i])   ?         
                        labelButtonWidth -2 :  labelButtonWidth,
                    XmNheight,  (radioPtr[i+1] > radioPtr[i])   ?         
                        labelButtonHeight -2 :  labelButtonHeight,
                    XmNborderWidth,  (radioPtr[i+1] > radioPtr[i])   ?         
                          (Dimension) 1:    (Dimension) 0,
/*
                    XmNwidth,           labelButtonWidth,
                    XmNheight,          labelButtonHeight,
*/
                    XmNtopAttachment,   (i < across) ? 
                        XmATTACH_FORM :  XmATTACH_WIDGET,
                    XmNtopWidget,       (i < across) ? 
                        NULL : varNameBox[i-across],
                    XmNleftAttachment,  (counter == 0) ?
                        XmATTACH_FORM :  XmATTACH_WIDGET,
                    XmNleftWidget,      (counter == 0) ?
                        NULL : varValueBox[i-1],
                    XmNtopOffset,       labelVertSep,
                    XmNleftOffset,      labelHorizSep,
                    XmNfontList,        myPopupFont,
                    XmNrecomputeSize,    FALSE,
                    NULL);

                varRadioMark[i] = 0;
                XmChangeColor (varNameBox[i],  myBackground);
                if(radioPtr[i+1] > radioPtr[i]) {
                    XtAddEventHandler(varNameBox[i],
                            EnterWindowMask | LeaveWindowMask | ButtonPressMask,
                            (int) 0, (XtEventHandler)  radioEventHandle, 
                            (XtPointer) i);
                    XtVaSetValues(varNameBox[i], 
                        XmNborderColor, myButtonBackground,
                        NULL);
                }
                f2cstring(varvalue, &sval[80*i], 80);
                varValueBox[i] = XtVaCreateManagedWidget(varvalue,
                    xmTextFieldWidgetClass, myPopup,                
                    XmNwidth,           ( i < numS ) ? 
                        labelButtonWidth : stringButtonWidth ,
                    XmNheight,          labelButtonHeight ,
                    XmNhighlightThickness, myBorder, 
                    XmNmarginHeight,    tbMargin,
                    XmNmarginWidth,     lrMargin,
                    XmNtopAttachment,   (i < across) ? 
                        XmATTACH_FORM :  XmATTACH_WIDGET,
                    XmNtopWidget,       (i < across) ?
                        NULL : varValueBox[i-across],
                    XmNleftAttachment,  XmATTACH_WIDGET,
                    XmNleftWidget,      varNameBox[i],
                    XmNrightAttachment,  ( counter == across) ?
                        XmATTACH_FORM :  0 ,
                    XmNfontList,        myPopupFont,
                    XmNmaxLength,       80, 
                    XmNtopOffset,       labelVertSep,
                    XmNleftOffset,      labelHorizSep,
                    XmNvalue,           varvalue,
                    NULL);
 
                XmChangeColor (varValueBox[i],  myButtonBackground);
                counter++;
            }

            mySeparator = XtVaCreateManagedWidget("myseparator",
                xmSeparatorWidgetClass, myPopup,                
                XmNorientation,         XmHORIZONTAL,
                XmNtopAttachment,       XmATTACH_WIDGET,
                XmNtopWidget,           varNameBox[numV-1],
                XmNleftAttachment,      XmATTACH_FORM,
                XmNrightAttachment,     XmATTACH_FORM,
                NULL); 
            XmChangeColor (mySeparator,  myBackground);

/*      ok button */

            f2cstring(cString, &buttonLabels[ 15 * jcmd ], 8);
            myLabel = XmStringCreateLocalized(cString);
            myOkButton = XtVaCreateManagedWidget(cString,
                xmPushButtonWidgetClass, myPopup,
                XmNlabelString,         myLabel,
                XmNtopAttachment,       XmATTACH_WIDGET,
                XmNtopWidget,           mySeparator,
                XmNleftAttachment,      XmATTACH_FORM,
                XmNwidth,               labelButtonWidth,
                XmNheight,              labelButtonHeight,
                XmNfontList,            myPopupFont,          
                XmNleftOffset,          buttonHorizSep, 
                XmNtopOffset,           buttonVertSep,
                XmNshowAsDefault,       True,
                XmNdefaultButtonShadowThickness, 1, 
                NULL); 
            XmStringFree(myLabel);
            XmChangeColor (myOkButton,  myBackground);


            myString = "cancel";
            myLabel = XmStringCreateLocalized(myString);
            myCancelButton = XtVaCreateManagedWidget(myString,
                xmPushButtonWidgetClass, myPopup,
                XmNlabelString,         myLabel,
                XmNtopAttachment,       XmATTACH_WIDGET,
                XmNtopWidget,           mySeparator,
                XmNrightAttachment,     XmATTACH_FORM,
                XmNwidth,               labelButtonWidth,
                XmNheight,              labelButtonHeight,
                XmNfontList,            myPopupFont,          
                XmNrightOffset,         buttonHorizSep, 
                XmNtopOffset,           buttonVertSep,
                XmNshowAsDefault,       False,
                XmNdefaultButtonShadowThickness, 1, 
                NULL); 
            XmStringFree(myLabel);
            XmChangeColor (myCancelButton,  myBackground);

/*  set up callback   */

            XtAddCallback (myOkButton, XmNactivateCallback,
                (XtCallbackProc)btnCallback,(XtPointer) jcmd );
            XtAddCallback (myCancelButton, XmNactivateCallback,
                (XtCallbackProc)cnclCallback,(XtPointer)numV);
            XtManageChild (myPopup);
            XtPopup (XtParent (myPopup), XtGrabNone);
        }

/*   turn control over to user */

        commandBuffer[0]='\0';
        commandSwitch = -1;
        cancelSwitch = 0;
        while (commandSwitch == -1) {
                XtAppNextEvent(myAppCon,&myEvent);
                XtDispatchEvent(&myEvent);
        }
        c2fstring(commandBuffer,  &list[0], 80);
  
/*   update sval array and mark entries which are updated */

        if(cancelSwitch == 0) {
            for (i = 0 ; i < numV ; i++) {
                myString =XmTextFieldGetString (varValueBox[i]);
                f2cstring(varvalue, &sval[80*i], 80);
                if(strcmp( (char*)varvalue, myString) != 0){
                    mark[i]=1;
                    c2fstring(myString, &sval[80*i] ,80);    
                } else {
                    mark[i]=0;
                }
            }
        } else {
            for (i = 0 ; i < numV ; i++) {
                mark[i]=0;
            }
        }
        if(numV > 0)  {
            for (i = 0 ; i < numV ; i++) {
                if ( varRadioMark[ i ] == 1 ) {
                    XtDestroyWidget(varRadioBox[i]);
                    varRadioMark[i] = 0;
                    XFlush(myDisplay);
                }
            }
            XtDestroyWidget(myPopup);
            XFlush(myDisplay);
        }
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
        XEvent         myEvent;
        String         myString;
        char           varvalue[81];
        XmString       myOkLabel, myLabel, newFilter;
        Widget         tempw, tempv, temps, temph;
        int            i, jcmd, myPopupWidth, myPopupHeight;
        int            myPopupScrollBarWidth;
        Arg            args[20];
        XmFontList     myPopupFont;

/*      create file selection popup */

        if( popupWidth > mainFormWidth) {
            myPopupWidth = popupWidth * 2 / 3;
            myPopupFont = popupFont;
        } else {
            myPopupWidth = mainFormWidth * 2 / 3;
            myPopupFont = myFont;
        }
        jcmd = *icmd -1;
        myPopupHeight = myPopupWidth * 3 / 4;
        myPopupScrollBarWidth =  myPopupWidth * 3 / 100;

        f2cstring(varvalue, &buttonLabels[ 15 * jcmd ], 8);
        myOkLabel = XmStringCreateLocalized(varvalue);
        f2cstring(varvalue, &tval[0], 9);
        myLabel = XmStringCreateLocalized(varvalue);
        f2cstring(varvalue, &buttonLabels[ 15 * jcmd ], 6);

        i = 0;
        XtSetArg(args[i], XmNwidth,                 myPopupWidth); i++;
        XtSetArg(args[i], XmNheight,                myPopupHeight); i++;
        XtSetArg(args[i], XmNresizePolicy,          XmRESIZE_NONE); i++;
        XtSetArg(args[i], XmNnoResize,              True); i++;
        XtSetArg(args[i], XmNlabelFontList,         myPopupFont); i++;
        XtSetArg(args[i], XmNbuttonFontList,        myPopupFont); i++;
        XtSetArg(args[i], XmNtextFontList,          myPopupFont); i++;
        XtSetArg(args[i], XmNokLabelString,         myOkLabel); i++;
        XtSetArg(args[i], XmNselectionLabelString,  myLabel); i++;

        myPopup = XmCreateFileSelectionDialog (outputFile,
            varvalue, args, i);
        XmStringFree(myLabel);
        XmStringFree(myOkLabel);
        XmChangeColor (myPopup, myBackground);

/*   remove help button */
  
        tempw = XtNameToWidget(myPopup,"Help");
        XtUnmanageChild(tempw);
  
/*   the rest of the buttons */

        tempw = XtNameToWidget(myPopup,"OK");
        XtVaSetValues(tempw, XmNfillOnArm, FALSE, NULL);
        tempw = XtNameToWidget(myPopup,"Apply");
        XtVaSetValues(tempw, XmNfillOnArm, FALSE, NULL);
        tempw = XtNameToWidget(myPopup,"Cancel");
        XtVaSetValues(tempw, XmNfillOnArm, FALSE, NULL);
            

/*  directory list widgets */
  
        tempw = XtNameToWidget(myPopup,"DirListSW.DirList");
        tempv = XtNameToWidget(myPopup,"DirListSW");
  
        XtVaGetValues(tempv, XmNverticalScrollBar ,&temps ,NULL );
        XtVaGetValues(tempv, XmNhorizontalScrollBar ,&temph ,NULL );

        XmChangeColor (tempw,  myButtonBackground);
        XmChangeColor (tempv,  myBackground);
        XmChangeColor (temps,  myBackground);
        XmChangeColor (temph,  myBackground);
  
        XtVaSetValues(tempv, 
            XmNscrollBarPlacement,     XmBOTTOM_RIGHT, 
            NULL);
        XtVaSetValues(temps, 
            XmNwidth,                 myPopupScrollBarWidth,
            XmNtroughColor,           myButtonBackground, 
            NULL);
        XtVaSetValues(temph, 
            XmNheight,                myPopupScrollBarWidth,          
            XmNtroughColor,           myButtonBackground, 
            NULL);
  
/*  file list widgets      */
  
        tempw = XtNameToWidget(myPopup,"ItemsListSW.ItemsList");
        tempv = XtNameToWidget(myPopup,"ItemsListSW");
        XtVaGetValues(tempv, XmNverticalScrollBar ,&temps ,NULL );
        XtVaGetValues(tempv, XmNhorizontalScrollBar ,&temph ,NULL );

        XmChangeColor (tempw,  myButtonBackground);
        XmChangeColor (tempv,  myBackground);
        XmChangeColor (temps,  myBackground);
        XmChangeColor (temph,  myBackground);
  
        XtVaSetValues(tempv, 
            XmNscrollBarPlacement,     XmBOTTOM_RIGHT, 
            NULL);
        XtVaSetValues(temps, 
            XmNwidth,                 myPopupScrollBarWidth,
            XmNtroughColor,           myButtonBackground, 
            NULL);
        XtVaSetValues(temph, 
            XmNheight,                myPopupScrollBarWidth,          
            XmNtroughColor,           myButtonBackground, 
            NULL);
  
/*   filter text widgets   */
  
        tempw = XtNameToWidget(myPopup,"FilterText");
        XmChangeColor (tempw, myButtonBackground);

        if(filterSwitch[jcmd] == 1) {
            newFilter = XmStringCreateLocalized(myFilter[jcmd]);
            XmFileSelectionDoSearch(myPopup, newFilter);
        }
  
/*  selection text widgets   */

        tempw = XtNameToWidget(myPopup,"Text");
        XmChangeColor (tempw, myButtonBackground);
        f2cstring(varvalue, &sval[0], 80);
        XmTextFieldSetString (tempw, varvalue);    

/*  set up callback   */

        XtAddCallback (myPopup, XmNokCallback,
            (XtCallbackProc)btnCallback,(XtPointer)jcmd );
        XtAddCallback (myPopup, XmNcancelCallback,
            (XtCallbackProc)cnclCallback,(XtPointer)0);


        XtManageChild (myPopup);
        XtPopup (XtParent (myPopup), XtGrabNone);

/*   turn control over to user */

        commandBuffer[0]='\0';
        commandSwitch = -1;
        cancelSwitch = 0;
        while (commandSwitch == -1) {
                XtAppNextEvent(myAppCon,&myEvent);
                XtDispatchEvent(&myEvent);
        }
        c2fstring(commandBuffer,  &list[0], 80);
  
/*   get filename, save the current filter */

        if(cancelSwitch == 0 ) {
            myString= XmTextGetString(XtNameToWidget(myPopup,"Text"));
            f2cstring(varvalue, &sval[0], 80);
            if(strcmp( (char*)varvalue, myString) != 0){
                c2fstring(myString, &sval[0] ,80);    
            }
            myFilter[jcmd] = XmTextGetString(
                XtNameToWidget(myPopup,"FilterText"));
            filterSwitch[jcmd] = 1;
        }
        XtDestroyWidget(myPopup);
        XFlush(myDisplay);
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
        XEvent         myEvent;

        commandBuffer[0]='\0';
        commandSwitch = -1;
        while (commandSwitch == -1) {
            XtAppNextEvent(myAppCon,&myEvent);
            XtDispatchEvent(&myEvent);
        }
        c2fstring(commandBuffer,  &list[0], 80);
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
        XEvent         myEvent;
        char           cString[100];
        String         myString;
        int            i, myPopupWidth , myPopupHeight, jcmd;
        int            labelButtonWidth, labelButtonHeight;
        int            myPopupFontHeight;
        int            buttonVertSep, buttonHorizSep;
        Arg            args[20];
        Widget         myOkButton;
        XmString       myLabel;
        XmFontList     myPopupFont;

/*      creat continue button popup */
  
        if( popupWidth > mainFormWidth) {
            myPopupWidth = popupWidth;
            myPopupFont = popupFont;
            myPopupFontHeight =popupFontHeight;
        } else {
            myPopupWidth = mainFormWidth;
            myPopupFont =  myFont;
            myPopupFontHeight =fontHeight;
        }
        labelButtonWidth  = myPopupWidth *5/34 ;   
        labelButtonHeight = 8 + myPopupWidth /20 ;    
        if( labelButtonHeight <  5 * myPopupFontHeight /2 )
            labelButtonHeight = 5 * myPopupFontHeight /2 ;

        buttonHorizSep = labelButtonWidth/2; 
        buttonVertSep = labelButtonHeight/2;
        myPopupHeight =  labelButtonHeight +  3 * buttonVertSep;
        myPopupWidth =  labelButtonWidth +  3 * buttonHorizSep;
        jcmd =  -1;

/*      continue button */
                       
        myString = "continue";
        myLabel = XmStringCreateLocalized(myString);

        i = 0;
        XtSetArg(args[i], XmNwidth,    myPopupWidth); i++;
        XtSetArg(args[i], XmNheight,  myPopupHeight); i++;

        myPopup = XmCreateFormDialog (outputFile,
            myString, args, i);
        XmChangeColor (myPopup,  myBackground);

        myOkButton = XtVaCreateManagedWidget(cString,
            xmPushButtonWidgetClass, myPopup,
            XmNlabelString,         myLabel,
            XmNtopAttachment,       XmATTACH_FORM,
            XmNleftAttachment,      XmATTACH_FORM,
            XmNbottomAttachment,    XmATTACH_FORM,
            XmNrightAttachment,     XmATTACH_FORM,
            XmNwidth,               labelButtonWidth,
            XmNheight,              labelButtonHeight,
            XmNfontList,            myPopupFont,          
            XmNleftOffset,          buttonHorizSep, 
            XmNrightOffset,         buttonHorizSep, 
            XmNtopOffset,           buttonVertSep,
            XmNbottomOffset,        buttonVertSep,
            XmNshowAsDefault,       True,
            XmNdefaultButtonShadowThickness, 1, 
            NULL); 
        XmStringFree(myLabel);
        XmChangeColor (myOkButton,  myBackground);

/*  set up callback   */

        XtAddCallback (myOkButton, XmNactivateCallback,
            (XtCallbackProc)contCallback,(XtPointer) jcmd );
        XtManageChild (myPopup);
        XtPopup (XtParent (myPopup), XtGrabNone);

/*   turn control over to user */

        commandSwitch = -1;
        while (commandSwitch == -1) {
                XtAppNextEvent(myAppCon,&myEvent);
                XtDispatchEvent(&myEvent);
        }

        XtDestroyWidget(myPopup);
        XFlush(myDisplay);
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
        char list[82];
                               
        list[0]='\n';        
        f2cstring(&list[1], &line[0], 80);

        XmTextSetInsertionPosition (outputFile, myPosition );
        XmTextInsert (outputFile, myPosition, &list[0] );

        myPosition = myPosition + strlen(list);

        XmTextShowPosition (outputFile, myPosition);
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void  cmdCallback( w , clientData, callData )
{

        String            myString;

        XmCommandCallbackStruct *cbs = 
            (XmCommandCallbackStruct *) callData;
        XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &myString);
        strcpy(commandBuffer,myString);
        commandSwitch =0;
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void  btnCallback( w , clientData, callData )
{
        int jcmd = (int) clientData;

        commandBuffer[0]=buttonLabels[15 * jcmd + 7];
        commandBuffer[1]='\0';
        commandSwitch =0;
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void  cnclCallback( w , clientData, callData )
{
        int numV = (int) clientData;
        Widget tempw = (Widget) w;
        int i;

        for (i = 0 ; i < numV ; i++) {
            if ( varRadioMark[ i ] == 1 ) {
                XtDestroyWidget(varRadioBox[i]);
                varRadioMark[i] = 0;
                XFlush(myDisplay);
            }
        }
        cancelSwitch = 1;
        XtUnmanageChild (tempw);     
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void  contCallback( w , clientData, callData )
{
        commandSwitch =0;
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void resizeEventHandle( Widget w, int number, XEvent *event, 
        Boolean *garbage)
{
/*      the user has resized the main window */

        switch (event->type) {
        case ConfigureNotify:
            XtUnrealizeWidget (topLevel);       
            makegeom();      
            XtRealizeWidget (topLevel);       
            break;
        default:
            break;
        } /* end switch */
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void radioEventHandle( Widget w, int number, XEvent *event, 
        Boolean *garbage)
{
        int               i,ivar;

        switch (event->type) {

/*      pop up a radio button display */

        case ButtonPress:
            i = event->xbutton.button;
            if(i == 1) {
                ivar = number + 1;
                xradio( &ivar );
            } 
            break;

/*      a reset value window is entered/exited by the pointer */

/*
        case EnterNotify:
                XmChangeColor (w,  myForeground);
                break;
        case LeaveNotify:
                XmChangeColor (w,  myBackground);
                break;
*/
        default:
            break;
        } /* end switch */
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void cmdEventHandle( Widget w, int number, XEvent *event, 
        Boolean *garbage)
{
        int               i;

/*      a command button has been pressed */

        i = event->xbutton.button;
        if(i == 1) {
            commandBuffer[0]=buttonLabels[15 * number + 7];
        } else {
            commandBuffer[0]=buttonLabels[15 * number + 7] - 32;
        }
        commandBuffer[1]='\0';
        commandSwitch =0;
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

/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void makelogo( void )
{
unsigned int myIconSize = 40;
int theta = 3;
int x1,y1,x2,y2,x3,y3;


        trianglePixmap = XCreatePixmap ( myDisplay,
                RootWindowOfScreen(myScreen), myIconSize, myIconSize,
                DefaultDepthOfScreen(myScreen));
        XSetForeground(myDisplay,DefaultGCOfScreen(myScreen),
                myButtonBackground);
        XFillRectangle(myDisplay, trianglePixmap,
                DefaultGCOfScreen(myScreen),0,0,myIconSize,myIconSize);
  
        x1 = theta;
        x2 = myIconSize/2;
        x3 = myIconSize-x1;
        y2= (x3 - x1) * .866;
        y2= (myIconSize - y2)/2;
        y1= myIconSize-y2;
        y3=y1;
  
        XSetForeground(myDisplay, DefaultGCOfScreen(myScreen),
                myButtonForeground);
        XDrawLine( myDisplay, trianglePixmap, DefaultGCOfScreen(myScreen),
                x1, y1, x2, y2);
        XDrawLine( myDisplay, trianglePixmap, DefaultGCOfScreen(myScreen),
                x2, y2, x3, y3);
        XDrawLine( myDisplay, trianglePixmap, DefaultGCOfScreen(myScreen),
                x3, y3, x1, y1);
        XDrawLine( myDisplay, trianglePixmap, DefaultGCOfScreen(myScreen),
                (x1 + x3)/2, (y1 + y3)/2, (x1 + x2)/2, (y1 + y2)/2);
        XDrawLine( myDisplay, trianglePixmap, DefaultGCOfScreen(myScreen),
                (x2 + x1)/2, (y2 + y1)/2, (x2 + x3)/2, (y2 + y3)/2);
        XDrawLine( myDisplay, trianglePixmap, DefaultGCOfScreen(myScreen),
                (x3 + x2)/2, (y3 + y2)/2, (x3 + x1)/2, (y3 + y1)/2);
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void makecolors()
{
        XColor          color;
        int             Status1, Status2;

        myBackground = BlackPixelOfScreen(myScreen);
        myButtonBackground = BlackPixelOfScreen(myScreen);

        Status1 = XParseColor( myDisplay, 
            DefaultColormapOfScreen(myScreen), bgclr, &color) ;
        if(Status1 != 0 ) {
            Status2 =  XAllocColor(myDisplay, 
                DefaultColormapOfScreen(myScreen),&color ); 
            if(Status2 != 0 ) {
                myBackground = color.pixel;
            }
        }

        Status1 = XParseColor( myDisplay, 
            DefaultColormapOfScreen(myScreen), btnbg, &color) ;
        if(Status1 != 0 ) {
            Status2 =  XAllocColor(myDisplay, 
                DefaultColormapOfScreen(myScreen),&color ); 
            if(Status2 != 0 ) {
                myButtonBackground = color.pixel;
            }
        }

        XmGetColors ( myScreen, 
                DefaultColormapOfScreen(myScreen),
                myBackground, &myForeground, 
                &myTopShadow, &myBottomShadow, &mySelect);

        XmGetColors ( myScreen, 
                DefaultColormapOfScreen(myScreen),
                myButtonBackground, &myButtonForeground, 
                &myTopShadow, &myBottomShadow, &mySelect);
}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void makegeom( void )
{
        char            newfont[100];
        char            *fontname ;
        int             i,across, stack;
        Dimension       winX, winY;
        XmFontListEntry myFontEntry;
        Widget          tempw, temps, tempv, temph;

        if( ifirst == 1 ) {
            mainFormWidth = XWidthOfScreen(myScreen);
            mainFormHeight = XHeightOfScreen(myScreen);
            if( 2 * mainFormWidth < 3 * mainFormHeight) {
                    minWidth = mainFormWidth / 3 ;
                    minHeight = (minWidth * 2) / 3;
                    mainFormWidth = (mainFormWidth * overallScale) / 100;
                    mainFormHeight = (mainFormWidth * 2) / 3;
            }
            else {
                    minHeight = mainFormHeight / 3 ;
                    minWidth = (minHeight * 3) / 2;
                    mainFormHeight = (mainFormHeight * overallScale) / 100;
                    mainFormWidth = (mainFormHeight * 3) / 2;
            }
            popupWidth = mainFormWidth;
        } else {

            XtVaGetValues(topLevel, 
                XtNwidth,          &winX, 
                XtNheight,         &winY, 
                NULL);
            if( winX < minWidth ) winX = minWidth;
            if( winY < minHeight ) winY = minHeight;

            if( 2 * winX  < 3 * winY ) {
                    winY = (winX * 2) / 3 ;
            }
            else {
                    winX = (winY * 3) / 2 ;
            }

            mainFormWidth =  winX;
            mainFormHeight =  winY;
        }

        defaultBorder = 1;
        defaultSep = mainFormWidth / 200;
        if (defaultSep > 6 ) defaultSep = 6;      
        if (defaultSep <= 0 ) {
                defaultSep = 0;      
                defaultBorder = 0;
        }

/*      main form geometry */

        across = 8;
        stack = lastNcmd/across +1;
        buttonWidth  = mainFormWidth *2/17 ;   
        buttonWidth  = (mainFormWidth - (across + 1) * defaultSep )/ across;
        buttonHeight = buttonWidth * 5/12;

        commandFieldHeight = buttonHeight * 7/4;
        mpiLabelHeight = buttonHeight;
        mpiLabelWidth = buttonHeight;
        mpiBorder= (commandFieldHeight - mpiLabelHeight) /2;
        commandFieldWidth = mainFormWidth -  2 * defaultBorder   
            - mpiLabelWidth- 2 * mpiBorder;
        outputFileWidth = mainFormWidth -  4 * defaultBorder;
        outputFileHeight = mainFormHeight - commandFieldHeight 
                - stack * buttonHeight - (stack + 2) * defaultSep;

        scrollBarWidth = mainFormWidth/50;
        myHorizSep = (mainFormWidth - across * buttonWidth) / (across + 1);
        myVertSep  = (mainFormHeight -outputFileHeight - commandFieldHeight
                 - stack * buttonHeight ) / (stack + 1) ;

/*  try scalable font */

        fontHeight = (buttonHeight * 40) / 100;
        if (fontHeight > buttonHeight - 4) 
                fontHeight = buttonHeight - 4;
        if( fontHeight < 5 ) fontHeight = 5;     

        fontname = "-*-courier-bold-r-*-*-00-*-*-*-*-*-*-*";  
        strcpy(newfont,fontname);
        newfont[22] = 48 + fontHeight / 10;
        newfont[23] = 48 + fontHeight - (fontHeight / 10) * 10;
        myFontEntry = XmFontListEntryLoad(myDisplay, newfont ,
                XmFONT_IS_FONT, XmFONTLIST_DEFAULT_TAG);
        myFont= XmFontListAppendEntry( NULL, myFontEntry);
        if( ifirst == 1 ) {
            ifirst = 0;
            popupFont = myFont;
            popupFontHeight = fontHeight;
        }
          
/*      the main form and command and command window form */

        XtVaSetValues(topLevel,
                XmNwidth,           mainFormWidth, 
                XmNheight,          mainFormHeight,
                XmNborderWidth,     defaultBorder,
                NULL);

        XtVaSetValues(mainForm,
                XmNwidth,           mainFormWidth, 
                XmNheight,          mainFormHeight,
                NULL);

/*      the rest of the command buttons   */

        for (i=0; i< lastNcmd; i++) {
            XtVaSetValues(myButton[i],
                XmNwidth,           buttonWidth,
                XmNheight,          buttonHeight,
                XmNfontList,        myFont,          
                XmNtopOffset,       myVertSep,
                XmNleftOffset,      myHorizSep,
                NULL);
        }

/*      command    */

        XtVaSetValues(mainFormSep[0],
                XmNtopOffset,       myVertSep,
                NULL); 
        XtVaSetValues(mainFormSep[1],
                XmNtopOffset,       myVertSep,
                NULL); 
        XtVaSetValues(commandField,
                XmNwidth,           commandFieldWidth,
                XmNheight,          commandFieldHeight,
                XmNmarginWidth,     defaultBorder,
                XmNmarginHeight,    defaultBorder,
                NULL); 
  
        tempw =XmCommandGetChild(commandField,XmDIALOG_COMMAND_TEXT);
        XtVaSetValues(tempw, XmNfontList, myFont, NULL);
        temps =XmCommandGetChild(commandField,XmDIALOG_PROMPT_LABEL);
        XtVaSetValues(temps, XmNfontList, myFont, NULL);

/* mpi label */

        XtVaSetValues(mpiLabel,
                XmNfontList,        myFont,
                XmNwidth,           mpiLabelWidth,
                XmNheight,          mpiLabelHeight,
                XmNborderWidth,     mpiBorder,
                NULL); 

/*      output file */
                                 
        XtVaSetValues(outputFile,
            XmNfontList,                   myFont,
            XmNwidth,                      outputFileWidth,
            XmNheight,                     outputFileHeight,
            XmNborderWidth,                0,
            NULL);
                    
        tempv=XtParent(outputFile);
        XtVaGetValues(tempv, XmNverticalScrollBar ,&temps ,NULL );
        XtVaGetValues(tempv, XmNhorizontalScrollBar ,&temph ,NULL );
        XtVaSetValues(temps, XmNwidth, scrollBarWidth, NULL);
        XtVaSetValues(temph, XmNheight, scrollBarWidth, NULL);
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
        int            i;
        XColor         color;

        curId = *id ;
        shift = 256 * curId;

/*      creat a popup if necessary   */

        if( ! graphicsPopup[curId] ) mkgr ( &curId );

/*      initialize drawing area, pixmap, and colors   */

        popgr(&curId);

        for (i = 0; i < lastNcolor[curId]; i++) {
            if(ColorStatus[shift + i] != 0 ) {
                XFreeColors(myDisplay,
                    DefaultColormapOfScreen(myScreen),
                    &ColorMapPixels[shift + i], 1 , 0);
            } 
        } 
        lastNcolor[curId] = *ncolor ;
        for (i = 0; i < *ncolor; i++) {
            color.red = (unsigned short) rgb[3 * i];
            color.green = (unsigned short) rgb[3 * i + 1];
            color.blue = (unsigned short) rgb[3 * i + 2];
            color.flags = DoRed | DoGreen | DoBlue;
            ColorStatus[shift+i] =  XAllocColor(myDisplay, 
                      DefaultColormapOfScreen(myScreen), 
                      &color );
            if ( ColorStatus[shift + i] == 0 ) {
                ColorMapPixels[shift + i] = 
                    WhitePixelOfScreen(myScreen);   
            } else {
                ColorMapPixels[shift + i] = color.pixel;
            } 
        }  
        *width = graphicsAreaWidth[ curId];
        *height = graphicsAreaHeight[ curId];
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
        int ix, iy, idx, ic, jj, xshift, px, py;

/*      create XImage and display it */

        jj = *ishift;
        xshift = *nx;
        for (ix = 0; ix < *nx; ix++) {
            for (iy = 0; iy < *ny; iy++) {
               idx=ix+iy*xshift;
               px=ix;
               py=*ny -iy -1;
               ic=myimage[idx] - (myimage[idx]/jj)*jj + shift;
               XPutPixel( graphicsImage[curId], px, py,
                  ColorMapPixels[ic]);
            }
        }
        XPutImage( myDisplay, XtWindow(graphicsArea[curId]), 
            DefaultGCOfScreen(myScreen), graphicsImage[curId], 0, 0,
            0, 0, graphicsAreaWidth[curId], graphicsAreaHeight[curId]);

}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void  expCallback( w , clientData, callData )
{
        int k = (int) clientData;

/*      expose callback for graphics windows */

        XPutImage( myDisplay, XtWindow(graphicsArea[k]), 
            DefaultGCOfScreen(myScreen), graphicsImage[k], 0, 0,
            0, 0, graphicsAreaWidth[k], graphicsAreaHeight[k]);

}
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void  rszCallback( w , clientData, callData )
{
        Dimension       winX, winY;
        int k = (int) clientData;
        Position       pX, pY;

/*      resize callback for graphics windows    */

        XtVaGetValues(graphicsPopup[k],
            XmNwidth,    &winX,
            XmNheight,   &winY,
            XmNx,        &pX,
            XmNy,        &pY,
            NULL);
        if(winX <(Dimension)minWidth ) winX = (Dimension) minWidth;
        if(winY <(Dimension)minHeight ) winY = (Dimension) minHeight;
        if(winX >(Dimension)maxWidth ) winX = (Dimension) maxWidth;
        if(winY >(Dimension)maxHeight ) winY = (Dimension) maxHeight;
        if( 2 * winX < 3 * winY ) {
            winX = ( winX / 3) * 3;
            winY = ( winX * 2) / 3;
        } else {
            winY = ( winY / 2) * 2;
            winX = ( winY * 3) / 2;
        }
        graphicsAreaWidth[k] = winX ;      
        graphicsAreaHeight[k] = winY;      
        graphicsPositionX[k] = pX;      
        graphicsPositionY[k] = pY;      

        XtDestroyWidget(graphicsPopup[k]);
        XDestroyImage ( graphicsImage[k]);
    
        mkgr ( &k );
        popgr( &k);
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
        int          k, knum;
        Dimension      winX, winY;

/*      initialize num graphics windows (could be incorporated in xwinit) */

        winX = (Dimension)  XWidthOfScreen(myScreen);
        winY = (Dimension)  XHeightOfScreen(myScreen);
        if( 2 * winX < 3 * winY ) {
            winX = ( winX * overallScale) / 100;
            winX = ( winX / 3) * 3;
            winY = ( winX * 2) / 3;
        } else {
            winY = ( winY * overallScale) / 100;
            winY = ( winY / 2) * 2;
            winX = ( winY * 3) / 2;
        }

        for (k = 0; k < maxgraph ; k++)  {
            graphicsAreaWidth[k]=winX;
            graphicsAreaHeight[k]=winY;
            graphicsPositionX[k]= (Position)0;
            graphicsPositionY[k]= (Position)0;
        } 
        knum = *num;
        if (knum > maxgraph) knum=maxgraph;
        for (k = 0; k < knum; k++)  {
            mkgr ( &k );
            popgr ( &k );
        } 
    }
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void mkgr(int *id)
    {
        int            i, j, k;
        Arg            args[20];
        String         myLabel[10] = {"graphics window 0",
                        "graphics window 1","graphics window 2",
                        "graphics window 3","graphics window 4",
                        "graphics window 5","graphics window 6",
                        "graphics window 7","graphics window 8",
                        "graphics window 9"};

/*     create graphics popup number id   */

        k =  *id;
        i = 0;
        XtSetArg(args[i], XmNwidth,  graphicsAreaWidth[k]); i++;
        XtSetArg(args[i], XmNheight, graphicsAreaHeight[k]); i++;
        XtSetArg(args[i], XmNx,      graphicsPositionX[k]); i++;
        XtSetArg(args[i], XmNy,      graphicsPositionY[k]); i++;
        XtSetArg(args[i], XmNdefaultPosition , FALSE); i++;
                     
        graphicsPopup[k] = XmCreateFormDialog (outputFile,
            myLabel[k], args, i);      
        XmChangeColor (graphicsPopup[k],  myBackground);

        graphicsArea[k] = XtVaCreateManagedWidget("graphicsArea",
            xmDrawingAreaWidgetClass,     graphicsPopup[k],
            XmNwidth,                     graphicsAreaWidth[k], 
            XmNheight,                    graphicsAreaHeight[k],
            XmNforeground,                BlackPixelOfScreen(myScreen), 
            XmNbackground,                WhitePixelOfScreen(myScreen),
            XmNborderColor,               myBackground,
            XmNtopAttachment,             XmATTACH_FORM,
            XmNleftAttachment,            XmATTACH_FORM,
            XmNrightAttachment,           XmATTACH_FORM,
            XmNbottomAttachment,          XmATTACH_FORM,
            NULL);

        XtAddCallback (graphicsArea[k] , XmNexposeCallback, 
            (XtCallbackProc)expCallback,(XtPointer) k );
  
        XtAddCallback (graphicsArea[k], XmNresizeCallback, 
            (XtCallbackProc)rszCallback,(XtPointer) k );

       graphicsImage[k] = XCreateImage (myDisplay, 
            DefaultVisualOfScreen(myScreen),
            DefaultDepthOfScreen(myScreen),
            ZPixmap, 
            0, 0,
            graphicsAreaWidth[k], 
            graphicsAreaHeight[k],
            32, 0);

       if (graphicsImage[k] != 0) graphicsImage[k]->data = 
            malloc(graphicsImage[k]->bytes_per_line * graphicsAreaHeight[k]);  
    
        for (i = 0; i < graphicsAreaWidth[k]; i++)  {
            for (j = 0; j < graphicsAreaHeight[k]; j++)  {
               XPutPixel( graphicsImage[k], i, j,
                  WhitePixelOfScreen(myScreen));
            }
        }
    }
/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void popgr(int *id)
    {
        int            k;

/*     initialize graphics popup number id for a new picture   */

        k =  *id;
        XtManageChild (graphicsPopup[k]);
        XtPopup (XtParent (graphicsPopup[k]), XtGrabNone);
        XClearWindow(myDisplay, XtWindow(graphicsArea[k]));     
  
    }

/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void xradio( int *ivar)
        
{
        String         myString;
        int            i, slen, mxlen, which;
        int            myPopupWidth, myPopupHeight, myPopupFontHeight;
        int            checkBoxSpace;
        char           varvalue[81], togglename[81];
        Arg            args[20];
        XmFontList     myPopupFont;

/*      create display for integer, real  and string parameters */
  

        if( popupWidth > mainFormWidth) {
            myPopupWidth = popupWidth;
            myPopupFont = popupFont;
            myPopupFontHeight =popupFontHeight;
        } else {
            myPopupWidth = mainFormWidth;
            myPopupFont =  myFont;
            myPopupFontHeight =fontHeight;
        }
        if ( varRadioMark[ *ivar -1] == 1 ) {
            XtDestroyWidget(varRadioBox[ *ivar -1]);
            XFlush(myDisplay);
            varRadioMark[ *ivar -1] = 0;
        }
        myString =XmTextFieldGetString (varValueBox[ *ivar - 1]);
        mxlen=0;
        which=radioPtr[ *ivar -1];
        for (i=radioPtr[ *ivar -1];  i< radioPtr[ *ivar] ; i++) {
            f2cstring(&togglename[0], &radioLabels[80*i], 80);
            f2cstring(varvalue, &radioValues[80*i], 80);
            if(strcmp( (char*)varvalue, myString) == 0 ) which = i;
            slen = (int) strlen(togglename);
            if (slen > mxlen) mxlen = slen;
        }

        checkBoxSpace = 3;
        myPopupWidth=myPopupFontHeight * mxlen + 2 * checkBoxSpace ;
        myPopupHeight = (radioPtr[ *ivar] -radioPtr[ *ivar -1] ) 
           * ( myPopupFontHeight * 2 + 1 + checkBoxSpace);

/*      setup reset display */

        f2cstring(varvalue, &radioTable[9*(*ivar -1)], 6);

        i = 0;
/*
        XtSetArg(args[i], XmNwidth,    myPopupWidth); i++;
        XtSetArg(args[i], XmNheight,  myPopupHeight); i++;

*/
        varRadioBox[ *ivar-1] = XmCreateFormDialog (varValueBox[ *ivar -1],
            varvalue, args, i);
        XmChangeColor (varRadioBox[ *ivar-1],  myBackground);

        varCheckBox[ *ivar - 1]  = XtVaCreateManagedWidget("checkbox",
                xmRowColumnWidgetClass, varRadioBox[ *ivar -1],      
/*
                XmNwidth,          myPopupWidth,
                XmNheight,         myPopupHeight,
*/
                XmNnavigationType, XmTAB_GROUP,
                XmNrowColumnType,  XmWORK_AREA,
                XmNradioBehavior,  TRUE,
                XmNradioAlwaysOne,  TRUE,
                XmNtraversalOn,    TRUE,
                XmNtopAttachment,  XmATTACH_FORM,
                XmNleftAttachment, XmATTACH_FORM,
                XmNspacing,        checkBoxSpace,
                NULL);
        XmChangeColor (varCheckBox[ *ivar -1],  myBackground);

        for (i=radioPtr[ *ivar -1];  i< radioPtr[ *ivar] ; i++) {

            f2cstring(&togglename[0], &radioLabels[80*i], 80);
            varToggle[i] = XtVaCreateManagedWidget(togglename,
                xmToggleButtonWidgetClass, varCheckBox[ *ivar - 1],    
                XmNfontList,        myPopupFont,
                XmNindicatorType,   XmONE_OF_MANY,
                XmNset,            ( which == i) ? TRUE : FALSE ,
                NULL);
            XmChangeColor(varToggle[i],  myBackground);
            XtVaSetValues(varToggle[i],  
                XmNhighlightOnEnter, FALSE,
                XmNhighlightColor, myBackground,
            NULL);
            XtAddCallback (varToggle[i], XmNvalueChangedCallback,
                 (XtCallbackProc)radioCallback,(XtPointer) *ivar );

        }

        XtManageChild (varRadioBox[ *ivar - 1]);
        XtPopup (XtParent (varRadioBox[ *ivar -1]), XtGrabNone);
        varRadioMark[ *ivar - 1] = 1;
}        

/**********************************************************************/
/*                                                                    */
/*            piecewise linear triangle multi grid package            */
/*                                                                    */
/*                   edition 9.0 - - - march, 2004                    */
/*                                                                    */
/**********************************************************************/
void  radioCallback( w , clientData, callData )
{
        int            k = (int) clientData;
        int            i, which;
        char           varvalue[81];

        for (i=radioPtr[ k -1 ];  i< radioPtr[ k ] ; i++) {
            if(XmToggleButtonGetState(varToggle[i])  == TRUE) {
                which = i;
            } 
        }
        f2cstring(varvalue, &radioValues[80*which], 80);
        XmTextFieldSetString (varValueBox[ k - 1], varvalue);
        XtDestroyWidget(varRadioBox[ k -1]);
        XFlush(myDisplay);
        varRadioMark[ k - 1] =0;
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
        XmString        myLabel;
 
        if( *mpisw == 1 ) {
            myLabel = XmStringCreateLocalized("mpi");
            XmChangeColor (mpiLabel,  myButtonBackground);
            XtVaSetValues(mpiLabel, 
                XmNborderColor, myBackground, 
                XmNlabelString,      myLabel,
                NULL );
         } else {
            myLabel = XmStringCreateLocalized("   ");
            XmChangeColor (mpiLabel,  myBackground);
            XtVaSetValues(mpiLabel, 
                XmNborderColor, myBackground, 
                XmNlabelString,      myLabel,
                NULL );
        }
        XmStringFree(myLabel);

}
