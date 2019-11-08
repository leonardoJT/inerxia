&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File:

  Description: from FIELD.W - Template for ADM2 SmartDataField object

  Created: June 1999 -- Progress Version 9.1A

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR location AS INTEGER FORMAT "99" NO-UNDO.
DEF VAR lweekday AS INTEGER NO-UNDO.
DEF VAR lstday AS INTEGER NO-UNDO.
DEF VAR d AS INTEGER NO-UNDO.
DEF VAR tnum AS INTEGER format "z9" NO-UNDO.
DEF VAR tchar AS CHAR FORMAT "X(9)" NO-UNDO.
DEF VAR lastday AS INTEGER NO-UNDO.
DEF VAR lastdate AS DATE NO-UNDO.
DEF VAR lentdate AS DATE LABEL " Date" INIT TODAY NO-UNDO.
DEF VAR lmonth AS INTEGER NO-UNDO.
DEF VAR lm LIKE lmonth no-undo.
DEF VAR lyear AS INTEGER FORMAT "9999" NO-UNDO.
def var lyr like lyear no-undo.
DEF var cur as WIDGET-HANDLE.
DEF VAR mnthname AS CHAR FORMAT "stdate(10)" EXTENT 12 INIT
["January",
 "February",
 "March",
 "April",
 "May",
 "June",
 "July",
 "August",
 "September",
 "October",
 "November",
 "December"] NO-UNDO.
def var lbut as logical NO-UNDO.
DEF VAR newdate AS DATE NO-UNDO.
def var totbut as int no-undo.
def var bhandle as widget-handle extent 42 no-undo.
def var lname as char no-undo.
def var i as integer no-undo.
def var lday as integer no-undo.
def var ok as logical no-undo.
DEF VAR barray AS HANDLE EXTENT 42 NO-UNDO.
DEF VAR pmod AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataField
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS b-prevbmonth b-prevyear b-nextmonth ~
b-nextyear monthimage RECT-15 RECT-16 RECT-17 
&Scoped-Define DISPLAYED-OBJECTS ldday styear 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDataValue sObject 
FUNCTION getDataValue RETURNS DATE
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setDataValue sObject 
FUNCTION setDataValue RETURNS LOGICAL
  ( pcdate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON b-nextmonth 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON b-nextyear 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON b-prevbmonth 
     IMAGE-UP FILE "imagenes/arwup.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON b-prevyear 
     IMAGE-UP FILE "imagenes/arwup.gif":U
     LABEL "Btn 3":L 
     SIZE 2.57 BY .62.

DEFINE BUTTON B1  NO-FOCUS NO-CONVERT-3D-COLORS
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b10 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b11 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b12 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b13 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b14 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b15 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b16 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b17  NO-FOCUS
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b18 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b19 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON B2  NO-FOCUS
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b20 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b21 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b22 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b23 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b24 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b25 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b26 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b27 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b28 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b29 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON B3  NO-FOCUS
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b30 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b31 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b32 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b33 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b34 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b35 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b36 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b37 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b38 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b39 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON B4  NO-FOCUS
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b40 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b41 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b42 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b5 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b6 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b7 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b8 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE BUTTON b9 
     LABEL "":L 
     SIZE 2.72 BY .81
     FONT 4.

DEFINE VARIABLE ldday AS CHARACTER FORMAT "XXX":U 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1.08
     BGCOLOR 17 FGCOLOR 7 FONT 6 NO-UNDO.

DEFINE VARIABLE styear AS CHARACTER FORMAT "X(5)":U 
     VIEW-AS FILL-IN 
     SIZE 4.86 BY 1.08
     BGCOLOR 17 FGCOLOR 7 FONT 6 NO-UNDO.

DEFINE IMAGE monthimage
     FILENAME "imagenes/month1.gif":U
     SIZE 11 BY .92.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 19 BY 1.62.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 28 BY 5.65
     BGCOLOR 17 .

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 8.86 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     B1 AT ROW 3.69 COL 4
     b-prevbmonth AT ROW 1.27 COL 13
     ldday AT ROW 1.27 COL 14 COLON-ALIGNED NO-LABEL
     styear AT ROW 1.27 COL 18.57 COLON-ALIGNED NO-LABEL
     b17 AT ROW 5.31 COL 10
     b-prevyear AT ROW 1.27 COL 25.72
     b-nextmonth AT ROW 1.81 COL 13
     b-nextyear AT ROW 1.81 COL 25.72
     b5 AT ROW 3.69 COL 16
     b6 AT ROW 3.69 COL 19
     b7 AT ROW 3.69 COL 22
     b8 AT ROW 4.5 COL 4
     b9 AT ROW 4.5 COL 7
     b10 AT ROW 4.5 COL 10
     b11 AT ROW 4.5 COL 13
     b12 AT ROW 4.5 COL 16
     b13 AT ROW 4.5 COL 19
     b14 AT ROW 4.5 COL 22
     b15 AT ROW 5.31 COL 4
     b16 AT ROW 5.31 COL 7
     b18 AT ROW 5.31 COL 13
     b19 AT ROW 5.31 COL 16
     b20 AT ROW 5.31 COL 19
     b21 AT ROW 5.31 COL 22
     b22 AT ROW 6.12 COL 4
     b23 AT ROW 6.12 COL 7
     b24 AT ROW 6.12 COL 10
     B2 AT ROW 3.69 COL 7
     b25 AT ROW 6.12 COL 13
     b26 AT ROW 6.12 COL 16
     b27 AT ROW 6.12 COL 19
     b28 AT ROW 6.12 COL 22
     b29 AT ROW 6.92 COL 4
     b30 AT ROW 6.92 COL 7
     b31 AT ROW 6.92 COL 10
     b32 AT ROW 6.92 COL 13
     b33 AT ROW 6.92 COL 16
     b34 AT ROW 6.92 COL 19
     b35 AT ROW 6.92 COL 22
     b36 AT ROW 7.73 COL 4
     b37 AT ROW 7.73 COL 7
     b38 AT ROW 7.73 COL 10
     b39 AT ROW 7.73 COL 13
     b40 AT ROW 7.73 COL 16
     b41 AT ROW 7.73 COL 19
     B3 AT ROW 3.69 COL 10
     B4 AT ROW 3.69 COL 13
     b42 AT ROW 7.73 COL 22
     monthimage AT ROW 1.46 COL 2
     RECT-15 AT ROW 1 COL 1
     RECT-16 AT ROW 3.15 COL 1
     RECT-17 AT ROW 1 COL 20
     "Dm Ln Ma Mi Ju Vi Sa" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 2.88 COL 4
          FGCOLOR 7 FONT 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataField
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW sObject ASSIGN
         HEIGHT             = 7.96
         WIDTH              = 28.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB sObject 
/* ************************* Included-Libraries *********************** */

{src/adm2/field.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW sObject
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON b-nextmonth IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b-nextyear IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b-prevbmonth IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON b-prevyear IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B1 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       B1:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b10 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b10:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b11 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b11:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b12 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b12:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b13 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b13:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b14 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b14:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b15 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b15:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b16 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b16:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b17 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b17:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b18 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b18:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b19 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b19:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON B2 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       B2:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b20 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b20:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b21 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b21:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b22 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b22:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b23 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b23:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b24 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b24:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b25 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b25:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b26 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b26:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b27 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b27:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b28 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b28:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b29 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b29:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON B3 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       B3:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b30 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b30:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b31 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b31:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b32 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b32:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b33 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b33:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b34 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b34:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b35 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b35:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b36 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b36:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b37 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b37:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b38 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b38:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b39 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b39:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON B4 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       B4:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b40 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b40:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b41 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b41:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b42 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b42:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b5 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b5:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b6 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b6:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b7 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b7:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b8 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b8:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR BUTTON b9 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       b9:PRIVATE-DATA IN FRAME F-Main     = 
                "day".

/* SETTINGS FOR FILL-IN ldday IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN styear IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME b-nextmonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-nextmonth sObject
ON CHOOSE OF b-nextmonth IN FRAME F-Main /* Btn 3 */
DO:
      IF lmonth < 12 THEN
      lmonth = lmonth + 1.
      
      ELSE DO: 
           lmonth = 1.
           lyear = lyear + 1.
      END.
     
      RUN setday. 
      RUN setimage. 
      pmod = TRUE.
      /*DYNAMIC-FUNCTION('setDataModified':U,
      INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-nextyear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-nextyear sObject
ON CHOOSE OF b-nextyear IN FRAME F-Main /* Btn 3 */
DO:

     lyear = lyear + 1.
     RUN setday. 
     RUN  setimage. 
     pmod = TRUE.
    /* DYNAMIC-FUNCTION('setDataModified':U,INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-prevbmonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-prevbmonth sObject
ON CHOOSE OF b-prevbmonth IN FRAME F-Main /* Btn 3 */
DO:
   
    IF  lmonth > 1 THEN
    lmonth = lmonth - 1.
    
    ELSE DO:
            lmonth = 12.
            lyear = lyear - 1.
    END.
    
    RUN setday. 
    RUN setimage. 
    pmod = TRUE.
    /*DYNAMIC-FUNCTION('setDataModified':U,
     INPUT pMod).   */
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-prevyear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-prevyear sObject
ON CHOOSE OF b-prevyear IN FRAME F-Main /* Btn 3 */
DO:

     lyear = lyear - 1.
     RUN setday. 
     RUN setimage. 
     pmod = TRUE.
     /*DYNAMIC-FUNCTION('setDataModified':U,
     INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B1 sObject
ON CHOOSE OF B1 IN FRAME F-Main
,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26,b27,b28,
 b29,b30,b31,b32,b33,b34,b35,b36,b37,b38,b39,b40,b41,b42 DO:
     ASSIGN
       lday = integer(self:label).
     
       RUN dispdate.
    
  pmod = TRUE.
/*DYNAMIC-FUNCTION('setDataModified':U,
     INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableField sObject 
PROCEDURE disableField :
/*------------------------------------------------------------------------------
  Purpose:   Disable the field   
  Parameters:  <none>
  Notes:    SmartDataViewer:disableFields will call this for all Objects of type
            PROCEDURE that it encounters in the EnableFields Property.  
            The developer must add logic to disable the actual SmartField.    
------------------------------------------------------------------------------*/
   
   {set FieldEnabled FALSE}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI sObject  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dispdate sObject 
PROCEDURE dispdate :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
       DO WITH FRAME {&FRAME-NAME}:
       
           OK = monthimage:LOAD-IMAGE("imagenes\month" + STRING(lmonth) + ".gif").
           
           newdate = DATE(lmonth,lday,lyear).
          
           ASSIGN
           ldday = string(lday,"z9") 
           styear =  string(lyear,"9999").
           DISPLAY  ldday styear WITH FRAME {&FRAME-NAME}. 
       END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableField sObject 
PROCEDURE enableField :
/*------------------------------------------------------------------------------
  Purpose:   Enable the field   
  Parameters:  <none>
  Notes:    SmartDataViewer:enableFields will call this for all Objects of type
            PROCEDURE that it encounters in the EnableFields Property.  
            The developer must add logic to enable the SmartField.    
------------------------------------------------------------------------------*/
   
   {set FieldEnabled TRUE}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject sObject 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

                            
  RUN SUPER.
  ASSIGN
  lmonth = MONTH(lentdate)
  lyear = YEAR(lentdate)
  lday = DAY(lentdate).

 
/*       ASSIGN                                         */
/*       styear:sensitive in frame {&FRAME-NAME} = TRUE */
/*       ldday:sensitive in frame {&FRAME-NAME} = TRUE  */
/*       stdate:sensitive in frame {&FRAME-NAME} = TRUE. */
   DO WITH FRAME {&FRAME-NAME}:
       barray[1] = b1:HANDLE.
       barray[2] = b2:HANDLE.
       barray[3] = b3:HANDLE.
       barray[4] = b4:HANDLE.
       barray[5] = b5:HANDLE.
       barray[6] = b6:HANDLE.
       barray[7] = b7:HANDLE.
       barray[8] = b8:HANDLE.
       barray[9] = b9:HANDLE.
      barray[10] = b10:HANDLE.
      barray[11] = b11:HANDLE.
      barray[12] = b12:HANDLE.
      barray[13] = b13:HANDLE.
      barray[14] = b14:HANDLE.
      barray[15] = b15:HANDLE.
      barray[16] = b16:HANDLE.
      barray[17] = b17:HANDLE.
      barray[18] = b18:HANDLE.
      barray[19] = b19:HANDLE.
      barray[20] = b20:HANDLE.
      barray[21] = b21:HANDLE.
      barray[22] = b22:HANDLE.
      barray[23] = b23:HANDLE.
      barray[24] = b24:HANDLE.
      barray[25] = b25:HANDLE.
      barray[26] = b26:HANDLE.
      barray[27] = b27:HANDLE.
      barray[28] = b28:HANDLE.
      barray[29] = b29:HANDLE.
      barray[30] = b30:HANDLE.
      barray[31] = b31:HANDLE.
      barray[32] = b32:HANDLE.
      barray[33] = b33:HANDLE.
      barray[34] = b34:HANDLE.
      barray[35] = b35:HANDLE.
      barray[36] = b36:HANDLE.
      barray[37] = b37:HANDLE.
      barray[38] = b38:HANDLE.
      barray[39] = b39:HANDLE.
      barray[40] = b40:HANDLE.
      barray[41] = b41:HANDLE.
      barray[42] = b42:HANDLE.
       
      RUN setday.
      RUN setimage.   
     
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setday sObject 
PROCEDURE setday :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    lweekday = WEEKDAY (DATE(lmonth,1,lyear)).
            lm = lmonth + 1.
            lyr = lyear.
            
            IF lm > 12 THEN DO:
                lm = 1.
                lyr =  lyr + 1.
            END.

           ASSIGN
           lastdate = (date(lm,1,lyr)) - 1
           lastday = day(lastdate)
           lstday = lweekday.
           
           IF lday >= 28 THEN DO:
                   DO WHILE lastday < lday:
                      lday = lday - 1.
                   END.
           END.  /*lday >= 28*/  
          
           RUN dispdate.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setimage sObject 
PROCEDURE setimage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     ASSIGN      
     tnum = 0.
   
          DO d = 1 to 42:
                     IF d >= lstday AND d <= (lastday + lstday - 1) THEN DO:     
                          ASSIGN
                          tnum = tnum + 1      
                          barray[d]:LABEL = STRING(tnum, "z9").
                           
                          
                          IF barray[d]:SENSITIVE = FALSE THEN barray[d]:SENSITIVE = TRUE. 
                    END. /*d >= lastday*/
                   
                   ELSE DO:
                         IF barray[d]:SENSITIVE = TRUE THEN barray[d]:SENSITIVE = FALSE.
                         barray[d]:LABEL = "  ".
                   END.
             END. /*do d = 1 to 42*/
             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDataValue sObject 
FUNCTION getDataValue RETURNS DATE
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  Returns the current value of the SmartDataField object.
   Params:  none
    Notes:  This function must be defined by the developer of the object
            to return its value.
------------------------------------------------------------------------------*/
 newdate = DATE(lmonth,lday,lyear).
  RETURN newdate .   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setDataValue sObject 
FUNCTION setDataValue RETURNS LOGICAL
  ( pcdate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  This function receives the value for the SmartDataField and assigns it.
   Params:  The parameter and its datatype must be defined by the developer.
    Notes:  
------------------------------------------------------------------------------*/
IF pcdate <> ? THEN DO: 
  ASSIGN
  lmonth = MONTH(pcdate)
  lyear = YEAR(pcdate)
  lday = DAY(pcdate).
END.
ELSE DO: 
  ASSIGN
  lmonth = MONTH(TODAY)
  lyear = YEAR(TODAY)
  lday = DAY(TODAY).
   

END.

      RUN setday.
      RUN setimage.   

     

  
  RETURN TRUE. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

