&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE OUTPUT PARAMETER WImagen  AS CHARACTER FORMAT "X(40)".
DEFINE OUTPUT PARAMETER WAno     AS INTEGER FORMAT "9999".
DEFINE OUTPUT PARAMETER WMes     AS INTEGER FORMAT "99".
DEFINE OUTPUT PARAMETER WFec     AS DATE.


{Incluido\VARIABLE.I "SHARED"}
/*definicion de variable del calendario*/
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
/*fin definicion variables calendario*/

/*variables locales del programa*/
  DEFINE TEMP-TABLE TEMPORAL
       FIELD Dia1      AS INTEGER FORMAT ">>" LABEL "Dom."
       FIELD Dia2      AS INTEGER FORMAT ">>" LABEL "Lun."
       FIELD Dia3      AS INTEGER FORMAT ">>" LABEL "Mar."
       FIELD Dia4      AS INTEGER FORMAT ">>" LABEL "Mie."
       FIELD Dia5      AS INTEGER FORMAT ">>" LABEL "Jue."
       FIELD Dia6      AS INTEGER FORMAT ">>" LABEL "Vie."
       FIELD Dia7      AS INTEGER FORMAT ">>" LABEL "Sab."
       FIELD Tipo1     LIKE Calendario.Tipo
       FIELD Tipo2     LIKE Calendario.Tipo
       FIELD Tipo3     LIKE Calendario.Tipo
       FIELD Tipo4     LIKE Calendario.Tipo
       FIELD Tipo5     LIKE Calendario.Tipo
       FIELD Tipo6     LIKE Calendario.Tipo
       FIELD Tipo7     LIKE Calendario.Tipo
       FIELD Cierre    AS LOGICAL.

  DEF VAR Lo           AS LOGICAL.
  DEF VAR W_Sw         AS LOGICAL  INITIAL FALSE.
  DEF VAR Sw_Generar   AS LOGICAL.
  /*DEF VAR I                AS INTEGER.*/
  DEF VAR J                AS INTEGER. 
  DEF VAR W_Rta            AS LOGICAL.
  DEF VAR W_EraDiaCierre   AS LOGICAL.
  DEF VAR W_EraDiaCieAnual AS LOGICAL.
  DEF VAR W_Nomdia     AS CHAR.
  DEF VAR W_DiaSel     AS INTEGER FORMAT "99".
  DEF VAR W_AuxMes     AS INTEGER FORMAT "99".
  DEF VAr W_FecStr     AS DATE    FORMAT "99/99/9999".
  DEF VAR In-Date      AS DATE    FORMAT "99/99/9999".
  DEF VAR W_FecNva     AS DATE    FORMAT "99/99/9999".
  DEF VAR Numdia       AS INTEGER FORMAT "99".
  DEF VAR P_Agencia    AS INTEGER FORMAT "999".
  DEF VAR NumMes       AS INTEGER FORMAT "9".
  DEF VAR Sem          AS INTEGER FORMAT "9".
  DEF VAR W_ValCal     AS INTEGER FORMAT "99".
  DEF VAR W_DiaSelAnt  AS INTEGER FORMAT "99"    INITIAL 1.
  DEF VAR W_ProcCie    AS INTEGER FORMAT "99999" INITIAL 1.
  DEF VAR W_Crear      AS LOGICAL                INITIAL TRUE.
  DEF VAR W_MesAnt     AS CHAR    FORMAT "X(9)"  INITIAL "".
  DEF VAR W_Rowid      AS ROWID.
  DEF VAR W_AuxFec     AS DATE.
  DEF VAR W_Vb         AS HANDLE.
  DEF VAR Day-name     AS CHAR    NO-UNDO
                          INIT "Domingo,Lunes,Martes,Miercoles,Jueves,Viernes,Sabado".
  DEF VAR W_CodPro     LIKE Varios.Codigo.
  DEF VAR W_RowTempo     AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS b-prevbmonth b-prevyear b-nextmonth ~
b-nextyear BtnDone monthimage RECT-15 RECT-16 RECT-17 
&Scoped-Define DISPLAYED-OBJECTS ldday styear 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetDataValue wWin 
FUNCTION GetDataValue RETURNS DATE
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetDataValue wWin 
FUNCTION SetDataValue RETURNS LOGICAL
  ( pcdate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

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

DEFINE BUTTON BtnDone DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Salir" 
     SIZE 7 BY 1.62
     BGCOLOR 8 .

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
     B1 AT ROW 3.96 COL 6
     b-prevbmonth AT ROW 1.54 COL 14
     ldday AT ROW 1.54 COL 15 COLON-ALIGNED NO-LABEL
     styear AT ROW 1.54 COL 19.57 COLON-ALIGNED NO-LABEL
     b-prevyear AT ROW 1.54 COL 26.72
     b-nextmonth AT ROW 2.08 COL 14
     b-nextyear AT ROW 2.08 COL 26.72
     b5 AT ROW 3.96 COL 18
     b6 AT ROW 3.96 COL 21
     b7 AT ROW 3.96 COL 24
     b8 AT ROW 4.77 COL 6
     b9 AT ROW 4.77 COL 9
     b10 AT ROW 4.77 COL 12
     b11 AT ROW 4.77 COL 15
     b12 AT ROW 4.77 COL 18
     b13 AT ROW 4.77 COL 21
     b14 AT ROW 4.77 COL 24
     b15 AT ROW 5.58 COL 6
     b16 AT ROW 5.58 COL 9
     b18 AT ROW 5.58 COL 15
     b19 AT ROW 5.58 COL 18
     b20 AT ROW 5.58 COL 21
     b21 AT ROW 5.58 COL 24
     b22 AT ROW 6.38 COL 6
     b23 AT ROW 6.38 COL 9
     b24 AT ROW 6.38 COL 12
     b25 AT ROW 6.38 COL 15
     b26 AT ROW 6.38 COL 18
     b27 AT ROW 6.38 COL 21
     b28 AT ROW 6.38 COL 24
     b29 AT ROW 7.19 COL 6
     b30 AT ROW 7.19 COL 9
     b31 AT ROW 7.19 COL 12
     b32 AT ROW 7.19 COL 15
     b33 AT ROW 7.19 COL 18
     b34 AT ROW 7.19 COL 21
     b35 AT ROW 7.19 COL 24
     b36 AT ROW 8 COL 6
     b37 AT ROW 8 COL 9
     b38 AT ROW 8 COL 12
     b39 AT ROW 8 COL 15
     b17 AT ROW 5.58 COL 12
     b40 AT ROW 8 COL 18
     b41 AT ROW 8 COL 21
     B2 AT ROW 3.96 COL 9
     B3 AT ROW 3.96 COL 12
     b42 AT ROW 8 COL 24
     BtnDone AT ROW 9.35 COL 23
     B4 AT ROW 3.96 COL 15
     monthimage AT ROW 1.73 COL 3
     RECT-15 AT ROW 1.27 COL 2
     RECT-16 AT ROW 3.42 COL 2
     RECT-17 AT ROW 1.27 COL 21
     " Do Ln Ma Mi Ju Vi Sa" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 3.15 COL 5
          FGCOLOR 7 FONT 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 30 BY 10.19
         BGCOLOR 17 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Escoja una Fecha"
         HEIGHT             = 10.19
         WIDTH              = 30
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         TOP-ONLY           = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Escoja una Fecha */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Escoja una Fecha */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-nextmonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-nextmonth wWin
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-nextyear wWin
ON CHOOSE OF b-nextyear IN FRAME F-Main /* Btn 3 */
DO:

     lyear = lyear + 1.
     FIND FIRST Calendario WHERE Calendario.Ano EQ LYear NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Calendario) THEN
     DO:
         MESSAGE "El calendario para el año " STRING(Lyear) " no ha sido creado" SKIP
                 "utilice el boton de 'Generación de Calendario' para crear un nuevo" SKIP
                 "Calendario!" VIEW-AS ALERT-BOX INFORMATION.
         Lyear = Lyear - 1.
         RETURN NO-APPLY.
     END.
     RUN setday. 
     RUN  setimage. 
     pmod = TRUE.
    /* DYNAMIC-FUNCTION('setDataModified':U,INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-prevbmonth
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-prevbmonth wWin
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-prevyear wWin
ON CHOOSE OF b-prevyear IN FRAME F-Main /* Btn 3 */
DO:

     lyear = lyear - 1.
     FIND FIRST Calendario WHERE Calendario.Ano EQ LYear NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Calendario) THEN
     DO:
         MESSAGE "El calendario para el año " STRING(Lyear) " no ha sido creado" SKIP
                 "utilice el boton de 'Generación de Calendario' para crear un nuevo" SKIP
                 "Calendario!" VIEW-AS ALERT-BOX INFORMATION.
         Lyear = Lyear + 1.
         RETURN NO-APPLY.
     END.
     RUN setday. 
     RUN setimage. 
     pmod = TRUE.
     /*DYNAMIC-FUNCTION('setDataModified':U,
     INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B1 wWin
ON CHOOSE OF B1 IN FRAME F-Main
,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23,b24,b25,b26,b27,b28,
 b29,b30,b31,b32,b33,b34,b35,b36,b37,b38,b39,b40,b41,b42 DO:
     ASSIGN
       lday = integer(self:label).
     RUN dispdate.
   
  pmod = TRUE.
  /*RUN BuscarDia.*/
/*DYNAMIC-FUNCTION('setDataModified':U,
     INPUT pMod).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME F-Main /* Salir */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuscarDia wWin 
PROCEDURE BuscarDia :
DEFINE VARIABLE W_Hour   AS INTEGER.
  DEFINE VARIABLE W_Min    AS INTEGER.
  DEFINE VARIABLE W_Seg    AS INTEGER.
  DEFINE VARIABLE W_Tiempo AS INTEGER.
  DEFINE VARIABLE W_Factor AS INTEGER INITIAL 0.
  ASSIGN FRAME F-Main LDDay.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DispDate wWin 
PROCEDURE DispDate :
DO WITH FRAME {&FRAME-NAME}:
   OK = monthimage:LOAD-IMAGE("imagenes\month" + STRING(lmonth) + ".gif") IN FRAME F-Main.
   newdate = DATE(lmonth,lday,lyear).
   ASSIGN ldday = string(lday,"z9") 
          styear =  string(lyear,"9999").
   DISPLAY  ldday styear WITH FRAME {&FRAME-NAME}. 
   ASSIGN WImagen = "imagenes\month" + STRING(lmonth) + ".gif"
          WAno    = LYear
          WMes    = LMonth
          WFec    = Newdate.
 END.
 RUN BuscarDia.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY ldday styear 
      WITH FRAME F-Main IN WINDOW wWin.
  ENABLE b-prevbmonth b-prevyear b-nextmonth b-nextyear BtnDone monthimage 
         RECT-15 RECT-16 RECT-17 
      WITH FRAME F-Main IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
  ASSIGN
  lmonth = MONTH(W_Fecha)
  lyear = YEAR(W_Fecha)
  lday = DAY(W_Fecha).

 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDay wWin 
PROCEDURE SetDay :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetImage wWin 
PROCEDURE SetImage :
ASSIGN tnum = 0.
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetDataValue wWin 
FUNCTION GetDataValue RETURNS DATE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetDataValue wWin 
FUNCTION SetDataValue RETURNS LOGICAL
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

