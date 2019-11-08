&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

   DEFINE INPUT         PARAMETER P_Programa  AS CHARACTER.
   DEFINE INPUT         PARAMETER P_Titulo    AS CHARACTER.
   DEFINE INPUT         PARAMETER pc01   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc02   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc03   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc04   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc05   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc06   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc07   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc08   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc09   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc10   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc11   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc12   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc13   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc14   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc15   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc16   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc17   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc18   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc19   AS CHARACTER.
   DEFINE INPUT         PARAMETER pc20   AS CHARACTER.
   DEFINE INPUT         PARAMETER pdt01   AS DATE.
   DEFINE INPUT         PARAMETER pdt02   AS DATE.
   DEFINE INPUT         PARAMETER pdt03   AS DATE.
   DEFINE INPUT         PARAMETER pdt04   AS DATE.
   DEFINE INPUT         PARAMETER pdt05   AS DATE.
   DEFINE INPUT         PARAMETER pdt06   AS DATE.
   DEFINE INPUT         PARAMETER pdt07   AS DATE.
   DEFINE INPUT         PARAMETER pdt08   AS DATE.
   DEFINE INPUT         PARAMETER pl01    AS LOGICAL.
   DEFINE INPUT         PARAMETER pl02    AS LOGICAL.
   DEFINE INPUT         PARAMETER pl03    AS LOGICAL.
   DEFINE INPUT         PARAMETER pl04    AS LOGICAL.


/* Local Variable Definitions ---                                       */

   {incluido\Variable.i "SHARED"}


   DEFINE VARIABLE W_Fuente     AS INTEGER INITIAL 2 NO-UNDO.
   DEFINE VARIABLE W_TmpArchivo AS CHARACTER.
   DEFINE VARIABLE W_Status     AS LOGICAL.   
   DEFINE VARIABLE W_archivo    AS CHARACTER INITIAL "DEFAULT" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-14 Btn_Done EDITOR-1 BUTTON-Printer ~
EDITOR-2 FILL-IN-2 BUTTON-File EDITOR-3 BUTTON-Excel EDITOR-4 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-1 FILL-IN-1 EDITOR-2 FILL-IN-2 ~
EDITOR-3 FILL-IN-3 EDITOR-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done AUTO-GO DEFAULT 
     IMAGE-UP FILE "imagenes/session.bmp":U
     LABEL "A&ceptar" 
     SIZE 6 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Excel 
     IMAGE-UP FILE "imagenes/logoexcel.bmp":U
     LABEL "Excel" 
     SIZE 6 BY 1.62.

DEFINE BUTTON BUTTON-File 
     IMAGE-UP FILE "imagenes/r-save.bmp":U
     LABEL "Archivo" 
     SIZE 6 BY 1.62.

DEFINE BUTTON BUTTON-Printer 
     IMAGE-UP FILE "imagenes/print-u.bmp":U
     LABEL "Impresora" 
     SIZE 6 BY 1.62.

DEFINE VARIABLE EDITOR-1 AS CHARACTER INITIAL "Obtenga el informe inmediato con salida a su monitor!" 
     VIEW-AS EDITOR NO-BOX
     SIZE 28 BY 1.08
     FONT 2 NO-UNDO.

DEFINE VARIABLE EDITOR-2 AS CHARACTER INITIAL "Escoja el dispositivo de impresión y número de copias" 
     VIEW-AS EDITOR NO-BOX
     SIZE 28 BY 1.31
     FONT 2 NO-UNDO.

DEFINE VARIABLE EDITOR-3 AS CHARACTER INITIAL "Digite el nombre del Archivo y escoja la ruta de grabación!" 
     VIEW-AS EDITOR NO-BOX
     SIZE 30 BY 1.27
     FONT 2 NO-UNDO.

DEFINE VARIABLE EDITOR-4 AS CHARACTER INITIAL "Envía la información a una hoja de Excel!" 
     VIEW-AS EDITOR NO-BOX
     SIZE 28 BY 1.27
     FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 35 BY .04
     BGCOLOR 15 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY .04
     BGCOLOR 15 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY .04
     BGCOLOR 15 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NumLineas AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 66 
     LABEL "# Líneas" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 10  NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 12.12
     BGCOLOR 17 FGCOLOR 17 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Done AT ROW 1.81 COL 2.57 WIDGET-ID 36
     EDITOR-1 AT ROW 2.35 COL 9.43 NO-LABEL WIDGET-ID 12
     FILL-IN-1 AT ROW 3.77 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 30 DEBLANK 
     BUTTON-Printer AT ROW 4.54 COL 2.57 WIDGET-ID 6
     EDITOR-2 AT ROW 5.08 COL 9.43 NO-LABEL WIDGET-ID 14
     FILL-IN-2 AT ROW 6.92 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     BUTTON-File AT ROW 7.46 COL 2.57 WIDGET-ID 8
     EDITOR-3 AT ROW 7.81 COL 9.43 NO-LABEL WIDGET-ID 18
     FILL-IN-3 AT ROW 9.88 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     BUTTON-Excel AT ROW 10.42 COL 2.57 WIDGET-ID 4
     EDITOR-4 AT ROW 10.69 COL 9.43 NO-LABEL WIDGET-ID 22
     W_NumLineas AT ROW 12.04 COL 26 COLON-ALIGNED
     "Excel" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 10.12 COL 9.43 WIDGET-ID 24
          FGCOLOR 7 FONT 11
     "Pantalla!" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.77 COL 9.43 WIDGET-ID 10
          FGCOLOR 7 FONT 11
     "Impresora" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 4.5 COL 9.43 WIDGET-ID 16
          FGCOLOR 7 FONT 11
     "Archivo" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 7.23 COL 9.43 WIDGET-ID 20
          FGCOLOR 7 FONT 11
     RECT-14 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 39 BY 12.12
         BGCOLOR 17 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 12.12
         WIDTH              = 39
         MAX-HEIGHT         = 27.42
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.42
         VIRTUAL-WIDTH      = 146.29
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NumLineas IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_NumLineas:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON GO OF FRAME DEFAULT-FRAME
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Aceptar */
DO:
    ASSIGN {&WINDOW-NAME}:SENSITIVE    = FALSE
           {&WINDOW-NAME}:WINDOW-STATE = 2.

    RUN runReport.

        MESSAGE "El Listado ha sido Generado con el nombre de: " W_Archivo
          VIEW-AS ALERT-BOX.
        RUN cpantalla.w (W_Archivo, P_Titulo).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-File
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-File C-Win
ON CHOOSE OF BUTTON-File IN FRAME DEFAULT-FRAME /* Archivo */
DO:
    SYSTEM-DIALOG GET-FILE W_TmpArchivo
          TITLE      "Selecciona el Archivo de Impresión"
          FILTERS    "Archivos Textos (*.txt)"   "*.txt",
                     "Listados        (*.lst)"   "*.lst",
                     "Todos los Archivos (*.*)"   "*.*"
          /*MUST-EXIST*/
          INITIAL-DIR SESSION:TEMP-DIRECTORY
          ASK-OVERWRITE
          USE-FILENAME
          UPDATE W_Status.

    IF W_Status THEN
       ASSIGN W_Archivo = W_TmpArchivo.

     RUN runReport.

     MESSAGE "El Listado ha sido Generado con el nombre de: " W_Archivo
         VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Archivo Generado".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Printer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Printer C-Win
ON CHOOSE OF BUTTON-Printer IN FRAME DEFAULT-FRAME /* Impresora */
DO:
    RUN runReport.

    MESSAGE "El Listado ha sido Generado con el nombre de: " W_Archivo
        VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Archivo Generado".
    RUN _osprint.p ( INPUT ?, 
                     INPUT W_Archivo, 
                     INPUT 2, /*INPUT W_Fuente, /*por default = 2*/*/
                     INPUT 1, 
                     INPUT 1, 
                     INPUT 999999, 
                     OUTPUT W_Status).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NumLineas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NumLineas C-Win
ON LEAVE OF W_NumLineas IN FRAME DEFAULT-FRAME /* # Líneas */
DO:
  ASSIGN W_NumLineas.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  ASSIGN {&WINDOW-NAME}:TITLE = P_Titulo
         W_Fuente             = ?.

  APPLY "LEAVE" TO W_Numlineas.


  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY EDITOR-1 FILL-IN-1 EDITOR-2 FILL-IN-2 EDITOR-3 FILL-IN-3 EDITOR-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-14 Btn_Done EDITOR-1 BUTTON-Printer EDITOR-2 FILL-IN-2 
         BUTTON-File EDITOR-3 BUTTON-Excel EDITOR-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE runReport C-Win 
PROCEDURE runReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN VALUE(P_Programa) (W_NumLineas, INPUT-OUTPUT W_Archivo, P_Titulo,
                         STRING(pc01), 
                         STRING(pc02), 
                         STRING(pc03),
                         STRING(pc04), 
                         STRING(pc05), 
                         STRING(pc06),
                         STRING(pc07), 
                         STRING(pc08), 
                         STRING(pc09),
                         STRING(pc10), 
                         STRING(pc11),
                         STRING(pc12),
                         STRING(pc13),
                         STRING(pc14),
                         STRING(pc15),
                         STRING(pc16),
                         STRING(pc17),
                         STRING(pc18),
                         STRING(pc19),
                         STRING(pc20),
                         pdt01,
                         pdt02,
                         pdt03,
                         pdt04,
                         pdt05,
                         pdt06,
                         pdt07,
                         pdt08,
                         pl01,
                         pl02,
                         pl03,
                         pl04).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

