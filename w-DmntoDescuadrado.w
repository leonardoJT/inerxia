&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
{Incluido\variable.i "shared"}
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME hFrm_Clientes

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Ahorros

/* Definitions for FRAME hFrm_Clientes                                  */
&Scoped-define SELF-NAME hFrm_Clientes
&Scoped-define QUERY-STRING-hFrm_Clientes FOR EACH Ahorros SHARE-LOCK
&Scoped-define OPEN-QUERY-hFrm_Clientes OPEN QUERY {&SELF-NAME} FOR EACH Ahorros SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-hFrm_Clientes Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-hFrm_Clientes Ahorros


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS F-FecFin F-FecIni Btn_Cancelar BUTTON-1 ~
BUTTON-95 BtnDone-2 BTN_Print ECT-309 RECT-274 RECT-275 
&Scoped-Define DISPLAYED-OBJECTS F-FecFin F-FecIni F-Log 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Cancelar Btn_Consulta BUTTON-1 BUTTON-95 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Title 
     LABEL "" 
     SIZE 35 BY 1.

DEFINE BUTTON BtnDone-2 DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 7 BY 1.65 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/borrar.bmp":U
     LABEL "&Cancelar" 
     SIZE 7 BY 1.65 TOOLTIP "Cancelar"
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 7 BY 1.65 TOOLTIP "Buscar".

DEFINE BUTTON BTN_Print 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 7 BY 1.65 TOOLTIP "Actualizar".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 7 BY 1.65 TOOLTIP "Información".

DEFINE BUTTON BUTTON-95 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 95" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE F-FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY .81
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY .81
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-Log AS CHARACTER FORMAT "X(50)":U 
     LABEL "Log" 
     VIEW-AS FILL-IN 
     SIZE 46 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE ECT-309
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 7.27.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 9.42.

DEFINE RECTANGLE RECT-275
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY hFrm_Clientes FOR 
      Ahorros SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME hFrm_Clientes
     BTN-Title AT ROW 1.46 COL 27.57 WIDGET-ID 38
     F-FecFin AT ROW 7.73 COL 59 COLON-ALIGNED WIDGET-ID 34
     F-FecIni AT ROW 7.73 COL 19 COLON-ALIGNED WIDGET-ID 32
     Btn_Cancelar AT ROW 6.38 COL 91.14
     Btn_Consulta AT ROW 3.15 COL 91.14
     BUTTON-1 AT ROW 1.54 COL 91.14
     BUTTON-95 AT ROW 10.96 COL 92.57
     F-Log AT ROW 10.69 COL 22 COLON-ALIGNED
     BtnDone-2 AT ROW 8.54 COL 91.14 WIDGET-ID 30
     BTN_Print AT ROW 4.77 COL 91.14 WIDGET-ID 2
     "Documentos Descuadrados" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 1.65 COL 32 WIDGET-ID 40
          BGCOLOR 19 FONT 1
     ECT-309 AT ROW 3.15 COL 2
     RECT-274 AT ROW 1.27 COL 90.14
     RECT-275 AT ROW 1.31 COL 26.57 WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.86 BY 11.08
         BGCOLOR 17 FONT 4.


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
         TITLE              = "Documentos descuadrados - w-DmntoDescuadrado.w"
         HEIGHT             = 11.08
         WIDTH              = 98.86
         MAX-HEIGHT         = 21.27
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.27
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME hFrm_Clientes
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* SETTINGS FOR BUTTON BTN-Title IN FRAME hFrm_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME hFrm_Clientes
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME hFrm_Clientes
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME hFrm_Clientes
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-95 IN FRAME hFrm_Clientes
   6                                                                    */
/* SETTINGS FOR FILL-IN F-Log IN FRAME hFrm_Clientes
   NO-ENABLE                                                            */
ASSIGN 
       F-Log:READ-ONLY IN FRAME hFrm_Clientes        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME hFrm_Clientes
/* Query rebuild information for FRAME hFrm_Clientes
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Ahorros SHARE-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* FRAME hFrm_Clientes */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Documentos descuadrados - w-DmntoDescuadrado.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Documentos descuadrados - w-DmntoDescuadrado.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME hFrm_Clientes
&Scoped-define SELF-NAME BtnDone-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone-2 C-Win
ON CHOOSE OF BtnDone-2 IN FRAME hFrm_Clientes
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


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON CHOOSE OF Btn_Cancelar IN FRAME hFrm_Clientes /* Cancelar */
DO:
    RUN inicializar_variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&Scoped-define SELF-NAME BTN_Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_Print C-Win
ON CHOOSE OF BTN_Print IN FRAME hFrm_Clientes /* Imprimir */
DO:     
    RUN wimprime.w ("prCptesDescuadrados.p", "Comprobantes Descuadrados",
                    "", /* 20 Campos Char o Numéricos*/
                    "",
                    "",
                    "",
                    "", "",
                    "", "",
                    "", "",
                    "", "",
                    "", "",
                    "", "",
                    "", "",
                    "", "",
                    DATE(F-FecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}), /* 8 Campos Fechas - 4 Fechas*/
                    DATE(F-FecFin:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                    ?, ?,
                    ?, ?,
                    ?, ?,
                    ?, ?,  /* 4 Campos Lógicos*/
                    ?, ?).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME hFrm_Clientes /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
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
  RUN inicializar_variables.
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

  {&OPEN-QUERY-hFrm_Clientes}
  GET FIRST hFrm_Clientes.
  DISPLAY F-FecFin F-FecIni F-Log 
      WITH FRAME hFrm_Clientes IN WINDOW C-Win.
  ENABLE F-FecFin F-FecIni Btn_Cancelar BUTTON-1 BUTTON-95 BtnDone-2 BTN_Print 
         ECT-309 RECT-274 RECT-275 
      WITH FRAME hFrm_Clientes IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-hFrm_Clientes}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inicializar_variables C-Win 
PROCEDURE inicializar_variables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN 
      F-FecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ""
      F-FecFin:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ""
      BTN_Print:SENSITIVE = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

