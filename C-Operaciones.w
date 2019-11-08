&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  DEFINE OUTPUT PARAMETER P_Codigo LIKE Operacion.Cod_Operacion.
  DEFINE OUTPUT PARAMETER p_Nombre LIKE Operacion.Nom_Operacion.

  DEFINE SHARED VAR       W_Manija   AS HANDLE.
  DEFINE        VAR       W_Rpta     AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br_Operacion

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Operacion

/* Definitions for BROWSE Br_Operacion                                  */
&Scoped-define FIELDS-IN-QUERY-Br_Operacion Operacion.Cod_Operacion Operacion.Nom_Operacion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Operacion   
&Scoped-define SELF-NAME Br_Operacion
&Scoped-define QUERY-STRING-Br_Operacion FOR EACH Operacion NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Operacion OPEN QUERY Br_Operacion     FOR EACH Operacion NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Operacion Operacion
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Operacion Operacion


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-Br_Operacion}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Br_Operacion Btn_Salir R_Busca F_Busca ~
RECT-127 
&Scoped-Define DISPLAYED-OBJECTS R_Busca F_Busca 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Salir AUTO-END-KEY 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 90" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE F_Busca AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Codigo", 1,
"Nombre", 2
     SIZE 30 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-127
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66 BY 1.88.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Operacion FOR 
      Operacion SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Operacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Operacion C-Win _FREEFORM
  QUERY Br_Operacion NO-LOCK DISPLAY
      Operacion.Cod_Operacion
  Operacion.Nom_Operacion
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 74 BY 7
         BGCOLOR 15 FONT 5 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Br_Operacion AT ROW 1.27 COL 2
     Btn_Salir AT ROW 8.81 COL 69
     R_Busca AT ROW 9.35 COL 3 NO-LABEL
     F_Busca AT ROW 9.35 COL 33 COLON-ALIGNED NO-LABEL
     RECT-127 AT ROW 8.81 COL 2
     " Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.54 COL 3
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.86 BY 10.04
         BGCOLOR 17 FONT 5.


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
         TITLE              = "SFG - Consulta de Operaciones"
         HEIGHT             = 10.04
         WIDTH              = 76.86
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
IF NOT C-Win:LOAD-ICON("imagenes/magnify0.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/magnify0.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB Br_Operacion 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Operacion
/* Query rebuild information for BROWSE Br_Operacion
     _START_FREEFORM
  OPEN QUERY Br_Operacion
    FOR EACH Operacion NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Operacion */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SFG - Consulta de Operaciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SFG - Consulta de Operaciones */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Operacion
&Scoped-define SELF-NAME Br_Operacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Operacion C-Win
ON MOUSE-SELECT-DBLCLICK OF Br_Operacion IN FRAME DEFAULT-FRAME
DO:
  APPLY "choose" TO Btn_Salir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir C-Win
ON CHOOSE OF Btn_Salir IN FRAME DEFAULT-FRAME /* Button 90 */
DO:
  IF Br_Operacion:NUM-SELECTED-ROWS > 0 THEN                 
     ASSIGN P_Nombre   = Operacion.Nom_Operacion
            P_Codigo   = Operacion.Cod_Operacion.
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


&Scoped-define SELF-NAME F_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Busca C-Win
ON LEAVE OF F_Busca IN FRAME DEFAULT-FRAME
DO:
   ASSIGN FRAME {&FRAME-NAME} R_Busca F_Busca.
   CASE R_Busca:
      WHEN 1 THEN
        OPEN QUERY Br_Operacion FOR EACH Operacion  WHERE Operacion.Cod_Operacion EQ INTEGER(F_Busca)
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 2 THEN
        OPEN QUERY Br_Operacion FOR EACH Operacion WHERE Operacion.Nom_Operacion BEGINS F_Busca
                                        NO-LOCK INDEXED-REPOSITION.
   END CASE.

  F_Busca = "".
  DISPLAY F_Busca WITH FRAME {&FRAME-NAME}.
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
  DISPLAY R_Busca F_Busca 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE Br_Operacion Btn_Salir R_Busca F_Busca RECT-127 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

