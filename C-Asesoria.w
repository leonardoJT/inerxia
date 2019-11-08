&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  DEFINE INPUT  PARAMETER P_Agencia  LIKE Agencias.Agencia.
  DEFINE OUTPUT PARAMETER P_Puntero  AS ROWID.

  DEFINE SHARED VAR       W_Manija   AS HANDLE.
  DEFINE        VAR       W_Rpta     AS LOGICAL.
  DEFINE SHARED VAR       W_Fecha    AS DATE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br_Asesorias

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Asesoria

/* Definitions for BROWSE Br_Asesorias                                  */
&Scoped-define FIELDS-IN-QUERY-Br_Asesorias Asesoria.Agencia Asesoria.Nit Asesoria.Num_Asesoria Asesoria.Clase_Producto Asesoria.Cod_Producto Asesoria.Fec_Asesoria   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Asesorias   
&Scoped-define SELF-NAME Br_Asesorias
&Scoped-define OPEN-QUERY-Br_Asesorias ASSIGN FRAME {&FRAME-NAME} T_ClientesAgencia. IF T_AsesoriasAgencia THEN   OPEN QUERY Br_Asesorias     FOR EACH Asesoria WHERE Asesoria.Agencia EQ P_Agencia NO-LOCK INDEXED-REPOSITION. ELSE   OPEN QUERY Br_Asesorias     FOR EACH Asesoria NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Asesorias Asesoria
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Asesorias Asesoria


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_FecIni W_FecFin F_Busca Br_Asesorias ~
R_Busca T_AsesoriasAgencia Btn_Salir BUTTON-91 RECT-127 
&Scoped-Define DISPLAYED-OBJECTS W_FecIni W_FecFin F_Busca R_Busca ~
T_AsesoriasAgencia 

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

DEFINE BUTTON BUTTON-91 
     LABEL "Ver Todas las Asesorias" 
     SIZE 25 BY 1.08.

DEFINE VARIABLE F_Busca AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nit", 1,
"Número", 2
     SIZE 18 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-127
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 3.42.

DEFINE VARIABLE T_AsesoriasAgencia AS LOGICAL INITIAL yes 
     LABEL "Solamente Asesorias de esta Agencia" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Asesorias FOR 
      Asesoria SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Asesorias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Asesorias C-Win _FREEFORM
  QUERY Br_Asesorias NO-LOCK DISPLAY
      Asesoria.Agencia FORMAT "999":U    COLUMN-LABEL "Ag."
      Asesoria.Nit FORMAT "X(12)":U
      Asesoria.Num_Asesoria FORMAT "99999999":U   COLUMN-LABEL "Número"
      Asesoria.Clase_Producto FORMAT "9":U LABEL "ClasePdt"
      Asesoria.Cod_Producto FORMAT "999":U COLUMN-LABEL "Producto"
      Asesoria.Fec_Asesoria  COLUMN-LABEL "De Fecha"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 59 BY 7
         BGCOLOR 15 FONT 5 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     W_FecIni AT ROW 10.85 COL 20.72 COLON-ALIGNED
     W_FecFin AT ROW 10.92 COL 43.43 COLON-ALIGNED
     F_Busca AT ROW 9.35 COL 21 COLON-ALIGNED NO-LABEL
     Br_Asesorias AT ROW 1.27 COL 2
     R_Busca AT ROW 9.35 COL 4 NO-LABEL
     T_AsesoriasAgencia AT ROW 12.5 COL 2.72
     Btn_Salir AT ROW 13.04 COL 53.72
     BUTTON-91 AT ROW 13.58 COL 1.72
     RECT-127 AT ROW 8.81 COL 2
     " Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.54 COL 3
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 61 BY 13.85
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
         TITLE              = "SFG - Consulta de Asesorias de Productos"
         HEIGHT             = 13.85
         WIDTH              = 61
         MAX-HEIGHT         = 22.35
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.35
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
   Custom                                                               */
/* BROWSE-TAB Br_Asesorias F_Busca DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Asesorias
/* Query rebuild information for BROWSE Br_Asesorias
     _START_FREEFORM
ASSIGN FRAME {&FRAME-NAME} T_ClientesAgencia.
IF T_AsesoriasAgencia THEN
  OPEN QUERY Br_Asesorias
    FOR EACH Asesoria WHERE Asesoria.Agencia EQ P_Agencia NO-LOCK INDEXED-REPOSITION.
ELSE
  OPEN QUERY Br_Asesorias
    FOR EACH Asesoria NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Asesorias */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SFG - Consulta de Asesorias de Productos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SFG - Consulta de Asesorias de Productos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Asesorias
&Scoped-define SELF-NAME Br_Asesorias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Asesorias C-Win
ON MOUSE-SELECT-DBLCLICK OF Br_Asesorias IN FRAME DEFAULT-FRAME
DO:
  APPLY 'choose' TO Btn_Salir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir C-Win
ON CHOOSE OF Btn_Salir IN FRAME DEFAULT-FRAME /* Button 90 */
DO:
  IF Br_Asesorias:NUM-SELECTED-ROWS > 0 THEN                 
     ASSIGN P_Puntero = ROWID(Asesoria).
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


&Scoped-define SELF-NAME BUTTON-91
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-91 C-Win
ON CHOOSE OF BUTTON-91 IN FRAME DEFAULT-FRAME /* Ver Todas las Asesorias */
DO:
  ASSIGN FRAME {&FRAME-NAME} T_AsesoriasAgencia.
  IF T_AsesoriasAgencia EQ YES THEN
    OPEN QUERY Br_Asesorias FOR EACH Asesoria WHERE Asesoria.Agencia EQ P_Agencia NO-LOCK INDEXED-REPOSITION.  
  ELSE
    OPEN QUERY Br_Asesorias FOR EACH Asesoria NO-LOCK INDEXED-REPOSITION.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Busca C-Win
ON LEAVE OF F_Busca IN FRAME DEFAULT-FRAME
DO:
  ASSIGN FRAME {&FRAME-NAME} F_Busca R_Busca T_AsesoriasAgencia.
  IF T_AsesoriasAgencia THEN DO:
    CASE R_Busca:
      WHEN 1 THEN
        OPEN QUERY Br_Asesorias FOR EACH Asesoria  WHERE 
                                         Asesoria.Agencia EQ P_Agencia AND
                                         Asesoria.Nit EQ F_Busca
                                         NO-LOCK INDEXED-REPOSITION.
      WHEN 2 THEN
        OPEN QUERY Br_Asesorias FOR EACH Asesoria WHERE 
                                         Asesoria.Agencia EQ P_Agencia AND
                                         Asesoria.Num_Asesoria EQ INTEGER(F_Busca)
                                         NO-LOCK INDEXED-REPOSITION.
    END CASE.
  END.
  ELSE DO:
      CASE R_Busca:
        WHEN 1 THEN
          OPEN QUERY Br_Asesorias FOR EACH Asesoria  WHERE 
                                           Asesoria.Nit EQ F_Busca
                                           NO-LOCK INDEXED-REPOSITION.
        WHEN 2 THEN
          OPEN QUERY Br_Asesorias FOR EACH Asesoria WHERE 
                                           Asesoria.Num_Asesoria EQ INTEGER(F_Busca)
                                           NO-LOCK INDEXED-REPOSITION.
      END CASE.
  END.

  F_Busca = "".
  DISPLAY F_Busca WITH FRAME {&FRAME-NAME}.
  APPLY "ENTRY" TO F_Busca.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Busca C-Win
ON VALUE-CHANGED OF R_Busca IN FRAME DEFAULT-FRAME
DO:
   APPLY "Entry" TO F_Busca.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecFin C-Win
ON LEAVE OF W_FecFin IN FRAME DEFAULT-FRAME /* Hasta */
DO:
  ASSIGN W_FecFin.

  IF W_FecFin LT W_FecIni THEN
     ASSIGN W_FecFin = W_FecIni
            W_FecFin:SCREEN-VALUE = STRING(W_FecIni).

  OPEN QUERY Br_Asesorias FOR EACH Asesoria  WHERE 
                                         Asesoria.Fec_Asesoria GE W_FecIni AND
                                         Asesoria.Fec_Asesoria LE W_FecFin
                                         NO-LOCK INDEXED-REPOSITION.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecIni C-Win
ON LEAVE OF W_FecIni IN FRAME DEFAULT-FRAME /* Desde */
DO:
  ASSIGN W_FecIni.
  APPLY "ENTRY" TO W_FecFin.
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
  ASSIGN W_FecIni = W_Fecha
         W_FecFin = W_Fecha
         W_FecIni:SCREEN-VALUE = STRING(W_Fecha)
         W_FecFin:SCREEN-VALUE = STRING(W_Fecha).

  C-Win:MOVE-TO-TOP().
  APPLY "ENTRY" TO F_Busca.
  
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
  DISPLAY W_FecIni W_FecFin F_Busca R_Busca T_AsesoriasAgencia 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE W_FecIni W_FecFin F_Busca Br_Asesorias R_Busca T_AsesoriasAgencia 
         Btn_Salir BUTTON-91 RECT-127 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

