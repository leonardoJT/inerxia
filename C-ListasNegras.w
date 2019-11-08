&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE OUTPUT PARAMETER listaSuspendidoRowID AS ROWID.

DEFINE SHARED VAR W_Manija AS HANDLE.
DEFINE VAR W_Rpta AS LOGICAL.

DEFINE TEMP-TABLE ttsuspendidos
    FIELD lista AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD fec_suspension AS DATE
    FIELD estado AS CHARACTER
    FIELD vRowId AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br_Listas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttsuspendidos

/* Definitions for BROWSE Br_Listas                                     */
&Scoped-define FIELDS-IN-QUERY-Br_Listas ttsuspendidos.lista ttsuspendidos.Nit ttsuspendidos.Nombre ttsuspendidos.fec_suspension ttsuspendidos.estado   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Listas   
&Scoped-define SELF-NAME Br_Listas
&Scoped-define QUERY-STRING-Br_Listas FOR EACH ttsuspendidos NO-LOCK BY suspendidos.fec_exclusion INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Listas OPEN QUERY Br_Listas FOR EACH ttsuspendidos NO-LOCK BY suspendidos.fec_exclusion INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Listas ttsuspendidos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Listas ttsuspendidos


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-127 Br_Listas R_Busca BUTTON-90 ~
BUTTON-126 F_Busca 
&Scoped-Define DISPLAYED-OBJECTS R_Busca F_Busca 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-126 
     LABEL "Ver Todos" 
     SIZE 14 BY 1.12.

DEFINE BUTTON BUTTON-90 AUTO-END-KEY 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "Button 90" 
     SIZE 10 BY 1.92.

DEFINE VARIABLE F_Busca AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nit", 1,
"Nombre", 2,
"Apellido1", 3,
"Apellido2", 4,
"Lista", 5
     SIZE 55 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-127
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 2.96.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Listas FOR 
      ttsuspendidos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Listas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Listas C-Win _FREEFORM
  QUERY Br_Listas NO-LOCK DISPLAY
      ttsuspendidos.lista FORMAT "X(12)":U COLUMN-LABEL "LISTA"
      ttsuspendidos.Nit FORMAT "X(12)":U COLUMN-LABEL "IDENTIFICACIÓN"
      ttsuspendidos.Nombre FORMAT "X(45)":U COLUMN-LABEL "NOMBRE"
      ttsuspendidos.fec_suspension FORMAT "99/99/9999":U COLUMN-LABEL "FEC/SUSPENSIÓN"
      ttsuspendidos.estado FORMAT "X(12)":U COLUMN-LABEL "ESTADO"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 111 BY 7
         BGCOLOR 15 FONT 5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Br_Listas AT ROW 1.27 COL 2
     R_Busca AT ROW 9.35 COL 4 NO-LABEL
     BUTTON-90 AT ROW 9.92 COL 102.43
     BUTTON-126 AT ROW 10.31 COL 59
     F_Busca AT ROW 10.35 COL 2 COLON-ALIGNED NO-LABEL
     " Elija el parámetro de búsqueda" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 8.54 COL 3
          FGCOLOR 7 
     RECT-127 AT ROW 8.81 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.57 BY 11.08
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
         TITLE              = "SFG - Consulta de Listas Negras"
         HEIGHT             = 11.08
         WIDTH              = 112.57
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 112.57
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 112.57
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
   FRAME-NAME                                                           */
/* BROWSE-TAB Br_Listas RECT-127 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Listas
/* Query rebuild information for BROWSE Br_Listas
     _START_FREEFORM
OPEN QUERY Br_Listas FOR EACH ttsuspendidos NO-LOCK BY suspendidos.fec_exclusion INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Listas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SFG - Consulta de Listas Negras */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SFG - Consulta de Listas Negras */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Listas
&Scoped-define SELF-NAME Br_Listas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Listas C-Win
ON MOUSE-SELECT-DBLCLICK OF Br_Listas IN FRAME DEFAULT-FRAME
DO:
    APPLY "choose" TO button-90.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-126
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-126 C-Win
ON CHOOSE OF BUTTON-126 IN FRAME DEFAULT-FRAME /* Ver Todos */
DO:
    DEFINE VAR nombreLista AS CHARACTER.

    EMPTY TEMP-TABLE ttsuspendidos.

    FOR EACH listaNegra NO-LOCK BREAK BY listaNegra.codigo:
        CREATE ttsuspendidos.
        ttsuspendidos.nit = listaNegra.nit.

        IF FIRST-OF(listaNegra.codigo) THEN DO:
            FIND FIRST varios WHERE varios.tipo = 17 AND varios.codigo = listaNegra.codigo NO-LOCK NO-ERROR.
            IF AVAILABLE varios THEN
                nombreLista = varios.descripcion.
            ELSE
                nombreLista = "Desconocida".
        END.

        ttsuspendidos.lista = nombreLista.

        ttsuspendidos.nombre = listaNegra.nombre + " " + listaNegra.apellido1 + " " + listaNegra.apellido2.
        ttsuspendidos.fec_suspension = listaNegra.fec_actualizacion.

        IF listaNegra.estado = 1 THEN DO:
            CASE listaNegra.codigo:
                WHEN 4 THEN ttsuspendidos.estado = "SUSPENDIDO".
                WHEN 5 THEN ttsuspendidos.estado = "EXLUÍDO".
                OTHERWISE ttsuspendidos.estado = "REPORTADO".
            END CASE.
        END.
        ELSE
            ttsuspendidos.estado = "NORMAL".

        ttsuspendidos.vRowId = ROWID(listaNegra).
    END.

    OPEN QUERY Br_Listas FOR EACH ttsuspendidos NO-LOCK BY ttsuspendidos.fec_suspension BY ttsuspendidos.Nit INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-90
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-90 C-Win
ON CHOOSE OF BUTTON-90 IN FRAME DEFAULT-FRAME /* Button 90 */
DO:
    IF Br_Listas:NUM-SELECTED-ROWS > 0 THEN
        listaSuspendidoRowID = ttsuspendidos.vRowId.

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
    EMPTY TEMP-TABLE ttsuspendidos.

    ASSIGN FRAME {&FRAME-NAME} F_Busca R_Busca.

    CASE R_Busca:
        WHEN 1 THEN
            FOR EACH listaNegra WHERE ListaNegra.Nit EQ F_Busca NO-LOCK:
                RUN llenaTTsuspendidos.
            END.
        
        WHEN 2 OR WHEN 3 OR WHEN 4 THEN
            FOR EACH listaNegra WHERE ListaNegra.Nombre CONTAINS F_Busca OR
                                      ListaNegra.apellido1 CONTAINS F_Busca OR
                                      ListaNegra.apellido2 CONTAINS F_Busca NO-LOCK:
                RUN llenaTTsuspendidos.
            END.

        WHEN 5 THEN
            FOR EACH listaNegra WHERE ListaNegra.Codigo EQ INTEGER(F_Busca) NO-LOCK:
                RUN llenaTTsuspendidos.
            END.
    END CASE.

    OPEN QUERY Br_Listas FOR EACH ttsuspendidos NO-LOCK BY ttsuspendidos.fec_suspension BY ttsuspendidos.nit INDEXED-REPOSITION.

    /*F_Busca = "".*/

    DISPLAY F_Busca WITH FRAME {&FRAME-NAME}.
END.

PROCEDURE llenaTtSuspendidos:
    CREATE ttsuspendidos.
    ttsuspendidos.nit = listaNegra.nit.
    
    FIND FIRST varios WHERE varios.tipo = 17
                        AND varios.codigo = listaNegra.codigo NO-LOCK NO-ERROR.
    IF AVAILABLE varios THEN
        ttsuspendidos.lista = varios.descripcion.
    ELSE
        ttsuspendidos.lista = "Desconocida".

    ttsuspendidos.nombre = listaNegra.nombre + " " + listaNegra.apellido1 + " " + listaNegra.apellido2.
    ttsuspendidos.fec_suspension = listaNegra.fec_actualizacion.

    IF listaNegra.estado = 1 THEN DO:
        CASE listaNegra.codigo:
            WHEN 4 THEN ttsuspendidos.estado = "SUSPENDIDO".
            WHEN 5 THEN ttsuspendidos.estado = "EXLUÍDO".
            OTHERWISE ttsuspendidos.estado = "REPORTADO".
        END CASE.
    END.
    ELSE
        ttsuspendidos.estado = "NORMAL".

    ttsuspendidos.vRowId = ROWID(listaNegra).
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
  ENABLE RECT-127 Br_Listas R_Busca BUTTON-90 BUTTON-126 F_Busca 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

