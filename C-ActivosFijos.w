&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE OUTPUT PARAMETER idActivoFijo AS ROWID.
    
DEFINE VAR codDepartamento AS CHARACTER.
DEFINE VAR pNombre AS CHARACTER.

{Incluido/Variable.I "SHARED"}
DEFINE VAR W_Ok AS LOGICAL.

DEFINE TEMP-TABLE ttActivosFijos LIKE activosFijos
    FIELD txtTipoActivo AS CHARACTER
    FIELD vRowId AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwActivosFijos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttActivosFijos

/* Definitions for BROWSE brwActivosFijos                               */
&Scoped-define FIELDS-IN-QUERY-brwActivosFijos ttActivosFijos.agencia ttActivosFijos.txtTipoActivo ttActivosFijos.idActivo ttActivosFijos.nombre   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwActivosFijos   
&Scoped-define SELF-NAME brwActivosFijos
&Scoped-define QUERY-STRING-brwActivosFijos FOR EACH ttActivosFijos
&Scoped-define OPEN-QUERY-brwActivosFijos OPEN QUERY {&SELF-NAME} FOR EACH ttActivosFijos.
&Scoped-define TABLES-IN-QUERY-brwActivosFijos ttActivosFijos
&Scoped-define FIRST-TABLE-IN-QUERY-brwActivosFijos ttActivosFijos


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwActivosFijos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 cmbAgencia cmbTipoActivo txtNombre ~
brwActivosFijos btnCancelar btnAceptar 
&Scoped-Define DISPLAYED-OBJECTS cmbAgencia cmbTipoActivo txtNombre 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAceptar 
     LABEL "Aceptar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON btnCancelar 
     LABEL "Cancelar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE cmbAgencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 38.86 BY 1 NO-UNDO.

DEFINE VARIABLE cmbTipoActivo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipo Activo" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 38.86 BY 1 NO-UNDO.

DEFINE VARIABLE txtNombre AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 38.86 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 3.65.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwActivosFijos FOR 
      ttActivosFijos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwActivosFijos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwActivosFijos C-Win _FREEFORM
  QUERY brwActivosFijos DISPLAY
      ttActivosFijos.agencia        COLUMN-LABEL "Ag"                   FORMAT "99":U
      ttActivosFijos.txtTipoActivo  COLUMN-LABEL "Tipo de Activo"       FORMAT "X(20)":U
      ttActivosFijos.idActivo       COLUMN-LABEL "ID-activo"            FORMAT "X(16)":U
      ttActivosFijos.nombre         COLUMN-LABEL "Nombre del activo"    FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83 BY 9.42
         TITLE "Activos Fijos" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cmbAgencia AT ROW 2.69 COL 12.29 COLON-ALIGNED WIDGET-ID 24
     cmbTipoActivo AT ROW 3.65 COL 12.29 COLON-ALIGNED WIDGET-ID 28
     txtNombre AT ROW 4.58 COL 12.29 COLON-ALIGNED WIDGET-ID 46
     brwActivosFijos AT ROW 6.12 COL 2 WIDGET-ID 200
     btnCancelar AT ROW 16 COL 53.72 WIDGET-ID 10
     btnAceptar AT ROW 16 COL 69.72 WIDGET-ID 8
     "Filtre la información" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 1.92 COL 3 WIDGET-ID 44
     "                  Filtre la información a buscar" VIEW-AS TEXT
          SIZE 84 BY .81 AT ROW 1 COL 1 WIDGET-ID 6
          BGCOLOR 3 FONT 3
     RECT-5 AT ROW 2.19 COL 2 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.14 BY 16.31 WIDGET-ID 100.


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
         TITLE              = "Consulta de Activos Fijos"
         HEIGHT             = 16.31
         WIDTH              = 84.14
         MAX-HEIGHT         = 16.31
         MAX-WIDTH          = 84.14
         VIRTUAL-HEIGHT     = 16.31
         VIRTUAL-WIDTH      = 84.14
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brwActivosFijos txtNombre DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwActivosFijos
/* Query rebuild information for BROWSE brwActivosFijos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttActivosFijos.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwActivosFijos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Consulta de Activos Fijos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Consulta de Activos Fijos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwActivosFijos
&Scoped-define SELF-NAME brwActivosFijos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwActivosFijos C-Win
ON MOUSE-SELECT-CLICK OF brwActivosFijos IN FRAME DEFAULT-FRAME /* Activos Fijos */
DO:
    FIND FIRST activosFijos WHERE ROWID(activosFijos) = ttActivosFijos.vRowID NO-LOCK NO-ERROR.
    IF AVAILABLE activosFijos THEN
        idActivoFijo = ROWID(activosFijos).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwActivosFijos C-Win
ON MOUSE-SELECT-DBLCLICK OF brwActivosFijos IN FRAME DEFAULT-FRAME /* Activos Fijos */
DO:
    FIND FIRST activosFijos WHERE ROWID(activosFijos) = ttActivosFijos.vRowID NO-LOCK NO-ERROR.
    IF AVAILABLE activosFijos THEN DO:
        idActivoFijo = ROWID(activosFijos).
        APPLY "choose" TO btnAceptar.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwActivosFijos C-Win
ON VALUE-CHANGED OF brwActivosFijos IN FRAME DEFAULT-FRAME /* Activos Fijos */
DO:
    FIND FIRST activosFijos WHERE ROWID(activosFijos) = ttActivosFijos.vRowID NO-LOCK NO-ERROR.
    IF AVAILABLE activosFijos THEN
        idActivoFijo = ROWID(activosFijos).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAceptar C-Win
ON CHOOSE OF btnAceptar IN FRAME DEFAULT-FRAME /* Aceptar */
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


&Scoped-define SELF-NAME btnCancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancelar C-Win
ON CHOOSE OF btnCancelar IN FRAME DEFAULT-FRAME /* Cancelar */
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


&Scoped-define SELF-NAME cmbAgencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbAgencia C-Win
ON VALUE-CHANGED OF cmbAgencia IN FRAME DEFAULT-FRAME /* Agencia */
DO:
    RUN filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbTipoActivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbTipoActivo C-Win
ON VALUE-CHANGED OF cmbTipoActivo IN FRAME DEFAULT-FRAME /* Tipo Activo */
DO:
    RUN filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtNombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtNombre C-Win
ON LEAVE OF txtNombre IN FRAME DEFAULT-FRAME /* Nombre */
DO:
    RUN filtros.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

PAUSE 0 BEFORE-HIDE.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.

    /* Llena combo filtros de Agencias */
    W_Ok = cmbAgencia:ADD-LAST("").

    FOR EACH Agencias WHERE Agencias.Estado = 1 NO-LOCK:
        W_Ok = cmbAgencia:ADD-LAST(STRING(Agencias.Agencia,"99") + " - " + STRING(Agencias.Nombre,"X(16)")).
    END.
    /* ------------------------------- */

    /* Llena combo filtros de Tipo de activo */
    W_Ok = CmbTipoActivo:ADD-LAST("").

    FOR EACH varios WHERE varios.tipo = 7
                      AND varios.estado = 1 NO-LOCK:
        W_Ok = CmbTipoActivo:ADD-LAST(STRING(varios.codigo,"99") + " - " + varios.descripcion).
    END.
    /* ------------------------------------- */

    /* Llena la tabla temporal de activos fijos para los filtros */
    FOR EACH activosFijos NO-LOCK:
        CREATE ttActivosFijos.
        BUFFER-COPY activosFijos TO ttActivosFijos.
        ttActivosFijos.vRowId = ROWID(activosFijos).

        FIND FIRST varios WHERE varios.tipo = 7
                            AND varios.codigo = INTEGER(ttActivosFijos.tipoActivo) NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN
            ttActivosFijos.txtTipoActivo = varios.descripcion.
    END.

    OPEN QUERY brwActivosFijos FOR EACH ttActivosFijos NO-LOCK INDEXED-REPOSITION.
    
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
  DISPLAY cmbAgencia cmbTipoActivo txtNombre 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-5 cmbAgencia cmbTipoActivo txtNombre brwActivosFijos btnCancelar 
         btnAceptar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filtros C-Win 
PROCEDURE filtros :
/* Llena la tabla temporal de activos fijos para los filtros */
EMPTY TEMP-TABLE ttActivosFijos.

FOR EACH activosFijos NO-LOCK:
    CREATE ttActivosFijos.
    BUFFER-COPY activosFijos TO ttActivosFijos.
    ttActivosFijos.vRowId = ROWID(activosFijos).

    FIND FIRST varios WHERE varios.tipo = 7
                        AND varios.codigo = INTEGER(ttActivosFijos.tipoActivo) NO-LOCK NO-ERROR.
    IF AVAILABLE varios THEN
        ttActivosFijos.txtTipoActivo = varios.descripcion.
END.

/* Agencia */
IF cmbAgencia:SCREEN-VALUE IN FRAME default-frame <> "" AND cmbAgencia:SCREEN-VALUE <> ? THEN DO:
    FOR EACH ttActivosFijos WHERE ttActivosFijos.agencia <> INTEGER(SUBSTRING(cmbAgencia:SCREEN-VALUE,1,2)):
        DELETE ttActivosFijos.
    END.
END.

/* Tipo */
IF cmbTipoActivo:SCREEN-VALUE <> "" AND cmbTipoActivo:SCREEN-VALUE <> ? THEN DO:
    FOR EACH ttActivosFijos WHERE ttActivosFijos.tipoActivo <> INTEGER(SUBSTRING(cmbTipoActivo:SCREEN-VALUE,1,2)):
        DELETE ttActivosFijos.
    END.
END.

/* Nombre */
IF txtNombre:SCREEN-VALUE <> "" AND txtNombre:SCREEN-VALUE <> ? THEN DO:
    FOR EACH ttActivosFijos WHERE INDEX(ttActivosFijos.nombre,txtNombre:SCREEN-VALUE) = 0:
        DELETE ttActivosFijos.
    END.
END.

OPEN QUERY brwActivosFijos FOR EACH ttActivosFijos NO-LOCK INDEXED-REPOSITION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

