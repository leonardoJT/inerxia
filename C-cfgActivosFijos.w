&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE OUTPUT PARAMETER idCfgActivoFijo AS ROWID.
    
{Incluido/Variable.I "SHARED"}
DEFINE VAR W_Ok AS LOGICAL.

DEFINE TEMP-TABLE ttcfg
    FIELD tipoActivo AS CHARACTER
    FIELD activoFijo AS CHARACTER
    FIELD gasto AS CHARACTER
    FIELD depreciacion AS CHARACTER
    FIELD CxP AS CHARACTER
    FIELD ingreso_xAvaluo AS CHARACTER
    FIELD vRowId AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwCfgActivosFijos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttcfg

/* Definitions for BROWSE brwCfgActivosFijos                            */
&Scoped-define FIELDS-IN-QUERY-brwCfgActivosFijos ttcfg.tipoActivo ttcfg.activoFijo ttcfg.gasto ttcfg.depreciacion ttcfg.CxP ttcfg.ingreso_xAvaluo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwCfgActivosFijos   
&Scoped-define SELF-NAME brwCfgActivosFijos
&Scoped-define QUERY-STRING-brwCfgActivosFijos FOR EACH ttcfg
&Scoped-define OPEN-QUERY-brwCfgActivosFijos OPEN QUERY {&SELF-NAME} FOR EACH ttcfg.
&Scoped-define TABLES-IN-QUERY-brwCfgActivosFijos ttcfg
&Scoped-define FIRST-TABLE-IN-QUERY-brwCfgActivosFijos ttcfg


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwCfgActivosFijos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 txtNombre brwCfgActivosFijos ~
btnCancelar btnAceptar 
&Scoped-Define DISPLAYED-OBJECTS txtNombre 

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

DEFINE VARIABLE txtNombre AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 82.72 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96.43 BY 2.31.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwCfgActivosFijos FOR 
      ttcfg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwCfgActivosFijos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwCfgActivosFijos C-Win _FREEFORM
  QUERY brwCfgActivosFijos DISPLAY
      ttcfg.tipoActivo      COLUMN-LABEL "Nombre"           FORMAT "X(30)":U
      ttcfg.activoFijo      COLUMN-LABEL "Activo"           FORMAT "X(10)":U
      ttcfg.gasto           COLUMN-LABEL "Gasto"            FORMAT "X(10)":U
      ttcfg.depreciacion    COLUMN-LABEL "Depreciación"     FORMAT "X(10)":U
      ttcfg.CxP             COLUMN-LABEL "CxP Compra"       FORMAT "X(10)":U
      ttcfg.ingreso_xAvaluo COLUMN-LABEL "Ing. x Avalúo"    FORMAT "X(10)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96.43 BY 9.42
         FONT 2
         TITLE "Tipo de Activos Fijos" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     txtNombre AT ROW 2.77 COL 12.29 COLON-ALIGNED WIDGET-ID 46
     brwCfgActivosFijos AT ROW 4.69 COL 2 WIDGET-ID 200
     btnCancelar AT ROW 14.31 COL 67 WIDGET-ID 10
     btnAceptar AT ROW 14.31 COL 83 WIDGET-ID 8
     "Filtre la información" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 1.92 COL 3 WIDGET-ID 44
     "                    Filtre la información a buscar" VIEW-AS TEXT
          SIZE 96.43 BY .81 AT ROW 1.04 COL 1.86 WIDGET-ID 6
          BGCOLOR 3 FONT 3
     RECT-5 AT ROW 2.19 COL 2 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.43 BY 14.69 WIDGET-ID 100.


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
         HEIGHT             = 14.69
         WIDTH              = 97.43
         MAX-HEIGHT         = 16.31
         MAX-WIDTH          = 97.43
         VIRTUAL-HEIGHT     = 16.31
         VIRTUAL-WIDTH      = 97.43
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
/* BROWSE-TAB brwCfgActivosFijos txtNombre DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwCfgActivosFijos
/* Query rebuild information for BROWSE brwCfgActivosFijos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttcfg.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwCfgActivosFijos */
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


&Scoped-define BROWSE-NAME brwCfgActivosFijos
&Scoped-define SELF-NAME brwCfgActivosFijos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwCfgActivosFijos C-Win
ON MOUSE-SELECT-CLICK OF brwCfgActivosFijos IN FRAME DEFAULT-FRAME /* Tipo de Activos Fijos */
DO:
    idCfgActivoFijo = ttcfg.vRowID NO-ERROR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwCfgActivosFijos C-Win
ON MOUSE-SELECT-DBLCLICK OF brwCfgActivosFijos IN FRAME DEFAULT-FRAME /* Tipo de Activos Fijos */
DO:
    idCfgActivoFijo = ttcfg.vRowID NO-ERROR.
    APPLY "choose" TO btnAceptar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwCfgActivosFijos C-Win
ON VALUE-CHANGED OF brwCfgActivosFijos IN FRAME DEFAULT-FRAME /* Tipo de Activos Fijos */
DO:
    idCfgActivoFijo = ttcfg.vRowID NO-ERROR.  
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

    FOR EACH cfg_activosFijos NO-LOCK:
        FIND FIRST varios WHERE varios.tipo = 7
                            AND varios.codigo = cfg_activosFijos.tipoActivo NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN DO:
            CREATE ttcfg.
            ttcfg.tipoActivo = varios.descripcion.
            ttcfg.activoFijo = cfg_activosFijos.activoFijo.
            ttcfg.gasto = cfg_activosFijos.gasto.
            ttcfg.depreciacion = cfg_activosFijos.depreciacion.
            ttcfg.CxP = cfg_activosFijos.CxP.
            ttcfg.ingreso_xAvaluo = cfg_activosFijos.ingreso_xAvaluo.
            ttcfg.vRowId = ROWID(cfg_activosFijos).
        END.
    END.
    
    RUN Inicializar.

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
  DISPLAY txtNombre 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-5 txtNombre brwCfgActivosFijos btnCancelar btnAceptar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filtros C-Win 
PROCEDURE filtros :
/* Nombre */
IF txtNombre:SCREEN-VALUE IN FRAME default-frame <> "" THEN DO:
    OPEN QUERY brwCfgActivosFijos FOR EACH ttcfg WHERE INDEX(ttcfg.tipoActivo,txtNombre:SCREEN-VALUE) > 0 NO-LOCK INDEXED-REPOSITION.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar C-Win 
PROCEDURE Inicializar :
OPEN QUERY brwCfgActivosFijos FOR EACH ttcfg NO-LOCK INDEXED-REPOSITION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

