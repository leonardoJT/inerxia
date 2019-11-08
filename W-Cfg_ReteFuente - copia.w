&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

{Incluido/Variable.I "SHARED"}

DEFINE VAR W_Ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-MAIN
&Scoped-define BROWSE-NAME brwConceptos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cfg_reteFuente

/* Definitions for BROWSE brwConceptos                                  */
&Scoped-define FIELDS-IN-QUERY-brwConceptos cfg_reteFuente.concepto ~
cfg_reteFuente.tipo cfg_reteFuente.base cfg_reteFuente.porcentaje 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwConceptos 
&Scoped-define QUERY-STRING-brwConceptos FOR EACH cfg_reteFuente NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwConceptos OPEN QUERY brwConceptos FOR EACH cfg_reteFuente NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwConceptos cfg_reteFuente
&Scoped-define FIRST-TABLE-IN-QUERY-brwConceptos cfg_reteFuente


/* Definitions for FRAME F-MAIN                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-MAIN ~
    ~{&OPEN-QUERY-brwConceptos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-Cancelar Btn-Salvar Btn_Done btnEditar ~
RECT-325 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Cancelar 
     IMAGE-UP FILE "imagenes/volver2.bmp":U
     LABEL "Cancela" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn-Salvar 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "Salvar" 
     SIZE 8 BY 1.92.

DEFINE BUTTON btnEditar 
     IMAGE-UP FILE "imagenes/modify.jpg":U
     LABEL "btnagregar 2" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 110.14 BY 26.92.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwConceptos FOR 
      cfg_reteFuente SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwConceptos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwConceptos C-Win _STRUCTURED
  QUERY brwConceptos NO-LOCK DISPLAY
      cfg_reteFuente.concepto COLUMN-LABEL "Concepto" FORMAT "x(30)":U
            LABEL-BGCOLOR 10
      cfg_reteFuente.tipo COLUMN-LABEL "Tipo" FORMAT "x(25)":U
            LABEL-BGCOLOR 10 VIEW-AS COMBO-BOX INNER-LINES 15
                      LIST-ITEM-PAIRS "Salarios"," SAL",
                                      "Honorarios"," HON",
                                      "Servicios"," SRV",
                                      "Arrendamientos"," ARR",
                                      "Redimientos financieros"," RENFIN",
                                      "Compras"," CMP",
                                      "Impuesto a las ventas"," IVA",
                                      "Industria y Comercio"," INDCOM",
                                      "Otras"," OTR"
                      DROP-DOWN-LIST 
      cfg_reteFuente.base COLUMN-LABEL "Base" FORMAT ">>>,>>>,>>9.99":U
            LABEL-BGCOLOR 10
      cfg_reteFuente.porcentaje COLUMN-LABEL "Porcentaje" FORMAT ">>9.9999":U
            WIDTH 33.86 LABEL-BGCOLOR 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108 BY 26.38 ROW-HEIGHT-CHARS .61 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-MAIN
     brwConceptos AT ROW 3.42 COL 3 WIDGET-ID 200
     Btn-Cancelar AT ROW 6.85 COL 113.86 WIDGET-ID 128
     Btn-Salvar AT ROW 3.12 COL 113.86 WIDGET-ID 42
     Btn_Done AT ROW 28.15 COL 113.86 WIDGET-ID 38
     btnEditar AT ROW 4.96 COL 113.86 WIDGET-ID 16
     "               Administración de la configuración para la Retención en la Fuente" VIEW-AS TEXT
          SIZE 120.72 BY 1.08 AT ROW 1.46 COL 2 WIDGET-ID 8
          BGCOLOR 3 
     RECT-325 AT ROW 3.15 COL 1.86 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.72 BY 29.23 WIDGET-ID 100.


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
         TITLE              = "Configuración Parámeros Generales de Créditos"
         HEIGHT             = 29.23
         WIDTH              = 121.72
         MAX-HEIGHT         = 32.35
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 32.35
         VIRTUAL-WIDTH      = 146.29
         SHOW-IN-TASKBAR    = no
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
/* SETTINGS FOR FRAME F-MAIN
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brwConceptos 1 F-MAIN */
/* SETTINGS FOR BROWSE brwConceptos IN FRAME F-MAIN
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwConceptos
/* Query rebuild information for BROWSE brwConceptos
     _TblList          = "bdcentral.cfg_reteFuente"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.cfg_reteFuente.concepto
"concepto" "Concepto" ? "character" ? ? ? 10 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bdcentral.cfg_reteFuente.tipo
"tipo" "Tipo" "x(25)" "character" ? ? ? 10 ? ? no ? no no ? yes no no "U" "" "" "DROP-DOWN-LIST" "," ? "Salarios, SAL,Honorarios, HON,Servicios, SRV,Arrendamientos, ARR,Redimientos financieros, RENFIN,Compras, CMP,Impuesto a las ventas, IVA,Industria y Comercio, INDCOM,Otras, OTR" 15 no 0 no no
     _FldNameList[3]   > bdcentral.cfg_reteFuente.base
"base" "Base" ? "decimal" ? ? ? 10 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > bdcentral.cfg_reteFuente.porcentaje
"porcentaje" "Porcentaje" ? "decimal" ? ? ? 10 ? ? no ? no no "33.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwConceptos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Configuración Parámeros Generales de Créditos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Configuración Parámeros Generales de Créditos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cancelar C-Win
ON CHOOSE OF Btn-Cancelar IN FRAME F-MAIN /* Cancela */
DO:
    RUN inicializar_variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Salvar C-Win
ON CHOOSE OF Btn-Salvar IN FRAME F-MAIN /* Salvar */
DO:
    Grabando:
    DO TRANSACTION ON ERROR UNDO Grabando:
        /*IF cfg_creditos.provisionGeneralCaja:SCREEN-VALUE <> "" AND
           cfg_creditos.provisionGeneralCaja:SCREEN-VALUE <> ? AND
           cfg_creditos.provisionGeneralNomina:SCREEN-VALUE <> "" AND
           cfg_creditos.provisionGeneralNomina:SCREEN-VALUE <> ? AND
           cfg_creditos.montoMinimoRefinanciacion:SCREEN-VALUE <> ? THEN DO:
            ASSIGN FRAME F-MAIN
                cfg_creditos.liquidaMora
                cfg_creditos.provisionGeneralCaja
                cfg_creditos.provisionGeneralNomina
                cfg_creditos.montoMinimoRefinanciacion.

            MESSAGE "La configuración ha sido guardada" SKIP
                    "con éxito."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RUN inicializar_variables.
        END.
        ELSE DO:
            MESSAGE "Todos los parámetros sn obligatorios." SKIP
                    "Revise por favor la configuración..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.*/
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEditar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEditar C-Win
ON CHOOSE OF btnEditar IN FRAME F-MAIN /* btnagregar 2 */
DO:
    brwConceptos:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME F-MAIN /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwConceptos
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
  ENABLE Btn-Cancelar Btn-Salvar Btn_Done btnEditar RECT-325 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-MAIN}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
VIEW FRAME F-Main.

FOR EACH cfg_creditos NO-ERROR.
IF NOT AVAILABLE cfg_creditos THEN
    CREATE cfg_creditos.

DO WITH FRAME f-main:
    /*cfg_creditos.liquidaMora:SCREEN-VALUE = string(cfg_creditos.liquidaMora).
    cfg_creditos.provisionGeneralCaja:SCREEN-VALUE = STRING(cfg_creditos.provisionGeneralCaja).
    cfg_creditos.provisionGeneralNomina:SCREEN-VALUE = STRING(cfg_creditos.provisionGeneralNomina).
    cfg_creditos.montoMinimoRefinanciacion:SCREEN-VALUE = STRING(cfg_creditos.montoMinimoRefinanciacion).

    cfg_creditos.liquidaMora:SENSITIVE = FALSE.
    cfg_creditos.provisionGeneralCaja:SENSITIVE = FALSE.
    cfg_creditos.provisionGeneralNomina:SENSITIVE = FALSE.
    cfg_creditos.montoMinimoRefinanciacion:SENSITIVE = FALSE.*/
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

