&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

{Incluido/Variable.I "SHARED"}

DEFINE VAR W_Ok AS LOGICAL NO-UNDO.

/* oakley */

DEFI VAR W-Red        LIKE redes.red  INITIAL 1     NO-UNDO.
DEFI VAR W-CodAho     LIKE Ahorros.cod_Ahorro       NO-UNDO.
DEFINE VAR w-codAho2 LIKE ahorros.cod_ahorro NO-UNDO.
DEFI VAR W-CodCre     LIKE Creditos.Cod_Credito     NO-UNDO.
DEFI VAR W-CodCre2    LIKE Creditos.Cod_Credito     NO-UNDO.
DEFI VAR W-CbteAho    LIKE Comprobantes.comprobante NO-UNDO.
DEFI VAR W-CbteCupo   LIKE Comprobantes.Comprobante NO-UNDO.
DEFI VAR W-VariosTipo LIKE Cfg_Varios.Tipo          NO-UNDO.
DEFI VAR W-AgeAdmon   LIKE Agencias.Agencia         NO-UNDO.
DEFI VAR W-usuAdmon   LIKE Usuarios.Usuario         NO-UNDO.
DEFI VAR choice     AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-MAIN

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-Cancelar Btn-Salvar Btn_Done btnEditar ~
RECT-327 
&Scoped-Define DISPLAYED-OBJECTS txtNombreDeUsuario cmbUsuario 

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

DEFINE VARIABLE cmbUsuario AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 18 BY .92 NO-UNDO.

DEFINE VARIABLE txtNombreDeUsuario AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-327
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.57 BY 2.15.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-MAIN
     txtNombreDeUsuario AT ROW 2.23 COL 20 COLON-ALIGNED NO-LABEL WIDGET-ID 342
     cmbUsuario AT ROW 2.23 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 340
     Btn-Cancelar AT ROW 5.15 COL 67 WIDGET-ID 128
     Btn-Salvar AT ROW 1.38 COL 67 WIDGET-ID 42
     Btn_Done AT ROW 7.04 COL 67.14 WIDGET-ID 38
     btnEditar AT ROW 3.27 COL 67 WIDGET-ID 16
     "Usuario Oficial de Cumplimiento" VIEW-AS TEXT
          SIZE 30.29 BY .62 AT ROW 1.19 COL 2.72 WIDGET-ID 338
          BGCOLOR 3 
     RECT-327 AT ROW 1.54 COL 1.43 WIDGET-ID 336
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 75.29 BY 8.65 WIDGET-ID 100.


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
         TITLE              = "Configuración Parámeros SARLAFT"
         HEIGHT             = 8.08
         WIDTH              = 74.86
         MAX-HEIGHT         = 27.38
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.38
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
/* SETTINGS FOR COMBO-BOX cmbUsuario IN FRAME F-MAIN
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtNombreDeUsuario IN FRAME F-MAIN
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Configuración Parámeros SARLAFT */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Configuración Parámeros SARLAFT */
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
    MESSAGE "Está seguro que desea guardar los cambios" SKIP
            "realizados...?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE flagGrabar AS LOGICAL.

    IF flagGrabar = NO THEN
        RETURN NO-APPLY.

    /* Validaciones */
    Grabando:
    DO TRANSACTION ON ERROR UNDO Grabando:
        FIND FIRST usuarios WHERE usuarios.usuario = cmbusuario:SCREEN-VALUE
                              AND usuarios.estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE usuarios THEN DO:
            FIND FIRST cfg_sarlaft NO-ERROR.
            cfg_sarlaft.usuarioOficialDeCumplimiento = cmbusuario:SCREEN-VALUE.
        END.
        ELSE DO:
            MESSAGE "Debe seleccionarse un Usuario Activo con el rol de" SKIP
                    "Oficial de Cumplimiento. Revise por favor!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN NO-APPLY.
        END.

        RELEASE cfg_sarlaft.

        FIND FIRST cfg_sarlaft NO-LOCK NO-ERROR.

        cmbUsuario:SENSITIVE = FALSE.

        MESSAGE "Los cambios fueron realizados con éxito"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.


    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEditar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEditar C-Win
ON CHOOSE OF btnEditar IN FRAME F-MAIN /* btnagregar 2 */
DO:
    cmbUsuario:SENSITIVE = TRUE.
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


&Scoped-define SELF-NAME cmbUsuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbUsuario C-Win
ON VALUE-CHANGED OF cmbUsuario IN FRAME F-MAIN
DO:
    FIND FIRST usuarios WHERE usuarios.usuario = cmbusuario:SCREEN-VALUE NO-LOCK NO-ERROR.
    
    txtNombreDeUsuario:SCREEN-VALUE = usuarios.nombre.
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

    FOR EACH usuarios WHERE usuarios.Estado = 1 NO-LOCK:
        W_Ok = cmbUsuario:ADD-LAST(usuarios.usuario).
    END.

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
  DISPLAY txtNombreDeUsuario cmbUsuario 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  ENABLE Btn-Cancelar Btn-Salvar Btn_Done btnEditar RECT-327 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-MAIN}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
FIND FIRST cfg_sarlaft NO-LOCK NO-ERROR.
IF NOT AVAILABLE cfg_sarlaft THEN
    CREATE cfg_sarlaft.

DO WITH FRAME f-main:
    FIND FIRST usuarios WHERE usuarios.usuario = cfg_sarlaft.UsuarioOficialDeCumplimiento NO-LOCK NO-ERROR.
    IF AVAILABLE usuarios THEN DO:
        cmbUsuario:SCREEN-VALUE = usuarios.usuario.
        txtNombreDeUsuario:SCREEN-VALUE = usuarios.nombre.
    END.
    ELSE DO:
        cmbUsuario:SCREEN-VALUE = "".
        txtNombreDeUsuario:SCREEN-VALUE = "".
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

