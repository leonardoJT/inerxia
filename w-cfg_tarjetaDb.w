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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cfg_tarjetaDb

/* Definitions for FRAME F-MAIN                                         */
&Scoped-define FIELDS-IN-QUERY-F-MAIN Cfg_tarjetaDb.diaCorteCupoRotativo ~
Cfg_tarjetaDb.diaPagoCupoRotativo Cfg_tarjetaDb.plazoCupo ~
Cfg_tarjetaDb.montoCupo Cfg_tarjetaDb.MontoRetirosCajeroDia ~
Cfg_tarjetaDb.MontoRetirosPosDia Cfg_tarjetaDb.NumeroRetirosCajeroDia ~
Cfg_tarjetaDb.NumeroRetirosPosDia 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F-MAIN ~
Cfg_tarjetaDb.diaCorteCupoRotativo Cfg_tarjetaDb.diaPagoCupoRotativo ~
Cfg_tarjetaDb.plazoCupo Cfg_tarjetaDb.montoCupo ~
Cfg_tarjetaDb.MontoRetirosCajeroDia Cfg_tarjetaDb.MontoRetirosPosDia ~
Cfg_tarjetaDb.NumeroRetirosCajeroDia Cfg_tarjetaDb.NumeroRetirosPosDia 
&Scoped-define ENABLED-TABLES-IN-QUERY-F-MAIN Cfg_tarjetaDb
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F-MAIN Cfg_tarjetaDb
&Scoped-define QUERY-STRING-F-MAIN FOR EACH Cfg_tarjetaDb SHARE-LOCK
&Scoped-define OPEN-QUERY-F-MAIN OPEN QUERY F-MAIN FOR EACH Cfg_tarjetaDb SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F-MAIN Cfg_tarjetaDb
&Scoped-define FIRST-TABLE-IN-QUERY-F-MAIN Cfg_tarjetaDb


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Cfg_tarjetaDb.diaCorteCupoRotativo ~
Cfg_tarjetaDb.diaPagoCupoRotativo Cfg_tarjetaDb.plazoCupo ~
Cfg_tarjetaDb.montoCupo Cfg_tarjetaDb.MontoRetirosCajeroDia ~
Cfg_tarjetaDb.MontoRetirosPosDia Cfg_tarjetaDb.NumeroRetirosCajeroDia ~
Cfg_tarjetaDb.NumeroRetirosPosDia 
&Scoped-define ENABLED-TABLES Cfg_tarjetaDb
&Scoped-define FIRST-ENABLED-TABLE Cfg_tarjetaDb
&Scoped-Define ENABLED-OBJECTS tgSaldosGenerados cmbUsuario Btn-Cancelar ~
Btn-Salvar Btn_Done RECT-325 RECT-326 RECT-327 RECT-328 
&Scoped-Define DISPLAYED-FIELDS Cfg_tarjetaDb.diaCorteCupoRotativo ~
Cfg_tarjetaDb.diaPagoCupoRotativo Cfg_tarjetaDb.plazoCupo ~
Cfg_tarjetaDb.montoCupo Cfg_tarjetaDb.MontoRetirosCajeroDia ~
Cfg_tarjetaDb.MontoRetirosPosDia Cfg_tarjetaDb.NumeroRetirosCajeroDia ~
Cfg_tarjetaDb.NumeroRetirosPosDia 
&Scoped-define DISPLAYED-TABLES Cfg_tarjetaDb
&Scoped-define FIRST-DISPLAYED-TABLE Cfg_tarjetaDb
&Scoped-Define DISPLAYED-OBJECTS tgSaldosGenerados cmbUsuario 

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

DEFINE BUTTON btnCambioMasivo 
     LABEL "Cambio masivo -->" 
     SIZE 21.86 BY 1.12.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92
     BGCOLOR 8 .

DEFINE VARIABLE cmbUsuario AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 61 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.57 BY 3.04.

DEFINE RECTANGLE RECT-326
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.57 BY 5.92.

DEFINE RECTANGLE RECT-327
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.57 BY 2.15.

DEFINE RECTANGLE RECT-328
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.57 BY 2.15.

DEFINE VARIABLE tgSaldosGenerados AS LOGICAL INITIAL no 
     LABEL "Saldos generados para el día de hoy" 
     VIEW-AS TOGGLE-BOX
     SIZE 61 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F-MAIN FOR 
      Cfg_tarjetaDb SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-MAIN
     Cfg_tarjetaDb.diaCorteCupoRotativo AT ROW 7.73 COL 39 COLON-ALIGNED WIDGET-ID 358
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     Cfg_tarjetaDb.diaPagoCupoRotativo AT ROW 8.81 COL 39 COLON-ALIGNED WIDGET-ID 354
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     btnCambioMasivo AT ROW 5.58 COL 2.57 WIDGET-ID 352
     tgSaldosGenerados AT ROW 15.5 COL 3 WIDGET-ID 350
     Cfg_tarjetaDb.plazoCupo AT ROW 6.69 COL 39 COLON-ALIGNED WIDGET-ID 342
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     cmbUsuario AT ROW 12.46 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 340
     Cfg_tarjetaDb.montoCupo AT ROW 5.58 COL 39 COLON-ALIGNED WIDGET-ID 330
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     Cfg_tarjetaDb.MontoRetirosCajeroDia AT ROW 2.12 COL 27.72 COLON-ALIGNED WIDGET-ID 318
          LABEL "Monto Max. Retiros Día......."
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Cfg_tarjetaDb.MontoRetirosPosDia AT ROW 2.08 COL 45.72 COLON-ALIGNED NO-LABEL WIDGET-ID 326
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Cfg_tarjetaDb.NumeroRetirosCajeroDia AT ROW 3.19 COL 35.14 COLON-ALIGNED WIDGET-ID 320
          LABEL "Num. Retiros Día.............................."
          VIEW-AS FILL-IN 
          SIZE 9.57 BY 1
     Cfg_tarjetaDb.NumeroRetirosPosDia AT ROW 3.23 COL 53 COLON-ALIGNED NO-LABEL WIDGET-ID 328
          VIEW-AS FILL-IN 
          SIZE 9.57 BY 1
     Btn-Cancelar AT ROW 3.27 COL 67 WIDGET-ID 128
     Btn-Salvar AT ROW 1.38 COL 67 WIDGET-ID 42
     Btn_Done AT ROW 5.15 COL 67.14 WIDGET-ID 38
     "Topes Transacciones                       Cajeros                            POS" VIEW-AS TEXT
          SIZE 62.29 BY .62 AT ROW 1.23 COL 2.72 WIDGET-ID 322
          BGCOLOR 3 
     "Cupo Rotativo" VIEW-AS TEXT
          SIZE 22.29 BY .62 AT ROW 4.69 COL 2.72 WIDGET-ID 334
          BGCOLOR 3 
     "Usuario Administrador" VIEW-AS TEXT
          SIZE 22.29 BY .62 AT ROW 11.42 COL 2.72 WIDGET-ID 338
          BGCOLOR 3 
     "Meses" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 6.88 COL 52.86 WIDGET-ID 344
     "Generación de saldos" VIEW-AS TEXT
          SIZE 22.29 BY .62 AT ROW 14.38 COL 2.72 WIDGET-ID 348
          BGCOLOR 3 
     RECT-325 AT ROW 1.46 COL 1.43 WIDGET-ID 14
     RECT-326 AT ROW 5.04 COL 1.43 WIDGET-ID 332
     RECT-327 AT ROW 11.77 COL 1.43 WIDGET-ID 336
     RECT-328 AT ROW 14.73 COL 1.43 WIDGET-ID 346
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 75.29 BY 16.38 WIDGET-ID 100.


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
         TITLE              = "Configuración Parámeros Tarjeta Débito"
         HEIGHT             = 16.38
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
/* SETTINGS FOR BUTTON btnCambioMasivo IN FRAME F-MAIN
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_tarjetaDb.MontoRetirosCajeroDia IN FRAME F-MAIN
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Cfg_tarjetaDb.MontoRetirosPosDia IN FRAME F-MAIN
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Cfg_tarjetaDb.NumeroRetirosCajeroDia IN FRAME F-MAIN
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Cfg_tarjetaDb.NumeroRetirosPosDia IN FRAME F-MAIN
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-MAIN
/* Query rebuild information for FRAME F-MAIN
     _TblList          = "bdcentral.Cfg_tarjetaDb"
     _Query            is OPENED
*/  /* FRAME F-MAIN */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Configuración Parámeros Tarjeta Débito */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Configuración Parámeros Tarjeta Débito */
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
    /* Validaciones */
    IF INTEGER(cfg_tarjetaDB.diaPagoCupoRotativo:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "El día de pago para el Cupo Rotativo es inconsistente." SKIP
                "Revise por favor..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    IF INTEGER(cfg_tarjetaDB.diaCorteCupoRotativo:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "El día de corte para el Cupo Rotativo es inconsistente." SKIP
                "Revise por favor..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.
    /* ----------------------- */
    
    Grabando:
    DO TRANSACTION ON ERROR UNDO Grabando:
        FIND FIRST cfg_tarjetaDb NO-ERROR.
        IF NOT AVAILABLE cfg_tarjetaDB THEN
            CREATE cfg_tarjetaDb.

        DO WITH FRAME F-MAIN:
            ASSIGN FRAME F-MAIN
                Cfg_TarjetaDb.MontoRetirosCajeroDia
                Cfg_TarjetaDb.NumeroRetirosCajeroDia
                Cfg_TarjetaDb.MontoRetirosPosDia
                Cfg_TarjetaDb.NumeroRetirosPosDia
                cfg_tarjetaDB.montoCupo
                cfg_tarjetaDB.plazoCupo
                cfg_tarjetaDB.diaPagoCupoRotativo
                cfg_tarjetaDB.diaCorteCupoRotativo.

            btnCambioMasivo:SENSITIVE = TRUE.

            FIND FIRST usuarios WHERE usuarios.usuario = cmbusuario:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF AVAILABLE usuarios THEN
                cfg_tarjetaDB.usuarioAdministrador = cmbusuario:SCREEN-VALUE.
            ELSE
                cfg_tarjetaDB.usuarioAdministrador = ?.

            IF tgSaldosGenerados:SCREEN-VALUE = "no" THEN DO:
                FIND FIRST procDia WHERE procDia.cod_proceso = 14
                                     AND procDia.fecha_proc = w_fecha
                                     AND procDia.estado = 2 NO-ERROR.
                IF AVAILABLE procDia THEN
                    procDia.estado = 1.
            END.
            ELSE DO:
                FIND FIRST procDia WHERE procDia.cod_proceso = 14
                                     AND procDia.fecha_proc = w_fecha
                                     AND procDia.estado = 1 NO-ERROR.
                IF AVAILABLE procDia THEN
                    procDia.estado = 2.
            END.
        END.

        RELEASE cfg_tarjetaDb.

        FIND FIRST Cfg_tarjetaDb NO-LOCK NO-ERROR.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCambioMasivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCambioMasivo C-Win
ON CHOOSE OF btnCambioMasivo IN FRAME F-MAIN /* Cambio masivo --> */
DO:
    MESSAGE "Desea cambiar de forma masiva el cupo" SKIP
            "para todos los Asociados? (El nuevo cupo" SKIP
            "será de" STRING(cfg_tarjetaDB.montoCupo,"$>>>,>>>,>>9") ")"
        VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "Cambio masivo de cupos rotativos" UPDATE cambioMasivo AS LOGICAL.

    IF cambioMasivo = NO THEN
        RETURN.

    FOR EACH creditos WHERE creditos.cod_credito = 123 AND estado = 2:
        creditos.monto = cfg_tarjetaDB.montoCupo.
        creditos.val_desembolso = cfg_tarjetaDB.montoCupo.
    END.

    MESSAGE "El cambio de cupo fue realizado" SKIP
            "de forma exitosa"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
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


&Scoped-define SELF-NAME Cfg_tarjetaDb.diaCorteCupoRotativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_tarjetaDb.diaCorteCupoRotativo C-Win
ON LEAVE OF Cfg_tarjetaDb.diaCorteCupoRotativo IN FRAME F-MAIN /* Día de Corte Cupo Rotativo */
DO:
    IF INTEGER(cfg_tarjetaDB.diaCorteCupoRotativo:SCREEN-VALUE) <= 0 OR INTEGER(cfg_tarjetaDB.diaCorteCupoRotativo:SCREEN-VALUE) > 31 THEN DO:
        MESSAGE "El día de corte debe ser un día de mes válido." SKIP
                "Revise por favor..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
        cfg_tarjetaDB.diaCorteCupoRotativo:SCREEN-VALUE = "0".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_tarjetaDb.diaPagoCupoRotativo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_tarjetaDb.diaPagoCupoRotativo C-Win
ON LEAVE OF Cfg_tarjetaDb.diaPagoCupoRotativo IN FRAME F-MAIN /* Día de pago Cupo Rotativo */
DO:
    IF INTEGER(cfg_tarjetaDB.diaPagoCupoRotativo:SCREEN-VALUE) <= 0 OR INTEGER(cfg_tarjetaDB.diaPagoCupoRotativo:SCREEN-VALUE) > 31 THEN DO:
        MESSAGE "El día de pago debe ser un día de mes válido." SKIP
                "Revise por favor..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
        cfg_tarjetaDB.diaPagoCupoRotativo:SCREEN-VALUE = "0".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgSaldosGenerados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgSaldosGenerados C-Win
ON VALUE-CHANGED OF tgSaldosGenerados IN FRAME F-MAIN /* Saldos generados para el día de hoy */
DO:
    IF SELF:SCREEN-VALUE = "no" THEN
        MESSAGE "Se marcará el proceso de generación de saldos como" SKIP
                "no realizado. Haga clic en el botón Grabar sólo si" SKIP
                "está seguro"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
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

  {&OPEN-QUERY-F-MAIN}
  GET FIRST F-MAIN.
  DISPLAY tgSaldosGenerados cmbUsuario 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  IF AVAILABLE Cfg_tarjetaDb THEN 
    DISPLAY Cfg_tarjetaDb.diaCorteCupoRotativo Cfg_tarjetaDb.diaPagoCupoRotativo 
          Cfg_tarjetaDb.plazoCupo Cfg_tarjetaDb.montoCupo 
          Cfg_tarjetaDb.MontoRetirosCajeroDia Cfg_tarjetaDb.MontoRetirosPosDia 
          Cfg_tarjetaDb.NumeroRetirosCajeroDia Cfg_tarjetaDb.NumeroRetirosPosDia 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  ENABLE Cfg_tarjetaDb.diaCorteCupoRotativo Cfg_tarjetaDb.diaPagoCupoRotativo 
         tgSaldosGenerados Cfg_tarjetaDb.plazoCupo cmbUsuario 
         Cfg_tarjetaDb.montoCupo Cfg_tarjetaDb.MontoRetirosCajeroDia 
         Cfg_tarjetaDb.MontoRetirosPosDia Cfg_tarjetaDb.NumeroRetirosCajeroDia 
         Cfg_tarjetaDb.NumeroRetirosPosDia Btn-Cancelar Btn-Salvar Btn_Done 
         RECT-325 RECT-326 RECT-327 RECT-328 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-MAIN}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
HIDE FRAME Frm-ComAdicional.
HIDE FRAME Frm-ODBC.
VIEW FRAME F-Main.

FIND FIRST Cfg_tarjetaDb NO-LOCK NO-ERROR.
IF NOT AVAILABLE Cfg_tarjetaDb THEN
    CREATE cfg_tarjetaDB.

DO WITH FRAME f-main:
    ASSIGN Cfg_tarjetaDb.MontoRetirosCajeroDia:SCREEN-VALUE = STRING(Cfg_tarjetaDb.MontoRetirosCajeroDia,">>>,>>>,>>9")
           Cfg_tarjetaDb.NumeroRetirosCajeroDia:SCREEN-VALUE = STRING(Cfg_tarjetaDb.NumeroRetirosCajeroDia,">>9")
           Cfg_tarjetaDb.MontoRetirosPosDia:SCREEN-VALUE = STRING(Cfg_tarjetaDb.MontoRetirosPosDia,">>>,>>>,>>9")
           Cfg_tarjetaDb.NumeroRetirosPosDia:SCREEN-VALUE = STRING(Cfg_tarjetaDb.NumeroRetirosPosDia,">>9")
           Cfg_tarjetaDb.montoCupo:SCREEN-VALUE = STRING(Cfg_tarjetaDb.montoCupo,">>>,>>>,>>9.99")
           cfg_tarjetaDB.plazoCupo:SCREEN-VALUE = STRING(cfg_tarjetaDB.plazoCupo,">>9").

    FIND FIRST usuarios WHERE usuarios.usuario = cfg_TarjetaDB.usuarioAdministrador NO-LOCK NO-ERROR.
    IF AVAILABLE usuarios THEN
        cmbUsuario:SCREEN-VALUE = usuarios.usuario.
    ELSE
        cmbUsuario:SCREEN-VALUE = "".

    FIND FIRST procDia WHERE procdia.cod_proceso = 14
                          AND procdia.fecha_proc = w_fecha
                          AND procdia.estado = 2 NO-ERROR.
    IF AVAILABLE procdia THEN
        tgSaldosGenerados:SCREEN-VALUE = "yes".

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

