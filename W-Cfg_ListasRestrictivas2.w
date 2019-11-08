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
DEFINE VAR W_Cta AS CHARACTER.
DEFINE VAR W_NomPC AS CHARACTER.
DEFINE VAR W_Nat AS CHARACTER.
DEFINE VAR W_Ctr AS CHARACTER.
DEFINE VARIABLE TextoLC AS LONGCHAR   NO-UNDO.
DEF VAR W_Sarlaft AS LOG INIT FALSE.
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VAR Archivotxt AS CHARACTER.

DEFINE VAR esOficialDeCumplimiento AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-MAIN
&Scoped-define BROWSE-NAME brwListas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cfg_listasSarlaft

/* Definitions for BROWSE brwListas                                     */
&Scoped-define FIELDS-IN-QUERY-brwListas cfg_listasSarlaft.Lista ~
cfg_listasSarlaft.Tipo ~
IF (cfg_listasSarlaft.Estado = 1) THEN ("Activa") ELSE ("Inactiva") ~
cfg_listasSarlaft.created_at cfg_listasSarlaft.updated_at ~
cfg_listasSarlaft.Usuario 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwListas 
&Scoped-define QUERY-STRING-brwListas FOR EACH cfg_listasSarlaft SHARE-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwListas OPEN QUERY brwListas FOR EACH cfg_listasSarlaft SHARE-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwListas cfg_listasSarlaft
&Scoped-define FIRST-TABLE-IN-QUERY-brwListas cfg_listasSarlaft


/* Definitions for FRAME F-MAIN                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-MAIN ~
    ~{&OPEN-QUERY-brwListas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwListas btnCrear Btn_Done RECT-325 
&Scoped-Define DISPLAYED-OBJECTS nombreLista Tipo_fill Estado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCrear 
     IMAGE-UP FILE "imagenes/agregar.jpg":U
     LABEL "Crear" 
     SIZE 8 BY 1.92.

DEFINE BUTTON btnDeshacer 
     IMAGE-UP FILE "imagenes/deshacer.bmp":U
     LABEL "Cancelar" 
     SIZE 8 BY 1.88.

DEFINE BUTTON btnEditar 
     IMAGE-UP FILE "imagenes/modify.jpg":U
     LABEL "Editar" 
     SIZE 8 BY 1.92.

DEFINE BUTTON btnGuardar 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "Guardar" 
     SIZE 8 BY 1.88.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92
     BGCOLOR 8 .

DEFINE VARIABLE Estado AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Activa",1,
                     "Inactiva",2
     DROP-DOWN-LIST
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE Tipo_fill AS CHARACTER FORMAT "x(25)" 
     LABEL "Tipo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Restrictiva","RES",
                     "Informativa","INF"
     DROP-DOWN-LIST
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE nombreLista AS CHARACTER FORMAT "X(15)":U 
     LABEL "Nombre de la lista" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 130.14 BY 20.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwListas FOR 
      cfg_listasSarlaft SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwListas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwListas C-Win _STRUCTURED
  QUERY brwListas SHARE-LOCK NO-WAIT DISPLAY
      cfg_listasSarlaft.Lista FORMAT "x(30)":U WIDTH 34.43
      cfg_listasSarlaft.Tipo FORMAT "x(8)":U WIDTH 7.86
      IF (cfg_listasSarlaft.Estado = 1) THEN ("Activa") ELSE ("Inactiva") COLUMN-LABEL "Estado"
      cfg_listasSarlaft.created_at COLUMN-LABEL "Fecha creación" FORMAT "99/99/9999 HH:MM:SS":U
      cfg_listasSarlaft.updated_at COLUMN-LABEL "Fecha modificación" FORMAT "99/99/9999 HH:MM:SS":U
      cfg_listasSarlaft.Usuario FORMAT "x(60)":U WIDTH 33.43
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 126.86 BY 15.35 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-MAIN
     brwListas AT ROW 7.19 COL 3 WIDGET-ID 200
     nombreLista AT ROW 3.42 COL 19.29 COLON-ALIGNED WIDGET-ID 148
     btnDeshacer AT ROW 3.42 COL 113 WIDGET-ID 146
     btnGuardar AT ROW 3.42 COL 104 WIDGET-ID 144
     Tipo_fill AT ROW 4.73 COL 19.29 COLON-ALIGNED WIDGET-ID 140
     Estado AT ROW 5.85 COL 19.29 COLON-ALIGNED WIDGET-ID 170
     btnCrear AT ROW 3.42 COL 86 WIDGET-ID 42
     Btn_Done AT ROW 3.42 COL 122 WIDGET-ID 38
     btnEditar AT ROW 3.42 COL 95 WIDGET-ID 16
     "Administración de Listas restrictivas" VIEW-AS TEXT
          SIZE 130 BY 1.08 AT ROW 1.46 COL 2 WIDGET-ID 8
          BGCOLOR 3 
     RECT-325 AT ROW 2.88 COL 1.86 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 144.72 BY 25.08 WIDGET-ID 100.


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
         TITLE              = "Configuración"
         HEIGHT             = 22.42
         WIDTH              = 132.29
         MAX-HEIGHT         = 32.35
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 32.35
         VIRTUAL-WIDTH      = 194.86
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
/* BROWSE-TAB brwListas 1 F-MAIN */
/* SETTINGS FOR BUTTON btnDeshacer IN FRAME F-MAIN
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnEditar IN FRAME F-MAIN
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnGuardar IN FRAME F-MAIN
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Estado IN FRAME F-MAIN
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nombreLista IN FRAME F-MAIN
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Tipo_fill IN FRAME F-MAIN
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwListas
/* Query rebuild information for BROWSE brwListas
     _TblList          = "bdcentral.cfg_listasSarlaft"
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.cfg_listasSarlaft.Lista
"Lista" ? ? "character" ? ? ? ? ? ? no ? no no "34.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bdcentral.cfg_listasSarlaft.Tipo
"Tipo" ? ? "character" ? ? ? ? ? ? no ? no no "7.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"IF (cfg_listasSarlaft.Estado = 1) THEN (""Activa"") ELSE (""Inactiva"")" "Estado" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > bdcentral.cfg_listasSarlaft.created_at
"created_at" "Fecha creación" "99/99/9999 HH:MM:SS" "datetime" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > bdcentral.cfg_listasSarlaft.updated_at
"updated_at" "Fecha modificación" "99/99/9999 HH:MM:SS" "datetime" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > bdcentral.cfg_listasSarlaft.Usuario
"Usuario" ? ? "character" ? ? ? ? ? ? no ? no no "33.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwListas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Configuración */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Configuración */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwListas
&Scoped-define SELF-NAME brwListas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwListas C-Win
ON MOUSE-SELECT-CLICK OF brwListas IN FRAME F-MAIN
DO:
    IF AVAILABLE cfg_listasSarlaft THEN DO:
        NombreLista:SCREEN-VALUE IN FRAME F-Main = cfg_listasSarlaft.lista.
        tipo_fill:SCREEN-VALUE = cfg_listasSarlaft.tipo.
        estado:SCREEN-VALUE = estado:ENTRY(cfg_listasSarlaft.estado).
        btnEditar:SENSITIVE = TRUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCrear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCrear C-Win
ON CHOOSE OF btnCrear IN FRAME F-MAIN /* Crear */
DO:
    IF AVAILABLE cfg_listasSarlaft THEN
        RELEASE cfg_listasSarlaft.

    nombreLista:SCREEN-VALUE = ''.
    nombreLista:SENSITIVE = TRUE.

    tipo_fill:SCREEN-VALUE = 'RES'.
    tipo_fill:SENSITIVE = TRUE.

    estado:SCREEN-VALUE = "1".
    
    brwListas:SENSITIVE = FALSE.
    btnDeshacer:SENSITIVE = TRUE.
    SELF:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeshacer C-Win
ON CHOOSE OF btnDeshacer IN FRAME F-MAIN /* Cancelar */
DO:
    IF AVAILABLE cfg_listasSarlaft THEN DO:
        NombreLista:SCREEN-VALUE IN FRAME F-Main = cfg_listasSarlaft.lista.
        tipo_fill:SCREEN-VALUE = cfg_listasSarlaft.tipo.
        estado:SCREEN-VALUE = estado:ENTRY(cfg_listasSarlaft.estado).
    END.
    ELSE DO:
        NombreLista:SCREEN-VALUE IN FRAME F-Main = "".
        tipo_fill:SCREEN-VALUE = "RES".
        estado:SCREEN-VALUE = "".
    END.

    btnEditar:SENSITIVE = FALSE.
    btnGuardar:SENSITIVE = FALSE.
    btnDeshacer:SENSITIVE = FALSE.
    nombreLista:SENSITIVE = FALSE.
    tipo_fill:SENSITIVE = FALSE.
    estado:SENSITIVE = FALSE.
    brwListas:SENSITIVE = TRUE.
    btnCrear:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEditar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEditar C-Win
ON CHOOSE OF btnEditar IN FRAME F-MAIN /* Editar */
DO:
    nombreLista:SENSITIVE = TRUE.
    tipo_fill:SENSITIVE = TRUE.
    estado:SENSITIVE = TRUE.

    btnGuardar:SENSITIVE = TRUE.
    btnDeshacer:SENSITIVE = TRUE.
    btnEditar:SENSITIVE = FALSE.
    brwListas:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGuardar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGuardar C-Win
ON CHOOSE OF btnGuardar IN FRAME F-MAIN /* Guardar */
DO:
    DEFINE VAR flagOkLista AS LOGICAL INITIAL TRUE.
    DEFINE VAR flagNuevaLista AS LOGICAL.

    IF AVAILABLE cfg_listasSarlaft THEN
        MESSAGE "Está seguro que desea modificar la lista" STRING(nombreLista:SCREEN-VALUE) + "?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirma creación de Lista" UPDATE flagOkLista.
    ELSE DO:
        MESSAGE "Está seguro que desea crear la lista" STRING(nombreLista:SCREEN-VALUE) + "?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirma creación de Lista" UPDATE flagOkLista.

        flagNuevaLista = TRUE.
    END.

    IF flagOkLista = FALSE THEN
        RETURN NO-APPLY.

    IF flagNuevaLista = TRUE THEN
        CREATE cfg_listasSarlaft.

    cfg_listasSarlaft.lista = STRING(nombreLista:SCREEN-VALUE).
    cfg_listasSarlaft.tipo = STRING(Tipo_fill:SCREEN-VALUE).
    cfg_listasSarlaft.estado = integer(Estado:SCREEN-VALUE).
    cfg_listasSarlaft.updated_at = NOW.
    cfg_listasSarlaft.usuario = W_Usuario.
    
    IF flagNuevaLista = TRUE THEN DO:
        RUN escribirLog IN w_manija (INPUT w_usuario,
                                     INPUT "Crea Lista Restrictiva " + STRING(nombreLista:SCREEN-VALUE) + " de tipo " + STRING(Tipo_fill:SCREEN-VALUE)).

        MESSAGE "La Lista fue creada con éxito"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO:
        RUN escribirLog IN w_manija (INPUT w_usuario,
                                     INPUT "Modifica Lista Restrictiva " + STRING(nombreLista:SCREEN-VALUE) + " de tipo " + STRING(Tipo_fill:SCREEN-VALUE)).

        MESSAGE "La Lista fue modificada con éxito"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    APPLY 'choose' TO btnDeshacer.
       
    OPEN QUERY brwListas FOR EACH cfg_listasSarlaft SHARE-LOCK INDEXED-REPOSITION.
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


&Scoped-define SELF-NAME Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Estado C-Win
ON VALUE-CHANGED OF Estado IN FRAME F-MAIN /* Estado */
DO:
    IF nombreLista:SCREEN-VALUE <> "" AND
       tipo_fill:SCREEN-VALUE <> ? THEN
        btnGuardar:SENSITIVE = TRUE.
    ELSE
        btnGuardar:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nombreLista
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nombreLista C-Win
ON VALUE-CHANGED OF nombreLista IN FRAME F-MAIN /* Nombre de la lista */
DO:
    IF nombreLista:SCREEN-VALUE <> "" AND
       tipo_fill:SCREEN-VALUE <> ? THEN
        btnGuardar:SENSITIVE = TRUE.
    ELSE
        btnGuardar:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tipo_fill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tipo_fill C-Win
ON VALUE-CHANGED OF Tipo_fill IN FRAME F-MAIN /* Tipo */
DO:
    IF nombreLista:SCREEN-VALUE <> "" THEN
        btnGuardar:SENSITIVE = TRUE.
    ELSE
        btnGuardar:SENSITIVE = FALSE.
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
    
    btnGuardar:SENSITIVE = FALSE.

    /* Validamos si el Usuario està configurado como Oficial de Cumplimiento */
    FIND FIRST cfg_sarlaft NO-LOCK NO-ERROR.
    IF AVAILABLE cfg_sarlaft THEN DO:
        IF cfg_sarlaft.usuarioOficialDeCumplimiento = w_usuario THEN
            esOficialDeCumplimiento = TRUE.
        ELSE DO:
            /*btnCrear:SENSITIVE = FALSE.
            btnCrear:VISIBLE = FALSE.
            btnEditar:SENSITIVE = FALSE.
            btnEditar:VISIBLE = FALSE.
            btnGuardar:SENSITIVE = FALSE.
            btnGuardar:VISIBLE = FALSE.
            btnDeshacer:SENSITIVE = FALSE.
            btnDeshacer:VISIBLE = FALSE.*/
        END.
    END.

    tipo_fill:SCREEN-VALUE = "RES".

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
  DISPLAY nombreLista Tipo_fill Estado 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  ENABLE brwListas btnCrear Btn_Done RECT-325 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-MAIN}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

