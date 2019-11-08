&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

  DEFINE SHARED VARIABLE W_Nom_Agencia AS CHAR FORMAT "X(60)".
  DEFINE SHARED VARIABLE W_Fecha         AS DATE FORMAT "99/99/9999".
  DEFINE SHARED VARIABLE W_Usuario     LIKE Usuarios.Usuario.

  DEFINE VAR W_Estacion LIKE Estaciones.Estacion.
  DEFINE VAR Hora AS CHARACTER FORMAT "X(8)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Logs

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 logs.fecha hora logs.observacion logs.estado /*FORMAT "Si/No"*/   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 logs.estado   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 logs
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 logs
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define OPEN-QUERY-BROWSE-2 /* OPEN QUERY {&SELF-NAME} FOR EACH Logs WHERE Logs.Usuario EQ W_Usuario AND Logs.Estado EQ YES . */.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 Logs
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 Logs


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-5 B-Filtro BUTTON-9 BUTTON-7 BROWSE-2 ~
EDITOR-Obs 
&Scoped-Define DISPLAYED-OBJECTS W_AgeNom W_EstacionWk W_Usu W_Fec W_Nom ~
W_Hor EDITOR-Obs 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Filtro 
     IMAGE-UP FILE "imagenes/magnify0.ico":U
     LABEL "Filtro" 
     SIZE 11 BY 1.62 TOOLTIP "Filtro".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes\manomensaje":U
     LABEL "Button 5" 
     SIZE 11 BY 1.62 TOOLTIP "Ver los mensajes no leidos".

DEFINE BUTTON BUTTON-7 
     LABEL "Ocultar" 
     SIZE 11 BY 1.12.

DEFINE BUTTON BUTTON-9 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Button 9" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE EDITOR-Obs AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 91 BY 2.69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AgeNom AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_EstacionWk AS CHARACTER FORMAT "X(80)":U 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Fec AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Hor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hora" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Nom AS CHARACTER FORMAT "X(30)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Usu AS CHARACTER FORMAT "X(4)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-exitF 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Cierra Filtros" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-Update 
     IMAGE-UP FILE "imagenes/proceso_g.bmp":U
     LABEL "Actualizar" 
     SIZE 8 BY 1.62 TOOLTIP "Actualizar".

DEFINE VARIABLE Fec_Fin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fec_Ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      Logs SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 DISPLAY
      logs.fecha             COLUMN-LABEL "Fecha"
     hora               COLUMN-LABEL "Hora" FORMAT "x(8)"
     logs.observacion   COLUMN-LABEL "Observaciones" FORMAT "X(90)"
     logs.estado             COLUMN-LABEL "Estado" /*FORMAT "Si/No"*/
     ENABLE logs.estado
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91 BY 7.81
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     W_AgeNom AT ROW 1.27 COL 3 NO-LABEL
     BUTTON-5 AT ROW 1.27 COL 81
     W_EstacionWk AT ROW 2.35 COL 1 COLON-ALIGNED NO-LABEL
     B-Filtro AT ROW 2.88 COL 81
     W_Usu AT ROW 3.42 COL 9 COLON-ALIGNED
     W_Fec AT ROW 3.42 COL 48 COLON-ALIGNED
     W_Nom AT ROW 4.5 COL 9 COLON-ALIGNED
     W_Hor AT ROW 4.5 COL 48 COLON-ALIGNED
     BUTTON-9 AT ROW 4.5 COL 81 WIDGET-ID 4
     BUTTON-7 AT ROW 6.12 COL 81
     BROWSE-2 AT ROW 8 COL 2 WIDGET-ID 300
     EDITOR-Obs AT ROW 16.62 COL 2 NO-LABEL WIDGET-ID 2
     "Mensajes Activos" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 6.65 COL 2
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93.72 BY 19
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Filtros
     Fec_Ini AT ROW 1.27 COL 5 COLON-ALIGNED
     Fec_Fin AT ROW 2.35 COL 5 COLON-ALIGNED
     BUTTON-Update AT ROW 1.31 COL 29 WIDGET-ID 2
     BUTTON-exitF AT ROW 2.88 COL 29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 41 ROW 1.27
         SIZE 37 BY 4.85
         BGCOLOR 17 FONT 4
         TITLE "Filtros" WIDGET-ID 200.


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
         TITLE              = "Información del Día"
         COLUMN             = 43.57
         ROW                = 6.62
         HEIGHT             = 5.12
         WIDTH              = 93.72
         MAX-HEIGHT         = 23.54
         MAX-WIDTH          = 105.29
         VIRTUAL-HEIGHT     = 23.54
         VIRTUAL-WIDTH      = 105.29
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
IF NOT C-Win:LOAD-ICON("imagenes\desktop":U) THEN
    MESSAGE "Unable to load icon: imagenes\desktop"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Filtros:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 BUTTON-7 DEFAULT-FRAME */
ASSIGN 
       EDITOR-Obs:AUTO-INDENT IN FRAME DEFAULT-FRAME      = TRUE
       EDITOR-Obs:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE
       EDITOR-Obs:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN W_AgeNom IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_EstacionWk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Fec IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Hor IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Nom IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Usu IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Filtros
   Custom                                                               */
ASSIGN 
       FRAME F_Filtros:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
/* OPEN QUERY {&SELF-NAME} FOR EACH Logs WHERE Logs.Usuario EQ W_Usuario AND Logs.Estado EQ YES . */
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Información del Día */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Información del Día */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Filtro C-Win
ON CHOOSE OF B-Filtro IN FRAME DEFAULT-FRAME /* Filtro */
DO:
    VIEW FRAME F_Filtros.
    APPLY "ENTRY":U TO Fec_Ini IN FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON ITERATION-CHANGED OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
    ASSIGN EDITOR-Obs:SCREEN-VALUE = Logs.Observacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON ROW-DISPLAY OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
  Hora = STRING(Logs.HoraE, "hh:mm am").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
ON CHOOSE OF BUTTON-5 IN FRAME DEFAULT-FRAME /* Button 5 */
DO:
    APPLY "ITERATION-CHANGED":U TO browse-2.
  c-win:HEIGHT = 19.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 C-Win
ON CHOOSE OF BUTTON-7 IN FRAME DEFAULT-FRAME /* Ocultar */
DO:
  c-win:HEIGHT = 5.12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 C-Win
ON CHOOSE OF BUTTON-9 IN FRAME DEFAULT-FRAME /* Button 9 */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-exitF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-exitF C-Win
ON CHOOSE OF BUTTON-exitF IN FRAME F_Filtros /* Cierra Filtros */
DO:
    HIDE FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Update C-Win
ON CHOOSE OF BUTTON-Update IN FRAME F_Filtros /* Actualizar */
DO:     
    HIDE FRAME F_Filtros.
    CLOSE QUERY Browse-2.
    RUN cargaQuery.
    APPLY "ITERATION-CHANGED":U TO browse-2 IN FRAME DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fec_Fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fec_Fin C-Win
ON LEAVE OF Fec_Fin IN FRAME F_Filtros /* Hasta */
DO:
   ASSIGN Fec_Fin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fec_Ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fec_Ini C-Win
ON LEAVE OF Fec_Ini IN FRAME F_Filtros /* Desde */
DO:
  ASSIGN Fec_Ini.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  ASSIGN W_AgeNom = W_Nom_Agencia
         W_Fec    = W_Fecha
         W_Hor    = STRING(TIME,"hh:mm am")
         W_Usu    = W_Usuario
         W_Nom    = Usuarios.Nombre.
    RUN c:\SICOBEL\Confg.p (OUTPUT W_Estacion).
    FIND Estaciones WHERE Estaciones.Estacion EQ W_Estacion NO-LOCK NO-ERROR.
    IF AVAILABLE(Estaciones) THEN 
       W_EstacionWk = "Usted se encuentra en la estación: " + Estaciones.Estacion + " - " + Estaciones.Descripcion.
  RUN enable_UI.

        ASSIGN 
        Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros = STRING(W_fecha)
        Fec_Fin:SCREEN-VALUE IN FRAME F_Filtros = STRING(W_fecha).
        HIDE FRAME F_Filtros.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargaQuery C-Win 
PROCEDURE cargaQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OPEN QUERY Browse-2 FOR EACH Logs WHERE Logs.Usuario EQ W_Usuario AND Logs.Estado EQ YES 
    AND
    ((logs.fecha >= DATE(Fec_Ini:SCREEN-VALUE IN FRAME F_Filtros) AND
      logs.fecha <= DATE(Fec_fin:SCREEN-VALUE IN FRAME F_Filtros)
     ) OR (DATE(Fec_ini:SCREEN-VALUE IN FRAME F_Filtros) EQ ? OR
           DATE(Fec_fin:SCREEN-VALUE IN FRAME F_Filtros) EQ ?
          )
    )
    NO-LOCK BY Logs.Fecha 
            BY hora .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY W_AgeNom W_EstacionWk W_Usu W_Fec W_Nom W_Hor EDITOR-Obs 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-5 B-Filtro BUTTON-9 BUTTON-7 BROWSE-2 EDITOR-Obs 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY Fec_Ini Fec_Fin 
      WITH FRAME F_Filtros IN WINDOW C-Win.
  ENABLE Fec_Ini Fec_Fin BUTTON-Update BUTTON-exitF 
      WITH FRAME F_Filtros IN WINDOW C-Win.
  VIEW FRAME F_Filtros IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Filtros}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

