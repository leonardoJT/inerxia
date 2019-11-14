&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

  DEFINE SHARED VAR W_Manija AS HANDLE.
  DEFINE SHARED VAR W_Eleccion AS LOGICAL.
  DEFINE SHARED VAR W_Usuario  LIKE Usuarios.Usuario.
  DEFINE VAR Hora AS CHARACTER FORMAT "X(8)".
  DEFINE VAR Estacion AS CHARACTER FORMAT "X(20)".
  DEFINE VAR W_NomUsuario AS CHARACTER FORMAT "X(20)".

DEF VAR lHb AS LOGICAL NO-UNDO.
DEF VAR clbel AS CHAR NO-UNDO EXTENT 4.
clbel[1] = "Todos*".
clbel[2] = "Grupo".
clbel[3] = "Agencia".
clbel[4] = "Usuario".

DEF VAR it AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Usuarios

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 Usuarios.Agencia Usuarios.Usuario Usuarios.Nombre Usuarios.proActual Estacion Hora   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH Usuarios       WHERE Usuarios.Id_Entrada = TRUE NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH Usuarios       WHERE Usuarios.Id_Entrada = TRUE NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 Usuarios
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 Usuarios


/* Definitions for BROWSE BROWSE-6                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-6 Usuarios.Agencia Usuarios.Usuario Usuarios.Nombre Usuarios.proActual Estacion Hora   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-6   
&Scoped-define SELF-NAME BROWSE-6
&Scoped-define QUERY-STRING-BROWSE-6 FOR EACH Usuarios       WHERE Usuarios.Id_Entrada = FALSE NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-6 OPEN QUERY {&SELF-NAME} FOR EACH Usuarios       WHERE Usuarios.Id_Entrada = FALSE NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-6 Usuarios
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-6 Usuarios


/* Definitions for FRAME F-main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-main ~
    ~{&OPEN-QUERY-BROWSE-2}~
    ~{&OPEN-QUERY-BROWSE-6}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-218 TRefresh BUTTON-44 BROWSE-2 ~
BUTTON-48 BUTTON-35 Btn_Todos BROWSE-6 BUTTON-32 BUTTON-HbIta ~
BUTTON-InHabilita BUTTON-34 rs-Opciones iGrpo 
&Scoped-Define DISPLAYED-OBJECTS TRefresh rs-Opciones 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Todos 
     LABEL "X Todos" 
     SIZE 9 BY 1.38.

DEFINE BUTTON BUTTON-117 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 117" 
     SIZE 9 BY 1.62.

DEFINE BUTTON BUTTON-118 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 118" 
     SIZE 9 BY 1.62.

DEFINE BUTTON BUTTON-32 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "S&alir" 
     SIZE 9 BY 1.62 TOOLTIP "Salir del programa de monitoreo".

DEFINE BUTTON BUTTON-34 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda"
     BGCOLOR 8 FGCOLOR 8 FONT 12.

DEFINE BUTTON BUTTON-35 
     IMAGE-UP FILE "imagenes\borrar":U
     LABEL "&Borrar" 
     SIZE 9 BY 1.62 TOOLTIP "Desbloquea la cuenta del usuario que no puede entrar al sistema".

DEFINE BUTTON BUTTON-44 
     IMAGE-UP FILE "imagenes\informacion":U
     LABEL "Button 44" 
     SIZE 9 BY 1.62 TOOLTIP "Muestra la pantalla de Información de Sesión y mensajes del usuario activo".

DEFINE BUTTON BUTTON-48 
     IMAGE-UP FILE "imagenes\mensaje":U
     LABEL "Button 48" 
     SIZE 9 BY 1.65 TOOLTIP "Graba un mensaje para el usuario seleccionado en pantalla".

DEFINE BUTTON BUTTON-HbIta 
     LABEL "Habilita" 
     SIZE 9 BY 1.62 TOOLTIP "HaBilita Usuarios".

DEFINE BUTTON BUTTON-InHabilita 
     LABEL "InHabilita" 
     SIZE 9 BY 1.62 TOOLTIP "InHabilita Usuarios".

DEFINE VARIABLE iGrpo AS INTEGER FORMAT "999999":U INITIAL 0 
     LABEL "Grupo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE rs-Opciones AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 1,
"Grupos", 2,
"Agencia", 3,
"Usuario", 4
     SIZE 35 BY .81 TOOLTIP "Agrupamiento Para Habilitar/InHabilitar" NO-UNDO.

DEFINE RECTANGLE RECT-218
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11 BY 5.38.

DEFINE VARIABLE TRefresh AS LOGICAL INITIAL no 
     LABEL "Actualización Automática" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .77 NO-UNDO.

DEFINE BUTTON BUTTON-49 
     IMAGE-UP FILE "imagenes\mensaje":U
     LABEL "Graba el mensaje en la tabla de LOG" 
     SIZE 9 BY 1.62 TOOLTIP "Envia el mensaje digitado".

DEFINE BUTTON BUTTON-50 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Button 50" 
     SIZE 9 BY 1.65 TOOLTIP "Vuelve a la pantalla, y esconde este cuadro de mensaje".

DEFINE VARIABLE E_Mensaje AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 75 SCROLLBAR-VERTICAL
     SIZE 47 BY 1.62
     BGCOLOR 15 FONT 4 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      Usuarios SCROLLING.

DEFINE QUERY BROWSE-6 FOR 
      Usuarios SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 NO-LOCK DISPLAY
      Usuarios.Agencia COLUMN-LABEL "Age."
      Usuarios.Usuario
      Usuarios.Nombre FORMAT "X(30)"
      Usuarios.proActual FORMAT "X(30)"
      Estacion FORMAT "X(30)"
      Hora FORMAT "X(10)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 12.65
         BGCOLOR 15 FGCOLOR 0 FONT 4 ROW-HEIGHT-CHARS .65.

DEFINE BROWSE BROWSE-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-6 C-Win _FREEFORM
  QUERY BROWSE-6 NO-LOCK DISPLAY
      Usuarios.Agencia COLUMN-LABEL "Age."
      Usuarios.Usuario
      Usuarios.Nombre FORMAT "X(30)"
      Usuarios.proActual FORMAT "X(30)"
      Estacion FORMAT "X(30)"
      Hora FORMAT "X(10)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 5.12
         BGCOLOR 15 FGCOLOR 12 FONT 4 ROW-HEIGHT-CHARS .73.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-main
     TRefresh AT ROW 1.27 COL 78
     BUTTON-44 AT ROW 1.54 COL 103
     BROWSE-2 AT ROW 2.35 COL 4 HELP
          "Usuarios que estan trabajando en la aplicación"
     BUTTON-117 AT ROW 3.15 COL 103
     BUTTON-118 AT ROW 4.77 COL 103
     BUTTON-48 AT ROW 10.15 COL 103
     BUTTON-35 AT ROW 13.38 COL 103 HELP
          "Permite Eliminar un registro de datos"
     Btn_Todos AT ROW 15 COL 103 WIDGET-ID 2
     BROWSE-6 AT ROW 16.35 COL 4 HELP
          "Usuarios que estan trabajando en la aplicación"
     BUTTON-32 AT ROW 16.35 COL 103 HELP
          "Permite Regresar a la opción anterior"
     BUTTON-HbIta AT ROW 17.96 COL 103 WIDGET-ID 4
     BUTTON-InHabilita AT ROW 19.58 COL 103 WIDGET-ID 12
     BUTTON-34 AT ROW 21.46 COL 106
     rs-Opciones AT ROW 21.73 COL 42 NO-LABEL WIDGET-ID 6
     iGrpo AT ROW 21.73 COL 84 COLON-ALIGNED WIDGET-ID 10
     " Usuarios que no han comenzado sesión" VIEW-AS TEXT
          SIZE 95 BY .81 AT ROW 15.27 COL 4
          BGCOLOR 18 FGCOLOR 15 FONT 5
     " Usuarios Activos" VIEW-AS TEXT
          SIZE 73 BY .77 AT ROW 1.27 COL 4
          BGCOLOR 18 FGCOLOR 15 FONT 5
     RECT-218 AT ROW 1.27 COL 102
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115.72 BY 22.04
         BGCOLOR 17 FONT 4.

DEFINE FRAME F-Mensaje
     E_Mensaje AT ROW 1.27 COL 2 NO-LABEL
     BUTTON-49 AT ROW 1.27 COL 51
     BUTTON-50 AT ROW 3.69 COL 51
     "Recuerde:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 3.15 COL 2
          FGCOLOR 7 
     "La lectura de este mensaje por parte del usuario debe" VIEW-AS TEXT
          SIZE 37 BY .81 AT ROW 3.15 COL 12
          FONT 4
     "ser auditada, este mensaje quedará grabado en la tabla" VIEW-AS TEXT
          SIZE 38 BY .81 AT ROW 3.96 COL 12
          FONT 4
     "de LOG del sistema." VIEW-AS TEXT
          SIZE 15 BY .81 AT ROW 4.77 COL 12
          FONT 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 53 ROW 6.92
         SIZE 60 BY 5.65
         BGCOLOR 17 FONT 5
         TITLE "Mensaje para usuarios seleccionados".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Control de Usuarios del Sistema"
         HEIGHT             = 22.04
         WIDTH              = 114.29
         MAX-HEIGHT         = 25.12
         MAX-WIDTH          = 129.14
         VIRTUAL-HEIGHT     = 25.12
         VIRTUAL-WIDTH      = 129.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* REPARENT FRAME */
ASSIGN FRAME F-Mensaje:FRAME = FRAME F-main:HANDLE.

/* SETTINGS FOR FRAME F-main
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 BUTTON-44 F-main */
/* BROWSE-TAB BROWSE-6 Btn_Todos F-main */
/* SETTINGS FOR BUTTON BUTTON-117 IN FRAME F-main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-118 IN FRAME F-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iGrpo IN FRAME F-main
   NO-DISPLAY                                                           */
/* SETTINGS FOR FRAME F-Mensaje
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Mensaje:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Usuarios
      WHERE Usuarios.Id_Entrada = TRUE NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-6
/* Query rebuild information for BROWSE BROWSE-6
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Usuarios
      WHERE Usuarios.Id_Entrada = FALSE NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE BROWSE-6 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F-main:HANDLE
       ROW             = 14.19
       COLUMN          = 101
       HEIGHT          = 1.42
       WIDTH           = 7.57
       HIDDEN          = yes
       SENSITIVE       = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(BUTTON-35:HANDLE IN FRAME F-main).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SFG - Control de Usuarios del Sistema */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SFG - Control de Usuarios del Sistema */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON ROW-DISPLAY OF BROWSE-2 IN FRAME F-main
DO:
  Hora = STRING(Usuarios.Hentrada,"hh:mm am").
  IF Usuarios.Id_Estacion THEN DO:
     FIND Estaciones WHERE Estaciones.Estacion EQ Usuarios.Estacion NO-LOCK NO-ERROR.
     IF AVAILABLE(Estaciones) THEN
         Estacion = Estaciones.Descripcion.
  END.
  ELSE
     Estacion = "Todas".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-6
&Scoped-define SELF-NAME BROWSE-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-6 C-Win
ON ROW-DISPLAY OF BROWSE-6 IN FRAME F-main
DO:
  Hora = STRING(Usuarios.Hentrada,"hh:mm am").
  IF Usuarios.Id_Estacion THEN DO:
     FIND Estaciones WHERE Estaciones.Estacion EQ Usuarios.Estacion NO-LOCK NO-ERROR.
     IF AVAILABLE(Estaciones) THEN
         Estacion = Estaciones.Descripcion.
  END.
  ELSE
     Estacion = "Todas".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Todos C-Win
ON CHOOSE OF Btn_Todos IN FRAME F-main /* X Todos */
DO:
  IF NUM-RESULTS("{&BROWSE-NAME}") EQ 0 THEN
     RETURN NO-APPLY.
  FOR EACH Usuarios WHERE usuarios.usuario NE w_usuario:
     ASSIGN Usuarios.id_entrada = FALSE.
  END.
  FIND FIRST usuarios WHERE  usuario = w_usuario NO-LOCK NO-ERROR.
  ENABLE BUTTON-32 BUTTON-34 BUTTON-35 
      WITH FRAME f-main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  ENABLE BROWSE-2 
      WITH FRAME f-main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-main}
  VIEW C-Win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-32
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-32 C-Win
ON CHOOSE OF BUTTON-32 IN FRAME F-main /* Salir */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  ON RETURN RETURN.
  ON TAB TAB.
  ON CURSOR-DOWN CURSOR-DOWN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-34
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-34 C-Win
ON CHOOSE OF BUTTON-34 IN FRAME F-main
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "ayudas\utilidad" CONTEXT 5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-35
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-35 C-Win
ON CHOOSE OF BUTTON-35 IN FRAME F-main /* Borrar */
DO: 
  IF NUM-RESULTS("{&BROWSE-NAME}") EQ 0 THEN
     RETURN NO-APPLY.
  FIND Current Usuarios.
  ASSIGN Usuarios.id_entrada = FALSE.
  ENABLE BUTTON-32 BUTTON-34 BUTTON-35 
      WITH FRAME f-main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  ENABLE BROWSE-2 
      WITH FRAME f-main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-main}
  VIEW C-Win.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-44
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-44 C-Win
ON CHOOSE OF BUTTON-44 IN FRAME F-main /* Button 44 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-48
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-48 C-Win
ON CHOOSE OF BUTTON-48 IN FRAME F-main /* Button 48 */
DO:
  VIEW FRAME F-Mensaje.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Mensaje
&Scoped-define SELF-NAME BUTTON-49
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-49 C-Win
ON CHOOSE OF BUTTON-49 IN FRAME F-Mensaje /* Graba el mensaje en la tabla de LOG */
DO:
  ASSIGN FRAME F-Mensaje E_Mensaje.
  IF E_Mensaje NE "" THEN
   DO:
     FIND Current Usuarios.
     CREATE Logs.
     ASSIGN Logs.agencia     = Usuarios.Agencia
            Logs.estado      = YES
            Logs.Fecha       = TODAY
            Logs.HoraE       = TIME
            Logs.Observacion = E_Mensaje + " Env: " + W_Usuario + " - " + W_Nomusuario
            Logs.Usuario     = Usuarios.Usuario 
            Logs.IdRegistro  = 1 NO-ERROR.
     E_Mensaje = "".
     DISPLAY E_Mensaje WITH FRAME F-Mensaje.
     MESSAGE "EL mensaje acaba de ser enviado a su destinatario" 
             VIEW-AS ALERT-BOX INFORMATION TITLE "Mensaje". 
   END.
  ELSE
   MESSAGE "No se ha escrito ningún texto para enviarle al usuario"
           VIEW-AS ALERT-BOX INFORMATION TITLE "Falta Información".
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-50
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-50 C-Win
ON CHOOSE OF BUTTON-50 IN FRAME F-Mensaje /* Button 50 */
DO:
  E_Mensaje = "".
  DISPLAY E_Mensaje WITH FRAME F-Mensaje.
  HIDE FRAME F-Mensaje.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-main
&Scoped-define SELF-NAME BUTTON-HbIta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-HbIta C-Win
ON CHOOSE OF BUTTON-HbIta IN FRAME F-main /* Habilita */
DO:
    RUN hbltausuarios.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-InHabilita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-InHabilita C-Win
ON CHOOSE OF BUTTON-InHabilita IN FRAME F-main /* InHabilita */
DO:
    RUN inhbltausuarios.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
IF TRefresh:SCREEN-VALUE IN FRAME F-Main EQ "YES" THEN DO:
  OPEN QUERY Browse-2 FOR EACH Usuarios WHERE Usuarios.Id_Entrada = TRUE NO-LOCK.
  OPEN QUERY Browse-6 FOR EACH Usuarios WHERE Usuarios.Id_Entrada = FALSE NO-LOCK.
END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iGrpo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iGrpo C-Win
ON VALUE-CHANGED OF iGrpo IN FRAME F-main /* Grupo */
DO:
    ASSIGN     {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-Opciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-Opciones C-Win
ON VALUE-CHANGED OF rs-Opciones IN FRAME F-main
DO:
    DO WITH FRAME  {&FRAME-NAME}:
        ASSIGN  {&SELF-NAME}. 
        iGrpo:LABEL = clbel[rs-opciones].
        iGrpo:TOOLTIP = iGrpo:LABEL.
        IF rs-opciones = 1
        THEN igrpo:VISIBLE = FALSE.
        ELSE igrpo:VISIBLE = TRUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
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
  IF AVAILABLE(Usuarios) THEN
     W_NomUsuario = Usuarios.Nombre.
  RUN enable_UI.
    igrpo:VISIBLE = FALSE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "W-Control_Usuarios.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "W-Control_Usuarios.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  DISPLAY TRefresh rs-Opciones 
      WITH FRAME F-main IN WINDOW C-Win.
  ENABLE RECT-218 TRefresh BUTTON-44 BROWSE-2 BUTTON-48 BUTTON-35 Btn_Todos 
         BROWSE-6 BUTTON-32 BUTTON-HbIta BUTTON-InHabilita BUTTON-34 
         rs-Opciones iGrpo 
      WITH FRAME F-main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-main}
  DISPLAY E_Mensaje 
      WITH FRAME F-Mensaje IN WINDOW C-Win.
  ENABLE E_Mensaje BUTTON-49 BUTTON-50 
      WITH FRAME F-Mensaje IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Mensaje}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hbltausuarios C-Win 
PROCEDURE hbltausuarios :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hab:    
    REPEAT TRANSACTION ON ERROR UNDO hab, LEAVE hab:
        it = 0.
        FOR EACH usuarios EXCLUSIVE-LOCK
            WHERE
                usuarios.usuario <> w_usuario
            AND usuarios.estado = 9
            AND (IF rs-opciones = 1 THEN TRUE 
                ELSE (IF rs-opciones = 2 THEN usuarios.grupo = igrpo 
                      ELSE (IF rs-opciones = 3 THEN usuarios.agencia = igrpo
                            ELSE integer(usuario) = igrpo))):
            usuarios.estado = 1.
            it = it + 1.
        END.
        RELEASE usuarios.
        LEAVE.
    END.
    MESSAGE it " Usuarios Habilitados (" igrpo ")" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inhbltausuarios C-Win 
PROCEDURE inhbltausuarios :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hab:    
    REPEAT TRANSACTION ON ERROR UNDO hab, LEAVE hab:
        it = 0.
        FOR EACH usuarios fields(estado)EXCLUSIVE-LOCK
            WHERE
                usuarios.usuario <> w_usuario
            AND usuarios.estado = 1
            AND (IF rs-opciones = 1 THEN TRUE 
                 ELSE (IF rs-opciones = 2 THEN usuarios.grupo = igrpo 
                       ELSE (IF rs-opciones = 3 THEN usuarios.agencia = igrpo
                              ELSE integer(usuario) = igrpo))):
            usuarios.estado = 9.
            it = it + 1.
        END.
        RELEASE usuarios.
        LEAVE.
    END.
    MESSAGE it " Usuarios Inhabilitados (" igrpo ")" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

