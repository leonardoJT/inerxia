&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

DEFINE OUTPUT PARAMETER P_Permiso AS INTEGER.
DEFINE OUTPUT PARAMETER W_Grupo AS INTEGER.
DEFINE OUTPUT PARAMETER W_Ofi AS INTEGER.

DEFINE VAR Devolver AS LOGICAL.

{Incluido\Clientes.i "NEW GLOBAL SHARED "}
{Incluido\variable.i "NEW GLOBAL SHARED "}
{Incluido\VarCon.i   "NEW GLOBAL SHARED "}

DEFINE VAR W_Intentos AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-MAIN

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS txtUsuario Clave Aceptar BT_Cancelar IMAGE-3 
&Scoped-Define DISPLAYED-OBJECTS txtUsuario Clave Fecha 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Aceptar AUTO-GO DEFAULT 
     LABEL "Aceptar" 
     SIZE 13 BY 1.35.

DEFINE BUTTON BT_Cancelar DEFAULT 
     LABEL "&Cancelar" 
     SIZE 13 BY 1.35
     BGCOLOR 8 .

DEFINE VARIABLE Clave AS CHARACTER FORMAT "X(15)":U 
     LABEL "Clave" 
     VIEW-AS FILL-IN 
     SIZE 27.14 BY .92
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fecha AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 39.86 BY .62
     FONT 5 NO-UNDO.

DEFINE VARIABLE txtUsuario AS CHARACTER FORMAT "X(25)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 27.14 BY .92
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "imagenes/presentacion.jpg":U CONVERT-3D-COLORS
     SIZE 77 BY 13.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-MAIN
     txtUsuario AT ROW 10.27 COL 46.57 COLON-ALIGNED WIDGET-ID 2
     Clave AT ROW 11.31 COL 46.57 COLON-ALIGNED BLANK  DEBLANK 
     Aceptar AT ROW 12.42 COL 62.72
     BT_Cancelar AT ROW 12.42 COL 48.72 NO-TAB-STOP 
     Fecha AT ROW 14 COL 35.57 NO-LABEL
     IMAGE-3 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 77.86 BY 14.12
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON Aceptar.


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
         TITLE              = "Iniciar sesión"
         HEIGHT             = 13.77
         WIDTH              = 75.72
         MAX-HEIGHT         = 38.77
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 38.77
         VIRTUAL-WIDTH      = 274.29
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = yes
         CONTROL-BOX        = no
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
/* SETTINGS FOR FILL-IN Fecha IN FRAME F-MAIN
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       Fecha:READ-ONLY IN FRAME F-MAIN        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Iniciar sesión */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Iniciar sesión */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Aceptar C-Win
ON CHOOSE OF Aceptar IN FRAME F-MAIN /* Aceptar */
DO:
    ASSIGN Clave.
    W_Clave = Clave.

    RUN Buscar_Usuarios (OUTPUT P_Permiso,
                         INPUT W_Clave,
                         INPUT-OUTPUT W_Intentos).

    IF W_Intentos >= 3 THEN DO:
        FIND FIRST Usuarios WHERE Usuarios.Usuario = txtUsuario:SCREEN-VALUE NO-ERROR.
        IF AVAILABLE Usuarios THEN
            Usuarios.Id_Bloqueo = YES.

        RUN escribirLog IN w_manija (INPUT txtUsuario,
                                     INPUT 'Bloqueado por intentos fallidos') NO-ERROR.

        RELEASE Usuarios.

        MESSAGE "El Usuario ha sido bloqueado. Por favor," SKIP
                "comuníquese con el Administrador del Sistema."
            VIEW-AS ALERT-BOX WARNING.

        APPLY "choose" TO Bt_Cancelar IN FRAME F-Main.
    END.

    IF Devolver THEN DO:
        Devolver = NO.
        APPLY 'entry' TO txtUsuario.
        RETURN NO-APPLY.
    END.

    IF P_Permiso <> 0 THEN DO:
        APPLY "ENTRY" TO Clave.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        IF (Usuarios.Fec_UltCam + Usuarios.Tie_Renovacion) <= TODAY THEN DO:
            MESSAGE "Su password de acceso a la aplicación ha vencido," SKIP
                    "Debe realizar el cambio."
                VIEW-AS ALERT-BOX INFORMATION.

            RUN escribirLog IN W_Manija (INPUT usuarios.usuario,
                                         INPUT 'Clave vencida. El sistema solicita cambio de password') NO-ERROR.

            RUN P-Clave.r.

            APPLY "ENTRY" TO Clave.
        END.
    END.

    ON RETURN RETURN.

    IF P_Permiso = 0 THEN DO:
        FIND CURRENT Usuarios NO-ERROR.

        Usuarios.Id_Entrada = TRUE.
        W_Ofi = usuarios.agencia.

        FIND CURRENT Usuarios NO-LOCK NO-ERROR.

        RUN escribirLog IN W_Manija (INPUT usuarios.usuario,
                                     INPUT 'Ingreso exitoso al Sistema') NO-ERROR.

    END.

    &IF DEFINED (adm-panel) <> 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
    &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Aceptar C-Win
ON SELECTION OF Aceptar IN FRAME F-MAIN /* Aceptar */
DO:
    ASSIGN FRAME F-MAIN
        Clave
        W_Clave = Clave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT_Cancelar C-Win
ON CHOOSE OF BT_Cancelar IN FRAME F-MAIN /* Cancelar */
DO:
    ON RETURN RETURN.

    P_Permiso = -1.

    &IF DEFINED (adm-panel) <> 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
    &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clave C-Win
ON ENTRY OF Clave IN FRAME F-MAIN /* Clave */
DO:
    IF txtUsuario:SCREEN-VALUE IN FRAME F-Main EQ ? THEN DO:
        MESSAGE "Debe ingresar un usuario"
            VIEW-AS ALERT-BOX INFORMATION.

        APPLY 'entry' TO txtUsuario IN FRAME F-Main.
        
        RETURN NO-APPLY.
    END.

    FIND FIRST Usuarios WHERE Usuarios.Usuario EQ txtUsuario:SCREEN-VALUE
                          AND Usuarios.Estado EQ 1
                          AND Usuarios.Pedir_Clave NO-LOCK NO-ERROR.
    IF AVAILABLE Usuarios THEN DO:
        MESSAGE "Ahora deberá entrar la clave que lo identificará en el sistema"
            VIEW-AS ALERT-BOX INFORMATION.

        RUN P-Clave.r.

        ON RETURN TAB.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clave C-Win
ON LEAVE OF Clave IN FRAME F-MAIN /* Clave */
DO:
    APPLY 'choose' TO Aceptar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtUsuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtUsuario C-Win
ON LEAVE OF txtUsuario IN FRAME F-MAIN /* Usuario */
DO:
    w_usuario = txtUsuario:SCREEN-VALUE.
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
ON RETURN TAB.

RUN Crear_Listas.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Usuarios C-Win 
PROCEDURE Buscar_Usuarios :
DEFINE OUTPUT PARAMETER P_Permiso AS INTEGER INITIAL -1.
DEFINE INPUT PARAMETER P_Clave AS CHARACTER.
DEFINE INPUT-OUTPUT PARAMETER P_Intentos AS INTEGER.

FIND FIRST usuarios WHERE Usuarios.Usuario = W_Usuario NO-ERROR.
IF NOT AVAILABLE usuarios THEN DO:
    MESSAGE "Usuario o clave inválida"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ELSE DO:
    IF usuarios.clave <> ENCODE(TRIM(W_Clave + W_Cadena)) THEN DO:
        MESSAGE "Usuario o clave inválida"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        p_intentos = p_intentos + 1.
        
        RUN escribirLog IN W_Manija (INPUT usuarios.usuario,
                                     INPUT 'Intento de ingreso con clave errada') NO-ERROR.

        Devolver = YES.
    END.
    ELSE DO:
        p_Permiso = 0.
        W_Prioridad = Usuarios.Prioridad.
        W_Grupo = Usuarios.Grupo.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crear_Listas C-Win 
PROCEDURE Crear_Listas :
ASSIGN Fecha = "Fecha del día: " + STRING(TODAY) + " - Hora: " + STRING(TIME,"HH:MM:SS AM").

VIEW Fecha IN FRAME F-MAIN.

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
  DISPLAY txtUsuario Clave Fecha 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  ENABLE txtUsuario Clave Aceptar BT_Cancelar IMAGE-3 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-MAIN}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

