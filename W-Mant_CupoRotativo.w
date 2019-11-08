&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.
{Incluido\variable.i "shared"}
/* ***************************  Definitions  ************************** */

DEFINE VARIABLE P_Nit AS CHARACTER.
DEFINE VARIABLE p_Nombre AS CHARACTER.
DEFINE VARIABLE P_Apellido AS CHARACTER.
DEFINE VARIABLE P_AgeCli AS INTEGER.
DEFINE VARIABLE W_Error AS LOGICAL.
DEFINE VARIABLE W_Autorizo AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Frm_Clientes

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Ahorros

/* Definitions for FRAME Frm_Clientes                                   */
&Scoped-define SELF-NAME Frm_Clientes
&Scoped-define QUERY-STRING-Frm_Clientes FOR EACH Ahorros SHARE-LOCK
&Scoped-define OPEN-QUERY-Frm_Clientes OPEN QUERY {&SELF-NAME} FOR EACH Ahorros SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Frm_Clientes Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-Frm_Clientes Ahorros


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Clientes.Nit 
&Scoped-define ENABLED-TABLES Clientes
&Scoped-define FIRST-ENABLED-TABLE Clientes
&Scoped-Define ENABLED-OBJECTS wcupo wnewcupo wnrocre wsdocapital ~
btn_procesar Btn_Cancelar BtnDone Btn_Consulta BUTTON-1 BUTTON-95 ECT-309 ~
RECT-274 
&Scoped-Define DISPLAYED-FIELDS Clientes.Nit 
&Scoped-define DISPLAYED-TABLES Clientes
&Scoped-define FIRST-DISPLAYED-TABLE Clientes
&Scoped-Define DISPLAYED-OBJECTS wcupo wnewcupo wnrocre wsdocapital ~
W_NomTitular 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Cancelar BtnDone Btn_Consulta BUTTON-1 BUTTON-95 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY .81
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 10 BY .81
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 10 BY 1.62.

DEFINE BUTTON btn_procesar 
     LABEL "&Guardar" 
     SIZE 10 BY .81
     FONT 4.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-95 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 95" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE wcupo AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Cupo" 
     VIEW-AS FILL-IN 
     SIZE 17.43 BY .81 NO-UNDO.

DEFINE VARIABLE wnewcupo AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Nuevo Cupo" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE wnrocre AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Nro.Crédito" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE wsdocapital AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Sdo Capital" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .81 NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 46 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE ECT-309
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 5.65.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 3.77.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Frm_Clientes FOR 
      Ahorros SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm_Clientes
     wcupo AT ROW 4.5 COL 37.86 COLON-ALIGNED WIDGET-ID 10
     wnewcupo AT ROW 5.58 COL 12 COLON-ALIGNED WIDGET-ID 14
     wnrocre AT ROW 4.5 COL 12.29 COLON-ALIGNED WIDGET-ID 8
     wsdocapital AT ROW 4.5 COL 68.86 COLON-ALIGNED WIDGET-ID 12
     Clientes.Nit AT ROW 1.81 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     btn_procesar AT ROW 7.46 COL 13
     Btn_Cancelar AT ROW 7.46 COL 29
     BtnDone AT ROW 7.46 COL 47
     Btn_Consulta AT ROW 3.15 COL 90
     BUTTON-1 AT ROW 1.54 COL 90
     BUTTON-95 AT ROW 10.73 COL 93
     W_NomTitular AT ROW 1.81 COL 39 COLON-ALIGNED
     "                                            Información del Crédito Rotativo" VIEW-AS TEXT
          SIZE 85.29 BY .81 AT ROW 3.15 COL 2.57 WIDGET-ID 4
          BGCOLOR 18 FGCOLOR 15 
     ECT-309 AT ROW 1.27 COL 2
     RECT-274 AT ROW 1.27 COL 89
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.43 BY 14.46
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
         TITLE              = "Actualización de Cupos"
         HEIGHT             = 7.69
         WIDTH              = 100.29
         MAX-HEIGHT         = 22.35
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.35
         VIRTUAL-WIDTH      = 114.29
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
/* SETTINGS FOR FRAME Frm_Clientes
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* SETTINGS FOR BUTTON BtnDone IN FRAME Frm_Clientes
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME Frm_Clientes
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME Frm_Clientes
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME Frm_Clientes
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-95 IN FRAME Frm_Clientes
   6                                                                    */
ASSIGN 
       wcupo:READ-ONLY IN FRAME Frm_Clientes        = TRUE.

ASSIGN 
       wnrocre:READ-ONLY IN FRAME Frm_Clientes        = TRUE.

ASSIGN 
       wsdocapital:READ-ONLY IN FRAME Frm_Clientes        = TRUE.

/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME Frm_Clientes
   NO-ENABLE                                                            */
ASSIGN 
       W_NomTitular:READ-ONLY IN FRAME Frm_Clientes        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frm_Clientes
/* Query rebuild information for FRAME Frm_Clientes
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Ahorros SHARE-LOCK.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* FRAME Frm_Clientes */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Actualización de Cupos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Actualización de Cupos */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Frm_Clientes
&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME Frm_Clientes /* Salir */
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


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON CHOOSE OF Btn_Cancelar IN FRAME Frm_Clientes /* Cancelar */
DO:
RUN inicializar_variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta C-Win
ON CHOOSE OF Btn_Consulta IN FRAME Frm_Clientes /* Button 3 */
DO:
    RUN C-Clientes.R(INPUT 2, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_procesar C-Win
ON CHOOSE OF btn_procesar IN FRAME Frm_Clientes /* Guardar */
DO:
    DEFINE VAR wantcupo AS DECIMAL.
    DEFINE VAR pAutorizacion AS LOGICAL.
    DEFINE VAR pUsuarioAutoriza AS CHARACTER.

    ASSIGN Wnewcupo.

    /* Control para créditos de Solidaridad */
    IF wNewCupo > wCupo THEN DO:
        FIND FIRST creditos WHERE creditos.nit = clientes.nit:SCREEN-VALUE
                              AND creditos.cod_credito = 158
                              AND creditos.estado = 2 NO-LOCK NO-ERROR.
        IF AVAILABLE creditos THEN DO:
            MESSAGE "El Asociado tiene un Crédito de Solidaridad vigente. por esta razón" SKIP
                    "no es posible la ampliación del cupo del Asociado (Resolución 7 de 2019)." SKIP
                    "Se requiere autorización para realizar esta operación."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RUN p-AutorizacionTransaccion IN w_manija (INPUT 6,
                                                       OUTPUT pAutorizacion,
                                                       OUTPUT pUsuarioAutoriza).

            IF pAutorizacion = TRUE THEN DO:
                CREATE Hoja_Vida.
                Hoja_Vida.Tipo = 1.
                Hoja_Vida.Codigo = 1.
                Hoja_Vida.Nit = clientes.Nit:SCREEN-VALUE.
                Hoja_Vida.Usuario = w_usuario.
                Hoja_Vida.Fec_Grabacion = w_fecha.
                Hoja_Vida.Hora_Grabacion = TIME.
                Hoja_Vida.Asunto_Cumplido = YES.
                Hoja_Vida.Observacion = "Ampliación de Cupo rotativo autorizado por " + pUsuarioAutoriza.
            END.
            ELSE DO:
                MESSAGE "La operación no ha sido autorizada."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN NO-APPLY.
            END.
        END.
    END.
    /* ----------------------- */

    FIND FIRST creditos WHERE creditos.nit = clientes.nit:SCREEN-VALUE
                          AND creditos.cod_credito = 123
                          AND creditos.estado = 2 NO-ERROR.
    IF AVAILABLE(creditos) THEN DO:
        /* Se revisa el cupo máximo parametrizado en la tabla pro_creditos */
        IF wnewcupo <> 0 THEN DO:
            FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
            IF AVAILABLE pro_creditos THEN DO:
                IF DECIMAL(wnewcupo:SCREEN-VALUE) > pro_creditos.Val_Montomaximo OR DECIMAL(wnewcupo:SCREEN-VALUE) < pro_creditos.Val_MontoMinimo THEN DO:
                    MESSAGE "El Cupo otorgado no está dentro del rango permitido para este producto." SKIP
                            "Por favor, verifique y corrija."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                
                    RETURN.
                END.
            END.
        END.

        ASSIGN wantcupo = creditos.monto
               Creditos.monto = DECIMAL(wnewcupo:SCREEN-VALUE)
               Creditos.val_desembolso = DECIMAL(wnewcupo:SCREEN-VALUE).

        CREATE Hoja_Vida.
        ASSIGN Hoja_Vida.Tipo = 1
               Hoja_Vida.Codigo = 1
               Hoja_Vida.Nit = clientes.nit:SCREEN-VALUE
               Hoja_Vida.Usuario = W_Usuario
               Hoja_Vida.Fec_Grabacion = W_fecha
               Hoja_Vida.Hora_Grabacion = TIME
               Hoja_Vida.Observacion = "Cambio de Monto Cupo Rotativo al nit: " + clientes.nit:SCREEN-VALUE +
                                       " Cupo_ant: " + TRIM(STRING(Wantcupo)) + " Por nuevo Cupo: " + wnewcupo:SCREEN-VALUE.

        /* Revisamos si lo que se desea es cancelar el cupo rotativo */
        IF creditos.sdo_capital = 0 AND creditos.INT_corriente = 0 AND creditos.INT_morCobrar = 0 AND creditos.INT_difCobro = 0 AND creditos.Int_MoraDifCob = 0 THEN DO:
            MESSAGE "Desea cancelar definitivamente este cupo rotativo...?" SKIP
                    "(Tenga en cuenta que esta operación no es posible reversarla)"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Cancelación de Cupo Rotativo" UPDATE flagCancelaCupo AS LOGICAL.

            IF flagCancelaCupo = YES THEN DO:
                creditos.estado = 3.
                creditos.fec_canceTotal = TODAY.
                Hoja_Vida.Observacion = "Se cancela el cupo Rotativo".

                MESSAGE "El Cupo Rotativo ha sido cancelado." SKIP
                        "Recuerde reportar esta operación a la red Visionamos"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
        END.
        
        RELEASE creditos.

        FIND FIRST creditos NO-LOCK NO-ERROR.

        IF creditos.estado = 2 THEN
            MESSAGE "Terminó la operacion exitosamente." SKIP
                    "El nuevo cupo es de" wnewcupo:SCREEN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        btn_procesar:SENSITIVE = FALSE.

        RUN inicializar_variables.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME Frm_Clientes /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nit C-Win
ON LEAVE OF Clientes.Nit IN FRAME Frm_Clientes /* Nit */
DO:
    DO WITH FRAME F_Clientes:
        FIND Clientes WHERE Clientes.Nit = Clientes.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.

        IF AVAILABLE(Clientes) THEN
            W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
        ELSE DO:
            RUN C-Clientes.R(INPUT 2,
                             INPUT W_Agencia,
                             OUTPUT P_Nit,
                             OUTPUT P_Nombre,
                             OUTPUT P_Apellido,
                             OUTPUT P_AgeCli).

            Clientes.Nit:SCREEN-VALUE = P_Nit.
            W_Nomtitular:SCREEN-VALUE = CAPS(TRIM(P_Nombre) + " " + P_Apellido).

            FIND FIRST Clientes WHERE Clientes.Nit = P_Nit NO-LOCK NO-ERROR.
        END.

        FIND FIRST creditos WHERE creditos.nit = clientes.nit:SCREEN-VALUE
                              AND creditos.cod_credito = 123
                              AND creditos.estado = 2 NO-ERROR.
        IF AVAILABLE(creditos) THEN DO:
            wnrocre:SCREEN-VALUE = STRING(creditos.num_credito).
            wcupo:SCREEN-VALUE = STRING(creditos.Monto).
            wsdocapital:SCREEN-VALUE = STRING(creditos.sdo_capital).

            wnewcupo:SENSITIVE = TRUE.
            APPLY "Entry" TO wnewcupo.
        END.
        ELSE
            MESSAGE "Esta identificaci¢n no tiene Cupo Rotativo, Favor verifique la identificación"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wnewcupo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wnewcupo C-Win
ON LEAVE OF wnewcupo IN FRAME Frm_Clientes /* Nuevo Cupo */
DO:
  ASSIGN wnewcupo wcupo wsdocapital.
  IF wnewcupo GE wsdocapital THEN
    btn_procesar:SENSITIVE = TRUE.
  ELSE
    MESSAGE "No puede asignar un cupo inferior a su saldo de capital actual"
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
  DISPLAY wcupo wnewcupo wnrocre wsdocapital W_NomTitular 
      WITH FRAME Frm_Clientes IN WINDOW C-Win.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Nit 
      WITH FRAME Frm_Clientes IN WINDOW C-Win.
  ENABLE wcupo wnewcupo wnrocre wsdocapital Clientes.Nit btn_procesar 
         Btn_Cancelar BtnDone Btn_Consulta BUTTON-1 BUTTON-95 ECT-309 RECT-274 
      WITH FRAME Frm_Clientes IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm_Clientes}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inicializar_variables C-Win 
PROCEDURE inicializar_variables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN Clientes.Nit:SCREEN-VALUE IN FRAME FRM_Clientes   = "".
         btn_procesar:SENSITIVE = FALSE.
  ASSIGN wnrocre:SCREEN-VALUE = '0'
         wcupo:SCREEN-VALUE  = '0'
         wsdocapital:SCREEN-VALUE  = '0'
         wnewcupo:SCREEN-VALUE     = '0'.
  APPLY "entry" TO clientes.nit.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validar_transaccion C-Win 
PROCEDURE validar_transaccion :
/*------------------------------------------------------------------------------
  Purpose:     Seguridad en la operaci¢n de cambio de titular para el producto 3 
               de ahorros - CDAT
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER T_OfiVal  LIKE Agencias.Agencia.
  DEFINE INPUT  PARAMETER T_GrpVal  LIKE Grupos.Grupo.
  DEFINE INPUT  PARAMETER T_UsuVal  LIKE Usuarios.Usuario.
  DEFINE INPUT  PARAMETER T_OpeVal  LIKE Operacion.Cod_Operacion.
  DEFINE OUTPUT PARAMETER T_Validar AS LOGICAL.
  DEFINE OUTPUT PARAMETER T_NomOpe  LIKE Operacion.Nom_Operacion.
  
  DEFINE VAR T_Clave AS LOGICAL.
  
  ASSIGN T_Validar = FALSE T_Clave = FALSE.
  FIND Operacion WHERE Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Operacion) THEN DO:
     ASSIGN T_NomOpe = Operacion.Nom_Operacion
            T_Clave  = Operacion.Id_Clave.
  END.
  ELSE ASSIGN T_NomOpe = "".
  
  IF T_Clave THEN DO:
     MESSAGE "La Operación "  T_NomOpe   SKIP
             "Requiere Clave de SuperUsuario."
     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
     TITLE "Validación En Taquilla".
     RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
     IF W_Error EQ FALSE THEN DO:
        ASSIGN T_Validar = TRUE.
     END.
  END.
  
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 3
                           AND   Res_Operacion.Usuario       EQ T_UsuVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 2
                           AND   Res_Operacion.Grupo         EQ T_GrpVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 1
                           AND   Res_Operacion.Agencia       EQ T_OfiVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

