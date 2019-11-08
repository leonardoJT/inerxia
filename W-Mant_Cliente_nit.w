&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

{Incluido\variable.i "shared"}

DEFINE VARIABLE P_Nit AS CHARACTER.
DEFINE VARIABLE p_Nombre AS CHARACTER.
DEFINE VARIABLE P_Apellido AS CHARACTER.
DEFINE VARIABLE P_AgeCli AS INTEGER.
DEFINE VARIABLE W_Error AS LOGICAL.

/* oakley */

DEFINE VARIABLE W_Autorizo AS CHARACTER.

DEFINE TEMP-TABLE ttClientes LIKE clientes.

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
&Scoped-Define ENABLED-OBJECTS w_Nnit Btn_Cancelar Btn_Consulta BUTTON-1 ~
BUTTON-95 BtnDone-2 btn_procesar ECT-309 RECT-274 
&Scoped-Define DISPLAYED-FIELDS Clientes.Nit 
&Scoped-define DISPLAYED-TABLES Clientes
&Scoped-define FIRST-DISPLAYED-TABLE Clientes
&Scoped-Define DISPLAYED-OBJECTS w_Nnit W_NomTitular 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Cancelar Btn_Consulta BUTTON-1 BUTTON-95 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone-2 DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 7 BY 1.65 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/borrar.bmp":U
     LABEL "&Cancelar" 
     SIZE 7 BY 1.65 TOOLTIP "Cancelar"
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 7 BY 1.65 TOOLTIP "Buscar".

DEFINE BUTTON btn_procesar 
     IMAGE-UP FILE "imagenes/proceso_g.bmp":U
     LABEL "Actualizar" 
     SIZE 7 BY 1.65 TOOLTIP "Actualizar".

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 7 BY 1.65 TOOLTIP "Información".

DEFINE BUTTON BUTTON-95 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 95" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE w_Nnit AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nuevo Nit" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 46 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE ECT-309
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 86 BY 2.96.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 9.42.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Frm_Clientes FOR 
      Ahorros SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm_Clientes
     Clientes.Nit AT ROW 1.81 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
          BGCOLOR 15 
     w_Nnit AT ROW 3.15 COL 11 COLON-ALIGNED
     Btn_Cancelar AT ROW 6.38 COL 91.14
     Btn_Consulta AT ROW 3.15 COL 91.14
     BUTTON-1 AT ROW 1.54 COL 91.14
     BUTTON-95 AT ROW 10.96 COL 92.57
     W_NomTitular AT ROW 1.81 COL 39 COLON-ALIGNED
     BtnDone-2 AT ROW 8.54 COL 91.14 WIDGET-ID 30
     btn_procesar AT ROW 4.77 COL 91.14 WIDGET-ID 2
     ECT-309 AT ROW 1.27 COL 2
     RECT-274 AT ROW 1.27 COL 90.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.14 BY 11.08
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
         TITLE              = "Cambio de Nit para Clientes - W-Mant_Cliente_Nit.w"
         HEIGHT             = 11.08
         WIDTH              = 98.14
         MAX-HEIGHT         = 21.27
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.27
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
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME Frm_Clientes
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME Frm_Clientes
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME Frm_Clientes
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-95 IN FRAME Frm_Clientes
   6                                                                    */
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
     _Query            is OPENED
*/  /* FRAME Frm_Clientes */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Cambio de Nit para Clientes - W-Mant_Cliente_Nit.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Cambio de Nit para Clientes - W-Mant_Cliente_Nit.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Frm_Clientes
&Scoped-define SELF-NAME BtnDone-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone-2 C-Win
ON CHOOSE OF BtnDone-2 IN FRAME Frm_Clientes
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
ON CHOOSE OF btn_procesar IN FRAME Frm_Clientes /* Actualizar */
DO:
    DEFINE VAR pSecuencia AS INTEGER.
    DEFINE VAR pComprobante AS INTEGER INITIAL 4.
    DEFINE VAR cont AS INTEGER.
    DEFINE VAR saldoAnexo AS DECIMAL.
    
    ASSIGN W_Nnit.

    FIND FIRST clientes WHERE clientes.nit = W_Nnit NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        MESSAGE "El nuevo nit ya se encuentra matriculado en el Aplicativo" SKIP
                "No se permite esta operación"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    FIND FIRST clientes WHERE nit EQ clientes.nit:SCREEN-VALUE NO-ERROR.
    IF AVAILABLE(clientes) THEN DO:
        /*IF clientes.Anterior_nit NE "" THEN DO:
            MESSAGE "El nuevo nit ya se encuentra matriculado en el Aplicativo" SKIP
                    "No se permite esta operación"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN NO-APPLY.
        END.*/

        DISABLE TRIGGERS FOR LOAD OF clientes.
        DISABLE TRIGGERS FOR LOAD OF mov_ahorros.
        DISABLE TRIGGERS FOR LOAD OF mov_creditos.
        DISABLE TRIGGERS FOR LOAD OF ahorros.
        DISABLE TRIGGERS FOR LOAD OF creditos.
        DISABLE TRIGGERS FOR LOAD OF planpagos.
        DISABLE TRIGGERS FOR LOAD OF mov_instancias.
        DISABLE TRIGGERS FOR LOAD OF solicitud.
        DISABLE TRIGGERS FOR LOAD OF relaciones. /* Adicionado el 21 de diciembre de 2005 */

        Cliente:
        DO TRANSACTION ON ERROR UNDO Cliente:
            CREATE ttClientes.
            BUFFER-COPY clientes TO ttClientes.
            ttClientes.estado = 2.
            ttClientes.Fec_ModNit = TODAY.

            /* Para el caso de Asociados, se cambia a Cliente No Asociado */
            IF ttClientes.tipo_vinculo = 1 THEN
                ttClientes.tipo_vinculo = 2.

            ASSIGN Clientes.Anterior_Nit = Clientes.Nit 
                   Clientes.nit = W_Nnit
                   Clientes.Fec_ModNit = TODAY.

            FOR EACH mov_ahorros WHERE mov_ahorros.nit = clientes.nit:SCREEN-VALUE:
                mov_ahorros.nit = W_Nnit.
            END.

            FOR EACH mov_creditos WHERE mov_creditos.nit = clientes.nit:SCREEN-VALUE:
                mov_creditos.nit = W_Nnit.
            END.

            FOR EACH ahorros WHERE ahorros.nit = clientes.nit:SCREEN-VALUE:
                ahorros.nit = W_Nnit.
            END.

            FOR EACH creditos WHERE creditos.nit = clientes.nit:SCREEN-VALUE:
                creditos.nit = W_Nnit.
            END.

            FOR EACH planpagos WHERE planpagos.nit = clientes.nit:SCREEN-VALUE:
                planpagos.nit = W_Nnit.
            END.
            
            FOR EACH mov_instancias WHERE mov_instancias.nit = clientes.nit:SCREEN-VALUE:
                mov_instancias.nit = W_Nnit.
            END.
            
            FOR EACH solicitud WHERE solicitud.nit = clientes.nit:SCREEN-VALUE:
                solicitud.nit = W_Nnit.
            END.
            
            FOR EACH anexos WHERE anexos.nit = clientes.nit:SCREEN-VALUE
                              AND ano = YEAR(TODAY) BREAK BY anexos.agencia: /* 27/09/2005 jjmp */
                /*anexos.nit = W_Nnit.*/
                IF FIRST-OF(anexos.agencia) THEN DO:
                    FIND FIRST comprobantes WHERE comprobantes.agencia = anexos.agencia
                                              AND comprobantes.comprobante = pComprobante NO-ERROR.
                    comprobantes.secuencia = comprobantes.secuencia + 1.
                    pSecuencia = comprobantes.secuencia.

                    FIND CURRENT comprobantes NO-LOCK NO-ERROR.
                END.

                saldoAnexo = anexos.sdo_inicial.

                FIND FIRST cuentas WHERE cuentas.cuenta = anexos.cuenta NO-LOCK NO-ERROR.
                
                DO cont = 1 TO 12:
                    IF cuentas.naturaleza = "DB" THEN
                        saldoAnexo = saldoAnexo + anexos.db[cont] - anexos.cr[cont].
                    ELSE
                        saldoAnexo = saldoAnexo + anexos.cr[cont] - anexos.db[cont].
                END.

                IF saldoAnexo > 0 THEN DO:
                    /* Partida documento anterior */
                    CREATE mov_contable.
                    ASSIGN Mov_Contable.Agencia = anexos.agencia
                           mov_contable.nit = Clientes.Anterior_Nit
                           Mov_Contable.Cuenta = anexos.cuenta
                           Mov_Contable.Fec_Contable = W_Fecha
                           Mov_Contable.Comentario = "Cambio Documento Identificación"
                           Mov_Contable.Usuario = W_Usuario
                           Mov_Contable.Cen_Costos = W_Cencosgral
                           Mov_Contable.Comprobante = pComprobante
                           Mov_Contable.Num_Documento = pSecuencia
                           Mov_Contable.Fec_Grabacion = TODAY
                           Mov_Contable.Hora = TIME
                           Mov_Contable.Estacion = W_Estacion.

                    IF cuentas.naturaleza = "DB" THEN DO:
                        IF saldoAnexo > 0 THEN
                            mov_contable.cr = saldoAnexo.
                        ELSE
                            mov_contable.db = saldoAnexo.
                    END.
                    ELSE DO:
                        IF saldoAnexo > 0 THEN
                            mov_contable.db = saldoAnexo.
                        ELSE
                            mov_contable.cr = saldoAnexo.
                    END.
                    /* -------------------------- */

                    /* Partida documento nuevo */
                    CREATE mov_contable.
                    ASSIGN Mov_Contable.Agencia = anexos.agencia
                           mov_contable.nit = Clientes.nit
                           Mov_Contable.Cuenta = anexos.cuenta
                           Mov_Contable.Fec_Contable = W_Fecha
                           Mov_Contable.Comentario = "Cambio Documento Identificación"
                           Mov_Contable.Usuario = W_Usuario
                           Mov_Contable.Cen_Costos = W_Cencosgral
                           Mov_Contable.Comprobante = pComprobante
                           Mov_Contable.Num_Documento = pSecuencia
                           Mov_Contable.Fec_Grabacion = TODAY
                           Mov_Contable.Hora = TIME
                           Mov_Contable.Estacion = W_Estacion.

                    IF cuentas.naturaleza = "DB" THEN DO:
                        IF saldoAnexo > 0 THEN
                            mov_contable.db = saldoAnexo.
                        ELSE
                            mov_contable.cr = saldoAnexo.
                    END.
                    ELSE DO:
                        IF saldoAnexo > 0 THEN
                            mov_contable.cr = saldoAnexo.
                        ELSE
                            mov_contable.db = saldoAnexo.
                    END.
                    /* -------------------------- */
                END.
            END.

            FOR EACH relaciones WHERE relaciones.nit = Clientes.nit:SCREEN-VALUE:
                relaciones.nit = w_Nnit.
            END.

            FOR EACH relaciones WHERE relaciones.nit_relacion = Clientes.nit:SCREEN-VALUE:
                relaciones.nit_relacion = w_Nnit.
            END.

            CREATE Hoja_Vida.
            ASSIGN Hoja_Vida.Tipo = 1
                   Hoja_Vida.Codigo = 1
                   Hoja_Vida.Nit = W_Nnit
                   Hoja_Vida.Usuario = W_Usuario
                   Hoja_Vida.Fec_Grabacion = W_fecha
                   Hoja_Vida.Hora_Grabacion = TIME
                   Hoja_Vida.Observacion = "Se cambio el nit: " + Clientes.Anterior_Nit + " por el nit: " + W_Nnit.

            CREATE Hoja_Vida.
            ASSIGN Hoja_Vida.Tipo = 1
                   Hoja_Vida.Codigo = 1
                   Hoja_Vida.Nit = ttClientes.nit
                   Hoja_Vida.Usuario = W_Usuario
                   Hoja_Vida.Fec_Grabacion = W_fecha
                   Hoja_Vida.Hora_Grabacion = TIME
                   Hoja_Vida.Observacion = "Se cambio el nit: " + Clientes.Anterior_Nit + " por el nit: " + W_Nnit.

            CREATE clientes.
            BUFFER-COPY ttClientes TO clientes.
         
            RELEASE clientes.
            RELEASE mov_ahorros.
            RELEASE mov_creditos.
            RELEASE ahorros.
            RELEASE creditos.
            RELEASE planpagos.
            RELEASE mov_instancias.
            RELEASE solicitud.
            RELEASE Anexos.
            RELEASE relaciones.
            MESSAGE " Termino la operacion exitosamente " SKIP(1)
                    "El nuevo nit es " W_Nnit:SCREEN-VALUE  VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
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
     /* btn_procesar:SENSITIVE = FALSE. */
     FIND Clientes WHERE Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.

     IF AVAILABLE(Clientes) THEN
        W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
     ELSE DO:
        RUN C-Clientes.R(INPUT 2, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        ASSIGN Clientes.Nit:SCREEN-VALUE = P_Nit
               W_Nomtitular:SCREEN-VALUE = CAPS(TRIM(P_Nombre) + " " + P_Apellido).
        FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
     END.
     ASSIGN Clientes.nit:SENSITIVE = TRUE. 
      
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w_Nnit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w_Nnit C-Win
ON LEAVE OF w_Nnit IN FRAME Frm_Clientes /* Nuevo Nit */
DO:
    DO WITH FRAME F_Clientes:
        IF w_nNit:SCREEN-VALUE <> "" THEN DO:
            FIND FIRST Clientes WHERE Clientes.Nit EQ W_Nnit:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF AVAILABLE(Clientes) THEN DO:
                MESSAGE "Este Nit ya existe. No puede reasignarse..."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                W_Nnit:SCREEN-VALUE = "".
            END.
            ELSE
                btn_procesar:SENSITIVE = TRUE.
        END.
    END.
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

  {&OPEN-QUERY-Frm_Clientes}
  GET FIRST Frm_Clientes.
  DISPLAY w_Nnit W_NomTitular 
      WITH FRAME Frm_Clientes IN WINDOW C-Win.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Nit 
      WITH FRAME Frm_Clientes IN WINDOW C-Win.
  ENABLE Clientes.Nit w_Nnit Btn_Cancelar Btn_Consulta BUTTON-1 BUTTON-95 
         BtnDone-2 btn_procesar ECT-309 RECT-274 
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

