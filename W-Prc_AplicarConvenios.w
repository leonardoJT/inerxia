&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

{Incluido\variable.i "shared"}

DEFINE VAR nombreArchivo AS CHARACTER.
DEFINE VAR vlOkPressed AS LOGICAL.

DEFINE TEMP-TABLE registro
    FIELD agencia AS INTEGER
    FIELD cedula AS CHARACTER
    FIELD valor AS DECIMAL
    FIELD valorContrapartida AS DECIMAL.

DEFINE VAR totalCxP AS DECIMAL.
DEFINE VAR GMF_Aplicado AS DECIMAL.
DEFINE VAR numDocumento AS DECIMAL.
DEFINE VAR vCuenta AS CHARACTER.
DEFINE VAR vTime AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-347 btnImportar convenio CxC CxP ~
btnSalir 
&Scoped-Define DISPLAYED-OBJECTS convenio CxC CxP nitContrapartida ~
valorTotal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAplicar 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "Aplicar" 
     SIZE 10 BY 2.

DEFINE BUTTON btnImportar 
     IMAGE-UP FILE "imagenes/delimtxt.bmp":U
     LABEL "Importar" 
     SIZE 10 BY 2.

DEFINE BUTTON btnSalir 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "Salir" 
     SIZE 10 BY 2.

DEFINE VARIABLE convenio AS CHARACTER FORMAT "X(256)":U 
     LABEL "Convenio" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Auxilio Funerario","Auxilio por incapacidad médica","Seguro Tarjeta" 
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CxC AS CHARACTER FORMAT "X(256)":U INITIAL "16605004" 
     LABEL "Cuenta por cobrar" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CxP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cuenta por pagar (Empresa)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE nitContrapartida AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nit Contrapartida" 
     VIEW-AS FILL-IN 
     SIZE 16.14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE valorTotal AS DECIMAL FORMAT "->>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor total del recaudo" 
     VIEW-AS FILL-IN 
     SIZE 16.14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-347
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 8.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnImportar AT ROW 1.88 COL 45.72 WIDGET-ID 2
     convenio AT ROW 2.62 COL 11 COLON-ALIGNED WIDGET-ID 18
     btnAplicar AT ROW 3.96 COL 45.72 WIDGET-ID 20
     CxC AT ROW 4.88 COL 27.72 COLON-ALIGNED WIDGET-ID 6
     CxP AT ROW 6.04 COL 27.86 COLON-ALIGNED WIDGET-ID 12
     nitContrapartida AT ROW 7.88 COL 25.72 COLON-ALIGNED WIDGET-ID 14
     btnSalir AT ROW 8.31 COL 45.72 WIDGET-ID 24
     valorTotal AT ROW 8.96 COL 25.72 COLON-ALIGNED WIDGET-ID 16
     "RECAUDO DE CONVENIOS" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 1.54 COL 3.43 WIDGET-ID 4
          BGCOLOR 18 FGCOLOR 15 
     RECT-347 AT ROW 1.81 COL 2 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 55.72 BY 9.65 WIDGET-ID 100.


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
         TITLE              = "Recaudo de Convenios"
         HEIGHT             = 9.65
         WIDTH              = 55.72
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* SETTINGS FOR BUTTON btnAplicar IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN nitContrapartida IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN valorTotal IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Recaudo de Convenios */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Recaudo de Convenios */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAplicar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAplicar C-Win
ON CHOOSE OF btnAplicar IN FRAME DEFAULT-FRAME /* Aplicar */
DO:
    DEFINE VAR retiro AS DECIMAL.
    DEFINE VAR pError AS LOGICAL.
    
    vTime = TIME.
    
    ASSIGN CxC
           CxP
           nitContrapartida
           valorTotal.

    MESSAGE "Está seguro que desea recaudar los valores para el convenio" convenio:SCREEN-VALUE + "?." SKIP
            "Esta operacion no se puede deshacer, continue sólo si está seguro(a)."
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Recaudo de Convenio" UPDATE aplicar AS LOGICAL.

    IF aplicar = NO THEN
        RETURN NO-APPLY.

    OUTPUT TO VALUE("c:\INFO_Fodun\RecaudoDeConvenio" + w_usuario + STRING(DAY(w_fecha),"99") + STRING(MONTH(w_fecha),"99") + STRING(YEAR(w_fecha),"9999") + ".txt").
    DISPLAY "Agencia Cédula TipoAhorro CodAhorro Nombre Producto Recaudo".
    FOR EACH registro WHERE registro.valor > 0
                        AND registro.valorContrapartida = 0 BREAK BY registro.agencia:
        IF FIRST-OF(registro.agencia) THEN DO:
            FIND FIRST comprobantes WHERE comprobantes.comprobante = 21
                                      AND comprobantes.agencia = registro.agencia NO-ERROR.
            IF AVAILABLE comprobantes THEN DO:
                comprobantes.secuencia = comprobantes.secuencia + 1.
                numDocumento = comprobantes.secuencia.

                FIND CURRENT comprobantes NO-LOCK NO-ERROR.
            END.
        END.

        retiro = 0.

        IF registro.valor > 0 THEN DO:
            FIND FIRST ahorros WHERE ahorros.nit = registro.cedula
                                 AND ahorros.tip_ahorro = 1
                                 AND ahorros.cod_ahorro = 4
                                 AND ahorros.agencia = registro.agencia
                                 AND ahorros.sdo_disponible > 0
                                 AND ahorros.estado = 1 NO-ERROR.
            IF AVAILABLE ahorros THEN DO:
                FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Tip_Ahorro = ahorros.tip_ahorro
                                         AND pro_ahorros.cod_ahorro = ahorros.cod_ahorro
                                         AND Pro_Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
                IF AVAILABLE pro_ahorros THEN DO:
                    FIND FIRST CortoLargo WHERE CortoLargo.Clase_Producto = 1
                                            AND CortoLargo.Cod_Producto = Pro_Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
                    IF AVAILABLE cortoLargo THEN
                        vCuenta = CortoLargo.Cta_AsoAd.
                END.

                IF ahorros.sdo_disponible >= registro.valor * 1.004 THEN DO:
                    ahorros.sdo_disponible = ahorros.sdo_disponible - registro.valor.
                    ahorros.fec_ultTrans = w_fecha.
                    retiro = registro.valor.
                    registro.valor = 0.
                END.
                ELSE DO:
                    retiro = TRUNCATE((ahorros.sdo_disponible * 100) / 100.4,0).
                    ahorros.sdo_disponible = ahorros.sdo_disponible - TRUNCATE((ahorros.sdo_disponible * 100) / 100.4,0).
                    ahorros.fec_ultTrans = w_fecha.
                    registro.valor = registro.valor - retiro.
                END.

                IF retiro > 0 THEN DO:
                    DISPLAY ahorros.agencia ahorros.nit ahorros.tip_ahorro ahorros.cod_ahorro Pro_Ahorros.Nom_Producto FORMAT "X(20)" retiro FORMAT ">>>,>>>,>>9"
                        WITH WIDTH 150 NO-LABELS.

                    RUN movAhorros.
                    IF AVAILABLE mov_ahorros THEN
                        mov_ahorros.val_efectivo = retiro.

                    RUN movContable.
                    IF AVAILABLE mov_contable THEN
                        mov_contable.db = retiro.

                    totalCxP = totalCxP + retiro.

                    RUN rutGMF.r(INPUT TRUE,
                                 INPUT w_agencia,
                                 INPUT Ahorros.Agencia,
                                 INPUT 1,
                                 INPUT Ahorros.Cod_Ahorro,
                                 INPUT Ahorros.Nit,
                                 INPUT Ahorros.Cue_Ahorro,
                                 INPUT 010102001,
                                 INPUT retiro,
                                 INPUT 21,
                                 INPUT STRING(numDocumento),
                                 INPUT convenio:SCREEN-VALUE,
                                 INPUT 0,
                                 INPUT 3,
                                 OUTPUT GMF_Aplicado) NO-ERROR.
                END.
            END.
        END.


        /* Le cargamos al crédito rotativo en caso que no tenga ahorros */
        IF registro.valor > 0 AND convenio:SCREEN-VALUE = "Seguro Tarjeta" THEN DO:
            FIND FIRST creditos WHERE creditos.nit = registro.cedula
                                  AND creditos.cod_credito = 123
                                  AND creditos.estado = 2 NO-ERROR.
            IF AVAILABLE creditos THEN DO:
                FIND FIRST Pro_creditos WHERE Pro_creditos.Tip_credito = creditos.tip_credito
                                          AND pro_creditos.cod_credito = creditos.cod_credito
                                          AND Pro_creditos.Estado EQ 1 NO-LOCK NO-ERROR.
                IF AVAILABLE pro_creditos THEN DO:
                    FIND FIRST CortoLargo WHERE CortoLargo.Clase_Producto EQ 2
                                            AND CortoLargo.Cod_Producto EQ Pro_creditos.Cod_credito NO-LOCK NO-ERROR.
                    IF AVAILABLE cortoLargo THEN
                        vCuenta = CortoLargo.Cta_AsoAd.
                END.

                creditos.Sdo_capital = Creditos.Sdo_capital + registro.valor.

                RUN p-utilizacionRotativo.r (INPUT 'INT',
                                             INPUT creditos.nit,
                                             INPUT creditos.num_credito,
                                             INPUT 'Cobro Seguro Tarjeta',
                                             INPUT registro.valor,
                                             OUTPUT pError) NO-ERROR.

                CREATE mov_contable.
                ASSIGN Mov_Contable.Agencia = creditos.agencia
                       Mov_Contable.Destino = creditos.agencia
                       Mov_Contable.Comprobante = 21
                       Mov_Contable.Num_Documento = numDocumento
                       Mov_contable.Doc_referencia = creditos.pagare
                       Mov_Contable.Fec_Contable = w_fecha
                       Mov_Contable.Fec_Grabacion = TODAY
                       Mov_Contable.Cuenta = vCuenta
                       Mov_Contable.Comentario = convenio:SCREEN-VALUE
                       Mov_Contable.Usuario = W_usuario
                       Mov_Contable.Estacion = "005"
                       Mov_Contable.Nit = registro.cedula
                       mov_contable.DB = registro.valor
                       mov_contable.cen_costos = 999.

                CREATE Mov_Creditos.
                ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
                       Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
                       Mov_Creditos.Nit = Creditos.Nit
                       Mov_Creditos.Num_Credito = Creditos.Num_Credito
                       Mov_Creditos.Cod_Operacion = 020102001
                       Mov_Creditos.Ofi_Destino = Creditos.Agencia
                       Mov_Creditos.Ofi_Fuente = W_Agencia
                       Mov_Creditos.Pagare = Creditos.Pagare
                       Mov_Creditos.Fecha = W_Fecha
                       Mov_Creditos.Hora = vTime
                       Mov_Creditos.Num_Documento = STRING(numDocumento)
                       Mov_Creditos.Usuario = W_Usuario
                       Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
                       Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
                       Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
                       Mov_Creditos.Val_Efectivo = registro.valor
                       Mov_Creditos.Cpte = 21
                       Mov_Creditos.Descrip = "Pago Seguro Tarjeta".

                totalCxP = totalCxP + registro.valor.
                registro.valor = 0.
            END.
        END.
        
        IF registro.valor > 0 THEN DO:
            DISPLAY SKIP(1)
                    registro.agencia
                    registro.cedula FORMAT "X(10)"
                    CxC FORMAT "X(10)"
                    registro.valor FORMAT ">>>,>>>,>>9"
                WITH WIDTH 150 NO-LABELS.

            CREATE mov_contable.
            ASSIGN Mov_Contable.Agencia = registro.agencia
                   Mov_Contable.Destino = registro.agencia
                   Mov_Contable.Comprobante = 21
                   Mov_Contable.Num_Documento = numDocumento
                   Mov_contable.Doc_referencia = STRING(numDocumento)
                   Mov_Contable.Fec_Contable = w_fecha
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Cuenta = CxC
                   Mov_Contable.Comentario = convenio:SCREEN-VALUE
                   Mov_Contable.Usuario = W_usuario
                   Mov_Contable.Estacion = "005"
                   Mov_Contable.Nit = registro.cedula
                   mov_contable.DB = registro.valor
                   mov_contable.cen_costos = 999.

            totalCxP = totalCxP + registro.valor.
            registro.valor = 0.
        END.

        IF LAST-OF(registro.agencia) THEN DO:
            CREATE mov_contable.
            ASSIGN Mov_Contable.Agencia = registro.agencia
                   Mov_Contable.Destino = registro.agencia
                   Mov_Contable.Comprobante = 21
                   Mov_Contable.Num_Documento = numDocumento
                   Mov_contable.Doc_referencia = STRING(numDocumento)
                   Mov_Contable.Fec_Contable = w_fecha
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Cuenta = CxP
                   Mov_Contable.Comentario = convenio:SCREEN-VALUE
                   Mov_Contable.Usuario = W_usuario
                   Mov_Contable.Estacion = "005"
                   Mov_Contable.Nit = nitContrapartida
                   mov_contable.CR = totalCxP
                   mov_contable.cen_costos = 999.

            totalCxP = 0.
        END.
    END.
    OUTPUT CLOSE.

    MESSAGE "El proceso terminó de forma exitosa"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    APPLY "choose" TO btnSalir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImportar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImportar C-Win
ON CHOOSE OF btnImportar IN FRAME DEFAULT-FRAME /* Importar */
DO:
    DEFINE VAR sumaArchivo AS DECIMAL.
    DEFINE VAR flagError AS LOGICAL.

    SYSTEM-DIALOG GET-FILE nombreArchivo
        TITLE "Selección Archivo plano..."
        FILTERS "Delimitados por comas (*.csv)" "*.csv"
        MUST-EXIST
        USE-FILENAME
        UPDATE vlOkPressed.

    IF nombreArchivo <> "" THEN DO:
        EMPTY TEMP-TABLE registro.

        INPUT FROM VALUE(nombreArchivo).

        REPEAT:
            CREATE registro.
            IMPORT DELIMITER ";" registro.

            IF registro.valorContrapartida > 0 THEN
                ASSIGN nitContrapartida = registro.cedula
                       valorTotal = registro.valorContrapartida.

            sumaArchivo = sumaArchivo + registro.valor.

            FIND FIRST clientes WHERE clientes.nit = registro.cedula NO-LOCK NO-ERROR.
            IF NOT AVAILABLE clientes THEN DO:
                MESSAGE "La cédula" registro.cedula "no se encuentra matriculada en el Sistema." SKIP
                        "No se permite la operación hasta tanto no se corrija este documento o se elimine del archivo."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                /*RETURN NO-APPLY.*/
                flagError = TRUE.
            END.

            IF registro.valorContraPartida = 0 THEN DO:
                FIND FIRST ahorros WHERE ahorros.agencia = registro.agencia
                                     AND ahorros.tip_ahorro = 4
                                     AND ahorros.nit = registro.cedula
                                     AND ahorros.estado = 1 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE ahorros THEN DO:
                    MESSAGE "La cédula" registro.cedula "no pertence a un Asociado que se encuentre matriculado en la agencia" registro.agencia SKIP
                            "Revise la información contenida en el archivo, y repita la operación de subida del archivo. El proceso se cancelará..."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    flagError = TRUE.
                END.
            END.
        END.

        IF flagError = TRUE THEN
            RETURN NO-APPLY.

        nitContrapartida:SCREEN-VALUE = nitContrapartida.
        valorTotal:SCREEN-VALUE = STRING(valorTotal).

        ASSIGN nitContrapartida
               valorTotal.

        IF sumaArchivo = valorTotal THEN DO:
            btnImportar:SENSITIVE = FALSE.
            btnAplicar:SENSITIVE = TRUE.

            MESSAGE "El archivo se importó con éxito"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE DO:
            MESSAGE "No coincide la suma de los valores individuales con el" SKIP
                    "valor reportado como Total." SKIP
                    "El archivo no fue importado."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
    ELSE
        MESSAGE "Importación de Archivo Cancelada"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSalir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSalir C-Win
ON CHOOSE OF btnSalir IN FRAME DEFAULT-FRAME /* Salir */
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


&Scoped-define SELF-NAME convenio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL convenio C-Win
ON VALUE-CHANGED OF convenio IN FRAME DEFAULT-FRAME /* Convenio */
DO:
    CxP:SCREEN-VALUE = "".

    CASE convenio:SCREEN-VALUE:
        WHEN "Auxilio funerario" THEN CxP:SCREEN-VALUE = "26950501".
        WHEN "Auxilio por incapacidad médica" THEN CxP:SCREEN-VALUE = "26950502".
        WHEN "Seguro Tarjeta" THEN CxP:SCREEN-VALUE = "24101002".
    END CASE.

    ASSIGN CxP.

    btnAplicar:SENSITIVE = FALSE.
    btnImportar:SENSITIVE = TRUE.
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

    btnAplicar:SENSITIVE = FALSE.
    btnImportar:SENSITIVE = TRUE.

  RUN enable_UI.
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
  DISPLAY convenio CxC CxP nitContrapartida valorTotal 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-347 btnImportar convenio CxC CxP btnSalir 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MovAhorros C-Win 
PROCEDURE MovAhorros :
CREATE Mov_Ahorros.
ASSIGN Mov_Ahorros.Agencia = Ahorros.Agencia
       Mov_Ahorros.Age_Destino = Ahorros.Agencia
       Mov_Ahorros.Age_Fuente = w_agencia
       Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
       Mov_Ahorros.Cue_Ahorros = Ahorros.Cue_Ahorro
       Mov_Ahorros.Fecha = W_Fecha
       Mov_Ahorros.Hora = TIME
       Mov_Ahorros.Nit = Ahorros.Nit
       Mov_Ahorros.Num_Documento = STRING(numDocumento)
       Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
       Mov_Ahorros.Usuario = W_Usuario
       Mov_Ahorros.Cod_Operacion = 010102001
       Mov_Ahorros.Descrip = convenio:SCREEN-VALUE IN FRAME default-frame.
       Mov_Ahorros.Cpte = 21.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MovContable C-Win 
PROCEDURE MovContable :
CREATE mov_contable.
ASSIGN Mov_Contable.Agencia = ahorros.agencia
       Mov_Contable.Destino = ahorros.agencia
       Mov_Contable.Comprobante = 21
       Mov_Contable.Num_Documento = numDocumento
       Mov_contable.Doc_referencia = ahorros.cue_ahorros
       Mov_Contable.Fec_Contable = w_fecha
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Cuenta = vCuenta
       Mov_Contable.Comentario = convenio:SCREEN-VALUE IN FRAME default-frame
       Mov_Contable.Usuario = W_usuario
       Mov_Contable.Estacion = "005"
       Mov_Contable.Nit = ahorros.nit
       mov_contable.cen_costos = 999.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

