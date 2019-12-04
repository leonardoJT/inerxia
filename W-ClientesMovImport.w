&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

{Incluido\variable.i "shared"}

/* Parámetros */
DEFINE VAR nitCompensacion AS CHARACTER INITIAL "890203088".

DEFINE VARIABLE vlOkPressed AS LOGICAL NO-UNDO.
DEFINE VARIABLE vcComando AS CHARACTER NO-UNDO.
DEFINE variable vGrupoTransaccional AS INTEGER.
DEFINE variable vTransaccion AS CHARACTER.
DEFINE variable vSecuencia AS CHARACTER.
DEFINE variable vOrigen AS CHARACTER.
DEFINE variable documento_cliente AS CHARACTER.
DEFINE variable vCuenta1_origen AS CHARACTER.
DEFINE variable vCuenta2_destino AS CHARACTER.
DEFINE VARIABLE wdoc LIKE comprobantes.comprobante.
DEFINE variable valor AS DECIMAL.
DEFINE variable valor_transaccion AS DECIMAL.
DEFINE VARIABLE tottra AS DECIMAL FORMAT ">>,>>>,>>>>,>>>".
DEFINE VARIABLE totcom AS DECIMAL FORMAT ">>,>>>,>>>>,>>>".
DEFINE VARIABLE totreg AS INTEGER.

DEFINE TEMP-TABLE ECG
    FIELD fecha AS CHARACTER /* Fecha */
    FIELD hora AS CHARACTER /* Hora */
    FIELD comercio AS CHARACTER /* Comercio */
    FIELD sucursal AS CHARACTER /* Sucursal */
    FIELD vTerminal AS CHARACTER /* Terminal */
    FIELD grupoTransaccional AS CHARACTER /* Grupo transaccional */
    FIELD transaccion AS CHARACTER /* Transacción */
    FIELD secuencia AS CHARACTER /* Secuencia */
    FIELD origen AS CHARACTER /* Origen */
    FIELD documentoCliente AS CHARACTER /* Documento Cliente */
    FIELD tipoDocumentoCliente AS CHARACTER /* Tipo de documento */
    FIELD documentoClienteDestino AS CHARACTER /* Documento Cliente Destino */
    FIELD tipoDocumentoClienteDestino AS CHARACTER /* Tipo de documento cliente destino */
    FIELD causalTransaccion AS CHARACTER /* Causal de la Transacción */
    FIELD adquiriente AS CHARACTER /* Adquieriente */
    FIELD numeroDeAutorizacion AS CHARACTER
    FIELD PCI_TerminalSerial AS CHARACTER /* PCI Terminal Serial */
    FIELD terminalUbicacion AS CHARACTER /* Terminal - Ubicación */
    FIELD TerminalTipo AS CHARACTER /* Terminal - Tipo */
    FIELD cuentaOrigen AS CHARACTER /* Cuenta 1 / Origen */
    FIELD cuentaDestino AS CHARACTER /* Cuenta 2 / Destino */
    FIELD entidadDuenaCuentaOrigen AS CHARACTER /* Entidad dueña cuenta origen */
    FIELD entidadDuenaCuentaDestino AS CHARACTER /* Entidad dueña cuenta destino */
    FIELD entidadDuenaTerminal AS CHARACTER /* Entidad dueña terminal */
    FIELD entidadDuenaTerminalNit AS CHARACTER /* Comercio - Terminal - NIT/CC */
    FIELD entidadDuenaTerminalDescripcion AS CHARACTER /* Comercio - Terminal - Descripción */
    FIELD chequeCodigoDeBanco AS CHARACTER /* Cheque - Código de banco */
    FIELD chequeCuentaDeBanco AS CHARACTER /* Cheque - Cuenta de banco */
    FIELD chequeNumero AS CHARACTER /* Cheque - Número */
    FIELD chequeTotalCheques AS CHARACTER /* Cheque - Total cheques */
    FIELD codigoDeBarras AS CHARACTER /* Código de barras */
    FIELD numeroTarjeta AS CHARACTER /* Número de tarjeta */
    FIELD fechaContable AS CHARACTER /* Fecha contable */
    FIELD valor AS CHARACTER /* Valor */
    FIELD valorBase AS CHARACTER /* Valor base */
    FIELD valorImpuesto AS CHARACTER /* Valor impuesto */
    FIELD valorRetencion AS CHARACTER /* Valor reteción */
    FIELD valorPropina AS CHARACTER /* Valor propina */
    FIELD valorComision AS CHARACTER /* Valor comisión */
    FIELD cuotas AS CHARACTER /* Cuotas */
    FIELD numeroDeFactura AS CHARACTER /* Número de factura */
    FIELD tipoCuentaOrigen AS CHARACTER /* Tipo de cuenta 1 / Origen */
    FIELD tipoCuentaDestino AS CHARACTER /* Tipo de cuenta 2 / Destino */
    FIELD secuenciaReversar AS CHARACTER /* Tipo de cuenta 3 */
    FIELD usuarioAutorizador AS CHARACTER /* Usuario / Autorizador */
    FIELD usuarioCaja AS CHARACTER /* Usuariol / Caja */
    FIELD codigoDeError AS CHARACTER /* codigo error */
    FIELD CONTABILIZADO AS LOGICAL.

/* Archivo plano .autcon para conciliación */
DEFINE TEMP-TABLE autcon
    FIELD fechaTransaccion AS CHARACTER
    FIELD horaTransaccion AS CHARACTER
    FIELD comercio AS CHARACTER
    FIELD sucursal AS CHARACTER
    FIELD grupoTransaccional AS CHARACTER
    FIELD transaccion AS CHARACTER
    FIELD secuencia AS CHARACTER
    FIELD origen AS CHARACTER
    FIELD causalConvenio AS CHARACTER
    FIELD adquiriente AS CHARACTER
    FIELD PCI_TerminalSerial AS CHARACTER
    FIELD TerminalUbicacion AS CHARACTER
    FIELD TerminalTipo AS CHARACTER
    FIELD EntidadDuenaCuentaOrigen AS CHARACTER
    FIELD EntidadDuenaCuentaDestino AS CHARACTER
    FIELD EntidadDuenaCuentaTerminal AS CHARACTER
    FIELD CodigoConvenio AS CHARACTER
    FIELD NumeroTarjeta AS CHARACTER
    FIELD FechaContable AS CHARACTER
    FIELD Valor AS DECIMAL
    FIELD ValorBase AS DECIMAL
    FIELD ValorImpuesto AS DECIMAL
    FIELD ValorRetencion AS DECIMAL
    FIELD CodigoError AS CHARACTER
    FIELD identificacionAnulacion AS INTEGER
    FIELD transaccionLibre AS INTEGER
    FIELD entidadDeudora AS CHARACTER
    FIELD entidadAcreedora AS CHARACTER
    FIELD inconsistencia AS CHARACTER
    FIELD comisionRedes AS DECIMAL
    FIELD enrutamientoRedes AS DECIMAL
    FIELD comisionVisionamos AS DECIMAL
    FIELD comisionIntercooperativa AS DECIMAL.

DEFINE VAR cuentaRotativo AS CHARACTER.
DEFINE VAR cuentaSyA AS CHARACTER.

DEFINE TEMP-TABLE comaut
    FIELD fechaContable AS CHARACTER
    FIELD tipoRegistro AS CHARACTER
    FIELD codigoCooperativaOrigen AS CHARACTER
    FIELD valorCobrar AS DECIMAL FORMAT "$->>>,>>>,>>9.99"
    FIELD valorPagar AS DECIMAL FORMAT "$->>>,>>>,>>9.99".

DEFINE TEMP-TABLE aju
    FIELD fechaContable AS CHARACTER
    FIELD fecha AS CHARACTER
    FIELD hora AS CHARACTER
    FIELD fechaEvento AS CHARACTER
    FIELD compensadorOrigen AS CHARACTER
    FIELD compensadorDestino AS CHARACTER
    FIELD entidadOrigen AS CHARACTER
    FIELD entidadDestino AS CHARACTER
    FIELD tipo AS CHARACTER
    FIELD tarjeta AS CHARACTER
    FIELD secuencia AS CHARACTER
    FIELD operacion AS CHARACTER
    FIELD valor AS DECIMAL.

DEFINE TEMP-TABLE ttFOR
    FIELD fechaContable AS CHARACTER
    FIELD codigoTerminal AS CHARACTER
    FIELD NumOperacionesPropias AS CHARACTER
    FIELD NumOperacionesForaneasNales AS CHARACTER
    FIELD NumOperacionesForaneasInter AS CHARACTER
    FIELD NumOperacionesFamiliasEnAccion AS CHARACTER.

DEFINE TEMP-TABLE informe
    FIELD concepto AS CHARACTER
    FIELD entidad AS CHARACTER
    FIELD CxP AS DECIMAL FORMAT "$->>>,>>>,>>9.99"
    FIELD CxC AS DECIMAL FORMAT "$->>>,>>>,>>9.99"
    FIELD pdf AS DECIMAL FORMAT "$->>>,>>>,>>9.99".

DEFINE TEMP-TABLE dis
    FIELD registro AS CHARACTER
    FIELD fechaContable AS CHARACTER
    FIELD codigoTerminal AS CHARACTER
    FIELD numOperaciones AS INTEGER
    FIELD valor AS DECIMAL.

DEFINE TEMP-TABLE abo
    FIELD fechaContable AS CHARACTER
    FIELD entidadDuenaCuentaOrigen AS CHARACTER
    FIELD numeroTarjeta AS CHARACTER
    FIELD transaccion AS CHARACTER
    FIELD secuencia AS CHARACTER
    FIELD valor AS DECIMAL
    FIELD codigoTerminal AS CHARACTER
    FIELD ubicacionTerminal AS CHARACTER
    FIELD codError AS CHARACTER
    FIELD codAbono AS CHARACTER
    FIELD causalAbono AS CHARACTER
    FIELD fechaAbono AS CHARACTER
    FIELD valorAbono AS DECIMAL.

DEFINE TEMP-TABLE cms
    FIELD registro AS CHARACTER
    FIELD fecha AS CHARACTER
    FIELD codigoRed AS CHARACTER
    FIELD numeroTarjeta AS CHARACTER
    FIELD ubicacionEstablecimiento AS CHARACTER
    FIELD fechaHora AS CHARACTER
    FIELD secuencia AS CHARACTER
    FIELD numeroAutorizacion AS CHARACTER
    FIELD establecimiento AS CHARACTER
    FIELD valorComision AS DECIMAL
    FIELD valorCompra AS DECIMAL.

DEFINE VAR totalCxC AS DECIMAL.
DEFINE VAR totalCxP AS DECIMAL.

DEFINE TEMP-TABLE archivoAplicados
    FIELD fecha AS CHARACTER
    FIELD grupoTransaccional AS CHARACTER
    FIELD secuencia AS CHARACTER
    FIELD cedula AS CHARACTER
    FIELD terminalUbicacion AS CHARACTER
    FIELD numeroTarjeta AS CHARACTER
    FIELD fechaContable AS CHARACTER
    FIELD valor AS DECIMAL
    FIELD valorComision AS DECIMAL
    FIELD comentario AS CHARACTER.

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
&Scoped-Define ENABLED-OBJECTS RECT-346 RECT-345 btnConciliar B-Importar ~
BtnCancel Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS w-totreg W-TotTx W-TotCom w-Mensaje 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD intToDate C-Win 
FUNCTION intToDate RETURNS DATE
  ( INPUT ipcFecha  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Importar 
     LABEL "I&mportar" 
     SIZE 12 BY 1.19.

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 12 BY 1.19
     BGCOLOR 8 .

DEFINE BUTTON btnConciliar 
     LABEL "Conciliar" 
     SIZE 12 BY 1.19.

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 12 BY 1.19 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE VARIABLE w-Mensaje AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .54
     BGCOLOR 8 FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE W-TotCom AS INT64 FORMAT "->,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w-totreg AS DECIMAL FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W-TotTx AS INT64 FORMAT "->,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-345
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 3.19
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE RECT-346
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 10.77.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnConciliar AT ROW 1.38 COL 43 WIDGET-ID 192
     B-Importar AT ROW 2.81 COL 43 WIDGET-ID 188
     w-totreg AT ROW 3.19 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     W-TotTx AT ROW 4.12 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 168
     BtnCancel AT ROW 4.19 COL 43 WIDGET-ID 190
     W-TotCom AT ROW 5.04 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     Btn_Done AT ROW 5.65 COL 43 WIDGET-ID 20
     w-Mensaje AT ROW 6.35 COL 4 NO-LABEL WIDGET-ID 182
     " Registros Leídos:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 3.19 COL 10.86 WIDGET-ID 8
          FGCOLOR 12 
     " Resumen" VIEW-AS TEXT
          SIZE 8.14 BY .81 AT ROW 2.46 COL 4.86 WIDGET-ID 4
          FGCOLOR 0 
     "Valor Transacciones:" VIEW-AS TEXT
          SIZE 15 BY .81 AT ROW 4.12 COL 9 WIDGET-ID 172
          FGCOLOR 12 
     "Valor Comisiones:" VIEW-AS TEXT
          SIZE 11.86 BY .81 AT ROW 5 COL 11.29 WIDGET-ID 174
          FGCOLOR 12 
     "IMPORTAR OPERACIONES TARJETAS" VIEW-AS TEXT
          SIZE 35 BY .5 AT ROW 1.54 COL 5.72 WIDGET-ID 26
          BGCOLOR 8 FONT 1
     RECT-346 AT ROW 1 COL 1 WIDGET-ID 186
     RECT-345 AT ROW 2.96 COL 4 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70.14 BY 10.81
         BGCOLOR 17 FONT 4
         CANCEL-BUTTON BtnCancel WIDGET-ID 100.


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
         TITLE              = "Importar movimientos - W-ClientesMovImport.w"
         HEIGHT             = 6.31
         WIDTH              = 55.57
         MAX-HEIGHT         = 31.85
         MAX-WIDTH          = 205.72
         VIRTUAL-HEIGHT     = 31.85
         VIRTUAL-WIDTH      = 205.72
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
/* SETTINGS FOR FILL-IN w-Mensaje IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W-TotCom IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       W-TotCom:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN w-totreg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       w-totreg:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN W-TotTx IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       W-TotTx:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Importar movimientos - W-ClientesMovImport.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Importar movimientos - W-ClientesMovImport.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Importar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Importar C-Win
ON CHOOSE OF B-Importar IN FRAME DEFAULT-FRAME /* Importar */
DO:
    DEFINE VARIABLE vcRaiz AS CHARACTER INITIAL "c:\info_fodun\" NO-UNDO.
    DEFINE VARIABLE vcNomArchInput AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcFecMov AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vdaFecMov AS DATE NO-UNDO.

    vTime = TIME.

    EMPTY TEMP-TABLE ecg.
    EMPTY TEMP-TABLE ArchivoAplicados.

    RUN IniciaVar.

    FIND FIRST cfg_tarjetaDB NO-LOCK NO-ERROR.

    FIND FIRST procDia WHERE procDia.cod_proceso = 13
                         AND procDia.agencia = w_agencia
                         AND procDia.fecha_proc = w_fecha
                         AND procDia.estado = 2 NO-LOCK NO-ERROR.
    IF AVAILABLE procDia THEN DO:
        MESSAGE "El archivo de movimientos ya fue cargado para esta fecha. No es posible aplicar de nuevo la información"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    /* Validación de Usuario */
    IF w_usuario <> cfg_tarjetaDB.usuarioAdministrador THEN DO:
        MESSAGE "Su usuario no está autorizado para realizar esta operación." SKIP
                "Consulte con el Administrador del Sistema..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    /* Se selecciona el archivo a importar */
    SYSTEM-DIALOG GET-FILE vcNomArchInput
        TITLE "Selección Archivo Movimiento ..."
        FILTERS "Comprimidos (*.7z)" "*.7z", "Todos (*.*)" "*.*"
        MUST-EXIST
        USE-FILENAME
        UPDATE vlOkPressed.

    /* Como está comprimido en 7z, procedemos a descomprimir */
    ASSIGN vcComando = "C:\7-Zip\7z".
    /*OS-COMMAND SILENT VALUE(vcComando) x VALUE(vcNomArchInput) -oc:\info_FODUN\Visionamos -y -pD05207C1A254DAB3B07032F70EC25432.*/
    OS-COMMAND SILENT VALUE(vcComando) x VALUE(vcNomArchInput) -oc:\info_FODUN\Visionamos -y -pAF26BE74CD87AF23AC24AB65BA96DB12.

    ASSIGN vcRaiz = vcRaiz + "visionamos\"
           vcFecMov = SUBSTRING(vcNomArchInput,(LENGTH(vcNomArchInput) - 23),8)
           vcNomArchInput = SUBSTRING(vcNomArchInput,(LENGTH(vcNomArchInput) - 23),8) + ".AUT"
           vcNomArchInput = vcRaiz + vcNomArchInput.

    ASSIGN vdaFecMov = intToDate(vcFecMov)
           w-Mensaje:SCREEN-VALUE = "Cargando Archivo " + vcNomArchInput.

    FIND FIRST procDia WHERE procDia.cod_proceso = 13
                         AND procDia.fecha_proc = vdaFecMov NO-LOCK NO-ERROR.
    IF AVAILABLE procDia THEN DO:
        MESSAGE "El archivo de movimientos ya fue cargado para esta fecha. No es posible aplicar de nuevo la información"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.

    IF vcNomArchInput NE "" THEN DO:
        tdbError:
        DO TRANSACTION ON ERROR UNDO tdbError:
            FIND FIRST controlMovTDB WHERE controlMovTDB.fecMov EQ vdaFecMov NO-LOCK NO-ERROR.
            IF AVAILABLE controlMovTDB THEN DO:
                FIND FIRST usuarios WHERE usuarios.usuario EQ controlMovTDB.usuario NO-LOCK NO-ERROR.
                
                MESSAGE "Archivo ya fué procesado" SKIP
                        "el día: " controlMovTDB.fecCarga SKIP
                        "Registros: "controlMovTDB.numRegPro
                        "Usuario: " Usuarios.nombre
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                ASSIGN w-Mensaje:SCREEN-VALUE = "Archivo " + vcNomArchInput + " ya ha sido Importado".
            END.
            ELSE DO: /*Archivo no ha sido cargado*/
                INPUT FROM VALUE(vcNomArchInput).
                    REPEAT:
                        CREATE ecg.
                        IMPORT DELIMITER "," ecg.

                        IF ecg.fecha NE "00000018" THEN
                            totreg = totreg + 1.
                    END.
                INPUT CLOSE.

                ASSIGN w-totreg:SCREEN-VALUE IN FRAME default-frame = STRING(totreg,">>>,>>>,>>9")
                       totreg = 0.

                RUN ActualizarTablaTarjetas.

                FOR EACH ecg WHERE ecg.fecha <> "00000018" AND ecg.grupoTransaccional <> "0220":
                    FIND FIRST aplicarVisionamos WHERE aplicarVisionamos.fecha = ecg.fecha
                                                   AND aplicarVisionamos.grupo_transaccional = ecg.grupotransaccional
                                                   AND aplicarVisionamos.transaccion = ecg.transaccion
                                                   AND aplicarVisionamos.secuencia = ecg.secuencia
                                                   AND aplicarVisionamos.origen = ecg.origen
                                                   AND aplicarVisionamos.doc_cliente = ecg.documentoCliente
                                                   AND aplicarVisionamos.tipo_terminal = ecg.terminalTipo
                                                   AND aplicarVisionamos.cuenta1_origen = ecg.cuentaOrigen
                                                   AND aplicarVisionamos.cuenta2_destino = ecg.cuentaDestino
                                                   AND aplicarVisionamos.num_tarjeta = ecg.numeroTarjeta
                                                   AND DECIMAL(aplicarVisionamos.valor) = DECIMAL(ecg.valor)
                                                   AND aplicarVisionamos.tipoCuenta1_origen = ecg.tipoCuentaOrigen
                                                   AND aplicarVisionamos.tipoCuenta2_destino = ecg.tipoCuentaDestino
                                                   AND aplicarVisionamos.estado = 2 NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE aplicarVisionamos THEN DO:
                        totreg = totreg + 1.

                        CREATE archivoAplicados.

                        CASE ECG.grupoTransaccional:
                            WHEN "0200" THEN vGrupoTransaccional = 0.    /* Transacción normal - Validar condiciones y aplicar */
                            WHEN "0400" THEN vGrupoTransaccional = 2.    /* Reversión Red Visionamos */
                            WHEN "0420" THEN vGrupoTransaccional = 2.    /* Reversión Red Visionamos */
                        END.

                        archivoAplicados.grupoTransaccional = ecg.grupoTransaccional.

                        CASE ECG.transaccion:  /* Codigo_trans */
                            WHEN "00" THEN vTransaccion = "CR".
                            WHEN "01" THEN vTransaccion = "RE".
                            WHEN "02" THEN vTransaccion = "AjDB".
                            WHEN "20" THEN vTransaccion = "ACR".
                            WHEN "21" THEN vTransaccion = "CE".
                            WHEN "22" THEN vTransaccion = "AjCR".
                            WHEN "30" THEN vTransaccion = "CS".
                            WHEN "35" THEN vTransaccion = "CM".
                            WHEN "40" THEN vTransaccion = "TF".
                            WHEN "89" THEN vTransaccion = "CCT".
                            WHEN "90" THEN vTransaccion = "PIN".
                        END.

                        IF vTransaccion = "ACR" THEN
                            vGrupoTransaccional = 2.

                        vSecuencia = ECG.secuencia.

                        /* oakley */

                        archivoAplicados.secuencia = vSecuencia.

                        IF ECG.Origen = "01" OR ECG.Origen = "02" OR ECG.Origen = "03" OR ECG.Origen = "04" OR ECG.Origen = "05" THEN DO:
                            IF vTransaccion = "RE" THEN
                                vTransaccion = "RR".

                            IF vTransaccion = "CS" THEN
                                vTransaccion = "CSR".
                        END.
                        ELSE
                            vOrigen = "T".
                    
                        documento_cliente = TRIM(ECG.documentoCliente). /* nit */

                        archivoAplicados.cedula = documentoCliente.

                        CASE INTEGER(ECG.causalTransaccion):  /* Causal de la transacción */
                            WHEN 3 THEN
                                IF vTransaccion = "RE" THEN
                                    vTransaccion = "RC".

                            WHEN 4 THEN
                                IF vTransaccion = "CE" THEN
                                    vTransaccion = "CC".
                        END CASE.

                        CASE ECG.terminalTipo:  /* Terminal - Tipo */
                            WHEN "04" OR
                            WHEN "41" THEN DO:
                                IF vTransaccion = "CR" THEN DO:
                                    vTransaccion = "CP".
                                    vOrigen = "T".
                                END.
                            END.

                            WHEN "22" THEN vOrigen = "2".
                    
                            WHEN "20" OR
                            WHEN "24" THEN vOrigen = "3".

                            WHEN "23" THEN vOrigen = "4".
                            WHEN "40" THEN vOrigen = "1".
                            OTHERWISE vOrigen = ECG.terminalTipo.
                        END CASE.

                        /* Definición de transacciones Intercooperativas */
                        /* CEC - CCC */
                        IF (vTransaccion = "CE" OR vTransaccion = "CC") AND INTEGER(ECG.entidadDuenaCuentaOrigen) <> 18 THEN DO: /* Entidad dueña de la cuenta destino */
                            CASE vTransaccion:
                                WHEN "CE" THEN vTransaccion = "CEC".
                                WHEN "CC" THEN vTransaccion = "CCC".
                            END CASE.
                        END.

                        /* REO - RCO */
                        IF (vTransaccion = "RE" OR vTransaccion = "RC") AND INTEGER(ECG.entidadDuenaCuentaOrigen) <> 18 THEN DO: /* Entidad dueña de la cuenta origen */
                            CASE vTransaccion:
                                WHEN "RE" THEN vTransaccion = "REO".
                                WHEN "RC" THEN vTransaccion = "RCO".
                            END CASE.
                        END.

                        /* CEO - CCO */
                        IF (vTransaccion = "CE" OR vTransaccion = "CC") AND ECG.entidadDuenaTerminal <> "00000018" THEN DO: /* Entidad dueña terminal */
                            CASE vTransaccion:
                                WHEN "CE" THEN vTransaccion = "CEO".
                                WHEN "CC" THEN vTransaccion = "CCO".
                            END CASE.
                        END.

                        /* REC - RCC */
                        IF (vTransaccion = "RE" OR vTransaccion = "RC") AND ECG.entidadDuenaTerminal <> "00000018" THEN DO: /* Entidad dueña terminal */
                            CASE vTransaccion:
                                WHEN "RE" THEN vTransaccion = "REC".
                                WHEN "RC" THEN vTransaccion = "RCC".
                            END CASE.
                        END.

                        /* ----- */
                        /* TFC */
                        IF (vTransaccion = "RE" OR vTransaccion = "RC") AND ECG.entidadDuenaTerminal <> "00000018" AND ECG.entidadDuenaTerminal <> "" THEN /* Entidad dueña terminal */
                            vTransaccion = "TFC".
                
                        /* TFO */
                        IF (vTransaccion = "CE" OR vTransaccion = "CC") AND ECG.entidadDuenaTerminal <> "00000018" AND ECG.entidadDuenaTerminal <> "" THEN /* Entidad dueña terminal */
                            vTransaccion = "TFO".
                
                        /* Compras */
                        IF (vTransaccion = "CP") AND ECG.entidadDuenaTerminal <> "00000018" THEN /* Entidad dueña terminal */
                           vTransaccion = "CPC".
            
                        IF (vTransaccion = "CP") AND ECG.entidadDuenaTerminal = "00000018" THEN DO:
                            IF ECG.entidadDuenaCuentaOrigen <> "00000018" THEN
                                vTransaccion = "CPO".
                        END.

                        ASSIGN vCuenta1_Origen = TRIM(ECG.cuentaOrigen)                          /* Cuenta_origen */
                               vCuenta2_destino = TRIM(ECG.cuentaDestino).                         /* Cuenta_destino */

                        IF vCuenta2_destino = "" THEN
                            vCuenta2_destino = vCuenta1_origen.

                        archivoAplicados.fecha = ecg.fecha.
                        archivoAplicados.terminalUbicacion = ecg.terminalUbicacion.
                        archivoAplicados.numeroTarjeta = ecg.numeroTarjeta.
                        archivoAplicados.fechaContable = ecg.fechaContable.

                        ASSIGN valor = DECIMAL(ECG.valor) / 100                     /* Valor */
                               valor_transaccion = DECIMAL(ECG.valorComision) / 100.        /* valor_cobro */

                        /* Validamos los códigos de error */
                        IF INTEGER(ECG.codigodeError) > 0 THEN
                            valor = 0.

                        /* Si la transacción es de una consulta de saldo telefónica - IVR, cargamos el valor de la comisión */
                        IF vTransaccion = "CS" AND vOrigen = "90" THEN
                            valor_transaccion = 464.

                        /* contabiliza. */
                        ASSIGN tottra = tottra + valor
                               totcom = totcom + valor_transaccion.

                        IF valor + valor_transaccion > 0 THEN DO:
                            CASE vTransaccion:
                                WHEN "CE" OR
                                WHEN "CC" OR
                                WHEN "CEO" OR
                                WHEN "CCO" OR
                                WHEN "TFO" THEN DO:
                                    IF vTransaccion = "TFO" THEN
                                        vCuenta1_origen = vCuenta2_destino.

                                    IF INTEGER(ecg.tipoCuentaOrigen) = 10 THEN
                                        RUN consignacionesAhorros.
                                    ELSE
                                        RUN ConsignacionesCupo.
                                END.

                                WHEN "RE" OR
                                WHEN "RC" THEN
                                    RUN CxC.

                                WHEN "RR" OR
                                WHEN "REC" OR
                                WHEN "RCC" OR
                                WHEN "CR" OR
                                WHEN "TFC" OR
                                WHEN "CS" OR
                                WHEN "ACR" OR
                                WHEN "CSR" THEN DO:
                                    IF INTEGER(ecg.tipoCuentaOrigen) = 10 THEN
                                        RUN retiros.
                                    ELSE
                                        RUN Avances.
                                END.

                                WHEN "TF" THEN DO:
                                    IF INTEGER(ecg.tipoCuentaOrigen) = 10 THEN
                                        RUN retiros.
                                    ELSE
                                        RUN Avances.

                                    vCuenta1_origen = vCuenta2_destino.

                                    IF INTEGER(ecg.tipoCuentaOrigen) = 10 THEN
                                        RUN consignacionesAhorros.
                                    ELSE
                                        RUN ConsignacionesCupo.
                                END.
                            END CASE.
                        END.
                    END.
                END.

                ASSIGN  w-Mensaje:SCREEN-VALUE IN FRAME default-frame = "Cargado Archivo día " + STRING(vdaFecMov,"99/99/9999")
                        w-TotTx:SCREEN-VALUE = STRING(tottra,">>>,>>>,>>9")
                        w-TotCom:SCREEN-VALUE = STRING(totcom,">>>,>>>,>>9").

                CREATE ControlMovTDB.
                UPDATE controlMovTDB.fecCarga = NOW
                       controlMovTDB.fecMov = vdaFecMov
                       controlMovTDB.numRegPro = INTEGER(w-totreg:SCREEN-VALUE)
                       controlMovTDB.usuario = W_usuario.

                /* Reporte de Inconsistencias */
                /*DEFINE VAR Listado AS CHAR FORM "X(40)".

                Listado = W_PathSpl + "VisioInconsistencias-" + STRING(W_Agencia) + STRING(W_Usuario) + ".Lst".
                {Incluido\Imprimir.I "listado"}*/
                /* ********** */

                /* Creamos un registro en la tabla de procesos */
                FIND FIRST procDia WHERE procDia.cod_proceso = 13
                                     AND procDia.agencia = w_agencia
                                     AND procDia.fecha_proc = vdaFecMov
                                     AND procDia.estado = 1 NO-ERROR.
                IF NOT AVAILABLE procDia THEN
                    CREATE procDia.

                ASSIGN procDia.agencia = w_agencia
                       procDia.cod_proceso = 13
                       procDia.fecha_proc = vdaFecMov
                       procDia.estado = 2
                       procDia.usuario = w_usuario.

                OUTPUT TO VALUE(vcRaiz + "ImporteTarjetas_" + vcFecMov + ".csv").
                EXPORT DELIMITER ";" "FECHA_MOVIMIENTO" "TIPO_TRANS" "SECUENCIA" "CÉDULA" "DESCRIPCIÓN_TERMINAL" "NUMERO_TARJETA" "FECHA_CONTABLE" "VALOR" "VALOR_COMISIÓN" "COMENTARIO".
                FOR EACH archivoAplicados NO-LOCK:
                    EXPORT DELIMITER ";" archivoAplicados.
                END.
                OUTPUT CLOSE.

                MESSAGE "Proceso de Importación terminado. En la carpeta" vcRaiz SKIP
                        "se encuentra el archivo" STRING("ImporteTarjetas_" + vcFecMov + ".csv.") SKIP
                        "Utilice este archivo para realizar la respectiva conciliación" SKIP
                        "contra el archivo .AUTCON"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END. /*ELSE DO: /*Archivo no ha sido cargado*/*/
        END. /*DO TRANSACTION ON ERROR UNDO tdbError:*/
    END. /*IF vcNomArchInput NE "" THEN DO:*/
    ELSE
        MESSAGE "Importación Cancelada"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnConciliar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnConciliar C-Win
ON CHOOSE OF btnConciliar IN FRAME DEFAULT-FRAME /* Conciliar */
DO:
    DEFINE VARIABLE vcRaiz AS CHARACTER INITIAL "c:\info_fodun\" NO-UNDO.
    DEFINE VARIABLE vcNomArchInput AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcFecMov AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vdaFecMov AS DATE NO-UNDO.
    DEFINE VAR flagExiste AS CHARACTER.
    DEFINE VAR cont AS INTEGER INITIAL 0.

    EMPTY TEMP-TABLE autcon.

    ASSIGN w-Mensaje:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
           w-totreg:SCREEN-VALUE = ""
           w-TotTx:SCREEN-VALUE  = ""
           w-TotCom:SCREEN-VALUE = "".

    /* Se selecciona el archivo a importar */
    SYSTEM-DIALOG GET-FILE vcNomArchInput
        TITLE "Selección Archivo Movimiento ..."
        FILTERS "Comprimidos (*.7z)" "*.7z", "Todos (*.*)" "*.*"
        MUST-EXIST
        USE-FILENAME
        UPDATE vlOkPressed.

    RUN Conciliar (INPUT vcNomArchInput).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
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
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

  RUN IniciaVar.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActualizarTablaTarjetas C-Win 
PROCEDURE ActualizarTablaTarjetas :
FOR EACH ecg WHERE ecg.fecha NE "00000018":
    FIND FIRST clientes WHERE clientes.nit = ecg.documentoCliente NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN DO:
        FIND FIRST creditos WHERE creditos.nit = clientes.nit
                              AND creditos.num_credito = DECIMAL(ECG.cuentaOrigen)
                              AND creditos.estado = 2 NO-ERROR.
        IF NOT AVAILABLE creditos THEN
            FIND FIRST creditos WHERE creditos.nit = clientes.nit
                              AND creditos.cod_credito = 123
                              AND creditos.estado = 2 NO-ERROR.

        IF AVAILABLE creditos THEN DO:
            FIND FIRST tarjetas WHERE tarjetas.nit = ECG.documentoCliente NO-ERROR.
            IF AVAILABLE tarjetas THEN DO:
                IF tarjetas.tarjetaDB <> ECG.numeroTarjeta THEN
                    ASSIGN tarjetas.estado = "62"
                           tarjetas.fec_bloqueo = w_fecha
                           tarjetas.hora_bloqueo = TIME.
                ELSE DO:
                    ASSIGN tarjetas.fec_ulttransac = w_fecha
                           tarjetas.hora_ulttrans = TIME.
                    NEXT.
                END.
            END.

            FIND FIRST cfg_tarjetaDB NO-LOCK NO-ERROR.

            CREATE tarjetas.
            ASSIGN tarjetas.nit = ecg.documentoCliente
                   tarjetas.tarjetaDB = ecg.numeroTarjeta
                   tarjetas.estado = "01"
                   tarjetas.fec_cargue = w_fecha
                   tarjetas.hora_cargue = TIME
                   tarjetas.fec_ulttransac = w_fecha
                   tarjetas.hora_ulttrans = TIME
                   tarjetas.agencia = clientes.agencia
                   tarjetas.usuario = w_usuario
                   tarjetas.num_credito = creditos.num_Credito.

            IF AVAILABLE cfg_tarjetaDB THEN DO:
                tarjetas.operMaxCaj = cfg_tarjetaDB.numeroRetirosCajeroDia.
                tarjetas.operMaxPOS = cfg_tarjetaDB.numeroRetirosPOSDia.
                tarjetas.montoMaxCaj = cfg_tarjetaDB.montoRetirosCajeroDia.
                tarjetas.montoMaxPOS = cfg_tarjetaDB.montoRetirosPOSDia.
            END.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AjustesCredito C-Win 
PROCEDURE AjustesCredito :
EMPTY TEMP-TABLE informe.

FOR EACH aju WHERE aju.fechaContable NE "00000018"
               AND aju.operacion = "2" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "57" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "57".
    END.

    informe.CxC = informe.CxC + aju.valor.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "57" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "57" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "57".
    END.

    informe.pdf = comaut.valorCobrar.
END.

FOR EACH informe NO-LOCK:
    IF informe.CxC <> 0 OR informe.pdf <> 0 THEN
        DISPLAY "57 - Ajuste Crédito...................................:"
                informe.CxC AT 72
                informe.pdf AT 89
                informe.CxC - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmAjusteCredito USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxC = totalCxC + informe.CxC.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AjustesDebito C-Win 
PROCEDURE AjustesDebito :
EMPTY TEMP-TABLE informe.

FOR EACH aju WHERE aju.fechaContable NE "00000018"
               AND aju.operacion = "1" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "07" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "07".
    END.

    informe.CxP = informe.CxP + aju.valor.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "07" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "07" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "07".
    END.

    informe.pdf = comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    IF informe.CxP <> 0 OR informe.pdf <> 0 THEN
        DISPLAY "07 - Ajuste Débito....................................:"
                informe.CxP AT 56
                informe.pdf AT 89
                informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmAjusteDebito USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avances C-Win 
PROCEDURE Avances :
DEFINE VARIABLE vdb AS DECIMAL.
DEFINE VARIABLE vcr AS DECIMAL.
DEFINE VAR codOperacion AS INTEGER.
DEFINE VAR tasaCredito AS DECIMAL.
DEFINE VAR VRev AS CHARACTER.

FIND FIRST creditos WHERE creditos.nit = documento_cliente
                      AND creditos.num_credito = DECIMAL(vCuenta1_origen)
                      AND creditos.estado = 2 NO-ERROR.
IF NOT AVAILABLE creditos THEN
    FIND FIRST creditos WHERE creditos.nit = documento_cliente
                          AND creditos.cod_credito = 123
                          AND creditos.estado = 2 NO-ERROR.

IF AVAILABLE(creditos) THEN DO:
    FIND FIRST comprobante WHERE comprobante.comprobante = 22
                             AND comprobantes.agencia = creditos.agencia NO-ERROR.
    IF AVAILABLE(comprobante) THEN
        ASSIGN Wdoc = comprobante.secuencia + 1
               comprobantes.secuencia = comprobante.secuencia + 1.

    RELEASE comprobante.

    archivoAplicados.valor = valor.
    archivoAplicados.valorComision = valor_transaccion.

    IF vGrupotransaccional = 0 THEN DO:
        ASSIGN vdb = valor + valor_transaccion
               vcr = 0
               codOperacion = 020102001.

        creditos.Sdo_capital = Creditos.Sdo_capital + (valor + valor_transaccion).

        /* Actualizamos la tasa */
        FIND FIRST pro_creditos WHERE pro_credito.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
        IF AVAILABLE(pro_creditos) THEN
            FIND FIRST indicadores WHERE Indicadores.indicador = pro_creditos.cod_tasa  NO-LOCK NO-ERROR.
        
        IF AVAILABLE(indicadores) THEN DO:
            tasaCredito = (((EXP((indicadores.tasa / 100) + 1,1 / 12)) - 1) * 100) * 12.

            IF STRING(tasaCredito,">>9.99") <> STRING(creditos.tasa,">>9.99") THEN DO:
                CREATE Mov_Creditos.
                ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
                       Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
                       Mov_Creditos.Nit = Creditos.Nit
                       Mov_Creditos.Num_Credito = Creditos.Num_Credito
                       Mov_Creditos.Cod_Operacion = 999999999
                       Mov_Creditos.Ofi_Destino = Creditos.Agencia
                       Mov_Creditos.Ofi_Fuente = W_Agencia
                       Mov_Creditos.Pagare = Creditos.Pagare
                       Mov_Creditos.Fecha = W_Fecha
                       Mov_Creditos.Hora = vTime
                       Mov_Creditos.Usuario = W_Usuario
                       Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
                       Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
                       Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
                       Mov_Creditos.Cpte = 22
                       Mov_Creditos.Descrip = "Cambio de Tasa " + STRING(creditos.tasa,">>9.99") + "-->" + STRING(tasaCredito,">>9.99").

                creditos.tasa = tasaCredito.
            END.
        END.
        ELSE
            MESSAGE "Se debe establecer la configuracion de Cupo Rotativo en Pro_Creditos para los cambios de tasa." SKIP
                    "Este mensaje es informativo. La aplicación de los movimientos se realiza de forma correcta."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        /* -------------------- */
    END.
    ELSE DO:
        IF vGrupotransaccional = 2 THEN DO:
            ASSIGN vcr = valor + valor_transaccion
                   vdb = 0
                   codOperacion = 020101001.

            creditos.Sdo_capital = Creditos.Sdo_capital - (valor + valor_transaccion).

            vRev = "Rev-".
        END.
    END.

    CREATE mov_contable.
    ASSIGN Mov_Contable.Agencia = Creditos.agencia
           mov_contable.cen_costos = 999
           Mov_Contable.Destino = Creditos.agencia
           Mov_Contable.Comprobante = 22
           Mov_Contable.Num_Documento = wdoc
           Mov_contable.Doc_referencia = vCuenta1_origen
           Mov_Contable.Fec_Contable = w_fecha
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = cuentaRotativo
           Mov_Contable.Comentario = "Av-" + vTransaccion + vSecuencia + ecg.fecha
           Mov_Contable.Usuario = W_usuario
           Mov_Contable.Estacion = "005"
           Mov_Contable.Nit = creditos.nit
           Mov_contable.db = vdb
           mov_contable.cr = vcr.

    CREATE mov_contable.
    ASSIGN Mov_Contable.Agencia = Creditos.agencia
           mov_contable.cen_costos = 999
           Mov_Contable.Destino = Creditos.agencia
           Mov_Contable.Comprobante = 22
           Mov_Contable.Num_Documento = wdoc
           Mov_contable.Doc_referencia = vCuenta1_origen
           Mov_Contable.Fec_Contable = w_fecha
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = "24459550"
           Mov_Contable.Comentario = "Avance " + vTransaccion + vSecuencia + ecg.fecha
           Mov_Contable.Usuario = W_usuario
           Mov_Contable.Estacion = "005"
           Mov_Contable.Nit = nitCompensacion /*creditos.nit*/
           Mov_contable.db = vcr
           mov_contable.cr = vdb.

    IF creditos.agencia <> 1 THEN DO:
        mov_contable.cuenta = cuentaSyA.
        mov_contable.nit = "001".

        FIND FIRST comprobante WHERE comprobante.comprobante = 22
                                 AND comprobantes.agencia = 1 NO-ERROR.
        IF AVAILABLE(comprobante) THEN
            ASSIGN Wdoc = comprobante.secuencia + 1
                   comprobantes.secuencia = comprobante.secuencia + 1.

        RELEASE comprobante.

        CREATE mov_contable.
        ASSIGN Mov_Contable.Agencia = 1
               mov_contable.cen_costos = 999
               Mov_Contable.Destino = 1
               Mov_Contable.Comprobante = 22
               Mov_Contable.Num_Documento = wdoc
               Mov_contable.Doc_referencia = vCuenta1_origen
               Mov_Contable.Fec_Contable = w_fecha
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Cuenta = cuentaSyA
               Mov_Contable.Comentario = vRev + "Avance " + vTransaccion + vSecuencia + ecg.fecha
               Mov_Contable.Usuario = W_usuario
               Mov_Contable.Estacion = "005"
               Mov_Contable.Nit = STRING(creditos.agencia,"999")
               Mov_contable.db = vdb
               mov_contable.cr = vcr.

        CREATE mov_contable.
        ASSIGN Mov_Contable.Agencia = 1
               mov_contable.cen_costos = 999
               Mov_Contable.Destino = 1
               Mov_Contable.Comprobante = 22
               Mov_Contable.Num_Documento = wdoc
               Mov_contable.Doc_referencia = vCuenta1_origen
               Mov_Contable.Fec_Contable = w_fecha
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Cuenta = "24459550"
               Mov_Contable.Comentario = vRev + "Avance " + vTransaccion + vSecuencia + ecg.fecha
               Mov_Contable.Usuario = W_usuario
               Mov_Contable.Estacion = "005"
               Mov_Contable.Nit = nitCompensacion
               Mov_contable.db = vcr
               mov_contable.cr = vdb.
    END.

    IF valor > 0 THEN DO:
        CREATE Mov_Creditos.
        ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
               Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
               Mov_Creditos.Nit = Creditos.Nit
               Mov_Creditos.Num_Credito = Creditos.Num_Credito
               Mov_Creditos.Cod_Operacion = codOperacion
               Mov_Creditos.Ofi_Destino = Creditos.Agencia
               Mov_Creditos.Ofi_Fuente = W_Agencia
               Mov_Creditos.Pagare = Creditos.Pagare
               Mov_Creditos.Fecha = W_Fecha
               Mov_Creditos.Hora = vTime
               Mov_Creditos.Num_Documento = STRING(wdoc)
               Mov_Creditos.Usuario = W_Usuario
               Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
               Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
               Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
               Mov_Creditos.Val_Efectivo = valor
               Mov_Creditos.Cpte = 22
               Mov_Creditos.Descrip = vRev + SUBSTRING(ecg.entidadDuenaTerminalDescripcion,1,29) + " - " + ecg.fecha.
    END.

    IF valor_transaccion <> 0 THEN DO:
        CREATE Mov_Creditos.
        ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
               Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
               Mov_Creditos.Nit = Creditos.Nit
               Mov_Creditos.Num_Credito = Creditos.Num_Credito
               Mov_Creditos.Cod_Operacion = codOperacion
               Mov_Creditos.Ofi_Destino = Creditos.Agencia
               Mov_Creditos.Ofi_Fuente = W_Agencia
               Mov_Creditos.Pagare = Creditos.Pagare
               Mov_Creditos.Fecha = W_Fecha
               Mov_Creditos.Hora = vTime
               Mov_Creditos.Num_Documento = STRING(wdoc)
               Mov_Creditos.Usuario = W_Usuario
               Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
               Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
               Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
               Mov_Creditos.Val_Efectivo = valor_transaccion
               Mov_Creditos.Cpte = 22
               Mov_Creditos.Descrip = vRev + SUBSTRING(ecg.entidadDuenaTerminalDescripcion,1,18) + " - Comisión - " + ecg.fecha.
    END.

    CONTABILIZADO = YES.
END.
ELSE DO:
    CONTABILIZADO = NO. 

    archivoAplicados.comentario = "No se encontró el Crédito - transacción No Aplicada".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Conciliar C-Win 
PROCEDURE Conciliar :
DEFINE INPUT PARAMETER vcNomArchInput AS CHARACTER NO-UNDO.

DEFINE VARIABLE vcRaiz AS CHARACTER INITIAL "c:\info_fodun\" NO-UNDO.
DEFINE VARIABLE vcFecMov AS CHARACTER NO-UNDO.
DEFINE VARIABLE vdaFecMov AS DATE NO-UNDO.
DEFINE VAR flagExiste AS CHARACTER.
DEFINE VAR cont AS INTEGER INITIAL 0.

EMPTY TEMP-TABLE autcon.
EMPTY TEMP-TABLE comaut.
EMPTY TEMP-TABLE aju.
EMPTY TEMP-TABLE ttFor.
EMPTY TEMP-TABLE dis.
EMPTY TEMP-TABLE cms.

ASSIGN w-Mensaje:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
       w-totreg:SCREEN-VALUE = ""
       w-TotTx:SCREEN-VALUE  = ""
       w-TotCom:SCREEN-VALUE = "".

/* Como está comprimido en 7z, procedemos a descomprimir */
ASSIGN vcComando = "C:\7-Zip\7z".
OS-COMMAND SILENT VALUE(vcComando) x VALUE(vcNomArchInput) -oc:\info_FODUN\Visionamos -y -pD05207C1.

vcRaiz = vcRaiz + "visionamos\".
vcFecMov = SUBSTRING(vcNomArchInput,(LENGTH(vcNomArchInput) - 19),8).
vcNomArchInput = SUBSTRING(vcNomArchInput,(LENGTH(vcNomArchInput) - 19),8) + ".COMAUT".
vcNomArchInput = vcRaiz + vcNomArchInput.

ASSIGN vdaFecMov = intToDate(vcFecMov)
       w-Mensaje:SCREEN-VALUE = "Cargando Archivo " + vcNomArchInput.

IF vcNomArchInput NE "" THEN DO:
    /* 1. Subimos el archivo COMAUT */
    INPUT FROM VALUE(vcNomArchInput).
    REPEAT:
        CREATE comaut.
        IMPORT DELIMITER "," comaut.

        comaut.valorPagar = comaut.valorPagar / 100.
        comaut.valorCobrar = comaut.valorCobrar / 100.

        IF comaut.fechaContable NE "00000018" THEN
            totreg = totreg + 1.
        ELSE
            DELETE comaut.
    END.
    INPUT CLOSE.

    vcNomArchInput = SUBSTRING(vcNomArchInput,1,(LENGTH(vcNomArchInput) - 7)) + ".AUTCON".

    /* 2. Subimos el archivo AUTCON */
    INPUT FROM VALUE(vcNomArchInput).
    REPEAT:
        cont = cont + 1.

        CREATE autcon.
        IMPORT DELIMITER "," autcon NO-ERROR.

        autcon.valor = autcon.valor / 100 NO-ERROR.

        IF autcon.fechaTransaccion NE "00000018" THEN
            totreg = totreg + 1.
        ELSE
            DELETE autcon.
    END.
    INPUT CLOSE.

    vcNomArchInput = SUBSTRING(vcNomArchInput,1,(LENGTH(vcNomArchInput) - 7)) + ".AJU".

    flagExiste = SEARCH(vcNomArchInput).

    IF flagExiste <> ? THEN DO:
        /* 3. Subimos el archivo AJU */
        INPUT FROM VALUE(vcNomArchInput).
        REPEAT:
            CREATE aju.
            IMPORT DELIMITER "," aju.

            aju.valor = aju.valor / 100.

            IF aju.fechaContable NE "00000018" THEN
                totreg = totreg + 1.
            ELSE
                DELETE aju.
        END.
        INPUT CLOSE.
    END.

    /* 4. Subimos el archivo FOR */
    vcNomArchInput = SUBSTRING(vcNomArchInput,1,(LENGTH(vcNomArchInput) - 4)) + ".FOR".

    flagExiste = SEARCH(vcNomArchInput).

    IF flagExiste <> ? THEN DO:
        INPUT FROM VALUE(vcNomArchInput).
        REPEAT:
            CREATE ttFOR.
            IMPORT DELIMITER "," ttFOR.

            IF ttFOR.fechaContable NE "00000018" THEN
                totreg = totreg + 1.
            ELSE
                DELETE ttFOR.
        END.
        INPUT CLOSE.
    END.

    /* 5. Subimos el archivo DIS */
    vcNomArchInput = SUBSTRING(vcNomArchInput,1,(LENGTH(vcNomArchInput) - 4)) + ".DIS".

    flagExiste = SEARCH(vcNomArchInput).

    IF flagExiste <> ? THEN DO:
        INPUT FROM VALUE(vcNomArchInput).
        REPEAT:
            CREATE dis.
            IMPORT DELIMITER "," dis.

            IF dis.registro NE "00000018" THEN
                totreg = totreg + 1.
            ELSE
                DELETE dis.
        END.
        INPUT CLOSE.
    END.

    /* 6. Subimos el archivo ABO */
    vcNomArchInput = SUBSTRING(vcNomArchInput,1,(LENGTH(vcNomArchInput) - 4)) + ".ABO".

    flagExiste = SEARCH(vcNomArchInput).

    IF flagExiste <> ? THEN DO:
        INPUT FROM VALUE(vcNomArchInput).
        REPEAT:
            CREATE abo.
            IMPORT DELIMITER "," abo.

            abo.valorAbono = abo.valorAbono / 100.

            IF abo.fechaContable NE "00000018" THEN
                totreg = totreg + 1.
            ELSE
                DELETE abo.
        END.
        INPUT CLOSE.
    END.

    /* 7. Subimos el archivo CMS */
    vcNomArchInput = SUBSTRING(vcNomArchInput,1,(LENGTH(vcNomArchInput) - 4)) + ".CMS".

    flagExiste = SEARCH(vcNomArchInput).

    IF flagExiste <> ? THEN DO:
        INPUT FROM VALUE(vcNomArchInput).
        REPEAT:
            CREATE cms.
            IMPORT DELIMITER "," cms.

            IF cms.registro NE "00000018" THEN
                totreg = totreg + 1.
            ELSE DELETE cms.
        END.
        INPUT CLOSE.
    END.

    ASSIGN w-totreg:SCREEN-VALUE IN FRAME default-frame = STRING(totreg,">>>,>>>,>>9")
           totreg = 0.

    DEFINE VAR Listado AS CHAR FORM "X(40)".

    Listado = W_PathSpl + "ConciliacionVisionamos_" + STRING(DAY(w_fecha),"99") + STRING(MONTH(w_fecha),"99") + STRING(YEAR(w_fecha),"9999") + ".Lst".
    {Incluido\Imprimir.I "listado"}
END.
ELSE
    MESSAGE "Conciliación cancelada"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConsignacionesAhorros C-Win 
PROCEDURE ConsignacionesAhorros :
DEFINE VAR vdb AS DECIMAL.
DEFINE VAR vcr AS DECIMAL.
DEFINE VAR valEfectivo AS DECIMAL.
DEFINE VAR valCheque AS DECIMAL.
DEFINE VAR pResult AS LOGICAL.
DEFINE VAR vAgenciaDestino AS INTEGER.
DEFINE VAR vCuentaDestino AS CHARACTER.
DEFINE VAR vNitDestino AS CHARACTER.

FIND FIRST ahorros WHERE ahorros.nit = documento_cliente
                     AND INTEGER(ahorros.cue_ahorros) = INTEGER(vCuenta1_origen) NO-LOCK NO-ERROR.
IF AVAILABLE(ahorros) THEN DO:
    FIND FIRST comprobante WHERE comprobante.comprobante = 22 NO-ERROR.
    IF AVAILABLE(comprobante) THEN DO:
        Wdoc = comprobante.secuencia + 1.
        comprobantes.secuencia = comprobante.secuencia + 1.
    END.

    RELEASE comprobante.

    IF vGrupotransaccional = 0 THEN DO:
        vcr = valor - valor_transaccion.
        vdb = 0.

        IF INTEGER(ecg.causalTransaccion) = 4 THEN
            valCheque = vCr.
        ELSE
            valEfectivo = vCr.

        IF ahorros.tip_ahorro = 1 THEN DO:
            RUN p-consignacionAhorro_aLaVista.r (INPUT ahorros.agencia,
                                                 INPUT ahorros.cod_ahorro,
                                                 INPUT ahorros.nit,
                                                 INPUT ahorros.nit,
                                                 INPUT ahorros.cue_ahorros,
                                                 INPUT valEfectivo,
                                                 INPUT valCheque,
                                                 INPUT 22,
                                                 INPUT WDoc,
                                                 INPUT ecg.terminalUbicacion,
                                                 INPUT ecg.secuencia,
                                                 INPUT ecg.chequeNumero,
                                                 INPUT ecg.chequeCodigoDeBanco,
                                                 INPUT 'Red Coopcentral',
                                                 INPUT ecg.usuarioAutorizador,
                                                 INPUT FALSE,
                                                 OUTPUT pResult) NO-ERROR.
            IF pResult = FALSE THEN
                RETURN ERROR.
        END.
    END.
    ELSE DO:
        IF vGrupotransaccional = 2 THEN DO:
            vdb = valor - valor_transaccion.
            vcr = 0.

            RUN p-consignacionAhorro_aLaVista-Rev.r (INPUT ahorros.agencia,
                                                     INPUT ahorros.cod_ahorro,
                                                     INPUT ahorros.nit,
                                                     INPUT ahorros.nit,
                                                     INPUT ahorros.cue_ahorros,
                                                     INPUT valEfectivo,
                                                     INPUT valCheque,
                                                     INPUT 22,
                                                     INPUT WDoc,
                                                     INPUT ecg.terminalUbicacion,
                                                     INPUT ecg.secuencia,
                                                     INPUT ecg.chequeNumero,
                                                     INPUT ecg.chequeCodigoDeBanco,
                                                     INPUT 'Red Coopcentral',
                                                     INPUT ecg.usuarioAutorizador,
                                                     INPUT FALSE,
                                                     OUTPUT pResult) NO-ERROR.

            IF pResult = FALSE THEN
                RETURN ERROR.
        END.
    END.

    CREATE mov_contable.
    mov_contable.cen_costos = 999.
    Mov_Contable.Comprobante = comprobantes.comprobante.
    Mov_Contable.Num_Documento = wDoc.
    Mov_contable.Doc_referencia = vCuenta1_origen.
    Mov_Contable.Fec_Contable = TODAY.
    Mov_Contable.Fec_Grabacion = TODAY.
    Mov_Contable.Comentario = ecg.terminalUbicacion.
    Mov_Contable.Usuario = w_usuario.
    Mov_Contable.Estacion = "005".
    mov_contable.destino = ahorros.agencia.
    Mov_contable.db = vcr.
    mov_contable.cr = vdb.

    IF vTransaccion = "CE" OR vTransaccion = "CC" THEN DO:
        mov_contable.nit = ahorros.nit.
        Mov_Contable.Cuenta = "16603550".

        FIND FIRST usuarios WHERE usuarios.usuario = ecg.usuarioAutorizador NO-LOCK NO-ERROR.
        IF AVAILABLE usuarios THEN
            mov_contable.agencia = usuarios.agencia.
        ELSE
            mov_contable.agencia = w_agencia.
    END.
    ELSE DO:
        Mov_Contable.Cuenta = "24459550".
        Mov_Contable.Nit = nitCompensacion.
        mov_contable.agencia = 1.
    END.

    /* Para Sucursales y Agencias */
    IF mov_contable.agencia <> ahorros.agencia THEN DO:
        RUN cuentaSucursales&agencias.

        vAgenciaDestino = mov_contable.agencia.
        mov_contable.agencia = ahorros.agencia.
        vCuentaDestino = mov_contable.cuenta.
        mov_contable.cuenta = cuentaSyA.
        vNitDestino = mov_contable.nit.
        mov_contable.nit = STRING(vAgenciaDestino,"999").

        FIND FIRST comprobante WHERE comprobante.comprobante = 22
                                 AND comprobantes.agencia = vAgenciaDestino NO-ERROR.
        IF AVAILABLE(comprobante) THEN
            ASSIGN wDoc = comprobante.secuencia + 1
                   comprobantes.secuencia = comprobante.secuencia + 1.

        CREATE mov_contable.
        ASSIGN mov_contable.cen_costos = 999
               Mov_Contable.Comprobante = comprobantes.comprobante
               Mov_Contable.Num_Documento = wDoc
               Mov_contable.Doc_referencia = vCuenta1_origen
               Mov_Contable.Fec_Contable = w_fecha
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Comentario = ecg.terminalUbicacion
               Mov_Contable.Usuario = w_usuario
               Mov_Contable.Estacion = "005"
               Mov_Contable.Agencia = vAgenciaDestino
               Mov_Contable.Destino = ahorros.agencia
               Mov_Contable.Cuenta = cuentaSyA
               Mov_Contable.Nit = STRING(ahorros.agencia,"999")
               Mov_contable.db = vdb
               mov_contable.cr = vcr.

        CREATE mov_contable.
        ASSIGN mov_contable.cen_costos = 999
               Mov_Contable.Comprobante = comprobantes.comprobante
               Mov_Contable.Num_Documento = wDoc
               Mov_contable.Doc_referencia = vCuenta1_origen
               Mov_Contable.Fec_Contable = w_fecha
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Comentario = ecg.terminalUbicacion
               Mov_Contable.Usuario = w_usuario
               Mov_Contable.Estacion = "005"
               Mov_Contable.Agencia = vAgenciaDestino
               Mov_Contable.Destino = ahorros.agencia
               Mov_Contable.Cuenta = vCuentaDestino
               Mov_Contable.Nit = nitCompensacion
               Mov_contable.db = vcr
               mov_contable.cr = vdb.

        CONTABILIZADO = YES.
    END.
END.
ELSE
    CONTABILIZADO = NO. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConsignacionesCupo C-Win 
PROCEDURE ConsignacionesCupo :
DEFINE VARIABLE vdb AS DECIMAL.
DEFINE VARIABLE vcr AS DECIMAL.
DEFINE VAR P_Poliza AS DECIMAL.
DEFINE VAR P_Honora AS DECIMAL.
DEFINE VAR P_Costas AS DECIMAL.
DEFINE VAR P_SeguroVida AS DECIMAL.
DEFINE VAR P_SeguroDeudor AS DECIMAL.
DEFINE VAR P_IMorDifC AS DECIMAL.
DEFINE VAR P_IMora AS DECIMAL.
DEFINE VAR P_IDifCob AS DECIMAL.
DEFINE VAR P_ICte AS DECIMAL.
DEFINE VAR P_IAntic AS DECIMAL.
DEFINE VAR P_Capit AS DECIMAL.
DEFINE VAR P_VlrNoDist AS DECIMAL.
DEFINE VAR P_IAC AS DECIMAL.
DEFINE VAR pError AS LOGICAL.

FIND FIRST creditos WHERE creditos.nit = documento_cliente
                      AND creditos.num_credito = DECIMAL(vCuenta1_origen) NO-ERROR.
IF AVAILABLE(creditos) THEN DO:
    FIND FIRST comprobante WHERE comprobante.comprobante = 22 NO-ERROR.
    IF AVAILABLE(comprobante) THEN
        ASSIGN Wdoc = comprobante.secuencia + 1
               comprobantes.secuencia = comprobante.secuencia + 1.

    RELEASE comprobante.

    IF vGrupotransaccional = 0 THEN DO:
        ASSIGN vcr = valor - valor_transaccion
               vdb = 0.

        RUN p-pagoCredito.r(INPUT YES,
                            INPUT Creditos.Cod_Credito,
                            INPUT Creditos.Nit,
                            INPUT Creditos.Num_Credito,
                            INPUT valor - valor_transaccion,
                            INPUT Comprobantes.Comprobante,
                            INPUT Comprobantes.Secuencia,
                            INPUT 0,
                            INPUT 1,
                            INPUT w_fecha,
                            INPUT NO,
                            OUTPUT P_Poliza,
                            OUTPUT P_Honora,
                            OUTPUT P_Costas,
                            OUTPUT P_SeguroVida,
                            OUTPUT P_SeguroDeudor,
                            OUTPUT P_IMorDifC,
                            OUTPUT P_IMora,
                            OUTPUT P_IDifCob,
                            OUTPUT P_ICte,
                            OUTPUT P_IAntic,
                            OUTPUT P_Capit,
                            OUTPUT P_VlrNoDist,
                            OUTPUT pError).
    END.
    ELSE DO:
        IF vGrupotransaccional = 2 THEN DO:
            ASSIGN vdb = valor - valor_transaccion
                   vcr = 0.

            creditos.Sdo_capital = Creditos.Sdo_capital + (valor - valor_transaccion).

            CREATE mov_contable.
            ASSIGN Mov_Contable.Agencia = Creditos.agencia
                   mov_contable.cen_costos = 999
                   Mov_Contable.Destino = Creditos.agencia
                   Mov_Contable.Comprobante = 22
                   Mov_Contable.Num_Documento = wdoc
                   Mov_contable.Doc_referencia = vCuenta1_origen
                   Mov_Contable.Fec_Contable = w_fecha
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Cuenta = cuentaRotativo
                   Mov_Contable.Comentario = vTransaccion + vSecuencia + ecg.fecha
                   Mov_Contable.Usuario = W_usuario
                   Mov_Contable.Estacion = "005"
                   Mov_Contable.Nit = creditos.nit
                   Mov_contable.db = vdb
                   mov_contable.cr = vcr.

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
                   Mov_Creditos.Num_Documento = STRING(wdoc)
                   Mov_Creditos.Usuario = W_Usuario
                   Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
                   Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
                   Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
                   Mov_Creditos.Val_Efectivo = valor - valor_transaccion
                   Mov_Creditos.Cpte = 22
                   /*Mov_Creditos.Descrip = "Pago " + vTransaccion + vSecuencia + ecg.fecha.*/
                   Mov_Creditos.Descrip = "Rev" + "-" + ecg.entidadDuenaTerminalDescripcion + "-" + ecg.fecha.
        END.
    END.

    CREATE mov_contable.
    ASSIGN Mov_Contable.Agencia = Creditos.agencia
           mov_contable.cen_costos = 999
           Mov_Contable.Destino = Creditos.agencia
           Mov_Contable.Comprobante = 22
           Mov_Contable.Num_Documento = wdoc
           Mov_contable.Doc_referencia = vCuenta1_origen
           Mov_Contable.Fec_Contable = w_fecha
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = "24459550"
           Mov_Contable.Comentario = vTransaccion + vSecuencia + ecg.fecha
           Mov_Contable.Usuario = W_usuario
           Mov_Contable.Estacion = "005"
           Mov_Contable.Nit = nitCompensacion
           Mov_contable.db = vcr
           mov_contable.cr = vdb.

    IF creditos.agencia <> 1 THEN DO:
        mov_contable.cuenta = cuentaSyA.
        mov_contable.nit = "001".

        FIND FIRST comprobante WHERE comprobante.comprobante = 22
                                 AND comprobantes.agencia = 1 NO-ERROR.
        IF AVAILABLE(comprobante) THEN
            ASSIGN Wdoc = comprobante.secuencia + 1
                   comprobantes.secuencia = comprobante.secuencia + 1.

        RELEASE comprobante.

        CREATE mov_contable.
        ASSIGN Mov_Contable.Agencia = 1
               mov_contable.cen_costos = 999
               Mov_Contable.Destino = 1
               Mov_Contable.Comprobante = 22
               Mov_Contable.Num_Documento = wdoc
               Mov_contable.Doc_referencia = vCuenta1_origen
               Mov_Contable.Fec_Contable = w_fecha
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Cuenta = cuentaSyA
               Mov_Contable.Comentario = "Pago/Rev " + vTransaccion + vSecuencia + ecg.fecha
               Mov_Contable.Usuario = W_usuario
               Mov_Contable.Estacion = "005"
               Mov_Contable.Nit = STRING(creditos.agencia,"999")
               Mov_contable.db = vdb
               mov_contable.cr = vcr.

        CREATE mov_contable.
        ASSIGN Mov_Contable.Agencia = 1
               mov_contable.cen_costos = 999
               Mov_Contable.Destino = 1
               Mov_Contable.Comprobante = 22
               Mov_Contable.Num_Documento = wdoc
               Mov_contable.Doc_referencia = vCuenta1_origen
               Mov_Contable.Fec_Contable = w_fecha
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Cuenta = "24459550"
               Mov_Contable.Comentario = "Pago/Rev " + vTransaccion + vSecuencia + ecg.fecha
               Mov_Contable.Usuario = W_usuario
               Mov_Contable.Estacion = "005"
               Mov_Contable.Nit = nitCompensacion
               Mov_contable.db = vcr
               mov_contable.cr = vdb.
    END.

    CONTABILIZADO = YES.
END.
ELSE
    CONTABILIZADO = NO. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxC C-Win 
PROCEDURE CxC :
DEFINE VARIABLE vdb AS DECIMAL.
DEFINE VARIABLE vcr AS DECIMAL.
DEFINE VAR codOperacion AS INTEGER.
DEFINE VAR tasaCredito AS DECIMAL.

FIND FIRST creditos WHERE creditos.nit = documento_cliente
                      AND creditos.num_credito = DECIMAL(vCuenta1_origen)
                      AND creditos.estado = 2 NO-ERROR.
IF NOT AVAILABLE creditos THEN
    FIND FIRST creditos WHERE creditos.nit = documento_cliente
                          AND creditos.cod_credito = 123
                          AND creditos.estado = 2 NO-ERROR.

IF AVAILABLE(creditos) THEN DO:
    FIND FIRST comprobante WHERE comprobante.comprobante = 22
                             AND comprobantes.agencia = creditos.agencia NO-ERROR.
    IF AVAILABLE(comprobante) THEN
        ASSIGN Wdoc = comprobante.secuencia + 1
               comprobantes.secuencia = comprobante.secuencia + 1.

    RELEASE comprobante.

    IF vGrupotransaccional = 0 THEN DO:
        ASSIGN vdb = valor + valor_transaccion
               vcr = 0
               codOperacion = 020102001.

        creditos.Sdo_capital = Creditos.Sdo_capital + (valor + valor_transaccion).

        /* Actualizamos la tasa */
        FIND FIRST pro_creditos WHERE pro_credito.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
        IF AVAILABLE(pro_creditos) THEN
            FIND FIRST indicadores WHERE Indicadores.indicador = pro_creditos.cod_tasa  NO-LOCK NO-ERROR.
        
        IF AVAILABLE(indicadores) THEN DO:
            tasaCredito = (((EXP((indicadores.tasa / 100) + 1,1 / 12)) - 1) * 100) * 12.

            IF STRING(tasaCredito,">>9.99") <> STRING(creditos.tasa,">>9.99") THEN DO:
                CREATE Mov_Creditos.
                ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
                       Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
                       Mov_Creditos.Nit = Creditos.Nit
                       Mov_Creditos.Num_Credito = Creditos.Num_Credito
                       Mov_Creditos.Cod_Operacion = 999999999
                       Mov_Creditos.Ofi_Destino = Creditos.Agencia
                       Mov_Creditos.Ofi_Fuente = W_Agencia
                       Mov_Creditos.Pagare = Creditos.Pagare
                       Mov_Creditos.Fecha = W_Fecha
                       Mov_Creditos.Hora = vTime
                       Mov_Creditos.Usuario = W_Usuario
                       Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
                       Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
                       Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
                       Mov_Creditos.Cpte = 22
                       Mov_Creditos.Descrip = "Cambio de Tasa " + STRING(creditos.tasa,">>9.99") + "-->" + STRING(tasaCredito,">>9.99").

                creditos.tasa = tasaCredito.
            END.
        END.
        ELSE
            MESSAGE "Se debe establecer la configuracion de Cupo Rotativo en Pro_Creditos para los cambios de tasa." SKIP
                    "Este mensaje es informativo. La aplicación de los movimientos se realiza de forma correcta."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.
    ELSE DO:
        IF vGrupotransaccional = 2 THEN DO:
            ASSIGN vcr = valor + valor_transaccion
                   vdb = 0
                   codOperacion = 020101001.

            creditos.Sdo_capital = Creditos.Sdo_capital - (valor + valor_transaccion).
        END.
    END.

    CREATE mov_contable.
    ASSIGN Mov_Contable.Agencia = Creditos.agencia
           mov_contable.cen_costos = 999
           Mov_Contable.Destino = Creditos.agencia
           Mov_Contable.Comprobante = 22
           Mov_Contable.Num_Documento = wdoc
           Mov_contable.Doc_referencia = vCuenta1_origen
           Mov_Contable.Fec_Contable = w_fecha
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = cuentaRotativo
           Mov_Contable.Comentario = "Av-" + vTransaccion + vSecuencia + ecg.fecha
           Mov_Contable.Usuario = W_usuario
           Mov_Contable.Estacion = "005"
           Mov_Contable.Nit = creditos.nit
           Mov_contable.db = vdb
           mov_contable.cr = vcr.

    CREATE mov_contable.
    ASSIGN Mov_Contable.Agencia = Creditos.agencia
           mov_contable.cen_costos = 999
           Mov_Contable.Destino = Creditos.agencia
           Mov_Contable.Comprobante = 22
           Mov_Contable.Num_Documento = wdoc
           Mov_contable.Doc_referencia = vCuenta1_origen
           Mov_Contable.Fec_Contable = w_fecha
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = "16603550"
           Mov_Contable.Comentario = "Avance " + vTransaccion + vSecuencia + ecg.fecha
           Mov_Contable.Usuario = W_usuario
           Mov_Contable.Estacion = "005"
           Mov_Contable.Nit = creditos.nit
           Mov_contable.db = vcr
           mov_contable.cr = vdb.

    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia = Creditos.Agencia
           Mov_Creditos.Cod_Credito = Creditos.Cod_Credito
           Mov_Creditos.Nit = Creditos.Nit
           Mov_Creditos.Num_Credito = Creditos.Num_Credito
           Mov_Creditos.Cod_Operacion = codOperacion
           Mov_Creditos.Ofi_Destino = Creditos.Agencia
           Mov_Creditos.Ofi_Fuente = W_Agencia
           Mov_Creditos.Pagare = Creditos.Pagare
           Mov_Creditos.Fecha = W_Fecha
           Mov_Creditos.Hora = vTime
           Mov_Creditos.Num_Documento = STRING(wdoc)
           Mov_Creditos.Usuario = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
           Mov_Creditos.Val_Efectivo = valor + valor_transaccion
           Mov_Creditos.Cpte = 22
           /*Mov_Creditos.Descrip = "Av-" + vTransaccion + vSecuencia + ecg.fecha.*/
           Mov_Creditos.Descrip = ecg.entidadDuenaTerminalDescripcion + "-" + ecg.fecha.

    CONTABILIZADO = YES.
END.
ELSE
    CONTABILIZADO = NO. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxC_Abonos C-Win 
PROCEDURE CxC_Abonos :
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

FOR EACH abo NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "54" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "54".
    END.

    informe.CxC = informe.CxC + abo.valorAbono.
END.

FOR EACH comaut WHERE comaut.tipoRegistro = "54" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "54" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "54".
    END.

    informe.pdf = informe.pdf + comaut.valorCobrar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "54 - CxC Abonos (Servibanca)..........................:"
            informe.CxC AT 72
            informe.pdf AT 89
            informe.CxC - Informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxC USE-TEXT NO-LABELS STREAM-IO NO-BOX.
    
    totalCxC = totalCxC + informe.CxC.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxC_ComisionesCMC C-Win 
PROCEDURE CxC_ComisionesCMC :
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.terminalTipo = "23"
                  AND autcon.entidadDuenaCuentaTerminal = "00000018"
                  AND autcon.comisionIntercooperativa > 0 NO-LOCK BREAK BY autcon.entidadDuenacuentaOrigen:
    FIND FIRST informe WHERE informe.concepto = "51"
                         AND informe.entidad = autcon.entidadDuenacuentaOrigen NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "51".
        informe.entidad = autcon.entidadDuenacuentaOrigen.
    END.

    informe.CxC = informe.CxC + autCon.comisionIntercooperativa.
END.

FOR EACH comaut WHERE comaut.tipoRegistro = "51" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "51"
                         AND informe.entidad = comAut.codigoCooperativaOrigen NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "51".
        informe.entidad = comAut.codigoCooperativaOrigen.
    END.

    informe.pdf = informe.pdf + comaut.valorCobrar.
END.

FOR EACH informe NO-LOCK:
    RUN Entidades(INPUT informe.entidad,
                  OUTPUT nombreEntidad).

    DISPLAY "51 - CxC Comisiones CMC (" nombreEntidad ")..............:"
            informe.CxC AT 72
            informe.pdf AT 89
            informe.CxC - Informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxC USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxC = totalCxC + informe.CxC.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxC_ComisionesFinancierasRedeban C-Win 
PROCEDURE CxC_ComisionesFinancierasRedeban :
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

FOR EACH cms NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "55" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "55".
    END.

    informe.CxC = informe.CxC + cms.valorComision.
END.

FOR EACH comaut WHERE comaut.tipoRegistro = "55" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "55" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "55".
    END.

    informe.pdf = informe.pdf + comaut.valorCobrar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "55 - CxC Comisiones Financieras Redeban (Servibanca)..:"
            informe.CxC AT 72
            informe.pdf AT 89
            informe.CxC - Informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxC USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxC = totalCxC + informe.CxC.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxC_ComisionesIntercooperativas C-Win 
PROCEDURE CxC_ComisionesIntercooperativas :
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

FOR EACH comaut WHERE comaut.tipoRegistro = "59" NO-LOCK BREAK BY comaut.codigoCooperativaOrigen:
    IF FIRST-OF(comaut.codigoCooperativaOrigen) THEN DO:
        FIND FIRST informe WHERE informe.concepto = "59"
                             AND informe.entidad = comaut.codigoCooperativaOrigen NO-LOCK NO-ERROR.
        IF NOT AVAILABLE informe THEN DO:
            CREATE informe.
            informe.concepto = "59".
            informe.entidad = comaut.codigoCooperativaOrigen.
        END.

        informe.pdf = informe.pdf + comaut.valorCobrar.
    END.
END.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "06"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.codigoError = "0000"
                  AND autcon.EntidadDuenaCuentaTerminal = "00000018"
                  AND autcon.comisionIntercooperativa > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "59"
                         AND informe.entidad = autcon.entidadDuenaCuentaOrigen NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "59".
        informe.entidad = autcon.entidadDuenaCuentaOrigen.
    END.

    informe.CxC = informe.CxC + autCon.comisionIntercooperativa.
END.

FOR EACH informe NO-LOCK:
    RUN Entidades(INPUT informe.entidad,
                  OUTPUT nombreEntidad).

    DISPLAY "59 - CxC Comisiones Intercooperativas (" nombreEntidad "):"
            informe.CxC AT 72
            informe.pdf AT 89
            informe.CxC - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxC USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxC = totalCxC + informe.CxC.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxC_DispensadosCMC C-Win 
PROCEDURE CxC_DispensadosCMC :
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

FOR EACH dis WHERE dis.valor > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "53" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "53".
    END.

    informe.CxC = informe.CxC + dis.valor.
END.

FOR EACH comaut WHERE comaut.tipoRegistro = "53"
                  AND comaut.valorCobrar > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "53" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "53".
    END.

    informe.pdf = informe.pdf + comaut.valorCobrar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "53 - CxC Dispensados CMC (Servibanca).................:"
            informe.CxC AT 72
            informe.pdf AT 89
            informe.CxC - Informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxC USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxC = totalCxC + informe.CxC.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxC_ForaneasInternacionalesCMC C-Win 
PROCEDURE CxC_ForaneasInternacionalesCMC :
EMPTY TEMP-TABLE informe.

FOR EACH ttFOR WHERE ttFOR.fechaContable NE "00000018" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "62" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "62".
    END.

    informe.CxC = informe.CxC + DECIMAL(ttFOR.NumOperacionesForaneasInter) * 1758.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "62" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "62" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "62".
    END.

    informe.pdf = comaut.valorCobrar.
END.

FOR EACH informe NO-LOCK:
    IF informe.CxC <> 0 OR informe.pdf <> 0 THEN
        DISPLAY "62 - CxC Comisiones Foráneas Internac CMC (Servibanca):"
                informe.CxC AT 72
                informe.pdf AT 89
                informe.CxC - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmForaneasInter USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxC = totalCxC + informe.CxC.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxC_ForaneasNacionalesCMC C-Win 
PROCEDURE CxC_ForaneasNacionalesCMC :
DEFINE VAR suma AS INTEGER.

EMPTY TEMP-TABLE informe.

FOR EACH ttFOR WHERE ttFOR.fechaContable NE "00000018" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "56" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "56".
    END.

    suma = suma + INTEGER(NumOperacionesForaneasNales) + INTEGER(NumOperacionesFamiliasEnAccion).

    informe.CxC = informe.CxC + INTEGER(ttFOR.NumOperacionesForaneasNales) * (2550 - 792).
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "56" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "56" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "56".
    END.

    informe.pdf = comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    IF informe.CxC <> 0 OR informe.pdf <> 0 THEN
        DISPLAY "56 - CxP Foráneas Nacionales CMC (Servibanca).........:"
                informe.CxC AT 72
                informe.pdf AT 89
                informe.CxC - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmForaneasNales USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxC = totalCxC + informe.CxC.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxC_Intercooperativas C-Win 
PROCEDURE CxC_Intercooperativas :
DEFINE VAR CxP AS DECIMAL FORMAT "$->>>,>>>,>>9.99".
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

FOR EACH comaut WHERE comaut.tipoRegistro = "50" NO-LOCK BREAK BY comaut.codigoCooperativaOrigen:
    IF FIRST-OF(comaut.codigoCooperativaOrigen) THEN DO:
        FIND FIRST informe WHERE informe.concepto = "50"
                             AND informe.entidad = comaut.codigoCooperativaOrigen NO-LOCK NO-ERROR.
        IF NOT AVAILABLE informe THEN DO:
            CREATE informe.
            informe.concepto = "50".
            informe.entidad = comaut.codigoCooperativaOrigen.
        END.

        informe.pdf = informe.pdf + comaut.valorCobrar.
    END.
END.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "06"
                  /*AND autcon.grupoTransaccional = "0200"*/
                  AND autcon.codigoError = "0000"
                  AND autcon.entidadAcreedora = "00000018"
                  AND autcon.valor > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "50"
                         AND informe.entidad = autcon.entidadDeudora NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "50".
        informe.entidad = autcon.entidadDeudora.
    END.

    IF autcon.grupoTransaccional = "0200" THEN
        informe.CxC = informe.CxC + autCon.valor.

    IF autcon.grupoTransaccional = "0400" THEN
        informe.CxC = informe.CxC - autCon.valor.
END.

FOR EACH informe NO-LOCK:
    RUN Entidades(INPUT informe.entidad,
                  OUTPUT nombreEntidad).

    DISPLAY "50 - CxC Intercooperativas (" nombreEntidad ")...........:"
            informe.CxC AT 72
            informe.pdf AT 89
            informe.CxC - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxC USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxC = totalCxC + informe.CxC.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesATM_OTR C-Win 
PROCEDURE CxP_ComisionesATM_OTR :
EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.terminalTipo = "24"
                  AND autcon.comisionRedes > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "18" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "18".
    END.

    informe.CxP = informe.CxP + autCon.comisionredes.
END.

FOR EACH comaut WHERE comaut.tipoRegistro = "18" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "18" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "18".
    END.

    informe.pdf = informe.pdf + comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "18 - CxP Comisiones ATM OTR (Servibanca)..............:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - Informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesATM_OUT C-Win 
PROCEDURE CxP_ComisionesATM_OUT :
EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.terminalTipo = "25"
                  AND autcon.comisionRedes > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "19" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "19".
    END.

    informe.CxP = informe.CxP + autCon.comisionredes.
END.

FOR EACH comaut WHERE comaut.tipoRegistro = "19" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "19" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "19".
    END.

    informe.pdf = informe.pdf + comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "15 - CxP Comisiones ATM OUT (Servibanca)..............:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - Informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesATM_RED C-Win 
PROCEDURE CxP_ComisionesATM_RED :
EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.terminalTipo = "20"
                  AND autcon.comisionRedes > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "15" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "15".
    END.

    informe.CxP = informe.CxP + autCon.comisionredes.
END.

FOR EACH comaut WHERE comaut.tipoRegistro = "15" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "15" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "15".
    END.

    informe.pdf = informe.pdf + comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "15 - CxP Comisiones ATM RED (Servibanca)..............:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - Informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.
    
    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesATM_SVB C-Win 
PROCEDURE CxP_ComisionesATM_SVB :
EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.terminalTipo = "22"
                  AND autcon.comisionRedes > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "16" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "16".
    END.

    informe.CxP = informe.CxP + autCon.comisionredes.
END.

FOR EACH comaut WHERE comaut.tipoRegistro = "16" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "16" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "16".
    END.

    informe.pdf = informe.pdf + comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "16 - CxP Comisiones ATM SVB (Servibanca)..............:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - Informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesATM_VIS C-Win 
PROCEDURE CxP_ComisionesATM_VIS :
EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.terminalTipo = "23"
                  AND autcon.entidadDuenaCuentaOrigen = "00000018"
                  AND autcon.comisionRedes > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "17" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "17".
    END.

    informe.CxP = informe.CxP + autCon.comisionredes.
END.

FOR EACH comaut WHERE comaut.tipoRegistro = "17" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "17" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "17".
    END.

    informe.pdf = informe.pdf + comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "17 - CxP Comisiones ATM VIS (Servibanca)..............:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - Informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesCMC C-Win 
PROCEDURE CxP_ComisionesCMC :
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.terminalTipo = "23"
                  AND autcon.entidadDuenacuentaOrigen = "00000018"
                  AND autcon.comisionIntercooperativa > 0 NO-LOCK BREAK BY autcon.entidadDuenaCuentaTerminal:
    FIND FIRST informe WHERE informe.concepto = "01"
                         AND informe.entidad = autcon.entidadDuenaCuentaTerminal NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "01".
        informe.entidad = autcon.entidadDuenaCuentaTerminal.
    END.

    informe.CxP = informe.CxP + autCon.comisionIntercooperativa.
END.

FOR EACH comaut WHERE comaut.tipoRegistro = "01" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "01"
                         AND informe.entidad = comAut.codigoCooperativaOrigen NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "01".
        informe.entidad = comAut.codigoCooperativaOrigen.
    END.

    informe.pdf = informe.pdf + comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    RUN Entidades(INPUT informe.entidad,
                  OUTPUT nombreEntidad).

    DISPLAY "01 - CxP Comisiones CMC (" nombreEntidad ")..............:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - Informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesForaneasInter C-Win 
PROCEDURE CxP_ComisionesForaneasInter :
EMPTY TEMP-TABLE informe.

FOR EACH ttFOR WHERE ttFOR.fechaContable NE "00000018" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "13" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "13".
    END.

    informe.CxP = informe.CxP + DECIMAL(ttFOR.NumOperacionesForaneasInter) * 2550.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "13" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "13" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "13".
    END.

    informe.pdf = comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    IF informe.CxP <> 0 OR informe.pdf <> 0 THEN
        DISPLAY "13 - CxP Comisiones Foráneas Internac CMC (Visionamos):"
                informe.CxP AT 56
                informe.pdf AT 89
                informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmForaneasNales USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesForaneasNales C-Win 
PROCEDURE CxP_ComisionesForaneasNales :
EMPTY TEMP-TABLE informe.

FOR EACH ttFOR WHERE ttFOR.fechaContable NE "00000018" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "11" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "11".
    END.

    informe.CxP = informe.CxP + DECIMAL(ttFOR.NumOperacionesForaneasNales) * 2550.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "11" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "11" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "11".
    END.

    informe.pdf = comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    IF informe.CxP <> 0 OR informe.pdf <> 0 THEN
        DISPLAY "13 - CxP Comisiones Foráneas Nacional CMC (Visionamos):"
                informe.CxP AT 56
                informe.pdf AT 89
                informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmForaneasNales USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesInterCooperativas C-Win 
PROCEDURE CxP_ComisionesInterCooperativas :
DEFINE VAR CxP AS DECIMAL FORMAT "$->>>,>>>,>>9.99".
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "06"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.EntidadDuenaCuentaOrigen = "00000018"
                  AND autcon.EntidadDuenaCuentaTerminal <> "00000018"
                  AND autcon.codigoError = "0000"
                  AND autcon.comisionVisionamos > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "08" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "08".
    END.

    informe.CxP = informe.CxP + autCon.comisionVisionamos.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "08" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "08" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "08".
    END.

    informe.pdf = comAut.valorPagar.
END.

FOR EACH informe WHERE informe.CxP <> 0 OR informe.pdf <> 0 NO-LOCK:
    DISPLAY "08 - CxP Intercooperativas (Visionamos)...............:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesIntercooperativasEntidad C-Win 
PROCEDURE CxP_ComisionesIntercooperativasEntidad :
DEFINE VAR CxP AS DECIMAL FORMAT "$->>>,>>>,>>9.99".
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

FOR EACH comaut WHERE comaut.tipoRegistro = "09" NO-LOCK BREAK BY comaut.codigoCooperativaOrigen:
    IF FIRST-OF(comaut.codigoCooperativaOrigen) THEN DO:
        FIND FIRST informe WHERE informe.concepto = "09"
                             AND informe.entidad = comaut.codigoCooperativaOrigen NO-LOCK NO-ERROR.
        IF NOT AVAILABLE informe THEN DO:
            CREATE informe.
            informe.concepto = "09".
            informe.entidad = comaut.codigoCooperativaOrigen.
        END.

        informe.pdf = informe.pdf + comaut.valorPagar.
    END.
END.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "06"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.codigoError = "0000"
                  AND autcon.EntidadDuenaCuentaOrigen = "00000018"
                  AND autcon.comisionIntercooperativa > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "09"
                         AND informe.entidad = autcon.EntidadDuenaCuentaTerminal NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "09".
        informe.entidad = autcon.EntidadDuenaCuentaTerminal.
    END.

    informe.CxP = informe.CxP + autCon.comisionIntercooperativa.
END.

FOR EACH informe NO-LOCK:
    RUN Entidades(INPUT informe.entidad,
                  OUTPUT nombreEntidad).

    DISPLAY "09 - CxP Comisiones Intercooperativas (" nombreEntidad "):"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesIVR C-Win 
PROCEDURE CxP_ComisionesIVR :
EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "06"
                  AND autcon.grupoTransaccional = "I000"
                  AND autcon.comisionVisionamos > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "10" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "10".
    END.

    informe.CxP = informe.CxP + autCon.comisionVisionamos.
END.
    
FIND FIRST comaut WHERE comaut.tipoRegistro = "10" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "10" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "10".
    END.

    informe.pdf = comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "01 - CxP Comisiones CMC (Visionamos)..................:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesPOS_Redeban C-Win 
PROCEDURE CxP_ComisionesPOS_Redeban :
DEFINE VAR CxP AS DECIMAL FORMAT "$->>>,>>>,>>9.99".
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.terminalTipo = "40"
                  AND autcon.adquiriente = "011017"
                  AND autcon.comisionRedes > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "04" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "04".
    END.

    IF autcon.grupoTransaccional = "0200" THEN
        informe.CxP = informe.CxP + autCon.comisionRedes.

    IF autcon.grupoTransaccional = "0400" THEN
        informe.CxP = informe.CxP - autCon.comisionRedes.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "04" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "04" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "04".
    END.

    informe.pdf = comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "04 - CxP Comisiones POS Redeban (Servibanca)..........:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_ComisionesPOS_Visa C-Win 
PROCEDURE CxP_ComisionesPOS_Visa :
EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.terminalTipo = "40"
                  AND autcon.adquiriente = "010921"
                  AND autcon.comisionRedes > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "14" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "14".
    END.

    IF autcon.grupoTransaccional = "0200" THEN
        informe.CxP = informe.CxP + autCon.comisionRedes.

    IF autcon.grupoTransaccional = "0400" THEN
        informe.CxP = informe.CxP - autCon.comisionRedes.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "14" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "14" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "14".
    END.

    informe.pdf = comaut.valorPagar.
END.


FOR EACH informe NO-LOCK:
    DISPLAY "14 - CxP Comisiones POS Visa (Servibanca).............:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_EnrutamientoATM-RED C-Win 
PROCEDURE CxP_EnrutamientoATM-RED :
EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.transaccion <> "89"
                  AND autcon.terminalTipo = "20"
                  AND autcon.enrutamientoRedes > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "05" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "05".
    END.

    informe.CxP = informe.CxP + autCon.enrutamientoRedes.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "05" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "05" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "05".
    END.

    informe.pdf = comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "05 - CxP Enrutamiento ATM-RED (Servibanca)............:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_EnrutamientoATM_OUT C-Win 
PROCEDURE CxP_EnrutamientoATM_OUT :
EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.transaccion <> "89"
                  AND autcon.terminalTipo = "25"
                  AND autcon.enrutamientoRedes > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "21" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "21".
    END.

    informe.CxP = informe.CxP + autCon.enrutamientoRedes.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "21" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "21" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "21".
    END.

    informe.pdf = comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "21 - CxP Enrutamiento ATM-OUT (Servibanca)............:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_EnrutamientoCCT C-Win 
PROCEDURE CxP_EnrutamientoCCT :
EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.transaccion = "89"
                  AND (autcon.terminalTipo = "20" OR
                       autcon.terminalTipo = "25" OR
                       autcon.terminalTipo = "40")
                  AND autcon.enrutamientoRedes > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "22" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "22".
    END.

    informe.CxP = informe.CxP + autCon.enrutamientoRedes.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "22" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "22" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "22".
    END.

    informe.pdf = comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "22 - CxP Enrutamiento CCT (Servibanca)................:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_EnrutamientoPOS C-Win 
PROCEDURE CxP_EnrutamientoPOS :
EMPTY TEMP-TABLE informe.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.transaccion <> "89"
                  AND autcon.terminalTipo = "40"
                  AND autcon.enrutamientoRedes > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "23" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "23".
    END.

    informe.CxP = informe.CxP + autCon.enrutamientoRedes.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "23" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "23" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "23".
    END.

    informe.pdf = comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    DISPLAY "23 - CxP Enrutamiento POS (Servibanca)................:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_FamiliasEnAccion C-Win 
PROCEDURE CxP_FamiliasEnAccion :
EMPTY TEMP-TABLE informe.

FOR EACH ttFOR WHERE ttFOR.fechaContable NE "00000018" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "20" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "20".
    END.

    informe.CxP = informe.CxP + DECIMAL(ttFOR.NumOperacionesFamiliasEnAccion) * 662.
END.

FIND FIRST comaut WHERE comaut.tipoRegistro = "20" NO-LOCK NO-ERROR.
IF AVAILABLE comaut THEN DO:
    FIND FIRST informe WHERE informe.concepto = "20" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "20".
    END.

    informe.pdf = comaut.valorPagar.
END.

FOR EACH informe NO-LOCK:
    IF informe.CxP <> 0 OR informe.pdf <> 0 THEN
        DISPLAY "20 - CxP Familias en acción (Servibanca)..............:"
                informe.CxP AT 56
                informe.pdf AT 89
                informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmForaneasNales USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_Intercooperativas C-Win 
PROCEDURE CxP_Intercooperativas :
DEFINE VAR CxP AS DECIMAL FORMAT "$->>>,>>>,>>9.99".
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

FOR EACH comaut WHERE comaut.tipoRegistro = "00" NO-LOCK BREAK BY comaut.codigoCooperativaOrigen:
    IF FIRST-OF(comaut.codigoCooperativaOrigen) THEN DO:
        FIND FIRST informe WHERE informe.concepto = "00"
                             AND informe.entidad = comaut.codigoCooperativaOrigen NO-LOCK NO-ERROR.
        IF NOT AVAILABLE informe THEN DO:
            CREATE informe.
            informe.concepto = "00".
            informe.entidad = comaut.codigoCooperativaOrigen.
        END.

        informe.pdf = informe.pdf + comaut.valorPagar.
    END.
END.

FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "06"
                  /*AND autcon.grupoTransaccional = "0200"*/
                  AND autcon.codigoError = "0000"
                  AND autcon.entidadDeudora = "00000018"
                  AND autcon.valor > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "00"
                         AND informe.entidad = autcon.entidadAcreedora NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "00".
        informe.entidad = autcon.entidadAcreedora.
    END.

    IF autcon.grupoTransaccional = "0200" THEN
        informe.CxC = informe.CxC + autCon.valor.

    IF autcon.grupoTransaccional = "0400" THEN
        informe.CxC = informe.CxC - autCon.valor.
END.

FOR EACH informe NO-LOCK:
    RUN Entidades(INPUT informe.entidad,
                  OUTPUT nombreEntidad).

    DISPLAY "00 - CxP Intercooperativas (" nombreEntidad ")...........:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_Movimientos C-Win 
PROCEDURE CxP_Movimientos :
DEFINE VAR CxP AS DECIMAL FORMAT "$->>>,>>>,>>9.99".
DEFINE VAR nombreEntidad AS CHARACTER FORMAT "X(12)".

EMPTY TEMP-TABLE informe.

/* Transacciones exitosas */
FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.transaccion <> "89"
                  AND autcon.transaccion <> "90"
                  AND autcon.codigoError = "0000"
                  AND autcon.entidadDuenacuentaOrigen = "00000018"
                  AND autcon.valor > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "03" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "03".
    END.

    informe.CxP = informe.CxP + autCon.valor.
END.

/* Transacciones reversadas */
FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0400"
                  AND autcon.codigoError = "0000"
                  AND autcon.entidadDuenacuentaOrigen = "00000018"
                  AND autcon.valor > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "03" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "03".
    END.

    informe.CxP = informe.CxP - autCon.valor.
END.

/* Transacciones de compra anuladas */
FOR EACH autcon WHERE autcon.fechaTransaccion NE "00000018"
                  AND autcon.origen = "03"
                  AND autcon.grupoTransaccional = "0200"
                  AND autcon.transaccion = "20"
                  AND autcon.entidadDuenacuentaOrigen = "00000018"
                  AND autcon.valor > 0 NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "03" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "03".
    END.

    informe.CxP = informe.CxP - autCon.valor.
END.

FOR EACH comaut WHERE comaut.tipoRegistro = "03" NO-LOCK:
    FIND FIRST informe WHERE informe.concepto = "03" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE informe THEN DO:
        CREATE informe.
        informe.concepto = "03".
    END.

    informe.pdf = informe.pdf + comaut.valorPagar.
END.


FOR EACH informe NO-LOCK:
    DISPLAY "03 - CxP Movimientos (Servibanca).....................:"
            informe.CxP AT 56
            informe.pdf AT 89
            informe.CxP - Informe.pdf FORMAT "$->>,>>>,>>9.99" AT 105
        WITH DOWN WIDTH 180 FRAME frmCxP USE-TEXT NO-LABELS STREAM-IO NO-BOX.

    totalCxP = totalCxP + informe.CxP.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CxP_SolicitudesOTP C-Win 
PROCEDURE CxP_SolicitudesOTP :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  DISPLAY w-totreg W-TotTx W-TotCom w-Mensaje 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-346 RECT-345 btnConciliar B-Importar BtnCancel Btn_Done 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Entidades C-Win 
PROCEDURE Entidades :
DEFINE INPUT PARAMETER codigoEntidad AS CHARACTER.
DEFINE OUTPUT PARAMETER nombreEntidad AS CHARACTER.

CASE codigoEntidad:
    WHEN "00000001" THEN nombreEntidad = "CFA".
    WHEN "00000002" THEN nombreEntidad = "COOPETRABAN".
    WHEN "00000003" THEN nombreEntidad = "CUB".
    WHEN "00000004" THEN nombreEntidad = "CREARCOOP".
    WHEN "00000005" THEN nombreEntidad = "JURISCOOP".
    WHEN "00000006" THEN nombreEntidad = "FONEDH".
    WHEN "00000008" THEN nombreEntidad = "AYCCOLANTA".
    WHEN "00000009" THEN nombreEntidad = "COOPANTEX".
    WHEN "00000010" THEN nombreEntidad = "COOFISAN".
    WHEN "00000011" THEN nombreEntidad = "COOPECREDITO".
    WHEN "00000013" THEN nombreEntidad = "CREDIFUTURO".
    WHEN "00000014" THEN nombreEntidad = "CORPECOL".
    WHEN "00000015" THEN nombreEntidad = "COOMUCA".
    WHEN "00000016" THEN nombreEntidad = "FOMARNORT".
    WHEN "00000017" THEN nombreEntidad = "FOTRANORTE".
    WHEN "00000018" THEN nombreEntidad = "FODUN".
    WHEN "00000019" THEN nombreEntidad = "CAJA UNION".
    WHEN "00000020" THEN nombreEntidad = "COFACENEIVA".
    WHEN "00000021" THEN nombreEntidad = "COOGRANADA".
    WHEN "00000022" THEN nombreEntidad = "COOPCENTRAL".
    WHEN "00000023" THEN nombreEntidad = "COOPEAIPE".
    WHEN "N_SERVIB" THEN nombreEntidad = "SERVIBANCA".
    WHEN "N_VISION" THEN nombreEntidad = "VISIONAMOS".
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IniciaVar C-Win 
PROCEDURE IniciaVar :
ASSIGN w-Mensaje:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
       w-totreg:SCREEN-VALUE = ""
       w-TotTx:SCREEN-VALUE  = ""
       w-TotCom:SCREEN-VALUE = "".

FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = 123 NO-LOCK NO-ERROR.
IF AVAILABLE pro_creditos THEN DO:
    FIND FIRST cortoLargo WHERE cortoLargo.agencia = w_agencia
                            AND cortoLargo.clase = 2
                            AND cortoLargo.cod_producto = pro_creditos.cod_credito NO-LOCK NO-ERROR.
    IF AVAILABLE cortoLargo THEN DO:
        cuentaRotativo = cortoLargo.Cta_AsoAd.

        FIND FIRST liqui_int WHERE liqui_int.clase = 2
                               AND liqui_int.cod_producto = 123 NO-LOCK NO-ERROR.
        IF AVAILABLE liqui_int THEN
            cuentaSyA = liqui_Int.Cta_SucYAge.
    END.
    ELSE DO:
        MESSAGE "No existe la Cuenta Contable configurada para el producto" SKIP
                "Crédito Rotativo"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        B-Importar:SENSITIVE = FALSE.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.I}

W_Reporte = "REPORTE DE CONCILIACIÓN ARCHIVO VISIONAMOS - Fecha del Informe :" + STRING(W_Fecha,"99/99/9999") + "- Hora :" + STRING(TIME,"HH:MM:SS").
 
VIEW FRAME F-Encabezado.
VIEW FRAME f-ftr.

DISPLAY "                CONCEPTO                                            CxP          CxC                 PDF      DIFERENCIA" SKIP
        "------------------------------------------------------------------------------------------------------------------------"
    WITH DOWN WIDTH 180 FRAME frmEncabezados USE-TEXT NO-LABELS STREAM-IO NO-BOX.

totalCxC = 0.
totalCxP = 0.

RUN CxP_Intercooperativas.
RUN CxP_ComisionesCMC.
RUN CxP_Movimientos.
RUN CxP_ComisionesPOS_Redeban.
RUN CxP_EnrutamientoATM-RED.
RUN ajustesDebito.
RUN CxP_ComisionesInterCooperativas.
RUN CxP_ComisionesIntercooperativasEntidad.
RUN CxP_ComisionesIVR.
RUN CxP_ComisionesForaneasNales.
RUN CxP_ComisionesForaneasInter.
RUN CxP_ComisionesPOS_Visa.
RUN CxP_ComisionesATM_RED.
RUN CxP_ComisionesATM_SVB.
RUN CxP_ComisionesATM_VIS.
RUN CxP_ComisionesATM_OTR.
RUN CxP_ComisionesATM_OUT.
RUN CxP_FamiliasEnAccion.
RUN CxP_EnrutamientoATM_OUT.
RUN CxP_EnrutamientoCCT.
RUN CxP_EnrutamientoPOS.
RUN CxP_SolicitudesOTP.
RUN CxC_Intercooperativas.
RUN CxC_ComisionesCMC.
RUN CxC_DispensadosCMC.
RUN CxC_Abonos.
RUN CxC_ComisionesFinancierasRedeban.
RUN CxC_ForaneasNacionalesCMC.
RUN AjustesCredito.
RUN CxC_ComisionesIntercooperativas.
RUN CxC_ForaneasInternacionalesCMC.

DISPLAY "------------------------------------------------------------------------------------------------------------------------" SKIP
        "TOTALES:"
        totalCxP AT 56 FORMAT "$->>>,>>>,>>9.99"
        totalCxC AT 72 FORMAT "$->>>,>>>,>>9.99"
        totalCxP - totalCxC FORMAT "$->>,>>>,>>9.99" AT 105
    WITH DOWN WIDTH 180 FRAME frmTotales USE-TEXT NO-LABELS STREAM-IO NO-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Retiros C-Win 
PROCEDURE Retiros :
DEFINE VARIABLE vdb AS DECIMAL.
DEFINE VARIABLE vcr AS DECIMAL.
DEFINE VAR codOperacion AS INTEGER.
DEFINE VAR tasaCredito AS DECIMAL.
DEFINE VAR VRev AS CHARACTER.
DEFINE VAR gmfAplicado AS DECIMAL.

FIND FIRST ahorros WHERE ahorros.nit = documento_cliente
                     AND ahorros.cod_ahorro = 4
                     AND INTEGER(ahorros.cue_ahorros) = INTEGER(vCuenta1_origen) NO-ERROR.
IF AVAILABLE(ahorros) THEN DO:
    FIND FIRST comprobante WHERE comprobante.comprobante = 22 NO-ERROR.
    IF AVAILABLE(comprobante) THEN
        ASSIGN wdoc = comprobante.secuencia + 1
               comprobantes.secuencia = comprobante.secuencia + 1.

    archivoAplicados.valor = valor.
    archivoAplicados.valorComision = valor_transaccion.

    IF vGrupotransaccional = 0 THEN DO:
        ASSIGN vdb = valor + valor_transaccion
               vcr = 0
               codOperacion = 010102001.

        ahorros.sdo_disponible = ahorros.sdo_disponible - vdb.

        RUN RutGMF.R (INPUT TRUE,
                      INPUT W_Agencia,
                      INPUT Ahorros.Agencia,
                      INPUT 1,
                      INPUT Ahorros.Cod_Ahorro,
                      INPUT Ahorros.Nit,
                      INPUT Ahorros.Cue_Ahorros,
                      INPUT codOperacion,
                      INPUT vdb,
                      INPUT comprobantes.comprobante,
                      INPUT STRING(wdoc),
                      INPUT "RetiroTarjeta",
                      INPUT 0,
                      INPUT 0,
                      OUTPUT gmfAplicado) NO-ERROR.
    END.
    ELSE DO:
        IF vGrupotransaccional = 2 THEN DO:
            ASSIGN vcr = valor + valor_transaccion
                   vdb = 0
                   codOperacion = 020101001.

            ahorros.sdo_disponible = ahorros.sdo_disponible + vcr.
        END.
    END.

    CREATE mov_contable.
    ASSIGN Mov_Contable.Agencia = ahorros.agencia
           mov_contable.cen_costos = 999
           Mov_Contable.Destino = ahorro.agencia
           Mov_Contable.Comprobante = 22
           Mov_Contable.Num_Documento = wdoc
           Mov_contable.Doc_referencia = vCuenta1_origen
           Mov_Contable.Fec_Contable = w_fecha
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = "21050501"
           Mov_Contable.Comentario = ecg.entidadDuenaTerminalDescripcion
           Mov_Contable.Usuario = W_usuario
           Mov_Contable.Estacion = "005"
           Mov_Contable.Nit = ahorros.nit
           Mov_contable.db = vdb
           mov_contable.cr = vcr.

    CREATE mov_contable.
    ASSIGN Mov_Contable.Agencia = ahorros.agencia
           mov_contable.cen_costos = 999
           Mov_Contable.Destino = ahorros.agencia
           Mov_Contable.Comprobante = 22
           Mov_Contable.Num_Documento = wdoc
           Mov_contable.Doc_referencia = vCuenta1_origen
           Mov_Contable.Fec_Contable = w_fecha
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Cuenta = "24459550"
           Mov_Contable.Comentario = ecg.entidadDuenaTerminalDescripcion
           Mov_Contable.Usuario = W_usuario
           Mov_Contable.Estacion = "005"
           Mov_Contable.Nit = nitCompensacion
           Mov_contable.db = vcr
           mov_contable.cr = vdb.

    IF creditos.agencia <> 1 THEN DO:
        mov_contable.cuenta = cuentaSyA.
        mov_contable.nit = "001".

        FIND FIRST comprobante WHERE comprobante.comprobante = 22
                                 AND comprobantes.agencia = 1 NO-ERROR.
        IF AVAILABLE(comprobante) THEN
            ASSIGN Wdoc = comprobante.secuencia + 1
                   comprobantes.secuencia = comprobante.secuencia + 1.

        RELEASE comprobante.

        CREATE mov_contable.
        ASSIGN Mov_Contable.Agencia = 1
               mov_contable.cen_costos = 999
               Mov_Contable.Destino = 1
               Mov_Contable.Comprobante = 22
               Mov_Contable.Num_Documento = wdoc
               Mov_contable.Doc_referencia = vCuenta1_origen
               Mov_Contable.Fec_Contable = w_fecha
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Cuenta = cuentaSyA
               Mov_Contable.Comentario = ecg.entidadDuenaTerminalDescripcion
               Mov_Contable.Usuario = W_usuario
               Mov_Contable.Estacion = "005"
               Mov_Contable.Nit = STRING(ahorros.agencia,"999")
               Mov_contable.db = vdb
               mov_contable.cr = vcr.

        CREATE mov_contable.
        ASSIGN Mov_Contable.Agencia = 1
               mov_contable.cen_costos = 999
               Mov_Contable.Destino = 1
               Mov_Contable.Comprobante = 22
               Mov_Contable.Num_Documento = wdoc
               Mov_contable.Doc_referencia = vCuenta1_origen
               Mov_Contable.Fec_Contable = w_fecha
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Cuenta = "24459550"
               Mov_Contable.Comentario = ecg.entidadDuenaTerminalDescripcion
               Mov_Contable.Usuario = W_usuario
               Mov_Contable.Estacion = "005"
               Mov_Contable.Nit = nitCompensacion
               Mov_contable.db = vcr
               mov_contable.cr = vdb.
    END.

    IF valor > 0 THEN DO:
        CREATE mov_ahorro.
        ASSIGN mov_ahorros.Agencia = ahorros.agencia
               mov_ahorros.Cod_Ahorro = ahorros.cod_ahorro
               mov_ahorros.cpte = 22
               mov_ahorros.Cue_Ahorros = ahorros.cue_ahorros
               mov_ahorros.descrip = ecg.entidadDuenaTerminalDescripcion
               mov_ahorros.Fecha = TODAY
               mov_ahorros.Hora = TIME
               mov_ahorros.nit = ahorros.nit
               mov_ahorros.Nro_Auditoria = ecg.entidadDuenaTerminalDescripcion
               mov_ahorros.Num_Documento = STRING(wdoc)
               mov_ahorros.Usuario = w_usuario
               mov_ahorros.val_efectivo = valor
               mov_ahorros.cod_operacion = 010102001
               mov_ahorros.sdo_disponible = ahorros.sdo_disponible.
    END.

    IF valor_transaccion <> 0 THEN DO:
        CREATE mov_ahorro.
        ASSIGN mov_ahorros.Agencia = ahorros.agencia
               mov_ahorros.Cod_Ahorro = ahorros.cod_ahorro
               mov_ahorros.cpte = 22
               mov_ahorros.Cue_Ahorros = ahorros.cue_ahorros
               mov_ahorros.descrip = ecg.entidadDuenaTerminalDescripcion
               mov_ahorros.Fecha = TODAY
               mov_ahorros.Hora = TIME
               mov_ahorros.nit = ahorros.nit
               mov_ahorros.Nro_Auditoria = ecg.entidadDuenaTerminalDescripcion
               mov_ahorros.Num_Documento = STRING(wdoc)
               mov_ahorros.Usuario = w_usuario
               mov_ahorros.val_efectivo = valor_transaccion
               mov_ahorros.cod_operacion = 010102001
               mov_ahorros.sdo_disponible = ahorros.sdo_disponible.
    END.

    CONTABILIZADO = YES.
END.
ELSE DO:
    CONTABILIZADO = NO. 

    archivoAplicados.comentario = "No se encontró el Crédito - transacción No Aplicada".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION intToDate C-Win 
FUNCTION intToDate RETURNS DATE
  ( INPUT ipcFecha  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  20110525
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE vdaFecha AS DATE        NO-UNDO.

    ASSIGN vdaFecha = DATE(INTEGER(SUBSTRING(ipcFecha,5,2)),
                           INTEGER(SUBSTRING(ipcFecha,7,2)),
                           INTEGER(SUBSTRING(ipcFecha,1,4))).

    RETURN vdaFecha.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

