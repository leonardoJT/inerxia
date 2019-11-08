&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{Incluido\variable.i "shared"}

DEFINE VARIABLE vlOkPressed AS LOGICAL NO-UNDO.
DEFINE VARIABLE vcComando AS CHARACTER NO-UNDO.
DEFINE VARIABLE grupoTransaccional AS INTEGER.
DEFINE VARIABLE transaccion AS CHARACTER.

/* oakley */

DEFINE variable  secuencia AS CHARACTER.
DEFINE variable  origen AS CHARACTER.
DEFINE variable  documento_cliente AS CHARACTER.
DEFINE variable  documento_cliente_destino AS CHARACTER.
DEFINE variable  Cuenta1_origen AS CHARACTER.
DEFINE variable  cuenta2_destino AS CHARACTER.
DEFINE variable  entidad_duena_cuenta_origen AS INTEGER.
DEFINE variable  entidad_duena_cuenta_destino AS INTEGER.
DEFINE variable  entidad_duena_terminal AS CHARACTER.
DEFINE variable  cheque_codigo_banco AS INTEGER.
DEFINE variable  cheque_cuenta_banco AS CHARACTER.
DEFINE variable  cheque_numero AS CHARACTER.
DEFINE VARIABLE  wdoc LIKE comprobantes.comprobante.

DEFINE variable  cheque_total_cheques AS INTEGER.
DEFINE variable  codigo_barras AS CHARACTER.
DEFINE variable  numero_tarjeta AS CHARACTER.
DEFINE variable  fecha_contable AS INTEGER.
DEFINE variable  valor AS DECIMAL.
DEFINE variable  valor_base AS DECIMAL.
DEFINE variable  valor_impuesto AS DECIMAL.
DEFINE variable  valor_retencion AS DECIMAL.
DEFINE variable  valor_propina AS DECIMAL.
DEFINE variable  valor_transaccion AS DECIMAL.
DEFINE variable  cuotas AS INTEGER.
DEFINE variable  numero_factura AS INTEGER.
DEFINE variable  tipo_cuenta1_origen AS CHARACTER.
DEFINE variable  tipo_cuenta2_destino AS CHARACTER.
DEFINE variable  secuencia_reversar AS CHARACTER.
DEFINE variable  usuario_caja AS CHARACTER.
DEFINE variable  TransCobroComision AS CHARACTER.
DEFINE variable  ECG_STR0 AS CHARACTER.

/* Variables representing parameter values in the READ-XML( ), READ-XMLSCHEMA( ), WRITE-XML( ), and WRITE-XMLSCHEMA( ) methods. */
DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL NO-UNDO.
DEFINE VARIABLE cFieldTypeMapping AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVerifySchemaMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEncoding AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted AS LOGICAL NO-UNDO.
DEFINE VARIABLE lWriteSchema AS LOGICAL NO-UNDO.
DEFINE VARIABLE lMinSchema AS LOGICAL NO-UNDO.
DEFINE VARIABLE lWriteBeforeImage AS LOGICAL NO-UNDO.


DEFINE VARIABLE tottra AS DECIMAL FORMAT ">>>,>>>,>>>>,>>>".
DEFINE VARIABLE totcom AS DECIMAL FORMAT ">>>,>>>,>>>>,>>>".
DEFINE VARIABLE nombrearch AS CHARACTER.
DEFINE VARIABLE totreg AS INTEGER.
DEFINE VARIABLE regctr AS INTEGER.
DEFINE VARIABLE xfecha AS CHARACTER FORMAT "x(8)".

DEFINE TEMP-TABLE ECG
    /* Tags de transacción (lectura) */
    FIELD S001 AS CHARACTER /* Fecha */
    FIELD S002 AS CHARACTER /* Hora */
    FIELD S003 AS CHARACTER /* Comercio */
    FIELD S004 AS CHARACTER /* Sucursal */
    FIELD S005 AS CHARACTER /* Terminal */
    /*FIELD S006 AS CHARACTER /* Aplicativo */*/
    FIELD S007 AS CHARACTER /* Grupo transaccional */
    FIELD S008 AS CHARACTER /* Transacción */
    FIELD S009 AS CHARACTER /* Secuencia */
    FIELD S010 AS CHARACTER /* Origen */
    FIELD S011 AS CHARACTER /* Documento Cliente */
    FIELD S012 AS CHARACTER /* Tipo de documento */
    FIELD S013 AS CHARACTER /* Documento Cliente Destino */
    FIELD S014 AS CHARACTER /* Tipo de documento cliente destino */
    FIELD S015 AS CHARACTER /* Causal de la Transacción */
    FIELD S016 AS CHARACTER /* Adquieriente */
    /*FIELD S017 AS CHARACTER /* Flag de proceso de mensaje */
    FIELD S018 AS CHARACTER /* Canal */*/
    /*FIELD S019 AS CHARACTER /* Número de autorización / origen */
    FIELD S01A AS CHARACTER /* Track de la tarjeta */
    FIELD S01B AS CHARACTER /* Pin Block */
    FIELD S01C AS CHARACTER /* PCI Terminal Vendor */
    FIELD S01D AS CHARACTER /* PCI Terminal Model */
    FIELD S01E AS CHARACTER /* PCI Terminal Firmware */*/
    FIELD S01F AS CHARACTER /* PCI Terminal Serial */
    /*FIELD S01G AS CHARACTER /* PCI Terminal Vendor II */
    FIELD S01H AS CHARACTER /* PCI Terminal Model II */
    FIELD S01I AS CHARACTER /* PCI Terminal Firmware II */
    FIELD S01J AS CHARACTER /* PCI Terminal Serial II */
    FIELD S01K AS CHARACTER /* PCI Terminal Vendor III */
    FIELD S01L AS CHARACTER /* PCI Terminal Model III */
    FIELD S01M AS CHARACTER /* PCI Terminal Firmare III */
    FIELD S01N AS CHARACTER /* PCI Terminal Serial III */*/
    FIELD S01S AS CHARACTER /* Terminal - Ubicación */
    FIELD S01T AS CHARACTER /* Terminal - Tipo */
    FIELD S01U AS CHARACTER /* Terminal - País */
    /*FIELD S01V AS CHARACTER /* Terminal - Departamento */
    FIELD S01W AS CHARACTER /* Terminal - Ciudad */*/
    FIELD S020 AS CHARACTER /* Cuenta 1 / Origen */
    FIELD S021 AS CHARACTER /* Cuenta 2 / Destino */
    /*FIELD S022 AS CHARACTER /* Cuenta 3 */
    FIELD S023 AS CHARACTER /* Cuenta 4 */
    FIELD S024 AS CHARACTER /* Cuenta 5 */*/
    FIELD S025 AS CHARACTER /* Entidad dueña cuenta origen */
    FIELD S026 AS CHARACTER /* Entidad dueña cuenta destino */
    FIELD S027 AS CHARACTER /* Entidad dueña terminal */
    FIELD S028 AS CHARACTER /* Comercio - Terminal - NIT/CC */
    FIELD S029 AS CHARACTER /* Comercio - Terminal - Descripción */
    FIELD S02A AS CHARACTER /* Cheque - Código de banco */
    FIELD S02B AS CHARACTER /* Cheque - Cuenta de banco */
    FIELD S02C AS CHARACTER /* Cheque - Número */
    FIELD S02D AS CHARACTER /* Cheque - Total cheques */
    FIELD S02E AS CHARACTER /* Código de barras */
    /*FIELD S02F AS CHARACTER /* Código de transacción para cálculo de comisión */*/
    FIELD S030 AS CHARACTER /* Número de tarjeta */
    FIELD S031 AS CHARACTER /* Fecha contable */
    FIELD S032 AS CHARACTER /* Valor */
    FIELD S033 AS CHARACTER /* Valor base */
    FIELD S034 AS CHARACTER /* Valor impuesto */
    FIELD S035 AS CHARACTER /* Valor reteción */
    FIELD S036 AS CHARACTER /* Valor propina */
    FIELD S037 AS CHARACTER /* Valor comisión */
    FIELD S038 AS CHARACTER /* Cuotas */
    FIELD S039 AS CHARACTER /* Número de factura */
    FIELD S03A AS CHARACTER /* Tipo de cuenta 1 / Origen */
    FIELD S03B AS CHARACTER /* Tipo de cuenta 2 / Destino */
    FIELD S03C AS CHARACTER /* Tipo de cuenta 3 */
    /*FIELD S03D AS CHARACTER /* Tipo de cuenta 4 */
    FIELD S03E AS CHARACTER /* Tipo de cuenta 5 */
    FIELD S03F AS CHARACTER /* Secuencia a reversar */
    FIELD S03G AS CHARACTER /* Transacción para cobro comisión */
    FIELD S03H AS CHARACTER /* Código del equipo. Ej.: Número de teléfono desde donde se llamó al IVR */*/
    FIELD S03V AS CHARACTER /* Usuario / Autorizador */
    FIELD S03X AS CHARACTER /* Usuariol / Caja */
    FIELD S050 AS CHARACTER /* codigo error */
    FIELD CONTABILIZADO AS LOGICAL.
    /*FIELD S03Z AS CHARACTER /* Sesión IVR o WEB */
    FIELD S040 AS CHARACTER /* Documento */
    FIELD S041 AS CHARACTER /* Tipo de documento */
    FIELD S042 AS CHARACTER /* Nombres */
    FIELD S043 AS CHARACTER /* Apellidos */
    FIELD S044 AS CHARACTER /* Dirección residencia */
    FIELD S045 AS CHARACTER /* Dirección trabajo */
    FIELD S046 AS CHARACTER /* Teléfono residencia */
    FIELD S047 AS CHARACTER /* Teléfono trabajo */
    FIELD S048 AS CHARACTER /* Teléfono móvil */
    FIELD S049 AS CHARACTER /* Fecha de nacimiento */
    FIELD S04A AS CHARACTER /* Sexo del cliente, F o M */
    FIELD S04B AS CHARACTER /* Correo electrónico */
    FIELD S04C AS CHARACTER /* País de nacimiento */
    FIELD S04D AS CHARACTER /* Departamento de nacimiento */
    FIELD S04E AS CHARACTER /* Ciudad de nacimiento */
    FIELD S04F AS CHARACTER /* País de residencia */
    FIELD S04G AS CHARACTER /* Departamento de residencia */
    FIELD S04H AS CHARACTER /* Ciudad de residencia */
    FIELD S04I AS CHARACTER /* Tipo de cliente = ENROLAMIENTO BIOMETRÍA */
    FIELD S04J AS CHARACTER /* Información adicional cliente */
    FIELD STR0 AS CHARACTER.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-346 RECT-345 B-Importar BtnCancel ~
Btn_Done 
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
     SIZE 12 BY 1.38.

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 12 BY 1.38
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 12 BY 1.62 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-4 
     LABEL "" 
     SIZE 36 BY .81.

DEFINE VARIABLE w-Mensaje AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY .54
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
     BUTTON-4 AT ROW 1.54 COL 18 WIDGET-ID 22
     w-totreg AT ROW 3.42 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     B-Importar AT ROW 4.23 COL 56 WIDGET-ID 188
     W-TotTx AT ROW 4.35 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 168
     W-TotCom AT ROW 5.27 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 170
     BtnCancel AT ROW 5.85 COL 57 WIDGET-ID 190
     Btn_Done AT ROW 8.27 COL 56 WIDGET-ID 20
     w-Mensaje AT ROW 9.62 COL 4 NO-LABEL WIDGET-ID 182
     " Registros Leídos:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 3.42 COL 10.86 WIDGET-ID 8
          FGCOLOR 12 
     "IMPORTAR OPERACIONES TARJETAS" VIEW-AS TEXT
          SIZE 35 BY .5 AT ROW 1.69 COL 18.57 WIDGET-ID 26
          BGCOLOR 8 FONT 1
     "Valor Comisiones:" VIEW-AS TEXT
          SIZE 11.86 BY .81 AT ROW 5.23 COL 11.29 WIDGET-ID 174
          FGCOLOR 12 
     "Valor Transacciones:" VIEW-AS TEXT
          SIZE 15 BY .81 AT ROW 4.35 COL 9 WIDGET-ID 172
          FGCOLOR 12 
     " Resumen:" VIEW-AS TEXT
          SIZE 11 BY .81 AT ROW 2.69 COL 4.86 WIDGET-ID 4
          FGCOLOR 0 
     RECT-346 AT ROW 1 COL 1 WIDGET-ID 186
     RECT-345 AT ROW 3.19 COL 4 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70 BY 10.77
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
         HEIGHT             = 10.77
         WIDTH              = 70
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
/* SETTINGS FOR BUTTON BUTTON-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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
    DEFINE VARIABLE vcRaiz AS CHARACTER INITIAL "c:\info_fodun\".
    DEFINE VARIABLE vcNomArchInput AS CHARACTER.
    DEFINE VARIABLE vcFecMov AS CHARACTER.
    DEFINE VARIABLE vdaFecMov AS DATE.

    RUN IniciaVar.

    SYSTEM-DIALOG GET-FILE vcNomArchInput
        TITLE "Selección Archivo Movimiento ..."
        FILTERS "Comprimidos (*.7z)"   "*.7z", "Todos (*.*)"   "*.*"
        MUST-EXIST
        USE-FILENAME
        UPDATE vlOkPressed.

    ASSIGN vcComando = "C:\7-Zip\7z".

    OS-COMMAND SILENT VALUE(vcComando) x VALUE(vcNomArchInput) -oc:\info_FODUN\Visionamos -y -p0123456789ABCDEF0123456789ABCDEF.
    ASSIGN  vcRaiz = vcRaiz + "visionamos\"
            vcFecMov = SUBSTRING(vcNomArchInput,(LENGTH(vcNomArchInput) - 23),8)
            vcNomArchInput = SUBSTRING(vcNomArchInput,(LENGTH(vcNomArchInput) - 23),8) + ".AUT"
            vcNomArchInput = vcRaiz + vcNomArchInput.

    ASSIGN  vdaFecMov = intToDate(vcFecMov)
            w-Mensaje:SCREEN-VALUE = "Cargando Archivo " + vcNomArchInput.

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
                /*DISP nombrearch FORMAT "X(25)".*/
                INPUT FROM VALUE(vcNomArchInput).
                REPEAT:
                   CREATE ecg.
                   IMPORT DELIMITER "," ecg.
                   IF S001 NE "00000018" THEN
                      ASSIGN totreg = totreg + 1.  
                   ELSE ASSIGN regctr = integer(S003)
                               xfecha = S002.
                END.
                ASSIGN  w-totreg:SCREEN-VALUE IN FRAME default-frame  = STRING(totreg,">>>,>>>,>>9")
                        totreg = 0.
                /* IF S001 NE "00000018" THEN : es el registro de control que tiene */
                /* S001 - codigo entidad 00000018
                   S002 - fecha de cargue 20110218
                   S003 - numero de registros. 
                */
                /*DISP totreg regctr xfecha.*/
                FOR EACH ecg WHERE S001 NE "00000018":
                    ASSIGN totreg = totreg + 1.  
                    CASE ECG.S007:
                            WHEN "0200" THEN GrupoTransaccional = 0.    /* Transacción normal - Validar condiciones y aplicar */
                            WHEN "0220" THEN GrupoTransaccional = 1.    /* Transacción normal - Aplicar */
                            WHEN "0400" THEN GrupoTransaccional = 2.    /* Reversión Red Visionamos */
                            WHEN "0420" THEN GrupoTransaccional = 2.    /* Reversión Red Banco de Bogotá */
                    END.
                    CASE ECG.S008:  /* Codigo_trans */
                            WHEN "00" THEN transaccion = "CR".
                            WHEN "01" THEN transaccion = "RE".
                            WHEN "20" THEN transaccion = "ACR".
                            WHEN "21" THEN transaccion = "CE".
                            WHEN "30" THEN transaccion = "CS".
                            WHEN "35" THEN transaccion = "CM".
                            WHEN "40" THEN transaccion = "TF".
                            WHEN "89" THEN transaccion = "CCT".
                            WHEN "90" THEN transaccion = "PIN".
                    END.
                    secuencia = ECG.S009.
                    CASE ECG.S010:  /* Entidad_origen */
                            WHEN "03" THEN DO:
                                IF transaccion = "RE" THEN
                                    transaccion = "RR".
            
                                IF transaccion = "CS" THEN
                                    transaccion = "CSR".
                            END.
                            WHEN "06" THEN origen = "T".
                            /*WHEN "99" THEN DO:
                                /* Banco de Bogotá */
                                /* Códigos origen (<S016>) para el Banco de Bogotá
                                    1 - Servibanca
                                    2 - Redeban
                                    3 - Credibanco
                                    4 - Megabanco
                                    5 - RedExpress
                                    6 - ATH Banco de Bogotá
                                    7 - ATH Aval
                                    8 - Otras redes recibidas por ATH */
                                origen = "T".   /* Temporal, mientras se desarrolla el tema de las comisiones */
                                FIND FIRST ahorros WHERE /*ahorros.tarjetaDB*/ ahorros.cue_padre = ECG.S030 NO-LOCK NO-ERROR.
                                IF AVAILABLE ahorros THEN
                                    ECG.S011 = ahorros.nit.
                            END.*/
                    END CASE.
                    ASSIGN  documento_cliente = TRIM(ECG.S011)           /* nit */
                            documento_cliente_destino = TRIM(ECG.S013).   /* nit_destino */
                    CASE INTEGER(ECG.S015):  /* Causal de la transacción */
                       WHEN 3 THEN
                          IF transaccion = "RE" THEN transaccion = "RC".
                       WHEN 4 THEN
                          IF transaccion = "CE" THEN transaccion = "CC".
                    END CASE.
            
                    CASE ECG.S01T:  /* Terminal - Tipo */
                            WHEN "04" OR
                            WHEN "41" THEN DO:
                                IF transaccion = "CR" THEN DO:
                                    transaccion = "CP".
                                    origen = "T".
                                END.
                            END.
                            WHEN "22" THEN origen = "2".
                            WHEN "20" OR
                            WHEN "24" THEN origen = "3".
                            WHEN "23" THEN origen = "4".
                            WHEN "40" THEN origen = "1".
                    END CASE.
                    /* Definición de transacciones Intercooperativas */
                    /* CEC - CCC */
                    IF (transaccion = "CE" OR transaccion = "CC") AND INTEGER(ECG.S025) <> 18 THEN DO: /* Entidad dueña de la cuenta destino */
                       CASE transaccion:
                            WHEN "CE" THEN transaccion = "CEC".
                            WHEN "CC" THEN transaccion = "CCC".
                       END CASE.
                    END.
            
                    /* REO - RCO */
                    IF (transaccion = "RE" OR transaccion = "RC") AND INTEGER(ECG.S025) <> 18 THEN DO: /* Entidad dueña de la cuenta origen */
                       CASE transaccion:
                            WHEN "RE" THEN transaccion = "REO".
                            WHEN "RC" THEN transaccion = "RCO".
                       END CASE.
                    END.
            
                    /* CEO - CCO */
                    IF (transaccion = "CE" OR transaccion = "CC") AND ECG.S027 <> "00000018" THEN DO: /* Entidad dueña terminal */
                       CASE transaccion:
                            WHEN "CE" THEN transaccion = "CEO".
                            WHEN "CC" THEN transaccion = "CCO".
                       END CASE.
                    END.
            
                    /* REC - RCC */
                    IF (transaccion = "RE" OR transaccion = "RC") AND ECG.S027 <> "00000018" THEN DO: /* Entidad dueña terminal */
                        CASE transaccion:
                            WHEN "RE" THEN transaccion = "REC".
                            WHEN "RC" THEN transaccion = "RCC".
                        END CASE.
                    END.
                    /* ----- */
                    /* TFC */
                    IF (transaccion = "RE" OR transaccion = "RC") AND ECG.S027 <> "00000018" AND ECG.S027 <> "" THEN DO: /* Entidad dueña terminal */
                        transaccion = "TFC".
                    END.
            
                    /* TFO */
                    IF (transaccion = "CE" OR transaccion = "CC") AND ECG.S027 <> "00000018" AND ECG.S027 <> "" THEN DO: /* Entidad dueña terminal */
                        transaccion = "TFO".
                    END.
            
                    /* Compras */
                    IF (transaccion = "CP") AND ECG.S027 <> "00000018" THEN /* Entidad dueña terminal */
                       transaccion = "CPC".
            
                    IF (transaccion = "CP") AND ECG.S027 = "00000018" THEN DO:
                       IF ECG.S025 <> "00000018" THEN
                          transaccion = "CPO".
                    END.
            
                    ASSIGN  cuenta1_Origen = TRIM(ECG.S020)                          /* Cuenta_origen */
                            cuenta2_destino = TRIM(ECG.S021).                         /* Cuenta_destino */
            
                    IF cuenta2_destino = "" THEN
                       cuenta2_destino = cuenta1_origen.
            
                    entidad_duena_cuenta_origen = INTEGER(ECG.S025).    /* codigo_cooperativa_origen */ 
            
                    IF ECG.S026 = "" THEN
                       entidad_duena_cuenta_destino = INTEGER(ECG.S025).
                    ELSE
                        entidad_duena_cuenta_destino = INTEGER(ECG.S026).   /* codigo_cooperativa_origen */
            
                    IF INTEGER(entidad_duena_cuenta_destino) = 0 AND (transaccion = "CEO" OR transaccion = "CCO") THEN
                       entidad_duena_cuenta_destino = INTEGER(ECG.S027).
            
                    ASSIGN  entidad_duena_terminal = TRIM(ECG.S027)            /* Cod_Coop_RockPos */
                            cheque_codigo_banco = INTEGER(ECG.S02A)            /* Codigo_banco_cheque */
                            cheque_cuenta_banco = ECG.S02B                     /* Numero_Cuenta_Cheque */
                            cheque_numero = ECG.S02C                           /* Numero_Cheque */
                            cheque_total_cheques = INTEGER(ECG.S02D)           /* cantidad_cheque */
                            codigo_barras = ECG.S02E                           /* codigo_barras */
                            numero_tarjeta = TRIM(ECG.S030)                    /* tarjeta_cliente */
                            fecha_contable = INTEGER(ECG.S031)                 /* fecha_negocio */
                            valor = DECIMAL(ECG.S032) / 100                     /* Valor */
                            valor_base = DECIMAL(ECG.S033)                     /* Base */
                            valor_impuesto = DECIMAL(ECG.S034)                 /* IVA */
                            valor_retencion = DECIMAL(ECG.S035)                /* Retencion */
                            valor_propina = DECIMAL(ECG.S036)                  /* Propina */
                            valor_transaccion = DECIMAL(ECG.S037) / 100        /* valor_cobro */
                            cuotas = INTEGER(ECG.S038)                         /* cuotas */
                            numero_factura = INTEGER(ECG.S039).                /* Referencia */
            
                    CASE ECG.S03A:  /* Codigo_Producto_Origen */
                        WHEN "10" THEN tipo_cuenta1_origen = "AH".
                        WHEN "50" THEN tipo_cuenta1_origen = "CR".
                        WHEN "60" THEN tipo_cuenta1_origen = "CT".
                    END CASE.
            
                    CASE ECG.S03B:  /* Codigo_producto_destino */
                        WHEN "10" THEN tipo_cuenta2_destino = "AH".
                        WHEN "50" THEN tipo_cuenta2_destino = "CR".
                        WHEN "60" THEN tipo_cuenta2_destino = "CT".
                        OTHERWISE tipo_cuenta2_destino = tipo_cuenta1_origen.
                    END.
            
                    IF grupoTransaccional = 2 THEN
                        secuencia_reversar = ECG.S009.  /* Nro_trans_reversar */
                    /* contabiliza. */
            
                    ASSIGN tottra = tottra + valor 
                           totcom = totcom + valor_transaccion .
                    DEFINE VARIABLE vdb LIKE mov_contable.db.
                    DEFINE VARIABLE vcr LIKE mov_contable.cr.
                    /* transacciones habilitadas de cupo rotativo*/
                     FIND FIRST creditos WHERE creditos.nit = documento_cliente AND
                                               creditos.num_credito = DECIMAL(cuenta1_origen) NO-ERROR.
            
                     IF AVAILABLE(creditos) THEN DO:
                         FInd FIRST comprobante WHERE comprobante.comprobante = 22 NO-ERROR.
                         IF AVAILABLE(comprobante)  THEN ASSIGN Wdoc = comprobante.secuencia + 1
                                                     comprobantes.secuencia = comprobante.secuencia + 1.
                         RELEASE comprobante.
                         IF grupotransaccional = 0 THEN 
                           ASSIGN vdb = valor +  valor_transaccion
                                  vcr = 0.
                        IF grupotransaccional = 2 THEN 
                           ASSIGN vcr = valor +  valor_transaccion
                                  vdb = 0.
                       /* DISP transaccion grupotransaccional documento_cliente origen cuenta1_origen tipo_cuenta1_origen . */
                        ASSIGN creditos.Sdo_capital = Creditos.Sdo_capital + vdb - vcr.
                                CREATE mov_contable.
                                ASSIGN Mov_Contable.Agencia         = Creditos.agencia
                                       Mov_Contable.Destino         = Creditos.agencia
                                       Mov_Contable.Comprobante     = 22
                                       Mov_Contable.Num_Documento   = wdoc
                                       Mov_contable.Doc_referencia  = cuenta1_origen
                                       Mov_Contable.Fec_Contable    = TODAY
                                       Mov_Contable.Fec_Grabacion   = TODAY
                                       Mov_Contable.Cuenta          = "14410508"
                                       Mov_Contable.Comentario      = "Avance " + transaccion + secuencia
                                       Mov_Contable.Usuario          = W_usuario /*"999"*/
                                       Mov_Contable.Estacion         = "005"
                                       Mov_Contable.Nit              = creditos.nit
                                       Mov_contable.db               = vdb
                                       mov_contable.cr               = vcr.

                                CREATE mov_contable.
                                ASSIGN Mov_Contable.Agencia         = Creditos.agencia
                                       Mov_Contable.Destino         = Creditos.agencia
                                       Mov_Contable.Comprobante     = 22
                                       Mov_Contable.Num_Documento   = wdoc
                                       Mov_contable.Doc_referencia  = cuenta1_origen
                                       Mov_Contable.Fec_Contable    = TODAY
                                       Mov_Contable.Fec_Grabacion   = TODAY
                                       Mov_Contable.Cuenta          = "24459550"
                                       Mov_Contable.Comentario      = "Avance " + transaccion + secuencia
                                       Mov_Contable.Usuario          = W_usuario /*"999"*/
                                       Mov_Contable.Estacion         = "005"
                                       Mov_Contable.Nit              = creditos.nit
                                       Mov_contable.db               = vcr
                                       mov_contable.cr               = vdb.

                        ASSIGN CONTABILIZADO = YES.
                        CASE transaccion:
                            WHEN "RR"  THEN DO:
                            END.
                            WHEN "CR"  THEN DO:
                            END.
                            WHEN "CSR" THEN DO:
                            END.
                            WHEN "RE"  THEN DO:
                            END.
                            WHEN "CS"  THEN DO:
                            END.
                            WHEN "TF"  THEN DO:
                            END.
                        END.
                      END.
                     ELSE 
                        ASSIGN CONTABILIZADO = NO. 
                END.
            
                ASSIGN  w-Mensaje:SCREEN-VALUE IN FRAME default-frame = "Cargado Archivo día " + STRING(vdaFecMov,"99/99/9999")
                        w-TotTx:SCREEN-VALUE  = STRING(tottra,">>>,>>>,>>9")
                        w-TotCom:SCREEN-VALUE = STRING(totcom,">>>,>>>,>>9").
                CREATE ControlMovTDB.
                UPDATE  controlMovTDB.fecCarga  = NOW
                        controlMovTDB.fecMov    = vdaFecMov
                        controlMovTDB.numRegPro = INTEGER(w-totreg:SCREEN-VALUE)
                        controlMovTDB.usuario   = W_usuario.
            
                MESSAGE "Proceso de Importación terminado"
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
  ENABLE RECT-346 RECT-345 B-Importar BtnCancel Btn_Done 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IniciaVar C-Win 
PROCEDURE IniciaVar :
ASSIGN w-Mensaje:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
       w-totreg:SCREEN-VALUE = ""
       w-TotTx:SCREEN-VALUE  = ""
       w-TotCom:SCREEN-VALUE = "".

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

