&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI

/* oakley */

&ANALYZE-RESUME

/* oakley */

&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.
{Incluido\variable.i "shared"}
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE P_Nit LIKE Clientes.Nit.
DEFINE VARIABLE p_Nombre LIKE Clientes.Nombre.
DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
DEFINE VARIABLE P_AgeCli LIKE Clientes.Agencia.
DEFINE VARIABLE W_Error AS LOGICAL.
DEFINE VARIABLE W_Autorizo LIKE Usuarios.Usuario.
DEFINE VAR W_OK AS LOGICAL INITIAL YES.
DEFINE VAR vTime AS INTEGER.

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
&Scoped-Define ENABLED-OBJECTS wFecPagare chkReportaTopeIndividual ~
numeroCupo wSdo_Canje wSdo_Disponible wTarjetaDB wnewcupo btn_procesar ~
Btn_Cancelar BtnDone Btn_Consulta BUTTON-1 cuentaAhorros sdoDisponibleAh ~
ECT-309 RECT-274 
&Scoped-Define DISPLAYED-FIELDS Clientes.Nit 
&Scoped-define DISPLAYED-TABLES Clientes
&Scoped-define FIRST-DISPLAYED-TABLE Clientes
&Scoped-Define DISPLAYED-OBJECTS wFecPagare chkReportaTopeIndividual ~
numeroCupo wSdo_Canje wSdo_Disponible wTarjetaDB Cmb_agencia wnewcupo ~
W_NomTitular operacionesCajero montoCajero operacionesPOS montoPOS ~
cuentaAhorros sdoDisponibleAh 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Cancelar BtnDone Btn_Consulta BUTTON-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "&Salir" 
     SIZE 10 BY 1.62
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes/volver2.bmp":U
     LABEL "&Cancelar" 
     SIZE 10 BY 1.62
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 10 BY 1.62.

DEFINE BUTTON btn_procesar 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "&Guardar" 
     SIZE 10 BY 1.62
     FONT 4.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE Cmb_agencia AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 26 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cuentaAhorros AS CHARACTER FORMAT "X(14)" INITIAL "?" 
     LABEL "Número de Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .81 NO-UNDO.

DEFINE VARIABLE montoCajero AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Monto Máximo Cajero" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE montoPOS AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Monto Máximo POS" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE numeroCupo AS CHARACTER FORMAT "X(14)" INITIAL "?" 
     LABEL "Número de Crédito" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .81 NO-UNDO.

DEFINE VARIABLE operacionesCajero AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "# Operaciones Cajero" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE operacionesPOS AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "# Operaciones POS" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE sdoDisponibleAh AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 
     LABEL "Saldo Actual Disp" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .81 NO-UNDO.

DEFINE VARIABLE wFecPagare AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha firma de Pagaré" 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE wnewcupo AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Cupo a aprobar" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE wSdo_Canje AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 
     LABEL "Cupo aprobado" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .81 NO-UNDO.

DEFINE VARIABLE wSdo_Disponible AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 
     LABEL "Saldo actual" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .81 NO-UNDO.

DEFINE VARIABLE wTarjetaDB AS CHARACTER FORMAT "X(16)" 
     LABEL "#Tarjeta Débito" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .81
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 46 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE ECT-309
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 11.31.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 8.62.

DEFINE VARIABLE chkReportaTopeIndividual AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 1.72 BY .77 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Frm_Clientes FOR 
      Ahorros SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm_Clientes
     wFecPagare AT ROW 3.27 COL 67 COLON-ALIGNED WIDGET-ID 44
     chkReportaTopeIndividual AT ROW 7.73 COL 80 WIDGET-ID 40
     Clientes.Nit AT ROW 1.54 COL 6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     numeroCupo AT ROW 5.46 COL 20.14 COLON-ALIGNED HELP
          "Numero que identifica la cuenta de ahorros" WIDGET-ID 20
     wSdo_Canje AT ROW 7.15 COL 20.14 COLON-ALIGNED HELP
          "Ingrese el Saldo en Canje que posee la cuenta" WIDGET-ID 28
     wSdo_Disponible AT ROW 6.31 COL 20.14 COLON-ALIGNED HELP
          "Ingrese el Saldo disponible de la cuenta de ahorros" WIDGET-ID 26
     wTarjetaDB AT ROW 2.35 COL 60 COLON-ALIGNED HELP
          "Numero de la Tarjeta Débito" WIDGET-ID 22
     Cmb_agencia AT ROW 5.58 COL 54 COLON-ALIGNED WIDGET-ID 16
     wnewcupo AT ROW 6.58 COL 63 COLON-ALIGNED WIDGET-ID 14
     btn_procesar AT ROW 4.69 COL 90
     Btn_Cancelar AT ROW 6.31 COL 90
     BtnDone AT ROW 7.92 COL 90
     Btn_Consulta AT ROW 3.08 COL 90
     BUTTON-1 AT ROW 1.46 COL 90
     W_NomTitular AT ROW 1.54 COL 34 COLON-ALIGNED
     operacionesCajero AT ROW 8.77 COL 74 COLON-ALIGNED WIDGET-ID 32
     montoCajero AT ROW 10.54 COL 63 COLON-ALIGNED WIDGET-ID 34
     operacionesPOS AT ROW 9.58 COL 74 COLON-ALIGNED WIDGET-ID 38
     montoPOS AT ROW 11.35 COL 63 COLON-ALIGNED WIDGET-ID 36
     cuentaAhorros AT ROW 10.27 COL 20 COLON-ALIGNED HELP
          "Numero que identifica la cuenta de ahorros" WIDGET-ID 46
     sdoDisponibleAh AT ROW 11.12 COL 20 COLON-ALIGNED HELP
          "Ingrese el Saldo disponible de la cuenta de ahorros" WIDGET-ID 52
     "Matricular nuevo Cupo Rotativo" VIEW-AS TEXT
          SIZE 39.29 BY .81 AT ROW 4.58 COL 43 WIDGET-ID 4
          BGCOLOR 18 FGCOLOR 15 
     "Cupo de Crédito" VIEW-AS TEXT
          SIZE 35 BY .81 AT ROW 4.62 COL 3 WIDGET-ID 18
          BGCOLOR 18 FGCOLOR 15 
     "Reporta topes individual" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 7.77 COL 58.29 WIDGET-ID 42
     "Cuenta de Ahorros" VIEW-AS TEXT
          SIZE 35 BY .81 AT ROW 9.42 COL 3 WIDGET-ID 48
          BGCOLOR 18 FGCOLOR 15 
     ECT-309 AT ROW 1.27 COL 2
     RECT-274 AT ROW 1.27 COL 88.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.14 ROW 1
         SIZE 100.43 BY 12.77
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
         TITLE              = "Adjudicación de Cupo Rotativo"
         HEIGHT             = 11.85
         WIDTH              = 100.43
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
/* SETTINGS FOR COMBO-BOX Cmb_agencia IN FRAME Frm_Clientes
   NO-ENABLE                                                            */
ASSIGN 
       cuentaAhorros:READ-ONLY IN FRAME Frm_Clientes        = TRUE.

/* SETTINGS FOR FILL-IN montoCajero IN FRAME Frm_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN montoPOS IN FRAME Frm_Clientes
   NO-ENABLE                                                            */
ASSIGN 
       numeroCupo:READ-ONLY IN FRAME Frm_Clientes        = TRUE.

/* SETTINGS FOR FILL-IN operacionesCajero IN FRAME Frm_Clientes
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN operacionesPOS IN FRAME Frm_Clientes
   NO-ENABLE                                                            */
ASSIGN 
       sdoDisponibleAh:READ-ONLY IN FRAME Frm_Clientes        = TRUE.

ASSIGN 
       wSdo_Canje:READ-ONLY IN FRAME Frm_Clientes        = TRUE.

ASSIGN 
       wSdo_Disponible:READ-ONLY IN FRAME Frm_Clientes        = TRUE.

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
ON END-ERROR OF C-Win /* Adjudicación de Cupo Rotativo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Adjudicación de Cupo Rotativo */
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
    ASSIGN Wnewcupo.

    DEFINE VAR wcrage AS INTEGER.
    DEFINE VAR wcrnit AS CHARACTER.
    DEFINE VAR wcrcodcre AS INTEGER.
    DEFINE VAR wcrnumcre AS INTEGER.
    DEFINE VAR wtasa AS DECIMAL.
    DEFINE VAR wantcupo AS DECIMAL.
    DEFINE VAR flagTarjetaValida AS LOGICAL.

    vTime = TIME.

    /* Revisamos que la cédula digitada sea un Asociado */
    FIND FIRST ahorros WHERE ahorros.nit = clientes.nit:SCREEN-VALUE
                         AND ahorros.tip_ahorro = 4
                         AND ahorros.sdo_disponible > 0
                         AND ahorros.estado = 1 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ahorros THEN DO:
        MESSAGE "Está persona no es Asociada del Fondo (no posee Aportes activos con saldo)." SKIP
                "No se permite la creación de un Cupo Rotativo con las condiciones actuales."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    /* Revisamos la validez de la tarjeta débito */
    IF LENGTH(wTarjetaDB:SCREEN-VALUE) <> 16 AND SUBSTRING(wTarjetaDB:SCREEN-VALUE,1,4) <> '5907' AND SUBSTRING(wTarjetaDB:SCREEN-VALUE,1,4) <> '4177' THEN DO:
        MESSAGE wTarjetaDB:SCREEN-VALUE "no es un número de tarjeta válido para FODUN." SKIP
                "Es indipensabel ingresar un número de tarjeta válido. Revise por favor!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.

        flagTarjetaValida = FALSE.
    END.
    ELSE
        flagTarjetaValida = TRUE.

    FIND FIRST creditos WHERE creditos.nit EQ clientes.nit:SCREEN-VALUE
                          AND creditos.cod_credito = 123
                          AND creditos.estado = 2 NO-ERROR.
    IF NOT AVAILABLE(creditos) THEN DO:
        FIND FIRST pro_creditos WHERE (pro_credito.cod_credito = 123 OR pro_credito.cod_credito = 870) NO-LOCK NO-ERROR.
        IF AVAILABLE(pro_creditos) THEN
            FIND FIRST indicadores WHERE Indicadores.indicador = pro_creditos.cod_tasa  NO-LOCK NO-ERROR.

        IF AVAILABLE(indicadores) THEN DO:
            wtasa =  (((EXP( (indicadores.tasa / 100) + 1,1 / 12)) - 1 )  * 100) * 12.
        END.
        ELSE DO:
            MESSAGE "Se debe establecer la configuracion de Cupo Rotativo en Pro_Creditos"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN.
        END.

        /* Se revisa el cupo máximo parametrizado en la tabla pro_creditos */
        IF DECIMAL(wnewcupo:SCREEN-VALUE) > pro_creditos.Val_Montomaximo OR DECIMAL(wnewcupo:SCREEN-VALUE) < pro_creditos.Val_MontoMinimo THEN DO:
            MESSAGE "El Cupo otorgado no está dentro del rango permitido para este producto." SKIP
                    "Por favor, verifique y corrija."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN.
        END.

        /* Revisamos la vigencia del pagaré */
        IF wFecPagare:SCREEN-VALUE = "" OR ADD-INTERVAL(DATE(wFecPagare:SCREEN-VALUE),5,"years") <= w_fecha THEN DO:
            MESSAGE "El Asociado no posee un pagaré vigente." SKIP
                    "Por favor, corrija esta situación."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN.
        END.

        ASSIGN wFecPagare.

        IF clientes.fecPagare <> wFecPagare THEN
            clientes.fecPagare = wFecPagare.

        CREATE creditos.
        ASSIGN creditos.fec_aprobacion = w_fecha
               creditos.agencia = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
               creditos.nit = clientes.nit:SCREEN-VALUE
               creditos.cod_credito = 123
               creditos.tip_credito = 1
               creditos.num_credito = NEXT-VALUE(sec_creditos)
               creditos.pagare = STRING(CURRENT-VALUE(sec_creditos))
               creditos.tasa = wtasa
               creditos.fec_desembolso = w_fecha
               creditos.usuario = w_usuario
               creditos.monto = DECIMAL(wnewcupo:SCREEN-VALUE)
               creditos.val_desembolso = DECIMAL(wnewcupo:SCREEN-VALUE)
               creditos.estado = 2
               creditos.detalle_estado = 1.

        FIND FIRST cfg_tarjetaDB NO-LOCK NO-ERROR.
        IF AVAILABLE cfg_tarjetaDB THEN
            creditos.plazo = cfg_tarjetaDB.plazoCupo.

        CREATE mov_creditos.
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
               Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
               Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
               Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
               Mov_Creditos.Cpte = 20
               Mov_Creditos.Descrip = "Asignación de Cupo -->" + STRING(wnewcupo:SCREEN-VALUE,"$>>>,>>>,>>9.99").

        IF flagTarjetaValida = TRUE THEN DO:
            CREATE tarjetas.
            ASSIGN tarjetas.nit = creditos.nit
                   tarjetas.tarjetaDB = wTarjetaDB:SCREEN-VALUE
                   tarjetas.estado = "01"
                   tarjetas.fec_cargue = w_fecha
                   tarjetas.hora_cargue = TIME
                   tarjetas.agencia = creditos.agencia
                   tarjetas.usuario = w_usuario
                   tarjetas.num_credito = creditos.num_Credito
                   tarjetas.Cue_Ahorros = cuentaAhorros:SCREEN-VALUE
                   tarjetas.operMaxCaj = DECIMAL(operacionesCajero:SCREEN-VALUE)
                   tarjetas.operMaxPOS = DECIMAL(operacionesPOS:SCREEN-VALUE)
                   tarjetas.montoMaxCaj = DECIMAL(montoCajero:SCREEN-VALUE)
                   tarjetas.montoMaxPOS = DECIMAL(montoPOS:SCREEN-VALUE)
                   tarjetas.ReportaTopesIndividual = LOGICAL(chkReportaTopeIndividual:SCREEN-VALUE).
        END.

        FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
        IF AVAILABLE pro_creditos THEN DO:
            FIND FIRST CortoLargo WHERE CortoLargo.Agencia EQ Creditos.Agencia
                                    AND CortoLargo.Clase_Producto EQ 2
                                    AND CortoLargo.Cod_Producto EQ Creditos.Cod_Credito
                                    AND CortoLargo.Plazo_Inicial GE 0 NO-LOCK NO-ERROR.
            IF AVAILABLE cortoLargo THEN
                creditos.cta_contable = cortoLargo.cta_AsoAd.
        END.

        ASSIGN wcrage = creditos.agencia
               wcrnit = creditos.nit
               wcrcodcre = creditos.cod_credito
               wcrnumcre = creditos.num_credito.

        RELEASE creditos.

        /*FIND FIRST creditos NO-LOCK NO-ERROR.*/

        RUN w-planpagosCupoRotativo.r (INPUT wcrage,
                                       INPUT wcrnit,
                                       INPUT wcrcodcre,
                                       INPUT wcrnumcre).

        CREATE Hoja_Vida.
        ASSIGN Hoja_Vida.Tipo = 1
               Hoja_Vida.Codigo = 1
               Hoja_Vida.Nit = clientes.nit:SCREEN-VALUE
               Hoja_Vida.Usuario = W_Usuario
               Hoja_Vida.Fec_Grabacion = W_fecha
               Hoja_Vida.Hora_Grabacion = TIME
               Hoja_Vida.Observacion = "Asignación de cupo rotativo #" + wnewcupo:SCREEN-VALUE.

        RELEASE creditos.

        MESSAGE "Terminó la creación del Cupo Rotativo exitosamente"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        btn_procesar:SENSITIVE = FALSE.
        RUN inicializar_variables.
    END.
    ELSE DO:
        MESSAGE "Está seguro que desea actualizar este Cupo Rotativo?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar actualización" UPDATE flagActualizar AS LOGICAL.

        IF flagActualizar = TRUE THEN DO:
            IF creditos.monto <> DECIMAL(wnewcupo:SCREEN-VALUE) THEN DO:
                CREATE mov_creditos.
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
                       Mov_Creditos.Int_Corriente = Creditos.Int_Corriente + creditos.INT_difCobro
                       Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
                       Mov_Creditos.Sdo_Capital = Creditos.Sdo_Capital
                       Mov_Creditos.Cpte = 20
                       Mov_Creditos.Descrip = "Reasignación de Cupo [" + STRING(creditos.monto,"$>>>,>>>,>>9.99") + "-->" + STRING(wnewcupo:SCREEN-VALUE,"$>>>,>>>,>>9.99") + "]".

                creditos.monto = DECIMAL(wnewcupo:SCREEN-VALUE).
                creditos.val_desembolso = DECIMAL(wnewcupo:SCREEN-VALUE).

                CREATE Hoja_Vida.
                ASSIGN Hoja_Vida.Tipo = 1
                       Hoja_Vida.Codigo = 1
                       Hoja_Vida.Nit = clientes.nit:SCREEN-VALUE
                       Hoja_Vida.Usuario = W_Usuario
                       Hoja_Vida.Fec_Grabacion = W_fecha
                       Hoja_Vida.Hora_Grabacion = TIME
                       Hoja_Vida.Observacion = "Reasignación de Cupo (" + STRING(creditos.num_credito) + ") - [" + STRING(creditos.monto,"$>>>,>>>,>>9.99") + "-->" + STRING(wnewcupo:SCREEN-VALUE,"$>>>,>>>,>>9.99") + "]".
            END.

            FIND FIRST tarjetas WHERE tarjetas.nit = creditos.nit
                                  AND tarjetas.num_credito = creditos.num_credito
                                  AND tarjetas.estado = '01' NO-ERROR.
            IF AVAILABLE tarjetas THEN DO:
                IF tarjetas.operMaxCaj <> DECIMAL(operacionesCajero:SCREEN-VALUE) OR
                    tarjetas.operMaxPOS <> DECIMAL(operacionesPOS:SCREEN-VALUE) OR
                    tarjetas.montoMaxCaj <> DECIMAL(montoCajero:SCREEN-VALUE) OR
                    tarjetas.montoMaxPOS <> DECIMAL(montoPOS:SCREEN-VALUE) THEN DO:

                    CREATE Hoja_Vida.
                    ASSIGN Hoja_Vida.Tipo = 1
                           Hoja_Vida.Codigo = 1
                           Hoja_Vida.Nit = clientes.nit:SCREEN-VALUE
                           Hoja_Vida.Usuario = W_Usuario
                           Hoja_Vida.Fec_Grabacion = W_fecha
                           Hoja_Vida.Hora_Grabacion = TIME
                           Hoja_Vida.Observacion = "Se cambian topes de Cupo (" + STRING(creditos.num_credito) + ")."

                    tarjetas.operMaxCaj = DECIMAL(operacionesCajero:SCREEN-VALUE).
                    tarjetas.operMaxPOS = DECIMAL(operacionesPOS:SCREEN-VALUE).
                    tarjetas.montoMaxCaj = DECIMAL(montoCajero:SCREEN-VALUE).
                    tarjetas.montoMaxPOS = DECIMAL(montoPOS:SCREEN-VALUE).
                END.
            END.
            ELSE DO:
                CREATE tarjetas.
                ASSIGN tarjetas.nit = creditos.nit
                       tarjetas.tarjetaDB = wTarjetaDB:SCREEN-VALUE
                       tarjetas.estado = "01"
                       tarjetas.fec_cargue = w_fecha
                       tarjetas.hora_cargue = TIME
                       tarjetas.agencia = creditos.agencia
                       tarjetas.usuario = w_usuario
                       tarjetas.num_credito = creditos.num_Credito
                       tarjetas.Cue_Ahorros = cuentaAhorros:SCREEN-VALUE
                       tarjetas.operMaxCaj = DECIMAL(operacionesCajero:SCREEN-VALUE)
                       tarjetas.operMaxPOS = DECIMAL(operacionesPOS:SCREEN-VALUE)
                       tarjetas.montoMaxCaj = DECIMAL(montoCajero:SCREEN-VALUE)
                       tarjetas.montoMaxPOS = DECIMAL(montoPOS:SCREEN-VALUE)
                       tarjetas.ReportaTopesIndividual = LOGICAL(chkReportaTopeIndividual:SCREEN-VALUE).
            END.

            tarjetas.ReportaTopesIndividual = LOGICAL(chkReportaTopeIndividual:SCREEN-VALUE).

            tarjetas.cue_ahorros = cuentaAhorros:SCREEN-VALUE.

            MESSAGE "El Cupo ha sido actualizado"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
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


&Scoped-define SELF-NAME chkReportaTopeIndividual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL chkReportaTopeIndividual C-Win
ON VALUE-CHANGED OF chkReportaTopeIndividual IN FRAME Frm_Clientes
DO:
    ASSIGN chkReportaTopeIndividual.

    IF chkReportaTopeIndividual = TRUE THEN DO:
        operacionesCajero:SENSITIVE = TRUE.
        operacionesPOS:SENSITIVE = TRUE.
        montoCajero:SENSITIVE = TRUE.
        montoPOS:SENSITIVE = TRUE.
    END.
    ELSE DO:
        operacionesCajero:SENSITIVE = FALSE.
        operacionesPOS:SENSITIVE = FALSE.
        montoCajero:SENSITIVE = FALSE.
        montoPOS:SENSITIVE = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_agencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_agencia C-Win
ON VALUE-CHANGED OF Cmb_agencia IN FRAME Frm_Clientes /* Agencia */
DO:
  ASSIGN cmb_agencia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME montoCajero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL montoCajero C-Win
ON LEAVE OF montoCajero IN FRAME Frm_Clientes /* Monto Máximo Cajero */
DO:
    ASSIGN wnewcupo.

    IF wnewcupo GE 0 THEN
        btn_procesar:SENSITIVE = TRUE.
    ELSE DO:
        MESSAGE "No puede asignar un cupo inferior a su saldo de capital actual"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        btn_procesar:SENSITIVE = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME montoPOS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL montoPOS C-Win
ON LEAVE OF montoPOS IN FRAME Frm_Clientes /* Monto Máximo POS */
DO:
    ASSIGN wnewcupo.

    IF wnewcupo GE 0 THEN
        btn_procesar:SENSITIVE = TRUE.
    ELSE DO:
        MESSAGE "No puede asignar un cupo inferior a su saldo de capital actual"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        btn_procesar:SENSITIVE = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nit C-Win
ON LEAVE OF Clientes.Nit IN FRAME Frm_Clientes /* Nit */
DO:
    RUN inicializar_variables.

    DO WITH FRAME F_Clientes:
        FIND FIRST Clientes WHERE Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE NO-ERROR.
        IF AVAILABLE(Clientes) THEN
            W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
        ELSE DO:
            RUN C-Clientes.R(INPUT 2,
                             INPUT W_Agencia,
                             OUTPUT P_Nit,
                             OUTPUT P_Nombre,
                             OUTPUT P_Apellido,
                             OUTPUT P_AgeCli).

            ASSIGN Clientes.Nit:SCREEN-VALUE = P_Nit
                   W_Nomtitular:SCREEN-VALUE = CAPS(TRIM(P_Nombre) + " " + P_Apellido).

            FIND FIRST Clientes WHERE Clientes.Nit EQ P_Nit NO-ERROR.
        END.

        IF AVAILABLE clientes THEN DO:
            FIND FIRST listaNegra WHERE listaNegra.nit = clientes.nit
                                    AND listaNegra.fec_exclusion > w_fecha
                                    AND listaNegra.estado = 1 NO-LOCK NO-ERROR.
            IF AVAILABLE listaNegra THEN DO:
                MESSAGE "Este Asociado se encuentra suspendido." SKIP
                        "No se permite la operación."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                Clientes.Nit:SCREEN-VALUE = "".

                RETURN.
            END.

            wFecPagare:SCREEN-VALUE = STRING(clientes.fecPagare,"99/99/9999").

            FIND FIRST agencias WHERE agencias.agencia = clientes.agencia NO-LOCK NO-ERROR.
            IF AVAILABLE agencias THEN
                Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre.

            IF wnewcupo GE 0 THEN
                btn_procesar:SENSITIVE = TRUE.
        END.

        FIND FIRST cfg_tarjetaDB NO-LOCK NO-ERROR.
        IF AVAILABLE cfg_tarjetaDB THEN DO:
            operacionesCajero:SCREEN-VALUE = STRING(cfg_tarjetaDB.numeroRetirosCajeroDia).
            operacionesPOS:SCREEN-VALUE = STRING(cfg_tarjetaDB.numeroRetirosPOSDia).
            montoCajero:SCREEN-VALUE = STRING(cfg_tarjetaDB.montoRetirosCajeroDia).
            montoPOS:SCREEN-VALUE = STRING(cfg_tarjetaDB.montoRetirosPOSDia).
            wNewCupo:SCREEN-VALUE = STRING(cfg_tarjetaDB.montoCupo).
        END.
        ELSE DO:
            wTarjetaDB:SCREEN-VALUE = ''.
            operacionesCajero:SCREEN-VALUE = ''.
            operacionesPOS:SCREEN-VALUE = ''.
            montoCajero:SCREEN-VALUE = ''.
            montoPOS:SCREEN-VALUE = ''.
            wNewCupo:SCREEN-VALUE = ''.
        END.

        FIND FIRST creditos WHERE creditos.nit = clientes.nit:SCREEN-VALUE
                              AND creditos.cod_credito = 123
                              AND creditos.estado = 2 NO-ERROR.
        IF AVAILABLE creditos THEN DO:
            ASSIGN numeroCupo:SCREEN-VALUE = STRING(creditos.num_credito)
                   wsdo_disponible:SCREEN-VALUE = string(creditos.sdo_capital,"->>>,>>>,>>9")
                   wsdo_canje:SCREEN-VALUE = string(creditos.monto,"->>>,>>>,>>9")
                   wnewCupo:SCREEN-VALUE = string(creditos.monto,"->>>,>>>,>>9").

            FIND FIRST tarjetas WHERE tarjetas.nit = creditos.nit
                                  AND tarjetas.num_credito = creditos.num_credito
                                  AND tarjetas.estado = '01' NO-LOCK NO-ERROR.
            IF AVAILABLE tarjetas THEN DO:
                wTarjetaDB:SCREEN-VALUE = tarjetas.tarjetaDB.
                operacionesCajero:SCREEN-VALUE = STRING(tarjetas.operMaxCaj).
                operacionesPOS:SCREEN-VALUE = STRING(tarjetas.operMaxPOS).
                montoCajero:SCREEN-VALUE = STRING(tarjetas.montoMaxCaj).
                montoPOS:SCREEN-VALUE = STRING(tarjetas.montoMaxPOS).
                chkReportaTopeIndividual:SCREEN-VALUE = STRING(tarjetas.reportaTopesIndividual).

                wTarjetaDB:READ-ONLY = TRUE.
            END.
            ELSE DO:
                IF AVAILABLE cfg_tarjetaDB THEN DO:
                    operacionesCajero:SCREEN-VALUE = STRING(cfg_tarjetaDB.numeroRetirosCajeroDia).
                    operacionesPOS:SCREEN-VALUE = STRING(cfg_tarjetaDB.numeroRetirosPOSDia).
                    montoCajero:SCREEN-VALUE = STRING(cfg_tarjetaDB.montoRetirosCajeroDia).
                    montoPOS:SCREEN-VALUE = STRING(cfg_tarjetaDB.montoRetirosPOSDia).
                    wNewCupo:SCREEN-VALUE = STRING(cfg_tarjetaDb.montoCupo).
                END.
                ELSE DO:
                    operacionesCajero:SCREEN-VALUE = ''.
                    operacionesPOS:SCREEN-VALUE = ''.
                    montoCajero:SCREEN-VALUE = ''.
                    montoPOS:SCREEN-VALUE = ''.
                    wNewCupo:SCREEN-VALUE = ''.
                END.

                chkReportaTopeIndividual:SCREEN-VALUE = "no".
            END.
        END.
        ELSE DO:
            ASSIGN wnewcupo:SENSITIVE = TRUE.
            wTarjetaDB:READ-ONLY = FALSE.
            APPLY "Entry" TO wnewcupo.
        END.

        FIND FIRST ahorros WHERE ahorros.nit = clientes.nit:SCREEN-VALUE
                             AND ahorros.cod_ahorro = 4
                             AND ahorros.estado = 1 NO-ERROR.
        IF AVAILABLE ahorros THEN
            ASSIGN cuentaAhorros:SCREEN-VALUE = ahorros.cue_ahorros
                   sdoDisponibleAh:SCREEN-VALUE = string(ahorros.sdo_disponible,"->>>,>>>,>>9").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME operacionesCajero
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL operacionesCajero C-Win
ON LEAVE OF operacionesCajero IN FRAME Frm_Clientes /* # Operaciones Cajero */
DO:
    ASSIGN wnewcupo.

    IF wnewcupo GE 0 THEN
        btn_procesar:SENSITIVE = TRUE.
    ELSE DO:
        MESSAGE "No puede asignar un cupo inferior a su saldo de capital actual"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        btn_procesar:SENSITIVE = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME operacionesPOS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL operacionesPOS C-Win
ON LEAVE OF operacionesPOS IN FRAME Frm_Clientes /* # Operaciones POS */
DO:
    ASSIGN wnewcupo.

    IF wnewcupo GE 0 THEN
        btn_procesar:SENSITIVE = TRUE.
    ELSE DO:
        MESSAGE "No puede asignar un cupo inferior a su saldo de capital actual"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        btn_procesar:SENSITIVE = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wnewcupo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wnewcupo C-Win
ON LEAVE OF wnewcupo IN FRAME Frm_Clientes /* Cupo a aprobar */
DO:
    ASSIGN wnewcupo.

    IF wnewcupo GE 0 THEN
        btn_procesar:SENSITIVE = TRUE.
    ELSE DO:
        MESSAGE "No puede asignar un cupo inferior a su saldo de capital actual"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        btn_procesar:SENSITIVE = FALSE.
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
  RUN local-initialize.
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
  DISPLAY wFecPagare chkReportaTopeIndividual numeroCupo wSdo_Canje 
          wSdo_Disponible wTarjetaDB Cmb_agencia wnewcupo W_NomTitular 
          operacionesCajero montoCajero operacionesPOS montoPOS cuentaAhorros 
          sdoDisponibleAh 
      WITH FRAME Frm_Clientes IN WINDOW C-Win.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Nit 
      WITH FRAME Frm_Clientes IN WINDOW C-Win.
  ENABLE wFecPagare chkReportaTopeIndividual Clientes.Nit numeroCupo wSdo_Canje 
         wSdo_Disponible wTarjetaDB wnewcupo btn_procesar Btn_Cancelar BtnDone 
         Btn_Consulta BUTTON-1 cuentaAhorros sdoDisponibleAh ECT-309 RECT-274 
      WITH FRAME Frm_Clientes IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm_Clientes}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
ASSIGN /*Clientes.Nit:SCREEN-VALUE IN FRAME FRM_Clientes   = ""*/.
       btn_procesar:SENSITIVE IN FRAME FRM_Clientes = FALSE.

ASSIGN w_nomtitular:SCREEN-VALUE = ''
       wnewcupo:SCREEN-VALUE = ''
       numeroCupo:SCREEN-VALUE = ''
       wsdo_disponible:SCREEN-VALUE = ''
       wsdo_canje:SCREEN-VALUE = ''
       cuentaAhorros:SCREEN-VALUE = ''
       sdoDisponibleAh:SCREEN-VALUE = ''
       wtarjetaDB:SCREEN-VALUE = ''.

operacionesCajero:SCREEN-VALUE = ''.
operacionesPOS:SCREEN-VALUE = ''.
montoCajero:SCREEN-VALUE = ''.
montoPOS:SCREEN-VALUE = ''.

wnewcupo:SCREEN-VALUE = ''.

ASSIGN wnewCupo.

operacionesCajero:SENSITIVE = FALSE.
operacionesPOS:SENSITIVE = FALSE.
montoCajero:SENSITIVE = FALSE.
montoPOS:SENSITIVE = FALSE.

chkReportaTopeIndividual:SCREEN-VALUE = "no".
chkReportaTopeIndividual = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize C-Win 
PROCEDURE local-initialize :
ASSIGN Cmb_Agencia:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".

FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK BY Agencia.Agencia:
    W_OK = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre).

    IF W_Agencia EQ Agencias.Agencia THEN
        Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre.
END.

wnewcupo:SCREEN-VALUE = ''.

ASSIGN wnewCupo.

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

