&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE INPUT PARAMETER P_NomIns AS CHARACTER FORMAT "X(50)".
DEFINE INPUT PARAMETER P_NitCli AS CHARACTER.
DEFINE INPUT PARAMETER P_CodCre AS INTEGER.
DEFINE INPUT PARAMETER P_TipCre AS INTEGER.
DEFINE INPUT PARAMETER P_NumCre AS INTEGER.
DEFINE INPUT PARAMETER P_CodOpe AS INTEGER.
DEFINE INPUT PARAMETER P_Progra AS INTEGER FORMAT "9".

DEFINE VAR Pagas AS INTEGER.
DEFINE VAR FecPP AS DATE.
DEFINE VAR W_PdoPag AS INTEGER FORMAT "99".

DEFINE TEMP-TABLE TPlanP LIKE PlanPagos.
DEFINE TEMP-TABLE CPlanP LIKE PlanPagos.
DEFINE TEMP-TABLE TExtras LIKE Extras.

DEFINE TEMP-TABLE TProIns
    FIELD TP_Agencia AS INTEGER
    FIELD TP_Orden AS INTEGER
    FIELD TP_Instancia AS INTEGER
    FIELD TP_NomInstan AS CHARACTER FORMAT "X(30)"
    FIELD TP_Usuario AS CHARACTER
    FIELD TP_NomUsuar AS CHARACTER FORMAT "X(30)"
    FIELD TP_Cantidad AS INTEGER FORMAT "999".

DEFINE TEMP-TABLE TUXI
    FIELD Agencia AS INTEGER
    FIELD Usuario AS CHARACTER
    FIELD Nombre AS CHARACTER
    FIELD Cantidad AS INTEGER FORMAT "999"
    FIELD Proceso AS LOGICAL.

/* oakley */
    
/*para guardar las instancias de una solicitud*/
  DEFINE TEMP-TABLE TCerradas
    FIELD Instancia      LIKE Mov_Instancias.Instancia
    FIELD INom_Instancia AS CHARACTER FORMAT "X(20)"
    FIELD Fec_Ingreso    LIKE Mov_Instancias.Fec_Ingreso
    FIELD Fec_Retiro     LIKE Mov_Instancias.Fec_Retiro
    FIELD Hora_Ingreso   LIKE Mov_Instancias.Hora_Ingreso
    FIELD Hora_Retiro    LIKE Mov_Instancias.Hora_Retiro
    FIELD Estado         LIKE Mov_Instancias.Estado
    FIELD Num_Solicitud   LIKE Mov_Instancias.Num_Solicitud
    FIELD Usuario        LIKE Mov_Instancias.Usuario
    FIELD INom_Usuario   AS CHARACTER FORMAT "X(30)"
    FIELD Descripcion    LIKE Mov_Instancias.Descripcion.
  
  
  DEFINE VARIABLE i AS INTEGER.
  DEFINE VARIABLE W_Ok AS LOGICAL.
  DEFINE VARIABLE W_TipoProducto LIKE Pro_Creditos.Tip_Credito.
  DEFINE VARIABLE Dias      AS DECIMAL.
  DEFI   VAR      W_PdoAno  AS INTEG FORM "99".
  
  DEFINE TEMP-TABLE TIns    LIKE Cfg_Instancias.
  
  DEFINE TEMP-TABLE TScoring
      FIELD CodS LIKE Scoring.Codigo
      FIELD TabS LIKE Pro_Scoring.Tabla
      FIELD VarS LIKE Scoring.VARIABLE
      FIELD VVaS LIKE Scoring.Valor_Variable
      FIELD PunS LIKE Scoring.Puntaje
      FIELD FecS LIKE Scoring.Fec_Scoring.
      
 DEFINE TEMP-TABLE Consulta
      FIELD Num_Credito   LIKE Creditos.Num_Credito
      FIELD Num_Solicitud LIKE Creditos.Num_Solicitud
      FIELD AgeCredito    LIKE Agencias.Agencia
      FIELD Nit           LIKE Clientes.Nit
      FIELD Estado        LIKE Creditos.Estado
      FIELD Nombre        AS CHARACTER FORMAT "X(40)"
      FIELD Fec_Ingreso   LIKE Mov_Instancias.Fec_Ingreso
      FIELD Hor_Ingreso   AS CHARACTER FORMAT "X(15)"
      FIELD Monto         LIKE Solicitud.Monto
      FIELD Vigencia      AS INTEGER FORMAT "9999".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Cre
&Scoped-define BROWSE-NAME Br_Extras

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TExtras

/* Definitions for BROWSE Br_Extras                                     */
&Scoped-define FIELDS-IN-QUERY-Br_Extras TExtras.Agencia TExtras.Cod_Credito TExtras.Nit TExtras.Num_Solicitud TExtras.Nro_Cuota TExtras.Vr_CuoExtra TExtras.Fec_Vcto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Extras   
&Scoped-define SELF-NAME Br_Extras
&Scoped-define QUERY-STRING-Br_Extras FOR EACH TExtras WHERE TExtras.Nit EQ Creditos.Nit                                         AND  TExtras.Num_Solicitud EQ Creditos.Num_solicitud NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Extras OPEN QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit EQ Creditos.Nit                                         AND  TExtras.Num_Solicitud EQ Creditos.Num_solicitud NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Extras TExtras
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Extras TExtras


/* Definitions for FRAME F_Extras                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Extras ~
    ~{&OPEN-QUERY-Br_Extras}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Creditos.Per_Pago Creditos.For_Pago ~
Creditos.Reestructurado Creditos.Abogado 
&Scoped-define ENABLED-TABLES Creditos
&Scoped-define FIRST-ENABLED-TABLE Creditos
&Scoped-Define ENABLED-OBJECTS RECT-296 RECT-321 RECT-322 RECT-333 RECT-325 ~
RECT-326 RECT-335 Btn_Acepta_Terminos W_NvoPlazo Rs_SistAmort ~
txtFechaDesembolso W_FecIni Btn_Salir W_NvaCuota W_NvaTasa txtFechaProxPago ~
Btn_Extras 
&Scoped-Define DISPLAYED-FIELDS Creditos.Fec_Desembolso ~
Creditos.Fec_Reestructurado Creditos.Fec_Pago Creditos.Provision ~
Creditos.Fec_UltPago Creditos.Cuo_Atraso Creditos.Dias_Atraso ~
Creditos.Val_Atraso Creditos.Int_DifCobro Creditos.Cuo_Pagadas ~
Creditos.Int_Corrientes Creditos.Sdo_Capital Creditos.Sdo_Proyectado ~
Creditos.Plazo Creditos.Cuota Creditos.Tasa Creditos.Per_Pago ~
Creditos.For_Pago Creditos.Reestructurado Creditos.Abogado 
&Scoped-define DISPLAYED-TABLES Creditos
&Scoped-define FIRST-DISPLAYED-TABLE Creditos
&Scoped-Define DISPLAYED-OBJECTS W_PerPag W_CuoFaltan W_SdoOtros W_IntMora ~
W_NvoPlazo Rs_SistAmort txtFechaDesembolso W_FecIni W_NvaCuota W_NvaTasa ~
txtFechaProxPago W_TotExt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Acepta_Terminos 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "Aceptar Nuevos Términos" 
     SIZE 8 BY 1.88.

DEFINE BUTTON Btn_Extras 
     LABEL "Extras" 
     SIZE 5.43 BY 1.08.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "Button 176" 
     SIZE 8 BY 1.88.

DEFINE VARIABLE txtFechaDesembolso AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha desembolso" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .85
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE txtFechaProxPago AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha prox. pago" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .85
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CuoFaltan AS DECIMAL FORMAT "9999":U INITIAL 0 
     LABEL "Cuotas X Pagar" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha de inicio" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .85
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_IntMora AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>9.99" INITIAL 0 
     LABEL "Interès Moratorio" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15 .

DEFINE VARIABLE W_NvaCuota AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Nueva Cuota" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .92
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NvaTasa AS DECIMAL FORMAT ">>9.9999999":U INITIAL 0 
     LABEL "Nueva Tasa" 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .81 TOOLTIP "Nueva Tasa para el Crédito"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NvoPlazo AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Nuevo Plazo" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .85
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_PerPag AS CHARACTER FORMAT "X(15)":U 
     LABEL "Pdo de Pago" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_SdoOtros AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Sdo.Cobro Jurídico" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotExt AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Extras" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_SistAmort AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Cuota Fija", 1,
"Cuota Unica", 2
     SIZE 12 BY 2.69 NO-UNDO.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89.57 BY 8.08.

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.14 BY 3.5.

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 4.35.

DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.86 BY 3.5.

DEFINE RECTANGLE RECT-326
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.14 BY 3.5.

DEFINE RECTANGLE RECT-333
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14 BY 3.5.

DEFINE RECTANGLE RECT-335
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.14 BY 7.81.

DEFINE BUTTON Btn_AdExt 
     LABEL "&Salvar Extra" 
     SIZE 15.43 BY 1.12.

DEFINE BUTTON Btn_EliExt 
     LABEL "&Eliminar Extra" 
     SIZE 15.43 BY 1.12.

DEFINE BUTTON BUTTON-170 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 170" 
     SIZE 10.57 BY 1.62 TOOLTIP "Retorna a la Ventana Principal".

DEFINE VARIABLE W_PPExtra AS INTEGER FORMAT ">>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrExtra AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.86 BY 1
     BGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Extras FOR 
      TExtras SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Extras Wwin _FREEFORM
  QUERY Br_Extras NO-LOCK DISPLAY
      TExtras.Agencia FORMAT "999":U                        COLUMN-LABEL "Ag."
      TExtras.Cod_Credito FORMAT "999":U                    COLUMN-LABEL "Pdcto"
      TExtras.Nit FORMAT "X(12)":U                          COLUMN-LABEL "Ced./Nit"
      TExtras.Num_Solicitud FORMAT "99999999":U             COLUMN-LABEL "No.Solicitud"
      TExtras.Nro_Cuota FORMAT "9999":U                     COLUMN-LABEL "Pdo.Pago"
      TExtras.Vr_CuoExtra FORMAT "->>>>,>>>,>>9.99":U       COLUMN-LABEL "Vr.Cuota Extra"
      TExtras.Fec_Vcto FORMAT "99/99/9999":U                COLUMN-LABEL "Fecha-Vcto"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 70.72 BY 7.88
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN TOOLTIP "Con CLICK selecciona la Extra para ser Modificada y/o Eliminarla".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Extras
     Btn_AdExt AT ROW 2.23 COL 40
     W_PPExtra AT ROW 2.27 COL 3.72 COLON-ALIGNED NO-LABEL
     W_VrExtra AT ROW 2.27 COL 17.43 COLON-ALIGNED NO-LABEL
     Btn_EliExt AT ROW 3.65 COL 40.14
     BUTTON-170 AT ROW 4.73 COL 58.86
     Br_Extras AT ROW 6.46 COL 1.72
     "Pdo.Pago         Valor Cuota Extra" VIEW-AS TEXT
          SIZE 35 BY .62 AT ROW 1.38 COL 4.72
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 2.14 ROW 1
         SIZE 72.57 BY 14.62
         BGCOLOR 17 
         TITLE "Extras Futuras Pactadas".

DEFINE FRAME F_Cre
     Creditos.Fec_Desembolso AT ROW 2.08 COL 14 COLON-ALIGNED
          LABEL "Fecha Desembolso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Reestructurado AT ROW 2.08 COL 43.72 COLON-ALIGNED
          LABEL "Fec-Reestructurado"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_PerPag AT ROW 2.08 COL 71 COLON-ALIGNED
     Creditos.Fec_Pago AT ROW 2.88 COL 14 COLON-ALIGNED
          LABEL "Fec.Próximo Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Provision AT ROW 2.88 COL 43.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_CuoFaltan AT ROW 2.88 COL 71 COLON-ALIGNED
     Creditos.Fec_UltPago AT ROW 3.69 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuo_Atraso AT ROW 3.69 COL 43.72 COLON-ALIGNED
          LABEL "Cuotas_Vencidas"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     W_SdoOtros AT ROW 4.5 COL 14 COLON-ALIGNED
     Creditos.Dias_Atraso AT ROW 4.5 COL 43.72 COLON-ALIGNED
          LABEL "Dias_Vencidos" FORMAT "-99999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     W_IntMora AT ROW 5.31 COL 14 COLON-ALIGNED
     Creditos.Val_Atraso AT ROW 5.31 COL 43.86 COLON-ALIGNED
          LABEL "Capital_Vencido" FORMAT "->,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Int_DifCobro AT ROW 6.12 COL 14 COLON-ALIGNED
          LABEL "Interés Contingente"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuo_Pagadas AT ROW 6.12 COL 43.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Corrientes AT ROW 6.92 COL 13.86 COLON-ALIGNED
          LABEL "Interés Corriente"
          VIEW-AS FILL-IN 
          SIZE 11.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Capital AT ROW 6.92 COL 43.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Sdo_Proyectado AT ROW 7.73 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Plazo AT ROW 7.73 COL 43.86 COLON-ALIGNED
          LABEL "Plazo Actual"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuota AT ROW 8.54 COL 14 COLON-ALIGNED
          LABEL "Cuota Actual"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Tasa AT ROW 8.54 COL 44 COLON-ALIGNED
          LABEL "T.Nominal Anual"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Acepta_Terminos AT ROW 11.19 COL 82
     Creditos.Per_Pago AT ROW 11.46 COL 3.29 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Semanal", 1,
"Decadal", 2,
"Quincenal", 3,
"Mensual", 4,
"Trimestral", 6,
"Semestral", 8,
"Anual", 9
          SIZE 10.72 BY 3.81
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.86 BY 19.88
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Cre
     W_NvoPlazo AT ROW 11.46 COL 65.86 COLON-ALIGNED
     Creditos.For_Pago AT ROW 11.73 COL 20.86 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Caja", 1,
"Nómina", 2,
"Déb.Automático", 3
          SIZE 15.14 BY 2.69
     Rs_SistAmort AT ROW 11.73 COL 40 NO-LABEL
     txtFechaDesembolso AT ROW 12.35 COL 65.86 COLON-ALIGNED WIDGET-ID 30
     W_FecIni AT ROW 13.23 COL 65.86 COLON-ALIGNED
     Btn_Salir AT ROW 13.35 COL 82
     W_NvaCuota AT ROW 14.12 COL 65.86 COLON-ALIGNED
     W_NvaTasa AT ROW 15.08 COL 66 COLON-ALIGNED
     txtFechaProxPago AT ROW 15.92 COL 65.86 COLON-ALIGNED WIDGET-ID 36
     Creditos.Reestructurado AT ROW 16.81 COL 21.14 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "No-Aplica", 0,
"Reestructurado", 1,
"No-Reestructurado", 2
          SIZE 16 BY 2.42
     Creditos.Abogado AT ROW 16.85 COL 3.29 HELP
          "" NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Abogado", yes,
"No-Abogado", no
          SIZE 12 BY 2.42
     Btn_Extras AT ROW 17.73 COL 75 WIDGET-ID 32
     W_TotExt AT ROW 17.96 COL 61.29 COLON-ALIGNED WIDGET-ID 34
     "Información del Crédito" VIEW-AS TEXT
          SIZE 19.57 BY .77 AT ROW 1.19 COL 2.14
          FGCOLOR 7 FONT 5
     "Reestructurado" VIEW-AS TEXT
          SIZE 11.14 BY .5 AT ROW 16 COL 21 WIDGET-ID 20
          BGCOLOR 18 
     "Amortización" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 10.92 COL 40 WIDGET-ID 12
          BGCOLOR 18 
     "Periodicidad de Pago" VIEW-AS TEXT
          SIZE 15.14 BY .5 AT ROW 10.92 COL 3 WIDGET-ID 8
          BGCOLOR 18 
     "Abogado" VIEW-AS TEXT
          SIZE 8.14 BY .5 AT ROW 16.04 COL 3.14 WIDGET-ID 16
          BGCOLOR 18 
     "Otras condiciones" VIEW-AS TEXT
          SIZE 13.14 BY .5 AT ROW 10.92 COL 54.86 WIDGET-ID 28
          BGCOLOR 18 
     "Forma de pago" VIEW-AS TEXT
          SIZE 11.14 BY .5 AT ROW 10.92 COL 20.86 WIDGET-ID 4
          BGCOLOR 18 
     RECT-296 AT ROW 1.58 COL 1.43
     RECT-321 AT ROW 11.19 COL 19.86 WIDGET-ID 2
     RECT-322 AT ROW 11.19 COL 2 WIDGET-ID 6
     RECT-333 AT ROW 11.19 COL 39 WIDGET-ID 10
     RECT-325 AT ROW 16.31 COL 2.14 WIDGET-ID 14
     RECT-326 AT ROW 16.27 COL 20 WIDGET-ID 18
     RECT-335 AT ROW 11.19 COL 53.86 WIDGET-ID 26
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.86 BY 19.88
         BGCOLOR 17 FONT 4
         TITLE "".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW Wwin ASSIGN
         HIDDEN             = YES
         TITLE              = "Refinanciación Créditos, Programa W-ProRec_Refinancia.W"
         COLUMN             = 78.14
         ROW                = 10.23
         HEIGHT             = 19.88
         WIDTH              = 90.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Wwin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW Wwin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Cre
   FRAME-NAME                                                           */
/* SETTINGS FOR RADIO-SET Creditos.Abogado IN FRAME F_Cre
   EXP-HELP                                                             */
/* SETTINGS FOR FILL-IN Creditos.Cuota IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Pagadas IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Dias_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Fec_Desembolso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Pago IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Reestructurado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_Corrientes IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_DifCobro IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Plazo IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Provision IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Capital IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Proyectado IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Tasa IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Val_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN W_CuoFaltan IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_IntMora IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_PerPag IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_SdoOtros IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TotExt IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Extras
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Extras BUTTON-170 F_Extras */
ASSIGN 
       FRAME F_Extras:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Extras
/* Query rebuild information for BROWSE Br_Extras
     _START_FREEFORM
OPEN QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit EQ Creditos.Nit
                                        AND  TExtras.Num_Solicitud EQ Creditos.Num_solicitud NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Extras */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cre
/* Query rebuild information for FRAME F_Cre
     _Query            is NOT OPENED
*/  /* FRAME F_Cre */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Extras
/* Query rebuild information for FRAME F_Extras
     _Query            is NOT OPENED
*/  /* FRAME F_Extras */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON END-ERROR OF Wwin /* Refinanciación Créditos, Programa W-ProRec_Refinancia.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON WINDOW-CLOSE OF Wwin /* Refinanciación Créditos, Programa W-ProRec_Refinancia.W */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Extras
&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME Br_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Extras Wwin
ON MOUSE-SELECT-CLICK OF Br_Extras IN FRAME F_Extras
DO:
   IF AVAIL(TExtras) THEN
      ASSIGN W_PPExtra              = TExtras.Nro_Cuota 
             W_PPExtra:SCREEN-VALUE = STRING(TExtras.Nro_Cuota)
             W_VrExtra              = TExtras.Vr_CuoExtra   
             W_VrExtra:SCREEN-VALUE = STRING(TExtras.Vr_CuoExtra).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME Btn_Acepta_Terminos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Acepta_Terminos Wwin
ON CHOOSE OF Btn_Acepta_Terminos IN FRAME F_Cre /* Aceptar Nuevos Términos */
DO:
    DEFI VAR NomOp AS CHAR FORM "X(20)".
    DEFI VAR NomReest AS CHAR FORM "X(20)" INIT "No - ReEstructurado".
    DEFI VAR NomReIn  AS CHAR FORM "X(15)" INIT "No - ReIniciar".
    DEFI VAR W_CuoYaPag LIKE Creditos.Cuo_Pagadas.
    DEFI VAR W_RowIdCr AS ROWID.
    DEFI VAR W_TasaUs LIKE Indicadores.Tasa.
    DEFINE VAR flagCambioCuota AS LOGICAL.
    DEFINE VAR flagCambiaFechaPago AS LOGICAL.

    ASSIGN W_NvaTasa
           W_FecIni.

    FIND CURRENT Creditos NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "El Crèdito no puede ser actualizado, Està en Bloqueo..." SKIP
                "              Intente nuevamente."
            VIEW-AS ALERT-BOX.

        APPLY "choose" TO Btn_Salir.
    END.

    IF creditos.cod_credito <> 123 THEN DO:
        IF W_FecIni LT Creditos.Fec_Desemb THEN DO:
            MESSAGE "Revise por favor, La nueva fecha de inicio es anterior a la fecha de Desembolso..." SKIP
                    "Será Modificada la de Desembolso..." SKIP
                    "Está seguro(a) de modificar la fecha de Inicio del Plan de Pagos...?" SKIP
                    "Por nueva fecha : " W_FecIni
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Fec-Inicio" UPDATE W_RptaFI AS LOGICAL.

            IF NOT W_RptaFI THEN
                RETURN.
        END.

        IF creditos.cod_credito <> 123 THEN DO:
            IF DECIMAL(W_NvoPlazo:SCREEN-VALUE IN FRAME F_Cre) LE 0 OR DECIMAL(W_NvaCuota:SCREEN-VALUE IN FRAME F_Cre) LE 0 OR W_NvaCuota:SCREEN-VALUE IN FRAME F_Cre EQ "?" THEN DO:
                MESSAGE "El plazo o la cuota están en 0." SKIP
                        "No se aceptan los nuevos términos..."
                    VIEW-AS ALERT-BOX.

                APPLY "Entry" TO W_NvoPlazo IN FRAME F_Cre.
                RETURN.
            END.
        END.

        FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
        FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_Usura
                                 AND Indicadores.Estado EQ 1
                                 AND Indicadores.FecVcto GE W_Fecha NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Indicadores) THEN DO:
            MESSAGE "Falta Indicador de Usura Vigente, No se permite la Modificaciòn."
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.

        W_TasaUs = Indicadores.Tasa.

        RUN EFNV IN W_ManFin  (INPUT W_TasaUs / 100,
                               INPUT 12,
                               OUTPUT W_TasaUS).

        W_TasaUS = W_TasaUS * 1200.

        ASSIGN W_CuoFaltan.

        MESSAGE "El Crèdito lo Liquidò con la siguiente OPCIÓN: " NomOp SKIP
                "                                                          " NomReIn SKIP
                "Plazo del Crèdito : " Creditos.Plazo    "            Plazo Nuevo : " W_NvoPlazo   SKIP
                "Cuota del Crèdito : " Creditos.Cuota    "            Cuota Nueva : " W_NvaCuota   SKIP
                "Tasa del Crèdito  : " Creditos.Tasa    "              Tasa Nueva : " W_NvaTasa    SKIP
                "Crèdito quedarà marcado como : " NomReest  "     En Abogado : " Creditos.Abogado:SCREEN-VALUE SKIP
                "                    Està Segura(o) de Salvar la Modificaciòn...?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Refinanciaciòn" UPDATE W_RptaConf AS LOGICAL.

        IF NOT W_RptaConf THEN
            RETURN.

        ASSIGN txtFechaDesembolso.

        ASSIGN Creditos.Per_Pago = INTEG(Creditos.Per_Pago:SCREEN-VALUE)
               Creditos.Sistema = INTEGER(Rs_SistAmort:SCREEN-VALUE)
               Creditos.For_Pago = INTEG(Creditos.For_Pago:SCREEN-VALUE)
               Creditos.Reestructurado = INTEG(Creditos.Reestructurado:SCREEN-VALUE)
               Creditos.Abogado = FALSE
               Creditos.Tasa = W_NvaTasa
               creditos.fec_desembolso = txtFechaDesembolso
               Creditos.Fec_PagAnti = W_FecIni.

        IF W_FecIni LT Creditos.Fec_Desemb THEN
            Creditos.Fec_Desemb = W_FecIni.

        FOR EACH extras WHERE extras.Agencia = creditos.agencia
                          AND extras.Cod_Credito = creditos.cod_credito
                          AND extras.Nit = creditos.nit
                          AND extras.Num_Solicitud = creditos.num_solicitud:
            DELETE Extras.
        END.

        FOR EACH TExtras:
            CREATE extras.
            BUFFER-COPY TExtras TO extras.
        END.

        FOR EACH extras WHERE extras.Agencia = creditos.agencia
                          AND extras.Cod_Credito = creditos.cod_credito
                          AND extras.Nit = creditos.nit
                          AND extras.Num_Solicitud = creditos.num_solicitud:
            FIND FIRST CONTROL_pagos WHERE CONTROL_pagos.nit = extras.nit
                                       AND CONTROL_pagos.num_credito = creditos.num_credito
                                       AND CONTROL_pagos.nro_cuota = extras.nro_cuota NO-LOCK NO-ERROR.
            IF AVAILABLE CONTROL_pagos THEN
                extras.fec_Vcto = CONTROL_pagos.fec_Vcto.
        END.

        Creditos.Plazo = INTEGER(W_NvoPlazo:SCREEN-VALUE).

        IF creditos.cuota <> DECIMAL(W_NvaCuota:SCREEN-VALUE) THEN DO:
            Creditos.Cuota = DECIMAL(W_NvaCuota:SCREEN-VALUE).

            RUN CrearControlPagos.r(INPUT creditos.nit,
                                    INPUT creditos.num_credito,
                                    INPUT creditos.tasa).
        END.
        ELSE
            RUN crearControlPagosRefinancia.r(INPUT creditos.nit,
                                              INPUT creditos.num_credito,
                                              INPUT creditos.tasa).
    END.
    ELSE DO:
        MESSAGE "Para los créditos rotativos únicamente será efectivo" SKIP
                "el cambio de fecha de próximo pago, marcación como" SKIP
                "restructurado y marcación de crédito en abogado. Todas" SKIP
                "las demás modificaciones serán descartadas."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    IF INTEG(Creditos.Reestructurado:SCREEN-VALUE) EQ 1 THEN
        Creditos.Fec_Reestructurado = W_Fecha.

    IF Creditos.Abogado:SCREEN-VALUE EQ "Yes" THEN
        Creditos.Abogado = TRUE.

    ASSIGN txtFechaProxPago.

    IF txtFechaProxPago <> creditos.fec_pago THEN DO:
        creditos.fec_pago = txtFechaProxPago.
        RUN ActualizaFechaPago.
    END.

    RUN Mov_Credito.
    
    FIND CURRENT Creditos NO-LOCK NO-ERROR.
    
    RUN Imprimir.

    APPLY "choose" TO Btn_Salir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME Btn_AdExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AdExt Wwin
ON CHOOSE OF Btn_AdExt IN FRAME F_Extras /* Salvar Extra */
DO:
    DEFI VAR W_NroDias AS INTEG FORM "99".
    DEFI VAR JJ AS INTEG FORM "99999".
    DEFI VAR W_FecTra AS DATE.
    DEFI VAR W_FecIni AS DATE.

    W_NroDias = 30.

    IF Creditos.Per_Pago EQ 1 THEN
        W_NroDias = 7.
    ELSE
        IF Creditos.Per_Pago EQ 2 THEN
            W_NroDias = 10.
        
    IF Creditos.Per_Pago EQ 3 THEN
        W_NroDias = 15.

    IF W_PPExtra LE 0 OR W_PPExtra GT Creditos.Plazo THEN DO:
        MESSAGE "El Perìodo de Pago de la Cuota Extra debe ser Mayor que Cero y," SKIP
                "Menor o Igual al Plazo Total...No se Acepta la Operaciòn."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    IF W_VrExtra LE 0 OR W_VrExtra GE Creditos.Sdo_Capital THEN DO:
        MESSAGE "El Valor de la Cuota Extra debe ser Mayor que Cero y," SKIP
                "Menor al Saldo de Capital...No se Acepta la Operaciòn."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    W_TotExt = W_VrExtra.

    FOR EACH TExtras WHERE TExtras.Nit EQ Creditos.Nit
                       AND TExtras.Num_Solicitud EQ Creditos.Num_solicitud:
        IF TExtras.Estado EQ 1 AND TExtras.Nro_Cuota NE W_PPExtra THEN
            W_TotExt = W_TotExt + TExtras.Vr_CuoExtra.
        ELSE
            IF TExtras.Estado NE 1 THEN
                DELETE TExtras.
    END.

    IF W_TotExt GE Creditos.Sdo_Capital THEN DO:
        MESSAGE "El Valor Total de las Cuotas Extras + El valor de Esta Extra" SKIP
                "Debe ser Menor al Sdo-Capital...No se Acepta la Operaciòn."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    FIND FIRST TExtras WHERE TExtras.Nit EQ Creditos.Nit
                         AND TExtras.Num_Solicitud EQ Creditos.Num_solicitud
                         AND TExtras.Nro_Cuota EQ W_PPExtra NO-ERROR.
    IF NOT AVAIL(TExtras) THEN
        CREATE TExtras.

    ASSIGN W_FecIni = Creditos.Fec_Desem
           txtFechadesembolso = creditos.fec_desembolso
           txtFechaProxPago = creditos.fec_pago.

    IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desem THEN
        W_FecIni = Creditos.Fec_PagAnti.

    ASSIGN TExtras.Agencia = Creditos.Agencia
           TExtras.Cod_Credito = Creditos.Cod_Credito
           TExtras.Nit = Creditos.Nit
           TExtras.Num_Solicitud = Creditos.Num_solicitud
           TExtras.Nro_Cuota = W_PPExtra
           TExtras.Vr_CuoExtra = W_VrExtra
           TExtras.Estado = 1
           W_FecTra = W_FecIni.

    DO JJ = 1 TO W_PPExtra:
        RUN Halla_FecVcto.R (INPUT W_FecIni,
                             INPUT W_NroDias,
                             INPUT W_FecTra,
                             OUTPUT W_FecTra).

        TExtras.Fec_Vcto = W_FecTra.
    END.

    CLOSE QUERY Br_Extras.
    OPEN QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit EQ Creditos.Nit
                                            AND TExtras.Num_Solicitud EQ Creditos.Num_solicitud NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_EliExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_EliExt Wwin
ON CHOOSE OF Btn_EliExt IN FRAME F_Extras /* Eliminar Extra */
DO:
    FIND FIRST TExtras WHERE TExtras.Nit EQ Creditos.Nit
                         AND TExtras.Num_Solicitud EQ Creditos.Num_solicitud
                         AND TExtras.Nro_Cuota EQ W_PPExtra NO-ERROR.
    IF AVAIL(TExtras) THEN
        DELETE TExtras.
    
    CLOSE QUERY Br_Extras.

    OPEN QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit EQ Creditos.Nit
                                            AND TExtras.Num_Solicitud EQ Creditos.Num_solicitud NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME Btn_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Extras Wwin
ON CHOOSE OF Btn_Extras IN FRAME F_Cre /* Extras */
DO:
   ASSIGN FRAME F_Cre:SENSITIVE  = FALSE
          FRAME F_Extras:VISIBLE = TRUE.

   CLOSE QUERY Br_Extras.
   OPEN QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit EQ Creditos.Nit
                                           AND TExtras.Num_Solicitud EQ Creditos.Num_solicitud NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Wwin
ON CHOOSE OF Btn_Salir IN FRAME F_Cre /* Button 176 */
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


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME BUTTON-170
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-170 Wwin
ON CHOOSE OF BUTTON-170 IN FRAME F_Extras /* Button 170 */
DO:
    ASSIGN FRAME F_Extras:VISIBLE = FALSE
           FRAME F_Cre:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME Creditos.For_Pago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.For_Pago Wwin
ON MOUSE-SELECT-CLICK OF Creditos.For_Pago IN FRAME F_Cre /* Forma de Pago */
DO:
  APPLY "Value-Changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.For_Pago Wwin
ON VALUE-CHANGED OF Creditos.For_Pago IN FRAME F_Cre /* Forma de Pago */
DO:
    APPLY "Value-Changed" TO Creditos.Per_Pago.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Creditos.Per_Pago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Per_Pago Wwin
ON MOUSE-SELECT-CLICK OF Creditos.Per_Pago IN FRAME F_Cre /* Periodo de Pago */
DO:
  APPLY "Value-Changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Creditos.Per_Pago Wwin
ON VALUE-CHANGED OF Creditos.Per_Pago IN FRAME F_Cre /* Periodo de Pago */
DO:
    IF INTEG(Creditos.For_Pago:SCREEN-VALUE) EQ 2 THEN DO:
        FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.

        IF AVAIL(Empresas) THEN DO:
            Creditos.Per_Pago:SCREEN-VALUE = STRING(Empresas.FOR_Pago).

            MESSAGE 'La forma de pago para este crédito es "Nómina". Mientras esta' SKIP
                    'sea   la   forma   de   pago,   no  se  permite  el  cambio   de  la' SKIP
                    'periodicidad de pago, ya que se mantiene la de la empresa.'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
    
    IF Creditos.Per_Pago = INTEG(Creditos.Per_Pago:SCREEN-VALUE) THEN
        W_NvaTasa:SCREEN-VALUE = STRING(Creditos.Tasa).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_SistAmort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_SistAmort Wwin
ON MOUSE-SELECT-CLICK OF Rs_SistAmort IN FRAME F_Cre
DO:
    ASSIGN Rs_SistAmort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_SistAmort Wwin
ON VALUE-CHANGED OF Rs_SistAmort IN FRAME F_Cre
DO:
   ASSIGN Rs_SistAmort. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtFechaDesembolso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtFechaDesembolso Wwin
ON LEAVE OF txtFechaDesembolso IN FRAME F_Cre /* Fecha desembolso */
DO:
    ASSIGN txtfechaDesembolso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtFechaProxPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtFechaProxPago Wwin
ON LEAVE OF txtFechaProxPago IN FRAME F_Cre /* Fecha prox. pago */
DO:
    ASSIGN txtFechaProxPago.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NvaCuota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NvaCuota Wwin
ON LEAVE OF W_NvaCuota IN FRAME F_Cre /* Nueva Cuota */
DO:
   ASSIGN W_NvaCuota.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NvaTasa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NvaTasa Wwin
ON LEAVE OF W_NvaTasa IN FRAME F_Cre /* Nueva Tasa */
DO:
  ASSIGN W_NvaTasa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NvoPlazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NvoPlazo Wwin
ON LEAVE OF W_NvoPlazo IN FRAME F_Cre /* Nuevo Plazo */
DO:
  ASSIGN W_NvoPlazo.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Extras
&Scoped-define SELF-NAME W_PPExtra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_PPExtra Wwin
ON LEAVE OF W_PPExtra IN FRAME F_Extras
DO:
  ASSIGN W_PPExtra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_VrExtra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrExtra Wwin
ON LEAVE OF W_VrExtra IN FRAME F_Extras
DO:
  ASSIGN W_VrExtra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Wwin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActualizaFechaPago Wwin 
PROCEDURE ActualizaFechaPago :
IF creditos.cod_credito <> 123 THEN DO:
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito BY CONTROL_pagos.nro_cuota:
        IF CONTROL_pagos.fec_Vcto < txtFechaProxPago THEN DO:
            CONTROL_pagos.id_PdoMes = 2.
            control_pagos.Cap_pagado = control_pagos.pagos_capitalAcum.
            control_pagos.Int_pagado = control_pagos.Pagos_IntAcum.
        END.
        ELSE DO:
            CONTROL_pagos.id_PdoMes = 0.
            control_pagos.Cap_pagado = 0.
            control_pagos.Int_pagado = 0.
        END.
    END.
END.
ELSE DO:
    creditos.cuo_atraso = 0.
    
    IF creditos.fec_pago >= w_fecha THEN
        creditos.dias_atraso = 0.
    ELSE
        creditos.dias_atraso = w_fecha - creditos.fec_pago.

    creditos.val_atraso = 0.

    FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                           AND facturacion.num_credito = creditos.num_credito BY facturacion.fec_pago:
        IF facturacion.fec_pago < txtFechaProxPago THEN DO:
            facturacion.estado = 2.
            facturacion.pago_capital = facturacion.capital.
            facturacion.pago_intCorriente = facturacion.INT_corriente.
            facturacion.pago_intDifCobro = facturacion.INT_difCobro.
            facturacion.pago_mora = facturacion.INT_mora.
        END.
        ELSE DO:
            facturacion.estado = 1.
            facturacion.pago_capital = 0.
            facturacion.pago_intCorriente = 0.
            facturacion.pago_intDifCobro = 0.
            facturacion.pago_mora = 0.

            ASSIGN creditos.cuo_atraso = creditos.cuo_atraso + 1 WHEN facturacion.fec_pago < w_fecha.
            ASSIGN creditos.val_atraso = creditos.val_atraso + facturacion.capital WHEN facturacion.fec_pago < w_fecha.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects Wwin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cambio_Plan Wwin 
PROCEDURE Cambio_Plan :
/*------------------------------------------------------------------------------
  Purpose: Modifica el planPagos por cambio de Pdo.de pago    
  Notes:   Nov.9/05 GAER.
------------------------------------------------------------------------------*/
DEFI VAR W_ContCuota LIKE PlanPagos.Nro_Cuota INIT 0.
DEFI VAR I AS INTEG FORM "9999".

FOR EACH TPlanP.
    DELETE TPlanP.
END.

ASSIGN W_PdoPag = 30.

IF INTEG(Creditos.Per_Pago:SCREEN-VALUE IN FRAME F_Cre) EQ 1 THEN
    W_PdoPag = 7.
ELSE
    IF INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 2 THEN
        W_PdoPag = 10.
    ELSE
        IF INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 3 THEN
            W_PdoPag = 15.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Wwin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
  THEN DELETE WIDGET Wwin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Wwin  _DEFAULT-ENABLE
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
  DISPLAY W_PerPag W_CuoFaltan W_SdoOtros W_IntMora W_NvoPlazo Rs_SistAmort 
          txtFechaDesembolso W_FecIni W_NvaCuota W_NvaTasa txtFechaProxPago 
          W_TotExt 
      WITH FRAME F_Cre IN WINDOW Wwin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Fec_Desembolso Creditos.Fec_Reestructurado Creditos.Fec_Pago 
          Creditos.Provision Creditos.Fec_UltPago Creditos.Cuo_Atraso 
          Creditos.Dias_Atraso Creditos.Val_Atraso Creditos.Int_DifCobro 
          Creditos.Cuo_Pagadas Creditos.Int_Corrientes Creditos.Sdo_Capital 
          Creditos.Sdo_Proyectado Creditos.Plazo Creditos.Cuota Creditos.Tasa 
          Creditos.Per_Pago Creditos.For_Pago Creditos.Reestructurado 
          Creditos.Abogado 
      WITH FRAME F_Cre IN WINDOW Wwin.
  ENABLE RECT-296 RECT-321 RECT-322 RECT-333 RECT-325 RECT-326 RECT-335 
         Btn_Acepta_Terminos Creditos.Per_Pago W_NvoPlazo Creditos.For_Pago 
         Rs_SistAmort txtFechaDesembolso W_FecIni Btn_Salir W_NvaCuota 
         W_NvaTasa txtFechaProxPago Creditos.Reestructurado Creditos.Abogado 
         Btn_Extras 
      WITH FRAME F_Cre IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cre}
  DISPLAY W_PPExtra W_VrExtra 
      WITH FRAME F_Extras IN WINDOW Wwin.
  ENABLE Btn_AdExt W_PPExtra W_VrExtra Btn_EliExt BUTTON-170 Br_Extras 
      WITH FRAME F_Extras IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Extras}
  VIEW Wwin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject Wwin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir Wwin 
PROCEDURE Imprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR Listado  AS CHARACTER INITIAL "".
  Listado = W_PathSpl + W_Usuario + "Refinan.LST".
  {INCLUIDO\Imprimir.I "Listado"}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Wwin 
PROCEDURE initializeObject :
RUN SUPER.
  FIND Clientes WHERE Clientes.Nit EQ P_NitCli NO-LOCK NO-ERROR.

  ASSIGN Wwin:TITLE = "Modificaciòn-Refinanciación de Crèditos, Prog.W-ProRec_Refinancia.W.".
         FRAME F_Cre:TITLE = "Ced./Nit Asociado : " + P_NitCli + "   " +
                              TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " +
                              TRIM(Clientes.Apellido2).

  IF P_Progra EQ 9 THEN 
     ASSIGN Wwin:TITLE = "Simulación Refinanciación, Prog.W-ProRec_Refinancia.W."
            Btn_Acepta_Terminos:VISIBLE IN FRAME F_Cre = FALSE.
            

  FIND Creditos WHERE
       Creditos.Tip_Credito EQ P_TipCre AND
       Creditos.Cod_Credito EQ P_CodCre AND
       Creditos.Num_Credito EQ P_NumCre AND
       Creditos.Nit         EQ P_NitCli NO-LOCK NO-ERROR.
  IF AVAILABLE Creditos THEN DO:
     FRAME F_Cre:TITLE = FRAME F_Cre:TITLE + "  -   Nro_Credito : " + STRING(P_NumCre). 
     /* IF Creditos.Val_Atraso GT 0 THEN DO:
        MESSAGE "No se puede refinanciar un Crédito" SKIP
                "Que se encuentre atrasado." VIEW-AS ALERT-BOX INFORMATION.
        APPLY "choose" TO Btn_Salir IN FRAME F_Cre.
     END.*/
     
     FIND Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito AND
                             Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
     IF NOT AVAILABLE(Pro_Creditos) THEN DO:
        MESSAGE "El Producto de Créditos no se encuentra Disponible" SKIP
               "Hable con el Administrador!" VIEW-AS ALERT-BOX ERROR.
        APPLY "choose" TO Btn_Salir IN FRAME F_Cre.
     END.       
     
     RUN Mostrar_Credito.
  END.
  ELSE APPLY "CHHOSE" TO Btn_Salir.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquidar Wwin 
PROCEDURE Liquidar :
/*Halla Cuota o plazo */
  
  DEFINE VARIABLE PlazoW     LIKE Solicitud.Plazo.
  DEFINE VARIABLE TotPtW     LIKE Solicitud.Monto.
  DEFINE VARIABLE CuotaW     LIKE Solicitud.Cuota.
  DEFI   VAR      Cop_CuotaW LIKE Solicitud.Cuota.
  DEFINE VARIABLE TasaW      LIKE Solicitud.Tasa.
  DEFINE VARIABLE TinteW     LIKE Solicitud.Monto.

  DEFI   VAR W_CuoPla    AS INTEG FORM "9".
  DEFI   VAR NPdo        LIKE Creditos.Per_Pago.

  DEFINE VAR W_Resultado AS INTEGER INITIAL 0.
  DEFINE VAR Wimp_Coop   AS INTEGER INITIAL 0. 

  
  DO WITH FRAME F_Cre:
     ASSIGN TotPtW   = Creditos.Sdo_Capital
            CuotaW   = Creditos.Cuota
            W_CuoPla = 3
            TasaW    = Creditos.Tasa
            NPdo     = Creditos.Per_Pago.

     RUN Calculo_Cuota.R (INPUT-OUTPUT TotPtw, INPUT-OUTPUT PlazoW, INPUT-OUTPUT CuotaW,            
                     INPUT-OUTPUT TInteW, INPUT-OUTPUT TasaW, INPUT 0,                              
                     INPUT 0,INPUT NPdo, INPUT W_CuoPla,                                      
                     INPUT 1,                                                    
                     INPUT Creditos.Sistema).   

     ASSIGN W_NvaCuota:SCREEN-VALUE = STRING(CuotaW)
            W_NvoPlazo:SCREEN-VALUE = STRING(PlazoW)
            W_NvaCuota
            W_NvoPlazo.
            
     
     IF W_NvaCuota LE 0 OR W_NvoPlazo LE 0 THEN DO:                                                                        
        MESSAGE "El Valor de la cuota y/o Plazo debe ser mayor a cero. Rectifique!" VIEW-AS ALERT-BOX ERROR.  
                                                         
        RETURN ERROR.                                                                               
     END.                                                                                                  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Credito Wwin 
PROCEDURE Mostrar_Credito :
DO WITH FRAME F_Cre:
    ASSIGN Creditos.Fec_Desembolso:SCREEN-VALUE = STRING(Creditos.Fec_Desembolso)
           Creditos.Fec_Pago:SCREEN-VALUE = STRING(Creditos.Fec_Pago)
           Creditos.Fec_UltPago:SCREEN-VALUE = STRING(Creditos.Fec_UltPago)
           Creditos.INT_DifCobro:SCREEN-VALUE = STRING(Creditos.INT_DifCobro)
           Creditos.Sdo_Proyectado:SCREEN-VALUE = STRING(Creditos.Sdo_Proyectado)
           Creditos.Cuo_Pagadas:SCREEN-VALUE = STRING(Creditos.Cuo_Pagadas)
           Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Val_Atraso)
           Creditos.Dias_Atraso:SCREEN-VALUE = STRING(Creditos.Dias_Atraso)
           Creditos.Cuo_Atraso:SCREEN-VALUE = STRING(Creditos.Cuo_Atraso)
           Creditos.Provision:SCREEN-VALUE = STRING(Creditos.Provision)
           Creditos.Fec_Reestructurado:SCREEN-VALUE = STRING(Creditos.Fec_Reestructurado,"99/99/99")
           Creditos.Plazo:SCREEN-VALUE = STRING(Creditos.Plazo)
           Creditos.Tasa:SCREEN-VALUE = STRING(Creditos.Tasa)
           Creditos.Sdo_Capital:SCREEN-VALUE = STRING(Creditos.Sdo_Capital)
           Creditos.Cuota:SCREEN-VALUE = STRING(Creditos.Cuota)
           Creditos.Reestructurado:SCREEN-VALUE = STRING(Creditos.Reestructurado)
           Creditos.INT_Corrientes:SCREEN-VALUE = STRING(Creditos.INT_Corrientes).

    ASSIGN Creditos.Per_Pago:SCREEN-VALUE = STRING(Creditos.Per_Pago)
           Creditos.For_Pago:SCREEN-VALUE = STRING(Creditos.For_Pago)
           Creditos.Abogado:SCREEN-VALUE  = "No"
           W_NvaCuota:SCREEN-VALUE = STRING(Creditos.Cuota)
           W_NvoPlazo:SCREEN-VALUE = STRING(Creditos.Plazo)
           W_INTMora:SCREEN-VALUE = STRING(Creditos.INT_MorCobrar + Creditos.INT_MoraDifCob)
           W_SdoOtros:SCREEN-VALUE = STRING(Creditos.Costas + Creditos.Honorarios + Creditos.Polizas)
           W_CuoFaltan:SCREEN-VALUE = STRING(Creditos.Plazo - Creditos.Cuo_Pagadas)
           W_NvaTasa:SCREEN-VALUE = STRING(Creditos.Tasa)
           Rs_SistAmort:SCREEN-VALUE = STRING(Creditos.Sistema)
           W_FecIni:SCREEN-VALUE = STRING(Creditos.Fec_PagAnti)
           txtFechaDesembolso:SCREEN-VALUE = STRING(creditos.fec_desembolso)
           txtFechaProxPago:SCREEN-VALUE = STRING(creditos.fec_pago)
           W_FecIni
           txtFechaDesembolso
           txtFechaProxPago
           Rs_SistAmort
           W_NvaCuota
           W_NvoPlazo.

    IF Creditos.Sdo_Proyectado LE 0 THEN
        Creditos.Sdo_Proyectado:SCREEN-VALUE = "0".

    IF Creditos.Val_Atraso GT Creditos.Sdo_Capital THEN
        Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Sdo_Capital).

    IF Creditos.Abogado THEN
        Creditos.Abogado:SCREEN-VALUE = "Yes".

    FOR EACH TExtras:
        DELETE TExtras.
    END.

    FOR EACH extras WHERE extras.Agencia = creditos.agencia
                      AND extras.Cod_Credito = creditos.cod_credito
                      AND extras.Nit = creditos.nit
                      AND extras.Num_Solicitud = creditos.num_solicitud NO-LOCK:
        CREATE TExtras.
        ASSIGN TExtras.Agencia = extras.agencia
               TExtras.Cod_Credito = extras.cod_credito
               TExtras.Nit = extras.Nit
               TExtras.Num_Solicitud = extras.Num_solicitud
               TExtras.Nro_Cuota = extras.nro_cuota
               TExtras.Vr_CuoExtra = extras.vr_cuoExtra
               TExtras.Fec_Vcto = extras.Fec_Vcto
               TExtras.Estado = extras.estado
               W_TotExt = W_TotExt + TExtras.Vr_CuoExtra
               W_TotExt:SCREEN-VALUE = STRING(W_TotExt).
    END.

    OPEN QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit EQ Creditos.Nit
                                            AND TExtras.Num_Solicitud EQ Creditos.Num_solicitud NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
    
    CASE Creditos.Per_Pago:
        WHEN 1 THEN
            ASSIGN W_PerPag:SCREEN-VALUE = "Semanas"
                   W_PdoPag = 7
                   W_PdoAno = 52.

        WHEN 2 THEN
            ASSIGN W_PerPag:SCREEN-VALUE = "Dècadas"
                   W_PdoPag = 10
                   W_PdoAno = 36.

        WHEN 3 THEN
            ASSIGN W_PerPag:SCREEN-VALUE = "Quincenas"
                   W_PdoPag = 15
                   W_PdoAno = 24.

        WHEN 4 THEN
            ASSIGN W_PerPag:SCREEN-VALUE = "Meses"
                   W_PdoPag = 30
                   W_PdoAno = 12.
    END CASE.

    ASSIGN Pagas = Creditos.Cuo_Pagadas
           FecPP = Creditos.Fec_Pago.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mov_Credito Wwin 
PROCEDURE Mov_Credito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia       = Creditos.Agencia
           Mov_Creditos.Cod_Credito   = Creditos.Cod_Credito
           Mov_Creditos.Nit           = Creditos.Nit
           Mov_Creditos.Num_Credito   = Creditos.Num_Credito
           Mov_Creditos.Ofi_Destino   = Creditos.agencia
           Mov_Creditos.Ofi_Fuente    = W_Agencia
           Mov_Creditos.Pagare        = Creditos.Pagare
           Mov_Creditos.Fecha         = W_Fecha
           Mov_Creditos.Hora          = TIME
           Mov_Creditos.Num_Documento = STRING(W_Fecha,"99999999")           
           Mov_Creditos.Usuario       = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital   = Creditos.Sdo_Capital
           Mov_Creditos.Val_Efectivo  = 0
           Mov_Creditos.Cpte          = 4
           Mov_Creditos.Cod_Operacion = 030303001
           Mov_Creditos.Descrip       = "X Refin,Cuota y Plazo :" + STRING(Creditos.Cuota,">>>>>>>,>>9") + " " +
                                        STRING(Creditos.Plazo,">>>9").
                                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir Wwin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}

DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(60)".
DEFINE VAR T_Plazo AS CHARACTER FORMAT "X(30)".
DEFINE VAR T_Dedu AS CHARACTER FORMAT "X(30)".

FIND FIRST Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
ASSIGN W_Cliente = Creditos.Nit + " - " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.

W_EncColumna = "Cliente Solicitante         :   " + W_Cliente.
W_Reporte = "REESTRUCTURACION   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

VIEW FRAME F-Encabezado.
VIEW FRAME F-Ftr.

DO WITH FRAME F_Cre:
    CASE Creditos.Per_Pago:
        WHEN 1 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Semanas".
        WHEN 2 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Dècadas".
        WHEN 3 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Quincenas".
        WHEN 4 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Meses".
    END CASE.

    DISPLAY "=============================================DATOS GENERALES DEL CREDITO=================================================" AT 1
            "Agencia de Radicación       : " AT 1
            Creditos.Agencia
            "Número del Crédito          : " AT 65
            Creditos.Num_Credito             AT 98
            "Número de Solicitud         : " AT 1
            Creditos.Num_Solicitud           AT 33
            "Fecha de Aprobación         : " AT 65
            Creditos.Fec_Aprobacion          AT 98
            "Producto de Crédito         : " AT 1
            Pro_Creditos.Nom_Producto        AT 33
            "Tipo de Producto            : " AT 65
            Pro_Creditos.Tip_Credito         AT 98
            "Instancia Actual            : " AT 1
            P_NomIns                         AT 33
            "Usuario Actualmente Procesa : " AT 65
            W_Usuario                        AT 98
            "Forma de Pago de la Cuota   : " AT 1
            Creditos.FOR_Pago                AT 33
            "=============================================DETALLE DE VALORES DEL CREDITO==============================================" AT 1
            "Monto Inicial del Credito   : " AT 1
            Creditos.Val_Desemb              AT 33
            "Tasa Anterior               : " AT 65
            Creditos.Tasa:SCREEN-VALUE IN FRAME F_Cre AT 98 SKIP(1)
        WITH FRAME F_Sol WIDTH 150 NO-BOX USE-TEXT NO-LABELS STREAM-IO.

    DISPLAY "CAMBIOS POR MODIFICACION" AT 1
            "Saldo Capital               : " AT 1
            Creditos.Sdo_Capital             AT 33  FORMAT ">>>,>>>,>>9"
            "Tasa Nominal Anual          : " AT 65
            Creditos.Tasa                    AT 98
            "Plazo Anterior              : " AT 1
            T_Plazo                          AT 33
            "Nuevo Plazo                 : " AT 65
            W_NvoPlazo                       AT 98
            "Cuota Anterior              : " AT 1
            Creditos.Cuota:SCREEN-VALUE      AT 33
            "Nueva Cuota                 : " AT 65
            W_NvaCuota                       AT 98
            "Cuotas Pagadas Anterior y Actual      :" AT 1
            Pagas                                     AT 40
            Creditos.Cuo_Pagadas                      AT 52
            "Fecha-Proximo Pago Anterior y Actual  :" AT 1
            FecPP                                     AT 40
            Creditos.Fec_Pago                         AT 52
        WITH FRAME F_Sol2 WIDTH 150 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
END.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proyeccion Wwin 
PROCEDURE Proyeccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*RUN Proyeccion_Credito.R (INPUT DECIMAL(Solicitud.TOTAL_Prestamo:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT DECIMAL(Solicitud.Plazo:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT DECIMAL(Solicitud.Cuota:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT 0, /*total extras que ya no se utiliza*/
                INPUT TODAY, /*no se que fecha se pone o se pide*/
                INPUT DECIMAL(W_TasaPeriodo:SCREEN-VALUE IN FRAME F_Solicitud) / 100,
                INPUT DECIMAL(Solicitud.Incremento:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT 0, /*gracia*/
                INPUT INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,1,1)),
                INPUT INTEGER(Solicitud.FOR_Interes:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT INTEGER(SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,1,5)),
                INPUT Solicitud.Nit:SCREEN-VALUE IN FRAME F_Solicitud,
                INPUT NomNit:SCREEN-VALUE IN FRAME F_Solicitud,
                INPUT INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,1,3)),   
                INPUT SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto,7,30),
                INPUT "S",
                INPUT DECIMAL(Solicitud.Num_Solicitud:SCREEN-VALUE IN FRAME F_Solicitud),
                INPUT SUBSTRING(Cmb_Sistemas:SCREEN-VALUE IN FRAME F_Solicitud,9,30),
                INPUT SUBSTRING(Cmb_PerPago:SCREEN-VALUE IN FRAME F_Solicitud,5,15),
                INPUT 1,Solicitud.Monto).                          */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Refinanc Wwin 
PROCEDURE Refinanc :
ASSIGN Creditos.Cuota = W_NvaCuota
       Creditos.Plazo = W_NvoPlazo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReInicio Wwin 
PROCEDURE ReInicio :
DEFI VAR W_ContCuota LIKE PlanPagos.Nro_Cuota INIT 0.
DEFI VAR I AS INTEG FORM "9999".
DEFI VAR W_DiasTrans AS INTEG FORM "9999".

FOR EACH TPlanP.
    DELETE TPlanP.
END.

ASSIGN Creditos.Monto = Creditos.Sdo_Capital
       Creditos.Sdo_Proyecta = Creditos.Sdo_Capital
       Creditos.Cuota = W_NvaCuota
       Creditos.Plazo = W_NvoPlazo
       Creditos.Capital_Acum = 0
       Creditos.Sdo_CapPag = 0
       Creditos.Sdo_IntMor = 0
       Creditos.Cuo_Pagadas = 0
       Creditos.Val_Atraso = 0
       Creditos.Cuo_Atraso = 0
       Creditos.Dias_Atraso = 0
       Creditos.Int_LiqAcum = 0
       Creditos.Sdo_IntPag = Creditos.Int_Anticipado.

CREATE TPlanP.

W_ContCuota = 1.

CREATE TPlanP.

IF W_ContCuota GE W_NvoPlazo THEN
    RETURN.

IF W_ContCuota LT W_NvoPlazo THEN
    DO I = W_ContCuota + 1 TO W_NvoPlazo:
        W_DiasTrans = W_DiasTrans + W_PdoPag.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TotalMoroso Wwin 
PROCEDURE TotalMoroso :
ASSIGN Creditos.Sdo_proyectado = 0
       Creditos.Capital_Acum = Creditos.Monto
       Creditos.Cuo_Atraso = Creditos.Plazo - Creditos.Cuo_Pagadas
       Creditos.Dias_Atraso = (W_Fecha + 1)  - Creditos.Fec_Pago
       Creditos.Val_Atraso = Creditos.Sdo_capital.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

