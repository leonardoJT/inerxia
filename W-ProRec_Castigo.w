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

DEFINE VAR P_Poliza AS DECIMAL.
DEFINE VAR P_Honora AS DECIMAL.
DEFINE VAR P_Costas AS DECIMAL.
DEFINE VAR P_IDifCobMor AS DECIMAL.
DEFINE VAR P_IMora AS DECIMAL.
DEFINE VAR P_IDifCob AS DECIMAL.
DEFINE VAR P_ICte AS DECIMAL.
DEFINE VAR P_IAntic AS DECIMAL. /*Si P_IAntic(-) Neg.son cargos*/
DEFINE VAR P_Capital AS DECIMAL.
DEFINE VAR P_VlrNoDist AS DECIMAL. /*Valor NO Distribuido*/
DEFINE VAR W_CtaCap AS CHARACTER.
DEFINE VAR W_CtaIntCor_Db AS CHARACTER.
DEFINE VAR W_CtaIntCor_Cr AS CHARACTER.
DEFINE VAR W_CtaIntMor AS CHARACTER.
DEFINE VAR W_CtaDifCob_db AS CHARACTER.

   /* oakley */

   DEFINE VAR W_CtaDifCob_Cr  LIKE Cuentas.Cuenta.           
   DEFINE VAR W_Caja          LIKE Cuentas.Cuenta.           
   DEFINE VAR W_CtaSya_Des    LIKE Cuentas.Cuenta.           
   DEFINE VAR W_CtaSya_Fte    LIKE Cuentas.Cuenta.           
   DEFINE VAR W_CtaCosDB      LIKE Cuentas.Cuenta.           
   DEFINE VAR W_CtaHonDB      LIKE Cuentas.Cuenta.           
   DEFINE VAR W_CtaPolDB      LIKE Cuentas.Cuenta.           
   /*DEFINE VAR W_Castigo       LIKE Cuentas.Cuenta.          
   DEFINE VAR W_OrdCasDB      LIKE Cuentas.Cuenta.           
   DEFINE VAR W_OrdCasCR      LIKE Cuentas.Cuenta.*/
   
   DEFINE VAR W_Cbte          LIKE Comprobantes.Comprobante. 
   DEFINE VAR W_NumCbt        LIKE Comprobantes.Secuencia.   
   DEFINE VAR W_Canje         LIKE Bancos.Dia_Canje.         
   DEFINE VAR W_ValEfe        LIKE Creditos.Sdo_Capital.     
                                                             
   DEFINE VARIABLE W_NumSeq   AS INTEGER FORMAT "9999999".

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-296 RECT-299 RECT-300 RECT-301 RECT-302 ~
W_CtaGasto W_CastigInt W_CastigPol W_CtaDbInt W_CtaCrInt W_Castigo ~
W_CtaDbPol W_CtaCrPol W_OrdCasDB Btn_Grabar Btn_Salir W_OrdCasCR 
&Scoped-Define DISPLAYED-FIELDS Creditos.Int_Anticipado ~
Creditos.Fec_Desembolso Creditos.Int_MoraDifCob Creditos.Tasa ~
Creditos.Fec_Pago Creditos.Int_MorCobrar Creditos.Plazo ~
Creditos.Sdo_Proyectado Creditos.Int_Corrientes Creditos.Fec_UltPago ~
Creditos.Categoria Creditos.Int_DifCobro Creditos.Cuo_Pagadas ~
Creditos.Val_Atraso Creditos.Sdo_Capital Creditos.Dias_Atraso ~
Creditos.Cuo_Atraso Creditos.Cuota Creditos.Provision ~
Creditos.Provision_Interes Creditos.Fec_Reestructurado 
&Scoped-define DISPLAYED-TABLES Creditos
&Scoped-define FIRST-DISPLAYED-TABLE Creditos
&Scoped-Define DISPLAYED-OBJECTS W_Jurid W_Total W_CtaGasto Nom-10 ~
W_CastigInt Nom1 W_CastigPol Nom-4 W_CtaDbInt Nom-2 W_CtaCrInt Nom-3 ~
W_Castigo Nom-7 W_CtaDbPol Nom-5 W_CtaCrPol Nom-6 W_OrdCasDB Nom-8 ~
W_OrdCasCR Nom-9 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Grabar 
     LABEL "Grabar Operación" 
     SIZE 15 BY 1.19.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_Salir" 
     SIZE 15 BY 1.23.

DEFINE VARIABLE Nom-10 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 29.86 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom-2 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 29.57 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom-3 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 29.43 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom-4 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 29.86 BY .85
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom-5 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 29.43 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom-6 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 29.57 BY .77
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom-7 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 27.86 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom-8 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 27.57 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom-9 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 27.86 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Nom1 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CastigInt AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta-Prov.(Castigo Interes)" 
     VIEW-AS FILL-IN 
     SIZE 11.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Castigo AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 11.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CastigPol AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta-Prov.(Castigo Poliza)" 
     VIEW-AS FILL-IN 
     SIZE 11.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaCrInt AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta-Orden (CR Intereses)" 
     VIEW-AS FILL-IN 
     SIZE 11.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaCrPol AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta-Orden (CR Polizas)" 
     VIEW-AS FILL-IN 
     SIZE 11.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaDbInt AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta-Orden (DB Intereses)" 
     VIEW-AS FILL-IN 
     SIZE 11.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaDbPol AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cta-Orden (DB Polizas)" 
     VIEW-AS FILL-IN 
     SIZE 11.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaGasto AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta-Gastos" 
     VIEW-AS FILL-IN 
     SIZE 11.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Jurid AS DECIMAL FORMAT ">>>>,>>>,>>9":U INITIAL 0 
     LABEL "Por cobro Jurìdico" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_OrdCasCR AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 11.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_OrdCasDB AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 11.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Total AS DECIMAL FORMAT ">>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total de Saldos" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 11.31.

DEFINE RECTANGLE RECT-299
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 6.54.

DEFINE RECTANGLE RECT-300
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 4.62.

DEFINE RECTANGLE RECT-301
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 62.14 BY 7.5.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40.43 BY 4.54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Cre
     Creditos.Int_Anticipado AT ROW 2.04 COL 46 COLON-ALIGNED
          LABEL "Interés Anticipado"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Desembolso AT ROW 2.08 COL 14 COLON-ALIGNED
          LABEL "Desembolso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_MoraDifCob AT ROW 2.27 COL 79.86 COLON-ALIGNED
          LABEL "Mora Contingente"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Tasa AT ROW 3.12 COL 46 COLON-ALIGNED
          LABEL "Tasa"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Pago AT ROW 3.15 COL 14 COLON-ALIGNED
          LABEL "Fecha Prox.Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_MorCobrar AT ROW 3.19 COL 79.86 COLON-ALIGNED
          LABEL "Interés Mora"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Plazo AT ROW 4.19 COL 46 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Proyectado AT ROW 4.27 COL 12.86 COLON-ALIGNED
          LABEL "Saldo Proyectado"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Corrientes AT ROW 4.27 COL 79.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_UltPago AT ROW 5.19 COL 13.72 COLON-ALIGNED
          LABEL "Último Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Categoria AT ROW 5.23 COL 53.14 COLON-ALIGNED HELP
          ""
          LABEL "Categoria de la Cartera" FORMAT "X"
          VIEW-AS FILL-IN 
          SIZE 4.43 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Int_DifCobro AT ROW 5.35 COL 79.86 COLON-ALIGNED
          LABEL "Interés Contingente"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuo_Pagadas AT ROW 6.12 COL 13.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_Jurid AT ROW 6.31 COL 79.86 COLON-ALIGNED
     Creditos.Val_Atraso AT ROW 7 COL 13.72 COLON-ALIGNED
          LABEL "Valor Vencido"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Sdo_Capital AT ROW 7.27 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_Total AT ROW 7.5 COL 52.14 COLON-ALIGNED
     Creditos.Dias_Atraso AT ROW 8 COL 13.72 COLON-ALIGNED
          LABEL "Días Vencidos"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Cuo_Atraso AT ROW 8.88 COL 13.72 COLON-ALIGNED
          LABEL "Cuotas Vencidas"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     W_CtaGasto AT ROW 8.96 COL 58.72 COLON-ALIGNED
     Nom-10 AT ROW 9 COL 70.29 COLON-ALIGNED NO-LABEL
     Creditos.Cuota AT ROW 9.77 COL 12.72 COLON-ALIGNED
          LABEL "Cuota del Crédito"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.29 BY 16.62
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Cre
     W_CastigInt AT ROW 9.92 COL 58.72 COLON-ALIGNED
     Nom1 AT ROW 9.96 COL 70.29 COLON-ALIGNED NO-LABEL
     Creditos.Provision AT ROW 10.69 COL 13.72 COLON-ALIGNED
          LABEL "Provisión-Capital"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 4 FGCOLOR 15 
     Creditos.Provision_Interes AT ROW 10.81 COL 27.14 COLON-ALIGNED NO-LABEL FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11.86 BY .81
          BGCOLOR 19 
     W_CastigPol AT ROW 11 COL 58.72 COLON-ALIGNED
     Nom-4 AT ROW 11.04 COL 70.29 COLON-ALIGNED NO-LABEL
     Creditos.Fec_Reestructurado AT ROW 11.77 COL 13.72 COLON-ALIGNED
          LABEL "Reestructuración"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_CtaDbInt AT ROW 11.96 COL 58.72 COLON-ALIGNED
     Nom-2 AT ROW 12.04 COL 70.29 COLON-ALIGNED NO-LABEL
     W_CtaCrInt AT ROW 12.96 COL 58.72 COLON-ALIGNED
     Nom-3 AT ROW 13.04 COL 70.29 COLON-ALIGNED NO-LABEL
     W_Castigo AT ROW 13.65 COL 2.14 NO-LABEL
     Nom-7 AT ROW 13.65 COL 11.57 COLON-ALIGNED NO-LABEL
     W_CtaDbPol AT ROW 14.04 COL 58.72 COLON-ALIGNED
     Nom-5 AT ROW 14.08 COL 70.29 COLON-ALIGNED NO-LABEL
     W_CtaCrPol AT ROW 15 COL 58.72 COLON-ALIGNED
     Nom-6 AT ROW 15.08 COL 70.29 COLON-ALIGNED NO-LABEL
     W_OrdCasDB AT ROW 15.54 COL 2.29 NO-LABEL
     Nom-8 AT ROW 15.54 COL 11.72 COLON-ALIGNED NO-LABEL
     Btn_Grabar AT ROW 16.23 COL 64.57
     Btn_Salir AT ROW 16.23 COL 80.57
     W_OrdCasCR AT ROW 16.46 COL 2.14 NO-LABEL
     Nom-9 AT ROW 16.5 COL 11.57 COLON-ALIGNED NO-LABEL
     "Provisión_Interés" VIEW-AS TEXT
          SIZE 11.72 BY .5 AT ROW 10.31 COL 29.29
          BGCOLOR 18 FGCOLOR 15 
     "Valores a Castigar" VIEW-AS TEXT
          SIZE 16 BY 1.08 AT ROW 1.27 COL 68
          FGCOLOR 7 FONT 5
     "Cuentas Orden(Db y Cr Capital)" VIEW-AS TEXT
          SIZE 25.86 BY .5 AT ROW 15.08 COL 7.29
          BGCOLOR 15 FGCOLOR 1 
     "Cuenta Provision(Castigo Capital)" VIEW-AS TEXT
          SIZE 25.86 BY .5 AT ROW 13.15 COL 8.14
          BGCOLOR 15 FGCOLOR 1 
     "Información del Crédito" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 1.15 COL 3
          FGCOLOR 7 FONT 5
     RECT-296 AT ROW 1.54 COL 1
     RECT-299 AT ROW 1.81 COL 66
     RECT-300 AT ROW 1.81 COL 30
     RECT-301 AT ROW 8.58 COL 41.43
     RECT-302 AT ROW 12.92 COL 1.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.29 BY 16.62
         BGCOLOR 17 FONT 4.


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
         TITLE              = "Castigos de Cartera, Programa W-ProRec_Castigo.W"
         COLUMN             = 8.29
         ROW                = 4.5
         HEIGHT             = 16.62
         WIDTH              = 103.29
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
/* SETTINGS FOR FILL-IN Creditos.Categoria IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN Creditos.Cuota IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Pagadas IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Dias_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Desembolso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Pago IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Reestructurado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Anticipado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Corrientes IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_DifCobro IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_MoraDifCob IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_MorCobrar IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Nom-10 IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom-2 IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom-3 IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom-4 IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom-5 IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom-6 IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom-7 IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom-8 IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom-9 IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nom1 IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Plazo IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Provision IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Provision_Interes IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Capital IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Proyectado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Tasa IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Val_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN W_Castigo IN FRAME F_Cre
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_Jurid IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_OrdCasCR IN FRAME F_Cre
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_OrdCasDB IN FRAME F_Cre
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_Total IN FRAME F_Cre
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cre
/* Query rebuild information for FRAME F_Cre
     _Query            is NOT OPENED
*/  /* FRAME F_Cre */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON END-ERROR OF Wwin /* Castigos de Cartera, Programa W-ProRec_Castigo.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON WINDOW-CLOSE OF Wwin /* Castigos de Cartera, Programa W-ProRec_Castigo.W */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar Wwin
ON CHOOSE OF Btn_Grabar IN FRAME F_Cre /* Grabar Operación */
DO:
  ASSIGN FRAME F_Cre W_Total.

  IF Creditos.INT_Anticipado NE 0 THEN DO:
     MESSAGE "Debe Cruzar los Int-Antcipados por Traslados para poder Efectuar el castigo."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF W_CtaDbInt    LE " " OR W_CtaCrInt LE " " OR W_CastigInt LE " " OR W_CastigPol LE " " 
     OR W_CtaDbPol LE " " OR W_CtaCrPol LE " " 
     OR W_CtaDbInt EQ ?   OR W_CtaCrInt EQ ?   OR W_CastigInt EQ ?   OR W_CastigPol EQ ? 
     OR W_CtaDbPol LE " " OR W_CtaCrPol EQ ? 
     OR W_Castigo  LE " " OR W_Castigo  EQ ?   OR W_CtaGasto  LE " " OR W_CtaGasto  EQ ?
     OR W_OrdCasDB LE " " OR W_OrdCasDB EQ ?   OR W_OrdCasCr  LE " " OR W_OrdCasCr  EQ ? THEN DO:
     MESSAGE "Las Ctas-Contables Config.en Pantalla Todas son Indispensables."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.
  
  IF W_Total LE 0 THEN DO:
     MESSAGE "No existe valor para el Castigo." 
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.
  
  FIND Comprobantes WHERE Comprobante.Agencia      EQ Creditos.Agencia AND 
                          Comprobantes.Comprobante EQ 3 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE (Comprobantes) THEN DO:
     MESSAGE "Falta en Comprobantes el Cod. 3 en la Agencia del Crédito." 
          VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.
  
  RUN Transaccion NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "Se ha producido un error en la grabación del Castigo." SKIP
             "Revise por favor" VIEW-AS ALERT-BOX ERROR.
     APPLY "choose" TO Btn_Salir IN FRAME F_Cre.
  END.

  DEFI VAR Listado AS CHAR FORM "X(40)".                                                                   
                                                                                                                
  Listado = W_PathSpl + "CastigCont-" + STRING(Comprobantes.Secuencia)  + ".Lst".                
                                                                                                                
  {Incluido\Imprimir.I "listado"}                               
  
  APPLY "choose" TO Btn_Salir IN FRAME F_Cre.                           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Wwin
ON CHOOSE OF Btn_Salir IN FRAME F_Cre /* Btn_Salir */
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


&Scoped-define SELF-NAME W_CastigInt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CastigInt Wwin
ON LEAVE OF W_CastigInt IN FRAME F_Cre /* Cta-Prov.(Castigo Interes) */
DO:
  ASSIGN W_CastigInt.
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CastigInt
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La Cta-Contable debe ser de Db-Provision Intereses y Activa en el PUC."
         VIEW-AS ALERT-BOX ERROR.
     ASSIGN W_CastigInt = " "
            W_CastigInt:SCREEN-VALUE = "".
     RETURN.
  END.
  ELSE Nom1:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Castigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Castigo Wwin
ON LEAVE OF W_Castigo IN FRAME F_Cre
DO:
  ASSIGN W_Castigo.
  
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_Castigo
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La Cta-Contable debe ser de Movimiento y Activa en el PUC."
         VIEW-AS ALERT-BOX ERROR.
     ASSIGN W_Castigo = " "
            W_Castigo:SCREEN-VALUE = "".
     RETURN.
  END.
  ELSE Nom-7:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CastigPol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CastigPol Wwin
ON LEAVE OF W_CastigPol IN FRAME F_Cre /* Cta-Prov.(Castigo Poliza) */
DO:
  ASSIGN W_CastigPol.
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CastigPol
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La Cta-Contable debe ser de Provision C x C y Activa en el PUC."
         VIEW-AS ALERT-BOX ERROR.
     ASSIGN W_CastigPol = " "
            W_CastigPol:SCREEN-VALUE = "".
     RETURN.
  END.
  ELSE Nom-4:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaCrInt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaCrInt Wwin
ON LEAVE OF W_CtaCrInt IN FRAME F_Cre /* Cta-Orden (CR Intereses) */
DO:
   ASSIGN W_CtaCrInt.
   FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaCrInt
                        AND Cuentas.Tipo   EQ 2
                        AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) OR SUBSTRING(W_CtaCrInt,1,2) LE "79" THEN DO:
     MESSAGE "La Cta-Contable debe ser de Orden y Activa en el PUC."
         VIEW-AS ALERT-BOX ERROR.
     ASSIGN W_CtaCrInt = " "
            W_CtaCrInt:SCREEN-VALUE = "".
  END.
  ELSE Nom-3:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaCrPol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaCrPol Wwin
ON LEAVE OF W_CtaCrPol IN FRAME F_Cre /* Cta-Orden (CR Polizas) */
DO:
   ASSIGN W_CtaCrPol.
   FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaCrPol
                        AND Cuentas.Tipo   EQ 2
                        AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) OR SUBSTRING(W_CtaCrPol,1,2) LE "79" THEN DO:
     MESSAGE "La Cta-Contable debe ser de Orden y Activa en el PUC."
         VIEW-AS ALERT-BOX ERROR.
     ASSIGN W_CtaCrPol = " "
            W_CtaCrPol:SCREEN-VALUE = "".
  END.
  ELSE Nom-6:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaDbInt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaDbInt Wwin
ON LEAVE OF W_CtaDbInt IN FRAME F_Cre /* Cta-Orden (DB Intereses) */
DO:
  ASSIGN W_CtaDbInt.
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaDbInt
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) OR SUBSTRING(W_CtaDbInt,1,2) LE "79" THEN DO:
     MESSAGE "La Cta-Contable debe ser de Orden y Activa en el PUC."
         VIEW-AS ALERT-BOX ERROR.
     ASSIGN W_CtaDbInt = " "
            W_CtaDbInt:SCREEN-VALUE = "".
     RETURN.
  END.
  ELSE Nom-2:SCREEN-VALUE = Cuentas.Nombre.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaDbPol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaDbPol Wwin
ON LEAVE OF W_CtaDbPol IN FRAME F_Cre /* Cta-Orden (DB Polizas) */
DO:
  ASSIGN W_CtaDbPol.
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaDbPol
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) OR SUBSTRING(W_CtaDbPol,1,2) LE "79" THEN DO:
     MESSAGE "La Cta-Contable debe ser de Orden y Activa en el PUC."
         VIEW-AS ALERT-BOX ERROR.
     ASSIGN W_CtaDbPol = " "
            W_CtaDbPol:SCREEN-VALUE = "".
  END.
  ELSE Nom-5:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaGasto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaGasto Wwin
ON LEAVE OF W_CtaGasto IN FRAME F_Cre /* Cuenta-Gastos */
DO:
  ASSIGN W_CtaGasto.
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaGasto
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La Cta-Contable debe ser de Movimiento y Activa en el PUC."
         VIEW-AS ALERT-BOX ERROR.
     ASSIGN W_CtaGasto = " "
            W_CtaGasto:SCREEN-VALUE = "".
     RETURN.
  END.
  ELSE Nom-10:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_OrdCasCR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OrdCasCR Wwin
ON LEAVE OF W_OrdCasCR IN FRAME F_Cre
DO:
  ASSIGN W_OrdCasCr.
  
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_OrdCasCr
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La Cta-Contable debe ser de Movimiento y Activa en el PUC."
         VIEW-AS ALERT-BOX ERROR.
     ASSIGN W_OrdCasCr = " "
            W_OrdCasCr:SCREEN-VALUE = "".
     RETURN.
  END.
  ELSE Nom-9:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_OrdCasDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OrdCasDB Wwin
ON LEAVE OF W_OrdCasDB IN FRAME F_Cre
DO:
  ASSIGN W_OrdCasDb.
  
  FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_OrdCasDb
                       AND Cuentas.Tipo   EQ 2
                       AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Cuentas) THEN DO:
     MESSAGE "La Cta-Contable debe ser de Movimiento y Activa en el PUC."
         VIEW-AS ALERT-BOX ERROR.
     ASSIGN W_OrdCasDb = " "
            W_OrdCasDb:SCREEN-VALUE = "".
     RETURN.
  END.
  ELSE Nom-8:SCREEN-VALUE = Cuentas.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Wwin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Local Wwin 
PROCEDURE Contabilizar_Local :
IF P_IDifCob + P_IDifCobMor > 0 THEN DO:
    CREATE Mov_Contable.
    RUN Graba_Mov.
    ASSIGN Mov_Contable.Cuenta = W_CtaIntCor_Cr
           Mov_Contable.DB = P_IDifCob + P_IDifCobMor
           Mov_Contable.Comentario = "Reversa X Proc.Castigo".
END.

/* ----- */
IF P_Capital > 0 THEN DO:
    CREATE Mov_Contable.
    RUN Graba_Mov.
    Mov_Contable.Cuenta = W_Castigo.

    IF Creditos.Provision >= P_Capital THEN DO:
        Mov_Contable.DB = P_Capital.
    END.
    ELSE DO:
        IF Creditos.Provision > 0 THEN DO:
            Mov_Contable.DB = Creditos.Provision.
            
            CREATE Mov_Contable.
            RUN Graba_Mov.
            ASSIGN Mov_Contable.Cuenta = W_CtaGasto
                   Mov_Contable.DB = P_Capital - Creditos.Provision.
        END.
        ELSE DO:
            ASSIGN Mov_Contable.Cuenta = W_CtaGasto
                   Mov_Contable.DB = P_Capital.
        END.
    END.

    CREATE Mov_Contable.
    RUN Graba_Mov.
    ASSIGN Mov_Contable.Cuenta = W_OrdCasDB
           Mov_Contable.DB = P_Capital.

    CREATE Mov_Contable.
    RUN Graba_Mov.
    ASSIGN Mov_Contable.Cuenta = W_OrdCasCr
           Mov_Contable.Cr = P_Capital.
END.

IF P_Poliza + P_Honora + P_Costas > 0 THEN DO:
    CREATE Mov_Contable.
    RUN Graba_Mov.
    ASSIGN Mov_Contable.Cuenta = W_CastigPol
           Mov_Contable.DB = P_Poliza + P_Honora + P_Costas.

    CREATE Mov_Contable.
    RUN Graba_Mov.
    ASSIGN Mov_Contable.Cuenta = W_CtaDbPol
           Mov_Contable.DB = P_Poliza + P_Honora + P_Costas.

    CREATE Mov_Contable.
    RUN Graba_Mov.
    ASSIGN Mov_Contable.Cuenta = W_CtaCrPol
           Mov_Contable.Cr = P_Poliza + P_Honora + P_Costas.
END.

IF P_IDifCob + P_IDifCobMor + P_IMora + P_ICte > 0 THEN DO:
    IF P_IMora + P_ICte > 0 THEN DO:
        CREATE Mov_Contable.
        RUN Graba_Mov.

        IF Creditos.Provision_Interes >= P_IMora + P_ICte THEN DO:
            ASSIGN Mov_Contable.Cuenta = W_CastigInt
                   Mov_Contable.DB = P_IMora + P_ICte.
        END.
        ELSE DO:
            IF Creditos.Provision_Interes > 0 THEN DO:
                ASSIGN Mov_Contable.Cuenta = W_CastigInt
                       Mov_Contable.DB = Creditos.Provision_Interes.

                CREATE Mov_Contable.
                RUN Graba_Mov.
                ASSIGN Mov_Contable.Cuenta = W_CtaGasto
                       Mov_Contable.DB = (P_IMora + P_ICte) - Creditos.Provision_Interes.
            END.
            ELSE DO:
                ASSIGN Mov_Contable.Cuenta = W_CtaGasto
                       Mov_Contable.DB = (P_IMora + P_ICte).
            END.
        END.
    END.

    CREATE Mov_Contable.
    RUN Graba_Mov.
    ASSIGN Mov_Contable.Cuenta = W_CtaDbInt
           Mov_Contable.DB = P_IDifCob + P_IDifCobMor + P_IMora + P_ICte.

    CREATE Mov_Contable.
    RUN Graba_Mov.
    ASSIGN Mov_Contable.Cuenta = W_CtaCrInt
           Mov_Contable.Cr = P_IDifCob + P_IDifCobMor + P_IMora + P_ICte.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CortoLargoCreditos Wwin 
PROCEDURE CortoLargoCreditos :
FIND FIRST CortoLargo WHERE CortoLargo.Agencia     EQ Creditos.Agencia
                 AND   CortoLargo.Clase_Producto    EQ 2
                 AND   CortoLargo.Cod_Producto      EQ Creditos.Cod_Credito
                 AND   CortoLargo.Cta_ContingenteDB NE "" 
                 AND   CortoLargo.Cta_ContingenteCR NE "" 
                 AND   CortoLargo.Comprobante       NE 0 NO-LOCK NO-ERROR.
 IF AVAILABLE(CortoLargo) THEN DO:
    ASSIGN W_CtaSyA_Des  = CortoLargo.Cta_SYA
           W_OrdCasDB    = CortoLargo.Cta_OrdCasDB
           W_OrdCasDB:SCREEN-VALUE IN FRAME F_Cre = W_OrdCasDB
           W_OrdCasCR    = CortoLargo.Cta_OrdCasCR
           W_OrdCasCR:SCREEN-VALUE = W_OrdCasCR
           W_Castigo               = CortoLargo.Cta_Castigo
           W_Castigo:SCREEN-VALUE  = W_Castigo
           W_CtaCosDB    = CortoLargo.Cta_CostasDB    
           W_CtaHonDB    = CortoLargo.Cta_HonorariosDB               
           W_CtaPolDB    = CortoLargo.Cta_PolizasDB.         

    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_Castigo NO-LOCK NO-ERROR.
    ASSIGN Nom-7:SCREEN-VALUE = Cuentas.Nombre.

    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_OrdCasDB NO-LOCK NO-ERROR.
    ASSIGN Nom-8:SCREEN-VALUE = Cuentas.Nombre.

    FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_OrdCasCR NO-LOCK NO-ERROR.
    ASSIGN Nom-9:SCREEN-VALUE = Cuentas.Nombre.
           
    IF W_CtaSyA_Des EQ "" THEN DO:
       MESSAGE "No esta configurada la cuenta de sucursales y agencias" SKIP
               "para la agencia de trabajo actual" SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.
    
    /*busca si hay garantias admisibles para cuenta de Capital*/
    FIND FIRST Garantias WHERE 
         Garantias.Num_Credito EQ Creditos.Num_Credito  AND 
         Garantias.Estado      EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Garantias THEN DO:
      IF Clientes.Tipo_Vinculo EQ 1 THEN 
         W_CtaCap = CortoLargo.Cta_AsoAd.
      ELSE 
         W_CtaCap = CortoLargo.Cta_NoaAd.
    END.
    ELSE DO:
      IF Clientes.Tipo_Vinculo EQ 1 THEN 
         W_CtaCap = CortoLargo.Cta_AsoNa.
      ELSE 
         W_CtaCap = CortoLargo.Cta_NoaNa.
    END.
    IF W_CtaCap EQ "" THEN DO:
       MESSAGE "No se ha encontrado la cuenta para el Castigo" SKIP
               "del capital. Comuniquese con el Administrador" VIEW-AS ALERT-BOX.
       RETURN ERROR.
    END.
    /*busca cta en tabla liqint*/
    ASSIGN W_CtaIntCor_Db = ""
           W_CtaIntCor_Cr = ""
           W_CtaDifCob_Db = ""
           W_CtaIntMor    = "".

    FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2 AND
                               Liqui_Int.Cod_Producto   EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
    IF AVAILABLE Liqui_Int THEN DO:
       IF Creditos.FOR_Pago EQ 2 THEN /*pago por nomina*/
          ASSIGN W_CtaIntCor_Db = Liqui_Int.CtaDb_LiqAso
                 W_CtaIntCor_Cr = Liqui_Int.CtaCr_LiqAso
                 W_CtaIntMor    = Liqui_Int.CtaDb_MoraAso
                 W_CtaDifCob_Db = Liqui_Int.CtaDb_DifCobAso
                 W_CtaDifCob_Cr = Liqui_Int.CtaCr_DifCobAso.
       ELSE
          ASSIGN W_CtaIntCor_Db = Liqui_Int.CtaDb_Liq
                 W_CtaIntCor_Cr = Liqui_Int.CtaCr_Liq
                 W_CtaIntMor    = Liqui_Int.CtaDb_Mora
                 W_CtaDifCob_Db = Liqui_Int.CtaDb_DifCob
                 W_CtaDifCob_Cr = Liqui_Int.CtaCr_DifCob.
    END.
    ELSE DO:
        MESSAGE "No se ha encontrado la configuracón de cuentas en Liqui_Int" SKIP
                "de interes para el producto de créditos" SKIP(1)
                "Comuniquese con el Administrador!" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
    
    IF W_CtaIntCor_Db EQ "" OR W_CtaIntMor EQ "" OR 
       W_CtaDifCob_Db EQ "" OR W_OrdCasDB  EQ "" OR 
       W_OrdCasCR     EQ "" OR W_Castigo   EQ "" THEN DO:
       MESSAGE "Falta una o varias cuentas de interes por configurar" SKIP
               "para el producto." SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.
 END.
 ELSE DO:
   MESSAGE "No se ha encontrado la configuracón de cuentas de CortoLargo" SKIP
           "para el producto de créditos" SKIP(1)
           "Comuniquese con el Administrador!" VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
 END.
 
/* IF Creditos.Agencia NE W_Agencia THEN DO:
   FIND CortoLargo WHERE CortoLargo.Agencia           EQ Creditos.Agencia
                   AND   CortoLargo.Clase_Producto    EQ 2
                   AND   CortoLargo.Cod_Producto      EQ Creditos.Cod_Credito
                   AND   CortoLargo.Cta_ContingenteDB NE ""
                   AND   CortoLargo.Cta_ContingenteCR NE ""
                   AND   CortoLargo.Comprobante       NE 0 NO-LOCK NO-ERROR.
   IF AVAILABLE(CortoLargo) THEN                                         
      W_CtaSYA_FTE = CortoLargo.Cta_SYA.                                   
   ELSE DO:                                                                
     MESSAGE "No se encontra la cuenta de sucursales y agencias" SKIP      
             "de la Agencia de donde es el crédito" SKIP(1)                
             "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.   
     RETURN ERROR.                                                         
   END.                                                                    
 END.*/
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
  DISPLAY W_Jurid W_Total W_CtaGasto Nom-10 W_CastigInt Nom1 W_CastigPol Nom-4 
          W_CtaDbInt Nom-2 W_CtaCrInt Nom-3 W_Castigo Nom-7 W_CtaDbPol Nom-5 
          W_CtaCrPol Nom-6 W_OrdCasDB Nom-8 W_OrdCasCR Nom-9 
      WITH FRAME F_Cre IN WINDOW Wwin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Int_Anticipado Creditos.Fec_Desembolso 
          Creditos.Int_MoraDifCob Creditos.Tasa Creditos.Fec_Pago 
          Creditos.Int_MorCobrar Creditos.Plazo Creditos.Sdo_Proyectado 
          Creditos.Int_Corrientes Creditos.Fec_UltPago Creditos.Categoria 
          Creditos.Int_DifCobro Creditos.Cuo_Pagadas Creditos.Val_Atraso 
          Creditos.Sdo_Capital Creditos.Dias_Atraso Creditos.Cuo_Atraso 
          Creditos.Cuota Creditos.Provision Creditos.Provision_Interes 
          Creditos.Fec_Reestructurado 
      WITH FRAME F_Cre IN WINDOW Wwin.
  ENABLE RECT-296 RECT-299 RECT-300 RECT-301 RECT-302 W_CtaGasto W_CastigInt 
         W_CastigPol W_CtaDbInt W_CtaCrInt W_Castigo W_CtaDbPol W_CtaCrPol 
         W_OrdCasDB Btn_Grabar Btn_Salir W_OrdCasCR 
      WITH FRAME F_Cre IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cre}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba_Mov Wwin 
PROCEDURE Graba_Mov :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN Mov_Contable.Agencia        = Creditos.Agencia
         Mov_Contable.Comprobante    = W_Cbte
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Proc.Castigo"
         Mov_Contable.Usuario        = W_Usuario
         Mov_contable.Nit            = Creditos.Nit
         Mov_Contable.Destino        = 10
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Num_Documento  = W_NumCbt
         Mov_Contable.Doc_Referencia = STRING(Creditos.Num_Credito)
         Mov_contable.Enlace         = STRING(W_NumCbt)
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Wwin 
PROCEDURE initializeObject :
RUN SUPER.
  Wwin:TITLE = P_NomIns + ", Prog.W-ProRec_Castigo.W.".
  
  FIND Operacion WHERE Operacion.Cod_Operacion EQ P_CodOpe NO-LOCK NO-ERROR.
  IF AVAILABLE Operacion THEN 
     W_Cbte = Operacion.Comprobante.
  ELSE 
    MESSAGE "No se ha encontrado la operación de créditos" SKIP
            "Comuniquese con el Administrador!" SKIP
            "La Operacion pasada fue: " P_CodOpe VIEW-AS ALERT-BOX ERROR.
  
  FIND FIRST Creditos WHERE Creditos.Nit         EQ P_NitCli AND
                            Creditos.Cod_Credito EQ P_CodCre AND
                            Creditos.Num_Credito EQ P_NumCre AND
                            Creditos.Tip_Credito EQ P_TipCre AND
                            Creditos.Estado      EQ 2  NO-LOCK NO-ERROR.
  
  IF AVAILABLE Creditos THEN DO:
     FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.          
     FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito AND
                             Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito NO-LOCK NO-ERROR.
     IF AVAILABLE Pro_Creditos THEN DO:
        RUN CortoLargoCreditos NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
           APPLY "CHOOSE" TO Btn_Salir IN FRAME F_Cre.
     END.
     ELSE MESSAGE "No se Halló el producto de Creditos" VIEW-AS ALERT-BOX ERROR.
             
     RUN Mostrar_Credito.
  END.
  
  IF NOT AVAIL(Creditos) OR NOT AVAIL(Pro_Creditos) OR NOT AVAIL(Operacion)
  OR NOT AVAIL(Clientes) THEN
     APPLY "CHOOSE" TO Btn_Salir IN FRAME F_Cre.
     
  HIDE FRAME F_Acuerdos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Credito Wwin 
PROCEDURE Mostrar_Credito :
DO WITH FRAME F_Cre:
   ASSIGN Creditos.Fec_Desembolso:SCREEN-VALUE = STRING(Creditos.Fec_Desembolso)
          Creditos.Fec_Pago:SCREEN-VALUE           = STRING(Creditos.Fec_Pago)
          Creditos.Fec_Ultpago:SCREEN-VALUE        = STRING(Creditos.Fec_Ultpago)
          Creditos.Categoria:SCREEN-VALUE          = STRING(Creditos.Categoria)
          Creditos.INT_DifCobro:SCREEN-VALUE = STRING(Creditos.INT_DifCobro)
          Creditos.INT_MoraDifCob:SCREEN-VALUE = STRING(Creditos.INT_MoraDifCob)
          W_Jurid:SCREEN-VALUE = STRING(Creditos.Polizas + Creditos.Honorarios + Creditos.Costas)
          Creditos.Sdo_Proyectado:SCREEN-VALUE = STRING(Creditos.Sdo_Proyectado)
          Creditos.Cuo_Pagadas:SCREEN-VALUE = STRING(Creditos.Cuo_Pagadas)
          Creditos.Tasa:SCREEN-VALUE       = STRING(Creditos.Tasa).

   ASSIGN Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Val_Atraso)
          Creditos.Dias_Atraso:SCREEN-VALUE = STRING(Creditos.Dias_Atraso)
          Creditos.Cuo_Atraso:SCREEN-VALUE = STRING(Creditos.Cuo_Atraso)
          Creditos.Provision:SCREEN-VALUE = STRING(Creditos.Provision)
          Creditos.Fec_Reestructurado:SCREEN-VALUE = STRING(Creditos.Fec_Reestructurado)
          Creditos.Cuota:SCREEN-VALUE = STRING(Creditos.Cuota)
          Creditos.INT_Corrientes:SCREEN-VALUE = STRING(Creditos.INT_Corrientes)
          Creditos.INT_Anticipado:SCREEN-VALUE = STRING(Creditos.INT_Anticipado)
          Creditos.Sdo_Capital:SCREEN-VALUE = STRING(Creditos.Sdo_Capital)
          Creditos.INT_MorCobrar:SCREEN-VALUE = STRING(Creditos.INT_MorCobrar)
          Creditos.Plazo:SCREEN-VALUE = STRING(Creditos.Plazo)
          W_Total = Creditos.Sdo_Capital  + Creditos.INT_MorCobrar  + Creditos.INT_MoraDifCob + 
                    Creditos.Polizas      + Creditos.Honorarios     + Creditos.Costas         +
                    Creditos.INT_DifCobro + Creditos.INT_Corrientes - Creditos.INT_Anticipado
          W_Total:SCREEN-VALUE = STRING(W_Total).

   FIND FIRST CortoLargo WHERE CortoLargo.Cod_Produc EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
          
   ASSIGN W_Castigo:SCREEN-VALUE  = CortoLargo.Cta_Castigo
          W_Castigo
          W_OrdCasDB:SCREEN-VALUE = CortoLargo.Cta_OrdCasDb
          W_OrdCasDB
          W_OrdCasCr:SCREEN-VALUE = CortoLargo.Cta_OrdCasCr
          W_OrdCasCr
          W_CtaDBInt:SCREEN-VALUE = CortoLargo.Cta_OrdCasDb   /*Iden K no existe config.Int*/
          W_CtaDBInt
          W_CtaCrInt:SCREEN-VALUE = CortoLargo.Cta_OrdCasCr   /*Iden K no existe config.Int*/
          W_CtaCrInt
          W_CtaDBPol:SCREEN-VALUE = CortoLargo.Cta_OrdCasDb   /*Iden K no existe config.Int*/
          W_CtaDBPol
          W_CtaCrPol:SCREEN-VALUE = CortoLargo.Cta_OrdCasCr   /*Iden K no existe config.Int*/
          W_CtaCrPol.

   FIND LAST CarteraVencida WHERE CarteraVencida.Cod_Produc EQ Creditos.Cod_Credito 
                              AND CarteraVencida.Cod_Calif  GT 2 NO-LOCK NO-ERROR.

   ASSIGN W_Castigo:SCREEN-VALUE   = CarteraVencida.Cta_AsoPrvNaCr
          W_Castigo
          W_CtaGasto:SCREEN-VALUE  = CarteraVencida.Cta_AsoPrvNaDb
          W_CtaGasto
          W_CastigInt:SCREEN-VALUE = CarteraVencida.Cta_AsoIntNaCr 
          W_CastigInt
          W_CastigPol:SCREEN-VALUE = CarteraVencida.Cta_CostasCr 
          W_CastigPol.

   APPLY "Leave" TO W_Castigo.
   APPLY "Leave" TO W_OrdCasDB.
   APPLY "Leave" TO W_OrdCasCR.
   APPLY "Leave" TO W_CtaDBInt.
   APPLY "Leave" TO W_CtaCrInt.
   APPLY "Leave" TO W_CtaDBPol.
   APPLY "Leave" TO W_CtaCrPol.
   APPLY "Leave" TO W_CtaGasto.
   APPLY "Leave" TO W_CastigInt.
   APPLY "Leave" TO W_CastigPol.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir Wwin 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
 ------------------------------------------------------------------------------*/
 DEFI VAR TotD   LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TotC   LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TTotD  LIKE Mov_Contable.Db INIT 0.
 DEFI VAR TTotC  LIKE Mov_Contable.Db INIT 0.

 {Incluido\RepEncabezado.I}

 ASSIGN W_Reporte = "Cpte Resumen : Contabilización Castigo-Crèdito      Fecha del Informe: " +
                     STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
        W_EncColumna = "Comprobante: " + STRING(Comprobantes.Comprobante,"99") + "-" + 
                       STRING(Comprobantes.Secuencia,"99999999") + "-" + Comprobantes.Nombre.

 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.

 FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                         AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                         AND Mov_Contable.Fec_Contable  EQ W_Fecha NO-LOCK
                             BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta
                                   BY Mov_Contable.Nit:
     ASSIGN TotD  = TotD  + Mov_Contable.Db
            TTotD = TTotD + Mov_Contable.Db
            TotC  = TotC  + Mov_Contable.Cr
            TTotC = TTotC + Mov_Contable.Cr.

     IF LAST-OF(Mov_Contable.Nit) THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta
                             AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
        DISPLAY Mov_Contable.Agencia   LABEL "Ag."
                Mov_Contable.Cuenta    LABEL "Cta-Contable"
                Cuentas.Nombre         LABEL "Descripciòn de la Cuenta" WHEN AVAIL(Cuentas)
                Mov_Contable.Nit       LABEL "Ced/Nit"
                Mov_Contable.Doc_Refer LABEL "Doc-Refer"
                TotD                   LABEL "TOTAL DEBITOS"  FORM "->>>>>>,>>>,>>9.99"
                TotC                   LABEL "TOTAL CREDITOS" FORM "->>>>>>,>>>,>>9.99"
            WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

        ASSIGN TotD  = 0
               TotC  = 0.
     END.

 END.

 DISPLAY SKIP(1)
         "                     TOTAL FINAL------------>                                       ------------------ ------------------"
         SKIP
         "                                                                                   "
         TTotD      FORM "->>>>>>,>>>,>>9.99"
         TTotC      FORM "->>>>>>,>>>,>>9.99"
            WITH DOWN WIDTH 180 FRAME FT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transaccion Wwin 
PROCEDURE Transaccion :
DO TRANSACTION ON ERROR UNDO:
   FIND Comprobantes WHERE Comprobante.Agencia      EQ Creditos.Agencia AND 
                           Comprobantes.Comprobante EQ 3 NO-ERROR.
   IF AVAILABLE Comprobantes THEN 
      ASSIGN W_Cbte                 = 3
             W_NumCbt               = Comprobantes.Secuencia + 1
             Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
  
   FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
   IF (creditos.cod_credito NE 570 AND creditos.cod_credito NE 870) THEN
       RUN AboCredito.R    /*Distribuye abonos en Créditos,graba Mov_creditos,Mov_Contable y PlanPagos*/
          (INPUT YES,
           INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
           INPUT Creditos.Num_Credito,W_Total,
           INPUT W_Cbte,W_NumCbt,0,1,
           OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IDifCobMor, OUTPUT P_IMora,
           OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capital,
           OUTPUT P_VlrNoDist) NO-ERROR.
   ELSE
       RUN AboCredito_cuporotativo.R    /*Distribuye abonos en Créditos,graba Mov_creditos,Mov_Contable y PlanPagos*/
          (INPUT YES,
           INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
           INPUT Creditos.Num_Credito,W_Total,
           INPUT W_Cbte,W_NumCbt,0,1,
           OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IDifCobMor, OUTPUT P_IMora,
           OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capital,
           OUTPUT P_VlrNoDist) NO-ERROR.


   IF ERROR-STATUS:ERROR OR P_VlrNoDist NE 0 THEN DO:
      MESSAGE "Prog.AboCredito.P...Retornò Error, Ò," SKIP
              "Retornò Valor no-distribuìdo $" P_VlrNoDist SKIP
              "               No se acepta el Castigo." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.                                                                                 
   END. 
     
   FIND CURRENT Creditos NO-ERROR.
   ASSIGN Creditos.Estado = 5.
   
   RUN Contabilizar_Local NO-ERROR.  /*Solo contrapartida y Ctas-Orden, el resto lo ejecutò el Prog.AboCredito.P */
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Error al contabiliar" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.
   
   FIND CURRENT Creditos NO-LOCK NO-ERROR.
END.  /*Fin Tx*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

