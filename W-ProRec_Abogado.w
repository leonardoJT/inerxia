&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME

/* Connected Databases 
          bdcentral        PROGRESS
*/

&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE VAR Op_Capital AS INTEGER INITIAL "020101001".
DEFINE VAR Op_Mora AS INTEGER INITIAL "020101002".

/* oakley */

DEFINE VAR Op_IntCor  LIKE Operacion.Cod_Operacion INITIAL "020101003".
DEFINE VAR Op_IntDif  LIKE Operacion.Cod_Operacion INITIAL "020101004".
DEFINE VAR Op_IntAnt  LIKE Operacion.Cod_Operacion INITIAL "020101005".
DEFINE VAR Op_Costas  LIKE Operacion.Cod_Operacion INITIAL "020101008".
DEFINE VAR Op_Honorarios LIKE Operacion.Cod_Operacion INITIAL "020101007".
DEFINE VAR Op_Polizas LIKE Operacion.Cod_Operacion INITIAL "020101006".

{INCLUIDO\VARIABLE.I "SHARED"}
DEFINE INPUT PARAMETER P_NomIns AS CHARACTER FORMAT "X(50)".
DEFINE INPUT PARAMETER P_NitCli LIKE Clientes.Nit.
DEFINE INPUT PARAMETER P_CodCre LIKE Creditos.Cod_Credito.
DEFINE INPUT PARAMETER P_TipCre LIKE Creditos.Tip_Credito.
DEFINE INPUT PARAMETER P_NumCre LIKE Creditos.Num_Credito.
DEFINE INPUT PARAMETER P_CodOpe LIKE Operacion.Cod_Operacion.
DEFINE INPUT PARAMETER P_Progra AS INTEGER FORMAT "9".
DEFINE VAR W_Cuo AS INTEGER.

DEFINE VAR P_VrInteres LIKE Creditos.Sdo_Capital.
DEFINE VAR P_VrCapital LIKE Creditos.Sdo_Capital.
DEFINE VAR T_VrInteres LIKE Creditos.Sdo_Capital.
DEFINE VAR T_VrCapital LIKE Creditos.Sdo_Capital.
DEFINE VAR T_VrMora    LIKE Creditos.INT_MorCobrar.

DEFINE VARIABLE Tot_intAnticipado LIKE creditos.sdo_capital INITIAL 0.
DEFINE VARIABLE Tot_intDifCobro LIKE creditos.sdo_capital INITIAL 0.
DEFINE VARIABLE Tot_IntCorriente LIKE creditos.sdo_capital INITIAL 0.
DEFINE VARIABLE Tot_Mora LIKE creditos.sdo_capital INITIAL 0.
DEFINE VARIABLE Tot_Capital LIKE creditos.sdo_capital INITIAL 0.
DEFINE VARIABLE Tot_Costas LIKE creditos.sdo_capital INITIAL 0.
DEFINE VARIABLE Tot_Honorarios LIKE creditos.sdo_capital INITIAL 0.
DEFINE VARIABLE Tot_Polizas LIKE creditos.sdo_capital INITIAL 0.


DEFINE VAR W_Ok AS LOGICAL.


DEFINE VAR W_CtaCap        LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaIntCor_Db  LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaIntCor_Cr  LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaIntMor     LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaIntAnt     LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaDifCob_db  LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaDifCob_Cr  LIKE Cuentas.Cuenta.
DEFINE VAR W_Caja          LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaSya_Des    LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaSya_Fte    LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaCosDB      LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaHonDB      LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaPolDB      LIKE Cuentas.Cuenta.


DEFINE VAR W_Cbte          LIKE Comprobantes.Comprobante.
DEFINE VAR W_NumCbt        LIKE Comprobantes.Secuencia.
DEFINE VAR W_Canje         LIKE Bancos.Dia_Canje.
DEFINE VAR W_ValChe        LIKE Creditos.Sdo_Capital.
DEFINE VAR W_ValEfe        LIKE Creditos.Sdo_Capital.

DEFINE VARIABLE W_NumSeq   AS INTEGER FORMAT "9999999".

DEFINE VAR P_Poliza LIKE Creditos.Sdo_Capital.
DEFINE VAR P_Honora LIKE Creditos.Sdo_Capital.
DEFINE VAR P_Costas LIKE Creditos.Sdo_Capital.
DEFINE VAR P_IMorDifC LIKE creditos.sdo_capital.
DEFINE VAR P_IMora  LIKE Creditos.Sdo_Capital.
DEFINE VAR P_IDifCob   LIKE Creditos.Sdo_Capital.
DEFINE VAR P_ICte      LIKE Creditos.Sdo_Capital.
DEFINE VAR P_IAntic    LIKE Creditos.Sdo_Capital.
DEFINE VAR P_Capital   LIKE Creditos.Sdo_Capital.
DEFINE VAR P_VlrNoDist LIKE Creditos.Sdo_Capital.
DEFINE VAR vTime AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Acuerdos
&Scoped-define BROWSE-NAME B_Acuerdos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cobros Creditos

/* Definitions for BROWSE B_Acuerdos                                    */
&Scoped-define FIELDS-IN-QUERY-B_Acuerdos Cobros.Fec_Acuerdo Cobros.Fec_Compromiso Cobros.Val_Compromiso Cobros.Fec_Cumplimiento   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Acuerdos   
&Scoped-define SELF-NAME B_Acuerdos
&Scoped-define QUERY-STRING-B_Acuerdos FOR EACH Cobros WHERE Cobros.Nit EQ Creditos.Nit     AND Cobros.Num_Credito EQ Creditos.Num_Credito NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Acuerdos OPEN QUERY {&SELF-NAME} FOR EACH Cobros WHERE Cobros.Nit EQ Creditos.Nit     AND Cobros.Num_Credito EQ Creditos.Num_Credito NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Acuerdos Cobros
&Scoped-define FIRST-TABLE-IN-QUERY-B_Acuerdos Cobros


/* Definitions for FRAME F_Acuerdos                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Acuerdos ~
    ~{&OPEN-QUERY-B_Acuerdos}

/* Definitions for FRAME F_Cre                                          */
&Scoped-define FIELDS-IN-QUERY-F_Cre Creditos.Fec_Desembolso ~
Creditos.Fec_Pago Creditos.Fec_ProxLiquidacion Creditos.Tasa ~
Creditos.Fec_UltPago Creditos.Cuota Creditos.Fec_UltLiquidacion ~
Creditos.Sdo_Capital Creditos.Plazo Creditos.Int_Corrientes ~
Creditos.Sdo_Proyectado Creditos.Int_DifCobro Creditos.Cuo_Pagadas ~
Creditos.Int_MorCobrar Creditos.Val_Atraso Creditos.Int_Anticipado ~
Creditos.Dias_Atraso Creditos.Cuo_Atraso Creditos.Provision ~
Creditos.Fec_Reestructurado 
&Scoped-define QUERY-STRING-F_Cre FOR EACH Creditos SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Cre OPEN QUERY F_Cre FOR EACH Creditos SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Cre Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-F_Cre Creditos


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B_Acuerdos BUTTON-182 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-182 
     LABEL "Salir" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_Acuerdos 
     LABEL "Ver Acuerdos" 
     SIZE 25 BY 1.12.

DEFINE BUTTON Btn_Grabar 
     LABEL "Grabar Operación" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_Salir" 
     SIZE 15 BY 1.62.

DEFINE VARIABLE W_AboCap AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Abono a Capital" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboCostas AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Abono  Costas" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboHonora AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Abono a Honorarios" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboInt AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Interés Corriente" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboIntAnt AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>9.99" INITIAL 0 
     LABEL "Interes Anticipado" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15 .

DEFINE VARIABLE W_AboIntDifCob AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>9.99" INITIAL 0 
     LABEL "Interes Dif. Cobro" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15 .

DEFINE VARIABLE W_AboIntMor AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>9.99" INITIAL 0 
     LABEL "Interés de Mora" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15 .

DEFINE VARIABLE W_AboPoliza AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Abono a Pólizas" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NumBan AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Banco" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NumChe AS CHARACTER FORMAT "X(10)":U 
     LABEL "Número de Cheque" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Valor AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor a Pagar" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ValPag AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor Total del Abono" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 7 FGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE R_TipCon AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Efectivo", 1,
"Cheque", 2
     SIZE 21 BY .81
     FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 11.31.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_Acuerdos FOR 
      Cobros SCROLLING.

DEFINE QUERY F_Cre FOR 
      Creditos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_Acuerdos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Acuerdos Wwin _FREEFORM
  QUERY B_Acuerdos NO-LOCK DISPLAY
      Cobros.Fec_Acuerdo COLUMN-LABEL "Fecha Acuerdo" FORMAT "99/99/99":U
      Cobros.Fec_Compromiso COLUMN-LABEL "Compromiso" FORMAT "99/99/99":U
      Cobros.Val_Compromiso COLUMN-LABEL "Valor" FORMAT ">>>,>>>,>>9":U
      Cobros.Fec_Cumplimiento COLUMN-LABEL "Cumplimiento" FORMAT "99/99/9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 56 BY 3.15
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Cre
     W_Valor AT ROW 1.27 COL 74 COLON-ALIGNED
     R_TipCon AT ROW 1.54 COL 37 NO-LABEL
     Creditos.Fec_Desembolso AT ROW 2.08 COL 14 COLON-ALIGNED
          LABEL "Desembolso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NumChe AT ROW 2.27 COL 42 COLON-ALIGNED
     W_AboCap AT ROW 2.54 COL 74 COLON-ALIGNED
     Creditos.Fec_Pago AT ROW 2.88 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NumBan AT ROW 3.15 COL 42 COLON-ALIGNED
     W_AboInt AT ROW 3.38 COL 74 COLON-ALIGNED
     Creditos.Fec_ProxLiquidacion AT ROW 3.69 COL 14 COLON-ALIGNED
          LABEL "Prox. Liquidación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Tasa AT ROW 4.23 COL 42 COLON-ALIGNED
          LABEL "Tasa"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_AboIntAnt AT ROW 4.23 COL 74 COLON-ALIGNED
     Creditos.Fec_UltPago AT ROW 4.5 COL 14 COLON-ALIGNED
          LABEL "Último Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuota AT ROW 5.04 COL 42 COLON-ALIGNED
          LABEL "Cuota"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 4
     W_AboIntDifCob AT ROW 5.08 COL 74 COLON-ALIGNED
     Creditos.Fec_UltLiquidacion AT ROW 5.31 COL 14 COLON-ALIGNED
          LABEL "Última Liquidación"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Capital AT ROW 5.85 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_AboIntMor AT ROW 5.92 COL 74 COLON-ALIGNED
     Creditos.Plazo AT ROW 6.12 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Corrientes AT ROW 6.69 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_AboCostas AT ROW 6.77 COL 74 COLON-ALIGNED
     Creditos.Sdo_Proyectado AT ROW 6.92 COL 14 COLON-ALIGNED
          LABEL "Saldo Proyectado"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_DifCobro AT ROW 7.54 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_AboHonora AT ROW 7.62 COL 74 COLON-ALIGNED
     Creditos.Cuo_Pagadas AT ROW 7.73 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_MorCobrar AT ROW 8.38 COL 42 COLON-ALIGNED
          LABEL "Interes Mora x Cobrar"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_AboPoliza AT ROW 8.46 COL 74 COLON-ALIGNED
     Creditos.Val_Atraso AT ROW 8.54 COL 14 COLON-ALIGNED
          LABEL "Valor Atraso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Int_Anticipado AT ROW 9.23 COL 42 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Dias_Atraso AT ROW 9.35 COL 14 COLON-ALIGNED
          LABEL "Días de Atraso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.29 BY 11.96
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Cre
     W_ValPag AT ROW 9.62 COL 74 COLON-ALIGNED
     Creditos.Cuo_Atraso AT ROW 10.15 COL 14 COLON-ALIGNED
          LABEL "Cuotas Atrasadas"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Btn_Grabar AT ROW 10.69 COL 67
     Btn_Salir AT ROW 10.69 COL 83
     Creditos.Provision AT ROW 10.96 COL 14 COLON-ALIGNED
          LABEL "Provisión"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Acuerdos AT ROW 10.96 COL 31
     Creditos.Fec_Reestructurado AT ROW 11.77 COL 14 COLON-ALIGNED
          LABEL "Reestructuración"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     "Información del Crédito" VIEW-AS TEXT
          SIZE 20 BY .81 AT ROW 1.15 COL 3
          FGCOLOR 7 FONT 5
     RECT-296 AT ROW 1.54 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.29 BY 11.96
         BGCOLOR 17 FONT 4.

DEFINE FRAME F_Acuerdos
     B_Acuerdos AT ROW 1.27 COL 2
     BUTTON-182 AT ROW 4.77 COL 42
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 6.92
         SIZE 58 BY 5.92
         BGCOLOR 17 FONT 4
         TITLE "Acuerdos de Pago".


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
         TITLE              = "Recaudos de Creditos En Abogado"
         COLUMN             = 12.14
         ROW                = 9.88
         HEIGHT             = 11.96
         WIDTH              = 97.29
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
/* REPARENT FRAME */
ASSIGN FRAME F_Acuerdos:FRAME = FRAME F_Cre:HANDLE.

/* SETTINGS FOR FRAME F_Acuerdos
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB B_Acuerdos 1 F_Acuerdos */
ASSIGN 
       FRAME F_Acuerdos:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Cre
                                                                        */
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
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Fec_ProxLiquidacion IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Reestructurado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltLiquidacion IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Anticipado IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_Corrientes IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_DifCobro IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_MorCobrar IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Plazo IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Provision IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Capital IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Proyectado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Tasa IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Val_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN W_AboCap IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AboCostas IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AboHonora IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AboInt IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AboIntAnt IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AboIntDifCob IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AboIntMor IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AboPoliza IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NumBan IN FRAME F_Cre
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_NumBan:HIDDEN IN FRAME F_Cre           = TRUE.

/* SETTINGS FOR FILL-IN W_NumChe IN FRAME F_Cre
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_NumChe:HIDDEN IN FRAME F_Cre           = TRUE.

/* SETTINGS FOR FILL-IN W_ValPag IN FRAME F_Cre
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Acuerdos
/* Query rebuild information for BROWSE B_Acuerdos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cobros WHERE Cobros.Nit EQ Creditos.Nit
    AND Cobros.Num_Credito EQ Creditos.Num_Credito NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Cobros.Nit"
     _Query            is OPENED
*/  /* BROWSE B_Acuerdos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cre
/* Query rebuild information for FRAME F_Cre
     _TblList          = "bdCentral.Creditos"
     _Query            is OPENED
*/  /* FRAME F_Cre */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON END-ERROR OF Wwin /* Recaudos de Creditos En Abogado */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON WINDOW-CLOSE OF Wwin /* Recaudos de Creditos En Abogado */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME Btn_Acuerdos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Acuerdos Wwin
ON CHOOSE OF Btn_Acuerdos IN FRAME F_Cre /* Ver Acuerdos */
DO:
  VIEW FRAME F_Acuerdos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar Wwin
ON CHOOSE OF Btn_Grabar IN FRAME F_Cre /* Grabar Operación */
DO:
    vTime = TIME.

    ASSIGN FRAME F_Cre
        W_AboCap
        R_TipCon
        W_NumChe
        W_NumBan.

    IF R_TipCon EQ 2 AND (W_NumChe EQ "" OR W_NumBan EQ 0) THEN DO:
        MESSAGE "Debe digitarse el número del cheque y del banco. Rectifique!"
            VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO W_NumChe.
        RETURN NO-APPLY.
    END.

    IF R_TipCon EQ 2 THEN DO: /*verifica que exista el banco en BANCOS*/
        W_Canje = 0.

        FIND Bancos WHERE Bancos.Cod_Compensa EQ W_NumBan NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Bancos THEN DO:
            MESSAGE "No existe el banco dentro de la tabla de Bancos" SKIP
                    "Cominiquese con el Administrador para que lo cree" SKIP
                VIEW-AS ALERT-BOX ERROR.
            APPLY "choose" TO Btn_Salir IN FRAME F_Cre.
        END.
        ELSE
            W_Canje = Bancos.Dia_Canje.
    END.

    DO TRANSACTION:
        RUN Transaccion NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Se ha producido un error en la grabacion de la transacción" SKIP
                    "consulte con el Administrador del Sistema"
                VIEW-AS ALERT-BOX ERROR.
        END.
    END.

    DO TRANSACTION:
        RUN Contabilizar NO-ERROR.
    END.

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


&Scoped-define FRAME-NAME F_Acuerdos
&Scoped-define SELF-NAME BUTTON-182
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-182 Wwin
ON CHOOSE OF BUTTON-182 IN FRAME F_Acuerdos /* Salir */
DO:
  HIDE FRAME F_Acuerdos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME R_TipCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_TipCon Wwin
ON VALUE-CHANGED OF R_TipCon IN FRAME F_Cre
DO:
  IF SELF:SCREEN-VALUE EQ "2" THEN DO:
     ENABLE W_NumChe W_NumBan WITH FRAME F_Cre.
  END.
  ELSE DO:
    ASSIGN W_NumChe:SCREEN-VALUE = ""
           W_NumChe:HIDDEN = YES
           W_NumBan:SCREEN-VALUE = ""
           W_NumBan:HIDDEN = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Valor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Valor Wwin
ON LEAVE OF W_Valor IN FRAME F_Cre /* Valor a Pagar */
DO:
  DEFINE VAR W_NroDia AS INTEGER FORMAT "99".
  DEFINE VAR W_NroPer AS INTEGER FORMAT "99".
  DEFINE VAR W_CuoTras AS INTEGER FORMAT "999".
  DEFINE VAR W_Nrocuopag AS INTEGER FORMAT "999".
DO WITH FRAME F_Cre:
  CASE Creditos.Per_Pago:
    WHEN 1 THEN ASSIGN W_NroDia = 7
                       W_NroPer = 52
                       W_CuoTras = ((W_Fecha - Creditos.Fec_Desembolso) / 7) + 1.
    WHEN 3 THEN ASSIGN W_NroDia = 15
                       W_NroPer = 24
                       W_CuoTras = ((W_Fecha - Creditos.Fec_Desembolso) / 15) + 1.
    WHEN 4 THEN ASSIGN W_NroDia = 30
                       W_NroPer = 12
                       W_CuoTras = ((W_Fecha - Creditos.Fec_Desembolso) / 30) + 1.
  END CASE.
  FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito AND
       Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito NO-LOCK NO-ERROR.
          
  ASSIGN W_Nrocuopag = Creditos.Cuo_pagadas
         P_VrInteres = 0
         P_VrCapital = 0.
     
   RUN AboCredito.R         /*Distribuye abonos en Créditos,graba Mov_creditos y PlanPagos*/
            /*(INPUT NO,
             INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
             INPUT Creditos.Num_Credito,DECIMAL(SELF:SCREEN-VALUE),
             INPUT 0,0,0,
             OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IMora,
             OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capital,
             OUTPUT P_VlrNoDist).     */
       (INPUT NO,
             INPUT Creditos.Agencia,
             Creditos.Cod_Credito,
             Creditos.Nit,
             INPUT Creditos.Num_Credito,
             DECIMAL(SELF:SCREEN-VALUE),
             INPUT 0,
             INPUT 0,
             0,
             0,
             OUTPUT P_Poliza,
             OUTPUT P_Honora,
             OUTPUT P_Costas,
             OUTPUT P_IMorDifC,
             OUTPUT P_IMora,
             OUTPUT P_IDifCob,
             OUTPUT P_ICte,
             OUTPUT P_IAntic,
             OUTPUT P_Capital,
             OUTPUT P_VlrNoDist).
  
  IF P_VlrNoDist GT 0 THEN DO:
     MESSAGE "El valor digitado es mayor que el debido pagar" SKIP
             "el sobrante de $" STRING(P_VlrNoDist,">>,>>>,>>9") " Se restará" SKIP
             "para que de el pago completo" VIEW-AS ALERT-BOX INFORMATION.
     RUN AboCredito.R         /*Distribuye abonos en Créditos,graba Mov_creditos y PlanPagos*/
            (INPUT NO,
             INPUT Creditos.Agencia,
             Creditos.Cod_Credito,
             Creditos.Nit,
             INPUT Creditos.Num_Credito,
             (DECIMAL(SELF:SCREEN-VALUE) - P_VlrNoDist),
             INPUT 0,
             INPUT 0,
             0,
             0,
             OUTPUT P_Poliza,
             OUTPUT P_Honora,
             OUTPUT P_Costas,
             OUTPUT P_IMorDifC,
             OUTPUT P_IMora,
             OUTPUT P_IDifCob,
             OUTPUT P_ICte,
             OUTPUT P_IAntic,
             OUTPUT P_Capital,
             OUTPUT P_VlrNoDist).
  END.
  ASSIGN W_AboInt:SCREEN-VALUE = STRING(P_ICte)
         W_AboCap:SCREEN-VALUE = STRING(P_Capital)
         W_AboIntMor:SCREEN-VALUE = STRING(P_IMora)
         W_AboIntAnt:SCREEN-VALUE = STRING(P_IAntic)
         W_AboIntDifCob:SCREEN-VALUE = STRING(P_IDifCob)
         W_AboPoliza:SCREEN-VALUE    = STRING(P_Poliza)
         W_AboHonora:SCREEN-VALUE    = STRING(P_Honora)
         W_AboCostas:SCREEN-VALUE    = STRING(P_Costas)
         W_ValPag:SCREEN-VALUE =
           STRING(P_ICte + P_Capital + P_IMora + P_IAntic + P_IDifCob + P_Poliza + P_Honora + P_Costas).
         
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Acuerdos
&Scoped-define BROWSE-NAME B_Acuerdos
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar Wwin 
PROCEDURE Contabilizar :
FIND Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia
                    AND Comprobantes.Comprobante EQ W_Cbte NO-ERROR.
IF NOT AVAILABLE Comprobantes THEN DO:
    MESSAGE "No se ha encontrado el comprobante para la contabilización" SKIP
            "Rectifique con el Administrador!"
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

IF AVAILABLE Comprobantes THEN
    ASSIGN W_Cbte = Comprobantes.Comprobante.
ELSE DO:
    MESSAGE "No se ha encontrado el comprobante para la agencia"
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

FOR EACH Taquilla WHERE Taquilla.Usuario EQ W_Usuario
                    AND Taquilla.Contabiliza EQ NO BREAK BY Taquilla.Nro_Transaccion:
    IF FIRST-OF(Taquilla.Nro_Transaccion) THEN DO:
        FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
        IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN
            W_Cbte = Operacion.Comprobante.
        ELSE DO:
            RUN MostrarMensaje IN W_Manija (INPUT 143,OUTPUT W_Ok).
            RETURN ERROR.
        END.
    END.

    RUN Contabilizar_Partidas NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
    ELSE
        Taquilla.Contabiliza = YES.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Partidas Wwin 
PROCEDURE Contabilizar_Partidas :
DEFINE VAR WComentario AS CHARACTER FORMAT "X(30)".

CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia        = Taquilla.Agencia
       Mov_Contable.Comprobante    = W_Cbte
       Mov_Contable.Cuenta         = Taquilla.Cuenta
       Mov_Contable.Fec_Contable   = W_Fecha
       Mov_Contable.Comentario     = Taquilla.Descripcion
       Mov_Contable.Usuario        = Taquilla.Usuario
       Mov_contable.Nit            = Taquilla.Nit
       Mov_Contable.Cen_Costos     = 999
       Mov_Contable.Destino        = Taquilla.Age_Destino
       Mov_Contable.Num_Documento  = INTEGER(Taquilla.Nro_Transaccion)
       Mov_Contable.Doc_Referencia = Taquilla.Num_Retcheque
       Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)
       Mov_Contable.Fec_Grabacion  = TODAY
       Mov_Contable.Hora           = TIME
       Mov_Contable.Estacion       = W_Estacion NO-ERROR.


IF Taquilla.Naturaleza EQ "DB" THEN 
    ASSIGN Mov_Contable.DB = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.
ELSE
    ASSIGN Mov_Contable.CR = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.

IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.

/*contrapartida*/
CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia        = Taquilla.Agencia
       Mov_Contable.Comprobante    = W_Cbte
       Mov_Contable.Cuenta         = Taquilla.Cta_Contra
       Mov_Contable.Fec_Contable   = W_Fecha
       Mov_Contable.Comentario     = Taquilla.Descripcion
       Mov_Contable.Usuario        = Taquilla.Usuario
       Mov_contable.Nit            = Taquilla.Nit
       Mov_Contable.Destino        = Taquilla.Age_Destino
       Mov_Contable.Cen_Costos     = 999
       Mov_Contable.Num_Documento  = INTEGER(Taquilla.Nro_Transaccion)
       Mov_Contable.Doc_Referencia = Taquilla.Num_Retcheque
       Mov_contable.Enlace         = STRING(Taquilla.Nro_Transaccion)
       Mov_Contable.Fec_Grabacion  = TODAY
       Mov_Contable.Hora           = TIME
       Mov_Contable.Estacion       = W_Estacion  NO-ERROR.

IF Taquilla.Naturaleza EQ "DB" THEN
    ASSIGN Mov_Contable.CR = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.
ELSE
    ASSIGN Mov_Contable.DB = Taquilla.Val_Cheque + Taquilla.Val_Efectivo NO-ERROR.

IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CortoLargoCreditos Wwin 
PROCEDURE CortoLargoCreditos :
FIND CortoLargo WHERE CortoLargo.Agencia        EQ Creditos.Agencia
                 AND   CortoLargo.Clase_Producto EQ 2
                 AND   CortoLargo.Cod_Producto   EQ Creditos.Cod_Credito
                 AND   CortoLargo.Cta_ContingenteDB NE "" 
                 AND   CortoLargo.Cta_ContingenteCR NE "" 
                 AND   CortoLargo.Comprobante       NE 0 NO-LOCK NO-ERROR.
            
 IF AVAILABLE(CortoLargo) THEN DO:
    ASSIGN W_CtaSyA_Des  = CortoLargo.Cta_SYA
           W_CtaCosDB    = CortoLargo.Cta_CostasDB
           W_CtaHonDB    = CortoLargo.Cta_HonorariosDB
           W_CtaPolDB    = CortoLargo.Cta_PolizasDB.
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
       MESSAGE "No se ha encontrado la cuenta para el pago" SKIP
               "del capital. Comuniquese con el Administrador" VIEW-AS ALERT-BOX.
       RETURN ERROR.
    END.
    /*busca cta en tabla liqint*/
    ASSIGN W_CtaIntCor_Db = ""
           W_CtaIntCor_Cr = ""
           W_CtaIntAnt    = ""
           W_CtaDifCob_Db = ""
           W_CtaDifCob_Db = ""
           W_CtaIntMor    = "".
    FIND Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2 AND
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
        IF Clientes.Tipo_Vinculo EQ 1 THEN
           W_CtaIntAnt = Liqui_Int.CtaInt_AntAso.
        ELSE W_CtaIntAnt = Liqui_Int.CtaInt_Ant.
                 
    END.
    IF W_CtaIntCor_Db EQ "" OR W_CtaIntCor_Cr EQ "" OR
       W_CtaIntMor    EQ "" OR W_CtaDifCob_Db EQ "" OR
       W_CtaDifCob_Cr EQ "" THEN DO:
       MESSAGE "Falta una o varias cuentas de interes por configurar" SKIP
               "para el producto." SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.
 END.
 ELSE DO:
   MESSAGE "No se ha encontrado la configuracón de cuentas" SKIP
           "de interes para el producto de créditos" SKIP(1)
           "Comuniquese con el Administrador!" VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
 END.
 IF Creditos.Agencia NE W_Agencia THEN DO:
   FIND CortoLargo WHERE CortoLargo.Agencia        EQ W_Agencia
                   AND   CortoLargo.Clase_Producto EQ 2
                   AND   CortoLargo.Cod_Producto   EQ Creditos.Cod_Credito
                   AND   CortoLargo.Cta_ContingenteDB NE ""
                   AND   CortoLargo.Cta_ContingenteCR NE ""
                   AND   CortoLargo.Comprobante       NE 0 NO-LOCK NO-ERROR.
     IF AVAILABLE(CortoLargo) THEN W_CtaSYA_FTE = CortoLargo.Cta_SYA.
     ELSE DO:
       MESSAGE "No se encontra la cuenta de sucursales y agencias" SKIP
               "de la Agencia de donde es el crédito" SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
     END.
 END.
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

  {&OPEN-QUERY-F_Cre}
  GET FIRST F_Cre.
  DISPLAY W_Valor R_TipCon W_AboCap W_AboInt W_AboIntAnt W_AboIntDifCob 
          W_AboIntMor W_AboCostas W_AboHonora W_AboPoliza W_ValPag 
      WITH FRAME F_Cre IN WINDOW Wwin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Fec_Desembolso Creditos.Fec_Pago Creditos.Fec_ProxLiquidacion 
          Creditos.Tasa Creditos.Fec_UltPago Creditos.Cuota 
          Creditos.Fec_UltLiquidacion Creditos.Sdo_Capital Creditos.Plazo 
          Creditos.Int_Corrientes Creditos.Sdo_Proyectado Creditos.Int_DifCobro 
          Creditos.Cuo_Pagadas Creditos.Int_MorCobrar Creditos.Val_Atraso 
          Creditos.Int_Anticipado Creditos.Dias_Atraso Creditos.Cuo_Atraso 
          Creditos.Provision Creditos.Fec_Reestructurado 
      WITH FRAME F_Cre IN WINDOW Wwin.
  ENABLE RECT-296 W_Valor R_TipCon Btn_Grabar Btn_Salir Btn_Acuerdos 
      WITH FRAME F_Cre IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cre}
  ENABLE B_Acuerdos BUTTON-182 
      WITH FRAME F_Acuerdos IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Acuerdos}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_CheTransito Wwin 
PROCEDURE Gra_CheTransito :
/*------------------------------------------------------------------------------
  OBSERVACIONES :       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER C_Banco   LIKE Che_Transito.Cod_Compensa.
DEFINE INPUT PARAMETER C_Cheque  LIKE Che_Transito.Cheque.
DEFINE INPUT PARAMETER C_CodPto  LIKE Che_Transito.Cod_Producto.
DEFINE INPUT PARAMETER C_Estado  LIKE Che_Transito.Estado.
DEFINE INPUT PARAMETER C_Cuenta  LIKE Che_Transito.Num_Cuenta.
DEFINE INPUT PARAMETER C_Agencia LIKE Che_Transito.Agencia.
DEFINE INPUT PARAMETER C_TipPto  LIKE Che_Transito.Tip_Producto.
DEFINE INPUT PARAMETER C_Valor   LIKE Che_Transito.Valor.
DEFINE INPUT PARAMETER C_Canje   LIKE Che_Transito.Tip_Remesa.

FIND Che_Transito WHERE Che_Transito.Agencia      EQ C_Agencia
                    AND Che_Transito.Cod_Compensa EQ C_Banco
                    AND Che_Transito.Cheque       EQ C_Cheque
                    AND Che_Transito.Cod_Producto EQ C_CodPto
                    AND Che_Transito.Num_Cuenta   EQ C_Cuenta EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE(Che_Transito) THEN
   ASSIGN Che_Transito.Valor = Che_Transito.Valor + C_Valor.
ELSE DO:
   CREATE Che_Transito.
   ASSIGN Che_Transito.Cod_Compensa     = C_Banco
          Che_Transito.Cheque           = C_Cheque
          Che_Transito.Cod_Producto     = C_CodPto
          Che_Transito.Estado           = C_Estado
          Che_Transito.Fec_Canje        = TODAY
          Che_Transito.Fec_Confirmacion = TODAY
          Che_Transito.Int_Generado     = 0
          Che_Transito.Num_Cuenta       = C_Cuenta
          Che_Transito.Agencia          = C_Agencia
          Che_Transito.Ofi_Destino      = W_Agencia
          Che_Transito.Tip_Producto     = C_TipPto
          Che_Transito.Valor            = C_Valor
          Che_Transito.Tip_Remesa       = C_Canje NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Error al Grabar en Chequesen Transito... "
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Error en Taquilla".
      RETURN ERROR.
   END.
END.

RELEASE Che_Transito.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_MovCreditos Wwin 
PROCEDURE Gra_MovCreditos :
/*------------------------------------------------------------------------------
  Observaciones : Permite Gravar el Detalle de la Operación en Movimientos.       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER C_CodOper LIKE Mov_Creditos.Cod_Operacion.
  DEFINE INPUT PARAMETER C_CodPto  LIKE Mov_creditos.cod_credito.
  DEFINE INPUT PARAMETER C_NumCre  LIKE Mov_Creditos.Num_Credito.
  DEFINE INPUT PARAMETER C_Dto     LIKE Mov_Creditos.Num_Documento.
  DEFINE INPUT PARAMETER C_Agencia LIKE Mov_Creditos.Agencia.
  DEFINE INPUT PARAMETER C_OfiFte  LIKE Mov_Creditos.Ofi_Fuente.
  DEFINE INPUT PARAMETER C_OfiDest LIKE Mov_Creditos.Ofi_Destino.
  DEFINE INPUT PARAMETER C_Usuario LIKE Mov_Creditos.Usuario.
  DEFINE INPUT PARAMETER C_VlrChe  LIKE Mov_Creditos.Val_Cheque.
  DEFINE INPUT PARAMETER C_VlrEfe  LIKE Mov_Creditos.Val_Efectivo.
  
  CREATE Mov_Creditos.
  ASSIGN Mov_Creditos.Cod_Operacion  = C_CodOper
         Mov_creditos.cod_credito    = C_CodPto
         Mov_Creditos.Nit            = Creditos.nit
         Mov_Creditos.Fecha          = TODAY
         Mov_Creditos.Hora           = vTime
         Mov_Creditos.Pagare         = Creditos.Pagare
         Mov_Creditos.Num_Documento  = C_Dto
         Mov_Creditos.Agencia        = C_Agencia
         Mov_Creditos.Ofi_Fuente     = C_OfiFte
         Mov_Creditos.Ofi_Destino    = C_OfiDest
         Mov_Creditos.Num_Credito    = C_NumCre
         Mov_Creditos.Usuario        = C_Usuario
         Mov_Creditos.Val_Cheque     = C_VlrChe
         Mov_Creditos.Val_Efectivo   = C_VlrEfe
         Mov_Creditos.Sdo_Capital    = Creditos.Sdo_Capital
         Mov_Creditos.INT_Corrientes = Creditos.INT_Corrientes
         Mov_Creditos.INT_MorCobrar  = Creditos.INT_MorCobrar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_Taquilla Wwin 
PROCEDURE Gra_Taquilla :
/*------------------------------------------------------------------------------
  OBSERVACIONES: Permite Almacenar el Registro en Taquilla.       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER T_Autorizo  LIKE Taquilla.Autorizo.
DEFINE INPUT PARAMETER T_Banco     LIKE Taquilla.Cod_Compensa.
DEFINE INPUT PARAMETER T_CodOper   LIKE Taquilla.Cod_Operacion.
DEFINE INPUT PARAMETER T_CodPto    LIKE Taquilla.Cod_Producto.
DEFINE INPUT PARAMETER T_Cuenta    LIKE Taquilla.Cuenta.
DEFINE INPUT PARAMETER T_CtraCta   LIKE Taquilla.Cuenta.
DEFINE INPUT PARAMETER T_Nat       LIKE Taquilla.Naturaleza.
DEFINE INPUT PARAMETER T_Nit       LIKE Taquilla.Nit.
DEFINE INPUT PARAMETER T_Nrocuenta LIKE Taquilla.Nro_cuenta.
DEFINE INPUT PARAMETER T_NumDto    LIKE Taquilla.Num_Documento.
DEFINE INPUT PARAMETER T_NumRetche LIKE Taquilla.Num_Retcheque.
DEFINE INPUT PARAMETER T_Agencia   LIKE Taquilla.Agencia.
DEFINE INPUT PARAMETER T_OfiDes    LIKE Taquilla.Age_Destino.
DEFINE INPUT PARAMETER T_OfiFue    LIKE Taquilla.Age_Fuente.
DEFINE INPUT PARAMETER T_TipPto    LIKE Taquilla.Tip_Producto.
DEFINE INPUT PARAMETER T_Usuario   LIKE Taquilla.Usuario.
DEFINE INPUT PARAMETER T_ValChe    LIKE Taquilla.Val_Cheque.
DEFINE INPUT PARAMETER T_ValEfec   LIKE Taquilla.Val_Efectivo.
DEFINE INPUT PARAMETER T_Segmento  LIKE Clientes.Cod_Segmento.
DEFINE INPUT PARAMETER T_Comenta   AS CHARACTER FORMAT "X(25)".

CREATE Taquilla.
ASSIGN Taquilla.Autorizo         = T_Autorizo
       Taquilla.Nro_Transaccion  = W_NumSeq
       Taquilla.Cod_Compensa     = T_Banco
       Taquilla.Cod_Operacion    = T_CodOper
       Taquilla.Cod_Producto     = T_CodPto
       Taquilla.Contabilizar     = FALSE
       Taquilla.Cuenta           = T_Cuenta
       Taquilla.Cta_Contra       = T_CtraCta
       Taquilla.Duracion         = 0
       Taquilla.Est_Linea        = 0
       Taquilla.Fec_Transaccion  = TODAY
       Taquilla.Hora_Transaccion = TIME
       Taquilla.Naturaleza       = T_Nat
       Taquilla.Nit              = T_Nit
       Taquilla.Nro_cuenta       = T_Nrocuenta
       Taquilla.Num_Documento    = T_NumDto
       Taquilla.Num_Retcheque    = T_NumRetche
       Taquilla.Agencia          = T_Agencia
       Taquilla.Age_Destino      = T_OfiDes
       Taquilla.Age_Fuente       = T_OfiFue
       Taquilla.Tip_Producto     = T_TipPto
       Taquilla.Usuario          = T_Usuario
       Taquilla.Val_Cheque       = T_ValChe
       Taquilla.Val_Efectivo     = T_ValEfec
       Taquilla.Estacion         = W_Estacion
       Taquilla.Cod_Segmento     = T_Segmento.
       Taquilla.Descripcion      = T_Comenta.

RELEASE Taquilla.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Wwin 
PROCEDURE initializeObject :
RUN SUPER.

Wwin:TITLE = P_NomIns.

FIND Operacion WHERE Operacion.Cod_Operacion EQ Op_Capital NO-LOCK NO-ERROR.
IF AVAILABLE Operacion THEN
    W_Cbte = Operacion.Comprobante.
ELSE DO:
    MESSAGE "No se ha encontrado la operación de créditos" SKIP
            "Comuniquese con el Administrador!" SKIP
            "La Operacion pasada fue: " P_CodOpe
        VIEW-AS ALERT-BOX ERROR.
    DISABLE Btn_Grabar WITH FRAME F_Cre.
END.

FIND Creditos WHERE Creditos.Tip_Credito EQ P_TipCre
                AND Creditos.Cod_Credito EQ P_CodCre
                AND Creditos.Num_Credito EQ P_NumCre
                AND Creditos.Nit EQ P_NitCli EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE Creditos THEN DO:
    FIND FIRST Cobros WHERE Cobros.Nit EQ Creditos.Nit
                        AND Cobros.Num_Credito EQ Creditos.Num_Credito NO-LOCK NO-ERROR.
    IF AVAILABLE Cobros THEN
        OPEN QUERY B_Acuerdos FOR EACH Cobros WHERE Cobros.Nit EQ Creditos.Nit
                                                AND Cobros.Num_Credito EQ Creditos.Num_Credito NO-LOCK INDEXED-REPOSITION.
    ELSE
        DISABLE Btn_Acuerdos WITH FRAME F_Cre.

    
    IF NOT Creditos.Abogado AND creditos.estado <> 5 /*Castigado*/ THEN DO:
        MESSAGE "El Credito no se encuentra en abogado." SKIP
                "para realizar la operacion. " SKIP(1)
                "Escoja un recaudo normal"
            VIEW-AS ALERT-BOX ERROR.
        DISABLE ALL EXCEPT Btn_Salir WITH FRAME F_Cre.
    END.

    IF Creditos.Val_Atraso EQ 0 AND creditos.estado <> 5 /*Castigado*/ THEN DO:
        MESSAGE "El Credito no se encuentra en proceso de Cobros" SKIP
                "para realizar la operacion. " SKIP(1)
                "Escoja un recaudo normal"
            VIEW-AS ALERT-BOX ERROR.
        DISABLE ALL EXCEPT Btn_Salir WITH FRAME F_Cre.
    END.

    FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
    FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito
                        AND Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito NO-LOCK NO-ERROR.
    IF AVAILABLE Pro_Creditos THEN
        RUN CortoLargoCreditos NO-ERROR.
    ELSE
        MESSAGE "No se encontro el producto de Creditos"
            VIEW-AS ALERT-BOX ERROR.

    IF ERROR-STATUS:ERROR THEN
        DISABLE ALL EXCEPT Btn_Salir WITH FRAME F_Cre.

    /*busca cuenta de caja*/
    FIND FIRST Cuentas WHERE Cuentas.Cod_FlujoEfec EQ "D"
                         AND Cuentas.Car_Efectivo EQ 2
                         AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Cuentas THEN
        W_Caja = Cuentas.Cuenta.
    ELSE DO:
        MESSAGE "No se ha encontrado la cuenta de caja efectivo" SKIP
                "para realizar la operacion. " SKIP(1)
                "Comuniquese con el Administrador"
            VIEW-AS ALERT-BOX ERROR.
        DISABLE ALL EXCEPT Btn_Salir WITH FRAME F_Cre.
    END.

    RUN Mostrar_Credito.
END.

HIDE FRAME F_Acuerdos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Credito Wwin 
PROCEDURE Mostrar_Credito :
DO WITH FRAME F_Cre:
   ASSIGN Creditos.Fec_Desembolso:SCREEN-VALUE = STRING(Creditos.Fec_Desembolso)
          Creditos.Fec_Pago:SCREEN-VALUE = STRING(Creditos.Fec_Pago)
          Creditos.Fec_Ultpago:SCREEN-VALUE = STRING(Creditos.Fec_Ultpago)
          Creditos.Fec_ProxLiquidacion:SCREEN-VALUE = STRING(Creditos.Fec_ProxLiquidacion)
          Creditos.Fec_UltLiquidacion:SCREEN-VALUE = STRING(Creditos.Fec_UltLiquidacion)
          Creditos.INT_DifCobro:SCREEN-VALUE = STRING(Creditos.INT_DifCobro)
          Creditos.Sdo_Proyectado:SCREEN-VALUE = STRING(Creditos.Sdo_Proyectado)
          Creditos.Cuo_Pagadas:SCREEN-VALUE = STRING(Creditos.Cuo_Pagadas)
          Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Val_Atraso)
          Creditos.Dias_Atraso:SCREEN-VALUE = STRING(Creditos.Dias_Atraso)
          Creditos.Cuo_Atraso:SCREEN-VALUE = STRING(Creditos.Cuo_Atraso)
          Creditos.Provision:SCREEN-VALUE = STRING(Creditos.Provision)
          Creditos.Fec_Reestructurado:SCREEN-VALUE = STRING(Creditos.Fec_Reestructurado)
          Creditos.Cuota:SCREEN-VALUE = STRING(Creditos.Cuota)
          Creditos.INT_Corrientes:SCREEN-VALUE = STRING(Creditos.INT_Corrientes)
          Creditos.INT_Anticipado:SCREEN-VALUE = STRING(Creditos.INT_Anticipado)
          Creditos.Sdo_Capital:SCREEN-VALUE = STRING(Creditos.Sdo_Capital)
          Creditos.INT_MorCobrar:SCREEN-VALUE = STRING(Creditos.INT_MorCobrar)
          Creditos.Plazo:SCREEN-VALUE = STRING(Creditos.Plazo).
/*          W_AboCostas:SCREEN-VALUE = STRING(Creditos.Costas)
          W_AboHonora:SCREEN-VALUE = STRING(Creditos.Honorarios)
          W_AboPoliza:SCREEN-VALUE = STRING(Creditos.Polizas).*/
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taquilla Wwin 
PROCEDURE Taquilla :
ASSIGN FRAME F_Cre R_TipCon.

W_NumSeq = NEXT-VALUE(Sec_Taquilla).

RUN AboCredito.R(INPUT YES, /*Distribuye abonos en Créditos,graba Mov_creditos y PlanPagos*/
                 INPUT Creditos.Agencia,
                 Creditos.Cod_Credito,
                 Creditos.Nit,
                 INPUT Creditos.Num_Credito,
                 DECIMAL(W_Valor:SCREEN-VALUE),
                 INPUT W_Cbte,
                 W_NumSeq,
                 R_TipCon,
                 INPUT 1,
                 OUTPUT P_Poliza,
                 OUTPUT P_Honora,
                 OUTPUT P_Costas,
                 OUTPUT P_IMorDifC,
                 OUTPUT P_IMora,
                 OUTPUT P_IDifCob,
                 OUTPUT P_ICte,
                 OUTPUT P_IAntic,
                 OUTPUT P_Capital,
                 OUTPUT P_VlrNoDist).

IF P_Costas GT 0 THEN DO:
    RUN Taq_Costas NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al grabar las costas juridicas"
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
END.

IF P_Honora GT 0 THEN DO:
    RUN Taq_Honorarios NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al grabar los Honorarios"
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
END.

IF P_Poliza GT 0 THEN DO:
    RUN Taq_Polizas NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al grabar las Polizas"
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
END.

IF P_ICte GT 0 THEN DO:
    RUN Taq_IntCorriente NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al grabar los interes corrientes en taquilla"
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
END.

IF P_IDifCob GT 0 THEN DO:
    RUN Taq_DifCobro NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al grabar el interes corriente en taquilla"
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
END.

IF P_IAntic GT 0 THEN DO:
    RUN Taq_IntAnticipado NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al grabar el interes anricipado en taquilla"
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
END.

IF P_IMora GT 0 THEN DO:
    RUN Taq_IntMora NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al grabar el interes de mora en taquilla" VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
END.

IF P_Capital GT 0 THEN DO:
    RUN Taq_Capital NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Error al grabar el capital en taquilla"
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taq_Capital Wwin 
PROCEDURE Taq_Capital :
IF R_TipCon EQ 2 THEN
   ASSIGN W_ValChe = P_Capital W_ValEfe = 0.
ELSE ASSIGN W_ValChe = 0 W_ValEfe = P_Capital.

/*RUN Gra_MovCreditos(INPUT Op_Capital, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_NumSeq,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT W_ValChe, 
                    INPUT W_ValEfe).*/

IF Creditos.Agencia EQ W_Agencia THEN
      RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT Op_Capital, 
                       INPUT Creditos.Cod_Credito, INPUT W_CtaCap,     INPUT W_Caja,
                       INPUT "CR",                 INPUT Creditos.Nit, INPUT STRING(Creditos.Num_Credito), 
                       INPUT STRING(Creditos.Num_Credito), INPUT 0,    INPUT W_Agencia,           
                       INPUT Creditos.Agencia,     INPUT W_Agencia,    INPUT "1",            
                       INPUT W_Usuario,            INPUT W_ValChe,     INPUT W_ValEfe,
                       INPUT 0, INPUT "Abono a Capital").
   ELSE DO:
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT Op_Capital, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des, INPUT W_Caja,
                        INPUT "CR",                 INPUT Creditos.Nit, INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0,    INPUT W_Agencia,           
                        INPUT Creditos.Agencia,     INPUT W_Agencia,    INPUT "1",            
                        INPUT W_Usuario,            INPUT W_ValChe,     INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abono a Capital").
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_Capital, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaCap,     INPUT W_CtaSYA_Fte,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT Creditos.Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abono a Capital").
   END.
   IF R_TipCon:SCREEN-VALUE IN FRAME F_Cre EQ "2" THEN 
      RUN Gra_CheTransito(INPUT W_NumBan, INPUT W_NumChe, INPUT Creditos.Cod_Credito,
                           INPUT 1,        INPUT STRING(Creditos.Num_Credito),  INPUT W_Agencia, 
                           INPUT 2,        INPUT P_Capital,  INPUT W_Canje) NO-ERROR.

   RELEASE Taquilla.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taq_Costas Wwin 
PROCEDURE Taq_Costas :
IF R_TipCon EQ 2 THEN
    ASSIGN W_ValChe = P_Costas
           W_ValEfe = 0.
ELSE
    ASSIGN W_ValChe = 0
           W_ValEfe = P_Costas.

/*RUN Gra_MovCreditos(INPUT Op_Costas, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_NumSeq,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT W_ValChe, 
                    INPUT W_ValEfe).*/

IF Creditos.Agencia EQ W_Agencia THEN
    RUN Gra_Taquilla(INPUT W_Usuario,
                     INPUT 0,
                     INPUT Op_Capital,
                     INPUT Creditos.Cod_Credito,
                     INPUT W_CtaCosDB,
                     INPUT W_Caja,
                     INPUT "CR",
                     INPUT Creditos.Nit,
                     INPUT STRING(Creditos.Num_Credito),
                     INPUT STRING(Creditos.Num_Credito),
                     INPUT 0,
                     INPUT W_Agencia,
                     INPUT Creditos.Agencia,
                     INPUT W_Agencia,
                     INPUT "1",
                     INPUT W_Usuario,
                     INPUT W_ValChe,
                     INPUT W_ValEfe,
                     INPUT 0,
                     INPUT "Abono a Costas").
ELSE DO:
    RUN Gra_Taquilla(INPUT W_Usuario,
                     INPUT 0,
                     INPUT Op_Costas,
                     INPUT Creditos.Cod_Credito,
                     INPUT W_CtaSYA_Des,
                     INPUT W_Caja,
                     INPUT "CR",
                     INPUT Creditos.Nit,
                     INPUT STRING(Creditos.Num_Credito),
                     INPUT STRING(Creditos.Num_Credito),
                     INPUT 0,
                     INPUT W_Agencia,
                     INPUT Creditos.Agencia,
                     INPUT W_Agencia,
                     INPUT "1",
                     INPUT W_Usuario,
                     INPUT W_ValChe,
                     INPUT W_ValEfe,
                     INPUT 0,
                     INPUT "SYA - Abono a Costas").

    RUN Gra_Taquilla(INPUT W_Usuario,
                     INPUT 0,
                     INPUT Op_Costas,
                     INPUT Creditos.Cod_Credito,
                     INPUT W_CtaCosDB,
                     INPUT W_CtaSYA_Fte,
                     INPUT "CR",
                     INPUT Creditos.Nit,
                     INPUT STRING(Creditos.Num_Credito),
                     INPUT STRING(Creditos.Num_Credito),
                     INPUT 0,
                     INPUT Creditos.Agencia,
                     INPUT Creditos.Agencia,
                     INPUT W_Agencia,
                     INPUT "1",
                     INPUT W_Usuario,
                     INPUT W_ValChe,
                     INPUT W_ValEfe,
                     INPUT 0,
                     INPUT "SYA - Abono a Costas").
END.

IF R_TipCon:SCREEN-VALUE IN FRAME F_Cre EQ "2" THEN
    RUN Gra_CheTransito(INPUT W_NumBan,
                        INPUT W_NumChe,
                        INPUT Creditos.Cod_Credito,
                        INPUT 1,
                        INPUT STRING(Creditos.Num_Credito),
                        INPUT W_Agencia,
                        INPUT 2,
                        INPUT P_Costas,
                        INPUT W_Canje) NO-ERROR.

RELEASE Taquilla.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taq_DifCobro Wwin 
PROCEDURE Taq_DifCobro :
IF R_TipCon EQ 2 THEN
   ASSIGN W_ValChe = P_IDifCob W_ValEfe = 0.
ELSE ASSIGN W_ValChe = 0 W_ValEfe = P_IDifCob.

/*Partida con Caja*/

/*RUN Gra_MovCreditos(INPUT Op_IntDif, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_NumSeq,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT W_ValChe, 
                    INPUT W_ValEfe).*/

IF Creditos.Agencia EQ W_Agencia THEN
      RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT Op_IntDif, 
                       INPUT Creditos.Cod_Credito, INPUT W_CtaDifCob_Cr,   INPUT W_Caja,
                       INPUT "CR",                 INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                       INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                       INPUT Creditos.Agencia, INPUT W_Agencia, INPUT "1",            
                       INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                       INPUT 0, INPUT "Abono a Dificil Cobro").
   ELSE DO:
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_IntDif, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,      INPUT W_Caja,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abo Dificil Cobro").
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_IntDif, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaDifCob_Cr, INPUT W_CtaSYA_Fte,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT Creditos.Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abo Dificil Cobro").
   END.
   IF R_TipCon:SCREEN-VALUE IN FRAME F_Cre EQ "2" THEN 
      RUN Gra_CheTransito(INPUT W_NumBan, INPUT W_NumChe, INPUT Creditos.Cod_Credito,
                           INPUT 1,        INPUT STRING(Creditos.Num_Credito),  INPUT W_Agencia, 
                           INPUT 2,        INPUT P_IDifCob,  INPUT W_Canje) NO-ERROR.

   RELEASE Taquilla.

/*cruce de cuentas de orden*/
/*Partida con Caja*/
/*W_NumSeq = NEXT-VALUE(Sec_Taquilla).*/
/*RUN Gra_MovCreditos(INPUT P_CodOpe, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_NumSeq,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT W_ValChe, 
                    INPUT W_ValEfe).*/

IF Creditos.Agencia EQ W_Agencia THEN
      RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT P_CodOpe, 
                       INPUT Creditos.Cod_Credito, INPUT W_CtaDifCob_Db,   INPUT W_CtaDifCob_Cr,
                       INPUT "CR",                 INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                       INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                       INPUT Creditos.Agencia, INPUT W_Agencia, INPUT "1",            
                       INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                       INPUT 0, INPUT "C.Orden Abo Dificil Cobro").
   ELSE DO:
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT P_CodOpe, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,      INPUT W_CtaDifCob_Cr,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA C.Orden Abo DifCob").
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT P_CodOpe, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaDifCob_Db, INPUT W_CtaSYA_Fte,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT Creditos.Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - C.Orden Abo DifCob").
   END.
   IF R_TipCon:SCREEN-VALUE IN FRAME F_Cre EQ "2" THEN 
      RUN Gra_CheTransito(INPUT W_NumBan, INPUT W_NumChe, INPUT Creditos.Cod_Credito,
                           INPUT 1,        INPUT STRING(Creditos.Num_Credito),  INPUT W_Agencia, 
                           INPUT 2,        INPUT P_IDifCob,  INPUT W_Canje) NO-ERROR.

   RELEASE Taquilla.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taq_Honorarios Wwin 
PROCEDURE Taq_Honorarios :
IF R_TipCon EQ 2 THEN
   ASSIGN W_ValChe = P_Honora W_ValEfe = 0.
ELSE ASSIGN W_ValChe = 0 W_ValEfe = P_Honora.

/*RUN Gra_MovCreditos(INPUT Op_Honorarios, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_NumSeq,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT W_ValChe, 
                    INPUT W_ValEfe).*/

IF Creditos.Agencia EQ W_Agencia THEN
      RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT Op_Honorarios, 
                       INPUT Creditos.Cod_Credito, INPUT W_CtaHonDB,   INPUT W_Caja,
                       INPUT "CR",                 INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                       INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                       INPUT Creditos.Agencia, INPUT W_Agencia, INPUT "1",            
                       INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                       INPUT 0, INPUT "Abono a Honorarios").
   ELSE DO:
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_Honorarios, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,      INPUT W_Caja,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abo Honorarios").
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_Honorarios, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaHonDB,     INPUT W_CtaSYA_Fte,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT Creditos.Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abo Honorarios").
   END.
   IF R_TipCon:SCREEN-VALUE IN FRAME F_Cre EQ "2" THEN 
      RUN Gra_CheTransito(INPUT W_NumBan, INPUT W_NumChe, INPUT Creditos.Cod_Credito,
                           INPUT 1,        INPUT STRING(Creditos.Num_Credito),  INPUT W_Agencia, 
                           INPUT 2,        INPUT P_Honora,  INPUT W_Canje) NO-ERROR.

   RELEASE Taquilla.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taq_IntAnticipado Wwin 
PROCEDURE Taq_IntAnticipado :
IF R_TipCon EQ 2 THEN
   ASSIGN W_ValChe = P_IAntic W_ValEfe = 0.
ELSE ASSIGN W_ValChe = 0 W_ValEfe = P_IAntic.


/*RUN Gra_MovCreditos(INPUT Op_IntAnt, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_NumSeq,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT W_ValChe, 
                    INPUT W_ValEfe).*/

IF Creditos.Agencia EQ W_Agencia THEN
      RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT Op_IntAnt, 
                       INPUT Creditos.Cod_Credito, INPUT W_CtaIntAnt,   INPUT W_Caja,
                       INPUT "CR",                 INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                       INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                       INPUT Creditos.Agencia, INPUT W_Agencia, INPUT "1",            
                       INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                       INPUT 0, INPUT "Abono a Int. Anticipado").
   ELSE DO:
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_IntAnt, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,      INPUT W_Caja,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abo Int.Anticipado").
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_IntAnt, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaIntAnt,     INPUT W_CtaSYA_Fte,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT Creditos.Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abo Int.Anticipado").
   END.
   IF R_TipCon:SCREEN-VALUE IN FRAME F_Cre EQ "2" THEN 
      RUN Gra_CheTransito(INPUT W_NumBan, INPUT W_NumChe, INPUT Creditos.Cod_Credito,
                           INPUT 1,        INPUT STRING(Creditos.Num_Credito),  INPUT W_Agencia, 
                           INPUT 2,        INPUT P_IAntic,  INPUT W_Canje) NO-ERROR.

   RELEASE Taquilla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taq_IntCorriente Wwin 
PROCEDURE Taq_IntCorriente :
IF R_TipCon EQ 2 THEN
   ASSIGN W_ValChe = P_ICte W_ValEfe = 0.
ELSE ASSIGN W_ValChe = 0 W_ValEfe = P_ICte.


/*RUN Gra_MovCreditos(INPUT Op_IntCor, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_NumSeq,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT W_ValChe, 
                    INPUT W_ValEfe).*/

IF Creditos.Agencia EQ W_Agencia THEN
      RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT Op_IntCor, 
                       INPUT Creditos.Cod_Credito, INPUT W_CtaIntCor_Db,   INPUT W_Caja,
                       INPUT "CR",                 INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                       INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                       INPUT Creditos.Agencia, INPUT W_Agencia, INPUT "1",            
                       INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                       INPUT 0, INPUT "Abono a Int. Corriente").
   ELSE DO:
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_IntCor, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,      INPUT W_Caja,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abo Int.Corriente").
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_IntCor, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaIntCor_Db,     INPUT W_CtaSYA_Fte,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT Creditos.Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abo Int.Corriente").
   END.
   IF R_TipCon:SCREEN-VALUE IN FRAME F_Cre EQ "2" THEN 
      RUN Gra_CheTransito(INPUT W_NumBan, INPUT W_NumChe, INPUT Creditos.Cod_Credito,
                           INPUT 1,        INPUT STRING(Creditos.Num_Credito),  INPUT W_Agencia, 
                           INPUT 2,        INPUT P_ICte,  INPUT W_Canje) NO-ERROR.

   RELEASE Taquilla.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taq_IntMora Wwin 
PROCEDURE Taq_IntMora :
IF R_TipCon EQ 2 THEN
   ASSIGN W_ValChe = P_IMora W_ValEfe = 0.
ELSE ASSIGN W_ValChe = 0 W_ValEfe = P_IMora.


/*RUN Gra_MovCreditos(INPUT Op_Mora, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_NumSeq,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT W_ValChe, 
                    INPUT W_ValEfe).*/

IF Creditos.Agencia EQ W_Agencia THEN
      RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT Op_Mora, 
                       INPUT Creditos.Cod_Credito, INPUT W_CtaIntMor,   INPUT W_Caja,
                       INPUT "CR",                 INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                       INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                       INPUT Creditos.Agencia, INPUT W_Agencia, INPUT "1",            
                       INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                       INPUT 0, INPUT "Abono a Int. Mora").
   ELSE DO:
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_Mora, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,      INPUT W_Caja,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abo Int. Mora").
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_Mora, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaIntMor,     INPUT W_CtaSYA_Fte,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT Creditos.Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abo Int. Mora").
   END.
   IF R_TipCon:SCREEN-VALUE IN FRAME F_Cre EQ "2" THEN 
      RUN Gra_CheTransito(INPUT W_NumBan, INPUT W_NumChe, INPUT Creditos.Cod_Credito,
                           INPUT 1,        INPUT STRING(Creditos.Num_Credito),  INPUT W_Agencia, 
                           INPUT 2,        INPUT P_IMora,  INPUT W_Canje) NO-ERROR.

   RELEASE Taquilla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taq_Polizas Wwin 
PROCEDURE Taq_Polizas :
IF R_TipCon EQ 2 THEN
   ASSIGN W_ValChe = P_Poliza W_ValEfe = 0.
ELSE ASSIGN W_ValChe = 0 W_ValEfe = P_Poliza.

/*RUN Gra_MovCreditos(INPUT Op_Polizas, INPUT Creditos.Cod_Credito,
                    INPUT Creditos.Num_Credito, 
                    INPUT W_NumSeq,  INPUT Creditos.Agencia,
                    INPUT W_Agencia, INPUT Creditos.Agencia, 
                    INPUT W_Usuario, INPUT W_ValChe, 
                    INPUT W_ValEfe).*/

IF Creditos.Agencia EQ W_Agencia THEN
      RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT Op_Polizas, 
                       INPUT Creditos.Cod_Credito, INPUT W_CtaPolDB,   INPUT W_Caja,
                       INPUT "CR",                 INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                       INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                       INPUT Creditos.Agencia, INPUT W_Agencia, INPUT "1",            
                       INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                       INPUT 0, INPUT "Abono a Polizas").
   ELSE DO:
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_Polizas, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,      INPUT W_Caja,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYA - Abono a Polizas").
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_Polizas, 
                        INPUT Creditos.Cod_Credito, INPUT W_CtaPOLDB,     INPUT W_CtaSYA_Fte,
                        INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito), 
                        INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT Creditos.Agencia,           
                        INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "1",            
                        INPUT W_Usuario,       INPUT W_ValChe,   INPUT W_ValEfe,
                        INPUT 0, INPUT "SYa - Abono a Polizas").
   END.
   IF R_TipCon:SCREEN-VALUE IN FRAME F_Cre EQ "2" THEN 
      RUN Gra_CheTransito(INPUT W_NumBan, INPUT W_NumChe, INPUT Creditos.Cod_Credito,
                           INPUT 1,        INPUT STRING(Creditos.Num_Credito),  INPUT W_Agencia, 
                           INPUT 2,        INPUT P_Poliza,  INPUT W_Canje) NO-ERROR.

   RELEASE Taquilla.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transaccion Wwin 
PROCEDURE Transaccion :
DEFINE VARIABLE Sobra_Abono LIKE creditos.sdo_capital INITIAL 0.
DEFINE VARIABLE Sobra_Interes LIKE creditos.sdo_capital INITIAL 0.

DO WITH FRAME F_Cre:
    ASSIGN FRAME F_Cre
        W_AboInt
        W_AboCap
        W_ValPag
        W_Valor.

    FIND FIRST Cobros WHERE Cobros.Nit = Creditos.Nit
                        AND Cobros.Estado = 1 NO-ERROR.
    IF AVAILABLE Cobros THEN DO:
        ASSIGN Cobros.Fec_Cumplimiento = TODAY
               Cobros.Val_Cumplido = W_VALPAG
               Cobros.Estado = 2.
    END.
END.

RUN Taquilla NO-ERROR.
IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.

RUN F-Taquilla.r (INPUT W_NumSeq,
                  INPUT "").

MESSAGE "Aliste papel para Segunda Copia ".

RUN F-Taquilla.r (INPUT W_NumSeq, INPUT "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

