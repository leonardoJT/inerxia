&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 
 CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER P_NomIns AS CHARACTER FORMAT "X(50)".
DEFINE INPUT PARAMETER P_NitCli AS CHARACTER.
DEFINE INPUT PARAMETER P_CodCre AS INTEGER.
DEFINE INPUT PARAMETER P_TipCre AS INTEGER.
DEFINE INPUT PARAMETER P_NumCre AS INTEGER.
DEFINE INPUT PARAMETER P_CodOpe AS INTEGER.
DEFINE INPUT PARAMETER P_Progra AS INTEGER FORMAT "9".

{INCLUIDO\VARIABLE.I "SHARED"}

/* oakley */

DEFINE VAR Op_Capital LIKE Operacion.Cod_Operacion INITIAL "020101001".
DEFINE VAR Op_Mora    LIKE Operacion.Cod_Operacion INITIAL "020101002".
DEFINE VAR Op_IntCor  LIKE Operacion.Cod_Operacion INITIAL "020101003".
DEFINE VAR Op_IntDif  LIKE Operacion.Cod_Operacion INITIAL "020101004".
DEFINE VAR Op_IntAnt  LIKE Operacion.Cod_Operacion INITIAL "020101005".
DEFINE VAR Op_Polizas LIKE Operacion.Cod_Operacion INITIAL "020101006".
DEFINE VAR Op_Honora  LIKE Operacion.Cod_Operacion INITIAL "020101007".
DEFINE VAR Op_Costas  LIKE Operacion.Cod_Operacion INITIAL "020101008".

DEFINE VAR P_VrInteres LIKE Creditos.Sdo_Capital.
DEFINE VAR P_VrCapital LIKE Creditos.Sdo_Capital.
DEFINE VAR T_VrInteres LIKE Creditos.Sdo_Capital.
DEFINE VAR T_VrCapital LIKE Creditos.Sdo_Capital.
DEFINE VAR T_VrMora    LIKE Creditos.INT_MorCobrar.
DEFI   VAR W_TotPago   LIKE Creditos.INT_MorCobrar.

 DEFINE VARIABLE Tot_intAnticipado LIKE creditos.sdo_capital INITIAL 0.
 DEFINE VARIABLE Tot_intDifCobro LIKE creditos.sdo_capital INITIAL 0.
 DEFINE VARIABLE Tot_IntCorriente LIKE creditos.sdo_capital INITIAL 0.
 DEFINE VARIABLE Tot_Mora LIKE creditos.sdo_capital INITIAL 0.
 DEFINE VARIABLE Tot_Capital LIKE creditos.sdo_capital INITIAL 0.

DEFINE VAR W_Ok      AS LOGICAL.
DEFI   VAR W_SiEntry AS LOG INIT FALSE.

DEFINE VAR W_CtaCap        LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaIntCor_Db  LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaIntCor_Cr  LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaIntMor     LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaIntAnt     LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaDifCob_db  LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaDifCob_Cr  LIKE Cuentas.Cuenta.
DEFINE VAR W_Caja          LIKE Cuentas.Cuenta.
DEFINE VAR W_Banco         LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaSya_Des    LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaSya_Fte    LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaPolDB      LIKE Cuentas.Cuenta.  
DEFINE VAR W_CtaHonDB      LIKE Cuentas.Cuenta.
DEFINE VAR W_CtaCosDb     LIKE Cuentas.Cuenta.  /**/
DEFINE VAR W_CtaCosCr     LIKE Cuentas.Cuenta.  /**/
DEFINE VAR W_CtaHonora     LIKE Cuentas.Cuenta.  /**/


DEFINE VAR W_Cbte          LIKE Comprobantes.Comprobante.
DEFINE VAR W_NumCbt        LIKE Comprobantes.Secuencia.
DEFINE VAR W_Canje         LIKE Bancos.Dia_Canje.
/*DEFINE VAR W_ValChe        LIKE Creditos.Sdo_Capital.
DEFINE VAR W_ValEfe        LIKE Creditos.Sdo_Capital.*/

DEFINE VARIABLE W_NumSeq   AS INTEGER FORMAT "9999999".

DEFINE VAR P_Poliza     LIKE Creditos.Sdo_Capital. 
DEFINE VAR P_Honora     LIKE Creditos.Sdo_Capital. 
DEFINE VAR P_Costas     LIKE Creditos.Sdo_Capital. 
DEFINE VAR P_IDifCobMor LIKE Creditos.Sdo_Capital.
DEFINE VAR P_IMora      LIKE Creditos.Sdo_Capital.
DEFINE VAR P_IDifCob    LIKE Creditos.Sdo_Capital.
DEFINE VAR P_ICte       LIKE Creditos.Sdo_Capital.
DEFINE VAR P_IAntic     LIKE Creditos.Sdo_Capital.
DEFINE VAR P_Capital    LIKE Creditos.Sdo_Capital.
DEFINE VAR P_VlrNoDist  LIKE Creditos.Sdo_Capital.

DEFI TEMP-TABLE TCheq
     FIELD Bco LIKE Taquilla.Cod_Compensa
     FIELD Nro LIKE Taquilla.Num_Retcheque
     FIELD Val LIKE Creditos.Sdo_Capital
     FIELD Cta LIKE Che_transito.Num_Cuenta
     FIELD Cje LIKE Bancos.Dia_Canje.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Cheq
&Scoped-define BROWSE-NAME Br_Cheq

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TCheq

/* Definitions for BROWSE Br_Cheq                                       */
&Scoped-define FIELDS-IN-QUERY-Br_Cheq TCheq.Bco TCheq.Nro TCheq.Val   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Cheq   
&Scoped-define SELF-NAME Br_Cheq
&Scoped-define QUERY-STRING-Br_Cheq FOR EACH TCheq NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Cheq OPEN QUERY {&SELF-NAME} FOR EACH TCheq NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Cheq TCheq
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Cheq TCheq


/* Definitions for FRAME F_Cheq                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Cheq ~
    ~{&OPEN-QUERY-Br_Cheq}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_NumCBan W_NumCChe W_VrCCheq Br_Cheq ~
Btn_Eliminar Btn_SalirCheq 
&Scoped-Define DISPLAYED-OBJECTS W_VrCCheq W_VrTotCh 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Eliminar 
     LABEL "Eliminar Selecciòn" 
     SIZE 18 BY 1.

DEFINE BUTTON Btn_SalirCheq 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_Salir" 
     SIZE 12.72 BY 1.35.

DEFINE VARIABLE W_NumCBan AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NumCChe AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrCCheq AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.72 BY .81
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE W_VrTotCh AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 0 NO-UNDO.

DEFINE BUTTON Btn_Grabar 
     LABEL "Grabar Operación" 
     SIZE 14.57 BY 1.35.

DEFINE BUTTON Btn_salir DEFAULT 
     LABEL "&Salir" 
     SIZE 15 BY 1.35
     BGCOLOR 8 .

DEFINE BUTTON Btn_Validadora 
     LABEL "Validar" 
     SIZE 6 BY 1.12 TOOLTIP "Botòn Oculto, para independizar Impresiòn de la Tx.".

DEFINE BUTTON BUTTON-163 
     LABEL "Historial de Créditos" 
     SIZE 20.29 BY .92.

DEFINE VARIABLE W_AboCap AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Abono a Capital" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboCostas AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Abono a Costas" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboHonora AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Honorarios" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboInt AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Interés Corriente" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboIntAnt AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Intereses Anticipados" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_AboIntDifCob AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Intereses Dificl Cobro" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboIntMor AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Mora + DifCobMora" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboPoliza AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Poliza" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CuoPag AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Cuotas a Pagar" 
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .85
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE W_NoDistribuido AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Valor Restante" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_otroscargos AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE W_SdoDeuda AS DECIMAL FORMAT "->>>>>>>,>>9.99" INITIAL 0 
     LABEL "Sdo.Tot Deuda" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 12 FGCOLOR 15 .

DEFINE VARIABLE W_SdoOtros AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Costas + Honor.+ Poliza" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ValPag AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor del Abono" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE W_VrCheq AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Cheques" 
     VIEW-AS FILL-IN 
     SIZE 16.43 BY .81
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE W_VrEfec AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Efectivo" 
     VIEW-AS FILL-IN 
     SIZE 16.43 BY .81
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.43 BY 11.31.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32.14 BY 3.96.

DEFINE RECTANGLE RECT-320
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35.14 BY 11.27.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Cheq FOR 
      TCheq SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Cheq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Cheq Wwin _FREEFORM
  QUERY Br_Cheq NO-LOCK DISPLAY
      TCheq.Bco FORMAT "99":U column-LABEL "Bco"
      TCheq.Nro               column-LABEL "No.Cheque"
      TCheq.Val               column-LABEL "Valor C/Cheque"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 36.43 BY 5.23 FIT-LAST-COLUMN TOOLTIP "Con Click selecciona el cheque".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Cheq
     W_NumCBan AT ROW 1.96 COL 1.29 COLON-ALIGNED NO-LABEL
     W_NumCChe AT ROW 1.96 COL 7 COLON-ALIGNED NO-LABEL
     W_VrCCheq AT ROW 1.96 COL 18.14 COLON-ALIGNED NO-LABEL
     W_VrTotCh AT ROW 1.96 COL 35.72 COLON-ALIGNED NO-LABEL
     Br_Cheq AT ROW 3.35 COL 1.86
     Btn_Eliminar AT ROW 4.15 COL 38.86
     Btn_SalirCheq AT ROW 6.96 COL 41.43
     "Bco.   Cheque       Valor                       TOTAL CHEQUES" VIEW-AS TEXT
          SIZE 51.72 BY .85 AT ROW 1.15 COL 3.29
          FGCOLOR 7 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 6.43 ROW 6.15
         SIZE 58 BY 8.65
         BGCOLOR 17 
         TITLE "Cheques del Abono".

DEFINE FRAME F_Cre
     w_otroscargos AT ROW 7.46 COL 62 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     Btn_salir AT ROW 13.38 COL 81
     Creditos.Categoria AT ROW 12.85 COL 14.14 COLON-ALIGNED
          LABEL "Categoria" FORMAT "X(2)"
          VIEW-AS FILL-IN 
          SIZE 11 BY .69
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cta_Contable AT ROW 14 COL 42.72 COLON-ALIGNED
          LABEL "Cta-Contable" FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Num_Credito AT ROW 1.27 COL 14 COLON-ALIGNED
          LABEL "Num_Crédito" FORMAT "999999999"
          VIEW-AS FILL-IN 
          SIZE 11.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuota AT ROW 1.27 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Desembolso AT ROW 3 COL 14.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Tasa AT ROW 2.15 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_AboCap AT ROW 3.08 COL 42.86 COLON-ALIGNED
     Creditos.Fec_Pago AT ROW 3.81 COL 14.14 COLON-ALIGNED
          LABEL "Fecha Prox.Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Capital AT ROW 3.04 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_AboInt AT ROW 4.15 COL 42.86 COLON-ALIGNED
     Creditos.Fec_UltPago AT ROW 4.69 COL 14.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Corrientes AT ROW 3.92 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_AboIntMor AT ROW 5.23 COL 42.86 COLON-ALIGNED
     W_SdoDeuda AT ROW 5.5 COL 14.14 COLON-ALIGNED
     Creditos.Int_DifCobro AT ROW 4.81 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Plazo AT ROW 6.31 COL 14.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     w_AboIntDifCob AT ROW 6.31 COL 42.86 COLON-ALIGNED
     Creditos.Int_Anticipado AT ROW 5.69 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Proyectado AT ROW 7.12 COL 14.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_MorCobrar AT ROW 6.62 COL 80 COLON-ALIGNED
          LABEL "Mora + DifCobMora"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 12 FGCOLOR 15 
     W_AboIntAnt AT ROW 7.38 COL 42.86 COLON-ALIGNED
     Creditos.Cuo_Pagadas AT ROW 7.92 COL 14.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_SdoOtros AT ROW 7.5 COL 80 COLON-ALIGNED
     W_AboCostas AT ROW 8.46 COL 42.86 COLON-ALIGNED
     Creditos.Val_Atraso AT ROW 8.88 COL 14.14 COLON-ALIGNED
          LABEL "Valor Vencido" FORMAT "->>,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY .65
          BGCOLOR 12 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.86 BY 14.38
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON Btn_salir.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Cre
     Creditos.Dias_Atraso AT ROW 9.54 COL 14.14 COLON-ALIGNED
          LABEL "Dias_Vencidos" FORMAT "-99999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Btn_Validadora AT ROW 13.92 COL 2 HELP
          "Botòn Oculto, para independizar Impresiòn de la Tx."
     W_AboPoliza AT ROW 9.54 COL 42.86 COLON-ALIGNED
     W_CuoPag AT ROW 1.27 COL 55.14 COLON-ALIGNED
     W_VrEfec AT ROW 9.62 COL 77 COLON-ALIGNED
     W_VrCheq AT ROW 10.73 COL 77 COLON-ALIGNED
     Btn_Grabar AT ROW 13.38 COL 66
     Creditos.Cuo_Atraso AT ROW 10.35 COL 14.14 COLON-ALIGNED
          LABEL "Cuotas Vencidas" FORMAT "-9999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .65
          BGCOLOR 12 FGCOLOR 15 
     W_AboHonora AT ROW 10.62 COL 42.86 COLON-ALIGNED
     Creditos.Provision AT ROW 11.15 COL 14.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NoDistribuido AT ROW 11.69 COL 42.86 COLON-ALIGNED
     Creditos.Fec_Reestructurado AT ROW 12.04 COL 14.14 COLON-ALIGNED
          LABEL "Fec-Reestruct."
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_ValPag AT ROW 12.77 COL 42.86 COLON-ALIGNED
     BUTTON-163 AT ROW 14.08 COL 7.86
     "Información del Crédito" VIEW-AS TEXT
          SIZE 19.57 BY .73 AT ROW 2.15 COL 3.57
          FGCOLOR 7 FONT 5
     "Distribuciòn Preliminar del Abono" VIEW-AS TEXT
          SIZE 28.57 BY .65 AT ROW 2.27 COL 30.86
          FGCOLOR 7 FONT 5
     RECT-296 AT ROW 2.46 COL 1.72
     RECT-319 AT ROW 8.62 COL 64.86
     RECT-320 AT ROW 2.5 COL 28.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.86 BY 14.38
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON Btn_salir.


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
         TITLE              = "Recaudos de Creditos"
         COLUMN             = 6.72
         ROW                = 5.73
         HEIGHT             = 14.5
         WIDTH              = 96.86
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
/* SETTINGS FOR FRAME F_Cheq
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB Br_Cheq W_VrTotCh F_Cheq */
ASSIGN 
       FRAME F_Cheq:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN W_NumCBan IN FRAME F_Cheq
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN W_NumCChe IN FRAME F_Cheq
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN W_VrTotCh IN FRAME F_Cheq
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Cre
   Custom                                                               */
/* SETTINGS FOR BUTTON Btn_Grabar IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Grabar:HIDDEN IN FRAME F_Cre           = TRUE.

/* SETTINGS FOR BUTTON Btn_Validadora IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Validadora:HIDDEN IN FRAME F_Cre           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-163 IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-163:HIDDEN IN FRAME F_Cre           = TRUE.

/* SETTINGS FOR FILL-IN Creditos.Categoria IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Cta_Contable IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Cuota IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Pagadas IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Dias_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Fec_Desembolso IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Fec_Pago IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Reestructurado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_Anticipado IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_Corrientes IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_DifCobro IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_MorCobrar IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Num_Credito IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Plazo IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Provision IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Capital IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Proyectado IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Tasa IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Val_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
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
/* SETTINGS FOR FILL-IN w_AboIntDifCob IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AboIntMor IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AboPoliza IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NoDistribuido IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       w_otroscargos:READ-ONLY IN FRAME F_Cre        = TRUE.

/* SETTINGS FOR FILL-IN W_SdoDeuda IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_SdoOtros IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_VrCheq IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       W_VrCheq:HIDDEN IN FRAME F_Cre           = TRUE
       W_VrCheq:READ-ONLY IN FRAME F_Cre        = TRUE.

/* SETTINGS FOR FILL-IN W_VrEfec IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       W_VrEfec:HIDDEN IN FRAME F_Cre           = TRUE
       W_VrEfec:READ-ONLY IN FRAME F_Cre        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Cheq
/* Query rebuild information for BROWSE Br_Cheq
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCheq NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Cheq */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cheq
/* Query rebuild information for FRAME F_Cheq
     _Query            is NOT OPENED
*/  /* FRAME F_Cheq */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cre
/* Query rebuild information for FRAME F_Cre
     _Query            is NOT OPENED
*/  /* FRAME F_Cre */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON END-ERROR OF Wwin /* Recaudos de Creditos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON WINDOW-CLOSE OF Wwin /* Recaudos de Creditos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Cheq
&Scoped-define SELF-NAME Br_Cheq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cheq Wwin
ON MOUSE-SELECT-CLICK OF Br_Cheq IN FRAME F_Cheq
DO:
  IF AVAIL(TCheq) THEN
     ASSIGN W_NumCBan = TCheq.Bco 
            W_NumCBan:SCREEN-VALUE = STRING(TCheq.Bco)
            W_NumCChe = TCheq.Nro 
            W_NumCChe:SCREEN-VALUE = TCheq.Nro
            W_VrCCheq = TCheq.Val
            W_VrCCheq:SCREEN-VALUE = STRING(TCheq.Val). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Eliminar Wwin
ON CHOOSE OF Btn_Eliminar IN FRAME F_Cheq /* Eliminar Selecciòn */
DO:
  IF AVAIL(TCheq) THEN DO:
     DELETE Tcheq.

     RUN Reab_Query.
  END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar Wwin
ON CHOOSE OF Btn_Grabar IN FRAME F_Cre /* Grabar Operación */
DO:
  IF W_Valpag LE 0 THEN DO:                                                    
     MESSAGE "Falta valor para Distribuir..." VIEW-AS ALERT-BOX.               
     APPLY "ENTRY" TO W_ValPag.                                                
     RETURN NO-APPLY.                                                          
  END.   

  IF W_ValPag GT (Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
                  Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
                  Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado) THEN DO:
     MESSAGE "El valor a Pagar no puede superar el Sdo-Total de la deuda..." VIEW-AS ALERT-BOX. 
                  
     ASSIGN W_ValPag = (Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
                        Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
                        Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado)
            W_ValPag:SCREEN-VALUE = STRING(W_ValPag). 
     APPLY "ENTRY" TO W_ValPag.                                                
     RETURN NO-APPLY.           
  END.

  IF W_Valpag NE (W_VrEfec + W_VrCheq) THEN DO:                                                    
     MESSAGE "El valor para Distribuir es Diferente del Efectivo + Vr.Cheque..." VIEW-AS ALERT-BOX.               
     APPLY "ENTRY" TO W_VrEfec.                                                
     RETURN NO-APPLY.                                                          
  END.   
                                                                               
  IF W_VrCheq GT 0 THEN DO:              
     FIND FIRST TCheq NO-ERROR.
     IF NOT AVAIL(Tcheq) THEN DO:
        MESSAGE "Debe digitarse cheque, banco y valor. Rectifique!"    
              VIEW-AS ALERT-BOX ERROR.                                         
        APPLY "entry" TO W_VrCheq.                                                
        RETURN NO-APPLY.                                                          
     END.
  END.                                                                         
                                                                               
  FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ Op_Capital NO-LOCK NO-ERROR.
  IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN
      W_Cbte = Operacion.Comprobante.
  ELSE DO:
       RUN MostrarMensaje IN W_Manija (INPUT 143,OUTPUT W_Ok).
       RETURN NO-APPLY.
  END.
                                                                           
  FIND Comprobantes WHERE Comprobantes.Agencia   EQ W_Agencia AND
                        Comprobantes.Comprobante EQ W_Cbte NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Comprobantes THEN DO:
     MESSAGE "No se ha encontrado el comprobante para la contabilización" SKIP
           "Rectifique con el Administrador!"
           VIEW-AS ALERT-BOX ERROR. 
     RETURN NO-APPLY.
  END.
  ELSE
     ASSIGN W_Cbte = Comprobantes.Comprobante.
     
  IF W_SdoOtros GT 0 THEN DO:
     FIND FIRST COBROS WHERE COBROS.NIT = CREDITOS.NIT AND Cobros.Estado = 1 NO-LOCK NO-ERROR.
     IF NOT AVAILABLE (COBROS) THEN DO:
        MESSAGE "Crèdito en Cobro Jurìdico sin acuerdo de Pago..." SKIP
                "No se acepta Abono." VIEW-AS ALERT-BOX.
        RETURN.
     END.
  END.

  MESSAGE "El Valor Total a Abonar es $ " W_ValPag SKIP
          "                            Continue solo si està Segura(0)...?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Grabar Operaciòn"
      UPDATE W_RptaAbono AS LOGICAL.
     
  IF NOT W_RptaAbono THEN
     RETURN.

  DO TRANSACTION ON ERROR UNDO:
     W_NumSeq = NEXT-VALUE(Sec_Taquilla).
     
     RUN Transaccion NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Se ha producido un error en la grabación de la transacción" SKIP
                "consulte con el Administrador del Sistema" VIEW-AS ALERT-BOX ERROR.
        RETURN.
     END.     
  END. /*fin tx*/
  
  APPLY "choose" TO Btn_Validadora.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_salir Wwin
ON CHOOSE OF Btn_salir IN FRAME F_Cre /* Salir */
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


&Scoped-define FRAME-NAME F_Cheq
&Scoped-define SELF-NAME Btn_SalirCheq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalirCheq Wwin
ON CHOOSE OF Btn_SalirCheq IN FRAME F_Cheq /* Btn_Salir */
DO: 
   ASSIGN FRAME F_Cheq:VISIBLE   = FALSE
          W_NumCBan:SCREEN-VALUE = " "
          W_NumCChe:SCREEN-VALUE = " "
          W_VrCCheq:SCREEN-VALUE = " "
          FRAME F_Cre:SENSITIVE  = TRUE
          W_VrCheq               = W_VrTotCh
          W_VrCheq:SCREEN-VALUE  = STRING(W_VrTotCh)
          W_TotPago              = W_VrEfec + W_VrCheq
          W_ValPag               = W_TotPago
          W_ValPag:SCREEN-VALUE  = STRING(W_ValPag).

  RUN Muestra_DistPrel.

  APPLY "ENTRY" TO W_ValPag.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalirCheq Wwin
ON MOUSE-SELECT-CLICK OF Btn_SalirCheq IN FRAME F_Cheq /* Btn_Salir */
DO:
  APPLY "CHOOSE" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME Btn_Validadora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Validadora Wwin
ON CHOOSE OF Btn_Validadora IN FRAME F_Cre /* Validar */
DO:
  MESSAGE "Impresora(SI)   Ò    Validadora(NO)" VIEW-AS ALERT-BOX QUESTION BUTTONS 
       YES-NO  TITLE "Medio de Impresiòn" UPDATE RpaVI AS LOGICAL.                   
                                                                                  
  IF RpaVI THEN DO:                                                                  
     RUN F-Taquilla.r (INPUT W_NumSeq, INPUT "").                                    
     MESSAGE "Aliste papel para Segunda Copia ".                                     
     RUN F-Taquilla.r (INPUT W_NumSeq, INPUT "").                                    
  END.                                                                               
  ELSE                                                                               
     RUN Imp_Validadora (INPUT W_NumSeq, INPUT "Abono a Crédito").          
       
  FOR EACH TCheq: DELETE TCheq. END.   
    
  APPLY "choose" TO Btn_Salir IN FRAME F_Cre.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-163
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-163 Wwin
ON CHOOSE OF BUTTON-163 IN FRAME F_Cre /* Historial de Créditos */
DO:
  ASSIGN WWin:SENSITIVE = NO.
  WWin:MOVE-TO-BOTTOM().

  RUN W-Hist_Creditos.r (INPUT P_NitCli).
  
  ASSIGN WWin:SENSITIVE = YES.
  WWin:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CuoPag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CuoPag Wwin
ON ENTRY OF W_CuoPag IN FRAME F_Cre /* Cuotas a Pagar */
DO:
  /*MESSAGE "en entry de W_cuopag" VIEW-AS ALERT-BOX.*/
  ASSIGN W_SiEntry = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CuoPag Wwin
ON LEAVE OF W_CuoPag IN FRAME F_Cre /* Cuotas a Pagar */
DO:
  DEFINE VAR W_NroDia AS INTEGER FORMAT "99".
  DEFINE VAR W_NroPer AS INTEGER FORMAT "99".
  DEFINE VAR W_CuoTras AS INTEGER FORMAT "999".
  DEFINE VAR W_Nrocuopag AS INTEGER FORMAT "999".
  
  ASSIGN W_CuoPag.

  IF NOT W_SiEntry THEN DO:
     W_SiEntry = FALSE.
     RETURN.
  END.
  W_SiEntry = FALSE.

DO WITH FRAME F_Cre:
  IF (Creditos.Plazo - Creditos.Cuo_pagadas) LT W_CuoPag THEN DO:
      MESSAGE "Cuotas a pagar: " W_CuoPag SKIP
              "Mayor que plazo - Cuotas Pagadas: " Creditos.Plazo Creditos.Cuo_pagadas VIEW-AS alert-BOX.
      ASSIGN W_CuoPag              = (Creditos.Plazo - Creditos.Cuo_pagadas)
             W_CuoPag:SCREEN-VALUE = STRING(W_CuoPag).
  END.
  
  CASE Creditos.Per_Pago:
    WHEN 1 THEN ASSIGN W_NroDia = 7
                       W_NroPer = 52
                       W_CuoTras = ((W_Fecha - Creditos.Fec_Desembolso) / 7) + 1.
    WHEN 2 THEN ASSIGN W_NroDia = 10
                       W_NroPer = 36
                       W_CuoTras = ((W_Fecha - Creditos.Fec_Desembolso) / 10) + 1.                       
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
         P_VrCapital = 0
         W_ValPag              = 0
         W_VrEfec:SCREEN-VALUE = "0"
         W_ValPag:SCREEN-VALUE = "0".
         
  IF W_CuoPag GT 0 THEN
     ASSIGN W_ValPag           = (W_CuoPag * Creditos.Cuota) + Creditos.INT_MorCobrar + 
                                  Creditos.INT_MoraDifCob + W_SdoOtros   
         W_VrEfec:SCREEN-VALUE = STRING(W_ValPag)
         W_ValPag:SCREEN-VALUE = STRING(W_ValPag + W_VrCheq)
         W_ValPag.

  RUN Muestra_DistPrel.

  /*APPLY "ENTRY" TO W_VrEfec.
 4RETURN NO-APPLY.*/
  
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cheq
&Scoped-define SELF-NAME W_NumCBan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NumCBan Wwin
ON LEAVE OF W_NumCBan IN FRAME F_Cheq
DO:
     ASSIGN W_NumCBan.
     
     FIND Bancos WHERE Bancos.Cod_Compensa EQ W_NumCBan NO-LOCK NO-ERROR.       
     IF NOT AVAILABLE Bancos THEN DO:                                          
        MESSAGE "No existe el banco dentro de la tabla de Bancos" SKIP         
                "Cominiquese con el Administrador para que lo cree" SKIP       
                 VIEW-AS ALERT-BOX ERROR.                                      
        APPLY "ENTRY" TO SELF.
     END.                                                                      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NumCChe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NumCChe Wwin
ON LEAVE OF W_NumCChe IN FRAME F_Cheq
DO:
  ASSIGN W_NumCChe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME W_ValPag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_ValPag Wwin
ON LEAVE OF W_ValPag IN FRAME F_Cre /* Valor del Abono */
DO:
  ASSIGN W_ValPag.

  RUN Muestra_DistPrel.

  ASSIGN W_VrEfec = W_ValPag
         W_VrEfec:SCREEN-VALUE = STRING(W_ValPag).
           
/*  APPLY "ENTRY" TO W_VrEfec.
  RETURN NO-APPLY.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cheq
&Scoped-define SELF-NAME W_VrCCheq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrCCheq Wwin
ON LEAVE OF W_VrCCheq IN FRAME F_Cheq
DO:
  ASSIGN W_VrCCheq.

  IF W_VrCCheq GT 0 AND W_NumCBan GT 0 AND W_NumCChe GT "0" THEN DO:
     FIND Bancos WHERE Bancos.Cod_Compensa EQ W_NumCBan NO-LOCK NO-ERROR.       
     IF NOT AVAILABLE Bancos THEN DO:
        MESSAGE "Banco Inexistente." VIEW-AS ALERT-BOX.
        RETURN.
     END.

     FIND FIRST TCheq WHERE TCheq.Bco = W_NumCBan
                        AND TCheq.Nro = W_NumCChe NO-ERROR.
     IF NOT AVAIL(TCheq) THEN
        CREATE TCheq.

     ASSIGN TCheq.Bco = W_NumCBan
            TCheq.Nro = W_NumCChe
            TCheq.Val = W_VrCCheq
            TCheq.Cje = Bancos.Dia_Canje.

     RUN Reab_Query.

     APPLY "ENTRY" TO W_NumCBan.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME W_VrCheq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrCheq Wwin
ON LEAVE OF W_VrCheq IN FRAME F_Cre /* Valor Cheques */
DO:
  ASSIGN W_VrCheq
         W_TotPago = W_VrEfec + W_VrCheq
         W_ValPag  = W_TotPago
         W_ValPag:SCREEN-VALUE = STRING(W_ValPag).

  IF W_VrCheq GT 0 THEN DO:
     /*APPLY "Leave" TO W_ValPag.*/

     /* APPLY "ENTRY" TO W_NumChe.
     RETURN NO-APPLY.*/

    /* ENABLE W_NumChe W_NumBan WITH FRAME F_Cre.*/

     ASSIGN FRAME F_Cre:SENSITIVE = FALSE
            FRAME F_Cheq:VISIBLE  = TRUE. 

     FIND FIRST TCheq NO-ERROR.
     IF NOT AVAIL(TCheq) THEN
        ASSIGN W_VrTotCh = 0
               W_VrTotCh:SCREEN-VALUE = "0".

     APPLY "ENTRY" TO W_NumCBan.
     RETURN NO-APPLY.
  END.
  ELSE
    FOR EACH TCheq: DELETE TCheq. END.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_VrEfec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrEfec Wwin
ON LEAVE OF W_VrEfec IN FRAME F_Cre /* Valor Efectivo */
DO:
  ASSIGN W_VrEfec
         W_TotPago = W_VrEfec + W_VrCheq
         W_ValPag  = W_TotPago
         W_ValPag:SCREEN-VALUE = STRING(W_ValPag).

  APPLY "ENTRY" TO W_VrCheq.
  /*RETURN NO-APPLY.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cheq
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Partidas Wwin 
PROCEDURE Contabilizar_Partidas :
/*------------------------------------------------------------------------------
  Solo las Contrapartidas, Los valores al Haber de Crèditos ya los contabilizò el
  programa AboCredito.P.
--------------------------------------------------------------------------------*/  
IF W_VrEfec GT 0 THEN DO:
 /* CREATE Mov_Contable.          /*La Contrapartida de Caja en esta agencia*/
  ASSIGN Mov_Contable.Agencia        = W_Agencia
         Mov_Contable.Comprobante    = W_Cbte
         Mov_Contable.Cuenta         = W_Caja
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Abono a Crèdito"
         Mov_Contable.Usuario        = W_Usuario
         Mov_contable.Nit            = Creditos.Nit
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = Creditos.Agencia
         Mov_Contable.Num_Documento  = W_NumSeq
         Mov_Contable.Doc_Referencia = STRING(Creditos.Num_Credito)
         Mov_contable.Enlace         = STRING(W_NumSeq)
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion 
         Mov_Contable.DB             = W_VrEfec NO-ERROR.  */
  IF ERROR-STATUS:ERROR THEN 
     RETURN ERROR.  
END.

IF W_VrCheq GT 0 THEN FOR EACH TCheq:
  /*CREATE Mov_Contable.          /*La Contrapartida de Caja(Cheques) en esta agencia*/
  ASSIGN Mov_Contable.Agencia        = W_Agencia
         Mov_Contable.Comprobante    = W_Cbte
         Mov_Contable.Cuenta         = W_Banco
         Mov_Contable.Fec_Contable   = W_Fecha
         Mov_Contable.Comentario     = "Abono a Crèdito"
         Mov_Contable.Usuario        = W_Usuario
         Mov_contable.Nit            = Creditos.Nit
         Mov_Contable.Cen_Costos     = 999
         Mov_Contable.Destino        = Creditos.Agencia
         Mov_Contable.Num_Documento  = W_NumSeq
         Mov_Contable.Doc_Referencia = STRING(TCheq.Nro)
         Mov_contable.Enlace         = STRING(W_NumSeq)
         Mov_Contable.Fec_Grabacion  = TODAY
         Mov_Contable.Hora           = TIME
         Mov_Contable.Estacion       = W_Estacion 
         Mov_Contable.DB             = TCheq.Val NO-ERROR.  */
  IF ERROR-STATUS:ERROR THEN 
     RETURN ERROR.  
END.
           
  IF Creditos.Agencia NE W_Agencia THEN DO:
    /* CREATE Mov_Contable.          /*La Contrapartida de SyA en esta agencia*/
     ASSIGN Mov_Contable.Agencia        = W_Agencia                            
            Mov_Contable.Comprobante    = W_Cbte                               
            Mov_Contable.Cuenta         = W_CtaSYA_Fte                               
            Mov_Contable.Fec_Contable   = W_Fecha                              
            Mov_Contable.Comentario     = "Abono a Crèdito, Nit:" + STRING(Creditos.Nit)                    
            Mov_Contable.Usuario        = W_Usuario                            
            Mov_contable.Nit            = STRING(Creditos.Agencia,"999")                         
            Mov_Contable.Cen_Costos     = 999                                  
            Mov_Contable.Destino        = Creditos.Agencia
            Mov_Contable.Num_Documento  = W_NumSeq                             
            Mov_Contable.Doc_Referencia = STRING(Creditos.Num_Credito)         
            Mov_contable.Enlace         = STRING(W_NumSeq)                     
            Mov_Contable.Fec_Grabacion  = TODAY                                
            Mov_Contable.Hora           = TIME                                 
            Mov_Contable.Estacion       = W_Estacion                           
            Mov_Contable.CR             = W_ValPag NO-ERROR.                   */
                                                                               
     IF ERROR-STATUS:ERROR THEN RETURN ERROR.                                  
  
   /*  CREATE Mov_Contable.          /*La Contrapartida de SyA en la agencia Destino*/
     ASSIGN Mov_Contable.Agencia        = Creditos.Agencia                            
            Mov_Contable.Comprobante    = W_Cbte                               
            Mov_Contable.Cuenta         = W_CtaSYA_Fte                               
            Mov_Contable.Fec_Contable   = W_Fecha                              
            Mov_Contable.Comentario     = "Abono a Crèdito, Nit:" + STRING(Creditos.Nit)                    
            Mov_Contable.Usuario        = W_Usuario                            
            Mov_contable.Nit            = STRING(W_Agencia,"999")                         
            Mov_Contable.Cen_Costos     = 999                                  
            Mov_Contable.Destino        = W_Agencia
            Mov_Contable.Num_Documento  = W_NumSeq                             
            Mov_Contable.Doc_Referencia = STRING(Creditos.Num_Credito)         
            Mov_contable.Enlace         = STRING(W_NumSeq)                     
            Mov_Contable.Fec_Grabacion  = TODAY                                
            Mov_Contable.Hora           = TIME                                 
            Mov_Contable.Estacion       = W_Estacion                           
            Mov_Contable.DB             = W_ValPag NO-ERROR.                   */
     IF ERROR-STATUS:ERROR THEN RETURN ERROR.                                  
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
    ASSIGN W_CtaCosDb = CortoLargo.Cta_CostasDB
           W_CtaCosCr = CortoLargo.Cta_CostasCR
           W_CtaSyA_Des  = CortoLargo.Cta_SYA
           W_CtaHonDB    = CortoLargo.Cta_HonorariosDB
           W_CtaPolDB    = CortoLargo.Cta_PolizasDB.
    IF W_CtaSyA_Des EQ "" THEN DO:
       MESSAGE "No esta configurada la cuenta de sucursales y agencias" SKIP
               "para la agencia de trabajo actual" SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.
    /*busca si hay garantias admisibles para cuenta de Capital*/
    FIND FIRST Garantias WHERE Garantias.Num_Credito EQ Creditos.Num_Credito  AND 
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
    ELSE DO:
        MESSAGE "Falta Configurar Liqui_Int con cuentas de interes " SKIP
               "para el producto." SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
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
   MESSAGE "No se ha encontrado la configuracón de CortoLargo" SKIP
           "para el producto de créditos" SKIP(1)
           "Comuniquese con el Administrador!" VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
 END.
 
 IF Creditos.Agencia NE W_Agencia THEN DO:
     FIND CortoLargo WHERE CortoLargo.Agencia         EQ W_Agencia
                   AND   CortoLargo.Clase_Producto    EQ 2
                   AND   CortoLargo.Cod_Producto      EQ Creditos.Cod_Credito
                   AND   CortoLargo.Cta_ContingenteDB NE ""
                   AND   CortoLargo.Cta_ContingenteCR NE ""
                   AND   CortoLargo.Comprobante       NE 0 NO-LOCK NO-ERROR.
     IF AVAILABLE(CortoLargo) THEN 
        W_CtaSYA_FTE = CortoLargo.Cta_SYA.
     ELSE DO:
       MESSAGE "No se encontra la cuenta de sucursales y agencias en cortolargo" SKIP
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
  DISPLAY w_otroscargos W_AboCap W_AboInt W_AboIntMor W_SdoDeuda w_AboIntDifCob 
          W_AboIntAnt W_SdoOtros W_AboCostas W_AboPoliza W_CuoPag W_VrEfec 
          W_VrCheq W_AboHonora W_NoDistribuido W_ValPag 
      WITH FRAME F_Cre IN WINDOW Wwin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Categoria Creditos.Cta_Contable Creditos.Num_Credito 
          Creditos.Cuota Creditos.Fec_Desembolso Creditos.Tasa Creditos.Fec_Pago 
          Creditos.Sdo_Capital Creditos.Fec_UltPago Creditos.Int_Corrientes 
          Creditos.Int_DifCobro Creditos.Plazo Creditos.Int_Anticipado 
          Creditos.Sdo_Proyectado Creditos.Int_MorCobrar Creditos.Cuo_Pagadas 
          Creditos.Val_Atraso Creditos.Dias_Atraso Creditos.Cuo_Atraso 
          Creditos.Provision Creditos.Fec_Reestructurado 
      WITH FRAME F_Cre IN WINDOW Wwin.
  ENABLE w_otroscargos Btn_salir W_CuoPag W_ValPag RECT-296 RECT-319 RECT-320 
      WITH FRAME F_Cre IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cre}
  DISPLAY W_VrCCheq W_VrTotCh 
      WITH FRAME F_Cheq IN WINDOW Wwin.
  ENABLE W_NumCBan W_NumCChe W_VrCCheq Br_Cheq Btn_Eliminar Btn_SalirCheq 
      WITH FRAME F_Cheq IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cheq}
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
   
 /*  CREATE Che_Transito.                                     
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
          Che_Transito.Tip_Remesa       = C_Canje NO-ERROR. */
             
   IF ERROR-STATUS:ERROR THEN DO:                              
      MESSAGE "Error al Grabar en Cheques en Transito... "     
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Error en Taquilla".                               
      RETURN ERROR.                                            
   END.                                                        
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
   
/*   CREATE Taquilla.
   ASSIGN Taquilla.Autorizo         = T_Autorizo
          Taquilla.Nro_Transaccion  = W_NumSeq
          Taquilla.Cod_Compensa     = T_Banco
          Taquilla.Cod_Operacion    = T_CodOper
          Taquilla.Cod_Producto     = T_CodPto
          Taquilla.Contabilizar     = TRUE
          Taquilla.Cuenta           = T_Cuenta
          Taquilla.Cta_Contra       = T_CtraCta
          Taquilla.Duracion         = 0
          Taquilla.Est_Linea        = 0
          Taquilla.Fec_Transaccion  = W_Fecha
          Taquilla.Hora_Transaccion = TIME
          Taquilla.Naturaleza       = T_Nat
          Taquilla.Nit              = T_Nit
          Taquilla.Nro_cuenta       = T_Nrocuenta
          Taquilla.Num_Documento    = T_NumDto
          Taquilla.Num_Retcheque    = T_NumRetche
          Taquilla.Agencia          = T_Agencia
          Taquilla.Age_Destino      = T_OfiDes
          Taquilla.Age_Fuente       = T_OfiFue
          Taquilla.Tip_Producto     = 2
          Taquilla.Usuario          = T_Usuario
          Taquilla.Val_Cheque       = T_ValChe
          Taquilla.Val_Efectivo     = T_ValEfec
          Taquilla.Estacion         = W_Estacion
          Taquilla.Cod_Segmento     = T_Segmento
          Taquilla.Descripcion      = T_Comenta NO-ERROR.*/
          
   IF ERROR-STATUS:ERROR THEN DO:                              
      MESSAGE "Error al Grabar en Tabla Taquilla... "     
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK TITLE "Error en Taquilla".                               
      RETURN ERROR.                                            
   END.                                                                  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_Validadora Wwin 
PROCEDURE Imp_Validadora :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER P_transa   LIKE Taquilla.Nro_Transaccion.
 DEFINE INPUT PARAMETER P_Comentario AS CHARACTER FORMAT "X(62)".

 DEFINE VAR W_NomPcto          AS   CHARACTER FORMAT "X(15)".
 DEFINE VAR W_NomEntidad       AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NitEnti          LIKE Clientes.Nit.
 DEFINE VAR W_ConcatEnti       AS   CHARACTER FORMAT "X(57)".
 DEFINE VAR W_TvrCheque        AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99".
 DEFINE VAR W_TvrEfec          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99".
 DEFINE VAR W_DescOpe          AS   CHARACTER FORMAT "X(40)".
 DEFINE VAR W_VrOpera          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_VrConsig         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_VrRetiro         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR W_PrimerCom        AS   CHARACTER FORMAT "X(100)" INITIAL "".
 DEFINE VAR W_Comentario       AS   CHARACTER FORMAT "X(32)" INITIAL "".
 DEFINE VAR W_Nomofi           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_NomCli           AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_totopera         AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99".
 DEFINE VAR Total_Debito       LIKE Mov_Contable.db.
 DEFINE VAR Total_Credito      LIKE Mov_Contable.db.
 DEFINE VAR W_NomUsu           LIKE Usuarios.Nombre.
 DEFINE VAR W_Rpta             AS   LOGICAL. 


FOR EACH Taquilla WHERE Taquilla.Nro_Transaccion EQ P_Transa NO-LOCK:
      ASSIGN W_DescOpe = "Operación no Existe"
             W_NomPcto = ""
             w_NomCli  = "".

      FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
      IF AVAILABLE(Operacion) THEN
         W_DescOpe = Operacion.Nom_Operacion.

      FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Taquilla.Cod_Producto NO-LOCK NO-ERROR.
      IF AVAILABLE(Pro_Creditos) THEN 
         W_NomPcto = TRIM(Pro_Creditos.Nom_Producto).
    
      IF Taquilla.Naturaleza = "Cr" THEN DO:                            
        ASSIGN w_VrOpera = 0.                                               
        IF Taquilla.Val_Cheque GT 0 THEN                                    
          ASSIGN w_VrOpera = Taquilla.Val_Cheque                            
                 W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.           
        ELSE                                                                
          ASSIGN w_VrOpera = Taquilla.Val_Efectivo                          
                 W_TvrEfec  = W_TvrEfec + Taquilla.Val_Efectivo.            
        Total_Credito  = Total_Credito + w_VrOpera.                         
      END.                                                                  
      ELSE DO:                                                              
        IF Taquilla.Naturaleza = "Db" THEN DO:                              
          ASSIGN w_VrOpera = 0.                                             
          IF Taquilla.Val_Cheque GT 0 THEN                                  
            ASSIGN w_VrOpera = Taquilla.Val_Cheque                          
                   W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.         
          ELSE                                                              
            ASSIGN w_VrOpera = Taquilla.Val_Efectivo                        
                   W_TvrEfec  = W_TvrEfec + Taquilla.Val_Efectivo.          
          Total_Debito  = Total_Debito + w_VrOpera.                         
        END.                                                                
      END.                                                                  
      
      FIND FIRST Clientes WHERE Clientes.nit EQ Taquilla.Nit NO-LOCK NO-ERROR.
      IF AVAILABLE Clientes THEN 
        W_Nomcli = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).

     
      W_Comentario = CAPS(SUBSTRING(TRIM(W_DescOpe),1,16) + " " + TRIM(W_NomPcto)).
      IF Taquilla.Descripcion NE "" THEN
         W_Comentario = Taquilla.Descripcion.

      RUN Imp_Valida.R (INPUT 0,STRING(Taquilla.Age_Destino), Taquilla.Tip_Producto, Taquilla.Nro_Cuenta,
                              W_Comentario, W_Nomcli, W_NomPcto,
                              P_transa, W_VrOpera,Taquilla.Val_Cheque," ").

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Wwin 
PROCEDURE initializeObject :
RUN SUPER.
  ASSIGN Wwin:TITLE = P_NomIns + ", Recaudos para Crèditos, Prog.W-ProRec_Ordinario.W.".
 
  FIND Operacion WHERE Operacion.Cod_Operacion EQ Op_Capital
                   AND Operacion.Comprobante   GT 0 NO-LOCK NO-ERROR.
  IF AVAILABLE Operacion THEN 
     W_Cbte = Operacion.Comprobante.
  ELSE 
    MESSAGE "No se ha encontrado la operación de créditos, con Cpte para la Tx." SKIP
            "Comuniquese con el Administrador!" SKIP
            "La Operacion es: " Op_Capital VIEW-AS ALERT-BOX ERROR.

  FIND FIRST Creditos WHERE Creditos.Nit         EQ P_NitCli AND 
                            Creditos.Cod_Credito EQ P_CodCre AND
                            Creditos.Num_Credito EQ P_NumCre AND
                            Creditos.Tip_Credito EQ P_TipCre NO-LOCK NO-ERROR.
  IF AVAILABLE Creditos THEN DO:
     FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.          
     FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito AND
                             Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito NO-LOCK NO-ERROR.
     IF AVAILABLE Pro_Creditos THEN DO:
        RUN CortoLargoCreditos NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
           APPLY "choose" TO Btn_Salir IN FRAME F_Cre.
     END.
     ELSE MESSAGE "No se encontro el producto de Creditos" VIEW-AS ALERT-BOX ERROR.
        
     FIND FIRST COBROS WHERE COBROS.NIT = CREDITOS.NIT AND Cobros.estado = 1 NO-LOCK NO-ERROR.
     IF AVAILABLE COBROS THEN 
        MESSAGE "Acuerdo de pago" SKIP         
                 Cobros.Val_Compromiso SKIP(1) VIEW-AS ALERT-BOX TITLE "INFORMATIVO".       

     /*busca cuenta de caja-cheque*/
     FIND FIRST Cuentas WHERE Cuentas.Cuenta  GT "0"
                        AND   Cuentas.Cod_FlujoEfec EQ "D"
                     AND   Cuentas.Car_Efectivo     EQ  3
                     AND   Cuentas.Estado           EQ  1 NO-LOCK NO-ERROR.
     IF AVAIL(Cuentas) THEN 
        W_Banco = Cuentas.Cuenta.
     ELSE 
       MESSAGE "No se ha encontrado la cuenta de caja cheques" SKIP
               "para realizar la operacion. " SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.

    
     /*busca cuenta de caja-general*/
     FIND FIRST Cuentas WHERE Cuentas.Cod_FlujoEfec EQ "D"
                        AND   Cuentas.Car_Efectivo  EQ  2
                        AND   Cuentas.Estado        EQ  1 NO-LOCK NO-ERROR.
     IF AVAILABLE Cuentas THEN 
        W_Caja = Cuentas.Cuenta.
     ELSE 
       MESSAGE "No se ha encontrado la cuenta de caja efectivo" SKIP
               "para realizar la operacion. " SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
     
     RUN Mostrar_Credito.
  END.
  
  IF NOT AVAIL(Creditos) OR NOT AVAIL(Operacion) OR NOT AVAIL(Cuentas)
  OR W_Banco LE "0"      OR NOT AVAIL(Clientes)  OR NOT AVAIL(Pro_Creditos) THEN
     APPLY "choose" TO Btn_Salir IN FRAME F_Cre. 

  FRAME F_Cheq:VISIBLE = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Credito Wwin 
PROCEDURE Mostrar_Credito :
DO WITH FRAME F_Cre:
   ASSIGN Creditos.Fec_Desembolso:SCREEN-VALUE = STRING(Creditos.Fec_Desembolso)
          Creditos.Fec_Pago:SCREEN-VALUE = STRING(Creditos.Fec_Pago)
          Creditos.Fec_Ultpago:SCREEN-VALUE = STRING(Creditos.Fec_Ultpago)
          W_SdoDeuda:SCREEN-VALUE = STRING(Creditos.Honorarios + Creditos.Costas + Creditos.Polizas +
                        Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
                        Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado)
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
          Creditos.Sdo_Capital:SCREEN-VALUE    = STRING(Creditos.Sdo_Capital)
          Creditos.INT_MorCobrar:SCREEN-VALUE  = STRING(Creditos.INT_MorCobrar + Creditos.INT_MoraDifCob)
          Creditos.Plazo:SCREEN-VALUE          = STRING(Creditos.Plazo)
          Creditos.Tasa:SCREEN-VALUE           = STRING(Creditos.Tasa)
          Creditos.Num_Credito:SCREEN-VALUE    = STRING(Creditos.Num_Credito)
          W_SdoOtros:SCREEN-VALUE              = STRING(Creditos.Costas +
                                                      Creditos.Honorarios + Creditos.Polizas)
          W_SdoOtros.
   IF creditos.cod_credito = 570 THEN DO:
      ASSIGN W_otrosCargos:SCREEN-VALUE = "Otros Cargos + Sobrecupo:".
      IF creditos.sdo_capital GT Creditos.monto THEN
             W_SdoDeuda:SCREEN-VALUE = STRING( DECIMAL(W_SdoDeuda:SCREEN-VALUE) - (creditos.sdo_capital - Creditos.monto)) .
   END.
   ELSE
      W_otrosCargos:SCREEN-VALUE = "Costas + Honor + Polizas:".

   DISPLAY Creditos.Categoria Creditos.Cta_Contable.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Muestra_DistPrel Wwin 
PROCEDURE Muestra_DistPrel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF W_ValPag GT (Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
                  Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
                  Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado) THEN DO:
     MESSAGE "El valor a Pagar no puede superar el Sdo-Total de la deuda..." VIEW-AS ALERT-BOX. 
                  
     ASSIGN W_ValPag = (Creditos.Honorarios     + Creditos.Costas         + Creditos.Polizas        +
                        Creditos.Int_MorCobrar  + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
                        Creditos.Int_DifCobro   + Creditos.Sdo_Capital    - Creditos.Int_Anticipado)
            W_ValPag:SCREEN-VALUE IN FRAME F_Cre = STRING(W_ValPag).                    
  END.
            
  IF W_ValPag GT 0 THEN DO:
     RUN AboCredito.R         /*Distribuye abonos en Créditos,sin actualizar*/
            (INPUT NO,
             INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
             INPUT Creditos.Num_Credito,W_ValPag,
             INPUT 0,0,0,
             OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IDifCobMor, OUTPUT P_IMora,
             OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capital,
             OUTPUT P_VlrNoDist).     
   
     ASSIGN W_AboInt:SCREEN-VALUE       = STRING(P_ICte)       
            W_AboCap:SCREEN-VALUE       = STRING(P_Capital)    
            W_AboIntMor:SCREEN-VALUE    = STRING(P_IMora + P_IDifCobMor)      
            W_AboIntAnt:SCREEN-VALUE    = STRING(P_IAntic)     
            W_AboIntDifCob:SCREEN-VALUE = STRING(P_IDifCob)    
            W_AboPoliza:SCREEN-VALUE    = STRING(P_Poliza)     
            W_AboHonora:SCREEN-VALUE    = STRING(P_Honora)     
            W_AboCostas:SCREEN-VALUE    = STRING(P_Costas)     
            W_NoDistribuido:SCREEN-VALU = STRING(P_VlrNoDist). 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reab_Query Wwin 
PROCEDURE Reab_Query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_VrTotCh = 0
         W_VrTotCh:SCREEN-VALUE IN FRAME F_Cheq = "0".

  CLOSE QUERY Br_Cheq.
  OPEN  QUERY BR_Cheq FOR EACH TCheq INDEXED-REPOSITION.

  FOR EACH TCheq: 
      ASSIGN W_VrTotCh = W_VrTotCh + TCheq.Val
             W_VrTotCh:SCREEN-VALUE = STRING(W_VrTotCh).

  END.
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Taquilla Wwin 
PROCEDURE Taquilla :
IF W_VrEfec GT 0 THEN DO:
    RUN AboCredito.R    /*Distribuye abono-Efectivo en Créditos,graba Mov_creditos,Mov_Contable y PlanPagos*/
      (INPUT YES,
       INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
       INPUT Creditos.Num_Credito,W_VrEfec,
       INPUT W_Cbte,W_NumSeq,0,
       OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IDifCobMor, OUTPUT P_IMora,
       OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capital,
       OUTPUT P_VlrNoDist) NO-ERROR.

    IF ERROR-STATUS:ERROR OR P_VlrNoDist NE 0 THEN DO:
       MESSAGE "Prog.AboCredito.P...Retornò Error, Ò," SKIP
            "Retornò Valor no-distribuìdo $" P_VlrNoDist SKIP
            "               No se acepta el Abono." VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.                                                                                 
    END. 
 END.

 IF W_VrCheq GT 0 THEN DO:
    RUN AboCredito.R    /*Distribuye abono-Cheque en Créditos,graba Mov_creditos,Mov_Contable y PlanPagos*/
      (INPUT YES,
       INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
       INPUT Creditos.Num_Credito,W_VrCheq,
       INPUT W_Cbte,W_NumSeq,1,
       OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IDifCobMor, OUTPUT P_IMora,
       OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capital,
       OUTPUT P_VlrNoDist) NO-ERROR.

    IF ERROR-STATUS:ERROR OR P_VlrNoDist NE 0 THEN DO:                          
       MESSAGE "Prog.AboCredito.P...Retornò Error, Ò," SKIP                     
               "Retornò Valor no-distribuìdo $" P_VlrNoDist SKIP                
               "               No se acepta el Abono." VIEW-AS ALERT-BOX ERROR. 
       RETURN ERROR.                                                                              
    END. 

    FIND FIRST TCheq NO-ERROR.
 END.
 
 IF Creditos.Agencia EQ W_Agencia THEN DO:
    IF W_VrEfec GT 0 THEN
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT Op_Capital, 
                     INPUT Creditos.Cod_Credito, INPUT W_CtaCap,     INPUT W_Caja,
                     INPUT "CR",                 INPUT Creditos.Nit, INPUT STRING(Creditos.Num_Credito), 
                     INPUT STRING(Creditos.Num_Credito), INPUT 0,    INPUT W_Agencia,           
                     INPUT Creditos.Agencia, INPUT W_Agencia,        INPUT "2",            
                     INPUT W_Usuario,        INPUT 0,                INPUT W_VrEfec,
                     INPUT 0, INPUT "Abono al Crédito") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
       RETURN ERROR.  

    IF W_VrCheq GT 0 THEN
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT TCheq.Bco,   INPUT Op_Capital, 
                     INPUT Creditos.Cod_Credito, INPUT W_CtaCap,     INPUT W_Banco,
                     INPUT "CR",                 INPUT Creditos.Nit, INPUT STRING(Creditos.Num_Credito), 
                     INPUT STRING(Creditos.Num_Credito), INPUT TCheq.Nro,    INPUT W_Agencia,           
                     INPUT Creditos.Agencia, INPUT W_Agencia,        INPUT "2",            
                     INPUT W_Usuario,        INPUT W_VrCheq,         INPUT 0,
                     INPUT 0, INPUT "Abono al Crédito") NO-ERROR.
 END.
 ELSE DO:
    IF W_VrEfec GT 0 THEN
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,  INPUT Op_Capital,                                        
                     INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,INPUT W_Caja,                     
                     INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito),     
                     INPUT STRING(Creditos.Num_Credito), INPUT 0,   INPUT W_Agencia,                          
                     INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "2",                              
                     INPUT W_Usuario,        INPUT 0,               INPUT W_VrEfec,                               
                     INPUT 0, INPUT "SYA - Abono a Crédito") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
       RETURN ERROR.

    IF W_VrCheq GT 0 THEN
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT TCheq.Bco,  INPUT Op_Capital,                                        
                     INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,INPUT W_Banco,                     
                     INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito),     
                     INPUT STRING(Creditos.Num_Credito), INPUT TCheq.Nro,   INPUT W_Agencia,                          
                     INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "2",                              
                     INPUT W_Usuario,        INPUT W_VrCheq,        INPUT 0,                               
                     INPUT 0, INPUT "SYA - Abono a Crédito") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
       RETURN ERROR.
                                                                   
    IF W_VrEfec GT 0 THEN
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT Op_Capital,                                           
                     INPUT Creditos.Cod_Credito, INPUT W_CtaCap,    INPUT W_CtaSYA_Fte,                    
                     INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito),     
                     INPUT STRING(Creditos.Num_Credito), INPUT 0,   INPUT Creditos.Agencia,                   
                     INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "2",                              
                     INPUT W_Usuario,        INPUT 0,               INPUT W_VrEfec,                               
                     INPUT 0, INPUT "SYA - Abono a Crédito") NO-ERROR.  
    IF ERROR-STATUS:ERROR THEN
       RETURN ERROR. 

    IF W_VrCheq GT 0 THEN
       RUN Gra_Taquilla(INPUT W_Usuario, INPUT TCheq.Bco,   INPUT Op_Capital,                                           
                     INPUT Creditos.Cod_Credito, INPUT W_CtaCap,    INPUT W_CtaSYA_Fte,                    
                     INPUT "CR",            INPUT Creditos.Nit,     INPUT STRING(Creditos.Num_Credito),     
                     INPUT STRING(Creditos.Num_Credito), INPUT TCheq.Nro,   INPUT Creditos.Agencia,                   
                     INPUT Creditos.Agencia, INPUT W_Agencia,       INPUT "2",                              
                     INPUT W_Usuario,        INPUT W_VrCheq,        INPUT 0,                               
                     INPUT 0, INPUT "SYA - Abono a Crédito") NO-ERROR. 
 END.
 
 IF ERROR-STATUS:ERROR THEN
    RETURN ERROR. 
   
 IF W_VrCheq GT 0 THEN FOR EACH TCheq:
    RUN Gra_CheTransito(INPUT TCheq.Bco,INPUT TCheq.Nro,                     INPUT Creditos.Cod_Credito,
                        INPUT 1,        INPUT STRING(Creditos.Num_Credito),  INPUT Creditos.Agencia, 
                        INPUT 2,        INPUT TCheq.Val,                     INPUT TCheq.Cje) NO-ERROR.                       
    IF ERROR-STATUS:ERROR THEN
       RETURN ERROR. 
 END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transaccion Wwin 
PROCEDURE Transaccion :
DEFINE VARIABLE Sobra_Abono LIKE creditos.sdo_capital INITIAL 0.
DEFINE VARIABLE Sobra_Interes LIKE creditos.sdo_capital INITIAL 0.

Grabando:
DO TRANSACTION ON ERROR UNDO Grabando: 
   FIND FIRST COBROS WHERE COBROS.NIT = CREDITOS.NIT AND Cobros.Estado = 1  NO-ERROR.
   IF AVAILABLE COBROS THEN DO: 
      ASSIGN Cobros.Val_Cumplido     = Cobros.Val_Cumplido + W_VALPAG
             Cobros.Fec_Cumplimiento = W_Fecha.

      IF Cobros.Val_Cumplido GE Cobros.Val_Compromiso THEN 
         ASSIGN Cobros.Fec_Cumplimiento = W_Fecha    
                Cobros.Estado           = 2. 
   END.

   FIND CURRENT COBROS NO-LOCK NO-ERROR.
 
   RUN Taquilla NO-ERROR.                                                        
   IF ERROR-STATUS:ERROR THEN DO:                                                
      MESSAGE "Se ha producido un error en la transacción" SKIP                  
              "consulte con el Administrador del Sistema" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.                                                              
   END.                                                                          
                                                                                 
   RUN Contabilizar_Partidas NO-ERROR.   /*Solo contabiliza contrapartida y SyA*/
   IF ERROR-STATUS:ERROR THEN                                                                            
      RETURN ERROR.                                                              
                                                                                 
   RELEASE Taquilla.                                                             
   RELEASE Che_Transito.                                                         
   RELEASE Creditos.                                                             
END. /*Fin Tx*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

