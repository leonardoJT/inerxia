&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE VAR CtrLavado AS LOGICAL INITIAL YES.
DEFINE INPUT PARAMETER P_NomIns AS CHARACTER FORMAT "X(50)".
DEFINE INPUT PARAMETER P_NitCli AS CHARACTER.
DEFINE INPUT PARAMETER P_CodCre AS INTEGER.
DEFINE INPUT PARAMETER P_TipCre AS INTEGER.
DEFINE INPUT PARAMETER P_NumCre AS INTEGER.
DEFINE INPUT PARAMETER P_CodOpe AS INTEGER.
DEFINE INPUT PARAMETER P_Progra AS INTEGER FORMAT "9".

{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR Op_Capital AS INTEGER INITIAL "020101001".
DEFINE VAR OpSeguroVida AS CHARACTER INITIAL "020101010".
DEFINE VAR OpSeguroDeudor AS CHARACTER INITIAL "020101011".
DEFINE VAR Op_Mora AS INTEGER INITIAL "020101002".
DEFINE VAR Op_IntCor AS INTEGER INITIAL "020101003".
DEFINE VAR Op_IntDif AS INTEGER INITIAL "020101004".
DEFINE VAR Op_IntAnt AS INTEGER INITIAL "020101005".
DEFINE VAR Op_Polizas AS INTEGER INITIAL "020101006".
DEFINE VAR Op_Honora AS INTEGER INITIAL "020101007".
DEFINE VAR Op_Costas AS INTEGER INITIAL "020101008".
DEFINE VAR W_cuprot AS INTEGER.
DEFI VAR T_AboK AS DECIMAL.
DEFI VAR T_AboI AS DECIMAL.
DEFI VAR T_SdoI AS DECIMAL.
DEFI VAR T_SdoF AS DECIMAL.
DEFINE VAR P_VrInteres AS DECIMAL.
DEFINE VAR P_VrCapital AS DECIMAL.
DEFINE VAR T_VrInteres AS DECIMAL.
DEFINE VAR T_VrCapital AS DECIMAL.
DEFINE VAR T_VrMora AS DECIMAL.
DEFINE VAR W_TotPago AS DECIMAL.
DEFI VAR W_DocContab AS INTEGER.
DEFINE VAR W_Ok AS LOGICAL.
DEFI VAR W_SiEntry AS LOG INIT FALSE.
DEFINE VAR W_CtaCap AS CHARACTER.
DEFINE VAR W_CtaDifCob_db AS CHARACTER.
DEFINE VAR W_Caja AS CHARACTER.
DEFINE VAR W_Banco AS CHARACTER.
DEFINE VAR W_CtaSya_Des AS CHARACTER.
DEFINE VAR W_CtaSya_Fte AS CHARACTER.
DEFINE VAR W_Cbte AS INTEGER.
DEFINE VAR W_NumSeq AS INTEGER FORMAT "9999999".
DEFINE VAR P_otrosC AS DECIMAL.
DEFINE VAR P_Poliza AS DECIMAL.
DEFINE VAR P_Honora AS DECIMAL.
DEFINE VAR P_Costas AS DECIMAL.

/* oakley */

DEF VAR P_IDifCobMor   LIKE Creditos.Sdo_Capital   INITIAL 0 NO-UNDO.
DEFINE VAR P_SeguroVida AS DECIMAL.
DEFINE VAR P_SeguroDeudor AS DECIMAL.
DEF VAR P_IMora        LIKE Creditos.Sdo_Capital   INITIAL 0 NO-UNDO.
DEF VAR P_IDifCob      LIKE Creditos.Sdo_Capital   INITIAL 0 NO-UNDO.
DEF VAR P_ICte         LIKE Creditos.Sdo_Capital   INITIAL 0 NO-UNDO.
DEF VAR P_IAntic       LIKE Creditos.Sdo_Capital   INITIAL 0 NO-UNDO.
DEF VAR P_Capital      LIKE Creditos.Sdo_Capital   INITIAL 0 NO-UNDO.
DEF VAR P_VlrNoDist    LIKE Creditos.Sdo_Capital   INITIAL 0 NO-UNDO.
DEF VAR P_ComBruta     LIKE Creditos.Com_Bruta     INITIAL 0 NO-UNDO.
DEF VAR P_ComAdicional LIKE Creditos.Com_Adicional INITIAL 0 NO-UNDO.
DEF VAR P_GmfxC        LIKE Creditos.GmfxC         INITIAL 0 NO-UNDO.
DEF VAR P_Segcartera   LIKE Creditos.Seg_cartera   INITIAL 0 NO-UNDO.
DEF VAR P_IAC          LIKE Creditos.Sdo_Capital   INITIAL 0 NO-UNDO.

DEFI TEMP-TABLE TCheq
     FIELD Bco LIKE Taquilla.Cod_Compensa
     FIELD Nro LIKE Taquilla.Num_Retcheque
     FIELD Val LIKE Creditos.Sdo_Capital
     FIELD Cta LIKE Che_transito.Num_Cuenta
     FIELD Cje LIKE Bancos.Dia_Canje.

DEFI TEMP-TABLE tcontrol_pagos LIKE CONTROL_pagos
    FIELD marca AS LOG INITIAL FALSE
    FIELD Otros LIKE Creditos.Sdo_Capital 
    FIELD Dif_TasaM LIKE Creditos.Sdo_Capital
    FIELD Pag_Anticipado LIKE Creditos.Sdo_Capital
    FIELD TOTAL_Cuota  LIKE Creditos.Sdo_Capital
    FIELD intAnticipado AS DECIMAL.

DEF BUFFER btcontrol_pagos FOR tcontrol_pagos.

def var SP_Poliza     like  P_Poliza      .
def var SP_Honora     like  P_Honora      .
def var SP_Costas     like  P_Costas      .
DEFINE VAR SP_SeguroVida AS DECIMAL.
DEFINE VAR SP_SeguroDeudor AS DECIMAL.
def var SP_IDifCobMor like  P_IDifCobMor  .
def var SP_IMora      like  P_IMora       .
def var SP_IDifCob    like  P_IDifCob     .
def var SP_ICte       like  P_ICte        .
def var SP_IAntic     like  P_IAntic      .
def var SP_Capital    like  P_Capital     .
def var SP_VlrNoDist  like  P_VlrNoDist   .
def var SP_IAC        like  P_IAC         .

  DEFINE VAR W_NroDia AS INTEGER FORMAT "99".
  DEFINE VAR W_NroPer AS INTEGER FORMAT "99".
  DEFINE VAR P_NMeses AS INT.
  DEFINE VAR P_NomPer AS CHAR FORMAT "X(15)".
  DEF VAR w_TasaNominal AS DEC FORMAT "->>>>9.9999999".
  DEFINE VAR W_Comple AS LOG.
  DEF VAR w_saldok LIKE Creditos.Sdo_Capital.
  DEF VAR P_Benefi AS LOG.
  DEFINE VARIABLE W_preliquida AS DECIMAL.
DEFINE VARIABLE W_diapreliquida AS INTEGER.

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
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tcontrol_pagos TCheq Facturacion Creditos

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 tcontrol_pagos.Nro_Cuota tcontrol_pagos.Fec_Vcto tcontrol_pagos.TOTAL_Cuota tcontrol_pagos.Cap_pagado tcontrol_pagos.pagos_capitalAcum tcontrol_pagos.Pagos_IntAcum tControl_pagos.causacion tControl_pagos.contingente tControl_Pagos.INT_mora tcontrol_pagos.Otros tcontrol_pagos.Marca   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH tcontrol_pagos NO-LOCK     BY tcontrol_pagos.Nro_Cuota
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME}  FOR EACH tcontrol_pagos NO-LOCK     BY tcontrol_pagos.Nro_Cuota.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 tcontrol_pagos
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 tcontrol_pagos


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

/* Definitions for FRAME F_Factura                                      */
&Scoped-define SELF-NAME F_Factura
&Scoped-define QUERY-STRING-F_Factura FOR EACH Facturacion SHARE-LOCK, ~
             EACH Creditos WHERE TRUE /* Join to Facturacion incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Factura OPEN QUERY {&SELF-NAME} FOR EACH Facturacion SHARE-LOCK, ~
             EACH Creditos WHERE TRUE /* Join to Facturacion incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Factura Facturacion Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-F_Factura Facturacion
&Scoped-define SECOND-TABLE-IN-QUERY-F_Factura Creditos


/* Definitions for FRAME F_Plan                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Plan ~
    ~{&OPEN-QUERY-BROWSE-4}

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

DEFINE BUTTON Btn-Plan 
     LABEL "?" 
     SIZE 3 BY .81.

DEFINE BUTTON Btn_Factura 
     LABEL "Factura Actual" 
     SIZE 30 BY .81.

DEFINE BUTTON Btn_Grabar 
     LABEL "Grabar Operación" 
     SIZE 14.57 BY 1.35.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_Salir" 
     SIZE 14.57 BY 1.35.

DEFINE BUTTON Btn_Validadora 
     LABEL "Validar" 
     SIZE 7 BY .92 TOOLTIP "Botòn Oculto, para independizar Impresiòn de la Tx.".

DEFINE BUTTON BUTTON-163 
     LABEL "Historial de Créditos" 
     SIZE 19.29 BY .92.

DEFINE VARIABLE Fec_corte AS DATE FORMAT "99/99/9999":U 
     LABEL "Fec_corte" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE NomPdadP AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY .69
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W-VlrAboComis AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Comisiones" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W-VlrAboGmfxc AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Gmf x Cobrar" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W-VlrAboSegCart AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Fondo Solidaridad" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Wsegcart AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Fondo Solidaridad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15 .

DEFINE VARIABLE wtotcomision AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Comisiones Red" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15 .

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
     LABEL "Int.Ant Pagados" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_AboIntDifCob AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Interés Contingente" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboIntMor AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Mora + Conting.Mora" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboPoliza AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Póliza" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboSegDeudor AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Seguro Vivienda" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AboSegVida AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Seguro Vida" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AIA AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Int.Ant a pagar" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CuoPag AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Cuotas a Pagar" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE W_DescAnti AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Desc. * Anticipado" 
     VIEW-AS FILL-IN 
     SIZE 17.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DescTM AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Desc. * T.Maxima" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NoDistribuido AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Restante" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_NumFactura AS CHARACTER FORMAT "X(24)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 17 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE w_PagoRecaudo AS CHARACTER FORMAT "X(48)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81
     BGCOLOR 17 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_SdoDeuda AS DECIMAL FORMAT "->>>>>>>,>>9.99" INITIAL 0 
     LABEL "Saldo Tot.Deuda" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 12 FGCOLOR 15 .

DEFINE VARIABLE w_sdomoraCR AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 17 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_SdoOtros AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Costas + Honor.+ Póliza" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_sobrecupo AS CHARACTER FORMAT "X(24)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 17 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_TapaCV AS CHARACTER FORMAT "X(24)":U 
     VIEW-AS FILL-IN 
     SIZE 24.14 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE w_TapaFUP AS CHARACTER FORMAT "X(24)":U 
     VIEW-AS FILL-IN 
     SIZE 22.86 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE W_TSdoVdo AS DECIMAL FORMAT "->>>>>>>,>>9.99" INITIAL 0 
     LABEL "Vr.Para estar al Día" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 12 FGCOLOR 15 .

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
     SIZE 27.29 BY 12.77.

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32.14 BY 2.69.

DEFINE RECTANGLE RECT-320
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34.43 BY 17.12.

DEFINE BUTTON Btn_Salir-2 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_Salir" 
     SIZE 6.43 BY 1.73.

DEFINE VARIABLE FILL-IN-31 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 64 BY .23
     BGCOLOR 32 FGCOLOR 18  NO-UNDO.

DEFINE VARIABLE FILL-IN-32 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 64 BY .27
     BGCOLOR 32 FGCOLOR 18  NO-UNDO.

DEFINE VARIABLE W-OtrosC AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 TOOLTIP "Comisiones Brutas + Comisiones Adicionales"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FcomySobre AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Com/Sob" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_Fdisk AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_FintCtes AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Int Corrient" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_FintMora AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Int. Mora" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_Pagomin AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_PenIntCte AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_PenIntMora AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_PenSegCar AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_PteCapital AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_PteComySobre AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w_segcar AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Fdo.Solid" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15 .

DEFINE VARIABLE w_totatraso AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .81
     BGCOLOR 18 FGCOLOR 15 .

DEFINE RECTANGLE RECT-330
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 5.19.

DEFINE RECTANGLE RECT-331
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 1 BY 18.85
     BGCOLOR 0 .

DEFINE BUTTON Btn_Salir-3 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_Salir" 
     SIZE 11.72 BY 1.08.

DEFINE VARIABLE w_Total AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total..." 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .81 NO-UNDO.

DEFINE VARIABLE newCuota AS DECIMAL FORMAT "$>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Nueva cuota si refinancia" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 2  NO-UNDO.

DEFINE VARIABLE TotD AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo Total Deuda" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81 NO-UNDO.

DEFINE VARIABLE Val_Vdo AS DECIMAL FORMAT "->>,>>>,>>>,>>9" INITIAL 0 
     LABEL "Valor Vencido" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .92
     BGCOLOR 12 FGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      tcontrol_pagos SCROLLING.

DEFINE QUERY Br_Cheq FOR 
      TCheq SCROLLING.

DEFINE QUERY F_Factura FOR 
      Facturacion, 
      Creditos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 Wwin _FREEFORM
  QUERY BROWSE-4 DISPLAY
      tcontrol_pagos.Nro_Cuota         COLUMN-LABEL "Nro."
      tcontrol_pagos.Fec_Vcto          COLUMN-LABEL "Fec!Vencimiento"
      tcontrol_pagos.TOTAL_Cuota       COLUMN-LABEL "Cuota" FORMAT "->>,>>>,>>9"
      tcontrol_pagos.Cap_pagado        COLUMN-LABEL "Abonado" FORMAT "->>,>>>,>>9"
      tcontrol_pagos.pagos_capitalAcum COLUMN-LABEL "Dist!Capital" FORMAT "->>,>>>,>>9"
      tcontrol_pagos.Pagos_IntAcum     COLUMN-LABEL "Dist!Intereses" FORMAT "->>,>>>,>>9"
      tControl_pagos.causacion         COLUMN-LABEL "Causado" FORMAT "->>>,>>>,>>9"
      tControl_pagos.contingente       COLUMN-LABEL "Contingente" FORMAT "->>>,>>>,>>9"
      tControl_Pagos.INT_mora          COLUMN-LABEL "Mora" FORMAT "->>>,>>>,>>9"
      tcontrol_pagos.Otros             COLUMN-LABEL "Dist!Otros" FORMAT "->>,>>>,>>9"
      tcontrol_pagos.Marca             COLUMN-LABEL "Marca"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 89 BY 9.96
         FONT 4 ROW-HEIGHT-CHARS .58 FIT-LAST-COLUMN.

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
         AT COL 17.43 ROW 2.23
         SIZE 58 BY 8.65
         BGCOLOR 17 
         TITLE "Cheques del Abono".

DEFINE FRAME F_Factura
     Facturacion.cuota AT ROW 6.35 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 102
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 15 FGCOLOR 7 
     W-OtrosC AT ROW 6.92 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 226
     FILL-IN-31 AT ROW 11.5 COL 1 NO-LABEL WIDGET-ID 162
     w_Fdisk AT ROW 12.46 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     w_PteCapital AT ROW 12.46 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 132
     w_FintCtes AT ROW 13.35 COL 11 COLON-ALIGNED WIDGET-ID 112
     w_PenIntCte AT ROW 13.35 COL 55 RIGHT-ALIGNED NO-LABEL WIDGET-ID 24
     w_FintMora AT ROW 14.23 COL 10.86 COLON-ALIGNED WIDGET-ID 116
     W_PenIntMora AT ROW 14.23 COL 55.14 RIGHT-ALIGNED NO-LABEL WIDGET-ID 26
     Btn_Salir-2 AT ROW 14.65 COL 57 WIDGET-ID 10
     W_FcomySobre AT ROW 15.12 COL 10.86 COLON-ALIGNED WIDGET-ID 118
     W_PteComySobre AT ROW 15.12 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 130
     w_segcar AT ROW 16.04 COL 11 COLON-ALIGNED WIDGET-ID 184
     w_PenSegCar AT ROW 16.08 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 188
     w_Pagomin AT ROW 17.69 COL 11 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     w_totatraso AT ROW 17.69 COL 40.14 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     Creditos.Fec_UltPago AT ROW 18.62 COL 40.43 COLON-ALIGNED NO-LABEL WIDGET-ID 154
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 18 FGCOLOR 15 
     FILL-IN-32 AT ROW 19.85 COL 1 NO-LABEL WIDGET-ID 164
     "Fec.Ult.Pago:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 18.62 COL 28.72 WIDGET-ID 156
          FGCOLOR 7 
     "Saldo Mora     Pago Total      Pago Mínimo" VIEW-AS TEXT
          SIZE 40 BY .81 AT ROW 1 COL 3 WIDGET-ID 78
     "Fec Fin:" VIEW-AS TEXT
          SIZE 7.86 BY .81 AT ROW 1.77 COL 42.57 WIDGET-ID 146
          FGCOLOR 7 
     "(  - )  Pagos" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 9.08 COL 28.29 WIDGET-ID 90
     " Valor Facturado" VIEW-AS TEXT
          SIZE 15 BY .73 AT ROW 11.73 COL 11.72 WIDGET-ID 98
          FGCOLOR 7 
     " FACTURACION ACTUAL" VIEW-AS TEXT
          SIZE 23 BY .81 AT ROW 2.62 COL 21 WIDGET-ID 158
     "( + ) Fdo Solidaridad" VIEW-AS TEXT
          SIZE 19.86 BY .62 AT ROW 8.15 COL 28.14 WIDGET-ID 182
     " Abonado" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 11.69 COL 30 WIDGET-ID 120
          FGCOLOR 7 
     "( + ) Cargos del Mes:" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 4.27 COL 28 WIDGET-ID 86
     "Cupo Dispon." VIEW-AS TEXT
          SIZE 12 BY .73 AT ROW 4.5 COL 1 WIDGET-ID 82
     "( + ) Int. Mora :" VIEW-AS TEXT
          SIZE 16.29 BY .81 AT ROW 6.08 COL 28 WIDGET-ID 74
     "( + ) Otros Cargos" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 6.92 COL 28 WIDGET-ID 88
     " Pendiente" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 11.73 COL 42.72 WIDGET-ID 16
          FGCOLOR 7 
     "Capital:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 12.5 COL 2 WIDGET-ID 106
     "Cuota" VIEW-AS TEXT
          SIZE 9 BY .81 AT ROW 6.35 COL 3 WIDGET-ID 104
     "( + ) Int.Corrientes:" VIEW-AS TEXT
          SIZE 16.29 BY .54 AT ROW 5.35 COL 28 WIDGET-ID 4
     "Cupo  Total:" VIEW-AS TEXT
          SIZE 11.72 BY .81 AT ROW 3.38 COL 1 WIDGET-ID 48
          FGCOLOR 0 
     "Fec Inic:" VIEW-AS TEXT
          SIZE 8 BY .81 AT ROW 1 COL 42.72 WIDGET-ID 144
          FGCOLOR 7 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 1 ROW 1.54
         SIZE 64.29 BY 19.92
         BGCOLOR 17  WIDGET-ID 300.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Factura
     "Totales:" VIEW-AS TEXT
          SIZE 8.14 BY .77 AT ROW 17.69 COL 2.72 WIDGET-ID 128
          FGCOLOR 7 
     "Nuevo Saldo" VIEW-AS TEXT
          SIZE 16.43 BY .69 AT ROW 9.77 COL 31 WIDGET-ID 92
          BGCOLOR 15 FGCOLOR 7 
     "Fec.Lim.Pag:" VIEW-AS TEXT
          SIZE 12.86 BY .73 AT ROW 18.69 COL 1.14 WIDGET-ID 150
          FGCOLOR 7 
     RECT-330 AT ROW 12.23 COL 1.43 WIDGET-ID 190
     RECT-331 AT ROW 1 COL 64 WIDGET-ID 194
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 1 ROW 1.54
         SIZE 64.29 BY 19.92
         BGCOLOR 17 
         TITLE "Factura - Estado de Cuenta" WIDGET-ID 300.

DEFINE FRAME F_Plan
     BROWSE-4 AT ROW 1.27 COL 2 WIDGET-ID 700
     Btn_Salir-3 AT ROW 11.5 COL 79.14 WIDGET-ID 2
     w_Total AT ROW 11.65 COL 61 COLON-ALIGNED WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 2.72 ROW 8.42
         SIZE 91.57 BY 12.62
         BGCOLOR 17 FONT 4
         TITLE "Cuotas Pendientes" WIDGET-ID 600.

DEFINE FRAME F_Cre
     Creditos.Seg_Cartera AT ROW 4.77 COL 80 COLON-ALIGNED WIDGET-ID 62
          LABEL "Seguro de Vivienda"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.seg_vida AT ROW 3.88 COL 80 COLON-ALIGNED WIDGET-ID 60
          LABEL "Seguro de Vida"
          VIEW-AS FILL-IN 
          SIZE 13.86 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_DescAnti AT ROW 10.96 COL 43.14 COLON-ALIGNED WIDGET-ID 54
     W_DescTM AT ROW 10.04 COL 43 COLON-ALIGNED WIDGET-ID 52
     Btn-Plan AT ROW 1.27 COL 66 WIDGET-ID 50
     Btn_Factura AT ROW 14.08 COL 65.57 WIDGET-ID 16
     Creditos.Val_Desembolso AT ROW 1.27 COL 15 COLON-ALIGNED
          LABEL "Valor Desembolso" FORMAT ">>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     NomPdadP AT ROW 12.04 COL 28 COLON-ALIGNED NO-LABEL
     Creditos.Monto AT ROW 2.15 COL 15.29 COLON-ALIGNED
          LABEL "Monto Actual" FORMAT "->>>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Categoria AT ROW 12.81 COL 15.29 COLON-ALIGNED
          LABEL "Categoria" FORMAT "X(2)"
          VIEW-AS FILL-IN 
          SIZE 11 BY .69
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cta_Contable AT ROW 19.85 COL 42.72 COLON-ALIGNED
          LABEL "Cta-Contable" FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 18 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Num_Credito AT ROW 1.23 COL 36.86 COLON-ALIGNED
          LABEL "Num_Crédito" FORMAT "999999999"
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Cuota AT ROW 1.27 COL 80 COLON-ALIGNED
          LABEL "Cuota del Crédito"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Fec_Desembolso AT ROW 2.96 COL 15.29 COLON-ALIGNED
          LABEL "Fecha Desembolso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Tasa AT ROW 2.12 COL 80 COLON-ALIGNED
          LABEL "Tasa del Crédito"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_AboCap AT ROW 3.08 COL 42.86 COLON-ALIGNED
     Creditos.Fec_Pago AT ROW 3.77 COL 15.29 COLON-ALIGNED
          LABEL "Fecha Prox.Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Capital AT ROW 3 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_AboInt AT ROW 5.73 COL 42.86 COLON-ALIGNED
     Creditos.Fec_UltPago AT ROW 4.65 COL 15.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Corrientes AT ROW 5.65 COL 80 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_AboIntMor AT ROW 6.58 COL 42.86 COLON-ALIGNED
     W_SdoDeuda AT ROW 5.46 COL 15.29 COLON-ALIGNED
     Creditos.Int_DifCobro AT ROW 6.54 COL 80 COLON-ALIGNED
          LABEL "Interés Contingente"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Plazo AT ROW 8.04 COL 7.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.86 BY 21.77
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Cre
     w_AboIntDifCob AT ROW 7.42 COL 42.86 COLON-ALIGNED
     Creditos.Int_Anticipado AT ROW 7.42 COL 80 COLON-ALIGNED
          LABEL "Interés Anticipado"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Sdo_Proyectado AT ROW 7.08 COL 15.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_MorCobrar AT ROW 8.35 COL 80 COLON-ALIGNED
          LABEL "Mora + Conting.Mora"
          VIEW-AS FILL-IN 
          SIZE 14 BY .81
          BGCOLOR 12 FGCOLOR 15 
     W_AboIntAnt AT ROW 8.31 COL 42.86 COLON-ALIGNED
     Creditos.Cuo_Pagadas AT ROW 8.04 COL 21.57 COLON-ALIGNED
          LABEL "Pagadas" FORMAT "9999"
          VIEW-AS FILL-IN 
          SIZE 4.72 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_CuoPag AT ROW 1.27 COL 59 COLON-ALIGNED
     W_SdoOtros AT ROW 9.23 COL 80 COLON-ALIGNED
     W_AboCostas AT ROW 12.85 COL 43 COLON-ALIGNED
     Creditos.Val_Atraso AT ROW 8.85 COL 15.29 COLON-ALIGNED
          LABEL "Vr.Vencido Capital" FORMAT "->,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY .65
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Dias_Atraso AT ROW 9.5 COL 15.29 COLON-ALIGNED
          LABEL "Dias_Vencidos" FORMAT "-99999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Btn_Validadora AT ROW 19.96 COL 2 HELP
          "Botòn Oculto, para independizar Impresiòn de la Tx."
     W_AboPoliza AT ROW 13.69 COL 43 COLON-ALIGNED
     W_VrEfec AT ROW 16.5 COL 77 COLON-ALIGNED
     W_VrCheq AT ROW 17.62 COL 77 COLON-ALIGNED
     W_ValPag AT ROW 18.73 COL 43 COLON-ALIGNED
     Btn_Salir AT ROW 20.35 COL 82
     Btn_Grabar AT ROW 18.96 COL 82
     Creditos.Cuo_Atraso AT ROW 10.31 COL 15.29 COLON-ALIGNED
          LABEL "Cuotas Vencidas" FORMAT "-9999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .65
          BGCOLOR 12 FGCOLOR 15 
     W_AboHonora AT ROW 14.54 COL 43 COLON-ALIGNED
     Creditos.Provision AT ROW 11.12 COL 15.29 COLON-ALIGNED
          LABEL "Provisión"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NoDistribuido AT ROW 17.88 COL 43 COLON-ALIGNED
     Creditos.Fec_Reestructurado AT ROW 12 COL 15.29 COLON-ALIGNED
          LABEL "Fec-Reestruct."
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-163 AT ROW 20 COL 9
     W_TSdoVdo AT ROW 6.23 COL 15.29 COLON-ALIGNED
     w_NumFactura AT ROW 12 COL 43.29 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     w_PagoRecaudo AT ROW 15.04 COL 62.86 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     w_TapaFUP AT ROW 20.96 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     w_TapaCV AT ROW 20.96 COL 9 NO-LABEL WIDGET-ID 26
     w_sdomoraCR AT ROW 12.23 COL 62 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     w_sobrecupo AT ROW 13.15 COL 62 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     Wsegcart AT ROW 11.15 COL 80 COLON-ALIGNED WIDGET-ID 44
     wtotcomision AT ROW 10.23 COL 80.14 COLON-ALIGNED WIDGET-ID 40
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.86 BY 21.77
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Cre
     W-VlrAboGmfxc AT ROW 16.23 COL 43 COLON-ALIGNED WIDGET-ID 36
     W-VlrAboSegCart AT ROW 17.08 COL 43 COLON-ALIGNED WIDGET-ID 38
     W-VlrAboComis AT ROW 15.38 COL 43 COLON-ALIGNED WIDGET-ID 46
     W_AIA AT ROW 9.15 COL 43 COLON-ALIGNED WIDGET-ID 48
     W_AboSegVida AT ROW 3.96 COL 42.86 COLON-ALIGNED WIDGET-ID 56
     W_AboSegDeudor AT ROW 4.85 COL 42.86 COLON-ALIGNED WIDGET-ID 58
     Fec_corte AT ROW 14.19 COL 12 COLON-ALIGNED WIDGET-ID 2
     "Distribuciòn Preliminar del Abono" VIEW-AS TEXT
          SIZE 28.86 BY .65 AT ROW 2.27 COL 31.72
          FGCOLOR 7 FONT 5
     "Fecha Ultimo Pago:" VIEW-AS TEXT
          SIZE 14 BY .81 AT ROW 4.73 COL 3 WIDGET-ID 18
     RECT-296 AT ROW 1 COL 1.72
     RECT-319 AT ROW 16.04 COL 64.29
     RECT-320 AT ROW 2.58 COL 29.29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.86 BY 21.77
         BGCOLOR 17 FONT 4
         TITLE "".

DEFINE FRAME F_Simul
     TotD AT ROW 2.15 COL 17.86 COLON-ALIGNED
     Val_Vdo AT ROW 3.42 COL 17.72 COLON-ALIGNED
     newCuota AT ROW 4.69 COL 17.86 COLON-ALIGNED WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE 
         AT COL 63.72 ROW 8.65
         SIZE 33.14 BY 5.54
         FONT 4
         TITLE "Nuevos Saldos SI se abona el Pago".


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
         HEIGHT             = 20.96
         WIDTH              = 96.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 180.72
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 180.72
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         TOP-ONLY           = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = 12
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
/* SETTINGS FOR BUTTON Btn_Validadora IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Validadora:HIDDEN IN FRAME F_Cre           = TRUE.

/* SETTINGS FOR FILL-IN Creditos.Categoria IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Cta_Contable IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Cuota IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Cuo_Pagadas IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Dias_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Fec_corte IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Fec_Desembolso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Pago IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_Reestructurado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_Anticipado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Corrientes IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_DifCobro IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_MorCobrar IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Monto IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN NomPdadP IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Num_Credito IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Plazo IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Provision IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Capital IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Sdo_Proyectado IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Seg_Cartera IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.seg_vida IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Tasa IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Val_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Creditos.Val_Desembolso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN W-VlrAboComis IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W-VlrAboGmfxc IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W-VlrAboSegCart IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       W-VlrAboSegCart:READ-ONLY IN FRAME F_Cre        = TRUE.

/* SETTINGS FOR FILL-IN Wsegcart IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN wtotcomision IN FRAME F_Cre
   NO-ENABLE                                                            */
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
/* SETTINGS FOR FILL-IN W_AboSegDeudor IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AboSegVida IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AIA IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DescTM IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NoDistribuido IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w_NumFactura IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       w_NumFactura:READ-ONLY IN FRAME F_Cre        = TRUE.

/* SETTINGS FOR FILL-IN w_PagoRecaudo IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       w_PagoRecaudo:READ-ONLY IN FRAME F_Cre        = TRUE.

/* SETTINGS FOR FILL-IN W_SdoDeuda IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w_sdomoraCR IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       w_sdomoraCR:READ-ONLY IN FRAME F_Cre        = TRUE.

/* SETTINGS FOR FILL-IN W_SdoOtros IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w_sobrecupo IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       w_sobrecupo:READ-ONLY IN FRAME F_Cre        = TRUE.

/* SETTINGS FOR FILL-IN w_TapaCV IN FRAME F_Cre
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       w_TapaCV:READ-ONLY IN FRAME F_Cre        = TRUE.

/* SETTINGS FOR FILL-IN w_TapaFUP IN FRAME F_Cre
   NO-ENABLE                                                            */
ASSIGN 
       w_TapaFUP:READ-ONLY IN FRAME F_Cre        = TRUE.

/* SETTINGS FOR FILL-IN W_TSdoVdo IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Factura
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Factura:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Facturacion.cuota IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       Facturacion.cuota:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN Creditos.Fec_UltPago IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       Creditos.Fec_UltPago:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-31 IN FRAME F_Factura
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-31:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-32 IN FRAME F_Factura
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-32:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN W-OtrosC IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       W-OtrosC:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN W_FcomySobre IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       W_FcomySobre:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN w_Fdisk IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       w_Fdisk:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN w_FintCtes IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       w_FintCtes:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN w_FintMora IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       w_FintMora:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN w_Pagomin IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       w_Pagomin:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN w_PenIntCte IN FRAME F_Factura
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       w_PenIntCte:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN W_PenIntMora IN FRAME F_Factura
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       W_PenIntMora:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN w_PenSegCar IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       w_PenSegCar:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN w_PteCapital IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       w_PteCapital:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN W_PteComySobre IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       W_PteComySobre:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN w_segcar IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       w_segcar:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FILL-IN w_totatraso IN FRAME F_Factura
   NO-ENABLE                                                            */
ASSIGN 
       w_totatraso:READ-ONLY IN FRAME F_Factura        = TRUE.

/* SETTINGS FOR FRAME F_Plan
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-4 1 F_Plan */
ASSIGN 
       FRAME F_Plan:HIDDEN           = TRUE
       FRAME F_Plan:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN w_Total IN FRAME F_Plan
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Simul
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Simul:HIDDEN           = TRUE
       FRAME F_Simul:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN newCuota IN FRAME F_Simul
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TotD IN FRAME F_Simul
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Val_Vdo IN FRAME F_Simul
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}  FOR EACH tcontrol_pagos NO-LOCK
    BY tcontrol_pagos.Nro_Cuota.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Factura
/* Query rebuild information for FRAME F_Factura
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Facturacion SHARE-LOCK,
      EACH Creditos WHERE TRUE /* Join to Facturacion incomplete */ SHARE-LOCK.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* FRAME F_Factura */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Plan
/* Query rebuild information for FRAME F_Plan
     _Query            is NOT OPENED
*/  /* FRAME F_Plan */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Simul
/* Query rebuild information for FRAME F_Simul
     _Query            is NOT OPENED
*/  /* FRAME F_Simul */
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


&Scoped-define SELF-NAME F_Factura
&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define FRAME-NAME F_Plan
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 Wwin
ON MOUSE-SELECT-DBLCLICK OF BROWSE-4 IN FRAME F_Plan
DO:
    w_total = 0.
    W_CuoPag = 0.
    
    FOR EACH btcontrol_pagos:
        IF btcontrol_pagos.Nro_Cuota <= tcontrol_pagos.Nro_Cuota THEN DO:
            btcontrol_pagos.Marca = YES.
            w_total = w_total + btcontrol_pagos.TOTAL_Cuota.
            W_CuoPag = W_CuoPag + 1.
        END.
        ELSE
            btcontrol_pagos.Marca = NO.
    END.

    OPEN QUERY BROWSE-4 FOR EACH tcontrol_pagos WHERE tcontrol_pagos.Agencia = creditos.agencia
                                                  AND tcontrol_pagos.Nit = creditos.nit
                                                  AND tcontrol_pagos.Num_Credito = creditos.num_credito NO-LOCK BY tcontrol_pagos.Nro_Cuota.

    IF w_total MODULO 100 <> 0 THEN
        w_total = TRUNCATE((w_total / 100) + 1,0) * 100.

    DISPLAY w_total WITH FRAME f_plan.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 Wwin
ON ROW-DISPLAY OF BROWSE-4 IN FRAME F_Plan
DO:

IF tcontrol_pagos.Marca THEN 
DO:
    ASSIGN
        tcontrol_pagos.Nro_Cuota:BGCOL IN BROWSE BROWSE-4 = 12
        tcontrol_pagos.Fec_Vcto:BGCOL IN BROWSE BROWSE-4 = 12
        tcontrol_pagos.Cap_pagado:BGCOL IN BROWSE BROWSE-4 = 12
        tcontrol_pagos.TOTAL_Cuota:BGCOL IN BROWSE BROWSE-4 = 12
        tcontrol_pagos.pagos_capitalAcum:BGCOL IN BROWSE BROWSE-4 = 12
        tcontrol_pagos.Pagos_IntAcum:BGCOL IN BROWSE BROWSE-4 = 12
        tControl_pagos.causacion:BGCOL IN BROWSE BROWSE-4 = 12
        tControl_pagos.contingente:BGCOL IN BROWSE BROWSE-4 = 12
        tControl_Pagos.INT_mora:BGCOL IN BROWSE BROWSE-4 = 12
        tcontrol_pagos.Marca:BGCOL IN BROWSE BROWSE-4 = 12
        tcontrol_pagos.Otros:BGCOL IN BROWSE BROWSE-4 = 12.

END.
/*  IF Consulta.Estado EQ 4 THEN DO:
     ASSIGN Consulta.Num_Solicitud:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.AgeSolicitud:BGCOL IN BROWSE Br_Consulta = 12 
            Consulta.Nit:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 12.
     ASSIGN Consulta.Num_Solicitud:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.AgeSolicitud:FGCOL IN BROWSE Br_Consulta = 15 
            Consulta.Nit:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Nombre:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Fec_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Hor_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Monto:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Vigencia:FGCOL IN BROWSE Br_Consulta = 15.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Cheq
&Scoped-define FRAME-NAME F_Cheq
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


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME Btn-Plan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Plan Wwin
ON CHOOSE OF Btn-Plan IN FRAME F_Cre /* ? */
DO:
    VIEW FRAME F_plan.

    ASSIGN FRAME F_Cre:SENSITIVE = FALSE.

    OPEN QUERY BROWSE-4 FOR EACH tcontrol_pagos WHERE tcontrol_pagos.Agencia = creditos.agencia
                                                  AND tcontrol_pagos.Nit = creditos.nit
                                                  AND tcontrol_pagos.Num_Credito = creditos.num_credito NO-LOCK BY tcontrol_pagos.Nro_Cuota.
    DISPLAY w_total WITH FRAME f_plan.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cheq
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
&Scoped-define SELF-NAME Btn_Factura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Factura Wwin
ON CHOOSE OF Btn_Factura IN FRAME F_Cre /* Factura Actual */
DO:
  DEFINE VAR wtotatraso  AS DECIMAL INITIAL 0.00.
/*   DEBUGGER:INITIATE().   */
/*   DEBUGGER:SET-BREAK().  */
  IF NOT AVAILABLE(facturacion) THEN
     RETURN.
  DO WITH FRAME F_Factura:
  ASSIGN 
     w_Fdisk:SCREEN-VALUE                       = STRING(Facturacion.Cuota,"->>>,>>>,>>9.99")
     w_Fdisk
     w_FintMora:SCREEN-VALUE                    = STRING(Facturacion.Int_Mora,"->>>,>>>,>>9.99")
     w_segcar
     Creditos.Fec_ultpago:SCREEN-VALUE          = STRING(Creditos.Fec_UltPago)
     Facturacion.cuota:SCREEN-VALUE             = STRING(Facturacion.cuota,"->>>,>>>,>>9.99").
   /*  Facturacion.GmfxC:SCREEN-VALUE             = STRING(Facturacion.GmfxC,"->>>,>>>,>>9.99") 
     W-Gmfxc:SCREEN-VALUE                       = STRING(Facturacion.GmfxC,"->>>,>>>,>>9.99"). 
     W_VlrAboGmfxC:SCREEN-VALUE                 = STRING(Facturacion.Rec_Gmfxc,"->>>,>>>,>>9.99").
     IF (facturacion.GmfxC - Facturacion.Rec_Gmf) GT 0 THEN
       ASSIGN W_PteGmfxC:SCREEN-VALUE          = STRING(Facturacion.GmfxC - Facturacion.Rec_Gmfxc)
              wtotatraso = (facturacion.GmfxC - Facturacion.Rec_GmfxC).     
     ELSE
       ASSIGN W_PteGmfxC:SCREEN-VALUE        = "0,00". */
      ASSIGN W_totatraso:SCREEN-VALUE = STRING(wtotatraso,"->>>,>>>,>>9.99").

  END.
  ASSIGN FRAME F_Factura:VISIBLE = TRUE.
  VIEW FRAME F_Factura.

  ASSIGN FRAME F_Cre:SENSITIVE = FALSE
         FRAME F_Cre:HIDDEN    = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar Wwin
ON CHOOSE OF Btn_Grabar IN FRAME F_Cre /* Grabar Operación */
DO:
    DEF VAR tValor AS INT.

    OPEN QUERY BROWSE-4 FOR EACH tcontrol_pagos WHERE tcontrol_pagos.Agencia = creditos.agencia
                             AND tcontrol_pagos.Nit = creditos.nit
                             AND tcontrol_pagos.Num_Credito = creditos.num_credito NO-LOCK BY tcontrol_pagos.Nro_Cuota.

    IF W_Valpag LE 0 THEN DO:
        MESSAGE "Falta valor para Distribuir..."
            VIEW-AS ALERT-BOX.

        APPLY "ENTRY" TO W_ValPag.
        RETURN.
    END.

    IF W_ValPag GT (Creditos.Seg_Cartera + Creditos.Com_Bruta + Creditos.Com_Adicional + Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + creditos.seg_Vida +
                    Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob + creditos.int_difCobro + Creditos.Int_Corrientes + Creditos.Sdo_Capital - Creditos.Int_Anticipado) THEN DO:
        MESSAGE "El valor a Pagar no puede superar el Sdo-Total de la deuda..."
            VIEW-AS ALERT-BOX.

        ASSIGN W_ValPag = (Creditos.Seg_Cartera + Creditos.Com_Bruta + Creditos.Com_Adicional + Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + creditos.seg_vida +
                           Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob + creditos.int_difCobro + Creditos.Int_Corrientes + Creditos.Sdo_Capital - Creditos.Int_Anticipado)
               W_ValPag:SCREEN-VALUE = STRING(W_ValPag).

        APPLY "ENTRY" TO W_ValPag.
        RETURN.
    END.

    IF W_Valpag NE (W_VrEfec + W_VrCheq) THEN DO:
        MESSAGE "El valor para Distribuir es Diferente del Efectivo + Vr.Cheque..."
            VIEW-AS ALERT-BOX.
        
        APPLY "ENTRY" TO W_VrEfec.
        RETURN.
    END.

    IF W_VrCheq GT 0 THEN DO:
        FIND FIRST TCheq NO-ERROR.
        IF NOT AVAIL(Tcheq) THEN DO:
            MESSAGE "Debe digitarse cheque, banco y valor. Rectifique!"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO W_VrCheq.
            RETURN.
        END.
    END.

    FIND FIRST Operacion WHERE Operacion.Cod_Operacion EQ Op_Capital NO-LOCK NO-ERROR.
    IF AVAILABLE(Operacion) AND Operacion.Comprobante NE 0 THEN
        W_Cbte = Operacion.Comprobante.
    ELSE DO:
        RUN MostrarMensaje IN W_Manija (INPUT 143,
                                        OUTPUT W_Ok).
        RETURN.
    END.

    IF Creditos.Abogado THEN DO:  /*Junio 13/05 GAER*/
        FIND LAST COBROS WHERE COBROS.NIT = CREDITOS.NIT AND Cobros.Estado = 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE (COBROS) THEN DO:
            MESSAGE "Crèdito en Cobro Jurìdico - Abogado sin acuerdo de Pago..." SKIP
                    "No se acepta Abono."
                VIEW-AS ALERT-BOX.
            RETURN.
        END.
    END.

    /*Marca las cuotas a pagar*/
    FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
        btcontrol_pagos.Marca = FALSE.
    END.

    tValor = W_ValPag.

    FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
        tValor = tvalor - btcontrol_pagos.TOTAL_Cuota.
        btcontrol_pagos.Marca = TRUE.

        IF tvalor < 0 THEN
            LEAVE.
    END.

    MESSAGE "El Valor Total a Abonar es $ " W_ValPag SKIP
            "                            Continue solo si està Segura(0)...?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Grabar Operaciòn" UPDATE W_RptaAbono AS LOGICAL.

    IF NOT W_RptaAbono THEN
        RETURN.

    DO TRANSACTION ON ERROR UNDO:
        FIND FIRST Comprobantes WHERE Comprobantes.Agencia EQ W_Agencia
                                  AND Comprobantes.Comprobante EQ W_Cbte NO-ERROR.
        
        FIND FIRST mov_contable WHERE mov_contable.agencia = comprobantes.agencia
                                  AND mov_contable.comprobante = comprobantes.comprobante
                                  AND mov_contable.num_documento = comprobantes.secuencia NO-LOCK NO-ERROR.
        IF AVAILABLE mov_contable THEN
            Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
        
        ASSIGN W_NumSeq = NEXT-VALUE(Sec_Taquilla)
               W_DocContab = Comprobantes.Secuencia.

        FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

        RUN Transaccion NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Se ha producido un error en la grabación de la transacción" SKIP
                    "consulte con el Administrador del Sistema"
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END. /*fin tx*/

    APPLY "choose" TO Btn_Validadora.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Wwin
ON CHOOSE OF Btn_Salir IN FRAME F_Cre /* Btn_Salir */
DO: 
   APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Wwin
ON MOUSE-SELECT-CLICK OF Btn_Salir IN FRAME F_Cre /* Btn_Salir */
DO:
  APPLY "CHOOSE" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Factura
&Scoped-define SELF-NAME Btn_Salir-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir-2 Wwin
ON CHOOSE OF Btn_Salir-2 IN FRAME F_Factura /* Btn_Salir */
DO: 
  HIDE FRAME F_Factura.
  ASSIGN FRAME F_Cre:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir-2 Wwin
ON MOUSE-SELECT-CLICK OF Btn_Salir-2 IN FRAME F_Factura /* Btn_Salir */
DO:
  APPLY "CHOOSE" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Plan
&Scoped-define SELF-NAME Btn_Salir-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir-3 Wwin
ON CHOOSE OF Btn_Salir-3 IN FRAME F_Plan /* Btn_Salir */
DO: 
  HIDE FRAME F_plan.
  ASSIGN FRAME F_Cre:SENSITIVE = TRUE.
  DISPLAY w_CuoPag WITH FRAME f_Cre.
  APPLY "entry" TO w_CuoPag.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir-3 Wwin
ON MOUSE-SELECT-CLICK OF Btn_Salir-3 IN FRAME F_Plan /* Btn_Salir */
DO:
  APPLY "CHOOSE" TO SELF.
  RETURN NO-APPLY.
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
  /*MESSAGE "Impresora(SI)   Ò    Validadora(NO)" VIEW-AS ALERT-BOX QUESTION BUTTONS 
       YES-NO  TITLE "Medio de Impresiòn" UPDATE RpaVI AS LOGICAL.                   
                                                                                  
  IF RpaVI THEN DO:                                                                  
     RUN F-Taquilla.r (INPUT W_NumSeq, INPUT "").                                    
     MESSAGE "Aliste papel para Segunda Copia ".                                     
     RUN F-Taquilla.r (INPUT W_NumSeq, INPUT "").                                    
  END.                                                                               
  ELSE*/

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

  RUN W-Hist_Creditos.r (INPUT P_NitCli,Creditos.Num_Credito).
  
  ASSIGN WWin:SENSITIVE = YES.
  WWin:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fec_corte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fec_corte Wwin
ON LEAVE OF Fec_corte IN FRAME F_Cre /* Fec_corte */
DO:
  ASSIGN fec_corte.
  ASSIGN W_diapreliquida = fec_corte - TODAY 
         w_preliquida = CREDITOS.sdo_capital * W_diapreliquida * (tasa / 36000).
  RUN Mostrar_Credito.
  /*MESSAGE w_diapreliquida VIEW-AS ALERT-BOX.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CuoPag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CuoPag Wwin
ON ENTRY OF W_CuoPag IN FRAME F_Cre /* Cuotas a Pagar */
DO:
  ASSIGN W_SiEntry = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CuoPag Wwin
ON LEAVE OF W_CuoPag IN FRAME F_Cre /* Cuotas a Pagar */
DO:
    DEFINE VAR W_CuoTras AS INTEGER FORMAT "999".
    DEFINE VAR W_Nrocuopag AS INTEGER FORMAT "999".
    DEFINE VAR W_Kcuota LIKE creditos.cuota.
    DEFINE VAR W_Icuota LIKE creditos.cuota.
    DEFINE VAR W_I AS INTEGER.

    ASSIGN W_CuoPag.

    IF NOT W_SiEntry THEN DO:
        W_SiEntry = FALSE.
        RETURN.
    END.

    W_SiEntry = FALSE.
    w_I = 0.
    w_total = 0.

    w_Comple = TRUE.

    FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
        IF w_i = 0 THEN
            btcontrol_pagos.Otros = creditos.polizas + creditos.honorarios + creditos.costas.

        IF w_i < W_CuoPag THEN DO:
            btcontrol_pagos.Marca = YES.
            w_total = w_total + btcontrol_pagos.pagos_capitalAcum - btcontrol_pagos.Cap_pagado
                              + btcontrol_pagos.Pagos_IntAcum - btcontrol_pagos.Int_pagado
                              + btcontrol_pagos.Dif_TasaM + btcontrol_pagos.Pag_Anticipado
                              + btcontrol_pagos.Otros.
        END.
        ELSE DO:
            btcontrol_pagos.Marca = NO.
            W_Comple = FALSE.
        END.

        w_i = w_i + 1.
    END.

    IF W_Comple = YES AND creditos.cod_credito <> 123 THEN
        w_total = w_sdoDeuda.

    IF creditos.sdo_capital - creditos.sdo_proyectado + creditos.INT_corriente + creditos.INT_morCobrar > w_total THEN
        w_total = creditos.sdo_capital - creditos.sdo_proyectado + creditos.INT_corriente + creditos.INT_morCobrar.

    IF w_total MODULO 100 <> 0 THEN
        w_total = ROUND((w_total / 100) + 1,0) * 100.


    DO WITH FRAME F_Cre:
        IF (Creditos.Plazo - Creditos.Cuo_pagadas) LT W_CuoPag THEN DO:
            MESSAGE "Cuotas a pagar: " W_CuoPag SKIP
                    "Mayor que plazo - Cuotas Pagadas: " Creditos.Plazo Creditos.Cuo_pagadas
                VIEW-AS alert-BOX.

            ASSIGN W_CuoPag = (Creditos.Plazo - Creditos.Cuo_pagadas)
                   W_CuoPag:SCREEN-VALUE = STRING(W_CuoPag).
        END.

        RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                                       INPUT Creditos.Plazo,
                                       OUTPUT W_NroDia,
                                       OUTPUT P_NMeses,
                                       OUTPUT W_NroPer,
                                       OUTPUT P_NomPer).

        W_CuoTras = ((W_Fecha - Creditos.Fec_Desembolso) / W_NroDia) + 1.

        FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito
                            AND Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito NO-LOCK NO-ERROR.

        ASSIGN W_Nrocuopag = Creditos.Cuo_pagadas
               P_VrInteres = 0
               P_VrCapital = 0
               W_ValPag = 0
               W_VrEfec:SCREEN-VALUE = "0"
               W_ValPag:SCREEN-VALUE = "0".

        IF W_CuoPag GT 0 THEN DO:
            /*w_valpag = Creditos.INT_MorCobrar + Creditos.INT_MoraDifCob + W_SdoOtros.*/
            W_ValPag = w_total.


            IF W_cuprot EQ 570 THEN DO:
                /* Nuevo programa - Para segmentar el pago segun factura */
                FIND FIRST per_facturacion WHERE per_facturacion.estado EQ 1 NO-LOCK NO-ERROR.
                IF AVAILABLE(per_facturacion) THEN
                    FIND FIRST facturacion WHERE facturacion.nit EQ creditos.nit
                                             AND facturacion.num_credito EQ creditos.num_credito
                                             AND facturacion.estado EQ 1 NO-ERROR.
                IF AVAILABLE(factura) THEN DO:
                    IF Creditos.Monto LT Creditos.sdo_capital THEN
                        W_ValPag = W_ValPag + (Creditos.monto - Creditos.Sdo_capital).
                    ELSE DO:
                        IF W_CuoPag EQ 1 THEN DO:
                        END.
                        ELSE
                            W_ValPag = (Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Seg_Cartera +
                                        Creditos.Com_Adicional + Creditos.Com_Bruta + Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob +
                                        creditos.int_difCobro + Creditos.Int_Corrientes + Creditos.Sdo_Capital - Creditos.Int_Anticipado).
                    END.
                END.
            END.
        END.

        RUN Muestra_DistPrel.

        OPEN QUERY BROWSE-4 FOR EACH tcontrol_pagos WHERE tcontrol_pagos.Agencia = creditos.agencia
                                                      AND tcontrol_pagos.Nit = creditos.nit
                                                      AND tcontrol_pagos.Num_Credito = creditos.num_credito NO-LOCK BY tcontrol_pagos.Nro_Cuota.

        ASSIGN W_VrEfec:SCREEN-VALUE = STRING(W_ValPag)
               W_ValPag:SCREEN-VALUE = STRING(W_ValPag + W_VrCheq)
               W_ValPag.

        IF P_Progra EQ 9 THEN
            APPLY "Leave" TO W_ValPag.
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
    DEFINE VAR vCuota AS DECIMAL.
    DEFINE VAR diasPeriodo AS INTEGER.
    DEFINE VAR periodos AS INTEGER.

    ASSIGN W_ValPag.

    RUN Muestra_DistPrel.

    IF P_Progra <> 9 THEN DO:
        APPLY "ENTRY" TO W_VrEfec.
        RETURN NO-APPLY.
    END.
    ELSE
        IF W_ValPag > 0 THEN DO:
            ASSIGN FRAME F_Simul:VISIBLE = TRUE
                   Val_Vdo = (Creditos.Honorarios - P_Honora) +
                             (Creditos.Costas - P_Costas) +
                             (Creditos.Polizas - P_Poliza) +
                             (creditos.seg_vida - P_SeguroVida) +
                             (creditos.seg_cartera - P_SeguroDeudor) +
                             (Creditos.Int_MorCobrar - P_IMora) +
                             (Creditos.Int_MoraDifCob - P_IDifCobMor) +
                             (Capital_Acum - (Sdo_CapPag + P_Capital)) +
                             (Int_LiqAcum - (Sdo_IntPag + P_IDifCob + P_ICte)) -
                             (Creditos.Int_Anticipado + P_IAntic)
                   Val_Vdo:SCREEN-VALUE = STRING(Val_Vdo)
                   TotD = (Creditos.Honorarios - P_Honora) +
                          (Creditos.Costas - P_Costas) +
                          (Creditos.Polizas - P_Poliza) +
                          (creditos.seg_vida - P_SeguroVida) +
                          (creditos.seg_cartera - P_SeguroDeudor) +
                          (Creditos.Int_MorCobrar - P_IMora) +
                          (Creditos.Int_MoraDifCob - P_IDifCobMor) +
                          (Creditos.Int_Corrientes - P_ICte) +
                          (Creditos.Int_DifCobro - P_IDifCob) +
                          (Creditos.Sdo_Capital - P_Capital) -
                          (Creditos.Int_Anticipado + P_IAntic)
                   TotD:SCREEN-VALUE = STRING(TotD).

            IF Val_Vdo < 0 THEN
                ASSIGN Val_Vdo = 0
                       Val_Vdo:SCREEN-VALUE = "0".

            vCuota = creditos.cuota.

            FIND FIRST cfg_creditos NO-LOCK NO-ERROR.
            IF AVAILABLE cfg_creditos THEN DO:
                IF W_ValPag >= cfg_creditos.montoMinimoRefinanciacion THEN DO:
                    DEFINE VAR vPlazo AS INTEGER.
                    DEFINE VAR vInteres AS DECIMAL.
                    DEFINE VAR vTasa AS DECIMAL.
                    DEFINE VAR diasPreinicio AS INTEGER.
                    DEFINE VAR interesPreinicio AS DECIMAL.
                    DEFINE VAR vMonto AS DECIMAL.
                    DEFINE VAR numCuotaControlPagos AS INTEGER.
                    
                    vCuota = creditos.cuota.

                    RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                                                   INPUT Creditos.Plazo,
                                                   OUTPUT diasPeriodo,
                                                   OUTPUT P_NMeses,
                                                   OUTPUT Periodos,
                                                   OUTPUT P_NomPer).

                    RUN NVEF IN w_manfin (INPUT creditos.tasa / 100,
                                          INPUT periodos,
                                          OUTPUT vTasa).

                    vTasa = vTasa * 100.
                    vPlazo = creditos.plazo - (creditos.cuo_pagadas + 1 + 1).

                    /* Esto lo hacemos con el fin de distribuir los intereses adicionales a un periodo de forma que aumentando la cuota se puedan amortizar en el transcurso del crédito */
                    diasPreinicio = ADD-INTERVAL(creditos.fec_pago, -1,"months") - w_fecha.
                    interesPreInicio = (creditos.sdo_Capital / 100) * (((creditos.tasa / periodos) / 30) * diasPreinicio).
                    vMonto = creditos.sdo_capital - P_Capital + interesPreinicio.

                    RUN Calculo_Cuota.R (INPUT-OUTPUT vMonto,
                                         INPUT-OUTPUT vPlazo,
                                         INPUT-OUTPUT vCuota,
                                         INPUT-OUTPUT vInteres,
                                         INPUT-OUTPUT vTasa,
                                         INPUT 0,
                                         INPUT 0,
                                         INPUT creditos.per_Pago,
                                         INPUT 3,
                                         INPUT 1,
                                         INPUT Creditos.Sistema).

                    newCuota = vCuota.
                END.
            END.

            newCuota:SCREEN-VALUE = STRING(newCuota, "$>>>,>>>,>>>").


            APPLY "Entry" TO Btn_Salir.
            RETURN NO-APPLY.
        END.
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

  APPLY "Entry" TO W_ValPag.
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
&Scoped-define BROWSE-NAME BROWSE-4
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aplica_Consulta Wwin 
PROCEDURE Aplica_Consulta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar_Partidas Wwin 
PROCEDURE Contabilizar_Partidas :
IF W_VrEfec GT 0 THEN DO:
    CREATE Mov_Contable.          /*La Contrapartida de Caja en esta agencia*/
    ASSIGN Mov_Contable.Agencia = W_Agencia
           Mov_Contable.Comprobante = W_Cbte
           Mov_Contable.Cuenta = W_Caja
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = "Abono a Crèdito"
           Mov_Contable.Usuario = W_Usuario
           Mov_contable.Nit = Creditos.Nit
           Mov_Contable.Cen_Costos = 999
           Mov_Contable.Destino = Creditos.Agencia
           Mov_Contable.Num_Documento = W_DocContab
           Mov_Contable.Doc_Referencia = STRING(Creditos.Num_Credito)
           Mov_contable.Enlace = STRING(W_NumSeq)
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.DB = W_VrEfec NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
END.

IF W_VrCheq GT 0 THEN
    FOR EACH TCheq:
        CREATE Mov_Contable.          /*La Contrapartida de Caja(Cheques) en esta agencia*/
        ASSIGN Mov_Contable.Agencia = W_Agencia
               Mov_Contable.Comprobante = W_Cbte
               Mov_Contable.Cuenta = /*W_Banco*/ W_Caja
               Mov_Contable.Fec_Contable = W_Fecha
               Mov_Contable.Comentario = "Abono a Crèdito"
               Mov_Contable.Usuario = W_Usuario
               Mov_contable.Nit = Creditos.Nit
               Mov_Contable.Cen_Costos = 999
               Mov_Contable.Destino = Creditos.Agencia
               Mov_Contable.Num_Documento = W_DocContab
               Mov_Contable.Doc_Referencia = STRING(TCheq.Nro)
               Mov_contable.Enlace = STRING(W_NumSeq)
               Mov_Contable.Fec_Grabacion = TODAY
               Mov_Contable.Hora = TIME
               Mov_Contable.Estacion = W_Estacion
               Mov_Contable.DB = TCheq.Val NO-ERROR.

        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR.
    END.

IF Creditos.Agencia NE W_Agencia THEN DO:
   CREATE Mov_Contable.          /*La Contrapartida de SyA en esta agencia*/
   ASSIGN Mov_Contable.Agencia        = W_Agencia                            
          Mov_Contable.Comprobante    = W_Cbte                               
          Mov_Contable.Cuenta         = W_CtaSYA_Fte                               
          Mov_Contable.Fec_Contable   = W_Fecha                              
          Mov_Contable.Comentario     = "Abono a Crèdito, Nit:" + STRING(Creditos.Nit)                    
          Mov_Contable.Usuario        = W_Usuario                            
          Mov_contable.Nit            = STRING(Creditos.Agencia,"999")                         
          Mov_Contable.Cen_Costos     = 999                                  
          Mov_Contable.Destino        = Creditos.Agencia
          Mov_Contable.Num_Documento  = W_DocContab                             
          Mov_Contable.Doc_Referencia = STRING(Creditos.Num_Credito)         
          Mov_contable.Enlace         = STRING(W_NumSeq)                     
          Mov_Contable.Fec_Grabacion  = TODAY                                
          Mov_Contable.Hora           = TIME                                 
          Mov_Contable.Estacion       = W_Estacion                           
          Mov_Contable.CR             = W_ValPag NO-ERROR.                   

   IF ERROR-STATUS:ERROR THEN RETURN ERROR.                                  

   CREATE Mov_Contable.          /*La Contrapartida de SyA en la agencia Destino*/
   ASSIGN Mov_Contable.Agencia        = Creditos.Agencia                            
          Mov_Contable.Comprobante    = W_Cbte                               
          Mov_Contable.Cuenta         = W_CtaSYA_Fte                               
          Mov_Contable.Fec_Contable   = W_Fecha                              
          Mov_Contable.Comentario     = "Abono a Crèdito, Nit:" + STRING(Creditos.Nit)                    
          Mov_Contable.Usuario        = W_Usuario                            
          Mov_contable.Nit            = STRING(W_Agencia,"999")                         
          Mov_Contable.Cen_Costos     = 999                                  
          Mov_Contable.Destino        = W_Agencia
          Mov_Contable.Num_Documento  = W_DocContab                             
          Mov_Contable.Doc_Referencia = STRING(Creditos.Num_Credito)         
          Mov_contable.Enlace         = STRING(W_NumSeq)                     
          Mov_Contable.Fec_Grabacion  = TODAY                                
          Mov_Contable.Hora           = TIME                                 
          Mov_Contable.Estacion       = W_Estacion                           
          Mov_Contable.DB             = W_ValPag NO-ERROR.                   

   IF ERROR-STATUS:ERROR THEN RETURN ERROR.                                  
END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ControlLavado Wwin 
PROCEDURE ControlLavado :
DEFINE VAR RT_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR CS_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR AB_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR RT_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR CS_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR AB_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR PCod LIKE BorradorSipla.CodAutoriza.
DEFINE VAR PUsu LIKE W_Usuario.
DEFINE VAR MenSipla  AS CHARACTER FORMAT "X(5)".
DEFINE VAR MenSiplaM AS CHARACTER FORMAT "X(5)".

FIND LAST ControlSipla WHERE
          ControlSipla.Nit          EQ Creditos.Nit AND
          MONTH(ControlSipla.Fecha) EQ MONTH(W_Fecha) NO-ERROR.
IF AVAILABLE ControlSipla THEN DO:
   IF ControlSipla.Fecha EQ W_Fecha THEN
      ASSIGN CS_ValSiplaDia = ControlSipla.CS_TotalDia
             AB_ValSiplaDia = ControlSipla.AB_TotalDia
             RT_ValSiplaDia = ControlSipla.RT_TotalDia.
   ASSIGN CS_ValSiplaMes = ControlSipla.CS_TotalMes
          AB_ValSiplaMes = ControlSipla.AB_TotalMes
          RT_ValSiplaMes = ControlSipla.RT_TotalDia.
END.
ELSE DO:
    CREATE ControlSipla.
    ASSIGN ControlSipla.Nit         = Creditos.Nit
           ControlSipla.Fecha       = W_Fecha.
END.
IF W_VrEfec + W_VrCheq GT 0 THEN DO: /*es consignacion*/
/*    IF CS_ValSiplaDia + AB_ValSiplaDia + W_VrEfec + W_VrCheq GT Entidad.MaxOp_Efectivo_Dia THEN /*giocam Oct/16/07*/ */
   IF CS_ValSiplaDia + AB_ValSiplaDia + W_VrEfec GT Entidad.MaxOp_Efectivo_Dia THEN /*giocam - valida solo si consig. en efectivo se excede */
      ASSIGN MenSipla  = "CSDIA". 
/*    IF CS_ValSiplaMes + AB_ValSiplaMes + W_VrEfec + W_VrCheq  GT Entidad.MaxOp_Efectivo_Mes THEN /*giocam Oct/16/07 */ */
   IF CS_ValSiplaMes + AB_ValSiplaMes + W_VrEfec GT Entidad.MaxOp_Efectivo_Mes THEN /*giocam - valida solo si consig. en efectivo se excede */
      ASSIGN MenSiplaM = "CSMES".
END.
ASSIGN PCod = 0
       CtrLavado = NO.

IF MenSipla NE "" OR MenSiplaM NE "" THEN 
   RUN C-ControlSipla.w (INPUT Creditos.Nit, INPUT MenSipla, INPUT MenSiplaM, OUTPUT PCod).
ELSE DO: 
   CtrLavado = YES.
   /*MESSAGE controlsipla.fecha w_fecha VIEW-AS ALERT-BOX.*/
   IF ControlSipla.Fecha NE W_Fecha THEN DO:
       CREATE ControlSipla.
       ASSIGN ControlSipla.Nit         = Creditos.Nit
              ControlSipla.Fecha       = W_Fecha.
   END.
   ASSIGN ControlSipla.AB_TotalMes = ControlSipla.AB_TotalMes + W_VrEfec + W_VrCheq
          ControlSipla.AB_TotalDia = ControlSipla.AB_TotalDia + W_VrEfec + W_VrCheq.
END.

IF PCod NE 0 THEN DO: /*graba registro del dia de la transaccion*/
   CtrLavado = YES.
   IF ControlSipla.Fecha LT W_Fecha THEN DO:
       FIND ControlSipla       WHERE
            ControlSipla.Nit   EQ Creditos.Nit AND
            ControlSipla.Fecha EQ W_Fecha NO-ERROR.
       IF NOT AVAILABLE ControlSipla THEN DO:
          CREATE ControlSipla.
          ASSIGN ControlSipla.Nit         = Creditos.Nit
                 ControlSipla.Fecha       = W_Fecha
                 ControlSipla.CS_TotalMes = ControlSipla.CS_TotalMes + CS_ValSiplaMes
                 ControlSipla.AB_TotalMes = ControlSipla.AB_TotalMes + AB_ValSiplaMes
                 ControlSipla.RT_TotalMes = ControlSipla.RT_TotalMes + RT_ValSiplaMes.
       END.
   END.
   IF MenSipla  NE "" THEN ControlSipla.Id_NUD = YES.
   IF MenSiplaM NE "" THEN ControlSipla.Id_NUM = YES.
   ASSIGN ControlSipla.AB_TotalMes = ControlSipla.AB_TotalMes + W_Vrefec + W_VrCheq
          ControlSipla.AB_TotalDia = ControlSipla.AB_TotalDia + W_Vrefec + W_VrCheq.
   /*crea la instancia de mov_inssipla para el encargado de la agencia*/

   FIND Instancias WHERE
        Instancias.Tipo_Instancia EQ 6   AND 
        Instancias.Primera        EQ YES AND
        Instancias.Estado         EQ 1   NO-LOCK NO-ERROR.
   IF AVAILABLE Instancias THEN DO:
      FIND FIRST Cfg_Instancias WHERE 
           Cfg_Instancias.Agencia   EQ Creditos.Agencia /*W_Agencia*/ AND /*para que la instancia quede*/
           Cfg_Instancias.Instancia EQ Instancias.Instancia AND          /*en la agencia de la cuenta*/
           Cfg_Instancias.Estado    EQ 1 NO-LOCK NO-ERROR.
      IF AVAILABLE Cfg_Instancias THEN DO:
         FIND LAST BorradorSipla WHERE
              BorradorSipla.Agencia      EQ W_Agencia   AND
              BorradorSipla.Nit          EQ Creditos.Nit AND
              BorradorSipla.Fecha        EQ W_Fecha     AND
              /*BorradorSipla.Id_AhoCre    EQ 2           AND*/
              BorradorSipla.CodAutoriza  EQ PCod NO-ERROR.
         FIND FIRST Mov_InsSipla WHERE
              Mov_InsSipla.Agencia           EQ Creditos.Agencia AND
              Mov_InsSipla.Fecha_Transaccion EQ W_Fecha         AND
              Mov_InsSipla.Instancia         EQ Cfg_Instancias.Instancia AND
              Mov_InsSipla.Nit               EQ Creditos.Nit NO-ERROR.
         IF NOT AVAILABLE Mov_InsSipla THEN DO:
             CREATE Mov_InsSipla.
             ASSIGN Mov_InsSipla.Agencia           = Creditos.Agencia /*W_Agencia*/
                    Mov_InsSipla.Fecha_Transaccion = W_Fecha
                    Mov_InsSipla.Hora_Transaccion  = TIME
                    Mov_InsSipla.Instancia         = Cfg_Instancias.Instancia
                    Mov_InsSipla.Nit               = Creditos.Nit
                    Mov_InsSipla.UsuCajero         = W_Usuario
                    Mov_InsSipla.UsuGestiona       = Cfg_Instancias.Usuario.
                    /*Mov_InsSipla.AgeOrigen         = W_Agencia Ahorros.Agencia*/
         END.
         IF AVAILABLE BorradorSipla THEN DO:
             BorradorSipla.Id_AhoCre = 2.
             ASSIGN Mov_InsSipla.UsuReporta        = BorradorSipla.Usuario
                    Mov_InsSipla.Id_Exonerada      = BorradorSipla.Id_Exonerada
                    Mov_InsSipla.Id_NUD            = BorradorSipla.Id_NUD
                    Mov_InsSipla.Id_NUM            = BorradorSipla.Id_NUM
                    Mov_InsSipla.Id_RepUiaf        = BorradorSipla.Id_Repfiscalia
                    Mov_InsSipla.Id_Sospechosa     = BorradorSipla.Id_Sospechosa
                    Mov_InsSipla.CodAutoriza       = PCod.
             IF Creditos.Agencia NE W_Agencia THEN 
                Mov_InsSipla.Descripcion       = Mov_InsSipla.Descripcion + 
                                                 " Ag:" + STRING(W_Agencia,"99") + " - " +
                                                 BorradorSipla.Descripcion.
             ELSE
                 Mov_InsSipla.Descripcion       = Mov_InsSipla.Descripcion + " . " +
                                                  BorradorSipla.Descripcion.
         END.
         ELSE DO:
            MESSAGE "No se encontrado el borrador sipla que permita" SKIP
                    "identificar quien manejo al cliente en la agencia"
                    VIEW-AS ALERT-BOX.
            CtrLavado = NO.
            RETURN ERROR.
         END.
         IF MenSipla  NE "" THEN Mov_InsSipla.Id_NUD = YES.
         IF MenSiplaM NE "" THEN Mov_InsSipla.Id_NUM = YES.
      END.
      ELSE DO:
         MESSAGE "No se ha encontrado un usuario para asignarle" SKIP
                 "la primera instancia del control de lavado de" SKIP
                 "activos.  No  se podra realizar la operacion," SKIP
                 "hasta que no se asigne la instancia a un usuario" VIEW-AS ALERT-BOX.
         CtrLavado = NO.
      END.
   END.
   ELSE DO:
       MESSAGE "No se ha encontrado la primera instancia" SKIP
               "del control de lavado de activos." SKIP
               "la transaccion no se podra realizar!!!" SKIP(1)
               "comuniquese con el depto de sistemas" VIEW-AS ALERT-BOX ERROR.
       CtrLavado = NO.
   END.
END. 
RELEASE ControlSipla.
RELEASE Mov_InsSipla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ControlPagos Wwin 
PROCEDURE ControlPagos :
DEF VAR w_iTasaM LIKE Creditos.Sdo_Capital.
DEF VAR w_TasaNA AS DEC FORMAT "->>>>9.9999999".
DEF VAR w_TasaEA AS DEC FORMAT "->>>>9.9999999".
DEF VAR diasAnticipa AS INT.
DEFINE VAR diasVencidos AS INTEGER.
DEFINE VAR intVencido AS DECIMAL.
DEFINE VAR cont AS INTEGER.

/*Crea el temporal de control pagos*/
IF creditos.cod_credito <> 123 THEN DO:
    FOR EACH CONTROL_pagos WHERE control_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND control_pagos.Id_PdoMes < 2 NO-LOCK BREAK BY CONTROL_pagos.num_credito:
        CREATE tCONTROL_pagos.
        BUFFER-COPY CONTROL_pagos TO tcontrol_pagos.

        IF FIRST-OF(CONTROL_pagos.num_credito) THEN
            tControl_pagos.INT_mora = creditos.INT_morCobrar.
        ELSE
            tControl_pagos.INT_mora = 0.
    END.
END.
ELSE DO:
    FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                           AND facturacion.num_credito = creditos.num_credito
                           AND facturacion.estado = 1 NO-LOCK BY facturacion.fec_pago:
        cont = cont + 1.

        CREATE tControl_pagos.
        ASSIGN tcontrol_pagos.agencia = creditos.agencia
               tControl_pagos.nit = creditos.nit
               tControl_pagos.num_credito = creditos.num_credito
               tControl_pagos.nro_cuota = cont
               tControl_pagos.fec_Vcto = facturacion.fec_pago
               tControl_pagos.cap_pagado = facturacion.pago_capital
               tControl_pagos.INT_pagado = facturacion.pago_IntCorriente + facturacion.pago_intDifCobro
               tControl_pagos.TOTAL_cuota = facturacion.cuota - facturacion.pago_capital - facturacion.pago_intCorriente - facturacion.pago_intDifCobro - facturacion.pago_mora
               tControl_pagos.pagos_capitalAcum = facturacion.capital
               tControl_pagos.pagos_intAcum = facturacion.INT_corriente + facturacion.INT_difCobro
               tControl_pagos.INT_mora = facturacion.INT_mora - facturacion.pago_mora.
    END.
END.

FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.

FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
    btcontrol_pagos.Otros = 0.
END.

FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
    /*Se acumula las moras y otros valores en la primera cuota*/
    btcontrol_pagos.Otros = creditos.honorarios + creditos.costas + creditos.polizas.
    LEAVE.
END.

/*Revisamos el interes maximo*/
W_SaldoK = creditos.monto.

FOR EACH control_pagos WHERE control_pagos.Nit = creditos.nit
                         AND control_pagos.Num_Credito = creditos.num_credito
                         AND control_pagos.Nro_Cuota > 0:
    FIND FIRST btcontrol_pagos WHERE btcontrol_pagos.Nro_Cuota = control_pagos.Nro_Cuota NO-LOCK NO-ERROR.
    IF NOT AVAILABLE btcontrol_pagos THEN
        NEXT.

    /* Revisamos si al crédito se le puede o no cobrar la mora */
    IF control_pagos.int_mora > 0 AND creditos.for_pago = 2 THEN DO:
        FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
        FIND FIRST empresas WHERE empresas.cod_empresa = clientes.cod_empresa NO-LOCK NO-ERROR.
        IF creditos.dias_atraso <= empresas.dias_gracia THEN
            btCONTROL_pagos.int_mora = 0.
    END.
END.

IF creditos.cod_credito <> 123 THEN DO:
    FOR EACH tcontrol_pagos:
        /*tcontrol_pagos.TOTAL_Cuota = tcontrol_pagos.pagos_capitalAcum - tControl_pagos.cap_pagado + /* Capital */
                                     tcontrol_pagos.pagos_IntAcum - tControl_pagos.INT_pagado + /* Interés */
                                     tControl_pagos.INT_mora + /* Mora */
                                     tcontrol_pagos.SeguroDeudor + tcontrol_pagos.SeguroVida. /* Seguros */*/
        tControl_pagos.TOTAL_cuota = credito.cuota + tControl_pagos.INT_mora - tControl_pagos.cap_pagado.

        FIND FIRST extras WHERE extras.agencia = creditos.agencia
                            AND extras.nit = creditos.nit
                            AND extras.cod_credito = creditos.cod_credito
                            AND extras.num_solicitud = creditos.num_solicitud
                            AND extras.nro_cuota = tcontrol_pagos.nro_cuota NO-LOCK NO-ERROR.
        IF AVAILABLE extras THEN
            tControl_pagos.TOTAL_cuota = tControl_pagos.TOTAL_cuota + extras.vr_cuoExtra.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ControlPagos_bck Wwin 
PROCEDURE ControlPagos_bck :
DEF VAR w_iTasaM LIKE Creditos.Sdo_Capital.
DEF VAR w_TasaNA AS DEC FORMAT "->>>>9.9999999".
DEF VAR w_TasaEA AS DEC FORMAT "->>>>9.9999999".
DEF VAR diasAnticipa AS INT.
DEFINE VAR diasVencidos AS INTEGER.
DEFINE VAR intVencido AS DECIMAL.

/*Crea el temporal de control pagos*/
FOR EACH CONTROL_pagos WHERE control_pagos.nit = creditos.nit
                         AND CONTROL_pagos.num_credito = creditos.num_credito
                         AND control_pagos.Id_PdoMes < 2:
    CREATE tCONTROL_pagos.
    BUFFER-COPY CONTROL_pagos TO tcontrol_pagos.
END.

/* Hallar las variables para el manejo del periodo del crédito */
RUN HallarPeriodo IN W_ManFin (INPUT Creditos.Per_Pago,
                               INPUT Creditos.Plazo,
                               OUTPUT W_NroDia,
                               OUTPUT P_NMeses,
                               OUTPUT W_NroPer,
                               OUTPUT P_NomPer).


FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.

/*Busca indicador de tasa maxima.  Cargo en w_tasaNominal la tasa de usura */
FIND FIRST indicadores WHERE Indicadores.Indicador = Pro_Creditos.Cod_TasaMax NO-LOCK NO-ERROR.
IF AVAILABLE indicadores THEN DO:
    /*La tasa esta en efectivo anual toca pasarla a nominal vencida dependiendo del periodo*/
    RUN EFNV IN W_ManFin (Indicadores.Tasa / 100 ,W_NroPer,OUTPUT w_TasaNominal).
END.
ELSE
    w_TasaNominal = (creditos.tasa / 100) / w_nroPer.

IF w_tasaNominal > (creditos.tasa / 100) / w_nroPer THEN
    w_tasaNominal = (creditos.tasa / 100) / w_nroPer.

FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
    btcontrol_pagos.Otros = 0.
END.

FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
    /*Se acumula las moras y otros valores en la primera cuota*/
    btcontrol_pagos.Otros = creditos.honorarios + creditos.costas + creditos.polizas.
    LEAVE.
END.

/*Revisamos el interes maximo*/
W_SaldoK = creditos.monto.

FOR EACH control_pagos WHERE control_pagos.Nit = creditos.nit
                         AND control_pagos.Num_Credito = creditos.num_credito
                         AND control_pagos.Nro_Cuota > 0:
    w_saldok = w_saldok - control_pagos.pagos_capitalAcum. /* Descuento la cuota de capital para trabajar con el saldo proyectado */

    FIND FIRST btcontrol_pagos WHERE btcontrol_pagos.Nro_Cuota = control_pagos.Nro_Cuota NO-LOCK NO-ERROR.
    IF NOT AVAILABLE btcontrol_pagos THEN
        NEXT.

    /*Calculamos el valor a descontar por tasa maxima*/
    w_iTasaM = ROUND((w_SaldoK + control_pagos.pagos_capitalAcum) * w_TasaNominal,0). /* Interés con la tasa máxima */

    IF w_iTasaM < control_pagos.Pagos_IntAcum AND (control_pagos.Int_pagado + control_pagos.Cap_pagado) = 0 THEN DO: /* Si este interés es menor que el teórico entonces trabajo con este */
        RUN NVEF IN W_ManFin (w_TasaNominal * W_NroPer,W_NroPer,OUTPUT w_TasaEA).
        RUN EFNA1 IN W_ManFin (w_TasaEA,W_NroPer,OUTPUT w_TasaNA).
        btcontrol_pagos.Dif_TasaM = w_iTasaM - control_pagos.Pagos_IntAcum.
    END.
    ELSE DO:
        RUN NVEF IN W_ManFin (w_tasaNominal * W_NroPer,W_NroPer,OUTPUT w_TasaEA).
        RUN EFNA1 IN W_ManFin (w_TasaEA,W_NroPer,OUTPUT w_TasaNA).
        btcontrol_pagos.Dif_TasaM = 0.
    END.

    /* 2. Para una cuota al día - periodo vigente */
    IF control_pagos.fec_Vcto >= w_fecha AND CONTROL_pagos.fec_Vcto - W_NroDia <= w_fecha THEN DO:
        diasAnticipa = control_pagos.Fec_Vcto - w_fecha.

        IF diasAnticipa > 0 THEN DO:
            IF DAY(CONTROL_pagos.fec_Vcto) = 31 THEN
                diasAnticipa = diasAnticipa - 1.

            IF MONTH(CONTROL_pagos.fec_Vcto) = 2 THEN DO:
                IF DAY(CONTROL_pagos.fec_Vcto) = 28 THEN
                    diasAnticipa = diasAnticipa + 2.

                IF DAY(CONTROL_pagos.fec_Vcto) = 29 THEN
                    diasAnticipa = diasAnticipa + 1.
            END.
        END.

        IF diasAnticipa > W_NroDia THEN
            diasAnticipa = W_nroDia.

        btControl_pagos.intAnticipado = ROUND(diasAnticipa * ((w_saldoK * w_TasaNA / W_NroPer) / W_NroDia),0).
        
        /* Ajusto por los anticipados que pudieren existir */
        IF CONTROL_pagos.INT_pagado >= control_pagos.causacion THEN
            btControl_pagos.intAnticipado = 0.
        ELSE
            btControl_pagos.intAnticipado = btControl_pagos.intAnticipado - CONTROL_pagos.INT_pagado.

        IF btControl_pagos.intAnticipado > 0 THEN
            btcontrol_pagos.Pag_Anticipado = ((CONTROL_pagos.pagos_IntAcum + btcontrol_pagos.Dif_TasaM) - (btControl_pagos.intAnticipado + CONTROL_pagos.causacion)) * -1.

        IF CONTROL_pagos.INT_pagado >= CONTROL_pagos.pagos_intAcum + btcontrol_pagos.pag_Anticipado + btcontrol_pagos.Dif_TasaM THEN
             btcontrol_pagos.Pag_Anticipado = 0.
    END.

    /* 3. Para cuotas completas adelantadas */
    IF control_pagos.fec_Vcto > w_fecha AND CONTROL_pagos.fec_Vcto - W_NroDia >= w_fecha THEN DO:
        diasAnticipa = W_NroDia.

        btControl_pagos.intAnticipado = ROUND(diasAnticipa * ((w_saldoK * w_TasaNA / W_NroPer) / W_NroDia),0).

        /* Ajusto por los anticipados que pudieren existir */
        IF CONTROL_pagos.INT_pagado >=  btControl_pagos.intAnticipado THEN
            btControl_pagos.intAnticipado = 0.
        ELSE
            btControl_pagos.intAnticipado = btControl_pagos.intAnticipado - CONTROL_pagos.INT_pagado.

        btcontrol_pagos.Pag_Anticipado = ((CONTROL_pagos.pagos_IntAcum + btcontrol_pagos.Dif_TasaM) - (btControl_pagos.intAnticipado + CONTROL_pagos.causacion)) * -1.

        IF CONTROL_pagos.INT_pagado >= CONTROL_pagos.pagos_intAcum + btcontrol_pagos.Pag_Anticipado + btcontrol_pagos.Dif_TasaM THEN
            btcontrol_pagos.Pag_Anticipado = 0.
    END.

    /* Revisamos si al crédito se le puede o no cobrar la mora */
    IF control_pagos.int_mora > 0 AND creditos.for_pago = 2 THEN DO:
        FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.
        FIND FIRST empresas WHERE empresas.cod_empresa = clientes.cod_empresa NO-LOCK NO-ERROR.
        IF creditos.dias_atraso <= empresas.dias_gracia THEN
            btCONTROL_pagos.int_mora = 0.
    END.
END.

FOR EACH tcontrol_pagos:
    tcontrol_pagos.TOTAL_Cuota = tcontrol_pagos.pagos_capitalAcum - tControl_pagos.cap_pagado +
                                 tcontrol_pagos.causacion + tcontrol_pagos.contingente +
                                 tcontrol_pagos.SeguroDeudor + tcontrol_pagos.SeguroVida +
                                 tControl_pagos.intAnticipado - tControl_pagos.INT_pagado +
                                 tControl_pagos.INT_mora.
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
    ASSIGN W_CtaSyA_Des  = CortoLargo.Cta_SYA.
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
    ASSIGN W_CtaDifCob_Db = ""
           W_CtaDifCob_Db = "".
    FIND Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2 AND
                         Liqui_Int.Cod_Producto   EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
    IF AVAILABLE Liqui_Int THEN DO:
       IF Creditos.FOR_Pago EQ 2 THEN /*pago por nomina*/
          ASSIGN W_CtaDifCob_Db = Liqui_Int.CtaDb_DifCobAso.
       ELSE
          ASSIGN W_CtaDifCob_Db = Liqui_Int.CtaDb_DifCob.
    END.
    ELSE DO:
        MESSAGE "Falta Configurar Liqui_Int con cuentas de interes " SKIP
               "para el producto." SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.
    
    IF W_CtaDifCob_Db EQ "" THEN DO:
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
  DISPLAY W_DescAnti W_DescTM NomPdadP W_AboCap W_AboInt W_AboIntMor W_SdoDeuda 
          w_AboIntDifCob W_AboIntAnt W_CuoPag W_SdoOtros W_AboCostas W_AboPoliza 
          W_VrEfec W_VrCheq W_ValPag W_AboHonora W_NoDistribuido W_TSdoVdo 
          w_NumFactura w_PagoRecaudo w_TapaFUP w_TapaCV w_sdomoraCR w_sobrecupo 
          Wsegcart wtotcomision W-VlrAboGmfxc W-VlrAboSegCart W-VlrAboComis 
          W_AIA W_AboSegVida W_AboSegDeudor Fec_corte 
      WITH FRAME F_Cre IN WINDOW Wwin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Seg_Cartera Creditos.seg_vida Creditos.Val_Desembolso 
          Creditos.Monto Creditos.Categoria Creditos.Cta_Contable 
          Creditos.Num_Credito Creditos.Cuota Creditos.Fec_Desembolso 
          Creditos.Tasa Creditos.Fec_Pago Creditos.Sdo_Capital 
          Creditos.Fec_UltPago Creditos.Int_Corrientes Creditos.Int_DifCobro 
          Creditos.Plazo Creditos.Int_Anticipado Creditos.Sdo_Proyectado 
          Creditos.Int_MorCobrar Creditos.Cuo_Pagadas Creditos.Val_Atraso 
          Creditos.Dias_Atraso Creditos.Cuo_Atraso Creditos.Provision 
          Creditos.Fec_Reestructurado 
      WITH FRAME F_Cre IN WINDOW Wwin.
  ENABLE W_DescAnti Btn-Plan Btn_Factura W_CuoPag W_VrEfec W_VrCheq W_ValPag 
         Btn_Salir Btn_Grabar BUTTON-163 RECT-296 RECT-319 RECT-320 
      WITH FRAME F_Cre IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cre}
  DISPLAY W-OtrosC FILL-IN-31 w_Fdisk w_PteCapital w_FintCtes w_PenIntCte 
          w_FintMora W_PenIntMora W_FcomySobre W_PteComySobre w_segcar 
          w_PenSegCar w_Pagomin w_totatraso FILL-IN-32 
      WITH FRAME F_Factura IN WINDOW Wwin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Fec_UltPago 
      WITH FRAME F_Factura IN WINDOW Wwin.
  IF AVAILABLE Facturacion THEN 
    DISPLAY Facturacion.cuota 
      WITH FRAME F_Factura IN WINDOW Wwin.
  ENABLE RECT-330 RECT-331 Btn_Salir-2 
      WITH FRAME F_Factura IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Factura}
  DISPLAY W_VrCCheq W_VrTotCh 
      WITH FRAME F_Cheq IN WINDOW Wwin.
  ENABLE W_NumCBan W_NumCChe W_VrCCheq Br_Cheq Btn_Eliminar Btn_SalirCheq 
      WITH FRAME F_Cheq IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cheq}
  DISPLAY w_Total 
      WITH FRAME F_Plan IN WINDOW Wwin.
  ENABLE BROWSE-4 Btn_Salir-3 
      WITH FRAME F_Plan IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Plan}
  DISPLAY TotD Val_Vdo newCuota 
      WITH FRAME F_Simul IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Simul}
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
   
   CREATE Che_Transito.                                     
   ASSIGN Che_Transito.Cod_Compensa     = C_Banco              
          Che_Transito.Cheque           = C_Cheque             
          Che_Transito.Cod_Producto     = C_CodPto             
          Che_Transito.Estado           = C_Estado             
          Che_Transito.Fec_Canje        = W_Fecha                
          Che_Transito.Fec_Confirmacion = ?                
          Che_Transito.Int_Generado     = 0                    
          Che_Transito.Num_Cuenta       = C_Cuenta             
          Che_Transito.Agencia          = C_Agencia            
          Che_Transito.Ofi_Destino      = W_Agencia            
          Che_Transito.Tip_Producto     = C_TipPto             
          Che_Transito.Valor            = C_Valor              
          Che_Transito.Tip_Remesa       = C_Canje NO-ERROR. 
             
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
DEFINE INPUT PARAMETER T_Autorizo  LIKE Taquilla.Autorizo.
DEFINE INPUT PARAMETER T_Banco LIKE Taquilla.Cod_Compensa.
DEFINE INPUT PARAMETER T_CodOper LIKE Taquilla.Cod_Operacion.
DEFINE INPUT PARAMETER T_CodPto LIKE Taquilla.Cod_Producto.
DEFINE INPUT PARAMETER T_Cuenta LIKE Taquilla.Cuenta.
DEFINE INPUT PARAMETER T_CtraCta LIKE Taquilla.Cuenta.
DEFINE INPUT PARAMETER T_Nat LIKE Taquilla.Naturaleza.
DEFINE INPUT PARAMETER T_Nit LIKE Taquilla.Nit.
DEFINE INPUT PARAMETER T_Nrocuenta LIKE Taquilla.Nro_cuenta.
DEFINE INPUT PARAMETER T_NumDto LIKE Taquilla.Num_Documento.
DEFINE INPUT PARAMETER T_NumRetche LIKE Taquilla.Num_Retcheque.
DEFINE INPUT PARAMETER T_Agencia LIKE Taquilla.Agencia.
DEFINE INPUT PARAMETER T_OfiDes LIKE Taquilla.Age_Destino.
DEFINE INPUT PARAMETER T_OfiFue LIKE Taquilla.Age_Fuente.
DEFINE INPUT PARAMETER T_TipPto LIKE Taquilla.Tip_Producto.
DEFINE INPUT PARAMETER T_Usuario LIKE Taquilla.Usuario.
DEFINE INPUT PARAMETER T_ValChe LIKE Taquilla.Val_Cheque.
DEFINE INPUT PARAMETER T_ValEfec LIKE Taquilla.Val_Efectivo.
DEFINE INPUT PARAMETER T_Segmento LIKE Clientes.Cod_Segmento.
DEFINE INPUT PARAMETER T_Comenta AS CHARACTER FORMAT "X(25)".

CREATE Taquilla.
ASSIGN Taquilla.Autorizo = T_Autorizo
       Taquilla.Nro_Transaccion = W_NumSeq
       Taquilla.Cod_Compensa = T_Banco
       Taquilla.Cod_Operacion = T_CodOper
       Taquilla.Cod_Producto = T_CodPto
       Taquilla.Contabilizar = TRUE
       Taquilla.Cuenta = T_Cuenta
       Taquilla.Cta_Contra = T_CtraCta
       Taquilla.Duracion = 0
       Taquilla.Est_Linea = 0
       Taquilla.Fec_Transaccion = W_Fecha
       Taquilla.Hora_Transaccion = TIME
       Taquilla.Naturaleza = T_Nat
       Taquilla.Nit = T_Nit
       Taquilla.Nro_cuenta = T_Nrocuenta
       Taquilla.Num_Documento = T_NumDto
       Taquilla.Num_Retcheque = T_NumRetche
       Taquilla.Agencia = T_Agencia
       Taquilla.Age_Destino = T_OfiDes
       Taquilla.Age_Fuente = T_OfiFue
       Taquilla.Tip_Producto = 2
       Taquilla.Usuario = T_Usuario
       Taquilla.Val_Cheque = T_ValChe
       Taquilla.Val_Efectivo = T_ValEfec
       Taquilla.Estacion = W_Estacion
       Taquilla.Cod_Segmento = T_Segmento
       Taquilla.Descripcion = T_Comenta NO-ERROR.
       
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
 DEFINE VAR W_TVrCheque        AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99" INIT 0.
 DEFINE VAR W_TVrEfec          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>9.99" INIT 0.
 DEFINE VAR W_DescOpe          AS   CHARACTER FORMAT "X(40)".
 DEFINE VAR W_VrOpera          AS   DECIMAL   FORMAT ">>,>>>,>>>,>>>.99" INIT 0.
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


FOR EACH Taquilla WHERE Taquilla.Nro_Transaccion EQ P_Transa
                    AND Taquilla.Fec_Transaccion EQ W_Fecha NO-LOCK
                  BREAK BY Taquilla.Cod_Producto:
      IF  Taquilla.Naturaleza            EQ "Cr" 
      AND SUBSTRING(Taquilla.Cuenta,1,4) NE "2705" 
      AND SUBSTRING(Taquilla.Cuenta,1,4) NE "1904"   THEN
         ASSIGN W_TVrEfec   = W_TVrEfec   + Taquilla.Val_Efectivo
                W_TVrCheque = W_TVrCheque + Taquilla.Val_Cheque.
      
      IF LAST-OF(Taquilla.Cod_Producto) THEN DO:
         ASSIGN W_DescOpe = "Operación no Existe"
                W_NomPcto = ""
                W_NomCli  = "".

         FIND Operacion WHERE Operacion.Cod_Operacion EQ Taquilla.Cod_Operacion NO-LOCK NO-ERROR.
         IF AVAILABLE(Operacion) THEN
            W_DescOpe = Operacion.Nom_Operacion.

         FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Taquilla.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Creditos) THEN 
            W_NomPcto = TRIM(Pro_Creditos.Nom_Producto).

         FIND FIRST Clientes WHERE Clientes.nit EQ Taquilla.Nit NO-LOCK NO-ERROR.
         IF AVAILABLE Clientes THEN 
            W_Nomcli = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
     
         W_Comentario = CAPS(SUBSTRING(TRIM(W_DescOpe),1,16) + " " + TRIM(W_NomPcto)).
         IF Taquilla.Descripcion NE "" THEN
            W_Comentario = Taquilla.Descripcion.

         RUN Imp_Valida.R (INPUT 0,STRING(Taquilla.Age_Destino), Taquilla.Tip_Producto,1,Taquilla.Nit,
                                 T_SdoI,T_SdoF,T_AboK,T_AboI,
                                 Taquilla.Nro_Cuenta,
                                 W_Comentario, W_Nomcli, W_NomPcto,
                                 W_DocContab, W_TVrEfec,W_TVrCheque," ", P_OtrosC).

         ASSIGN W_TVrEfec   = 0
                W_TVrCheque = 0.
      END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Wwin 
PROCEDURE initializeObject :
/* Feb.17/05 GAER.
     Se agregó condicionamiento para simular recaudos.
     (P_Progra EQ 9)
  ---------------------------------------------------------------------*/
  FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  RUN SUPER.

  FIND Clientes WHERE Clientes.Nit EQ P_NitCli NO-LOCK NO-ERROR.

  ASSIGN Wwin:TITLE = P_NomIns + ", Recaudos para Crèditos, Prog.W-ProRec_Ordinario.W.".
         FRAME F_Cre:TITLE = "Ced./Nit Asociado : " + P_NitCli + "   " +
                              TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " +
                              TRIM(Clientes.Apellido2).

  IF P_Progra EQ 9 THEN DO:
     ASSIGN Wwin:TITLE = "Simulación Recaudo de Crédito, Prog.W-ProRec_Ordinario.W."
            BUTTON-163:VISIBLE IN FRAME F_Cre = FALSE
            W_VrEfec:VISIBLE   = FALSE
            W_VrCheq:VISIBLE   = FALSE
            Btn_Grabar:VISIBLE = FALSE.

     RUN Mostrar_Simulacion.
  END.
  ELSE RUN Proc_Recaudo.
  
  RUN controlpagos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Credito Wwin 
PROCEDURE Mostrar_Credito :
DO WITH FRAME F_Cre:
    IF fec_corte = ? THEN fec_corte = TODAY.
    ASSIGN W_diapreliquida = fec_corte - TODAY 
         w_preliquida = CREDITOS.sdo_capital * W_diapreliquida * (tasa / 36000).
    ASSIGN Creditos.Fec_Desembolso:SCREEN-VALUE = STRING(Creditos.Fec_Desembolso)
           Creditos.Monto:SCREEN-VALUE = STRING(Creditos.Monto)
           Creditos.Fec_Pago:SCREEN-VALUE = STRING(Creditos.Fec_Pago)
           Creditos.Fec_Ultpago:SCREEN-VALUE = STRING(Creditos.Fec_Ultpago)
           W_SdoDeuda = Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob + creditos.int_difCobro +
                        Creditos.Int_Corrientes + Creditos.Sdo_Capital - Creditos.Int_Anticipado + w_preliquida
           W_SdoDeuda:SCREEN-VALUE = STRING(W_SdoDeuda)
           Creditos.INT_DifCobro:SCREEN-VALUE = STRING(Creditos.INT_DifCobro)
           Creditos.Sdo_Proyectado:SCREEN-VALUE = STRING(Creditos.Sdo_Proyectado)
           Creditos.Cuo_Pagadas:SCREEN-VALUE = STRING(Creditos.Cuo_Pagadas)
           Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Val_Atraso)
           Creditos.Dias_Atraso:SCREEN-VALUE = STRING(Creditos.Dias_Atraso)
           Creditos.Cuo_Atraso:SCREEN-VALUE = STRING(Creditos.Cuo_Atraso)
           Creditos.Val_Desembolso:SCREEN-VALUE = STRING(Creditos.Val_Desembolso)
           wtotcomision:SCREEN-VALUE = STRING(Creditos.com_Bruta + Creditos.Com_Adicional) 
           WsegCart:SCREEN-VALUE = STRING(Creditos.Seg_Cartera)
           W_TSdoVdo = Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob +
                       creditos.int_difCobro + Creditos.Int_Corrientes - Creditos.Int_Anticipado
           NomPdadP:SCREEN-VALUE = "Plazo en Meses"
           creditos.seg_vida:SCREEN-VALUE = STRING(creditos.seg_vida)
           creditos.seg_cartera:SCREEN-VALUE = STRING(creditos.seg_cartera).

    IF cod_credito = 570 OR cod_credito = 870 THEN
        W_cuprot = 570.

    IF W_cuprot = 570 THEN DO:
        ASSIGN W_SdoDeuda = Creditos.Com_Bruta + Creditos.Com_Adicional + Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar +
                            Creditos.Int_MoraDifCob + creditos.int_difCobro + Creditos.Int_Corrientes + Creditos.Sdo_Capital - Creditos.Int_Anticipado
               W_SdoDeuda:SCREEN-VALUE = STRING(W_SdoDeuda)
               W_TSdoVdo = 0
               W_TSdoVdo:SCREEN-VALUE = STRING(W_TSdoVdo).

        /* Nuevo programa - Para segmentar el pago segun factura */
        FIND FIRST per_facturacion WHERE per_facturacion.estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE(per_facturacion) THEN DO:
            FIND FIRST facturacion WHERE facturacion.nit = creditos.nit
                                     AND facturacion.num_credito = creditos.num_credito
                                     AND facturacion.estado = 1 NO-ERROR.
            IF AVAILABLE(facturacion) THEN DO:
                ENABLE Btn_Factura WITH FRAME F-Cre.
                ASSIGN w_NumFactura:VISIBLE = TRUE
                       w_PagoRecaudo:VISIBLE = TRUE
                       w_tapaFUP:VISIBLE = TRUE
                       w_sdoMoraCR:VISIBLE = TRUE
                       w_sobrecupo:VISIBLE = TRUE
                       W_tapaCV:VISIBLE = TRUE
                       w_NumFactura:SCREEN-VALUE = "Nro. Factura: ".

                IF W_TSdoVdo LT 0 THEN
                    ASSIGN W_TSdoVdo = 0
                           W_TSdoVdo:SCREEN-VALUE = STRING(W_TSdoVdo).

                IF creditos.Monto GE Creditos.sdo_capital THEN
                    w_tapaFUP:SCREEN-VALUE = 'Total Disponible: ' + TRIM(STRING(Creditos.Monto - Creditos.Sdo_capital,"zzz,zzz,zz9.99")).
                ELSE
                    ASSIGN w_tapaFUP:SCREEN-VALUE = '     Sobrecupo  : ' + TRIM(STRING(Creditos.Sdo_capital - Creditos.monto,"zzz,zzz,zz9.99"))
                           w_sobrecupo:SCREEN-VALUE = '     Sobrecupo  : ' + TRIM(STRING(Creditos.Sdo_capital - Creditos.monto,"zzz,zzz,zz9.99")).
            END.
        END.
        ELSE DO:
            DISABLE Btn_Factura WITH FRAME F-Cre.
            ASSIGN w_NumFactura:VISIBLE = FALSE
                   w_PagoRecaudo:VISIBLE = FALSE
                   w_tapaFUP:VISIBLE = FALSE
                   W_tapaCV:VISIBLE = FALSE
                   w_sobrecupo:VISIBLE = FALSE
                   w_sdoMoraCR:VISIBLE = FALSE.
        END.

        IF creditos.sdo_capital GT Creditos.monto THEN
            ASSIGN W_SdoDeuda:SCREEN-VALUE = STRING(DECIMAL(W_SdoDeuda:SCREEN-VALUE) - (creditos.sdo_capital - Creditos.monto))
                   P_otrosC = Creditos.Com_Bruta + Creditos.Com_Adicional.
    END.

    IF Creditos.Val_Atraso GT Creditos.Sdo_Capital THEN
        Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Sdo_Capital).

    IF Creditos.Sdo_Capital GT Creditos.Sdo_Proyec THEN
        W_TSdoVdo = W_TSdoVdo + (Creditos.Sdo_Capital - Creditos.Sdo_Proyec).
    ELSE
        ASSIGN W_TSdoVdo = Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob - Creditos.Int_Anticipado.

    IF W_cuprot = 570 THEN
        W_TSdoVdo = Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob + creditos.int_difCobro +
                    Creditos.Int_Corrientes - Creditos.Int_Anticipado + Creditos.cuota.

    IF W_TSdoVdo LE 0 THEN
        W_TSdoVdo = 0.

    IF W_TSdoVdo GT W_SdoDeuda THEN
        W_TSdoVdo = W_SdoDeuda.

    IF Creditos.Val_Desembolso LE 0 THEN
        Creditos.Val_Desembolso:SCREEN-VALUE = STRING(Creditos.Monto).

    ASSIGN W_TSdoVdo:SCREEN-VALUE = STRING(W_TSdoVdo)
           Creditos.Provision:SCREEN-VALUE = STRING(Creditos.Provision)
           Creditos.Fec_Reestructurado:SCREEN-VALUE = STRING(Creditos.Fec_Reestructurado)
           Creditos.Cuota:SCREEN-VALUE = STRING(Creditos.Cuota)
           Creditos.INT_Corrientes:SCREEN-VALUE = STRING(Creditos.INT_Corrientes + w_preliquida) 
           Creditos.INT_Anticipado:SCREEN-VALUE = STRING(Creditos.INT_Anticipado)
           Creditos.Sdo_Capital:SCREEN-VALUE = STRING(Creditos.Sdo_Capital)
           Creditos.INT_MorCobrar:SCREEN-VALUE = STRING(Creditos.INT_MorCobrar + Creditos.INT_MoraDifCob)
           Creditos.Plazo:SCREEN-VALUE = STRING(Creditos.Plazo)
           Creditos.Tasa:SCREEN-VALUE = STRING(Creditos.Tasa)
           Creditos.Num_Credito:SCREEN-VALUE = STRING(Creditos.Num_Credito)
           W_SdoOtros:SCREEN-VALUE = STRING(Creditos.Costas + Creditos.Honorarios + Creditos.Polizas)
           W_SdoOtros
           creditos.seg_vida:SCREEN-VALUE = STRING(creditos.seg_vida)
           creditos.seg_cartera:SCREEN-VALUE = STRING(creditos.seg_cartera).

    DISPLAY Creditos.Categoria Creditos.Cta_Contable.

    IF Creditos.Per_pago EQ 1 THEN
        NomPdadP:SCREEN-VALUE = "Plazo en Semanas".
    ELSE
        IF Creditos.Per_pago EQ 2 THEN
            NomPdadP:SCREEN-VALUE = "Plazo en Décadas".

    IF Creditos.Per_pago EQ 3 THEN
        NomPdadP:SCREEN-VALUE = "Plazo en Quincenas".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Simulacion Wwin 
PROCEDURE Mostrar_Simulacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR W_RowIdCr AS ROWID.

  FRAME F_Cheq:VISIBLE = FALSE.

  FIND FIRST Creditos WHERE Creditos.Nit         EQ P_NitCli AND 
                            Creditos.Cod_Credito EQ P_CodCre AND
                            Creditos.Num_Credito EQ P_NumCre AND
                            Creditos.Tip_Credito EQ P_TipCre NO-LOCK NO-ERROR.
  IF AVAILABLE Creditos THEN DO:
     IF cod_credito = 570 OR cod_credito = 870 THEN W_cuprot = 570.
     ASSIGN W_RowIdCr = ROWID(Creditos).
     FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.  

     FOR EACH COBROS WHERE COBROS.NIT = CREDITOS.NIT AND Cobros.estado = 1 NO-LOCK
                        BY COBROS.Fec_Compromiso DESCEND:
         FIND FIRST Creditos WHERE Creditos.Nit         EQ P_NitCli AND 
                                   Creditos.Num_Credito EQ Cobros.Num_Credito AND
                                   Creditos.Sdo_capital GT 0 NO-LOCK NO-ERROR.
         IF AVAIL(Creditos) THEN DO:
            MESSAGE "Acuerdo de pago por VALOR de $" Cobros.Val_Compromiso SKIP
                    "Para el Credito Nro. : " Cobros.Num_Credito           SKIP
                    "Fecha del Compromiso : " Cobros.Fec_Compromiso
               VIEW-AS ALERT-BOX TITLE "INFORMATIVO ACUERDO DE PAGO".
            LEAVE.
         END.
     END.

     FIND Creditos WHERE ROWID(Creditos) EQ W_RowIdCr NO-LOCK NO-ERROR.
     RUN Mostrar_Credito.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Muestra_DistPrel Wwin 
PROCEDURE Muestra_DistPrel :
DEF VAR w_iTasaM LIKE Creditos.Sdo_Capital.
DEF VAR w_TasaNA AS DEC FORMAT "->>>>9.9999999".
DEF VAR w_TasaEA AS DEC FORMAT "->>>>9.9999999".
DEF VAR W_DiaAnt AS INT.
DEF VAR W_IAntic AS INT.
DEF VAR w_i AS INT.
DEFINE VAR valorAboCredito AS DECIMAL.
DEFINE VAR pError AS LOGICAL.

P_Poliza = 0.
P_Honora = 0.
P_Costas = 0.
P_IDifCobMor = 0.
P_IMora = 0.
P_IDifCob = 0.
P_ICte = 0.
P_IAntic = 0.
P_Capital = 0.
P_VlrNoDist = 0.
P_IAC = 0.
w_descAnti = 0.
w_descTM = 0.
P_SeguroVida = 0.
P_SeguroDeudor = 0.

ASSIGN P_IAC = 0
       W_SaldoK = creditos.monto.

IF W_ValPag > Creditos.Honorarios +
              Creditos.Costas +
              Creditos.Polizas +
              Creditos.Com_Adicional +
              Creditos.Com_Bruta +
              Creditos.Int_MorCobrar +
              Creditos.Int_MoraDifCob +
              creditos.int_difCobro +
              Creditos.Int_Corrientes +
              Creditos.Sdo_Capital -
              Creditos.Int_Anticipado THEN DO:
    MESSAGE "El valor a Pagar no puede superar el saldo total de la deuda..."
        VIEW-AS ALERT-BOX.

    ASSIGN W_ValPag = Creditos.Honorarios +
                      Creditos.Costas +
                      Creditos.Polizas +
                      Creditos.Com_Adicional +
                      Creditos.Com_Bruta +
                      Creditos.Int_MorCobrar +
                      Creditos.Int_MoraDifCob +
                      creditos.int_difCobro +
                      Creditos.Int_Corrientes +
                      Creditos.Sdo_Capital -
                      Creditos.Int_Anticipado
           W_ValPag:SCREEN-VALUE IN FRAME F_Cre = STRING(W_ValPag).
END.

/*IF w_Comple THEN DO:*/
    RUN p-pagoCredito.R(INPUT NO,
                        INPUT Creditos.Cod_Credito,
                        INPUT Creditos.Nit,
                        INPUT Creditos.Num_Credito, 
                        INPUT W_ValPag,
                        INPUT 0,
                        INPUT 0,
                        INPUT 0,
                        INPUT 1,
                        INPUT w_fecha,
                        INPUT YES,
                        OUTPUT P_Poliza,
                        OUTPUT P_Honora,
                        OUTPUT P_Costas,
                        OUTPUT P_SeguroVida,
                        OUTPUT P_SeguroDeudor,
                        OUTPUT P_IDifCobMor,
                        OUTPUT P_IMora,
                        OUTPUT P_IDifCob,
                        OUTPUT P_ICte,
                        OUTPUT P_IAntic,
                        OUTPUT P_Capital,
                        OUTPUT P_VlrNoDist,
                        OUTPUT pError).
/*END.
ELSE
    IF W_ValPag > 0 THEN DO:
        IF W_cuprot NE 570 THEN DO:
            FOR EACH control_pagos WHERE control_pagos.Nit = creditos.nit
                                     AND control_pagos.Num_Credito = creditos.num_credito
                                     AND control_pagos.Nro_Cuota > 0:
                w_saldok = w_saldok - control_pagos.Cap_pagado.

                FIND FIRST btcontrol_pagos WHERE btcontrol_pagos.Nro_Cuota = control_pagos.Nro_Cuota NO-LOCK NO-ERROR.
                IF NOT AVAILABLE btcontrol_pagos THEN
                    NEXT.

                IF btcontrol_pagos.marca THEN DO:
                    w_i = w_i + 1.

                    RUN AboCredito3.R(INPUT NO,
                                      INPUT Creditos.Agencia,
                                      Creditos.Cod_Credito,
                                      Creditos.Nit,
                                      INPUT Creditos.Num_Credito,
                                      /*btcontrol_pagos.TOTAL_Cuota*/ ROUND((btcontrol_pagos.TOTAL_Cuota / 100) + 1,0) * 100,
                                      INPUT 0,
                                      0,
                                      0,
                                      1,
                                      btcontrol_pagos.Nro_Cuota,
                                      1,
                                      YES,
                                      OUTPUT SP_Poliza,
                                      OUTPUT SP_Honora,
                                      OUTPUT SP_Costas,
                                      OUTPUT SP_SeguroVida,
                                      OUTPUT SP_SeguroDeudor,
                                      OUTPUT SP_IDifCobMor,
                                      OUTPUT SP_IMora,
                                      OUTPUT SP_IDifCob,
                                      OUTPUT SP_ICte,
                                      OUTPUT SP_IAntic,
                                      OUTPUT SP_Capital,
                                      OUTPUT SP_VlrNoDist,
                                      OUTPUT SP_IAC).

                    P_Poliza = P_Poliza + SP_Poliza.
                    P_Honora = P_Honora + SP_Honora.
                    P_Costas = P_Costas + SP_Costas.
                    P_SeguroVida = P_SeguroVida + SP_SeguroVida.
                    P_SeguroDeudor = P_SeguroDeudor + SP_SeguroDeudor.
                    P_IDifCobMor= P_IDifCobMor + SP_IDifCobMor.
                    P_IMora = P_IMora + SP_IMora.
                    P_IDifCob = P_IDifCob + SP_IDifCob.
                    P_ICte = P_ICte + SP_ICte.
                    P_IAntic = P_IAntic + SP_IAntic.
                    P_Capital = P_Capital + SP_Capital.
                    P_VlrNoDist = P_VlrNoDist + SP_VlrNoDist.
                    P_IAC = P_IAC + SP_IAC.
                    w_saldok = w_saldok - control_pagos.pagos_capitalAcum.
                    w_descTM = w_descTM + btControl_pagos.dif_TasaM.
                    w_descAnti = w_descAnti + btControl_pagos.pag_anticipado.
                END.
            END.
        END.
        ELSE
            RUN AboCredito_CupoRotativo.R(INPUT NO,
                                          INPUT Creditos.Agencia,
                                          Creditos.Cod_Credito,
                                          Creditos.Nit,
                                          INPUT Creditos.Num_Credito,
                                          W_ValPag,
                                          INPUT 0,
                                          0,
                                          0,
                                          1, 
                                          OUTPUT P_Poliza,
                                          OUTPUT P_Honora,
                                          OUTPUT P_Costas,
                                          OUTPUT P_IDifCobMor,
                                          OUTPUT P_IMora,
                                          OUTPUT P_IDifCob,
                                          OUTPUT P_ICte,
                                          OUTPUT P_IAntic,
                                          OUTPUT P_Capital,
                                          OUTPUT P_VlrNoDist,
                                          OUTPUT P_ComBruta,
                                          OUTPUT P_ComAdicional,
                                          OUTPUT P_GmfxC,
                                          OUTPUT P_Segcartera).
    END.*/

ASSIGN W_AboInt:SCREEN-VALUE = STRING(P_ICte)
       W_AboCap:SCREEN-VALUE = STRING(P_Capital)
       W_AboIntMor:SCREEN-VALUE = STRING(P_IMora + P_IDifCobMor)
       W_AboIntAnt:SCREEN-VALUE = STRING(creditos.INT_anticipado) /**/
       W_AIA:SCREEN-VALUE = STRING(P_IAntic) /*W_AIA:SCREEN-VALUE = STRING(P_IAC)*/
       W_AboIntDifCob:SCREEN-VALUE = STRING(P_IDifCob)
       W_AboPoliza:SCREEN-VALUE = STRING(P_Poliza)
       W_AboHonora:SCREEN-VALUE = STRING(P_Honora)
       W_AboCostas:SCREEN-VALUE = STRING(P_Costas)
       W_AboSegVida:SCREEN-VALUE = STRING(P_SeguroVida)
       W_AboSegDeudor:SCREEN-VALUE = STRING(P_SeguroDeudor)
       W-VlrAboComis:SCREEN-VALUE = STRING(P_ComBruta + P_comAdicional)
       W-VlrAboGmfxC:SCREEN-VALUE = STRING(P_GmfxC)
       W-VlrAboSegCart:SCREEN-VALUE = STRING(P_SegCartera)
       W_NoDistribuido:SCREEN-VALU = STRING(P_VlrNoDist)
       w_DescTM:SCREEN-VALUE = STRING(w_DescTM).
       w_descAnti:SCREEN-VALUE = STRING(w_DescAnti).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Periodos30 Wwin 
PROCEDURE Periodos30 :
DEFINE INPUT PARAMETER fechaIni AS DATE.
DEFINE INPUT PARAMETER diasAconvertir AS INTEGER.
DEFINE OUTPUT PARAMETER dias30 AS INTEGER.

DEFINE VAR pCont AS INTEGER.
DEFINE VAR pDiasAsumar AS INTEGER.
DEFINE VAR pFechaAux AS DATE.

/* Se calculan los días de atraso de acuerdo a periodos de 30 días */
pFechaAux = fechaIni + 1. /* Arranca a sumar días de mora a partir de un día de la fecha de vencimiento */

DO pCont = 1 TO diasAconvertir:
    pDiasAsumar = 1.

    IF DAY(pFechaAux) = 31 THEN  /* Los días 31 no se tienen en cuenta */
        pDiasAsumar = 0.

    IF MONTH(pFechaAux) = 2 AND DAY(pFechaAux) = 28 THEN  /* Cuando pasa por un 28 de febrero suma lo correspondiente al 28, 29 y 30 */
        pDiasAsumar = 3.

    IF MONTH(pFechaAux) = 2 AND DAY(pFechaAux) = 29 THEN  /* Cuando pasa por un 29 de febrero no lo suma, porque ya lo sumo en el 28 */
        pDiasAsumar = 0.

    IF pCont = 1 THEN DO:
        IF DAY(creditos.fec_pago) = 28 AND MONTH(creditos.fec_pago) = 2 AND YEAR(creditos.fec_pago) = YEAR(pFechaAux) THEN    /* Cuando la fecha de pago es un 28 de febrero suma lo del 29 y el 30 */
            pDiasAsumar = 3.

        IF DAY(creditos.fec_pago) = 29 AND MONTH(creditos.fec_pago) = 2 AND YEAR(creditos.fec_pago) = YEAR(pFechaAux) THEN    /* Cuando la fecha de pago es un 29 de febrero suma lo del 30 */
            pDiasAsumar = 2.
    END.

    dias30 = dias30 + pDiasAsumar.
    pFechaAux = pFechaAux + 1.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proc_Recaudo Wwin 
PROCEDURE Proc_Recaudo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
      IF Creditos.Abogado THEN DO:  /*Junio 13/05 GAER*/
          MESSAGE "Crèdito en Cobro Jurìdico. " VIEW-AS ALERT-BOX.
         FIND LAST COBROS WHERE COBROS.NIT = CREDITOS.NIT AND Cobros.Estado = 1 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE (COBROS) THEN DO:
            MESSAGE "Crèdito en Cobro Jurìdico - Asociado sin acuerdo de Pago..." SKIP
                    "Llamar a Cartera para autorizacion." VIEW-AS ALERT-BOX.
            RETURN.
         END.
      END.

     IF creditos.cod_credito = 570 OR creditos.cod_credito = 870 THEN W_cuprot = 570.
     FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.          
     FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito AND
                             Pro_Creditos.Tip_Credito EQ Creditos.Tip_Credito NO-LOCK NO-ERROR.
     IF AVAILABLE Pro_Creditos THEN DO:
        RUN CortoLargoCreditos NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
           APPLY "choose" TO Btn_Salir IN FRAME F_Cre.
     END.
     ELSE MESSAGE "No se encontro el producto de Creditos" VIEW-AS ALERT-BOX ERROR.
        
/*     FIND LAST Cobros WHERE 
               Cobros.Num_Credito EQ Creditos.Num_Credito AND
               Cobros.Nit         EQ Creditos.Nit AND
               Cobros.Estado      EQ 1 AND
               YEAR(Cobros.Fec_Compromiso) EQ YEAR(W_Fecha) NO-LOCK NO-ERROR.
     IF AVAILABLE COBROS THEN 
        MESSAGE "Acuerdo de pago" SKIP         
                 Cobros.Val_Compromiso SKIP(1) VIEW-AS ALERT-BOX TITLE "INFORMATIVO".       
  */
     /*busca cuenta de caja-cheque*/
     FIND FIRST Cuentas WHERE Cuentas.Cuenta        GT "0"
                        AND   Cuentas.Cod_FlujoEfec EQ "D"
                     AND   Cuentas.Car_Efectivo     EQ  3
                     AND   Cuentas.Estado           EQ  1 NO-LOCK NO-ERROR.
     IF AVAIL(Cuentas) THEN 
        W_Banco = Cuentas.Cuenta.
     ELSE 
       MESSAGE "No se ha encontrado la cuenta de caja cheques" SKIP
               "para realizar la operacion. " SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.

    
     /*busca cuenta de caja-general la misma para Caja cheque*/
     FIND FIRST Cuentas WHERE Cuentas.Cod_FlujoEfec EQ "D"
                        AND   Cuentas.Car_Efectivo  EQ  2
                        AND   Cuentas.Estado        EQ  1 NO-LOCK NO-ERROR.
     IF AVAILABLE Cuentas THEN 
        ASSIGN W_Caja  = Cuentas.Cuenta.
               /*W_Banco = Cuentas.Cuenta.*/
     ELSE 
       MESSAGE "No se ha encontrado la cuenta de caja efectivo" SKIP
               "para realizar la operacion. " SKIP(1)
               "Comuniquese con el Administrador" VIEW-AS ALERT-BOX ERROR.
     
     RUN Mostrar_Credito.
  END.
  ASSIGN FRAME F_Factura:VISIBLE = FALSE.
  IF NOT AVAIL(Creditos) OR NOT AVAIL(Operacion) OR NOT AVAIL(Cuentas)
  OR W_Banco LE "0"      OR NOT AVAIL(Clientes)  OR NOT AVAIL(Pro_Creditos) THEN
     APPLY "choose" TO Btn_Salir IN FRAME F_Cre. 

  FRAME F_Cheq:VISIBLE = FALSE.

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
DEF VAR w_total_2 AS DEC.
DEF VAR w_distr_2 AS DEC.
DEFINE VAR pError AS LOGICAL.

RUN ControlLavado.
IF NOT CtrLavado THEN
    RETURN ERROR.

ASSIGN T_AboK = 0
       T_AboI = 0
       T_SdoI = Creditos.Sdo_Capital.

IF W_VrEfec GT 0 THEN DO:
    w_total_2 = W_VrEfec.

    IF W_cuprot NE 570 THEN DO:
        IF w_Comple THEN DO:
            /*Distribuye abonos en Créditos,sin actualizar*/
            RUN p-pagoCredito.R(INPUT YES,
                                INPUT Creditos.Cod_Credito,
                                INPUT Creditos.Nit,
                                INPUT Creditos.Num_Credito,
                                INPUT W_VrEfec,
                                INPUT W_Cbte,
                                INPUT W_DocContab,
                                INPUT 0,
                                INPUT 1,
                                INPUT w_fecha,
                                INPUT YES,
                                OUTPUT P_Poliza,
                                OUTPUT P_Honora,
                                OUTPUT P_Costas,
                                OUTPUT P_SeguroVida,
                                OUTPUT P_SeguroDeudor,
                                OUTPUT P_IDifCobMor,
                                OUTPUT P_IMora,
                                OUTPUT P_IDifCob,
                                OUTPUT P_ICte,
                                OUTPUT P_IAntic,
                                OUTPUT P_Capital,
                                OUTPUT P_VlrNoDist,
                                OUTPUT pError) NO-ERROR.
        END.
        ELSE DO:
            FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
                IF w_total_2 >= btcontrol_pagos.TOTAL_Cuota THEN DO:
                    w_distr_2 = btcontrol_pagos.TOTAL_Cuota.

                    IF btcontrol_pagos.Cap_pagado = 0 AND btcontrol_pagos.Int_pagado = 0 THEN
                        P_Benefi = TRUE.
                    ELSE
                        P_Benefi = FALSE.
                END.
                ELSE DO:
                    w_distr_2 = w_total_2.
                    P_Benefi = FALSE.
                END.

                w_distr_2 = w_total_2. /* Para que sólo lo haga  una vez */

                /*Distribuye abonos en Créditos,sin actualizar*/
                RUN p-pagocredito.R(INPUT YES,
                                    INPUT Creditos.Cod_Credito,
                                    INPUT Creditos.Nit,
                                    INPUT Creditos.Num_Credito,
                                    INPUT w_distr_2,
                                    INPUT W_Cbte,
                                    INPUT W_DocContab,
                                    INPUT 0,
                                    INPUT 1,
                                    INPUT w_fecha,
                                    INPUT YES,
                                    OUTPUT P_Poliza,
                                    OUTPUT P_Honora,
                                    OUTPUT P_Costas,
                                    OUTPUT P_SeguroVida,
                                    OUTPUT P_SeguroDeudor,
                                    OUTPUT P_IDifCobMor,
                                    OUTPUT P_IMora,
                                    OUTPUT P_IDifCob,
                                    OUTPUT P_ICte,
                                    OUTPUT P_IAntic,
                                    OUTPUT P_Capital,
                                    OUTPUT P_VlrNoDist,
                                    OUTPUT pError).

                w_total_2 = w_total_2 - w_distr_2.

                IF w_total_2 <= 0 THEN
                    LEAVE.
            END.
        END.
    END.
    ELSE DO:
        /*Distribuye abono-Efectivo en Créditos,graba Mov_creditos,Mov_Contable y PlanPagos*/
        RUN AboCredito_CupoRotativo.R(INPUT YES,
                                      INPUT Creditos.Agencia,
                                      Creditos.Cod_Credito,
                                      Creditos.Nit,
                                      INPUT Creditos.Num_Credito,
                                      W_VrEfec,
                                      INPUT W_Cbte,
                                      W_DocContab,
                                      0,
                                      1,
                                      OUTPUT P_Poliza,
                                      OUTPUT P_Honora,
                                      OUTPUT P_Costas,
                                      OUTPUT P_IDifCobMor,
                                      OUTPUT P_IMora,
                                      OUTPUT P_IDifCob,
                                      OUTPUT P_ICte,
                                      OUTPUT P_IAntic,
                                      OUTPUT P_Capital,
                                      OUTPUT P_VlrNoDist,
                                      OUTPUT P_ComBruta,
                                      OUTPUT P_ComAdicional,
                                      OUTPUT P_GmfxC,
                                      OUTPUT P_Segcartera) NO-ERROR.
    END.

    IF ERROR-STATUS:ERROR OR P_VlrNoDist NE 0 THEN DO:
        MESSAGE "Prog.AboCredito.P...Retornò Error, Ò," SKIP
                "Retornò Valor no-distribuìdo $" P_VlrNoDist SKIP
                "               No se acepta el Abono."
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.

    ASSIGN T_AboK = P_Capital
           T_AboI = P_IDifCobMor + P_IMora + P_IDifCob + P_ICte + P_IAntic.

    IF W_cuprot = 570 THEN
        P_OtrosC = P_Honora + P_poliza.
END.

IF W_VrCheq GT 0 THEN DO:
     w_total_2 = W_VrCheq.
     IF W_cuprot NE 570 THEN DO: 
       IF w_Comple THEN DO:
           RUN p-pagoCredito.R(INPUT YES,
                               INPUT Creditos.Cod_Credito,
                               INPUT Creditos.Nit,
                               INPUT Creditos.Num_Credito,
                               INPUT W_VrEfec,
                               INPUT W_Cbte,
                               INPUT W_DocContab,
                               INPUT 0,
                               INPUT 1,
                               INPUT w_fecha,
                               INPUT YES,
                               OUTPUT P_Poliza,
                               OUTPUT P_Honora,
                               OUTPUT P_Costas,
                               OUTPUT P_SeguroVida,
                               OUTPUT P_SeguroDeudor,
                               OUTPUT P_IDifCobMor,
                               OUTPUT P_IMora,
                               OUTPUT P_IDifCob,
                               OUTPUT P_ICte,
                               OUTPUT P_IAntic,
                               OUTPUT P_Capital,
                               OUTPUT P_VlrNoDist,
                               OUTPUT pError).
       END.
       ELSE DO:
           FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
               IF w_total_2 >= btcontrol_pagos.TOTAL_Cuota THEN DO:
                   w_distr_2 = btcontrol_pagos.TOTAL_Cuota.
                   
                   IF btcontrol_pagos.Cap_pagado = 0 and
                      btcontrol_pagos.Int_pagado = 0 THEN
                       P_Benefi = TRUE.
                   ELSE
                       P_Benefi = FALSE.
               END.
               ELSE DO:
                  w_distr_2 = w_total_2.
                  P_Benefi = FALSE.
               END.

               w_distr_2 = w_total_2.

               RUN p-pagoCredito.R(INPUT YES,
                                   INPUT Creditos.Cod_Credito,
                                   INPUT Creditos.Nit,
                                   INPUT Creditos.Num_Credito,
                                   INPUT w_distr_2,
                                   INPUT W_Cbte,
                                   INPUT W_DocContab,
                                   INPUT 1,
                                   INPUT 1,
                                   INPUT w_fecha,
                                   INPUT YES,
                                   OUTPUT P_Poliza,
                                   OUTPUT P_Honora,
                                   OUTPUT P_Costas,
                                   OUTPUT P_SeguroVida,
                                   OUTPUT P_SeguroDeudor,
                                   OUTPUT P_IDifCobMor,
                                   OUTPUT P_IMora,
                                   OUTPUT P_IDifCob,
                                   OUTPUT P_ICte,
                                   OUTPUT P_IAntic,
                                   OUTPUT P_Capital,
                                   OUTPUT P_VlrNoDist,
                                   OUTPUT pError).
    
                 w_total_2 = w_total_2 - w_distr_2. 
                 IF w_total_2 <= 0 THEN LEAVE.
           END.
       END.     
     END.
     ELSE
         RUN AboCredito_CupoRotativo.R    /*Distribuye abono-Cheque en Créditos,graba Mov_creditos,Mov_Contable y PlanPagos*/
           (INPUT YES,
            INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
            INPUT Creditos.Num_Credito,W_VrCheq,
            INPUT W_Cbte,W_DocContab,1,1,
            OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IDifCobMor, OUTPUT P_IMora,
            OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capital,
            OUTPUT P_VlrNoDist, OUTPUT P_ComBruta, OUTPUT P_ComAdicional, OUTPUT P_GmfxC, OUTPUT P_Segcartera) NO-ERROR.


    IF ERROR-STATUS:ERROR OR P_VlrNoDist NE 0 THEN DO:                          
       MESSAGE "Prog.AboCredito.P...Retornò Error, Ò," SKIP                     
               "Retornò Valor no-distribuìdo $" P_VlrNoDist SKIP                
               "               No se acepta el Abono." VIEW-AS ALERT-BOX ERROR. 
       RETURN ERROR.                                                                              
    END.

    ASSIGN T_AboK = T_AboK + P_Capital
           T_AboI = T_AboI + (P_IDifCobMor + P_IMora + P_IDifCob + P_ICte + P_IAntic).
    IF W_cuprot = 570 THEN P_OtrosC = P_Honora + P_poliza.

    FIND FIRST TCheq NO-ERROR.
 END.

 T_SdoF = Creditos.Sdo_Capital.
 
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
                     INPUT Creditos.Cod_Credito, INPUT W_CtaCap,     INPUT W_Caja,
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
                     INPUT Creditos.Cod_Credito, INPUT W_CtaSYA_Des,INPUT W_Caja,                     
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

 IF Creditos.Sdo_Capital LE 0 AND Creditos.Estado EQ 3 THEN  /*Junio 16/05 Gaer*/
    FOR EACH Relaciones WHERE 
            Relaciones.Nit            EQ Creditos.Nit                   AND
            INTEG(Relaciones.Cuenta)  EQ Creditos.Num_Credito           AND
            Relaciones.Clase_Producto EQ 2                              AND
            Relaciones.Cod_Producto   EQ Creditos.Cod_Credito           AND
            Relaciones.Cod_Relacion   EQ 11                             AND
            Relaciones.Estado         EQ 1 :
        ASSIGN Relaciones.Estado = 2.
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
    /*verificacion compromiso del credito*/
    FIND LAST Cobros WHERE Cobros.Num_Credito EQ Creditos.Num_Credito
                       AND Cobros.Nit EQ Creditos.Nit
                       AND Cobros.Estado EQ 1
                       AND Cobros.Fec_Compromiso LE W_Fecha NO-ERROR.
    IF NOT AVAILABLE Cobros THEN
        FIND LAST Cobros WHERE Cobros.Num_Credito EQ Creditos.Num_Credito
                           AND Cobros.Nit EQ Creditos.Nit
                           AND Cobros.Estado EQ 1 NO-ERROR.
    IF AVAILABLE Cobros THEN DO:
        ASSIGN Cobros.Age_Recaudo = W_Agencia
               Cobros.Usu_Recaudo = W_usuario
               Cobros.Val_Cumplido = Cobros.Val_Cumplido + W_ValPag.

        IF Cobros.Val_Cumplido GE Cobros.Val_Compromiso THEN
            ASSIGN Cobros.Estado = 2
                   Cobros.Fec_Cumplimiento = W_Fecha.

        MESSAGE "El Acuerdo de Pago Pactado fue Actualizado."
            VIEW-AS ALERT-BOX TITLE "INFORMATIVO".

        FIND CURRENT Cobros NO-LOCK NO-ERROR.
    END.

    RUN Taquilla NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Se ha producido un error en la transacción" SKIP
                "consulte con el Administrador del Sistema"
            VIEW-AS ALERT-BOX ERROR.
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

