&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Tras_Cuentas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Tras_Cuentas 
CREATE WIDGET-POOL.

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEFINE VAR W_NeedNit AS LOGICAL INITIAL NO.
DEFINE VAR W_MismoCliente AS LOGICAL INITIAL YES.
DEFI VAR W_SiCanc AS CHAR FORM "X(20)".
DEFI VAR W_SiImpErr AS LOG INIT FALSE.
DEFI VAR RowidTpc AS ROWID.
DEFI VAR W_NoGmf AS LOG INIT FALSE.
DEFI VAR W_Inicio AS LOG INIT FALSE.
DEFI VAR W_Age LIKE Agencias.Agencia.
DEFI VAR W_Nat LIKE Cuentas.Naturaleza.
DEFI VAR W_Ctr LIKE Cuentas.Ctr_Natur.
DEFI VAR W_Cpte AS INTEGER.
DEFI VAR VrCargos AS DECIMAL INIT 0 EXTENT 10.
DEFI VAR VrAbonos AS DECIMAL INIT 0 EXTENT 10.
DEFI VAR W_Valor LIKE Creditos.Cuota INITIAL 0.
DEFI VAR W_Oper LIKE Mov_Creditos.Cod_Operacion.
DEFI VAR W_Desc LIKE Mov_Creditos.Descrip.
DEFI VAR VrDist LIKE Creditos.Sdo_Capital INITIAL 0.
DEFI VAR W_DescC AS CHARACTER.
DEFI VAR W_MismoCte AS LOG INIT FALSE.
DEFI VAR Porc_Base LIKE Base_Ret.Porcentaje.
DEFI VAR Cta_Base LIKE Base_Ret.Cuenta.
DEFI VAR Nom_Base LIKE Base_Ret.Nombre.
DEFI VAR W_Autorizo LIKE Usuarios.Usuario.
DEFI VAR W_swGMF AS INTEGER INITIAL 0.
DEFI VAR W_tipopro LIKE ahorros.tip_ahorro.
DEFINE VAR P_Poliza LIKE Creditos.Cuota INITIAL 0.
DEFINE VAR P_Honora LIKE Creditos.Cuota INITIAL 0.
DEFINE VAR P_Costas LIKE Creditos.Cuota INITIAL 0.
DEFINE VAR P_IMorDifC LIKE Creditos.Cuota INITIAL 0.
DEFINE VAR P_IMora LIKE Creditos.Cuota INITIAL 0.
DEFINE VAR P_IDifCob LIKE Creditos.Cuota INITIAL 0.
DEFINE VAR P_ICte AS DECIMAL INITIAL 0.
DEFINE VAR P_IAntic AS DECIMAL INITIAL 0.
DEFINE VAR P_Capit AS DECIMAL INITIAL 0.
DEFINE VAR P_VlrNoDist AS DECIMAL INITIAL 0. /*Valor NO Distribuido*/
DEFINE VAR P_SeguroVida AS DECIMAL.
DEFINE VAR P_SeguroDeudor AS DECIMAL.

DEFI VAR W_OpAboCred AS INTEGER EXTENT 10. /*Abonos a Créditos*/
DEFI VAR W_OpCgoCred AS INTEGER EXTENT 10. /*Cargos a Créditos*/

ASSIGN W_OpAboCred [1] = 020101001  /*Abonos a capital Créditos*/
       W_OpAboCred [2] = 020101002  /*Abonos a IntMora Créditos*/
       W_OpAboCred [3] = 020101003  /*Abonos a IntCtes Créditos*/
       W_OpAboCred [4] = 020101004  /*Abonos a IntDifCob Créditos*/
       W_OpAboCred [5] = 020101005  /*Abonos a IntAntic. Créditos*/
       W_OpAboCred [6] = 020101006  /*Abonos a Polizas Créditos*/
       W_OpAboCred [7] = 020101007  /*Abonos a Honorarios Créditos*/
       W_OpAboCred [8] = 020101008  /*Abonos a CostasJ.Créditos*/
       W_OpAboCred [9] = 020101010 /* Abono Seguro de Vida */
       W_OpAboCred [10] = 020101011 /* Abono Seguro de Vivienda */
       W_OpCgoCred [1] = 020102001  /*Cargos a capital Créditos*/   
       W_OpCgoCred [2] = 020102003  /*Cargos a Honorarios Créditos*/
       W_OpCgoCred [3] = 020102004  /*Cargos a Polizas Créditos*/
       W_OpCgoCred [4] = 020102005  /*Cargos a CostasJ. Créditos*/
       W_OpCgoCred [5] = 020102006  /*Cargos a Intereses(Todos) Créditos*/
       W_OpCgoCred [9] = 020102008 /* Cargos Seguro de Vida */
       W_OpCgoCred [10] = 020102009. /* Cargos Seguro de Vivienda */

DEFI VAR W_OpTrasAg AS INTEGER INITIAL 030303001.
DEFI VAR W_OpAboAho AS INTEGER INITIAL 010301001.
DEFI VAR W_OpCgoAho AS INTEGER INITIAL 010102001.

DEFI TEMP-TABLE TPC
    FIELD TP AS CHAR FORM "X(1)"
    FIELD TipA AS INTEGER
    FIELD Op AS INTEG FORM "9"
    FIELD Cpto AS CHAR FORM "X(10)"
    FIELD AgeP AS INTEGER
    FIELD NitP AS CHARACTER
    FIELD CodP AS INTEGER
    FIELD CtaP AS CHARACTER
    FIELD NomP AS CHARACTER
    FIELD Cargo AS DECIMAL INIT 0
    FIELD Abono AS DECIMAL INIT 0
    FIELD Gmf AS DECIMAL INIT 0
    FIELD AgDest AS INTEGER
    FIELD Contra AS CHARACTER
    FIELD CedCont AS CHARACTER
    FIELD DocRef AS CHARACTER
    FIELD Descrip AS CHAR FORM "X(80)"
    FIELD CamAge AS LOGICAL INITIAL NO
    FIELD IdSiGmf AS LOGICAL INITIAL FALSE
    INDEX Ppal IS UNIQUE TP AgeP NitP CodP CtaP Cpto DocRef.

DEFI VAR W_VrConsig AS DECIMAL INIT 0.

DEFI TEMP-TABLE TPdctos
    FIELD Agen AS INTEGER
    FIELD FApert AS DATE
    FIELD TP AS CHARACTER FORMAT "X(1)"
    FIELD Estado AS INTEGER FORMAT "9"
    FIELD CodP AS INTEGER
    FIELD CtaP AS CHARACTER
    FIELD Pagare AS CHARACTER
    FIELD NomP AS CHARACTER
    FIELD Cuota AS DECIMAL
    FIELD VrSdo AS DECIMAL
    FIELD IntCte AS DECIMAL
    FIELD IntCau AS DECIMAL
    FIELD Canje AS DECIMAL
    FIELD SMinimo AS DECIMAL
    FIELD FecPxDb AS DATE
    FIELD ForPag AS INTEGER
    FIELD NroTarDB AS CHARACTER
    FIELD TipoPro AS INTEGER.

DEFI TEMP-TABLE TempCtas
    FIELD Agen AS INTEGER
    FIELD TipP AS CHARACTER FORM "X(1)"
    FIELD Pto AS INTEGER
    FIELD CtaPro AS CHARACTER
    FIELD CtaProCaja AS CHARACTER
    FIELD CtaIng AS CHARACTER
    FIELD CtaIngCaja AS CHARACTER
    FIELD CtaLiq AS CHARACTER
    FIELD CtaLiqCaja AS CHARACTER
    FIELD IntAnt AS CHARACTER
    FIELD IntAntCaja AS CHARACTER
    FIELD IntMor LIKE Cuentas.Cuenta
    FIELD IntMorCaja AS CHARACTER
    FIELD MorIng AS CHARACTER
    FIELD morIngCaja AS CHARACTER
    FIELD DifCoD AS CHARACTER
    FIELD difCoDCaja AS CHARACTER
    FIELD DifCoH AS CHARACTER
    FIELD difCoHCaja AS CHARACTER
    FIELD CtaPol AS CHARACTER
    FIELD CtaHon AS CHARACTER
    FIELD CtaCos AS CHARACTER
    FIELD CtaGar AS CHARACTER
    FIELD CtaCGa AS CHARACTER
    FIELD Oper AS INTEGER
    FIELD CtaSyA AS CHARACTER
    FIELD CtasSegVida AS CHARACTER
    FIELD CtasSegVivienda AS CHARACTER
    INDEX idxTCta Agen TipP Pto.

DEFINE TEMP-TABLE CopMov_Contable LIKE Mov_Contable.

DEFINE BUFFER BAhorros FOR Ahorros.

DEFINE TEMP-TABLE tcontrol_pagos LIKE CONTROL_pagos
    FIELD marca AS LOGICAL
    FIELD Otros AS DECIMAL
    FIELD TOTAL_Cuota  AS DECIMAL
    FIELD intAnticipado AS DECIMAL.

/* oakley */

DEF BUFFER btcontrol_pagos FOR tcontrol_pagos.

DEF VAR W_CuoPag LIKE Creditos.Sdo_Capital.

DEFINE VAR W_NroDia AS INTEGER FORMAT "99".
DEFINE VAR W_NroPer AS INTEGER FORMAT "99".
DEFINE VAR P_NMeses AS INT.
DEFINE VAR P_NomPer AS CHAR FORMAT "X(15)".
DEF VAR w_TasaNominal AS DEC FORMAT "->>>>9.9999999".
DEFINE VAR W_Comple AS LOG.
DEF VAR w_saldok LIKE Creditos.Sdo_Capital.
DEF VAR P_IAC AS DEC FORMAT "->>>>>>>>>>>>>>>>9.99".
DEF VAR P_Benefi AS LOG.

DEFINE VARIABLE W1_preliquida AS DECIMAL.
DEFINE VARIABLE W1_diapreliquida AS INTEGER.
DEFINE VAR flagCtaSucyAg AS LOGICAL INITIAL FALSE.

DEFINE BUFFER bfrComprobantes FOR comprobantes.

DEFINE TEMP-TABLE tt_tpc LIKE tpc.
    
DEFINE VAR numDocAux AS INTEGER.

DEFINE VAR rowIdCheque AS ROWID.
DEFINE VAR vTime AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Pdctos
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tcontrol_pagos TPC Tpdctos

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 tcontrol_pagos.Nro_Cuota tcontrol_pagos.Fec_Vcto tcontrol_pagos.TOTAL_Cuota tcontrol_pagos.Cap_pagado tcontrol_pagos.pagos_capitalAcum tcontrol_pagos.Pagos_IntAcum tControl_pagos.causacion tControl_pagos.contingente tControl_pagos.INT_mora tcontrol_pagos.Otros tcontrol_pagos.Marca   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH tcontrol_pagos WHERE tcontrol_pagos.nit = creditos.nit                                                   AND tcontrol_pagos.num_credito = creditos.num_credito NO-LOCK BY tcontrol_pagos.Nro_Cuota
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH tcontrol_pagos WHERE tcontrol_pagos.nit = creditos.nit                                                   AND tcontrol_pagos.num_credito = creditos.num_credito NO-LOCK BY tcontrol_pagos.Nro_Cuota.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 tcontrol_pagos
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 tcontrol_pagos


/* Definitions for BROWSE Br_PC                                         */
&Scoped-define FIELDS-IN-QUERY-Br_PC TPC.TP TPC.AgeP TPC.NitP TPC.CodP TPC.NomP TPC.CtaP TPC.Cargo TPC.Gmf TPC.Abono TPC.Cpto TPC.Contra TPC.CedCont TPC.DocRef TPC.AgDest TPC.Op   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_PC   
&Scoped-define SELF-NAME Br_PC
&Scoped-define QUERY-STRING-Br_PC FOR EACH TPC NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_PC OPEN QUERY {&SELF-NAME} FOR EACH TPC NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_PC TPC
&Scoped-define FIRST-TABLE-IN-QUERY-Br_PC TPC


/* Definitions for BROWSE Br_Pdctos                                     */
&Scoped-define FIELDS-IN-QUERY-Br_Pdctos TPdctos.Agen TPdctos.CodP "999" TPdctos.NomP TPdctos.NroTarDB TPdctos.TP TPdctos.Estado TPdctos.FApert TPdctos.CtaP TPdctos.Pagare TPdctos.Cuota TPdctos.VrSdo TPdctos.IntCte TPdctos.IntCau TPdctos.Canje TPdctos.SMinimo TPdctos.FecPxDb TPdctos.ForPag TPdctos.TipoPro   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Pdctos   
&Scoped-define SELF-NAME Br_Pdctos
&Scoped-define QUERY-STRING-Br_Pdctos FOR EACH Tpdctos NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Pdctos OPEN QUERY Br_Pdctos FOR EACH Tpdctos NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Pdctos Tpdctos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Pdctos Tpdctos


/* Definitions for FRAME F_Pdctos                                       */

/* Definitions for FRAME F_Plan                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Plan ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Definitions for FRAME F_Proc                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-319 Br_Pdctos Btn_FinImp Tg_ActPagare 
&Scoped-Define DISPLAYED-OBJECTS Tg_ActPagare 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Tras_Cuentas AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON SalirPDF 
     IMAGE-UP FILE "D:/SFG/Desarrollo/Obj/imagenes/volver.bmp":U
     LABEL "" 
     SIZE 7.72 BY 1.92.

DEFINE BUTTON btnImprimirCheque 
     LABEL "Imprimir" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE txtBeneficiario AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1.08 NO-UNDO.

DEFINE BUTTON Btn_FinImp 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "&Regresar" 
     SIZE 11.14 BY 1.46 TOOLTIP "Regresar a la ventana principal de Traslados".

DEFINE RECTANGLE RECT-319
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32.29 BY 1.19.

DEFINE VARIABLE Tg_ActPagare AS LOGICAL INITIAL no 
     LABEL "Activar Pagaré ya Cancelado" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.86 BY .77 NO-UNDO.

DEFINE BUTTON Btn_Salir-3 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_Salir" 
     SIZE 11.72 BY 1.08.

DEFINE VARIABLE w_Total AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Total..." 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY .81 NO-UNDO.

DEFINE BUTTON Btn_Adic 
     LABEL "&Adicionar" 
     SIZE 11.14 BY 1.38 TOOLTIP "Adiciona registro al comprobante (Pendientes X Contabilizar)".

DEFINE BUTTON Btn_Contabilizar 
     LABEL "&Contabilizar" 
     SIZE 11.14 BY 1.38 TOOLTIP "Carga/Abona productos y Contabiliza".

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 11.14 BY 1.38
     BGCOLOR 8 .

DEFINE BUTTON Btn_Elimina 
     LABEL "&Eliminar" 
     SIZE 11.14 BY 1.38 TOOLTIP "Elimina registros adicionados (El Seleccionado de la Consulta)".

DEFINE BUTTON BUTTON-173 
     LABEL "i" 
     SIZE 3 BY .81
     FONT 0.

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 11.14 BY 1.62.

DEFINE VARIABLE Cmb_Cptes AS CHARACTER FORMAT "X(30)":U 
     LABEL "Cpte Contab" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 36.43 BY 1 TOOLTIP "Seleccione el Comprobante para los Asientos Contables"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Cptos AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 11.72 BY 1 TOOLTIP "Concepto del asiento (El valor afecta a:)"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(50)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 30.86 BY 1 TOOLTIP "Agencias Disponibles"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Difer AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Diferencia" 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY 1 TOOLTIP "Diferencia en el comprobante"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fec_corte1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Fec_corte" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE TotC AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TotH AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Abono AS DECIMAL FORMAT ">>>>>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .92 TOOLTIP "Valor Abono/Crédito"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_AgDest AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Ag. Destino" 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .77 TOOLTIP "Agencia Destino del Traslado"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_AgP AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4.29 BY .92 TOOLTIP "Agencia del pdcto/cuenta"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cargo AS DECIMAL FORMAT ">>>>>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .92 TOOLTIP "Valor Cargo/Débito"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cpto AS CHARACTER FORMAT "X(10)":U INITIAL "Sdo-Cta" 
     VIEW-AS FILL-IN 
     SIZE 8.57 BY .92 TOOLTIP "Concepto"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cta AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .92 TOOLTIP "Nro.Cuenta del Pdcto o Contable"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CtaContra AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .92 TOOLTIP "Cuenta Contable Contrapartida, Con doble-click la captura"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Descrip AS CHARACTER FORMAT "X(80)":U 
     LABEL "Descripción del Traslado" 
     VIEW-AS FILL-IN 
     SIZE 51 BY .92
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DocRef AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .92 TOOLTIP "Documento Referencia"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NitContra AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .92 TOOLTIP "Ced/Nit del Cliente para cuenta Contrapartida, Con doble-click la captura"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NitCte AS CHARACTER FORMAT "X(12)":U 
     LABEL "Ced/Nit" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .85 TOOLTIP "Ced/Nit del Cliente, Con doble Click lo Captura."
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NomCte AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 42.14 BY .85 TOOLTIP "Nombre del Cliente"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCte2 AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 34.86 BY .92 TOOLTIP "Nombre del Cliente"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomPC AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 34.57 BY .92 TOOLTIP "Descripción del Pdcto o de la Cuenta Contable"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomPC2 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY .85 TOOLTIP "Descripción del Pdcto o de la Cuenta Contable Contrapartida"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Pdcto AS INTEGER FORMAT "999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .92 TOOLTIP "Código del Producto: 0 para cuenta contable"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TGmf AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.72 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Tipo AS CHARACTER FORMAT "X(1)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .92 TOOLTIP "Tipo Pdcto/Cuenta: 0 = Contable, A = Ahorro, C = Crédito"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_Op AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Aut/ca.", 1,
"Manual", 2,
"Tras.Ag.", 3,
"Cancelación.", 4
     SIZE 45.29 BY .62 TOOLTIP "Opción para el asiento"
     BGCOLOR 17  NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 13.29 BY 9.04.

DEFINE RECTANGLE RECT-318
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 10.86 BY .85.

DEFINE RECTANGLE RECT-320
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY .81.

DEFINE VARIABLE Tg_Pdctos AS LOGICAL INITIAL no 
     LABEL "Pdctos" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.14 BY .65 TOOLTIP "Captura productos del Cliente"
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE Rs_SiNo AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Si", yes,
"No", no
     SIZE 15 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-315
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.57 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      tcontrol_pagos SCROLLING.

DEFINE QUERY Br_PC FOR 
      TPC SCROLLING.

DEFINE QUERY Br_Pdctos FOR 
      Tpdctos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 W-Tras_Cuentas _FREEFORM
  QUERY BROWSE-4 DISPLAY
      tcontrol_pagos.Nro_Cuota            COLUMN-LABEL "Nro."
      tcontrol_pagos.Fec_Vcto             COLUMN-LABEL "Fec!Vencimiento"
      tcontrol_pagos.TOTAL_Cuota          COLUMN-LABEL "Cuota" FORMAT "->>,>>>,>>9"
      tcontrol_pagos.Cap_pagado           COLUMN-LABEL "Cap!Pagado" FORMAT "->>,>>>,>>9"
      tcontrol_pagos.pagos_capitalAcum    COLUMN-LABEL "Dist!Capital" FORMAT "->>,>>>,>>9"
      tcontrol_pagos.Pagos_IntAcum        COLUMN-LABEL "Dist!Intereses" FORMAT "->>,>>>,>>9"
      tControl_pagos.causacion            COLUMN-LABEL "Causado" FORMAT "->>>,>>>,>>9"
      tControl_pagos.contingente          COLUMN-LABEL "Contingente" FORMAT "->>>,>>>,>>9"
      tControl_pagos.INT_mora             COLUMN-LABEL "Mora" FORMAT "->>>,>>>,>>9"
      tcontrol_pagos.Otros                COLUMN-LABEL "Dist!Otros" FORMAT "->>,>>>,>>9"
      tcontrol_pagos.Marca                COLUMN-LABEL "Marca"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 89 BY 9.96
         FONT 4 ROW-HEIGHT-CHARS .45 FIT-LAST-COLUMN.

DEFINE BROWSE Br_PC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_PC W-Tras_Cuentas _FREEFORM
  QUERY Br_PC NO-LOCK DISPLAY
      TPC.TP         COLUMN-LABEL "T"
      TPC.AgeP       COLUMN-LABEL "Ag."
      TPC.NitP       COLUMN-LABEL "Ced/Nit"
      TPC.CodP       COLUMN-LABEL "Pto"
      TPC.NomP       COLUMN-LABEL "Descripción"    FORM "X(20)"
      TPC.CtaP       COLUMN-LABEL "Cta/Pdcto"
      TPC.Cargo      COLUMN-LABEL "Vr.de Cargos"   FORM ">>>>,>>>,>>9"
      TPC.Gmf        COLUMN-LABEL "V.GMF Ahorro"   FORM ">>>>,>>>,>>9"
      TPC.Abono      COLUMN-LABEL "Vr.de Abonos"   FORM ">>>>,>>>,>>9"
      TPC.Cpto       COLUMN-LABEL "Concepto"
      TPC.Contra     COLUMN-LABEL "Cta-Contrap." 
      TPC.CedCont    COLUMN-LABEL "Nit Contrap" 
      TPC.DocRef     COLUMN-LABEL "Doc.Ref."
      TPC.AgDest     COLUMN-LABEL "Ag.Dest"
      TPC.Op         COLUMN-LABEL "Op"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 99.57 BY 11.85
         BGCOLOR 15 FGCOLOR 0 FONT 4
         TITLE BGCOLOR 15 FGCOLOR 0 "Detalle Asientos para Contabilizar" ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN TOOLTIP "Con Click Captura el Asiento".

DEFINE BROWSE Br_Pdctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Pdctos W-Tras_Cuentas _FREEFORM
  QUERY Br_Pdctos NO-LOCK DISPLAY
      TPdctos.Agen        COLUMN-LABEL "Ag."                   
      TPdctos.CodP        COLUMN-LABEL "Pto"                   FORM   "999"
      TPdctos.NomP        COLUMN-LABEL "Descripc.del Producto" FORMAT "X(32)"
      TPdctos.NroTarDB    COLUMN-LABEL "Tarjeta DB"            FORMAT "X(20)"
      TPdctos.TP          COLUMN-LABEL "T"                     
      TPdctos.Estado      COLUMN-LABEL "E"                     
      TPdctos.FApert      COLUMN-LABEL "Apert/Desem"           
      TPdctos.CtaP        COLUMN-LABEL "No.Cuenta"             
      TPdctos.Pagare      COLUMN-LABEL "No.Pagarè"             
      TPdctos.Cuota       COLUMN-LABEL "Valor Cuota"           FORMAT "->>>>>>,>>9.99"
      TPdctos.VrSdo       COLUMN-LABEL "Saldo Actual"          FORMAT "->>>>>>>,>>9.99"
      TPdctos.IntCte      COLUMN-LABEL "Interés x Pagar"       FORMAT "->>>>>>>,>>9.99"
      TPdctos.IntCau      COLUMN-LABEL "Interés Causado"       FORMAT "->>>>>>>,>>9.99"
      TPdctos.Canje       COLUMN-LABEL "Sdo_Canje"             FORMAT "->>>>>>>,>>9.99"
      TPdctos.SMinimo     COLUMN-LABEL "Sdo_Mínimo"            FORMAT "->>>>>>>,>>9.99"
      TPdctos.FecPxDb     COLUMN-LABEL "F-ProxPago"
      TPdctos.ForPag      COLUMN-LABEL "FP"
      TPdctos.TipoPro     COLUMN-LABEL "TP"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 109.86 BY 8.27
         BGCOLOR 15 FGCOLOR 0 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_SiImp
     Rs_SiNo AT ROW 2.62 COL 10.57 NO-LABEL
     "Desea Imprimir el documento...?" VIEW-AS TEXT
          SIZE 29.86 BY 1.08 AT ROW 1.27 COL 2.72
          BGCOLOR 17 FGCOLOR 7 
     RECT-315 AT ROW 2.35 COL 9
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 37.86 ROW 8.35
         SIZE 33.86 BY 4.35
         BGCOLOR 17 
         TITLE "Imprimir Transacción".

DEFINE FRAME frmBeneficiario
     txtBeneficiario AT ROW 1.27 COL 2 NO-LABEL WIDGET-ID 2
     btnImprimirCheque AT ROW 2.54 COL 59 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 19.14 ROW 7.5
         SIZE 74.43 BY 3.69
         TITLE "Beneficiario Cheque" WIDGET-ID 800.

DEFINE FRAME F_Pdctos
     Br_Pdctos AT ROW 1.19 COL 1
     Btn_FinImp AT ROW 9.88 COL 96
     Tg_ActPagare AT ROW 10.31 COL 32.43
     RECT-319 AT ROW 10.08 COL 30.72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.69
         SIZE 111 BY 11.54
         BGCOLOR 17 FGCOLOR 0 
         TITLE "Productos Activos del Cliente".

DEFINE FRAME F_Plan
     BROWSE-4 AT ROW 1.27 COL 2 WIDGET-ID 700
     Btn_Salir-3 AT ROW 11.5 COL 79.14 WIDGET-ID 2
     w_Total AT ROW 11.65 COL 61 COLON-ALIGNED WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 9.72 ROW 8.19
         SIZE 91.14 BY 12.69
         BGCOLOR 17 
         TITLE "Cuotas Pendientes" WIDGET-ID 100.

DEFINE FRAME F_Proc
     W_CmbOfi AT ROW 1.27 COL 9 COLON-ALIGNED
     BUTTON-173 AT ROW 1.27 COL 64
     W_NomCte AT ROW 1.27 COL 67.72 COLON-ALIGNED NO-LABEL
     W_NitCte AT ROW 1.31 COL 48.86 COLON-ALIGNED
     W_AgDest AT ROW 2.31 COL 94.85
     Fec_corte1 AT ROW 2.35 COL 49 COLON-ALIGNED WIDGET-ID 2
     Tg_Pdctos AT ROW 2.38 COL 65
     W_Tipo AT ROW 3.19 COL 2.43 NO-LABEL
     W_AgP AT ROW 3.19 COL 6.14 NO-LABEL
     W_Pdcto AT ROW 3.19 COL 10.86 NO-LABEL
     W_Cta AT ROW 3.19 COL 15.43 NO-LABEL
     W_NomPC AT ROW 3.19 COL 29.43 NO-LABEL
     Rs_Op AT ROW 3.42 COL 64.57 NO-LABEL
     W_Cpto AT ROW 5.23 COL 2.43 NO-LABEL
     W_CtaContra AT ROW 5.23 COL 11.43 NO-LABEL
     W_NitContra AT ROW 5.23 COL 22.57 COLON-ALIGNED NO-LABEL
     W_DocRef AT ROW 5.23 COL 35.72 COLON-ALIGNED NO-LABEL
     Cmb_Cptos AT ROW 5.23 COL 47 NO-LABEL
     W_Cargo AT ROW 5.23 COL 57.14 COLON-ALIGNED NO-LABEL
     W_Abono AT ROW 5.23 COL 72 COLON-ALIGNED NO-LABEL
     W_NomPC2 AT ROW 5.31 COL 88 NO-LABEL
     W_NomCte2 AT ROW 6.35 COL 22.57 COLON-ALIGNED NO-LABEL
     Cmb_Cptes AT ROW 6.35 COL 71.57 COLON-ALIGNED
     W_Descrip AT ROW 7.46 COL 57 COLON-ALIGNED
     Br_PC AT ROW 8.54 COL 1.57
     BUTTON-5 AT ROW 9 COL 102.14
     Btn_Adic AT ROW 10.81 COL 102.14 HELP
          "Adiciona el registro al comprobante (Pendientes X Contabilizar)"
     Btn_Elimina AT ROW 12.46 COL 102.14 HELP
          "Elimina registros adicionados (El Seleccionado de la Consulta)"
     Btn_Contabilizar AT ROW 14.08 COL 102.14 HELP
          "Realiza la Contabilización de los Traslados"
     Btn_Done AT ROW 15.69 COL 102.14 HELP
          "Sale del proceso de Depreciación y Ajustes"
     Difer AT ROW 21.12 COL 21.86 COLON-ALIGNED
     TotC AT ROW 21.12 COL 36.43 COLON-ALIGNED NO-LABEL
     W_TGmf AT ROW 21.12 COL 50.29 COLON-ALIGNED NO-LABEL
     TotH AT ROW 21.12 COL 65.57 COLON-ALIGNED NO-LABEL
     "Concepto  Contrapartida     Ced./Nit            Ref. /Cheq." VIEW-AS TEXT
          SIZE 44.57 BY .62 AT ROW 4.54 COL 2.72
          BGCOLOR 18 FGCOLOR 15 
     "Concepto         Valor Débitos       Valor Créditos        Descripción Contrap." VIEW-AS TEXT
          SIZE 60.86 BY .62 AT ROW 4.54 COL 49
          BGCOLOR 18 FGCOLOR 15 
     "A C C I Ó N" VIEW-AS TEXT
          SIZE 9.72 BY .54 AT ROW 2.73 COL 81
          BGCOLOR 18 FGCOLOR 15 
     "TOT.CREDITOS" VIEW-AS TEXT
          SIZE 14.29 BY .54 AT ROW 20.54 COL 67.72
          BGCOLOR 18 FGCOLOR 15 
     "TOT.DEBITOS" VIEW-AS TEXT
          SIZE 13.14 BY .54 AT ROW 20.54 COL 38.43
          BGCOLOR 18 FGCOLOR 15 
     "        Total Gmf" VIEW-AS TEXT
          SIZE 14.86 BY .54 AT ROW 20.54 COL 52.29
          BGCOLOR 18 FGCOLOR 15 
     "Tip   Ag.   Pdto  Cuenta                  Descripción" VIEW-AS TEXT
          SIZE 37.43 BY .62 AT ROW 2.54 COL 2.57
          BGCOLOR 17 FGCOLOR 0 
     RECT-314 AT ROW 8.54 COL 101.14
     RECT-318 AT ROW 2.27 COL 63.86
     RECT-320 AT ROW 3.31 COL 64
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 113.86 BY 21.12
         BGCOLOR 17 FGCOLOR 0 FONT 5.

DEFINE FRAME FormatoEfectivo
     SalirPDF AT ROW 17.42 COL 85 WIDGET-ID 6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.08
         SIZE 95 BY 19.92
         TITLE "Formato transacciones mayores al límite" WIDGET-ID 900.


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
  CREATE WINDOW W-Tras_Cuentas ASSIGN
         HIDDEN             = YES
         TITLE              = "Traslados entre Cuentas, Programa W-Tras_Cuentas.W"
         HEIGHT             = 21.15
         WIDTH              = 113.86
         MAX-HEIGHT         = 36.62
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 36.62
         VIRTUAL-WIDTH      = 182.86
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR WINDOW W-Tras_Cuentas
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FormatoEfectivo:FRAME = FRAME F_Proc:HANDLE.

/* SETTINGS FOR FRAME FormatoEfectivo
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FormatoEfectivo:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME frmBeneficiario
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME frmBeneficiario:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN txtBeneficiario IN FRAME frmBeneficiario
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME F_Pdctos
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB Br_Pdctos RECT-319 F_Pdctos */
ASSIGN 
       FRAME F_Pdctos:HIDDEN           = TRUE
       FRAME F_Pdctos:MOVABLE          = TRUE.

ASSIGN 
       Br_Pdctos:SEPARATOR-FGCOLOR IN FRAME F_Pdctos      = 18.

/* SETTINGS FOR FRAME F_Plan
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-4 1 F_Plan */
ASSIGN 
       FRAME F_Plan:HIDDEN           = TRUE
       FRAME F_Plan:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN w_Total IN FRAME F_Plan
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Proc
                                                                        */
/* BROWSE-TAB Br_PC W_Descrip F_Proc */
/* SETTINGS FOR BUTTON BUTTON-173 IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Cptos IN FRAME F_Proc
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Difer IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fec_corte1 IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TotC IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TotH IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_AgDest IN FRAME F_Proc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_AgP IN FRAME F_Proc
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX W_CmbOfi IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Cpto IN FRAME F_Proc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_Cta IN FRAME F_Proc
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_CtaContra IN FRAME F_Proc
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_NomCte IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCte2 IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomPC IN FRAME F_Proc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_NomPC2 IN FRAME F_Proc
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_Pdcto IN FRAME F_Proc
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_Tipo IN FRAME F_Proc
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME F_SiImp
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_SiImp:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Tras_Cuentas)
THEN W-Tras_Cuentas:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tcontrol_pagos WHERE tcontrol_pagos.nit = creditos.nit
                                                  AND tcontrol_pagos.num_credito = creditos.num_credito NO-LOCK BY tcontrol_pagos.Nro_Cuota.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_PC
/* Query rebuild information for BROWSE Br_PC
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TPC NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_PC */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Pdctos
/* Query rebuild information for BROWSE Br_Pdctos
     _START_FREEFORM
OPEN QUERY Br_Pdctos FOR EACH Tpdctos NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Pdctos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FormatoEfectivo
/* Query rebuild information for FRAME FormatoEfectivo
     _Query            is NOT OPENED
*/  /* FRAME FormatoEfectivo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmBeneficiario
/* Query rebuild information for FRAME frmBeneficiario
     _Query            is NOT OPENED
*/  /* FRAME frmBeneficiario */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Pdctos
/* Query rebuild information for FRAME F_Pdctos
     _Query            is NOT OPENED
*/  /* FRAME F_Pdctos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_SiImp
/* Query rebuild information for FRAME F_SiImp
     _Query            is NOT OPENED
*/  /* FRAME F_SiImp */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME FormatoEfectivo:HANDLE
       ROW             = 1
       COLUMN          = 1
       HEIGHT          = .04
       WIDTH           = 2
       WIDGET-ID       = 2
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME FormatoEfectivo:HANDLE
       ROW             = 1.69
       COLUMN          = 4.14
       HEIGHT          = 17.62
       WIDTH           = 77.29
       WIDGET-ID       = 4
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {CA8A9780-280D-11CF-A24D-444553540000} type: AcroPDF */
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {CA8A9780-280D-11CF-A24D-444553540000} type: AcroPDF */
      CtrlFrame:MOVE-BEFORE(SalirPDF:HANDLE IN FRAME FormatoEfectivo).
      CtrlFrame-2:MOVE-AFTER(CtrlFrame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Tras_Cuentas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Tras_Cuentas W-Tras_Cuentas
ON END-ERROR OF W-Tras_Cuentas /* Traslados entre Cuentas, Programa W-Tras_Cuentas.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Tras_Cuentas W-Tras_Cuentas
ON WINDOW-CLOSE OF W-Tras_Cuentas /* Traslados entre Cuentas, Programa W-Tras_Cuentas.W */
DO:
  /* This event will close the window and terminate the procedure.  */



  FIND FIRST TPC NO-ERROR.
  IF AVAIL(TPC) THEN DO:
     MESSAGE "Existen Registros pendientes por Contabilizar...?" SKIP
             "                  Desea Ignorarlos...?" 
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Salir"
             UPDATE W_RptaRet AS LOGICAL.
     IF NOT W_RptaRet THEN RETURN. 
  END.      
               
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define FRAME-NAME F_Plan
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 W-Tras_Cuentas
ON MOUSE-SELECT-DBLCLICK OF BROWSE-4 IN FRAME F_Plan
DO:
    w_total = 0.
    W_CuoPag = 0.
    W_Comple = TRUE.

    FOR EACH btcontrol_pagos WHERE btcontrol_pagos.nit = creditos.nit
                               AND btcontrol_pagos.num_credito = creditos.num_credito BY btcontrol_pagos.Nro_Cuota:
        IF btcontrol_pagos.Nro_Cuota <= tcontrol_pagos.Nro_Cuota THEN DO:
            btcontrol_pagos.Marca = YES.
            w_total = w_total + btcontrol_pagos.TOTAL_Cuota.
            W_CuoPag = W_CuoPag + 1.
        END.
        ELSE DO:
            btcontrol_pagos.Marca = NO.
            w_Comple = FALSE.
        END.
    END.

    IF creditos.cod_credito = 123 THEN
        w_comple = FALSE.

    IF w_total MODULO 100 <> 0 THEN
        w_total = TRUNCATE((w_total / 100) + 1,0) * 100.

    IF w_total >= (Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob +
                   creditos.int_difCobro + Creditos.Int_Corrientes + Creditos.Sdo_Capital - Creditos.Int_Anticipado + 
                   Creditos.Seg_Cartera + Creditos.Seg_Vida) THEN DO:
        w_total = (Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob + Creditos.Seg_Cartera + Creditos.Seg_Vida +
                   creditos.int_difCobro + Creditos.Int_Corrientes + Creditos.Sdo_Capital - Creditos.Int_Anticipado).
    END.
    ELSE DO:
        IF W_Comple THEN DO:
            w_total = (Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob + Creditos.Seg_Cartera + Creditos.Seg_Vida +
                       creditos.int_difCobro + Creditos.Int_Corrientes + Creditos.Sdo_Capital - Creditos.Int_Anticipado).
        END.
    END.

    OPEN QUERY BROWSE-4 FOR EACH tcontrol_pagos WHERE tcontrol_pagos.Agencia = TPdctos.Agen
                                                  AND tcontrol_pagos.Nit = w_nitcte
                                                  AND tcontrol_pagos.Num_Credito = integer(TPdctos.CtaP) NO-LOCK BY tcontrol_pagos.Nro_Cuota.

    DISPLAY w_total WITH FRAME f_plan.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 W-Tras_Cuentas
ON ROW-DISPLAY OF BROWSE-4 IN FRAME F_Plan
DO:
    IF tcontrol_pagos.Marca THEN DO:
        ASSIGN tcontrol_pagos.Nro_Cuota:BGCOL IN BROWSE BROWSE-4 = 12
               tcontrol_pagos.Fec_Vcto:BGCOL IN BROWSE BROWSE-4 = 12
               tcontrol_pagos.Cap_pagado:BGCOL IN BROWSE BROWSE-4 = 12
               tcontrol_pagos.TOTAL_cuota:BGCOL IN BROWSE BROWSE-4 = 12
               tcontrol_pagos.pagos_capitalAcum:BGCOL IN BROWSE BROWSE-4 = 12
               tcontrol_pagos.Pagos_IntAcum:BGCOL IN BROWSE BROWSE-4 = 12
               tcontrol_pagos.causacion:BGCOL IN BROWSE BROWSE-4 = 12
               tcontrol_pagos.contingente:BGCOL IN BROWSE BROWSE-4 = 12
               tcontrol_pagos.Int_mora:BGCOL IN BROWSE BROWSE-4 = 12
               tcontrol_pagos.Otros:BGCOL IN BROWSE BROWSE-4 = 12
               tcontrol_pagos.Marca:BGCOL IN BROWSE BROWSE-4 = 12.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_PC
&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Br_PC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_PC W-Tras_Cuentas
ON MOUSE-SELECT-CLICK OF Br_PC IN FRAME F_Proc /* Detalle Asientos para Contabilizar */
DO:
  IF AVAIL(TPC) THEN
     ASSIGN W_Cargo:SCREEN-VALUE     = STRING(TPC.Cargo)
            W_Abono:SCREEN-VALUE     = STRING(TPC.Abono)
            W_CtaContra:SCREEN-VALUE = TPC.Contra
            W_NitContra:SCREEN-VALUE = TPC.CedCont
            W_DocRef:SCREEN-VALUE    = TPC.DocRef
            W_Tipo:SCREEN-VALUE      = TPC.TP     
            W_AgP:SCREEN-VALUE       = STRING(TPC.AgeP)
            /*W_NitCte:SCREEN-VALUE    = TPC.NitP  */
            W_Pdcto:SCREEN-VALUE     = STRING(TPC.CodP)
            W_Cta:SCREEN-VALUE       = TPC.CtaP
            W_Cpto:SCREEN-VALUE      = TPC.Cpto
            Cmb_Cptos:SCREEN-VALUE   = TPC.Cpto
            Rs_Op:SCREEN-VALUE       = STRING(TPC.Op)
            W_AgDest:SCREEN-VALUE    = STRING(TPC.AgDest)
            W_NomPc:SCREEN-VALUE     = TPC.NomP
            W_NomPc
            W_Cargo
            W_Abono
            W_CtaContra
            W_NitContra
            W_DocRef
            Rs_Op
            W_Tipo  
            W_AgP   
            W_NitCte
            W_Pdcto 
            W_Cta   
            W_Cpto
            W_AgDest.  

     /*APPLY "LEAVE" TO W_NitCte.*/
     IF W_Cta NE " " AND W_Cta NE ? THEN
        APPLY "LEAVE" TO W_Cta.

     IF W_CtaContra NE " " AND W_CtaContra NE ? THEN
        APPLY "LEAVE" TO W_CtaContra.

     APPLY "LEAVE" TO W_NitContra.
     APPLY "MOUSE-SELECT-CLICK" TO Rs_Op.  

     IF TPC.TP EQ "0" OR TPC.Contra NE " " THEN DO:
        ASSIGN W_NitContra              = TPC.CedCont
               W_NitContra:SCREEN-VALUE = W_NitContra.
        APPLY "LEAVE" TO W_NitContra.
     END.

     APPLY "ENTRY" TO W_Abono.

     ASSIGN W_Cpto:SCREEN-VALUE = TPC.Cpto
            W_Cpto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Pdctos
&Scoped-define FRAME-NAME F_Pdctos
&Scoped-define SELF-NAME Br_Pdctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Pdctos W-Tras_Cuentas
ON MOUSE-SELECT-CLICK OF Br_Pdctos IN FRAME F_Pdctos
DO:
  IF AVAIL(TPdctos) AND TPdctos.TP EQ "C" AND 
     (TPdctos.Estado EQ 3 OR TPdctos.Estado EQ 5) THEN DO:
     MESSAGE "Está Segura(o) de Activar el Pagaré Seleccionado...?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Activar Pagaré" 
           UPDATE W_SiAct AS LOGICAL.

     IF W_SiAct THEN DO:
        FIND FIRST Creditos WHERE Creditos.Agencia     EQ TPdctos.Agen  
                              AND Creditos.Nit         EQ W_NitCte           
                              AND Creditos.Cod_Credito EQ TPdctos.CodP
                              AND Creditos.Num_Credito EQ INTEG(TPdctos.CtaP) NO-ERROR.
        ASSIGN Creditos.Estado         = 2
               Creditos.Fec_CanceTotal = ?.
        FIND CURRENT Creditos NO-LOCK NO-ERROR.
     END.

     RUN LlenaPdctos.
  END.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Pdctos W-Tras_Cuentas
ON MOUSE-SELECT-DBLCLICK OF Br_Pdctos IN FRAME F_Pdctos
DO:
    IF AVAIL(Tpdctos) THEN DO:
        CLOSE QUERY Br_Pdctos.

        FRAME F_Pdctos:VISIBLE = FALSE.

        IF TPdctos.TP = "C" THEN DO:
            FIND FIRST creditos WHERE creditos.nit = w_nitcte
                                  AND creditos.num_credito = integer(TPdctos.CtaP) NO-LOCK NO-ERROR.

            EMPTY TEMP-TABLE tcontrol_pagos.

            RUN controlpagos.

            VIEW FRAME F_plan.

            FRAME F_Plan:SENSITIVE = TRUE.
            OPEN QUERY BROWSE-4 FOR EACH tcontrol_pagos WHERE tcontrol_pagos.Agencia = TPdctos.Agen
                                                          AND tcontrol_pagos.Nit = w_nitcte
                                                          AND tcontrol_pagos.Num_Credito = integer(TPdctos.CtaP) NO-LOCK BY tcontrol_pagos.Nro_Cuota.
        END.
        ELSE
            FRAME F_Proc:SENSITIVE = TRUE.
        
        ASSIGN W_AgP:SCREEN-VALUE     = STRING(TPdctos.Agen)
               W_Tipo:SCREEN-VALUE    = TPdctos.TP
               W_Pdcto:SCREEN-VALUE   = STRING(TPdctos.CodP)
               W_NomPC:SCREEN-VALUE   = TPdctos.NomP           
               W_Cta:SCREEN-VALUE     = TPdctos.CtaP
               W_AgP
               W_Tipo
               W_Pdcto
               W_NomPC
               W_Cta
               W_TipoPro = TPdctos.TipoPro.

        APPLY "LEAVE" TO W_Tipo.
        APPLY "ENTRY" TO W_Abono.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frmBeneficiario
&Scoped-define SELF-NAME btnImprimirCheque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImprimirCheque W-Tras_Cuentas
ON CHOOSE OF btnImprimirCheque IN FRAME frmBeneficiario /* Imprimir */
DO:
    DEFINE VAR W_Monto1 AS CHARACTER FORMAT "X(70)".
    DEFINE VAR W_Monto2 AS CHARACTER FORMAT "X(70)".
    DEFINE VAR W_Monto3 AS CHARACTER FORMAT "X(70)".
    DEFINE VAR C_Ciudad AS CHARACTER.

    FIND FIRST mov_contable WHERE rowid(mov_contable) = rowIdCheque NO-LOCK NO-ERROR.
    IF AVAILABLE mov_contable THEN DO:
        RUN MontoEsc.r (INPUT mov_contable.db + mov_Contable.cr,
                        INPUT 0,
                        OUTPUT W_Cadena).
    
        RUN PartirValor IN W_Manija (INPUT W_Cadena,
                                     INPUT 60,
                                     OUTPUT W_Monto1,
                                     OUTPUT W_Monto2,
                                     OUTPUT W_Monto3).

        /*MESSAGE w_monto1 SKIP
                w_monto2 SKIP
                txtBeneficiario:SCREEN-VALUE IN FRAME frmBeneficiario SKIP
                c_ciudad SKIP
                mov_Contable.db + mov_contable.cr SKIP
                mov_contable.comprobante SKIP
                mov_contable.num_documento
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

        RUN f-cheque.r(INPUT W_monto1,
                       INPUT W_monto2,
                       INPUT txtBeneficiario:SCREEN-VALUE IN FRAME frmBeneficiario,
                       INPUT c_ciudad,
                       INPUT mov_Contable.db + mov_contable.cr,
                       INPUT "",
                       INPUT STRING(mov_contable.comprobante),
                       INPUT "CR",
                       INPUT STRING(mov_contable.num_documento)).
    
        HIDE FRAME frmBeneficiario.

        ASSIGN FRAME F_SiImp:SENSITIVE = TRUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Btn_Adic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Adic W-Tras_Cuentas
ON CHOOSE OF Btn_Adic IN FRAME F_Proc /* Adicionar */
DO:
    DEFI VAR W_SiErr AS LOG.
    DEF VAR tValor AS INT.

    IF UPPER(W_Tipo) = "A" AND W_TipoPro = 3 THEN DO:
        FIND FIRST ahorros WHERE ahorros.cue_ahorros = W_Cta
                             AND ahorros.nit = W_NitCte
                             AND ahorros.tip_ahorro = 3 NO-LOCK NO-ERROR.
        IF AVAILABLE(ahorros) THEN DO:
            IF Ahorros.Fec_Vencimiento GT W_fecha AND Ahorros.Fec_Prorroga = ? AND DECIMAL(w_cargo:SCREEN-VALUE) GT 0 THEN
                MESSAGE "La Fecha de Vencimiento del Producto CDAT no se ha cumplido," SKIP
                        "por tanto no se permite Erogaciones o Cancelaciones."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        END.
    END.

    IF W_Descrip LE " " THEN DO:
        MESSAGE "El Detalle de la Tx. es Obligatorio."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    IF W_NeedNit AND W_NitContra:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "La partida contiene una cuenta contable" SKIP
                "que requiere NIT. Por favor ingrese el NIT"
            VIEW-AS ALERT-BOX.

        APPLY "entry" TO W_NitContra IN FRAME F_Proc.
        RETURN.
    END.

    IF (SUBSTRING(W_Cta,1,6) EQ "259595" OR SUBSTRING(W_CtaContra,1,6) EQ "259595") AND w_abono GT 0 THEN
        W_swGMF = W_swGMF + 1.

    IF SUBSTRING(W_Cta,1,4) EQ "1904" OR SUBSTRING(W_Cta,1,4) EQ "2704" OR
       SUBSTRING(W_CtaContra,1,4) EQ "1904" OR
       SUBSTRING(W_CtaContra,1,4) EQ "2704" THEN DO:
        IF W_NitContra GT "100" OR SUBSTRING(W_NitContra,1,1) NE "0" THEN DO:
            IF w_tipo:SCREEN-VALUE NE "A" AND w_tipo:SCREEN-VALUE NE "C" THEN DO:
                MESSAGE "SyA Deben tener Ced/Nit Auxiliar la Agencia."
                    VIEW-AS ALERT-BOX ERROR.

                APPLY  "ENTRY" TO W_NitContra.
                RETURN.
            END.
        END.
    END.

    W_NeedNit = NO.

    ASSIGN Porc_Base = 0
           Cta_Base = "".

    IF Rs_Op EQ 3 AND W_Tipo NE "0" THEN DO:
        RUN Valida_AgDestino NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN.

        IF W_Agencia NE W_AgP THEN DO:
            MESSAGE "La ACCION es Traslado entre Agencias...," SKIP
                    "No puede realizar el Traslado...SOLO SI ES Desde la Agencia Origen." SKIP
                    "...Revise por favor."
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.

        FIND FIRST TPC WHERE TPC.Op NE 3 NO-ERROR.
        IF AVAIL(TPC) THEN DO:
            MESSAGE "La ACCION es Traslado entre Agencias...Ya existe otra ACCION," SKIP
                    "No puede realizar el Traslado en el mismo Comprobante." SKIP
                    "...Revise por favor."
                VIEW-AS ALERT-BOX ERROR.
            
            RETURN.
        END.
    END.

    IF Rs_Op NE 3 THEN DO:
        FIND FIRST TPC WHERE TPC.Op EQ 3 NO-ERROR.
        IF AVAIL(TPC) THEN DO:
            MESSAGE "La ACCION NO es Traslado entre Agencias...Ya existe ésta ACCION," SKIP
                    "No puede realizar el Traslado en el mismo Comprobante." SKIP
                    "...Revise por favor."
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.

        IF W_Cargo LE 0 AND W_Abono LE 0 THEN DO:
            MESSAGE "Falta Valor para el Asiento..." SKIP
                    "...Revise por favor."
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.
    END.

    IF W_Tipo EQ "0" THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_Cta
                             AND Cuentas.Tipo EQ 2
                             AND Cuentas.Estado EQ 1
                             AND Cuentas.Id_NoMvto EQ FALSE NO-LOCK NO-ERROR.
        IF NOT AVAIL(Cuentas) THEN DO:
            MESSAGE "La Operación es Contable, La Cuenta debe Existir y no puede ser de Producto..."
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.
    END.

    IF W_CtaContra GT " " THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_CtaContra
                             AND Cuentas.Tipo EQ 2
                             AND Cuentas.Estado EQ 1
                             AND Cuentas.Id_NoMvto EQ FALSE NO-LOCK NO-ERROR.
        IF NOT AVAIL(Cuentas) THEN DO:
            MESSAGE "La Cuenta-Contrapartida debe Existir y no puede ser de Producto..."
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.
        ELSE DO:
            IF Cuentas.Id_Base AND Cuentas.Cod_Base EQ "" THEN DO:
                MESSAGE "La cuenta de contrapartida tiene configurado el id. de base de retención" SKIP
                        "sin embargo no se encuentra asociado ninguna base de retención a la cuenta"
                    VIEW-AS ALERT-BOX.

                RETURN.
            END.

            FIND FIRST Base_Ret WHERE Base_Ret.Cod_Base EQ Cuentas.Cod_Base
                                  AND Base_Ret.Estado EQ 1
                                  AND Base_Ret.Porcentaje NE 0.000000 NO-LOCK NO-ERROR.
            IF AVAILABLE Base_Ret THEN
                ASSIGN Porc_Base = Base_Ret.Porcentaje
                       Cta_Base = Base_Ret.Cuenta
                       Nom_Base = Base_Ret.Nombre.
        END.
    END.

    IF W_NitContra GT " " THEN DO:
        FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitContra NO-LOCK NO-ERROR.
        IF NOT AVAIL(Clientes) THEN DO:
            MESSAGE "El Nit de la Contrapartida debe Existir Activo en Clientes..."
                VIEW-AS ALERT-BOX ERROR.

            RETURN.
        END.
        ELSE DO:
            IF clientes.estado <> 1 THEN
                MESSAGE "El Cliente/Asociado identificado con la cédula/NIT #" W_NitCte "no se encuentra activo." SKIP
                        "Este es un mensaje informativo!"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.

    IF W_Tipo EQ "0" THEN
        FIND FIRST TPC WHERE TPC.TP EQ W_Tipo
                         AND TPC.AgeP EQ W_AgP
                         AND TPC.NitP EQ W_NitContra
                         AND TPC.CodP EQ W_Pdcto
                         AND TPC.CtaP EQ W_Cta NO-ERROR.
    ELSE
        FIND FIRST TPC WHERE TPC.TP EQ W_Tipo
                         AND TPC.AgeP EQ W_AgP
                         AND TPC.NitP EQ W_NitCte
                         AND TPC.CodP EQ W_Pdcto
                         AND TPC.CtaP EQ W_Cta NO-ERROR.

    IF AVAIL(TPC) AND TPC.Op NE Rs_Op THEN DO:
        MESSAGE "Ya Existe Asiento para esta Cta Y/O Producto, con ACCION Diferente..." SKIP
                "Solo se aceptan con la Misma Acción...Revise por favor."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    IF Rs_Op NE 3 AND W_Tipo NE "0" THEN DO:
        RUN HallaPdcto NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN.

        RUN ValidaPdcto NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN.
    END.

    IF W_Tipo EQ "0" THEN
        FIND FIRST TPC WHERE TPC.TP EQ W_Tipo
                         AND TPC.AgeP EQ W_AgP
                         AND TPC.NitP EQ W_NitContra
                         AND TPC.CodP EQ W_Pdcto
                         AND TPC.CtaP EQ W_Cta
                         AND TPC.Cpto EQ W_Cpto
                         AND TPC.DocRef EQ W_DocRef NO-ERROR.
    ELSE
        FIND FIRST TPC WHERE TPC.TP EQ W_Tipo
                         AND TPC.AgeP EQ W_AgP
                         AND TPC.NitP EQ W_NitCte
                         AND TPC.CodP EQ W_Pdcto
                         AND TPC.CtaP EQ W_Cta
                         AND TPC.Cpto EQ W_Cpto NO-ERROR.

    IF NOT AVAIL(TPC) THEN
        CREATE TPC.

    ASSIGN TPC.TP = W_Tipo
           TPC.AgeP = W_AgP
           TPC.NitP = W_NitCte
           TPC.CodP = W_Pdcto
           TPC.NomP = W_NomPC
           TPC.CtaP = W_Cta
           TPC.Cpto = W_Cpto
           TPC.Cargo = W_Cargo
           TPC.Abono = W_Abono
           TPC.Contra = W_CtaContra
           TPC.CedCont = W_NitContra
           TPC.DocRef = W_DocRef
           TPC.Op = Rs_Op
           TPC.AgDest = W_AgDest
           TPC.Descrip = W_Descrip.

    IF (W_Tipo EQ "2" OR W_Tipo EQ "C") THEN DO:
        FIND FIRST Creditos WHERE Creditos.Nit EQ TPC.NitP
                              AND Creditos.Cod_Credito EQ TPC.CodP
                              AND Creditos.Num_Credito EQ integer(TPC.CtaP)
                              AND Creditos.Abogado NO-LOCK NO-ERROR.

        /*se marca la tabla temporal de control pagos*/
        FOR EACH tcontrol_pagos WHERE tcontrol_pagos.Nit = TPC.NitP
                                  AND tcontrol_pagos.Num_Credito = integer(TPC.CtaP):
            tcontrol_pagos.Marca = FALSE.
        END.

        tvalor = w_abono.

        FIND FIRST creditos WHERE creditos.nit = TPC.NitP AND creditos.num_credito = integer(TPC.CtaP) NO-LOCK NO-ERROR.

        IF tValor >= (Creditos.Seg_Cartera + Creditos.Com_Bruta + Creditos.Com_Adicional + Creditos.Honorarios + Creditos.Costas + Creditos.Polizas +
                      Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob + creditos.int_difCobro + Creditos.Int_Corrientes + Creditos.Sdo_Capital -
                      Creditos.Int_Anticipado) THEN DO:
            FOR EACH tcontrol_pagos WHERE tcontrol_pagos.Nit = TPC.NitP
                                      AND tcontrol_pagos.Num_Credito = integer(TPC.CtaP) BY tcontrol_pagos.Nro_Cuota:
                tcontrol_pagos.Marca = TRUE.
            END.
        END.
        ELSE DO:
            FOR EACH tcontrol_pagos WHERE tcontrol_pagos.Nit = TPC.NitP
                                      AND tcontrol_pagos.Num_Credito = integer(TPC.CtaP) BY tcontrol_pagos.Nro_Cuota:
                tValor = tvalor - tcontrol_pagos.TOTAL_Cuota.
                tcontrol_pagos.Marca = TRUE.

                IF tvalor < 0 THEN
                    LEAVE.
            END.
        END.
    END.

    IF Rs_op EQ 3 AND Tpc.Tp EQ "A" AND (W_Pdcto EQ 5 OR W_Pdcto EQ 9) THEN
        Tpc.CamAge = YES.

    IF W_Tipo EQ "0" THEN
        TPC.NitP = W_NitContra.

    IF TPC.TP EQ "A" AND Rs_Op NE 3 THEN
        TPC.TipA = Ahorros.Tip_Ahorro.

    IF TPC.Op NE 3 AND TPC.TP EQ "A" AND Tpc.CodP NE 23 AND W_Cpte NE 31 AND TPC.Cpto EQ "SdoCta" AND TPC.TipA LE 2 AND TPC.Cargo GT 0 THEN
        TPC.Gmf = ROUND(TPC.Cargo * .004,0).

    IF TPC.TP EQ "A" THEN DO:
        IF pro_ahorros.id_Gmf = 2 THEN
            TPC.Gmf = 0.
    END.

    IF W_AgDest EQ 0 THEN
        TPC.AgDest = W_AgP.

    RUN QUERY_TPC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Contabilizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Contabilizar W-Tras_Cuentas
ON CHOOSE OF Btn_Contabilizar IN FRAME F_Proc /* Contabilizar */
DO:
    DEFINE VAR TGmf LIKE TPC.Gmf INITIAL 0.

    vTime = TIME.

    flagCtaSucyAg = FALSE.

    IF W_Descrip <= " " THEN DO:
        MESSAGE "La descripción de la transacción es indispensable."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    EMPTY TEMP-TABLE tempCtas.
    EMPTY TEMP-TABLE CopMov_Contable.

    W_MismoCliente = NO.

    RUN DetectaMismoCliente.

    FIND FIRST TPC NO-ERROR.
    IF NOT AVAILABLE(TPC) THEN DO:
        MESSAGE "No existen asientos para contabilizar... Proceso cancelado."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    RUN Valida NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "La configuración contable presentó errores... Proceso cancelado."
            VIEW-AS ALERT-BOX ERROR.

        RETURN.
    END.

    FIND FIRST Comprobantes WHERE Comprobantes.Agencia = W_Agencia
                              AND Comprobantes.Comprobante = W_Cpte
                              AND Comprobantes.Estado = 1 NO-ERROR.

    SESSION:SET-WAIT-STATE("GENERAL").

    RUN Contabilizar NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "La contabilización presentó errores... Proceso cancelado."
            VIEW-AS ALERT-BOX ERROR.

        SESSION:SET-WAIT-STATE("").
        RETURN.
    END.
    ELSE DO:
        W_swGMF = 0.

        VIEW FRAME F_SiImp.
        ASSIGN FRAME F_Proc:SENSITIVE = FALSE.
        SESSION:SET-WAIT-STATE("").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Tras_Cuentas
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:
  FIND FIRST TPC NO-ERROR.
  IF AVAIL(TPC) THEN DO:
     MESSAGE "Existen Registros pendientes por Contabilizar...?" SKIP
             "                  Desea Ignorarlos...?" 
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Salir"
             UPDATE W_RptaRet AS LOGICAL.
     IF NOT W_RptaRet THEN RETURN. 
  END.
    
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Elimina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Elimina W-Tras_Cuentas
ON CHOOSE OF Btn_Elimina IN FRAME F_Proc /* Eliminar */
DO:
  /*FIND FIRST TPC WHERE TPC.TP    EQ W_Tipo
                   AND TPC.AgeP  EQ W_AgP
                   AND TPC.NitP  EQ W_NitCte
                   AND TPC.CodP  EQ W_Pdcto
                   AND TPC.CtaP  EQ W_Cta
                   AND TPC.Cpto  EQ W_Cpto NO-ERROR.*/
  APPLY "Mouse-Select-Click" TO Br_Pc.
  IF AVAIL(TPC) THEN DO:
     MESSAGE "Desea Eliminar el Registro Seleccionado...?" SKIP
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Eliminar"
             UPDATE W_RptaRet AS LOGICAL.
     IF NOT W_RptaRet THEN RETURN. 
     IF (SUBSTRING(W_Cta,1,6) EQ "259595" OR SUBSTRING(W_CtaContra,1,6) EQ "259595") AND
         w_abono GT 0 THEN W_swGMF = W_swGMF - 1.
     DELETE TPC.

     RUN QUERY_Tpc.
  END.
  ELSE MESSAGE "No Existe el Registro Seleccionado para Eliminarlo...?" SKIP
               VIEW-AS ALERT-BOX ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Pdctos
&Scoped-define SELF-NAME Btn_FinImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FinImp W-Tras_Cuentas
ON CHOOSE OF Btn_FinImp IN FRAME F_Pdctos /* Regresar */
DO:
    ASSIGN FRAME F_Pdctos:VISIBLE = FALSE.

    IF TPdctos.TP = "C" THEN
        FRAME F_Plan:SENSITIVE = TRUE.
    ELSE
        FRAME F_Proc:SENSITIVE = TRUE.
        
    APPLY "ENTRY" TO W_Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Plan
&Scoped-define SELF-NAME Btn_Salir-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir-3 W-Tras_Cuentas
ON CHOOSE OF Btn_Salir-3 IN FRAME F_Plan /* Btn_Salir */
DO: 
  HIDE FRAME F_plan.
  ASSIGN FRAME F_Proc:SENSITIVE = TRUE.
  w_Abono = w_total.
  DISPLAY w_Abono WITH FRAME f_Proc.
  /*APPLY "entry" TO w_CuoPag.*/
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir-3 W-Tras_Cuentas
ON MOUSE-SELECT-CLICK OF Btn_Salir-3 IN FRAME F_Plan /* Btn_Salir */
DO:
  APPLY "CHOOSE" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME BUTTON-173
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-173 W-Tras_Cuentas
ON CHOOSE OF BUTTON-173 IN FRAME F_Proc /* i */
DO:
  DEFINE VARIABLE w_nitw LIKE clientes.nit.
  RUN p-Buscanit.r (OUTPUT W_NitW).
  IF W_NitW NE " " THEN W_NitCte:SCREEN-VALUE = W_nitW.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Tras_Cuentas
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Cptes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Cptes W-Tras_Cuentas
ON VALUE-CHANGED OF Cmb_Cptes IN FRAME F_Proc /* Cpte Contab */
DO:
  ASSIGN W_Cpte = INTEG(SUBSTRING(Cmb_Cptes:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Cptos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Cptos W-Tras_Cuentas
ON VALUE-CHANGED OF Cmb_Cptos IN FRAME F_Proc
DO:
    ASSIGN W_Cpto:SCREEN-VALUE = Cmb_Cptos:SCREEN-VALUE
           W_Cpto.

    IF W_Tipo = "A" THEN DO:
        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro = W_Pdcto NO-LOCK NO-ERROR.
        IF Pro_Ahorros.Tip_Ahorro = 4 THEN DO:
            ASSIGN Cmb_Cptos:SENSITIVE  = FALSE WHEN AVAIL(Pro_Ahorros).

            ASSIGN W_Cpto = "SdoCta"
                   W_Cpto:SCREEN-VALUE = "SdoCta"
                   Cmb_Cptos:SCREEN-VALUE = "SdoCta".
        END.
    END.

    FIND FIRST TPC WHERE TPC.TP = W_Tipo
                     AND TPC.AgeP = W_AgP
                     AND TPC.NitP = W_NitCte
                     AND TPC.CodP = W_Pdcto
                     AND TPC.CtaP = W_Cta
                     AND TPC.Cpto = W_Cpto NO-ERROR.
    IF AVAIL(TPC) THEN
        ASSIGN W_Cargo:SCREEN-VALUE = STRING(TPC.Cargo)
               W_Abono:SCREEN-VALUE = STRING(TPC.Abono)
               W_CtaContra:SCREEN-VALUE = TPC.Contra
               W_NitContra:SCREEN-VALUE = TPC.CedCont
               W_DocRef:SCREEN-VALUE = TPC.DocRef
               W_Cargo
               W_Abono
               W_CtaContra
               W_NitContra
               W_DocRef.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Fec_corte1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Fec_corte1 W-Tras_Cuentas
ON LEAVE OF Fec_corte1 IN FRAME F_Proc /* Fec_corte */
DO:
  ASSIGN fec_corte1 .
  ASSIGN W1_diapreliquida = fec_corte1 - TODAY .
 /* MESSAGE w1_diapreliquida VIEW-AS ALERT-BOX.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_Op
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Op W-Tras_Cuentas
ON MOUSE-SELECT-CLICK OF Rs_Op IN FRAME F_Proc
DO:
    DEFI VAR W_SiErr AS LOG INIT FALSE.

    ASSIGN Rs_Op
           W_AgDest:SENSITIVE = FALSE
           Cmb_Cptos:SENSITIVE = TRUE
           W_Cargo:SENSITIVE = TRUE
           W_Abono:SENSITIVE = TRUE.

    IF W_Tipo EQ "0" THEN
        ASSIGN W_Cpto = "SdoCta"
               W_Cpto:SCREEN-VALUE = "SdoCta"
               Cmb_Cptos:SENSITIVE = FALSE
               W_Pdcto:SCREEN-VALUE = "0"
               W_Pdcto.
    ELSE DO:
        IF Rs_Op EQ 3 THEN DO:
            ASSIGN W_AgDest:SENSITIVE = TRUE
                   W_Cpto = "SdoCta"
                   W_Cpto:SCREEN-VALUE = "SdoCta"
                   Cmb_Cptos:SENSITIVE = FALSE
                   W_Cargo:SENSITIVE = FALSE
                   W_Abono:SENSITIVE = FALSE
                   W_Cargo:SCREEN-VALUE = "0.00"
                   W_Abono:SCREEN-VALUE = "0.00"
                   W_Cargo
                   W_Abono.

            APPLY "ENTRY" TO W_AgDest.
            RETURN NO-APPLY.
        END.
        ELSE
            IF (Rs_Op EQ 1 OR Rs_Op EQ 4) AND (W_Tipo EQ "2" OR W_Tipo EQ "C") THEN DO:
                ASSIGN W_Cpto = "SdoCta"
                       W_Cpto:SCREEN-VALUE = "SdoCta"
                       Cmb_Cptos:SENSITIVE = FALSE
                       W_Cargo:SENSITIVE = FALSE.
                
                IF Rs_Op EQ 4 THEN DO:
                    RUN HallaPdcto NO-ERROR.
                    IF NOT ERROR-STATUS:ERROR THEN
                        ASSIGN W_Abono = Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar +
                                         Creditos.Int_MoraDifCob + Creditos.int_difcobro + Creditos.Int_Corrientes + Creditos.Sdo_Capital -
                                         Creditos.Int_Anticipado
                               W_Abono:SCREEN-VALUE = STRING(W_Abono).
                END.
            END.
            ELSE
                IF (W_Tipo EQ "2" OR W_Tipo EQ "C") THEN
                    ASSIGN W_Cpto = "Int-Ctes"
                           W_Cpto:SCREEN-VALUE = "Int-Ctes"
                           Cmb_Cptos = "Int-Ctes"
                           Cmb_Cptos:SCREEN-VALUE = "Int-Ctes"
                           Cmb_Cptos:SENSITIVE = TRUE
                           W_Cargo:SENSITIVE = TRUE
                           W_Abono:SENSITIVE = TRUE.
                ELSE
                    IF Rs_Op EQ 4 AND (W_Tipo EQ "1" OR W_Tipo EQ "A") THEN DO:
                        RUN HallaPdcto NO-ERROR.
                        IF NOT ERROR-STATUS:ERROR THEN DO:
                            ASSIGN W_Cpto = "SdoCta"
                                   W_Cpto:SCREEN-VALUE = "SdoCta"
                                   W_Abono:SCREEN-VALUE = "0"
                                   W_Abono = 0
                                   W_Cargo = Ahorros.Sdo_Disponible
                                   W_Cargo:SCREEN-VALUE = STRING(W_Cargo).
                        END.
                    END.
    END.

    IF AVAILABLE TPdctos THEN DO:
        IF TPdctos.TipoPro EQ 2 AND TPdctos.CodP EQ 221 AND W_Tipo:SCREEN-VALUE NE "0" THEN
            ASSIGN W_Cargo:SENSITIVE = FALSE.
    END.
    

    W-Tras_Cuentas:MOVE-TO-TOP().
    APPLY "ENTRY" TO W_NitCte.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_SiImp
&Scoped-define SELF-NAME Rs_SiNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_SiNo W-Tras_Cuentas
ON MOUSE-SELECT-CLICK OF Rs_SiNo IN FRAME F_SiImp
DO:
    ASSIGN Rs_SiNo.

    DEFINE VAR W_Rpta AS LOGICAL.

    FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante EQ Comprobantes.Comprobante
                            AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                            AND Mov_Contable.Fec_Contable EQ W_Fecha
                            AND Mov_Contable.Usuario EQ W_Usuario NO-LOCK BREAK BY Mov_Contable.Agencia:
        IF FIRST-OF(Mov_Contable.Agencia) THEN DO:
            FIND FIRST Formatos WHERE Formatos.Agencia EQ Mov_Contable.Agencia
                                  AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato NO-LOCK NO-ERROR.
            IF AVAILABLE(Formatos) THEN
                RUN VALUE(Formatos.Nom_Proceso) (INPUT Comprobantes.comprobante,
                                                 INPUT Comprobantes.Secuencia,
                                                 INPUT Comprobantes.Secuencia,
                                                 INPUT Mov_Contable.Agencia,
                                                 INPUT w_fecha).
            ELSE
                RUN MostrarMensaje IN W_Manija (INPUT 345,
                                                OUTPUT W_Rpta).
        END.

        IF (SUBSTRING(mov_contable.cuenta,1,6) = "111005" OR SUBSTRING(mov_contable.cuenta,1,6) = "111010") AND mov_contable.cr > 0 THEN DO:
            MESSAGE "Desea Imprimir Ahora el Cheque?"
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Impresión de Cheque" UPDATE choice2 AS LOGICAL.

            IF CHOICE2 THEN DO:
                rowIdCheque = ROWID(mov_contable).

                VIEW FRAME FrmBeneficiario.

                FIND FIRST clientes WHERE clientes.nit = mov_contable.nit NO-LOCK NO-ERROR.
                IF AVAILABLE clientes THEN DO:
                    txtBeneficiario:SCREEN-VALUE IN FRAME frmBeneficiario = clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2.
                END.
            END.
        END.
    END.

    SESSION:SET-WAIT-STATE("GENERAL").

    FIND CURRENT PlanPagos NO-LOCK NO-ERROR.
    FIND CURRENT Creditos NO-LOCK NO-ERROR.
    FIND CURRENT Ahorros NO-LOCK NO-ERROR.

    FOR EACH TPC:
        DELETE TPC.
    END.

    FOR EACH TPdctos:
        DELETE TPdctos.
    END.

    FOR EACH CopMov_Contable:
        DELETE CopMov_Contable.
    END.

    FOR EACH TempCtas:
        DELETE TempCtas.
    END.

    CLOSE QUERY Br_Pdctos.
    CLOSE QUERY Br_PC.

    HIDE FRAME F_SiImp.

    ASSIGN Rs_SiNo:SCREEN-VALUE = "No"
           Rs_SiNo.

    ASSIGN FRAME F_Proc:SENSITIVE = TRUE
           W_NitContra = ""
           W_NitContra:SCREEN-VALUE = ""
           W_CtaContra = ""
           W_CtaContra:SCREEN-VALUE = ""
           W_DocRef:SCREEN-VALUE = ""
           W_NomCte2:SCREEN-VALUE = ""
           W_Descrip:SCREEN-VALUE = ""
           W_Descrip = ""
           W_NomCte2
           W_DocRef
           W_TGmf = 0
           W_TGmf:SCREEN-VALUE = "0.00"
           Difer:SCREEN-VALUE = "0.00"
           TotH:SCREEN-VALUE = "0.00"
           TotC:SCREEN-VALUE = "0.00"
           Difer
           TotH
           TotC.

    SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FormatoEfectivo
&Scoped-define SELF-NAME SalirPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SalirPDF W-Tras_Cuentas
ON CHOOSE OF SalirPDF IN FRAME FormatoEfectivo
DO:
  HIDE FRAME FormatoEfectivo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Pdctos
&Scoped-define SELF-NAME Tg_ActPagare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_ActPagare W-Tras_Cuentas
ON MOUSE-SELECT-CLICK OF Tg_ActPagare IN FRAME F_Pdctos /* Activar Pagaré ya Cancelado */
DO:
  DEFI VAR W_SiErr AS LOG.

  ASSIGN Tg_ActPagare.

  IF Tg_ActPagare THEN DO:
     RUN P-ValiDarTrans IN W_Manija (OUTPUT W_SiErr,OUTPUT W_Autorizo).
     IF W_SiErr THEN DO:
        FOR EACH TPdctos: DELETE TPdctos. END.
        CLOSE QUERY Br_Pdctos.

        FOR EACH Creditos WHERE Creditos.Nit      EQ W_NitCte
                           AND (Creditos.Estado   EQ 3
                               OR Creditos.Estado EQ 5) NO-LOCK BY Creditos.Cod_Credito:
           FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito
                                     AND Pro_Creditos.Estado      EQ 1 NO-LOCK NO-ERROR. 
           CREATE TPdctos.
           ASSIGN TPdctos.CodP = Creditos.Cod_Credito
             TPdctos.TP        = "C"
             TPdctos.Estado    = Creditos.Estado
             TPdctos.CtaP      = STRING(Creditos.Num_Credito)
             TPdctos.Pagare    = Creditos.Pagare
             TPdctos.Agen      = Creditos.Agencia
             TPdctos.FApert    = Creditos.Fec_Desemb
             TPdctos.Cuota     = Creditos.Cuota
             TPdctos.VrSdo     = Creditos.Honorarios    + Creditos.Costas + Creditos.Polizas +
                                Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob +          
                                Creditos.Int_DifCobro  + Creditos.Int_Corrientes +
                                Creditos.Sdo_Capital   - Creditos.Int_Anticipado
             TPdctos.NomP      = Pro_Creditos.Nom_Produc
             TPdctos.FecPxDb   = Creditos.Fec_Pago
             TPdctos.ForPag    = Creditos.FOR_Pago. 
        END.
    
        OPEN QUERY Br_Pdctos FOR EACH TPdctos.

        MESSAGE "Seleccione el Crédito YA Cancelado para Activarlo..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
     END.        
  END.
  ELSE RUN LlenaPdctos.
  
  ASSIGN Tg_ActPagare                = FALSE
         Tg_ActPagare:SCREEN-VALUE   = "No".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_ActPagare W-Tras_Cuentas
ON VALUE-CHANGED OF Tg_ActPagare IN FRAME F_Pdctos /* Activar Pagaré ya Cancelado */
DO:
  ASSIGN Tg_ActPagare.

  APPLY "Mouse-Select-Click" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Tg_Pdctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_Pdctos W-Tras_Cuentas
ON VALUE-CHANGED OF Tg_Pdctos IN FRAME F_Proc /* Pdctos */
DO:
    ASSIGN Tg_Pdctos.

    IF Tg_Pdctos THEN DO:
        IF w_NitCte <> "" THEN DO:
            RUN LlenaPdctos.

            ASSIGN FRAME F_Proc:SENSITIVE = FALSE
                   FRAME F_Pdctos:VISIBLE = TRUE.
        END.
        ELSE
            MESSAGE "No ha digitado ningún documento de identidad"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
    
  ASSIGN Tg_Pdctos:SCREEN-VALUE = "No"
         Tg_Pdctos
         Rs_Op:SCREEN-VALUE     = "1"
         Rs_Op.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Abono
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Abono W-Tras_Cuentas
ON LEAVE OF W_Abono IN FRAME F_Proc
DO:
    ASSIGN W_Abono.

    IF W_Tipo NE "0" AND W_Abono GT 0 THEN DO:
        ASSIGN W_Cargo              = 0
               W_Cargo:SCREEN-VALUE = "0".

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_AgDest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_AgDest W-Tras_Cuentas
ON LEAVE OF W_AgDest IN FRAME F_Proc /* Ag. Destino */
DO:
  ASSIGN W_AgDest.
  RUN Valida_AgDestino.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_AgP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_AgP W-Tras_Cuentas
ON LEAVE OF W_AgP IN FRAME F_Proc
DO:
  ASSIGN W_AgP.

  IF W_AgP EQ 0 THEN
     ASSIGN W_AgP:SCREEN-VALUE = STRING(W_Agencia)
            W_AgP.
  ELSE DO:
     FIND FIRST Agencias WHERE Agencias.Agencia EQ W_AgP
                           AND Agencias.Estado  EQ 1 NO-LOCK NO-ERROR.
     IF NOT AVAIL(Agencias) THEN DO:
        MESSAGE "La agencia no está disponible...No se permite la operación para está Agencia." 
                VIEW-AS ALERT-BOX ERROR.

        ASSIGN W_AgP:SCREEN-VALUE = STRING(W_Agencia)
               W_AgP.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Cargo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cargo W-Tras_Cuentas
ON LEAVE OF W_Cargo IN FRAME F_Proc
DO:
  ASSIGN W_Cargo.
    
  IF W_Tipo NE "0" AND W_Cargo GT 0 THEN
     ASSIGN W_Abono              = 0
            W_Abono:SCREEN-VALUE = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Cta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Cta W-Tras_Cuentas
ON LEAVE OF W_Cta IN FRAME F_Proc
DO:
    DEFINE VAR numCheque AS DECIMAL INITIAL 0.
    DEFINE VAR aux AS INTEGER.

    ASSIGN W_Cta.

    DEFI VAR W_SiErr AS LOG INITIAL TRUE.

    IF W_Tipo EQ "0" THEN DO:
        FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_Cta
                             AND Cuentas.Tipo EQ 2
                             AND Cuentas.Estado EQ 1
                             AND Cuentas.Id_NoMvto EQ FALSE NO-LOCK NO-ERROR.
        IF AVAIL(Cuentas) THEN DO:
            ASSIGN W_NomPC:SCREEN-VALUE = Cuentas.Nombre
                   W_NomPC.

            W_NeedNit = NO.

            IF Cuentas.Id_Nit THEN
                W_NeedNit = YES.
        END.
        ELSE DO:
            ASSIGN FRAME F_Proc:SENSITIVE = FALSE.

            RUN C-Cuentas.R (OUTPUT W_Cta,
                             OUTPUT W_NomPC,
                             OUTPUT W_Nat,
                             OUTPUT W_Ctr,
                             INPUT 2).

            ASSIGN FRAME F_Proc:SENSITIVE = TRUE.

            FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ W_Cta
                                 AND Cuentas.Tipo EQ 2
                                 AND Cuentas.Estado EQ 1
                                 AND Cuentas.Id_NoMvto EQ FALSE NO-LOCK NO-ERROR.
            IF AVAIL(Cuentas) THEN DO:
                ASSIGN W_NomPC:SCREEN-VALUE = Cuentas.Nombre
                       W_Cta:SCREEN-VALUE = Cuentas.Cuenta /* W_Cta */
                       W_NomPC.

                W_NeedNit = NO.

                IF Cuentas.Id_Nit THEN
                    W_NeedNit = YES.
            END.
            ELSE DO:
                MESSAGE "La cuenta contable no puede ser de Producto y deber ser" SKIP
                        "de movimiento y Activa en el PUC...Ingrese una correcta por favor."
                    VIEW-AS ALERT-BOX.

                ASSIGN W_Cta = ""
                       W_Cta:SCREEN-VALUE = "".
            END.
        END.

        IF SUBSTRING(W_Cta,1,1) EQ "8" OR SUBSTRING(W_Cta,1,1) EQ "9" THEN DO:
            MESSAGE "Cuentas de Orden No pueden operar como Contrapartidas," SKIP
                    "Operan para Int-Contingentes en forma Automatica."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            ASSIGN W_Cta = ""
                   W_Cta:SCREEN-VALUE = "".
        END.

        IF SUBSTRING(W_Cta,1,4) EQ "1904" OR SUBSTRING(W_Cta,1,4) EQ "2704" THEN DO:
            ASSIGN W_NitContra = ""
                   W_NitContra:SCREEN-VALUE = ""
                   W_NomCte2:SCREEN-VALUE = ""
                   W_NomCte2.

            APPLY "ENTRY" TO W_NitContra.
        END.

        /* Consecutivo del cheque */
        IF AVAILABLE cuentas AND cuentas.cod_flujoEfec = "D" AND Cuentas.Car_Efectivo = 3 AND cuentas.estado = 1 THEN DO:
            FOR EACH mov_contable WHERE mov_contable.agencia = w_agencia
                                    AND mov_contable.cuenta = cuentas.cuenta
                                    AND mov_contable.fec_contable <= w_fecha NO-LOCK BY mov_contable.fec_contable DESCENDING
                                                                                     BY mov_contable.num_documento DESCENDING:
                aux = INTEGER(mov_contable.doc_referencia) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN.
                ELSE DO:
                    numCheque = aux.
                    LEAVE.
                END.
            END.

            W_DocRef:SCREEN-VALUE = STRING(numCheque + 1).
        END.
    END.
    ELSE
        RUN HallaPdcto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CtaContra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaContra W-Tras_Cuentas
ON LEAVE OF W_CtaContra IN FRAME F_Proc
DO:
    ASSIGN W_CtaContra.

    IF W_CtaContra = ? THEN
        W_CtaContra = "".

    IF SUBSTRING(W_CtaContra,1,1) EQ "8" OR SUBSTRING(W_CtaContra,1,1) EQ "9" THEN DO:
        MESSAGE "Cuentas de Orden No pueden operar como Contrapartidas," SKIP
                "Operan para Int-Contingentes en forma Automatica."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        ASSIGN W_CtaContra = ""
               W_CtaContra:SCREEN-VALUE = "".
    END.

    IF W_CtaContra LE " " THEN
        APPLY "Mouse-select-Dblclick" TO SELF.

    IF Rs_Op EQ 3 THEN
        ASSIGN W_CtaContra = ""
               W_CtaContra:SCREEN-VALUE = "".

    FIND Cuentas WHERE Cuentas.Cuenta EQ W_CtaContra
                   AND Cuentas.Tipo EQ 2
                   AND Cuentas.Estado EQ 1
                   AND Cuentas.Id_NoMvto EQ FALSE NO-LOCK NO-ERROR.
    IF AVAIL(Cuentas) THEN DO:
        ASSIGN W_NomPC2:SCREEN-VALUE = Cuentas.Nombre
               W_CtaContra:SCREEN-VALUE = W_CtaContra
               W_NomPC2.

        W_NeedNit = NO.

        IF Cuentas.Id_Nit THEN
            W_NeedNit = YES.
    END.
    ELSE
        IF W_CtaContra GT " " THEN DO:
            MESSAGE "La Cuenta debe ser No-Pdcto y Existir Activa...Revise por favor."
                VIEW-AS ALERT-BOX.

            ASSIGN W_CtaContra = ""
                   W_CtaContra:SCREEN-VALUE = "".
        END.

    IF W_CtaContra GT " " AND W_NitContra LE " " THEN
        ASSIGN W_NitContra = W_NitCte
               W_NitContra:SCREEN-VALUE = W_NitCte
               W_NomCte2:SCREEN-VALUE = W_NomCte:SCREEN-VALUE
               W_NomCte2.

    IF SUBSTRING(W_CtaContra,1,4) EQ "1904" OR SUBSTRING(W_CtaContra,1,4) EQ "2704" THEN DO:
        ASSIGN W_NitContra = ""
               W_NitContra:SCREEN-VALUE = ""
               W_NomCte2:SCREEN-VALUE = ""
               W_NomCte2.

        APPLY "ENTRY" TO W_NitContra.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CtaContra W-Tras_Cuentas
ON MOUSE-SELECT-DBLCLICK OF W_CtaContra IN FRAME F_Proc
DO:
    ASSIGN FRAME F_Proc:SENSITIVE = FALSE.

    RUN C-Cuentas.R (OUTPUT W_CtaContra,
                     OUTPUT W_NomPC2,
                     OUTPUT W_Nat,
                     OUTPUT W_Ctr,
                     INPUT 2).

    ASSIGN FRAME F_Proc:SENSITIVE = TRUE
           W_CtaContra:SCREEN-VALUE = W_CtaContra.

    IF W_CtaContra:SCREEN-VALUE = ? THEN
        W_CtaContra:SCREEN-VALUE = "".

    ASSIGN w_ctaContra.

    APPLY "LEAVE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Descrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Descrip W-Tras_Cuentas
ON LEAVE OF W_Descrip IN FRAME F_Proc /* Descripción del Traslado */
DO:
  ASSIGN W_Descrip.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_DocRef
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_DocRef W-Tras_Cuentas
ON LEAVE OF W_DocRef IN FRAME F_Proc
DO:
   ASSIGN W_DocRef.                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NitContra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitContra W-Tras_Cuentas
ON LEAVE OF W_NitContra IN FRAME F_Proc
DO:
    ASSIGN W_NitContra
           W_NomCte2:SCREEN-VALUE = "".

    IF W_CtaContra LE " " AND W_Cta LE " " THEN
        ASSIGN W_NitContra = " "
               W_NitContra:SCREEN-VALUE = " "
               W_NomCte2:SCREEN-VALUE = ""
               W_NomCte2.
    ELSE DO:
        FIND FIRST Clientes WHERE Clientes.Nit EQ W_NitContra NO-LOCK NO-ERROR.
        IF AVAIL(Clientes) THEN
            ASSIGN W_NitContra:SCREEN-VALUE = W_NitContra
                   W_NomCte2:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)
                   W_NomCte2.
        ELSE DO:
            IF W_NitContra GT " " THEN DO:
                MESSAGE "El Nit no existe activo en clientes, Revise por favor."
                    VIEW-AS ALERT-BOX.

                ASSIGN W_NitContra = " "
                       W_NitContra:SCREEN-VALUE = " "
                       W_NomCte2:SCREEN-VALUE = ""
                       W_NomCte2.
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitContra W-Tras_Cuentas
ON MOUSE-SELECT-DBLCLICK OF W_NitContra IN FRAME F_Proc
DO:
   ASSIGN FRAME F_Proc:SENSITIVE = FALSE.                                                           
                                                                                                 
   RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                     OUTPUT W_NitContra, OUTPUT W_NomCte2, OUTPUT W_NomCte2, OUTPUT W_Age).       
                                                                                                    
   ASSIGN FRAME F_Proc:SENSITIVE   = TRUE
          W_NitContra:SCREEN-VALUE = W_NitContra.
    
   APPLY "LEAVE" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NitCte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCte W-Tras_Cuentas
ON LEAVE OF W_NitCte IN FRAME F_Proc /* Ced/Nit */
DO:
    ASSIGN W_NitCte.

    /*IF W_NitCte = "10249571" THEN DO:
        MESSAGE "Para este número de identificación no se permite" SKIP
                "ningún tipo de operación. Por favor, consulte con" SKIP
                "el Administrador del Sistema."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        W_NitCte = ''.
        W_NitCte:SCREEN-VALUE = ''.
        W_NomCte:SCREEN-VALUE = ''.

        LEAVE.
    END.*/

    W_NomCte:SCREEN-VALUE = "".

    FIND FIRST Clientes WHERE Clientes.Nit = W_NitCte NO-LOCK NO-ERROR.
    IF NOT AVAILABLE(Clientes) THEN DO:
        ASSIGN FRAME F_Proc:SENSITIVE = FALSE.

        RUN C-Clientes.R (INPUT 1,
                          INPUT W_Agencia,
                          OUTPUT W_NitCte,
                          OUTPUT W_NomCte,
                          OUTPUT W_NomCte,
                          OUTPUT W_Age).

        ASSIGN FRAME F_Proc:SENSITIVE = TRUE.

        FIND FIRST Clientes WHERE Clientes.Nit = W_NitCte NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN DO:
            W_NitCte:SCREEN-VALUE = W_NitCte.
            W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre).
        END.
        ELSE
            MESSAGE "El Número de identificación debe estar registrado en el Sistema."
                VIEW-AS ALERT-BOX.
    END.
    ELSE DO:
        IF clientes.estado <> 1 THEN
            MESSAGE "El Cliente/Asociado identificado con número" W_NitCte "no se encuentra activo." SKIP
                    "Este es un mensaje informativo!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

        W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre).

        IF Clientes.Fec_fallecido <> ? THEN
            MESSAGE "Este cliente aparece como FALLECIDO fecha:" clientes.Fec_fallecido
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    ASSIGN fec_corte1:SCREEN-VALUE = string(TODAY)
           fec_corte1 = TODAY
           W1_diapreliquida = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCte W-Tras_Cuentas
ON MOUSE-SELECT-DBLCLICK OF W_NitCte IN FRAME F_Proc /* Ced/Nit */
DO:
   ASSIGN FRAME F_Proc:SENSITIVE = FALSE.                                                           

   RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                     OUTPUT W_NitCte, OUTPUT W_NomCte, OUTPUT W_NomCte, OUTPUT W_Age).

   ASSIGN W_NitCte:SCREEN-VALUE  = W_NitCte
          FRAME F_Proc:SENSITIVE = TRUE.

   APPLY "Entry" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Tipo W-Tras_Cuentas
ON LEAVE OF W_Tipo IN FRAME F_Proc
DO:
    ASSIGN W_Tipo
           Rs_Op:SCREEN-VALUE = "1"
           Rs_Op.

    IF W_Tipo NE "A" AND W_Tipo NE "C" AND W_Tipo NE "1" AND W_Tipo NE "2" THEN
        ASSIGN W_Tipo:SCREEN-VALUE = "0"
               W_Tipo.

    IF W_Tipo EQ "0" THEN DO:
        ASSIGN W_Cpto = "SdoCta"
               W_Cpto:SCREEN-VALUE = "SdoCta"
               Cmb_Cptos:SENSITIVE = FALSE
               W_Pdcto:SCREEN-VALUE = "0"
               W_Pdcto
               W_AgP:SCREEN-VALUE = STRING(W_Agencia)
               W_AgP
               W_Cta:SCREEN-VALUE = ""
               W_Cta.

        ASSIGN Rs_Op:SCREEN-VALUE = "1".
        ASSIGN Rs_Op.
        ASSIGN W_Cargo:SENSITIVE = TRUE.

        Cmb_Cptos:ADD-LAST("SdoCta").
    END.
    ELSE
        IF W_Tipo EQ "1" OR W_Tipo EQ "A" THEN DO:
            ASSIGN W_Cpto = "SdoCta"
                   W_Cpto:SCREEN-VALUE = "SdoCta"
                   Cmb_Cptos:SENSITIVE = TRUE
                   Cmb_Cptos:LIST-ITEMS = ""
                   W_Tipo:SCREEN-VALUE = "A"
                   W_Tipo.

            Cmb_Cptos:ADD-LAST("SdoCta").
            Cmb_Cptos:ADD-LAST("IntXPagar").
            Cmb_Cptos:ADD-LAST("IntCausado").
            Cmb_Cptos:ADD-LAST("RetFuente").
        END.
        ELSE
            IF W_Tipo EQ "2" OR W_Tipo EQ "C" THEN DO:
                ASSIGN W_Cpto = "SdoCta"
                       W_Cpto:SCREEN-VALUE = "SdoCta"
                       Cmb_Cptos:SENSITIVE = TRUE
                       Cmb_Cptos:LIST-ITEMS = ""
                       W_Tipo:SCREEN-VALUE = "C"
                       W_Tipo.

                Cmb_Cptos:ADD-LAST("SdoCta").
                Cmb_Cptos:ADD-LAST("Int-Ctes").
                /*Cmb_Cptos:ADD-LAST("IntAnticip").*/
                Cmb_Cptos:ADD-LAST("IntDifCob").
                Cmb_Cptos:ADD-LAST("IntMora").
                Cmb_Cptos:ADD-LAST("Costas").
                Cmb_Cptos:ADD-LAST("Honorarios").
                Cmb_Cptos:ADD-LAST("Polizas").
                Cmb_Cptos:ADD-LAST("SegVida").
                Cmb_Cptos:ADD-LAST("SegVivienda").
            END.

    Cmb_Cptos:SCREEN-VALUE = "SdoCta".

    APPLY "MOUSE-SELECT-CLICK" TO Rs_Op.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Pdctos
&Scoped-define BROWSE-NAME BROWSE-4
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Tras_Cuentas 


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

  FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  
  FIND FIRST Agencias WHERE Agencias.Agencia EQ W_Agencia
                        AND Agencias.Estado  EQ 1  NO-LOCK NO-ERROR.
  IF NOT AVAIL(Agencias) THEN DO:
     MESSAGE "La Agencia no está disponible para realizar Transacciones..." VIEW-AS ALERT-BOX ERROR.

     APPLY "CLOSE":U TO THIS-PROCEDURE.
     RETURN NO-APPLY.
  END.

  W_CmbOfi:ADD-LAST("000 CONSOLIDADO").
  
  ASSIGN W_OfiIni       = W_Agencia
         W_OfiFin       = W_Agencia.
                  
  FOR EACH Agencias WHERE Agencias.Estado  EQ 1
                      AND Agencias.Agencia GT 0 NO-LOCK:
      W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(25)")).

      IF Agencias.Agencia EQ W_Agencia THEN
         W_CmbOfi:SCREEN-VALUE = (STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(25)")).
  END. 

  FOR EACH Comprobantes WHERE Comprobantes.Agencia         EQ W_Agencia
                          AND Comprobantes.Estado          EQ 1
                         /* AND NOT Comprobantes.Id_Efectivo */      NO-LOCK:
     
    IF  comprobantes.comprobante EQ 1  OR  comprobantes.comprobante EQ 5 OR
        comprobantes.comprobante EQ 2  OR
        comprobantes.comprobante EQ 21 OR 
        comprobantes.comprobante EQ 31 OR
        comprobantes.comprobante EQ 8 THEN DO:
      Cmb_Cptes:ADD-LAST(STRING(Comprobantes.Comprobante,"999") + "-" + 
                         STRING(Comprobantes.Nombre,"X(25)")).
      ASSIGN Cmb_Cptes:SCREEN-VALUE = STRING(Comprobantes.Comprobante,"999") + "-" + 
                                      STRING(Comprobantes.Nombre,"X(25)")
             W_Cpte = INTEG(SUBSTRING(Cmb_Cptes:SCREEN-VALUE,1,3)).
    END.
  END.
  
  W-Tras_Cuentas:MOVE-TO-TOP().

  APPLY "LEAVE" TO W_Tipo.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN 
     WAIT-FOR CLOSE OF THIS-PROCEDURE.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActualizarDiasDeAtraso W-Tras_Cuentas 
PROCEDURE ActualizarDiasDeAtraso :
DEFINE VAR saldoProyectado AS DECIMAL.
DEFINE VAR fechaAux AS DATE.
DEFINE VAR diasAsumar AS INTEGER.
DEFINE VAR valorCredito AS DECIMAL.
DEFINE VAR flagFecPago AS LOGICAL.
DEFINE VAR diasPeriodo AS INTEGER.

creditos.val_atraso = 0.
creditos.cuo_atraso = 0.
creditos.dias_atraso = 0.

/* Días de atraso */
IF creditos.fec_pago < w_fecha THEN
    creditos.dias_atraso = w_fecha - creditos.fec_pago.
ELSE
    creditos.dias_atraso = 0.

/* Valor del atraso - Cuotas atrasadas */
IF creditos.dias_atraso > 0 THEN DO:
    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND CONTROL_pagos.id_pdoMes < 2
                             AND CONTROL_pagos.fec_Vcto <= w_fecha NO-LOCK:
        creditos.val_atraso = creditos.val_atraso + CONTROL_pagos.pagos_capitalAcum.
        creditos.cuo_atraso = creditos.cuo_atraso + 1.
    END.
END.

RUN HallarPeriodo IN W_ManFin(INPUT Creditos.Per_Pago,
                              INPUT Creditos.Plazo,
                              OUTPUT diasPeriodo,
                              OUTPUT P_NMeses,
                              OUTPUT W_NroPer,
                              OUTPUT P_NomPer).
/* Saldo proyectado */
saldoProyectado = creditos.monto.

FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                         AND CONTROL_pagos.num_credito = creditos.num_credito
                         AND CONTROL_pagos.fec_vcto <= w_fecha NO-LOCK BY CONTROL_pagos.fec_vcto:
    saldoProyectado = saldoProyectado - CONTROL_pagos.pagos_capitalAcum.
END.

IF saldoProyectado < creditos.sdo_capital OR creditos.dias_atraso > 0 THEN
    creditos.sdo_proyectado = saldoProyectado.
ELSE
    creditos.sdo_proyectado = creditos.sdo_capital.

IF creditos.val_atraso LE 0 THEN
    Creditos.Cod_Califica = 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calc_GmfAhorro W-Tras_Cuentas 
PROCEDURE Calc_GmfAhorro :
/*------------------------------------------------------------------------------
  Purpose:       
------------------------------------------------------------------------------*/
           IF TPC.CodP EQ 23 OR W_Cpte EQ 31 THEN
              RETURN.

           DEFI VAR RowidTpc AS ROWID.
           DEFI VAR P_ImpAplic LIKE Ahorros.Sdo_Dispon INIT 0.

           
           FIND FIRST Ahorros WHERE Ahorros.Nit        EQ TPC.NitP                                        
                                AND Ahorros.Cod_Ahorro EQ TPC.CodP                                    
                                AND Ahorros.Cue_Ahorro EQ TPC.CtaP                                    
                                AND Ahorros.Estado     EQ 1 NO-ERROR.  
           ASSIGN W_VrConsig = 0
                  W_MismoCte = FALSE
                  RowidTpc   = ROWID(TPC).

           /*RUN MismoCte. Retiros validar si es traslado del mismo nit y mismo pdcto*/
           FOR EACH TPC NO-LOCK WHERE TPC.TP   EQ "A"
                               AND TPC.NitP    EQ Ahorros.Nit
                               AND TPC.Abono   GT 0
                               AND TPC.TipA    EQ Ahorros.Tip_Ahorro:     
               ASSIGN W_VrConsig  = W_VrConsig + TPC.Abono
                      W_MismoCte  = TRUE.
           END.

           FIND TPC WHERE ROWID(TPC) EQ RowidTpc NO-ERROR.              
           
           IF (NOT W_MismoCte OR W_VrConsig LT VrCargos[1]) THEN DO: 
               W_VrConsig = VrCargos[1] - W_VrConsig.  
              
              IF W_VrConsig GT 0 AND ahorros.cod_ahorro NE 216 THEN DO: /* JJMP 18 mayo 2007 */
                 IF W_swGMF GT 0 THEN. /* jjmp 16/10/2007 */
                 ELSE
                    RUN RutGMF.R (INPUT  FALSE,
                                    W_Agencia,
                                    Ahorros.Agencia,
                                    1,
                                    Ahorros.Cod_Ahorro,
                                    Ahorros.Nit,
                                    INPUT  Ahorros.Cue_Ahorro,
                                    W_OpCgoAho,
                                    W_VrConsig,
                                    INPUT  1,
                                    INPUT  "1",
                                    "Nada",
                                    0,
                                    0,
                                    OUTPUT P_ImpAplic) NO-ERROR.                 
              END.

              IF  Ahorros.Sdo_Disponible - VrCargos[1] - P_ImpAplic LT 0
              AND P_ImpAplic GT 0 THEN 
                  TPC.Cargo = TPC.Cargo + (Ahorros.Sdo_Disponible - VrCargos[1] - P_ImpAplic).
           END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cobro_Deducible W-Tras_Cuentas 
PROCEDURE Cobro_Deducible :
CREATE CopMov_Contable.
    ASSIGN CopMov_Contable.Agencia = W_Agencia
           CopMov_Contable.Cuenta = Cta_Base
           CopMov_Contable.Nit = w_nitcontra:SCREEN-VALUE IN FRAME F_Proc
           CopMov_Contable.Fec_Contable = W_Fecha
           CopMov_Contable.Comentario = "CobroDeducible: " + Nom_Base
           CopMov_Contable.Usuario = W_Usuario
           CopMov_Contable.Cen_Costos = W_Cencosgral
           CopMov_Contable.Destino = W_Agencia
           CopMov_Contable.Comprobante = Comprobantes.Comprobante
           CopMov_Contable.Num_Documento = Comprobantes.Secuencia
           CopMov_Contable.Fec_Grabacion = TODAY
           CopMov_Contable.Hora = TIME
           CopMov_Contable.Estacion = W_Estacion
           CopMov_Contable.Cr = Tpc.Cargo * (Porc_Base / 100).

    IF Rs_Op = 2 THEN
        Copmov_contable.comentario = "TM/"+ CopMov_contable.comentario.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompletaSyA W-Tras_Cuentas 
PROCEDURE CompletaSyA :
DEFI VAR W_OtraAg LIKE Agencias.Agencia.
DEFI VAR W_CtaSyA LIKE Cuentas.Cuenta.

IF VrDist GT 0 THEN
    FIND FIRST CopMov_Contable WHERE CopMov_Contable.Agencia EQ TPC.AgDest
                                 AND CopMov_Contable.Cuenta EQ TempCtas.CtaSyA
                                 AND CopMov_Contable.Nit EQ STRING(W_Agencia,"999")
                                 AND CopMov_Contable.Cr GT 0 NO-ERROR.
ELSE
    FIND FIRST CopMov_Contable WHERE CopMov_Contable.Agencia EQ TPC.AgDest
                                 AND CopMov_Contable.Cuenta EQ TempCtas.CtaSyA
                                 AND CopMov_Contable.Nit EQ STRING(W_Agencia,"999")
                                 AND CopMov_Contable.Db GT 0 NO-ERROR.

IF NOT AVAIL(CopMov_Contable) THEN
    CREATE CopMov_Contable.

numDocAux = comprobantes.secuencia.

IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
    FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                 AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
    IF AVAILABLE bfrComprobantes THEN DO:
        bfrComprobantes.secuencia = bfrComprobantes.secuencia + 1.
        numDocAux = bfrComprobantes.secuencia.
        flagCtaSucyAg = TRUE.
    END.
END.
ELSE DO:
    IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
        FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                     AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
        IF AVAILABLE bfrComprobantes THEN
            numDocAux = bfrComprobantes.secuencia.
    END.
END.

ASSIGN CopMov_Contable.Agencia = TPC.AgDest
       CopMov_Contable.Cuenta = TempCtas.CtaSyA
       CopMov_Contable.Nit = STRING(W_Agencia,"999")
       CopMov_Contable.Fec_Contable = W_Fecha
       CopMov_Contable.Comentario = TPC.Descrip
       CopMov_Contable.Usuario = W_Usuario
       CopMov_Contable.Cen_Costos = W_Cencosgral
       CopMov_Contable.Destino = W_Agencia
       CopMov_Contable.Comprobante = Comprobantes.Comprobante
       CopMov_Contable.Num_Documento = /*Comprobantes.Secuencia*/ numDocAux
       CopMov_Contable.Fec_Grabacion = TODAY
       CopMov_Contable.Hora = TIME
       CopMov_Contable.Estacion = W_Estacion
       W_OtraAg = TPC.AgDest.

IF TPC.AgDest <> w_agencia THEN
    copMov_Contable.enlace = STRING(comprobantes.secuencia).

IF Rs_Op = 2 THEN
    Copmov_contable.comentario = "TM/"+ CopMov_contable.comentario.


IF VrDist GT 0 THEN
    CopMov_Contable.Cr = CopMov_Contable.Cr + VrDist.
ELSE
    CopMov_Contable.Db = CopMov_Contable.Db + (VrDist * -1).

W_CtaSyA = TempCtas.CtaSyA.

FIND FIRST TempCtas WHERE TempCtas.Agen EQ W_Agencia
                      AND TempCtas.TipP EQ TPdctos.TP
                      AND TempCtas.Pto EQ TPdctos.CodP NO-LOCK NO-ERROR.

IF VrDist GT 0 THEN
    FIND FIRST CopMov_Contable WHERE CopMov_Contable.Agencia EQ W_Agencia
                                 AND CopMov_Contable.Cuenta EQ W_CtaSyA
                                 AND CopMov_Contable.Nit EQ STRING(W_OtraAg,"999")
                                 AND CopMov_Contable.Db GT 0 NO-ERROR.
ELSE
    FIND FIRST CopMov_Contable WHERE CopMov_Contable.Agencia EQ W_Agencia
                                 AND CopMov_Contable.Cuenta EQ W_CtaSyA
                                 AND CopMov_Contable.Nit EQ STRING(W_OtraAg,"999")
                                 AND CopMov_Contable.Cr GT 0 NO-ERROR.

IF NOT AVAIL(CopMov_Contable) THEN
    CREATE CopMov_Contable.
    
ASSIGN CopMov_Contable.Agencia = W_Agencia
       CopMov_Contable.Cuenta = W_CtaSyA
       CopMov_Contable.Nit = STRING(W_OtraAg,"999")
       CopMov_Contable.Fec_Contable = W_Fecha
       CopMov_Contable.Comentario = TPC.Descrip
       CopMov_Contable.Usuario = W_Usuario
       CopMov_Contable.Cen_Costos = W_Cencosgral
       CopMov_Contable.Destino = W_Agencia
       CopMov_Contable.Comprobante = Comprobantes.Comprobante
       CopMov_Contable.Num_Documento = Comprobantes.Secuencia
       CopMov_Contable.Fec_Grabacion = TODAY
       CopMov_Contable.Hora = TIME
       CopMov_Contable.Estacion = W_Estacion.

IF Rs_Op = 2 THEN
    Copmov_contable.comentario = "TM/"+ CopMov_contable.comentario.

IF VrDist GT 0 THEN
    CopMov_Contable.Db = CopMov_Contable.Db + VrDist.
ELSE
    CopMov_Contable.Cr = CopMov_Contable.Cr + (VrDist * -1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar W-Tras_Cuentas 
PROCEDURE Contabilizar :
DEFINE VAR TotT AS DECIMAL.
DEFINE VAR TCr AS DECIMAL.
DEFINE VAR TDb AS DECIMAL.
DEFINE VAR Neto AS DECIMAL.
DEFINE VAR W_SiErr AS LOGICAL.
DEFINE VAR i AS INTEGER.

Contabilizando:
DO TRANS ON ERROR UNDO Contabilizando:
    FIND FIRST mov_contable WHERE mov_contable.agencia = comprobantes.agencia
                              AND mov_contable.comprobante = comprobantes.comprobante
                              AND mov_contable.num_documento = comprobantes.secuencia NO-LOCK NO-ERROR.
    IF AVAILABLE mov_contable OR comprobantes.secuencia = 0 THEN
        Comprobantes.Secuencia = Comprobantes.Secuencia + 1.
    
    FIND CURRENT Comprobantes NO-LOCK NO-ERROR.

    W_NoGmf = FALSE.

    FOR EACH TPC WHERE TPC.TP EQ "A" BREAK BY TPC.NitP BY TPC.TipA:
        Neto = Neto + (TPC.Cargo - TPC.Abono).

        IF LAST-OF(TPC.TipA) THEN DO:
            IF Neto = 0 THEN
                W_NoGmf = TRUE.

            Neto = 0.
        END.
    END.

    /* Distribuir aportes y ahorro permanente */
    EMPTY TEMP-TABLE tt_tpc.

    FOR EACH tpc WHERE tpc.tp = "A"
                   AND ((tpc.tipA = 4 AND tpc.codP = 2) OR (tpc.tipA = 2 AND tpc.codP = 3))
                   AND tpc.op = 1
                   AND tpc.cpto = "SdoCta"
                   AND tpc.abono > 0
                   AND tpc.op = 1 /* Automático */ :
        IF tpc.tipA = 4 AND tpc.codP = 2 THEN DO:
            FIND FIRST ahorros WHERE ahorros.nit = tpc.nitP
                                 AND ahorros.tip_ahorro = 2
                                 AND ahorros.cod_ahorro = 3
                                 AND ahorros.estado = 1 NO-LOCK NO-ERROR.
            IF AVAILABLE ahorros THEN DO:
                CREATE tt_tpc.
                BUFFER-COPY tpc TO tt_tpc.
                tt_tpc.tipA = 2.
                tt_tpc.codP = 3.
                tt_tpc.ctaP = ahorros.cue_ahorros.
                tt_tpc.abono = ROUND(tt_tpc.abono * 0.8,0).
                tpc.abono = tpc.abono - tt_tpc.abono.
            END.
        END.

        IF tpc.tipA = 2 AND tpc.codP = 3 THEN DO:
            FIND FIRST ahorros WHERE ahorros.nit = tpc.nitP
                                 AND ahorros.tip_ahorro = 4
                                 AND ahorros.cod_ahorro = 2
                                 AND ahorros.estado = 1 NO-LOCK NO-ERROR.
            IF AVAILABLE ahorros THEN DO:
                CREATE tt_tpc.
                BUFFER-COPY tpc TO tt_tpc.
                tt_tpc.tipA = 4.
                tt_tpc.codP = 2.
                tt_tpc.ctaP = ahorros.cue_ahorros.
                tt_tpc.abono = ROUND(tt_tpc.abono * 0.2,0).
                tpc.abono = tpc.abono - tt_tpc.abono.
            END.
        END.
    END.

    FOR EACH tt_tpc NO-LOCK:
        CREATE tpc.
        BUFFER-COPY tt_tpc TO tpc.
    END.

    FOR EACH TPC BREAK BY TPC.AgeP BY TPC.TP
                                   BY TPC.NitP
                                   BY TPC.CodP
                                   BY TPC.CtaP
                                   BY TPC.Cpto:
        IF TPC.DocRef LE " " THEN
            TPC.DocRef = STRING(Comprobantes.Secuencia).

        IF FIRST-OF(TPC.CtaP) THEN
            ASSIGN VrCargos = 0
                   VrAbonos = 0.

        IF TPC.TP NE "0" THEN DO:
            IF TPC.Cpto EQ "SdoCta" THEN
                ASSIGN VrCargos[1] = VrCargos[1] + TPC.Cargo
                       VrAbonos[1] = VrAbonos[1] + TPC.Abono.
            ELSE
                IF TPC.Cpto EQ "Int-Ctes" OR TPC.Cpto EQ "IntCausado" THEN
                    ASSIGN VrCargos[2] = VrCargos[2] + TPC.Cargo
                           VrAbonos[2] = VrAbonos[2] + TPC.Abono.
                ELSE
                    IF TPC.Cpto EQ "IntMora" OR TPC.Cpto EQ "IntXPagar" THEN
                        ASSIGN VrCargos[3] = VrCargos[3] + TPC.Cargo
                               VrAbonos[3] = VrAbonos[3] + TPC.Abono.
                    ELSE
                        IF TPC.Cpto EQ "IntAnticip" OR TPC.Cpto EQ "RetFuente" THEN
                            ASSIGN VrCargos[4] = VrCargos[4] + TPC.Cargo
                                   VrAbonos[4] = VrAbonos[4] + TPC.Abono.
                        ELSE
                            IF TPC.Cpto EQ "IntDifCob" THEN
                                ASSIGN VrCargos[5] = VrCargos[5] + TPC.Cargo
                                       VrAbonos[5] = VrAbonos[5] + TPC.Abono.
                            ELSE
                                IF TPC.Cpto EQ "Costas" THEN
                                    ASSIGN VrCargos[6] = VrCargos[6] + TPC.Cargo
                                           VrAbonos[6] = VrAbonos[6] + TPC.Abono.
                                ELSE
                                    IF TPC.Cpto EQ "Polizas" THEN
                                        ASSIGN VrCargos[7] = VrCargos[7] + TPC.Cargo
                                               VrAbonos[7] = VrAbonos[7] + TPC.Abono.
                                    ELSE
                                        IF TPC.Cpto EQ "Honorarios" THEN
                                            ASSIGN VrCargos[8] = VrCargos[8] + TPC.Cargo
                                                   VrAbonos[8] = VrAbonos[8] + TPC.Abono.
                                        ELSE
                                            IF TPC.Cpto EQ "SegVida" THEN
                                                ASSIGN VrCargos[9] = VrCargos[9] + TPC.Cargo
                                                       VrAbonos[9] = VrAbonos[9] + TPC.Abono.
                                            ELSE
                                                IF TPC.Cpto EQ "SegVivienda" THEN
                                                    ASSIGN VrCargos[10] = VrCargos[10] + TPC.Cargo
                                                           VrAbonos[10] = VrAbonos[10] + TPC.Abono.
        END.
        ELSE DO:   /*Contables Cta-PPal*/
            ASSIGN W_Cta = TPC.CtaP
                   W_Valor = TPC.Cargo - TPC.Abono.

            RUN OpContable NO-ERROR.
            IF W_Valor LT 0 THEN
                ASSIGN Mov_Contable.Cr = W_Valor * -1
                       Mov_Contable.Db = 0.
        END.

        IF TPC.Contra GT "0" AND TPC.Op NE 3 THEN DO:   /*Contables-Contrapartida*/
            IF TPC.TP EQ "A" AND TPC.Cargo GT 0 AND TPC.Cpto EQ "SdoCta" THEN
                RUN Calc_GMFAhorro.
            
            ASSIGN W_Cta = TPC.Contra
                   W_Valor = TPC.Cargo - TPC.Abono - (TPC.Cargo * (Porc_Base / 100)).

            RUN OpContable NO-ERROR.
            Mov_Contable.Nit = TPC.CedCont.
            
            IF W_Valor GT 0 THEN DO:
                ASSIGN Mov_Contable.Cr = W_Valor 
                       Mov_Contable.Db = 0.
            END.
            ELSE DO:
                ASSIGN Mov_Contable.Db = W_Valor * -1
                       Mov_Contable.Cr = 0.
            END.
        END.
        ELSE
            IF TPC.Op NE 3 AND TPC.TP EQ "A" AND TPC.Cargo GT 0 AND TPC.Cpto EQ "SdoCta" THEN
                RUN Calc_GMFAhorro.

        IF LAST-OF(TPC.CtaP) AND TPC.TP NE "0" THEN DO:
            IF TPC.TP EQ "A" THEN DO:
                RUN GrabaAhorro NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    RETURN ERROR.
            END.
            ELSE DO:
                RUN GrabaCredito NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    RETURN ERROR.
            END.

            ASSIGN VrCargos = 0
                   VrAbonos = 0.
        END.
    END.

    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.

    FOR EACH CopMov_Contable:
        CREATE Mov_Contable.
        BUFFER-COPY CopMov_Contable TO Mov_Contable.

        DELETE CopMov_Contable.
    END.

    FIND LAST Agencias WHERE Agencias.Agencia GT 0 AND Agencias.Estado NE 3 NO-LOCK NO-ERROR.
    DO I = 1 TO Agencias.Agencia:
        TotT = 0.

        FOR EACH Mov_Contable WHERE Mov_Contable.Agencia EQ I
                                AND Mov_Contable.Comprobante EQ Comprobantes.Comprobante
                                AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia NO-LOCK:
            IF Mov_Contable.Fec_Contable EQ W_Fecha AND Mov_Contable.Usuario EQ W_Usuario THEN
                ASSIGN TotT = TotT + (Mov_Contable.Db - Mov_Contable.Cr)
                       TCr = TCr + Mov_Contable.Cr
                       TDb = TDb + Mov_Contable.Db.
        END.
            
        FOR EACH Mov_Contable WHERE Mov_Contable.Agencia EQ I
                                AND Mov_Contable.Comprobante EQ Comprobantes.Comprobante
                                AND mov_contable.num_documento = numDocAux NO-LOCK:
            IF Mov_Contable.Fec_Contable EQ W_Fecha AND Mov_Contable.Usuario EQ W_Usuario THEN
                ASSIGN TotT = TotT + (Mov_Contable.Db - Mov_Contable.Cr)
                       TCr = TCr + Mov_Contable.Cr
                       TDb = TDb + Mov_Contable.Db.
        END.

        IF TotT NE 0 THEN DO:
            MESSAGE "Los DEBE - HABER en la Agencia : " I SKIP
                    "                 Están Diferentes...Revise por favor." SKIP
                    "Debitos --> " TDb SKIP
                    "Creditos--> " TCr
                VIEW-AS ALERT-BOX ERROR.

            ASSIGN W_SiErr = TRUE.
        END.
    END.

    IF W_SiErr THEN DO:
        RUN ImpError.

        W_SiImpErr = FALSE.

        MESSAGE "Transacción no efectuada"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN ERROR.
    END.

    FIND CURRENT Ahorros NO-LOCK NO-ERROR.
    FIND CURRENT Creditos NO-LOCK NO-ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContablesAhorro W-Tras_Cuentas 
PROCEDURE ContablesAhorro :
DEFINE VAR numDocAux AS INTEGER.

numDocAux = comprobantes.secuencia.

IF ahorros.agencia <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
    FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = ahorros.agencia
                                 AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
    IF AVAILABLE bfrComprobantes THEN DO:
        bfrComprobantes.secuencia = bfrComprobantes.secuencia + 1.
        numDocAux = bfrComprobantes.secuencia.
        flagCtaSucyAg = TRUE.
    END.
END.
ELSE DO:
    IF ahorros.agencia <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
        FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = ahorros.agencia
                                     AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
        IF AVAILABLE bfrComprobantes THEN
            numDocAux = bfrComprobantes.secuencia.
    END.
END.


CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = Ahorros.Agencia
       Mov_Contable.Cuenta = W_Cta
       Mov_Contable.Nit = TPC.NitP
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Comentario = TPC.Descrip
       Mov_Contable.Usuario = W_Usuario
       Mov_Contable.Cen_Costos = W_Cencosgral
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Comprobante = Comprobantes.Comprobante
       Mov_Contable.Num_Documento = /*Comprobantes.Secuencia*/ numDocAux
       Mov_Contable.Doc_Refer = TPC.CtaP
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Estacion = W_Estacion
       Mov_Contable.Db = W_Valor.

IF ahorros.agencia <> w_agencia THEN
    mov_contable.enlace = STRING(comprobantes.secuencia).

IF Rs_Op = 2 THEN
    mov_contable.comentario = "TM/"+ mov_contable.comentario.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContablesCred W-Tras_Cuentas 
PROCEDURE ContablesCred :
ASSIGN VrDist = P_Poliza + P_Honora + P_Costas + P_IMora + P_IMorDifC + P_IAntic + P_ICte + P_IDifCob + P_Capit + P_SeguroVida + P_SeguroDeudor
       VrDist = VrDist * -1.

IF P_Poliza NE 0 THEN DO:
    ASSIGN W_Valor = P_Poliza
           W_Cta = TempCtas.CtaPol
           W_DescC = "X Polizas".
    RUN GrabaContCred.   /*Al final de este mismo procedimiento*/

    IF P_Poliza LT 0 THEN DO:
        ASSIGN Mov_Contable.Db = W_Valor * -1
               Mov_Contable.Cr = 0.
    END.
END.

IF P_Honora NE 0 THEN DO:
    ASSIGN W_Valor = P_Honora
           W_Cta = TempCtas.CtaHon
           W_DescC = "X Honorarios".
    RUN GrabaContCred.

    IF P_Honora LT 0 THEN DO:
        ASSIGN Mov_Contable.Db = W_Valor * -1
               Mov_Contable.Cr = 0.
    END.
END.

IF P_Costas NE 0 THEN DO:
    ASSIGN W_Valor = P_Costas
           W_Cta = TempCtas.CtaCos
           W_DescC = "X Costas jurid.".
    RUN GrabaContCred.

    IF P_Costas LT 0 THEN DO:
        ASSIGN Mov_Contable.Db = W_Valor * -1
               Mov_Contable.Cr = 0.
    END.
END.

IF P_SeguroVida NE 0 THEN DO:
    ASSIGN W_Valor = P_SeguroVida
           W_Cta = TempCtas.CtaCos
           W_DescC = "X Seguro de Vida".
    RUN GrabaContCred.

    IF P_SeguroVida LT 0 THEN DO:
        ASSIGN Mov_Contable.Db = W_Valor * -1
               Mov_Contable.Cr = 0.
    END.
END.

IF P_SeguroDeudor NE 0 THEN DO:
    ASSIGN W_Valor = P_SeguroDeudor
           W_Cta = TempCtas.CtaCos
           W_DescC = "X Seguro de Vivienda".
    RUN GrabaContCred.

    IF P_SeguroVida LT 0 THEN DO:
        ASSIGN Mov_Contable.Db = W_Valor * -1
               Mov_Contable.Cr = 0.
    END.
END.

IF P_IMorDifC GT 0 THEN DO:
    IF creditos.FOR_pago = 2 THEN
        W_Cta = TempCtas.MorIng.
    ELSE
        W_Cta = TempCtas.MorIngCaja.

    ASSIGN W_Valor = P_IMorDifC
           W_DescC = "X Mora-DifCobro".
    RUN GrabaContCred.

    IF creditos.FOR_pago = 2 THEN
        W_Cta = TempCtas.DifCoD.
    ELSE
        W_Cta = TempCtas.DifCoDCaja.
    
    RUN GrabaContCred.

    IF creditos.FOR_pago = 2 THEN
        W_Cta = TempCtas.DifCoH.
    ELSE
        W_Cta = TempCtas.DifCoHCaja.
    
    RUN GrabaContCred.
    ASSIGN Mov_Contable.Cr = 0
           Mov_Contable.Db = W_Valor.
END.
ELSE
    IF P_IMorDifC LT 0 THEN DO:
        IF creditos.FOR_pago = 2 THEN
            W_Cta = TempCtas.DifCoH.
        ELSE
            W_Cta = TempCtas.DifCoHCaja.

        ASSIGN W_Valor = P_IMorDifC * -1
               W_DescC = "X Mora-DifCobro".
        
        RUN GrabaContCred.

        IF creditos.FOR_pago = 2 THEN
            W_Cta = TempCtas.DifCoD.
        ELSE
            W_Cta = TempCtas.DifCoDCaja.
        
        RUN GrabaContCred.
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = W_Valor.

        IF creditos.FOR_pago = 2 THEN
            W_Cta = TempCtas.MorIng.
        ELSE
            W_Cta = TempCtas.MorIngCaja.

        RUN GrabaContCred.
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = W_Valor.
    END.

IF P_IMora NE 0 THEN DO:
    IF creditos.FOR_pago = 2 THEN
        W_Cta = TempCtas.IntMor.
    ELSE
        W_Cta = TempCtas.IntMorCaja.

    ASSIGN W_Valor = P_IMora
           W_DescC = "X Int-Mora".
    RUN GrabaContCred.

    IF P_IMora LT 0 THEN DO:
        ASSIGN Mov_Contable.Db = W_Valor * -1
               Mov_Contable.Cr = 0.
    END.
END.

IF P_IAntic NE 0 THEN DO:
    IF creditos.FOR_pago = 2 THEN
        W_Cta = TempCtas.IntAnt.
    ELSE
        W_Cta = TempCtas.IntAntCaja.

    ASSIGN W_Valor = P_IAntic
           W_DescC = "X Int-Anticip".
    RUN GrabaContCred.

    IF P_IAntic LT 0 THEN DO:
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = P_IAntic * -1.
    END.
END.

IF P_ICte NE 0 THEN DO:
    IF creditos.FOR_pago = 2 THEN
        W_Cta = TempCtas.CtaLiq.
    ELSE
        W_Cta = TempCtas.CtaLiqCaja.

    ASSIGN W_Valor = P_ICte
           W_DescC = "X Int-Ctes".
    RUN GrabaContCred.

    IF P_ICte LT 0 THEN DO:
        ASSIGN Mov_Contable.Db = W_Valor * -1
               Mov_Contable.Cr = 0.
    END.
END.

IF P_Capit NE 0 THEN DO:
    IF creditos.FOR_pago = 2 THEN
        W_Cta = TempCtas.CtaPro.
    ELSE
        W_Cta = TempCtas.CtaProCaja.

    ASSIGN W_Valor = P_Capit
           W_DescC = "X Capital".
    RUN GrabaContCred.

    IF P_Capit LT 0 THEN DO:
        ASSIGN Mov_Contable.Db = W_Valor * -1
               Mov_Contable.Cr = 0.
    END.
END.

IF P_IDifCob GT 0 THEN DO:
    IF creditos.FOR_pago = 2 THEN
        W_Cta = TempCtas.DifCoD.
    ELSE
        W_Cta = TempCtas.DifCoDCaja.

    ASSIGN W_Valor = P_IDifCob
           W_DescC = "X Ctes-DifCobro".
    RUN GrabaContCred.

    IF creditos.FOR_pago = 2 THEN
        W_Cta = TempCtas.DifCoH.
    ELSE
        W_Cta = TempCtas.DifCoHCaja.
    
    RUN GrabaContCred.
    ASSIGN Mov_Contable.Cr = 0
           Mov_Contable.Db = W_Valor.

    IF creditos.FOR_pago = 2 THEN
        W_Cta = TempCtas.CtaIng.
    ELSE
        W_Cta = TempCtas.CtaIngCaja.

    RUN GrabaContCred.
END.
ELSE
    IF P_IDifCob LT 0 THEN DO:
        IF creditos.FOR_pago = 2 THEN
            W_Cta = TempCtas.DifCoH.
        ELSE
            W_Cta = TempCtas.DifCoHCaja.

        ASSIGN W_Valor = P_IDifCob * -1
               W_DescC = "X Ctes-DifCobro".
        RUN GrabaContCred.

        IF creditos.FOR_pago = 2 THEN
            W_Cta = TempCtas.DifCoD.
        ELSE
            W_Cta = TempCtas.DifCoDCaja.

        RUN GrabaContCred.
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = W_Valor.

        IF creditos.FOR_pago = 2 THEN
            W_Cta = TempCtas.CtaIng.
        ELSE
            W_Cta = TempCtas.CtaIngCaja.

        RUN GrabaContCred.
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = W_Valor.
    END.

END PROCEDURE.

PROCEDURE GrabaContCred:
    DEFINE VAR numDocAux AS INTEGER.
    
    numDocAux = comprobantes.secuencia.

    IF creditos.agencia <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
        FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = creditos.agencia
                                     AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
        IF AVAILABLE bfrComprobantes THEN DO:
            bfrComprobantes.secuencia = bfrComprobantes.secuencia + 1.
            numDocAux = bfrComprobantes.secuencia.
            flagCtaSucyAg = TRUE.
        END.
    END.
    ELSE DO:
        IF creditos.agencia <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
            FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = creditos.agencia
                                         AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
            IF AVAILABLE bfrComprobantes THEN
                numDocAux = bfrComprobantes.secuencia.
        END.
    END.

    CREATE Mov_Contable.
    ASSIGN Mov_Contable.Agencia = Creditos.Agen
           Mov_Contable.Cuenta = W_Cta
           Mov_Contable.Nit = TPC.NitP
           Mov_Contable.Fec_Contable = W_Fecha
           Mov_Contable.Comentario = TPC.Descrip + W_DescC
           Mov_Contable.Usuario = W_Usuario
           Mov_Contable.Cen_Costos = W_Cencosgral
           Mov_Contable.Destino = W_Agencia
           Mov_Contable.Comprobante = Comprobantes.Comprobante
           Mov_Contable.Num_Documento = /*Comprobantes.Secuencia*/ numDocAux
           Mov_Contable.Doc_Refer = Creditos.Pagare
           Mov_Contable.Enlace = STRING(Creditos.Num_credito)
           Mov_Contable.Fec_Grabacion = TODAY
           Mov_Contable.Hora = TIME
           Mov_Contable.Estacion = W_Estacion
           Mov_Contable.Cr = W_Valor.

    IF creditos.agencia <> w_agencia THEN
        mov_contable.enlace = STRING(comprobantes.secuencia).
    
    IF Rs_Op = 2 THEN
        mov_contable.comentario = "TM/"+ mov_contable.comentario.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ControlLavado_Aho W-Tras_Cuentas 
PROCEDURE ControlLavado_Aho :
DEFINE INPUT PARAMETER Tpdt AS INTEGER FORMAT 9.

DEFINE VAR RT_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR CS_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR AB_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR RT_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR CS_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR AB_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR PCod LIKE BorradorSipla.CodAutoriza.
DEFINE VAR PUsu LIKE W_Usuario.
DEFINE VAR PMen  AS CHARACTER FORMAT "X(5)".
DEFINE VAR PMenM AS CHARACTER FORMAT "X(5)".
FIND LAST ControlSipla WHERE
          ControlSipla.Nit          EQ Ahorros.Nit AND
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
    ASSIGN ControlSipla.Nit         = Ahorros.Nit
           ControlSipla.Fecha       = W_Fecha.
END.

IF Tpc.Abono GT 0 THEN DO: /*es consignacion*/
   IF CS_ValSiplaDia + AB_ValSiplaDia + Tpc.Abono GT Entidad.MaxOp_Efectivo_Dia THEN 
      ASSIGN PMen  = "CSDIA". 
   IF CS_ValSiplaMes + AB_ValSiplaMes + Tpc.Abono  GT Entidad.MaxOp_Efectivo_Mes THEN
      ASSIGN PMenM = "CSMES".
END.
IF Tpc.Cargo GT 0 THEN DO: /* es retiro */
   IF RT_ValSiplaDia + Tpc.Cargo GT Entidad.MaxOp_Efectivo_Dia THEN
      ASSIGN PMen  = "RTDIA".
   IF RT_ValSiplaMes + Tpc.Cargo GT Entidad.MaxOp_Efectivo_Mes THEN
      ASSIGN PMenM = "RTMES".
END.

ASSIGN PCod = 0.
       /*MESSAGE controlsipla.fecha w_fecha VIEW-AS ALERT-BOX.*/
   IF ControlSipla.Fecha NE W_Fecha THEN DO:
       CREATE ControlSipla.
       ASSIGN ControlSipla.Nit         = Ahorros.Nit
              ControlSipla.Fecha       = W_Fecha.
   END.
   ASSIGN ControlSipla.CS_TotalMes = ControlSipla.CS_TotalMes + Tpc.Abono
          ControlSipla.RT_TotalMes = ControlSipla.RT_TotalMes + Tpc.Cargo
          ControlSipla.CS_TotalDia = ControlSipla.CS_TotalDia + Tpc.Abono
          ControlSipla.RT_TotalDia = ControlSipla.RT_TotalDia + Tpc.Cargo.
IF Pmen NE "" OR PMenM NE "" THEN DO:
    FIND Instancias WHERE
         Instancias.Tipo_Instancia EQ 6   AND 
         Instancias.Primera        EQ YES AND
         Instancias.Estado         EQ 1   NO-LOCK NO-ERROR.
    IF AVAILABLE Instancias THEN DO:
       FIND FIRST Cfg_Instancias WHERE 
            Cfg_Instancias.Agencia   EQ Ahorros.Agencia /*W_Agencia*/ AND /*para que la instancia quede*/
            Cfg_Instancias.Instancia EQ Instancias.Instancia AND          /*en la agencia de la cuenta*/
            Cfg_Instancias.Estado    EQ 1 NO-LOCK NO-ERROR.
       IF AVAILABLE Cfg_Instancias THEN DO:
           CREATE BorradorSipla.
           ASSIGN BorradorSipla.Agencia        = W_Agencia                   
                  BorradorSipla.Nit            = Ahorros.Nit                        
                  BorradorSipla.Fecha          = W_Fecha                     
                  BorradorSipla.Hora           = TIME                        
                  BorradorSipla.Usuario        = W_Usuario                   
                  BorradorSipla.CodAutoriza    = NEXT-VALUE(Sec_Autorizacion)
                  BorradorSipla.Id_AhoCre      = Tpdt.
           IF PMen  NE "" THEN BorradorSipla.Id_Nud  = YES.
           IF PMenM NE "" THEN BorradorSipla.Id_Num  = YES.
       END. 
       ASSIGN BorradorSipla.Descripcion    = "[Control Automatico, Operacion de Traslado]"
              BorradorSipla.Id_Exonerada   = YES
              CodAutoriza                  = BorradorSipla.CodAutoriza
              PCod                         = BorradorSipla.CodAutoriza.
          FIND FIRST Mov_InsSipla WHERE
               Mov_InsSipla.Agencia           EQ Ahorros.Agencia AND
               Mov_InsSipla.Fecha_Transaccion EQ W_Fecha         AND
               Mov_InsSipla.Instancia         EQ Cfg_Instancias.Instancia AND
               Mov_InsSipla.Nit               EQ Ahorros.Nit NO-ERROR.
          IF NOT AVAILABLE Mov_InsSipla THEN DO:
              CREATE Mov_InsSipla.
              ASSIGN Mov_InsSipla.Agencia           = Ahorros.Agencia /*W_Agencia*/
                     Mov_InsSipla.Fecha_Transaccion = W_Fecha
                     Mov_InsSipla.Hora_Transaccion  = TIME
                     Mov_InsSipla.Instancia         = Cfg_Instancias.Instancia
                     Mov_InsSipla.Nit               = Ahorros.Nit
                     Mov_InsSipla.UsuCajero         = W_Usuario
                     Mov_InsSipla.UsuGestiona       = Cfg_Instancias.Usuario
                     Mov_InsSipla.Id_Traslado       = YES.
          END. 
          IF AVAILABLE BorradorSipla THEN DO:
              ASSIGN Mov_InsSipla.UsuReporta        = BorradorSipla.Usuario
                     Mov_InsSipla.Id_Exonerada      = BorradorSipla.Id_Exonerada
                     Mov_InsSipla.Id_NUD            = BorradorSipla.Id_NUD
                     Mov_InsSipla.Id_NUM            = BorradorSipla.Id_NUM
                     Mov_InsSipla.Id_RepUIAF        = BorradorSipla.Id_RepFiscalia
                     Mov_InsSipla.Id_Sospechosa     = BorradorSipla.Id_Sospechosa
                     Mov_InsSipla.CodAutoriza       = PCod.
              IF Ahorros.Agencia NE W_Agencia THEN 
                 Mov_InsSipla.Descripcion       = Mov_InsSipla.Descripcion + 
                                                  " Ag:" + STRING(W_Agencia,"99") + " - " +
                                                  BorradorSipla.Descripcion.
              ELSE
                  Mov_InsSipla.Descripcion       = Mov_InsSipla.Descripcion + " . " +
                                                   BorradorSipla.Descripcion.
          END.
          ELSE DO:
             MESSAGE "No se encontrador el borrador sipla que permita" SKIP
                     "identificar quien manejo al cliente en la agencia"
                     VIEW-AS ALERT-BOX.
             RETURN ERROR.
          END. 
          IF PMen  NE "" THEN Mov_InsSipla.Id_NUD = YES.
          IF PMenM NE "" THEN Mov_InsSipla.Id_NUM = YES. 
    END.
    ELSE DO:
        MESSAGE "No se ha encontrado la primera instancia" SKIP
                "del control de lavado de activos." SKIP
                "la transaccion no se podra realizar!!!" SKIP(1)
                "comuniquese con el depto de sistemas" VIEW-AS ALERT-BOX ERROR.
    END.
END.
RELEASE Mov_InsSipla.
RELEASE ControlSipla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ControlLavado_Cre W-Tras_Cuentas 
PROCEDURE ControlLavado_Cre :
DEFINE INPUT PARAMETER Tpdt AS INTEGER FORMAT 9.

DEFINE VAR RT_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR CS_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR AB_ValSiplaDia LIKE ControlSipla.CS_TotalDia.
DEFINE VAR RT_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR CS_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR AB_ValSiplaMes LIKE ControlSipla.CS_TotalDia.
DEFINE VAR PCod LIKE BorradorSipla.CodAutoriza.
DEFINE VAR PUsu LIKE W_Usuario.
DEFINE VAR PMen  AS CHARACTER FORMAT "X(5)".
DEFINE VAR PMenM AS CHARACTER FORMAT "X(5)".
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

IF Tpc.Abono GT 0 THEN DO: /*es consignacion*/
   IF CS_ValSiplaDia + AB_ValSiplaDia + Tpc.Abono GT Entidad.MaxOp_Efectivo_Dia THEN 
      ASSIGN PMen  = "CSDIA". 
   IF CS_ValSiplaMes + AB_ValSiplaMes + Tpc.Abono  GT Entidad.MaxOp_Efectivo_Mes THEN
      ASSIGN PMenM = "CSMES".
END.
IF Tpc.Cargo GT 0 THEN DO: /* es retiro */
   IF RT_ValSiplaDia + Tpc.Cargo GT Entidad.MaxOp_Efectivo_Dia THEN
      ASSIGN PMen  = "RTDIA".
   IF RT_ValSiplaMes + Tpc.Cargo GT Entidad.MaxOp_Efectivo_Mes THEN
      ASSIGN PMenM = "RTMES".
END.

ASSIGN PCod = 0.
       /*MESSAGE controlsipla.fecha w_fecha VIEW-AS ALERT-BOX.*/
   IF ControlSipla.Fecha NE W_Fecha THEN DO:
       CREATE ControlSipla.
       ASSIGN ControlSipla.Nit         = Creditos.Nit
              ControlSipla.Fecha       = W_Fecha.
   END.
   ASSIGN ControlSipla.AB_TotalMes = ControlSipla.AB_TotalMes + Tpc.Abono
          ControlSipla.AB_TotalDia = ControlSipla.AB_TotalDia + Tpc.Abono.
IF Pmen NE "" OR PmenM NE "" THEN DO:
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
           CREATE BorradorSipla.
           ASSIGN BorradorSipla.Agencia        = W_Agencia                   
                  BorradorSipla.Nit            = Creditos.Nit                        
                  BorradorSipla.Fecha          = W_Fecha                     
                  BorradorSipla.Hora           = TIME                        
                  BorradorSipla.Usuario        = W_Usuario                   
                  BorradorSipla.CodAutoriza    = NEXT-VALUE(Sec_Autorizacion)
                  BorradorSipla.Id_AhoCre      = Tpdt.
           IF PMen  NE "" THEN BorradorSipla.Id_Nud  = YES.
           IF PMenM NE "" THEN BorradorSipla.Id_Num  = YES.
       END. 
       ASSIGN BorradorSipla.Descripcion    = "[Control Automatico, Operacion de Traslado]"
              BorradorSipla.Id_Exonerada   = YES
              CodAutoriza                  = BorradorSipla.CodAutoriza
              PCod                         = BorradorSipla.CodAutoriza.
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
                     Mov_InsSipla.UsuGestiona       = Cfg_Instancias.Usuario
                     Mov_InsSipla.Id_Traslado       = YES.
                     /*Mov_InsSipla.AgeOrigen         = W_Agencia Creditos.Agencia*/
          END. 
          IF AVAILABLE BorradorSipla THEN DO:
              ASSIGN Mov_InsSipla.UsuReporta        = BorradorSipla.Usuario
                     Mov_InsSipla.Id_Exonerada      = BorradorSipla.Id_Exonerada
                     Mov_InsSipla.Id_NUD            = BorradorSipla.Id_NUD
                     Mov_InsSipla.Id_NUM            = BorradorSipla.Id_NUM
                     Mov_InsSipla.Id_RepUIAF        = BorradorSipla.Id_RepFiscalia
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
             MESSAGE "No se encontrador el borrador sipla que permita" SKIP
                     "identificar quien manejo al cliente en la agencia"
                     VIEW-AS ALERT-BOX.
             RETURN ERROR.
          END. 
          IF PMen  NE "" THEN Mov_InsSipla.Id_NUD = YES.
          IF PMenM NE "" THEN Mov_InsSipla.Id_NUM = YES. 
    END.
    ELSE DO:
        MESSAGE "No se ha encontrado la primera instancia" SKIP
                "del control de lavado de activos." SKIP
                "la transaccion no se podra realizar!!!" SKIP(1)
                "comuniquese con el depto de sistemas" VIEW-AS ALERT-BOX ERROR.
    END.
END.
RELEASE Mov_InsSipla.
RELEASE ControlSipla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ControlPagos W-Tras_Cuentas 
PROCEDURE ControlPagos :
DEFINE VAR tasaCausa AS DECIMAL.
DEFINE VAR diasVencidos AS DECIMAL.
DEFINE VAR intVencido AS DECIMAL.
DEFINE VAR cont AS INTEGER.

/*Crea el temporal de control pagos*/
IF creditos.cod_credito <> 123 THEN DO:
    FOR EACH CONTROL_pagos WHERE control_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito
                             AND control_pagos.id_pdoMes < 2 NO-LOCK BREAK BY CONTROL_pagos.num_credito
                                                                           BY CONTROL_pagos.nro_cuota:
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
               tControl_pagos.INT_pagado = facturacion.pago_IntCorriente
               tControl_pagos.TOTAL_cuota = facturacion.cuota - facturacion.pago_capital - facturacion.pago_intCorriente - facturacion.pago_intDifCobro - facturacion.pago_mora
               tControl_pagos.pagos_capitalAcum = facturacion.capital
               tControl_pagos.pagos_intAcum = facturacion.INT_corriente + facturacion.INT_difCobro
               tControl_pagos.INT_mora = facturacion.INT_mora - facturacion.pago_mora.
    END.
END.

FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.

/*limpio los descuentos*/
FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
    btcontrol_pagos.Otros = 0.
END.

FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
    btcontrol_pagos.Otros = creditos.honorarios + creditos.costas + creditos.polizas.
    LEAVE.
END.

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
        tcontrol_pagos.TOTAL_Cuota = tcontrol_pagos.cuota + tControl_pagos.INT_mora - tControl_pagos.cap_pagado.

        /*tcontrol_pagos.TOTAL_Cuota = creditos.cuota + tControl_pagos.INT_mora - tControl_pagos.cap_pagado.

        FIND FIRST extras WHERE extras.agencia = creditos.agencia
                            AND extras.nit = creditos.nit
                            AND extras.cod_credito = creditos.cod_credito
                            AND extras.num_solicitud = creditos.num_solicitud
                            AND extras.nro_cuota = tcontrol_pagos.nro_cuota NO-LOCK NO-ERROR.
        IF AVAILABLE extras THEN
            tControl_pagos.TOTAL_cuota = tControl_pagos.TOTAL_cuota + extras.vr_cuoExtra.*/

    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ControlPagos_bck W-Tras_Cuentas 
PROCEDURE ControlPagos_bck :
DEF VAR w_iTasaM LIKE Creditos.Sdo_Capital.
DEF VAR w_TasaNA AS DEC FORMAT "->>>>9.9999999".
DEF VAR w_TasaEA AS DEC FORMAT "->>>>9.9999999".
DEF VAR W_DiaAnt AS INT.
DEF VAR W_IAntic AS INT.
DEF VAR VtasaMax AS DEC.
DEFINE VAR tasaCausa AS DECIMAL.
DEFINE VAR diasVencidos AS DECIMAL.
DEFINE VAR intVencido AS DECIMAL.

RUN HallarPeriodo IN W_ManFin(INPUT Creditos.Per_Pago,
                              INPUT Creditos.Plazo,
                              OUTPUT W_NroDia,
                              OUTPUT P_NMeses,
                              OUTPUT W_NroPer,
                              OUTPUT P_NomPer).

/*Crea el temporal de control pagos*/
FOR EACH CONTROL_pagos WHERE control_pagos.nit = creditos.nit
                         AND CONTROL_pagos.num_credito = creditos.num_credito
                         AND control_pagos.Id_PdoMes < 2:
    IF control_pagos.Cap_pagado >= CONTROL_pagos.pagos_capitalAcum AND control_pagos.Int_pagado >= control_pagos.causacion THEN
        control_pagos.Id_PdoMes = 2.
    ELSE DO:
        CREATE tCONTROL_pagos.
        BUFFER-COPY CONTROL_pagos TO tcontrol_pagos.
    END.
END.

FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.

/*Busca indicador de tasa maxima.*/
FIND indicadores WHERE Indicadores.Indicador = Pro_Creditos.Cod_TasaMax NO-LOCK NO-ERROR.
IF AVAILABLE indicadores THEN DO:
    /*La tasa esta en efectivo anual toca pasarla a nominal vencida dependiendo del periodo*/
    RUN EFNV IN W_ManFin (Indicadores.Tasa / 100,W_NroPer,OUTPUT w_TasaNominal).
END.
ELSE
    w_TasaNominal = (creditos.tasa / 100) / W_NroPer.

IF w_TasaNominal > (creditos.tasa / 100) / W_NroPer THEN
    w_TasaNominal = (creditos.tasa / 100) / W_NroPer.

/*limpio los descuentos*/
FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
    btcontrol_pagos.Otros = 0.
END.

FOR EACH btcontrol_pagos BY btcontrol_pagos.Nro_Cuota:
    btcontrol_pagos.Otros = creditos.honorarios + creditos.costas + creditos.polizas.
    LEAVE.
END.

/*Revisamos le interes maximo*/
W_SaldoK = creditos.monto.

FOR EACH control_pagos WHERE control_pagos.Nit = creditos.nit
                         AND control_pagos.Num_Credito = creditos.num_credito
                         AND control_pagos.Nro_Cuota > 0:
    w_saldok = w_saldok - control_pagos.pagos_capitalAcum.

    FIND FIRST btcontrol_pagos WHERE btcontrol_pagos.Nro_Cuota = control_pagos.Nro_Cuota NO-LOCK NO-ERROR.
    IF NOT AVAILABLE btcontrol_pagos THEN
        NEXT.

    /*Calculamos el valor a descontar por tasa maxima*/
    w_iTasaM = ROUND((w_SaldoK + control_pagos.pagos_capitalAcum) * w_TasaNominal,0).

    IF w_iTasaM < control_pagos.Pagos_IntAcum AND (control_pagos.Int_pagado + control_pagos.Cap_pagado) = 0 THEN DO:
        RUN NVEF IN W_ManFin (w_TasaNominal * W_NroPer,W_NroPer,OUTPUT w_TasaEA).
        RUN EFNA1 IN W_ManFin (w_TasaEA,W_NroPer,OUTPUT w_TasaNA).
        tasaCausa = w_tasaNominal * W_NroPer.
    END.
    ELSE DO:
        RUN NVEF IN W_ManFin (w_TasaNominal * W_NroPer,W_NroPer,OUTPUT w_TasaEA).
        RUN EFNA1 IN W_ManFin (w_TasaEA,W_NroPer,OUTPUT w_TasaNA).
        tasaCausa = creditos.tasa / 100.
    END.

    /* 2. Para una cuota al día - periodo vigente */
    IF control_pagos.fec_Vcto >= w_fecha AND CONTROL_pagos.fec_Vcto - W_NroDia <= w_fecha THEN DO:
        W_DiaAnt = control_pagos.Fec_Vcto - w_fecha.

        IF w_DiaAnt > 0 THEN DO:
            IF DAY(control_pagos.Fec_Vcto) = 31 THEN
                w_DiaAnt = w_DiaAnt - 1.

            IF MONTH(control_pagos.Fec_Vcto) = 2 THEN DO:
                IF DAY(control_pagos.Fec_Vcto) = 28 THEN
                    w_DiaAnt = w_DiaAnt + 2.

                IF DAY(control_pagos.Fec_Vcto) = 29 THEN
                    w_DiaAnt = w_DiaAnt + 1.
            END.
        END.

        IF W_DiaAnt > W_NroDia THEN
            W_DiaAnt = W_nroDia.

        btControl_pagos.intAnticipado = ROUND(w_DiaAnt * ((w_saldoK * w_TasaNA / W_NroPer) / W_NroDia),0).
        
        /* Ajusto por los anticipados que pudieren existir */
        IF CONTROL_pagos.INT_pagado >= btControl_pagos.intAnticipado THEN
            btControl_pagos.intAnticipado = 0.
        ELSE
            btControl_pagos.intAnticipado = btControl_pagos.intAnticipado - CONTROL_pagos.INT_pagado.
    END.

    /* 3. Para cuotas completas adelantadas */
    IF control_pagos.fec_Vcto >= w_fecha AND CONTROL_pagos.fec_Vcto - W_NroDia > w_fecha THEN DO:
        W_DiaAnt = W_NroDia.

        btControl_pagos.intAnticipado = ROUND(W_DiaAnt * ((w_saldoK * w_TasaNA / W_NroPer) / W_NroDia),0).
        
        /* Ajusto por los anticipados que pudieren existir */
        IF CONTROL_pagos.INT_pagado >= btControl_pagos.intAnticipado THEN
            btControl_pagos.intAnticipado = 0.
        ELSE
            btControl_pagos.intAnticipado = btControl_pagos.intAnticipado - CONTROL_pagos.INT_pagado.
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
                                 tcontrol_pagos.causacion + tcontrol_pagos.contingente + tcontrol_pagos.SeguroDeudor + tcontrol_pagos.segurovida +
                                 tControl_pagos.intAnticipado - tControl_pagos.INT_pagado +
                                 tControl_pagos.INT_mora.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load W-Tras_Cuentas  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "W-Tras_Cuentas.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    CtrlFrame-2:NAME = "CtrlFrame-2":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "W-Tras_Cuentas.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crear_DetalGmf W-Tras_Cuentas 
PROCEDURE Crear_DetalGmf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN Mov_GMF.Agencia             = W_Agencia
         Mov_GMF.Agencia_Tx          = Ahorros.Agencia
         Mov_GMF.Documento           = STRING(Comprobantes.Secuencia)
         Mov_GMF.Fecha               = W_Fecha
         Mov_GMF.Hora                = TIME
         Mov_GMF.Descrip             = "GMF EXENTO x Trasl." 
         Mov_GMF.Id_EfChTras         = 2
         Mov_GMF.Nit                 = Ahorros.Nit
         Mov_GMF.Porc_Impto          = 0
         Mov_GMF.Renglon             = 00
         Mov_GMF.Tipo_Pdcto          = 1
         Mov_GMF.VrAcum_RetMes       = 0
         Mov_GMF.Cod_Pdcto           = Ahorros.Cod_Ahorro
         Mov_GMF.Cpte                = Comprobantes.Comprobante
         Mov_GMF.CtaDb_ImptoCliente  = ""
         Mov_GMF.CtaDb_ImptoEntidad  = ""
         Mov_GMF.Cta_ContableCr      = ""
         Mov_GMF.Cta_PdctoOContab    = Ahorros.Cue_Ahorro
         Mov_GMF.VrBase_Cliente      = 0
         Mov_GMF.VrBase_Entidad      = 0
         Mov_GMF.VrImpto_Cliente     = 0
         Mov_GMF.VrImpto_Entidad     = 0.
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DetectaMismoCliente W-Tras_Cuentas 
PROCEDURE DetectaMismoCliente :
DEFINE VAR Xnit LIKE Clientes.Nit.
  W_MismoCliente = YES.
  FIND FIRST Tpc WHERE Tpc.Nitp NE "".
  XNit = Tpc.NitP.
  FOR EACH TPC:
      IF XNit NE Tpc.NitP THEN DO:
         W_MismoCliente = NO. 
         NEXT.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Tras_Cuentas  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Tras_Cuentas)
  THEN DELETE WIDGET W-Tras_Cuentas.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Tras_Cuentas  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY W_CmbOfi W_NomCte W_NitCte W_AgDest Fec_corte1 Tg_Pdctos W_Tipo W_AgP 
          W_Pdcto W_Cta W_NomPC Rs_Op W_Cpto W_CtaContra W_NitContra W_DocRef 
          Cmb_Cptos W_Cargo W_Abono W_NomPC2 W_NomCte2 Cmb_Cptes W_Descrip Difer 
          TotC W_TGmf TotH 
      WITH FRAME F_Proc IN WINDOW W-Tras_Cuentas.
  ENABLE RECT-314 RECT-318 RECT-320 W_NitCte Tg_Pdctos W_Tipo W_AgP W_Pdcto 
         W_Cta Rs_Op W_CtaContra W_NitContra W_DocRef Cmb_Cptos W_Cargo W_Abono 
         Cmb_Cptes W_Descrip Br_PC BUTTON-5 Btn_Adic Btn_Elimina 
         Btn_Contabilizar Btn_Done W_TGmf 
      WITH FRAME F_Proc IN WINDOW W-Tras_Cuentas.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  ENABLE SalirPDF 
      WITH FRAME FormatoEfectivo IN WINDOW W-Tras_Cuentas.
  {&OPEN-BROWSERS-IN-QUERY-FormatoEfectivo}
  DISPLAY txtBeneficiario 
      WITH FRAME frmBeneficiario IN WINDOW W-Tras_Cuentas.
  ENABLE txtBeneficiario btnImprimirCheque 
      WITH FRAME frmBeneficiario IN WINDOW W-Tras_Cuentas.
  {&OPEN-BROWSERS-IN-QUERY-frmBeneficiario}
  DISPLAY w_Total 
      WITH FRAME F_Plan IN WINDOW W-Tras_Cuentas.
  ENABLE BROWSE-4 Btn_Salir-3 
      WITH FRAME F_Plan IN WINDOW W-Tras_Cuentas.
  {&OPEN-BROWSERS-IN-QUERY-F_Plan}
  DISPLAY Rs_SiNo 
      WITH FRAME F_SiImp IN WINDOW W-Tras_Cuentas.
  ENABLE RECT-315 Rs_SiNo 
      WITH FRAME F_SiImp IN WINDOW W-Tras_Cuentas.
  {&OPEN-BROWSERS-IN-QUERY-F_SiImp}
  DISPLAY Tg_ActPagare 
      WITH FRAME F_Pdctos IN WINDOW W-Tras_Cuentas.
  ENABLE RECT-319 Br_Pdctos Btn_FinImp Tg_ActPagare 
      WITH FRAME F_Pdctos IN WINDOW W-Tras_Cuentas.
  {&OPEN-BROWSERS-IN-QUERY-F_Pdctos}
  VIEW W-Tras_Cuentas.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabaAhorro W-Tras_Cuentas 
PROCEDURE GrabaAhorro :
DEFINE VAR P_ImpAplic LIKE Creditos.Cuota INITIAL 0.
DEFINE VAR P_BaseExen LIKE Creditos.Cuota INITIAL 0.
DEFINE VAR Pdo AS INTEG FORM "999".
DEFINE VAR Vigen AS INTEG FORM "9999999".
DEFINE VAR T_Gracia AS INTEG FORM "9999999".
DEFINE VAR Id_GMF AS LOGICAL INITIAL YES.
DEFINE VAR W_ValGmf LIKE Ahorros.Sdo_Disponible.

ASSIGN VrDist = 0
       W_MismoCte = FALSE
       W_VrConsig = 0.

FIND FIRST Ahorros WHERE Ahorros.Nit EQ TPC.NitP
                     AND Ahorros.Cod_Ahorro EQ TPC.CodP
                     AND Ahorros.Cue_Ahorro EQ TPC.CtaP
                     AND Ahorros.Estado EQ 1 NO-ERROR.
IF AVAIL(Ahorros) THEN
    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                             AND Pro_Ahorros.Estado EQ 1 NO-LOCK NO-ERROR.
IF NOT AVAIL(Ahorros) OR NOT AVAIL(Pro_Ahorros) THEN DO:
    MESSAGE "Falta Pro_Ahorros : " TPC.CodP SKIP
            "O Falta la Cuenta : " TPC.CtaP ",del Cliente con Cédula.: " TPC.NitP SKIP
            "Para realizar el Cargo/Abono ...?" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

FIND FIRST TempCtas WHERE TempCtas.Agen EQ Ahorros.Agencia
                      AND TempCtas.TipP EQ "A"
                      AND TempCtas.Pto  EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
IF NOT AVAIL(TempCtas) THEN DO:
    MESSAGE "Falta Configuración con Producto_Ahorro: " Ahorros.Cod_Ahorro SKIP
            "Para la Agencia : " Ahorros.Agencia VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

IF TPC.Op EQ 3 THEN DO:
    RUN Tras_EntreAgen.

    FIND FIRST Clientes WHERE Clientes.Nit EQ Tpc.NitP AND Tpc.CamAge NO-ERROR.
    IF AVAILABLE Clientes THEN
        Clientes.Agencia = Tpc.AgDest.
END.
ELSE DO:
    VrDist = (VrCargos[1] - VrAbonos[1]) + (VrCargos[2] - VrAbonos[2]) + (VrCargos[3] - VrAbonos[3]) + (VrCargos[4] - VrAbonos[4]).

    IF (VrCargos[1] - VrAbonos[1]) NE 0 THEN DO:
        IF VrCargos[1] GT 0 THEN DO:
            FIND FIRST Operacion WHERE Operacion.Cod_Operac EQ W_OpCgoAho
                                   AND Operacion.Estado EQ 1
                                   AND Operacion.Cod_Deduc GT " " NO-LOCK NO-ERROR.
            IF AVAIL(Operacion) THEN DO:
                RowidTpc = ROWID(TPC).

                RUN MismoCte. /*Retiros validar si es traslado del mismo nit y mismo pdcto*/

                FIND FIRST TPC WHERE ROWID(TPC) EQ RowidTpc NO-ERROR.
            END.

            P_BaseExen = 0.

            IF AVAIL(Operacion) AND (NOT W_MismoCte OR W_VrConsig LT VrCargos[1]) THEN DO:
                ASSIGN P_BaseExen = W_VrConsig
                       W_VrConsig = VrCargos[1] - W_VrConsig.  /*Solo SdoCta, control para Intereses Abajo*/

                IF W_VrConsig GT 0 AND NOT W_NoGmf THEN DO:
                    P_ImpAplic = 0.

                    IF Ahorros.Cod_Ahorro EQ 23 OR Comprobantes.Comprobante EQ 31 THEN.
                    ELSE DO:
                        IF ahorros.cod_ahorro NE 216 THEN DO:
                            IF W_swGMF GT 0 THEN.
                            ELSE DO:
                                RUN RutGMF.R (INPUT TRUE,
                                              INPUT W_Agencia,
                                              INPUT Ahorros.Agencia,
                                              INPUT 1,
                                              INPUT Ahorros.Cod_Ahorro,
                                              INPUT Ahorros.Nit,
                                              INPUT Ahorros.Cue_Ahorro,
                                              INPUT W_OpCgoAho,
                                              INPUT W_VrConsig,
                                              INPUT Comprobantes.Comprobante,
                                              INPUT STRING(Comprobantes.Secuencia),
                                              INPUT W_Desc,
                                              INPUT 0,
                                              INPUT 0,
                                              OUTPUT P_ImpAplic) NO-ERROR.
                                IF ERROR-STATUS:ERROR THEN DO:
                                    MESSAGE "El programa RutGMF.P...Retornò ERROR, no se permite la operaciòn."
                                        VIEW-AS ALERT-BOX ERROR.
                                    
                                    RETURN ERROR.
                                END.
                            END.
                        END.
                    END.

                    IF Ahorros.Sdo_Disponible - VrCargos[1] LT 0 AND P_ImpAplic GT 0 THEN DO:
                        VrCargos[1] = VrCargos[1] + (Ahorros.Sdo_Disponible - VrCargos[1]).

                        IF Ahorros.Sdo_Disponible + VrAbonos[1] - VrCargos[1] LT 0 THEN DO:
                            MESSAGE "La cuenta queda Sobregirada, no se permite la operaciòn..."
                                VIEW-AS ALERT-BOX ERROR.

                            RETURN ERROR.
                        END.
                    END.
                END.
            END.
        END.

        ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - (VrCargos[1] - VrAbonos[1])
               Ahorros.Fec_UltTrans = W_Fecha
               W_Oper = W_OpCgoAho
               W_Desc = "Retiro X Traslado"
               W_Valor = (VrCargos[1] - VrAbonos[1])
               W_Cta = TempCtas.CtaPro.

        CREATE movProductos.
        ASSIGN movProductos.agencia = ahorros.agencia
               movProductos.comprobante = comprobantes.comprobante
               movProductos.estado = 1
               movProductos.fecha = w_fecha
               movProductos.id_producto = ahorros.cue_ahorros
               movProductos.nit = ahorros.nit
               movProductos.num_documento = comprobantes.secuencia
               movProductos.sdo_disponible = ABSOLUTE(VrCargos[1] - VrAbonos[1])
               movProductos.tipo_producto = 1.

        IF w_valor < 0 THEN
            movProductos.tipo_transaccion = 1.
        ELSE
            movProductos.tipo_transaccion = 2.

        /* Reportar a Visionamos */
        FIND FIRST tarjetas WHERE tarjetas.nit = ahorros.nit
                              AND tarjetas.estado = '01'
                              AND tarjetas.cue_ahorros = ahorros.cue_ahorros NO-LOCK NO-ERROR.
        IF AVAILABLE tarjetas THEN DO:
            FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.

            CREATE reportarVisionamos.
            ASSIGN reportarVisionamos.fecha = w_fecha
                   reportarVisionamos.agencia = w_agencia
                   reportarVisionamos.nit = ahorros.nit
                   reportarVisionamos.numCuenta = ahorros.cue_ahorros
                   reportarVisionamos.tarjeta = tarjetas.tarjetaDB
                   reportarVisionamos.tipoCuenta = 'AH'
                   reportarVisionamos.tipoDoc = clientes.tipo_identificacion
                   reportarVisionamos.valor = w_valor
                   reportarVisionamos.usuario = w_usuario
                   reportarVisionamos.estado = 1.

            IF w_valor > 0 THEN
                reportarVisionamos.tipoTrans = 'R'.
            ELSE DO:
                reportarVisionamos.tipoTrans = 'C'.
                reportarVisionamos.valor = reportarVisionamos.valor * -1.
            END.

            RELEASE reportarVisionamos.
        END.


        /* Actualizo el sdo_pendiente en la tabla estadoAportes */
        IF ahorros.tip_ahorro = 4 THEN DO:
            FIND FIRST estadoAportes WHERE estadoAportes.nit = ahorros.nit
                                       AND estadoAportes.cod_ahorro = ahorros.cod_ahorro
                                       AND estadoAportes.cue_ahorros = ahorros.cue_ahorros
                                       AND estadoAportes.mes = MONTH(w_fecha)
                                       AND estadoAportes.ano = YEAR(w_fecha) NO-ERROR.
            IF AVAILABLE estadoAportes THEN
                estadoAportes.sdo_pendiente + (VrCargos[1] - VrAbonos[1]).
        END.
        /* ---------------------------------------------------- */

        RUN ContablesAhorro.

        IF W_Valor LT 0 THEN DO:
            ASSIGN Ahorros.Num_DepMes = Ahorros.Num_DepMes + 1
                   Ahorros.Val_DepMes = Ahorros.Val_DepMes + W_Valor * -1
                   Ahorros.Val_DepDia = Ahorros.Val_DepDia + W_Valor * -1
                   Ahorros.Num_DepDia = Ahorros.Num_DepDia + 1
                   Mov_Contable.Db = 0
                   Mov_Contable.Cr = W_Valor * -1
                   W_Oper = W_OpAboAho
                   W_Desc = "Consig.X Traslado".

            IF Ahorros.Detalle_Estado EQ 1 THEN DO: /*APERTURA DE LA CUENTA:Primera Consignacion*/
                ASSIGN Ahorros.Detalle_Estado = 2
                       Ahorros.Sdo_Inicial = W_Valor.

                IF Ahorros.Tip_Ahorro EQ 2 OR Ahorros.Tip_Ahorro EQ 3 THEN DO: /*asigna fecvencimiento para a termino y Contract*/
                    IF Ahorros.Per_Liquidacion EQ 6 AND Ahorros.Tip_Ahorro NE 2 THEN
                        ASSIGN Ahorros.Fec_ProLiquidacion = W_Fecha + Ahorros.Plazo - 1
                               Ahorros.Fec_Vencimiento = W_Fecha + Ahorros.Plazo.
                    ELSE
                        IF Ahorros.Per_Liquidacion EQ 1 THEN
                            ASSIGN Ahorros.Fec_ProLiquidacion = W_Fecha + 1
                                   Ahorros.Fec_Vencimiento = W_Fecha + Ahorros.Plazo.
                        ELSE DO:
                            RUN HallaFecProxLiq.

                            IF Ahorros.Tip_Ahorro EQ 2 THEN
                                Ahorros.Fec_Vencimiento = Ahorros.Fec_ProLiquidacion.
                            ELSE
                                RUN Halla_FVcto.
                        END.
                END.
            END.
        END.
        ELSE DO:
            ASSIGN Ahorros.Num_RetMes = Ahorros.Num_RetMes + 1
                   Ahorros.Num_RetDia = Ahorros.Num_RetDia + 1
                   Ahorros.Val_RetDia = Ahorros.Val_RetDia + W_Valor
                   Ahorros.Val_RetMes = Ahorros.Val_RetMes + W_Valor.
        END.

        RUN MovAhorro.

        IF NOT W_MismoCliente THEN DO:
            RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0, INPUT W_Oper, INPUT Ahorros.Cod_Ahorro, INPUT W_Cta, INPUT "", INPUT "CR", INPUT Ahorros.Nit, INPUT STRING(Ahorros.Cue_Ahorros),
                             INPUT STRING(Ahorros.Cue_Ahorros), INPUT 0, INPUT W_Agencia, INPUT Ahorros.Agencia, INPUT W_Agencia, INPUT STRING(Ahorros.Tip_Ahorro), INPUT W_Usuario, INPUT 0,
                             INPUT ABS(W_Valor), INPUT 0).
        END.
    END.

    /* oakley */

     IF (VrCargos[2] - VrAbonos[2]) NE 0 THEN DO:                                              
        ASSIGN Ahorros.Int_Causado = Ahorros.Int_Causado - (VrCargos[2] - VrAbonos[2])         
               W_Oper              = W_OpCgoAho
               W_Desc              = "Cargo Int_Causado X Trasl."
               W_Valor             = (VrCargos[2] - VrAbonos[2])                               
               W_Cta               = TempCtas.CtaLiq.

        CREATE movProductos.
        ASSIGN movProductos.agencia = ahorros.agencia
               movProductos.comprobante = comprobantes.comprobante
               movProductos.estado = 1
               movProductos.fecha = w_fecha
               movProductos.id_producto = ahorros.cue_ahorros
               movProductos.nit = ahorros.nit
               movProductos.num_documento = comprobantes.secuencia
               movProductos.INT_causado = ABSOLUTE(VrCargos[2] - VrAbonos[2])
               movProductos.tipo_producto = 1.

        IF VrCargos[2] - VrAbonos[2] < 0 THEN
            movProductos.tipo_transaccion = 1.
        ELSE
            movProductos.tipo_transaccion = 2.

        RUN ContablesAhorro.                                                                   
        IF W_Valor LT 0 THEN DO:
           ASSIGN Mov_Contable.Db = 0                                                          
                  Mov_Contable.Cr = W_Valor * -1
                  W_Oper          = W_OpAboAho
                  W_Desc          = "Abono Int_Causado X Trasl.".
        END.

        RUN MovAhorro.
        IF NOT W_MismoCliente THEN DO:
           RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                       INPUT Ahorros.Cod_Ahorro, INPUT W_Cta,  INPUT "",
                       INPUT "CR",               INPUT Ahorros.Nit,  INPUT STRING(Ahorros.Cue_Ahorros), 
                       INPUT STRING(Ahorros.Cue_Ahorros), INPUT 0, INPUT W_Agencia,           
                       INPUT Ahorros.Agencia, INPUT W_Agencia, INPUT STRING(Ahorros.Tip_Ahorro),            
                       INPUT W_Usuario,       INPUT 0,   INPUT ABS(W_Valor), INPUT 0).
        END.
     END.                                                                                      
     IF (VrCargos[3] - VrAbonos[3]) NE 0 THEN DO:                                              
        ASSIGN Ahorros.Int_Pagar = Ahorros.Int_Pagar - (VrCargos[3] - VrAbonos[3])             
               W_Oper            = W_OpCgoAho 
               W_Desc            = "Cargo IntXPagar X Trasl."
               W_Valor           = (VrCargos[3] - VrAbonos[3])                                 
               W_Cta             = TempCtas.CtaIng.                                                                                                                                           

        CREATE movProductos.
        ASSIGN movProductos.agencia = ahorros.agencia
               movProductos.comprobante = comprobantes.comprobante
               movProductos.estado = 1
               movProductos.fecha = w_fecha
               movProductos.id_producto = ahorros.cue_ahorros
               movProductos.nit = ahorros.nit
               movProductos.num_documento = comprobantes.secuencia
               movProductos.INT_pagar = ABSOLUTE(VrCargos[3] - VrAbonos[3])
               movProductos.tipo_producto = 1.

        IF w_valor < 0 THEN
            movProductos.tipo_transaccion = 1.
        ELSE
            movProductos.tipo_transaccion = 2.

        RUN ContablesAhorro.                                                                   
        IF W_Valor LT 0 THEN DO:
            ASSIGN Mov_Contable.Db = 0                                                          
                   Mov_Contable.Cr = W_Valor * -1
                   W_Oper          = W_OpAboAho
                   W_Desc          = "Abono IntXPagar X Trasl.".

        END.


        ELSE 
          IF Ahorros.Tip_Ahorro EQ 3 THEN DO:
           /*Nov.28/06 GAER, Para los A-Tno(Cdats) siempre Los IntXPagar cargos son grabados,Cgo-Entidad*/
            IF W_swGMF GT 0 THEN. /* jjmp 16/10/2007 */
            ELSE DO:
              RUN RutGMF.R (INPUT  TRUE,W_Agencia,Ahorros.Agencia,1,Ahorros.Cod_Ahorro,Ahorros.Nit,
                            INPUT  Ahorros.Cue_Ahorro,W_OpCgoAho,W_Valor,
                            INPUT  Comprobantes.Comprobante,
                            INPUT  STRING(Comprobantes.Secuencia),W_Desc,0,1,
                            OUTPUT P_ImpAplic) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                   MESSAGE "El programa RutGMF.P...Retornò ERROR, no se permite la operaciòn."
                                 VIEW-AS ALERT-BOX ERROR.  
                   RETURN ERROR.
              END.
            END.
          END.

        RUN MovAhorro.
        IF NOT W_MismoCliente THEN DO:
           RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                       INPUT Ahorros.Cod_Ahorro, INPUT W_Cta,  INPUT "",
                       INPUT "CR",               INPUT Ahorros.Nit,  INPUT STRING(Ahorros.Cue_Ahorros), 
                       INPUT STRING(Ahorros.Cue_Ahorros), INPUT 0, INPUT W_Agencia,           
                       INPUT Ahorros.Agencia, INPUT W_Agencia, INPUT STRING(Ahorros.Tip_Ahorro),            
                       INPUT W_Usuario,       INPUT 0,   INPUT ABS(W_Valor), INPUT 0).
        END.
     END.                                                                                      
                                                                                               
     IF (VrCargos[4] - VrAbonos[4]) NE 0 THEN DO:                                              
        ASSIGN Ahorros.Val_RetAcum = Ahorros.Val_RetAcum - (VrCargos[4] - VrAbonos[4])             
               W_Oper              = W_OpCgoAho 
               W_Desc              = "Cargo RetFuente X Trasl."
               W_Valor             = (VrCargos[4] - VrAbonos[4])                                 
               W_Cta               = TempCtas.IntAnt.                                                                                                                                           
        RUN ContablesAhorro.                                                                   
                                                                                               
        IF W_Valor LT 0 THEN DO:
           ASSIGN Mov_Contable.Db = 0                                                          
                  Mov_Contable.Cr = W_Valor * -1
                  W_Oper          = W_OpAboAho
                  W_Desc          = "Abono RetFuente X Trasl.".
        END.

        RUN MovAhorro.
        IF NOT W_MismoCliente THEN DO:
           RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                       INPUT Ahorros.Cod_Ahorro, INPUT W_Cta,  INPUT "",
                       INPUT "CR",               INPUT Ahorros.Nit,  INPUT STRING(Ahorros.Cue_Ahorros), 
                       INPUT STRING(Ahorros.Cue_Ahorros), INPUT 0, INPUT W_Agencia,           
                       INPUT Ahorros.Agencia, INPUT W_Agencia, INPUT STRING(Ahorros.Tip_Ahorro),            
                       INPUT W_Usuario,       INPUT 0,   INPUT ABS(W_Valor), INPUT 0).
        END.
     END.                                                                                      
     /*GCamacho  - Mayo06 /08 - Implementación Ahorro permanente */
     IF Ahorros.Sdo_Disponible EQ 0 OR
            (Ahorros.cod_ahorro NE 221 AND W_Cpte EQ 9) /* GCamacho controla saldo 0 Ahorro permanente con comprobate 9*/
            THEN DO:
        IF Ahorros.Int_Causado + Ahorros.Int_Pagar + Ahorros.Sdo_Canje EQ 0 THEN DO:
           MESSAGE "La Cuenta de ahorros : " Ahorros.Cue_Ahorros  SKIP
                   "queda con saldos = 0(Cero), Desea Inactivarla...?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "INACTIVAR CUENTA"
           UPDATE W_RapInac AS LOGICAL.
           IF W_RapInac THEN 
              ASSIGN Ahorros.Fec_Cancelacion    = W_Fecha
                     Ahorros.Estado             = 2
                     Ahorros.Detalle_Estado     = 9. 
        END.
        ELSE DO:
            MESSAGE "La Cuenta de ahorros : " Ahorros.Cue_Ahorros SKIP 
                    "Queda con Sdo_Disponible = Cero(0), Pero tiene Sdos.de Intereses o Sdo_Canje..." SKIP
                    "Si desea Continuar y Dejarla Activa con Saldos, TECLEE SI" SKIP
                    "Si desea Cancelar la Operación,                 TECLEE NO" 
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Saldo-Disponible en Cero(0)"
           UPDATE W_RapCero AS LOGICAL.
           IF NOT W_RapCero THEN 
              RETURN ERROR.
        END.
     END.
     IF Ahorros.Tip_Ahorro     EQ 3 AND
        Ahorros.Sdo_Disponible EQ 0 AND (Ahorros.Int_Causado + Ahorros.Int_Pagar NE 0) THEN DO:
        MESSAGE "El Sdo-Capital es Cero(0), Los intereses deben ser Cero(0)." SKIP
                "No se permite la operaciòn."
                    VIEW-AS ALERT-BOX ERROR.  
        RETURN ERROR.
     END.
  END.
  IF Ahorros.Agencia NE W_Agencia THEN 
     RUN CompletaSyA.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabaCredito W-Tras_Cuentas 
PROCEDURE GrabaCredito :
DEF VAR w_total_2 AS DEC.
DEF VAR w_distr_2 AS DEC.
DEFINE VAR pError AS LOGICAL.

ASSIGN VrDist = 0.

FIND FIRST Creditos WHERE Creditos.Nit EQ TPC.NitP
                      AND Creditos.Cod_Credito EQ TPC.CodP
                      AND Creditos.Num_Credito EQ INTEG(TPC.CtaP)
                      AND Creditos.Estado EQ 2 NO-ERROR.
IF NOT AVAIL(Creditos) THEN DO:
    MESSAGE "No se halló el Crédito vigente para Distribuir el Pago..." SKIP
            "Del Nit. : " TPC.NitP ", Cod_producto : " TPC.CodP ", Nro-Crédito : " TPC.CtaP SKIP
            "                        Revise por favor...Distribución cancelada."
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

IF NOT W_MismoCte THEN DO:
    RUN ControlLavado_Cre (INPUT 2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
END.

w_total_2 = VrAbonos[1].

FIND FIRST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                           AND CONTROL_pagos.num_credito = credito.num_credito NO-LOCK NO-ERROR.
IF NOT AVAILABLE CONTROL_pagos THEN DO:
    IF creditos.sdo_capital > 0 THEN DO:
        IF creditos.cod_credito = 123 AND creditos.fec_pago = ? THEN DO:
            creditos.cuota = ROUND((creditos.sdo_capital * (((creditos.tasa / 1200) * EXP((creditos.tasa / 1200) + 1,creditos.plazo)) / (EXP((creditos.tasa / 1200) + 1,creditos.plazo) - 1))),6).
            creditos.cuota = TRUNCATE((creditos.cuota + 100) / 100,0) * 100.

            IF DAY(w_fecha) > 16 THEN
                creditos.fec_pagAnti = ADD-INTERVAL(w_fecha,2,"months").
            ELSE
                creditos.fec_pagAnti = ADD-INTERVAL(w_fecha,1,"months").

            creditos.fec_pagAnti = DATE(MONTH(creditos.fec_pagAnti), 5, YEAR(creditos.fec_pagAnti)).
        END.

        RUN CrearControlPagos.r(INPUT creditos.nit,
                                INPUT creditos.num_credito).
    END.
END.

/*----- */
numDocAux = comprobantes.secuencia.

IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
    FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                 AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
    IF AVAILABLE bfrComprobantes THEN DO:
        bfrComprobantes.secuencia = bfrComprobantes.secuencia + 1.
        numDocAux = bfrComprobantes.secuencia.
        flagCtaSucyAg = TRUE.
    END.
END.
ELSE DO:
    IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
        FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                     AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
        IF AVAILABLE bfrComprobantes THEN
            numDocAux = bfrComprobantes.secuencia.
    END.
END.

IF TPC.Op EQ 1 OR TPC.Op EQ 4 THEN DO:
    IF VrAbonos[1] >= Creditos.Honorarios + Creditos.Costas + Creditos.Polizas + Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob + Creditos.Int_Corrientes +
                      Creditos.Int_DifCobro + Creditos.Sdo_Capital - Creditos.Int_Anticipado THEN
        w_Comple = TRUE.
    ELSE
        w_Comple = FALSE.

    IF w_Comple THEN DO:
        RUN p-pagoCredito.r (INPUT YES,
                             INPUT Creditos.Cod_Credito,
                             INPUT Creditos.Nit,
                             INPUT Creditos.Num_Credito,
                             INPUT VrAbonos[1],
                             INPUT Comprobantes.Comprobante,
                             INPUT numDocAux,
                             INPUT 0,
                             INPUT 1,
                             INPUT w_fecha,
                             INPUT YES,
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
        /*FOR EACH btcontrol_pagos WHERE tcontrol_pagos.Nit = TPC.NitP
                                   AND btcontrol_pagos.Num_Credito = integer(TPC.CtaP) BY btcontrol_pagos.Nro_Cuota:
            IF w_total_2 >= btcontrol_pagos.TOTAL_Cuota THEN DO:
                w_distr_2 = btcontrol_pagos.TOTAL_Cuota.
                P_Benefi = TRUE.
            END.
            ELSE DO:
                w_distr_2 = w_total_2.
                P_Benefi = FALSE.
            END.*/

            w_distr_2 = w_total_2.

            RUN p-pagoCredito.r (INPUT YES,
                                 INPUT Creditos.Cod_Credito,
                                 INPUT Creditos.Nit,
                                 INPUT Creditos.Num_Credito,
                                 INPUT w_distr_2,
                                 INPUT Comprobantes.Comprobante,
                                 INPUT numDocAux,
                                 INPUT 0,
                                 INPUT 1,
                                 INPUT w_fecha,
                                 INPUT YES,
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

            w_total_2 = w_total_2 - w_distr_2.

        /*    IF w_total_2 <= 0 THEN
                LEAVE.
        END.*/
    END.

    FOR EACH btcontrol_pagos WHERE tcontrol_pagos.Nit = TPC.NitP
                               AND btcontrol_pagos.Num_Credito = integer(TPC.CtaP) BY btcontrol_pagos.Nro_Cuota:
        DELETE btcontrol_pagos.
    END.

    IF /*ERROR-STATUS:ERROR OR*/ P_VlrNoDist <> 0 THEN DO:
        MESSAGE "El Programa AboCreditos.P...Retornó valor no distribuido :$" P_VlrNoDist SKIP
                "O Retornò ERROR..." SKIP
                "para el Nit. : " TPC.NitP ", Cod_producto : " TPC.CodP ", Nro-Crédito : " TPC.CtaP SKIP
                "                        Revise por favor...Distribución cancelada."
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.

    ASSIGN VrDist = P_Poliza + P_Honora + P_Costas  + P_IMora + P_IMorDifC + P_IAntic + P_ICte   + P_IDifCob + P_Capit + P_SeguroVida + P_SeguroDeudor
           VrDist = VrDist * -1.
END.
ELSE
    IF TPC.Op EQ 2 THEN DO:   /*Opción Cargos/Abonos manual*/
        ASSIGN P_Poliza = 0
               P_Honora = 0
               P_Costas = 0
               P_IMora = 0
               P_IMorDifC = 0
               P_IDifCob = 0
               P_ICte = 0
               P_IAntic = 0
               P_Capit = 0
               P_SeguroVida = 0
               P_SeguroDeudor = 0.

        RUN GrabaCredito_Manual NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR.
    END.
    ELSE
        IF TPC.Op EQ 3 THEN DO:           /*Traslado entre Agencias para Acreditar en la Origen*/
            ASSIGN P_Poliza = Creditos.Polizas
                   P_Honora = Creditos.Honorarios
                   P_Costas = Creditos.Costas
                   P_IMora = Creditos.Int_MorCobrar
                   P_IMorDifC = Creditos.Int_MoraDifCob
                   P_IDifCob = Creditos.Int_DifCobro
                   P_ICte = Creditos.Int_Corriente
                   P_IAntic = Creditos.Int_Anticipado * -1
                   P_Capit = Creditos.Sdo_Capital.

            RUN GrabaMov.  /*Al final del Procedimiento MovCreditos*/

            ASSIGN Mov_Creditos.Cod_Operacion = W_OpTrasAg
                   Mov_Creditos.Ofi_Destino = Creditos.Agencia
                   Mov_Creditos.Ofi_Fuente = Creditos.Agencia
                   Mov_Creditos.Val_Efectivo = 0.
        END.

FIND FIRST TempCtas WHERE TempCtas.Agen EQ Creditos.Agencia
                      AND TempCtas.TipP EQ "C"
                      AND TempCtas.Pto  EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.
IF NOT AVAIL(TempCtas) THEN DO:
    MESSAGE "Falta Configuración con Producto_Credito: " Creditos.Cod_Credito SKIP
            "Para la Agencia : " Creditos.Agencia
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

IF Creditos.Sdo_Capital LE 0 AND Creditos.Estado EQ 3 THEN
    FOR EACH Relaciones WHERE Relaciones.Nit EQ Creditos.Nit
                          AND INTEG(Relaciones.Cuenta) EQ Creditos.Num_Credito
                          AND Relaciones.Clase_Producto EQ 2
                          AND Relaciones.Cod_Producto EQ Creditos.Cod_Credito
                          AND Relaciones.Cod_Relacion EQ 11
                          AND Relaciones.Estado EQ 1:
        ASSIGN Relaciones.Estado = 2.
    END.

IF TPC.Op NE 1 AND TPC.Op NE 4 THEN DO:
    RUN ContablesCred NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
END.

IF TPC.Op EQ 3 THEN DO:
    ASSIGN P_Poliza = P_Poliza * -1
           P_Honora = P_Honora * -1
           P_Costas = P_Costas * -1
           P_IMora = P_IMora * -1
           P_IMorDifC = P_IMorDifC * -1
           P_IDifCob = P_IDifCob * -1
           P_ICte = P_ICte * -1
           P_IAntic = Creditos.Int_Anticipado
           P_Capit = P_Capit * -1
           Creditos.Agencia = TPC.AgDest
           P_SeguroVida = P_SeguroVida * -1
           P_SeguroDeudor = P_SeguroDeudor * -1.

    /* Trasladamos el Plan de Pagos */
    FOR EACH planPagos WHERE planPagos.nit = creditos.nit
                         AND planPagos.num_credito = creditos.num_credito:
        planPagos.agencia = TPC.AgDest.
    END.

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                             AND CONTROL_pagos.num_credito = creditos.num_credito:
        CONTROL_pagos.agencia = TPC.AgDest.
    END.

    RUN ContablesCred NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.

    FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit EQ Creditos.Nit
                             AND CONTROL_pagos.num_credito = creditos.Num_Credito:
        ASSIGN CONTROL_pagos.agencia = TPC.AgDest.
    END.

    FOR EACH Garantias WHERE Garantias.Nit EQ Creditos.Nit
                         AND Garantias.Cod_Credito EQ Creditos.Cod_Credito
                         AND Garantias.Num_Credito EQ Creditos.Num_Credito
                         AND Garantias.Estado EQ 1
                         AND Garantias.Val_Bien GT 0
                         AND Garantias.Contabil:
        ASSIGN W_Valor = Garantias.Val_Bien
               W_Cta = TempCtas.CtaGar.

        RUN OpContable.
        
        RUN OpContable.
        ASSIGN Mov_Contable.Cuenta = TempCtas.CtaCGa
               Mov_Contable.Cr = W_Valor
               Mov_Contable.Db = 0.

        RUN OpContable.
        ASSIGN Garantias.Agencia = TPC.AgDest
               Mov_Contable.Agencia = TPC.AgDest
               W_Cta = TempCtas.CtaGar.

        mov_contable.num_documento = numDocAux.
        /* ----- */

        RUN OpContable.
        ASSIGN Mov_Contable.Agencia = TPC.AgDest
               W_Cta = TempCtas.CtaCGa
               Mov_Contable.Cr = W_Valor
               Mov_Contable.Db = 0.
               
        IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
            mov_contable.num_documento = numDocAux.
        END.
    END.

    FOR EACH Mov_Creditos WHERE Mov_Creditos.Nit EQ Creditos.Nit
                            AND Mov_Creditos.Cod_Credito EQ Creditos.Cod_Credito
                            AND Mov_Creditos.Num_Credito EQ Creditos.Num_Credito:
        Mov_Creditos.Agencia = TPC.AgDest.
    END.
END.
    
IF Creditos.Agen NE W_Agencia THEN
    RUN CompletaSyA.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GrabaCredito_Manual W-Tras_Cuentas 
PROCEDURE GrabaCredito_Manual :
DEFINE VAR p_capital AS DECIMAL.
DEFINE VAR p_interes AS DECIMAL.
DEFINE VAR p_mora AS DECIMAL.
DEFINE VAR p_anticipado AS DECIMAL.
DEFINE VAR p_contingente AS DECIMAL.
DEFINE VAR p_segVida AS DECIMAL.
DEFINE VAR p_segDeudor AS DECIMAL.
DEFINE VAR flagFecPago AS LOGICAL INITIAL FALSE.
DEFINE VAR saldoProyectado AS DECIMAL.
DEFINE VAR diasPeriodo AS INTEGER.

FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_Usura AND Indicadores.Estado EQ 1 AND Indicadores.FecVcto GE W_Fecha NO-LOCK NO-ERROR.
IF NOT AVAILABLE(Indicadores) THEN DO:
    MESSAGE "Falta Indicador de Usura Vigente, No se permite el abono al Credito."
        VIEW-AS ALERT-BOX TITLE "Programa W-Tras_Cuentas.W".
    
    RETURN ERROR.
END.

W_SiCanc = "".

IF Creditos.Sdo_Capital LE 0 THEN
    ASSIGN W_SiCanc = "Pagaré Activado".  /*El pagaré estaba cancelado y lo Habilitaron*/

IF (Creditos.Sdo_Capital + (VrCargos[1] - VrAbonos[1]) LE 0) AND (Creditos.Int_Corriente + (VrCargos[2] - VrAbonos[2]) GT 0 OR
                                                                  Creditos.Int_MorCobrar  + (VrCargos[3] - VrAbonos[3]) GT 0 OR
                                                                  Creditos.Int_MoraDifCob + (VrCargos[9] - VrAbonos[9]) GT 0 OR
                                                                  Creditos.Int_Anticipado - (VrCargos[4] - VrAbonos[4]) NE 0 OR
                                                                  Creditos.Int_DifCobro + (VrCargos[5] - VrAbonos[5]) GT 0 OR
                                                                  Creditos.Costas + (VrCargos[6] - VrAbonos[6]) GT 0 OR
                                                                  Creditos.Polizas + (VrCargos[7] - VrAbonos[7]) GT 0 OR
                                                                  Creditos.Honorarios + (VrCargos[8] - VrAbonos[8]) GT 0) THEN DO:
    MESSAGE "El Sdo-Capital no puede Cancelarse mientras Hallan Otros Saldos..." SKIP
            "                                   Revise por favor..."
        VIEW-AS ALERT-BOX ERROR.

    RETURN ERROR.
END.

ASSIGN Creditos.Sdo_Capital = Creditos.Sdo_Capital + (VrCargos[1] - VrAbonos[1])
       Creditos.Int_Corriente = Creditos.Int_Corrientes + (VrCargos[2] - VrAbonos[2])
       Creditos.Int_Anticipado = Creditos.Int_Anticipado + (VrAbonos[4] - VrCargos[4])
       Creditos.Int_DifCobro = Creditos.Int_DifCobro + (VrCargos[5] - VrAbonos[5])
       Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + (VrCargos[3] - VrAbonos[3])
       Creditos.Int_MoraDifCob = Creditos.Int_MoraDifCob + (VrCargos[9] - VrAbonos[9])
       Creditos.Costas = Creditos.Costas + (VrCargos[6] - VrAbonos[6])
       Creditos.Polizas = Creditos.Polizas + (VrCargos[7] - VrAbonos[7])
       Creditos.Honorarios = Creditos.Honorarios + (VrCargos[8] - VrAbonos[8])
       creditos.seg_vida = creditos.seg_vida + (VrCargos[9] - VrAbonos[9])
       creditos.seg_Cartera = creditos.seg_Cartera + (VrCargos[10] - VrAbonos[10]).

IF ABS(VrCargos[2] - VrAbonos[2]) <> 0 THEN DO:
    CREATE movProductos.
    ASSIGN movProductos.agencia = creditos.agencia
           movProductos.comprobante = Comprobantes.Comprobante
           movProductos.estado = 1
           movProductos.fecha = w_fecha
           movProductos.id_producto = string(creditos.num_credito)
           movProductos.int_corriente = ABS(VrCargos[2] - VrAbonos[2])
           movProductos.nit = creditos.nit
           movProductos.num_documento = comprobantes.secuencia
           movProductos.tipo_producto = 2.

    IF (VrCargos[2] - VrAbonos[2]) > 0 THEN
        movProductos.tipo_transaccion = 2.
    ELSE
        movProductos.tipo_transaccion = 1.
END.

IF ABS(VrCargos[5] - VrAbonos[5]) <> 0 THEN DO:
    CREATE movProductos.
    ASSIGN movProductos.agencia = creditos.agencia
           movProductos.comprobante = Comprobantes.Comprobante
           movProductos.estado = 1
           movProductos.fecha = w_fecha
           movProductos.id_producto = string(creditos.num_credito)
           movProductos.int_difCobro = ABS(VrCargos[5] - VrAbonos[5])
           movProductos.nit = creditos.nit
           movProductos.num_documento = comprobantes.secuencia
           movProductos.tipo_producto = 2.

    IF (VrCargos[5] - VrAbonos[5]) > 0 THEN
        movProductos.tipo_transaccion = 2.
    ELSE
        movProductos.tipo_transaccion = 1.
END.

IF ABS(VrCargos[3] - VrAbonos[3]) <> 0 THEN DO:
    CREATE movProductos.
    ASSIGN movProductos.agencia = creditos.agencia
           movProductos.comprobante = Comprobantes.Comprobante
           movProductos.estado = 1
           movProductos.fecha = w_fecha
           movProductos.id_producto = string(creditos.num_credito)
           movProductos.int_mora = ABS(VrCargos[3] - VrAbonos[3])
           movProductos.nit = creditos.nit
           movProductos.num_documento = comprobantes.secuencia
           movProductos.tipo_producto = 2.

    IF (VrCargos[3] - VrAbonos[3]) > 0 THEN
        movProductos.tipo_transaccion = 2.
    ELSE
        movProductos.tipo_transaccion = 1.
END.

IF ABS(VrCargos[1] - VrAbonos[1]) <> 0 THEN DO:
    CREATE movProductos.
    ASSIGN movProductos.agencia = creditos.agencia
           movProductos.comprobante = Comprobantes.Comprobante
           movProductos.estado = 1
           movProductos.fecha = w_fecha
           movProductos.id_producto = string(creditos.num_credito)
           movProductos.nit = creditos.nit
           movProductos.num_documento = comprobantes.secuencia
           movProductos.sdo_capital = ABS(VrCargos[1] - VrAbonos[1])
           movProductos.tipo_producto = 2.

    IF (VrCargos[1] - VrAbonos[1]) > 0 THEN
        movProductos.tipo_transaccion = 2.
    ELSE
        movProductos.tipo_transaccion = 1.
END.


ASSIGN Creditos.Sdo_CapPag = Creditos.Sdo_CapPag + (VrAbonos[1] - VrCargos[1])
       Creditos.Sdo_IntMor = Creditos.Sdo_IntMor + (VrAbonos[3] + VrAbonos[9]) - (VrCargos[3] + VrCargos[9])
       Creditos.Sdo_IntPag = Creditos.Sdo_IntPag + (VrAbonos[2] + VrAbonos[4] + VrAbonos[5]) - (VrCargos[2] + VrCargos[4] + VrCargos[5])
       Creditos.Fec_UltPag = W_Fecha.

/* Devolvemos el Control_Pagos - Leonardo G. Ocampo - 26/05/2010 */
IF (VrCargos[1] - VrAbonos[1]) <> 0 OR /* sdo_capital */
   (VrCargos[2] - VrAbonos[2]) <> 0 OR /* int_corriente */
   (VrCargos[3] - VrAbonos[3]) <> 0 OR /* int_morCobrar */
   (VrCargos[4] - VrAbonos[4]) <> 0 OR /* int_Anticipado */
   (VrCargos[5] - VrAbonos[5]) <> 0 OR /* int_difCobro */
   (VrCargos[9] - VrAbonos[9]) <> 0 OR /* Seguro de vida */
   (VrCargos[10] - VrAbonos[10]) <> 0 /* Seguro de Vivienda */ THEN DO:
    p_capital = VrCargos[1] - VrAbonos[1].
    p_interes = VrCargos[2] - VrAbonos[2].
    p_mora = VrCargos[3] - VrAbonos[3].
    p_anticipado = VrAbonos[4] - VrCargos[4].
    p_contingente = VrCargos[5] - VrAbonos[5].
    p_SegVida = VrCargos[9] - VrAbonos[9].
    p_SegDeudor = VrCargos[10] - VrAbonos[10].
    
    RUN HallarPeriodo IN W_ManFin(INPUT Creditos.Per_Pago,
                                  INPUT Creditos.Plazo,
                                  OUTPUT diasPeriodo,
                                  OUTPUT P_NMeses,
                                  OUTPUT W_NroPer,
                                  OUTPUT P_NomPer).
    
    /* Capital */
    IF creditos.cod_credito <> 123 THEN DO:
        IF p_capital <> 0 THEN DO:

            saldoProyectado = creditos.monto.

            FOR EACH CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                                     AND CONTROL_pagos.num_credito = creditos.num_credito BY CONTROL_pagos.nro_cuota:
                saldoProyectado = saldoProyectado - CONTROL_pagos.pagos_capitalAcum.

                IF creditos.sdo_Capital <= saldoProyectado AND control_pagos.fec_Vcto <= w_fecha + diasPeriodo THEN DO:
                    CONTROL_pagos.id_pdoMes = 2.
                    CONTROL_pagos.cap_pagado = CONTROL_pagos.pagos_capitalAcum.
                    CONTROL_pagos.INT_pagado = CONTROL_pagos.pagos_intAcum.
                END.
                ELSE DO:
                    IF flagFecPago = FALSE THEN DO:
                        creditos.fec_pago = CONTROL_pagos.fec_Vcto.
                        flagFecPago = TRUE.
                    END.

                    CONTROL_pagos.id_pdoMes = 0.
                    CONTROL_pagos.cap_pagado = 0.
                    CONTROL_pagos.INT_pagado = 0.
                END.
            END.
        END.
    END.
END.


IF Creditos.Sdo_Capital = 0 AND
   Creditos.Int_Corriente = 0 AND
   Creditos.Int_Anticipado = 0 AND
   Creditos.Int_DifCobro = 0 AND
   Creditos.Int_MorCobrar = 0 AND
   Creditos.Int_MoraDifCob = 0 AND
   Creditos.Costas = 0 AND
   Creditos.Polizas = 0 AND
   Creditos.Honorarios = 0 AND
   creditos.cod_credito <> 123 /* Distintos al Rotativo */ THEN DO:
    ASSIGN Creditos.Fec_CanceTotal = W_Fecha
           Creditos.Fec_UltPag = W_Fecha
           Creditos.Cuo_Pagada = Creditos.Plazo
           creditos.val_atraso = 0
           creditos.dias_atraso = 0
           creditos.estado = 3.
END.

RUN MovCreditos.

FIND FIRST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                           AND CONTROL_pagos.num_credito = credito.num_credito NO-LOCK NO-ERROR.
IF NOT AVAILABLE CONTROL_pagos THEN DO:
    IF creditos.cod_credito = 123 AND creditos.fec_pago = ? THEN DO:
        creditos.cuota = ROUND((creditos.sdo_capital * (((creditos.tasa / 1200) * EXP((creditos.tasa / 1200) + 1,creditos.plazo)) / (EXP((creditos.tasa / 1200) + 1,creditos.plazo) - 1))),6).
        creditos.cuota = TRUNCATE((creditos.cuota + 100) / 100,0) * 100.

        IF DAY(w_fecha) > 16 THEN
            creditos.fec_pagAnti = ADD-INTERVAL(w_fecha,2,"months").
        ELSE
            creditos.fec_pagAnti = ADD-INTERVAL(w_fecha,1,"months").

        creditos.fec_pagAnti = DATE(MONTH(creditos.fec_pagAnti), 5, YEAR(creditos.fec_pagAnti)).
    END.
    ELSE
        RUN CrearControlPagos.r(INPUT creditos.nit,
                                INPUT creditos.num_credito).
END.

RUN ActualizarDiasDeAtraso.

IF creditos.cod_credito = 123 THEN
    RUN RevisarFactura(INPUT p_capital,
                      INPUT p_interes,
                      INPUT p_contingente,
                      INPUT p_mora).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TarDeb W-Tras_Cuentas 
PROCEDURE Grabar_TarDeb :
/*------------------------------------------------------------------------------
  Purpose:   Graba movimientos de Tarjeta debito  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER wvlrTrans  LIKE ahorros.sdo_disponible.
DEFINE INPUT PARAMETER wdesTrans AS CHARACTER.
DEFINE INPUT PARAMETER wtipoTrans AS INTEGER.
DEFINE VAR wvlrmonTD AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsecTD    AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsdoTD    AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wdisTD    AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wretcode  AS INTEGER INITIAL -1.

IF wtipotrans NE 0 THEN DO:
   FIND CURRENT ahorros NO-ERROR.
   CREATE tarjetadebito.
   ASSIGN tarjetadebito.Agencia         = w_agencia        
          tarjetadebito.Usuario         = w_usuario        
          tarjetadebito.Comprobante     = Comprobante.Comprobante
          tarjetadebito.Num_Documento   = Comprobantes.Secuencia
          tarjetadebito.Fec_Contable    = TODAY            
          tarjetadebito.Hora            = TIME             
          tarjetadebito.Comentario      = wdesTrans
          tarjetadebito.Aplicado        = NO
          tarjetadebito.ManBiometrico   = 1
          tarjetadebito.TipoTransaccion = wtipotrans
          tarjetadebito.Nit             = Ahorros.nit
          tarjetadebito.Cue_Ahorros     = Ahorros.cue_ahorros  /* SUBSTRING(Ahorros.TarjetaDB,10,7) */
          tarjetadebito.TarjetaDB       = Ahorros.TarjetaDB
          tarjetadebito.Monto           = wvlrTrans
          tarjetadebito.Secuencia       = "000000000000"
          tarjetadebito.SdoTotal        = 0
          tarjetadebito.SdoDispon       = 0
          tarjetadebito.RetCode         = -1.

   wvlrmonTD = STRING(wvlrtrans,'9999999999999').
   RUN TranWebCaja (1,wtipotrans,TRIM(ahorros.nit), TRIM(ahorros.cue_ahorros), ahorros.TarjetaDB, wvlrmonTD, 
                    INPUT-OUTPUT wsecTD, INPUT-OUTPUT wsdoTD, INPUT-OUTPUT wdisTD, OUTPUT wretcode).
   IF wretcode = 0 THEN 
      ASSIGN tarjetadebito.Secuencia = wsecTD 
             tarjetadebito.SdoTotal  = ROUND(decimal(wsdoTD) / 100,2)
             tarjetadebito.SdoDispon = ROUND(decimal(wdisTD) / 100,2)
             tarjetadebito.RetCode   = wretcode
             tarjetadebito.Aplicado  = YES.
   ELSE ASSIGN tarjetadebito.RetCode = wretcode.
   RELEASE tarjetadebito.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_Taquilla W-Tras_Cuentas 
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
   
   CREATE Taquilla.
   ASSIGN Taquilla.Autorizo         = T_Autorizo
          Taquilla.Nro_Transaccion  = Comprobantes.Secuencia
          Taquilla.Cod_Compensa     = T_Banco
          Taquilla.Cod_Operacion    = T_CodOper
          Taquilla.Cod_Producto     = T_CodPto
          Taquilla.Contabilizar     = YES
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
          Taquilla.Descripcion      = "Extra-Efectivo".
          
 IF Taquilla.Val_Cheque GT 0 THEN
    Taquilla.Descripcion = "Extra-Cheque".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hallaFecProxLiq W-Tras_Cuentas 
PROCEDURE hallaFecProxLiq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR FProxLiq AS DATE.
  DEFI VAR NMes     AS INTEG FORMAT "99".
 
  ASSIGN NMes     = MONTH(W_Fecha) + 1        /*Inicia en mensual*/         
         FProxLiq = W_Fecha.

  IF DAY(FProxLiq) EQ 31 THEN
     FProxLiq = W_Fecha - 1.

  IF Ahorros.Per_liquidacion EQ 3 THEN          /*Liq.Trimestral*/ 
     NMes = MONTH(W_Fecha) + 3.
  ELSE IF Ahorros.Per_liquidacion EQ 4 THEN     /*Liq.Semestral*/       
     NMes  = MONTH(W_Fecha) + 6.
  ELSE IF Ahorros.Per_liquidacion EQ 5 THEN       /*Liq.Anual*/  
     NMes = MONTH(W_Fecha) + 12.
  ELSE IF Ahorros.Per_liquidacion EQ 6 THEN       /*Liq.Al vcto contract.*/  
     NMes = MONTH(W_Fecha) + ROUND(Ahorros.Plazo / 30,0).

  IF NMes GE 13 THEN DO:
     ASSIGN NMes = NMes - 12.

     IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
        Ahorros.Fec_ProLiquidacion = DATE(NMes,28,YEAR(FProxLiq) + 1) - 1.
     ELSE
        Ahorros.Fec_ProLiquidacion = DATE(NMes,DAY(FProxLiq),YEAR(FProxLiq) + 1) - 1.
  END.
  ELSE DO:
     IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
        Ahorros.Fec_ProLiquidacion = DATE(NMes,28,YEAR(FProxLiq)) - 1.       
     ELSE
        Ahorros.Fec_ProLiquidacion = DATE(NMes,DAY(FProxLiq),YEAR(FProxLiq)) - 1.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaPdcto W-Tras_Cuentas 
PROCEDURE HallaPdcto :
IF W_Tipo EQ "2" OR W_Tipo EQ "C" THEN DO:
    FIND FIRST Creditos WHERE Creditos.Agencia EQ W_AgP
                          AND Creditos.Nit EQ W_NitCte
                          AND Creditos.Cod_Credito EQ W_Pdcto
                          AND Creditos.Num_Credito EQ INTEG(W_Cta)
                          AND Creditos.Estado EQ 2 NO-LOCK NO-ERROR.
    IF NOT AVAIL(Creditos) THEN DO:
        MESSAGE "El Crédito solicitado para la operación, No Existe Vigente...Revise por favor."
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.
END.
ELSE DO:
    FIND FIRST Ahorros WHERE Ahorros.Agencia EQ W_AgP
                         AND Ahorros.Nit EQ W_NitCte
                         AND Ahorros.Cod_Ahorro EQ W_Pdcto
                         AND Ahorros.Cue_Ahorro EQ W_Cta
                         /*AND Ahorros.Estado EQ 1*/ NO-LOCK NO-ERROR.
    IF NOT AVAIL(Ahorros) THEN DO:
        MESSAGE "La Cta-Ahorro solicitada para la operación, No Existe Vigente...Revise por favor."
            VIEW-AS ALERT-BOX ERROR.

        RETURN ERROR.
    END.

    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
    IF Pro_Ahorros.Tip_Ahorro EQ 4 THEN DO:
        ASSIGN Cmb_Cptos:SENSITIVE IN FRAME F_Proc = FALSE WHEN AVAIL(Pro_Ahorros).

        ASSIGN W_Cpto = "SdoCta"
               W_Cpto:SCREEN-VALUE = "SdoCta".
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_FVcto W-Tras_Cuentas 
PROCEDURE Halla_FVcto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR NMes      AS INTEG FORMAT "99"  INIT 0.
  DEFI VAR FProxVcto AS DATE.
  DEFI VAR NroM      AS INTEG FORMAT "99"  INIT 0.
  DEFI VAR FContVcto AS DATE.
 
  ASSIGN FProxVcto = W_Fecha + Ahorros.Plazo 
         FContVcto = W_Fecha 
         NMes      = MONTH(W_Fecha)
         NroM      = ROUND(Ahorros.Plazo / 30,0).  

  IF DAY(W_Fecha) EQ 31 THEN
     FContVcto = W_Fecha - 1.

  IF Ahorros.Plazo GT 31 THEN DO:        
     ASSIGN NMes = Nmes + NroM.

     IF NMes GE 25 THEN DO:                                                            
        ASSIGN NMes = NMes - 24.

        IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
           FProxVcto = DATE(NMes,28,YEAR(W_Fecha) + 2).
        ELSE 
           FProxVcto = DATE(NMes,DAY(FContVcto),YEAR(W_Fecha) + 2). 
     END.
     ELSE IF NMes GE 13 THEN DO:                                                            
        ASSIGN NMes = NMes - 12.

        IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
           FProxVcto = DATE(NMes,28,YEAR(W_Fecha) + 1).  
        ELSE 
           FProxVcto = DATE(NMes,DAY(FContVcto),YEAR(W_Fecha) + 1).
     END.
     ELSE DO:                                                                          
        IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
           FProxVcto = DATE(NMes,28,YEAR(W_Fecha)). 
        ELSE 
           FProxVcto = DATE(NMes,DAY(FContVcto),YEAR(W_Fecha)).
     END.

     ASSIGN Ahorros.Fec_Vencimiento = FProxVcto.
  END.
  ELSE DO:
     NMes = Nmes + 1.

     IF NMes GE 13 THEN DO:                                                            
        ASSIGN NMes = NMes - 12.

        IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
           Ahorros.Fec_Vencimiento = DATE(NMes,28,YEAR(W_Fecha) + 1).  
        ELSE 
           Ahorros.Fec_Vencimiento = DATE(NMes,DAY(FContVcto),YEAR(W_Fecha) + 1).
     END.
     ELSE DO:                                                                          
        IF NMes EQ 2 AND DAY(W_Fecha) GE 29 THEN
           Ahorros.Fec_Vencimiento = DATE(NMes,28,YEAR(W_Fecha)). 
        ELSE
           Ahorros.Fec_Vencimiento = DATE(NMes,DAY(FContVcto),YEAR(W_Fecha)). 
     END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpError W-Tras_Cuentas 
PROCEDURE ImpError :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR Listado AS CHAR FORM "X(40)".                                                                   
                                                                                                                
  ASSIGN Listado = W_PathSpl + "TCError-" + STRING(Comprobantes.Secuencia)  + ".Lst"
         W_SiImpErr = TRUE.  
  {Incluido\Imprimir.I "listado"}

  W_SiImpErr = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImprimeCpte W-Tras_Cuentas 
PROCEDURE ImprimeCpte :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR Listado AS CHAR FORM "X(40)".                                                                   
                                                                                                                
  Listado = W_PathSpl + "TCCont-" + STRING(W_Agencia) + STRING(W_Usuario) + STRING(Comprobantes.Secuencia)  + ".Lst".                
                                                                                                            
  {Incluido\Imprimir.I "listado"}                                              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LlenaPdctos W-Tras_Cuentas 
PROCEDURE LlenaPdctos :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vlsiga AS LOGICAL INITIAL FALSE NO-UNDO .
  FOR EACH TPdctos: DELETE TPdctos. END.
  CLOSE QUERY Br_Pdctos.
        
  FOR EACH Ahorros WHERE Ahorros.Nit       EQ W_NitCte
                     AND Ahorros.Estado    EQ 1 NO-LOCK BY Ahorros.Cod_Ahorro:
      FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                               AND Pro_Ahorros.Estado     EQ 1 NO-LOCK NO-ERROR. 
      CREATE TPdctos.
      ASSIGN TPdctos.CodP     = Ahorros.Cod_Ahorro
             TPdctos.TP       = "A"
             TPdctos.Estado   = 1
             TPdctos.Agen     = Ahorros.Agencia
             TPdctos.CtaP     = Ahorros.Cue_Ahorro
             TPdctos.FApert   = Ahorros.Fec_Apertura
             TPdctos.Cuota    = Ahorros.Cuota
             TPdctos.VrSdo    = Ahorros.Sdo_Dispon     /*- Pro_Ahorros.Val_SdoMinimo*/
             TPdctos.IntCte   = Ahorros.Int_Pagar
             TPdctos.IntCau   = Ahorros.Int_Causado
             TPdctos.Canje    = Ahorros.Sdo_canje
             TPdctos.SMinimo  = Ahorros.Sdo_Minimo
             TPdctos.NomP     = Pro_Ahorros.Nom_Produc
             TPdctos.FecPxDb  = Ahorros.Fec_ProxDeb
             TPdctos.ForPag   = Ahorros.FOR_Pago
             TPdctos.NroTarDB = Ahorros.TarjetaDB
             TPdctos.VrSdo    = Ahorros.Sdo_Dispon.
             TPdctos.TipoPro  = ahorros.tip_ahorro.

    
       IF ahorros.TarjetaDB NE "" THEN DO:
          DEFINE VAR wvlrmon  AS CHARACTER INITIAL '0000000000000'.
          DEFINE VAR wsec     AS CHARACTER INITIAL '0000000000000'.
          DEFINE VAR wsdo     AS CHARACTER INITIAL '0000000000000'.
          DEFINE VAR wdis     AS CHARACTER INITIAL '0000000000000'.
          DEFINE VAR wretcode AS INTEGER INITIAL -1.

      RUN TranWebCaja(1,0,TRIM(ahorros.nit), TRIM(ahorros.cue_Ahorros), ahorros.TarjetaDB, wvlrmon, INPUT-OUTPUT wsec, INPUT-OUTPUT wsdo, INPUT-OUTPUT wdis, OUTPUT wretcode).

      IF wretcode = 0 THEN
         ASSIGN TPdctos.VrSdo = ROUND(DECIMAL(wsdo) / 100,2)
                TPdctos.NomP  = TRIM(TPdctos.NomP) + " TDB:ON".
      ELSE DO:
         MESSAGE "Red electronica  NO  RESPONDE ....." SKIP(0)
                 "El Saldo Actual, es  el  registrado" SKIP(0)
                 "en la COOPERATIVA ... Se Recomienda" SKIP(0)
                 "NO REALIZAR Operaciones de Retiro."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ASSIGN TPdctos.VrSdo = Ahorros.Sdo_Dispon
                   TPdctos.NomP  = TRIM(TPdctos.NomP) + " TDB:OFF". /*- Pro_Ahorros.Val_SdoMinimo*/
         END.
      END.


  END.

  FOR EACH Creditos WHERE 
           Creditos.Nit    EQ W_NitCte AND
           Creditos.Estado EQ 2 NO-LOCK BY Creditos.Cod_Credito:
      ASSIGN vlsiga = TRUE.
      IF (Creditos.Cod_Credito EQ 57 OR Creditos.Cod_Credito EQ 56) AND Creditos.Fec_Bloqueo NE ? THEN
         ASSIGN vlsiga = FALSE.

      IF vlsiga THEN DO:
         FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito
                                   AND Pro_Creditos.Estado      EQ 1 NO-LOCK NO-ERROR. 
         w1_preliquida = sdo_capital * W1_diapreliquida * (creditos.tasa / 36000).
         /*MESSAGE "int " w1_preliquida VIEW-AS ALERT-BOX.*/
  
         CREATE TPdctos.
         ASSIGN TPdctos.CodP     = Creditos.Cod_Credito
                TPdctos.TP       = "C"
                TPdctos.Estado   = 2
                TPdctos.CtaP     = STRING(Creditos.Num_Credito)
                TPdctos.Pagare   = Creditos.Pagare
                TPdctos.Agen     = Creditos.Agencia
                TPdctos.FApert   = Creditos.Fec_Desemb
                TPdctos.Cuota    = Creditos.Cuota
                TPdctos.VrSdo    = Creditos.Honorarios    + Creditos.Costas + Creditos.Polizas +
                                   Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob +          
                                   Creditos.Int_DifCobro  + Creditos.Int_Corrientes +
                                   Creditos.Sdo_Capital   - Creditos.Int_Anticipado + w1_preliquida
                TPdctos.NomP     = Pro_Creditos.Nom_Produc
                TPdctos.FecPxDb  = Creditos.Fec_Pago
                TPdctos.ForPag   = Creditos.FOR_Pago
                Tpdctos.TipoPro  = Creditos.Tip_credito. 
      END.
      ELSE
          MESSAGE "El Crédito de Cupo Rotativo: " + STRING(Creditos.Num_Credito) SKIP 
                  "Esta Bloqueado....No Sera Cargado."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
    
  OPEN QUERY Br_Pdctos FOR EACH TPdctos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MismoCte W-Tras_Cuentas 
PROCEDURE MismoCte :
/*------------------------------------------------------------------------------
  Purpose:  Solo Retorna TRUE si el traslado para ahorros es del mismo cliente,
            y el vr.de consig.
 -----------------------------------------------------------------------------*/
  ASSIGN W_VrConsig = 0
         W_MismoCte = FALSE.
                  
  /*Para A-lavista, Contractuales y Aportes*/
  IF Ahorros.Tip_Ahorro NE 3 THEN   
     FOR EACH TPC NO-LOCK WHERE TPC.TP   EQ "A"
                         AND TPC.NitP    EQ Ahorros.Nit
                         AND TPC.Abono   GT 0
                         AND TPC.TipA    EQ Ahorros.Tip_Ahorro
                         AND NOT TPC.IdSiGmf:     
         ASSIGN W_VrConsig  = W_VrConsig + TPC.Abono
                TPC.IdSiGmf = TRUE
                W_MismoCte  = TRUE.
  END.
  ELSE FOR EACH TPC NO-LOCK WHERE TPC.TP EQ "A"   
                         AND TPC.NitP    EQ Ahorros.Nit
                         AND TPC.Abono   GT 0
                         AND TPC.TipA    EQ 3
                         AND NOT TPC.IdSiGmf: 
         ASSIGN W_VrConsig  = W_VrConsig + TPC.Abono
                TPC.IdSiGmf = TRUE
                W_MismoCte  = TRUE.
  END.

  FIND FIRST Tpc WHERE ROWID(Tpc) EQ RowidTpc NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MismoCteQuery W-Tras_Cuentas 
PROCEDURE MismoCteQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN W_VrConsig = 0
         W_MismoCte = FALSE.
                  
  FOR EACH TPC NO-LOCK WHERE TPC.TP     EQ "A"
                         AND TPC.NitP    EQ Ahorros.Nit
                         AND TPC.Abono   GT 0
                         AND TPC.TipA    EQ Ahorros.Tip_Ahorro:     
         ASSIGN W_VrConsig  = W_VrConsig + TPC.Abono
                W_MismoCte  = TRUE.
  END.
  
  FIND FIRST Tpc WHERE ROWID(Tpc) EQ RowidTpc NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MovAhorro W-Tras_Cuentas 
PROCEDURE MovAhorro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
       /*
       IF NOT W_MismoCte THEN DO:
      RUN ControlLavado_Aho (INPUT 1) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
         RETURN ERROR.
   END.
   */

  CREATE Mov_Ahorros.
  ASSIGN Mov_Ahorros.Agencia        = Ahorros.Agencia
         Mov_Ahorros.Age_Destino    = Ahorros.Agencia                   
         Mov_Ahorros.Age_Fuente     = W_Agencia                          
         Mov_Ahorros.Cod_Ahorro     = Ahorros.Cod_Ahorro                        
         Mov_Ahorros.Cue_Ahorros    = Ahorros.Cue_Ahorro                 
         Mov_Ahorros.Fecha          = W_Fecha                            
         Mov_Ahorros.Hora           = TIME                               
         Mov_Ahorros.Nit            = Ahorros.Nit                        
         Mov_Ahorros.Num_Documento  = STRING(Comprobantes.Secuencia)     
         Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
         Mov_Ahorros.Usuario        = W_Usuario                          
         Mov_Ahorros.Val_Efectivo   = W_Valor
         Mov_Ahorros.Cod_Operacion  = W_Oper
         Mov_Ahorros.Cpte           = Comprobantes.Comprobante
         Mov_Ahorros.Descrip        = Tpc.Descrip.
         
  IF W_Valor LT 0 THEN 
     Mov_Ahorros.Val_Efectivo = W_Valor * -1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MovCreditos W-Tras_Cuentas 
PROCEDURE MovCreditos :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
    IF (VrAbonos[7] - VrCargos[7]) NE 0 THEN DO:
       ASSIGN W_Valor  = (VrAbonos[7] - VrCargos[7])
              P_Poliza = (VrAbonos[7] - VrCargos[7])
              W_Oper   = W_OpAboCred[6]
              W_Desc   = "Abono Polizas X Trasl.".

       RUN GrabaMov. /*Al final de este mismo procedimiento*/
       IF W_Valor LT 0 THEN
          ASSIGN Mov_Creditos.Val_Efectivo  = W_Valor * -1
                 Mov_Creditos.Cod_Operacion = W_OpCgoCred[3]
                 Mov_Creditos.Descrip       = W_SiCanc + "Cgo.Polizas X Trasl."
                 W_Oper                     = W_OpCgoCred[3].
       IF NOT W_MismoCliente THEN DO:
          RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                      INPUT Creditos.Cod_Credito, INPUT W_Cta,  INPUT "",
                      INPUT "CR", INPUT Creditos.Nit,  INPUT STRING(Creditos.Num_Credito), 
                      INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                      INPUT Creditos.Agencia, INPUT W_Agencia, INPUT STRING(Creditos.Tip_Credito),            
                      INPUT W_Usuario, INPUT 0, INPUT ABS(W_Valor), INPUT 0).
       END.
    END.

    IF (VrAbonos[6] - VrCargos[6]) NE 0 THEN DO:
       ASSIGN W_Valor  = (VrAbonos[6] - VrCargos[6])
              P_Costas = (VrAbonos[6] - VrCargos[6])
              W_Oper   = W_OpAboCred[8]
              W_Desc   = "Abono CostasJ.X Trasl.".

       RUN GrabaMov.
       IF W_Valor LT 0 THEN
          ASSIGN Mov_Creditos.Val_Efectivo  = W_Valor * -1
                 Mov_Creditos.Cod_Operacion = W_OpCgoCred[4]
                 Mov_Creditos.Descrip       = W_SiCanc + "Cgo.CostasJ.X Trasl."
                 W_Oper                     = W_OpCgoCred[4].
       IF NOT W_MismoCliente THEN DO:
          RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                      INPUT Creditos.Cod_Credito, INPUT W_Cta,  INPUT "",
                      INPUT "CR", INPUT Creditos.Nit,  INPUT STRING(Creditos.Num_Credito), 
                      INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                      INPUT Creditos.Agencia, INPUT W_Agencia, INPUT STRING(Creditos.Tip_Credito),            
                      INPUT W_Usuario, INPUT 0, INPUT ABS(W_Valor), INPUT 0).
       END.
    END.

    IF (VrAbonos[8] - VrCargos[8]) NE 0 THEN DO:
       ASSIGN W_Valor  = (VrAbonos[8] - VrCargos[8])
              P_Honora = (VrAbonos[8] - VrCargos[8])
              W_Oper   = W_OpAboCred[7]
              W_Desc   = "Abono Honorarios X Trasl.".

       RUN GrabaMov. 
       IF W_Valor LT 0 THEN
          ASSIGN Mov_Creditos.Val_Efectivo  = W_Valor * -1
                 Mov_Creditos.Cod_Operacion = W_OpCgoCred[2]
                 W_Oper                     = W_OpCgoCred[2]
                 Mov_Creditos.Descrip       = W_SiCanc + "Cgo.Honorarios X Trasl.".
       IF NOT W_MismoCliente THEN DO:
          RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                      INPUT Creditos.Cod_Credito, INPUT W_Cta,  INPUT "",
                      INPUT "CR", INPUT Creditos.Nit,  INPUT STRING(Creditos.Num_Credito), 
                      INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                      INPUT Creditos.Agencia, INPUT W_Agencia, INPUT STRING(Creditos.Tip_Credito),            
                      INPUT W_Usuario, INPUT 0, INPUT ABS(W_Valor), INPUT 0).
       END.
    END.
    
    IF (VrAbonos[9] - VrCargos[9]) NE 0 THEN DO:
       ASSIGN W_Valor     = (VrAbonos[9] - VrCargos[9])
              P_IMorDifC  = (VrAbonos[9] - VrCargos[9])
              W_Oper      = W_OpAboCred[2]
              W_Desc      = "Abono Mora-DifCob X Trasl.".

       RUN GrabaMov.
       IF W_Valor LT 0 THEN
          ASSIGN Mov_Creditos.Val_Efectivo  = W_Valor * -1
                 Mov_Creditos.Cod_Operacion = W_OpCgoCred[5]
                 W_Oper                     = W_OpCgoCred[5]
                 Mov_Creditos.Descrip       = W_SiCanc + "Cgo.Mora-DifCob X Trasl.".
       IF NOT W_MismoCliente THEN DO:
          RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                      INPUT Creditos.Cod_Credito, INPUT W_Cta,  INPUT "",
                      INPUT "CR", INPUT Creditos.Nit,  INPUT STRING(Creditos.Num_Credito), 
                      INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                      INPUT Creditos.Agencia, INPUT W_Agencia, INPUT STRING(Creditos.Tip_Credito),            
                      INPUT W_Usuario, INPUT 0, INPUT ABS(W_Valor), INPUT 0).
       END.
    END.

    IF (VrAbonos[3] - VrCargos[3]) NE 0 THEN DO:
       ASSIGN W_Valor = (VrAbonos[3] - VrCargos[3])
              P_IMora = (VrAbonos[3] - VrCargos[3])
              W_Oper  = W_OpAboCred[2]
              W_Desc  = "Abono Int.Mora X Trasl.".

       RUN GrabaMov.
       IF W_Valor LT 0 THEN
          ASSIGN Mov_Creditos.Val_Efectivo  = W_Valor * -1
                 Mov_Creditos.Cod_Operacion = W_OpCgoCred[5]
                 W_Oper                     = W_OpCgoCred[5]
                 Mov_Creditos.Descrip       = W_SiCanc + "Cgo.Int.Mora X Trasl.".
       IF NOT W_MismoCliente THEN DO:
          RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                      INPUT Creditos.Cod_Credito, INPUT W_Cta,  INPUT "",
                      INPUT "CR", INPUT Creditos.Nit,  INPUT STRING(Creditos.Num_Credito), 
                      INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                      INPUT Creditos.Agencia, INPUT W_Agencia, INPUT STRING(Creditos.Tip_Credito),            
                      INPUT W_Usuario, INPUT 0, INPUT ABS(W_Valor), INPUT 0).
       END.
    END.

    IF (VrAbonos[2] - VrCargos[2]) NE 0 THEN DO:
       ASSIGN W_Valor = (VrAbonos[2] - VrCargos[2])
              P_ICte  = (VrAbonos[2] - VrCargos[2])
              W_Oper  = W_OpAboCred[3]
              W_Desc  = "Abono Int.Ctes.X Trasl.".

       RUN GrabaMov.
       IF W_Valor LT 0 THEN
          ASSIGN Mov_Creditos.Val_Efectivo  = W_Valor * -1
                 Mov_Creditos.Cod_Operacion = W_OpCgoCred[5]
                 W_Oper                     = W_OpCgoCred[5]
                 Mov_Creditos.Descrip       = W_SiCanc + "Cgo.Int.Ctes.X Trasl.".
       IF NOT W_MismoCliente THEN DO:
          RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                      INPUT Creditos.Cod_Credito, INPUT W_Cta,  INPUT "",
                      INPUT "CR", INPUT Creditos.Nit,  INPUT STRING(Creditos.Num_Credito), 
                      INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                      INPUT Creditos.Agencia, INPUT W_Agencia, INPUT STRING(Creditos.Tip_Credito),            
                      INPUT W_Usuario, INPUT 0, INPUT ABS(W_Valor), INPUT 0).
       END.
    END.

    IF (VrAbonos[5] - VrCargos[5]) NE 0 THEN DO:
       ASSIGN W_Valor   = (VrAbonos[5] - VrCargos[5])
              P_IDifCob = (VrAbonos[5] - VrCargos[5])
              W_Oper    = W_OpAboCred[4]
              W_Desc    = "Abono Int.DifCobro.X Trasl.".

       RUN GrabaMov.
       IF W_Valor LT 0 THEN
          ASSIGN Mov_Creditos.Val_Efectivo  = W_Valor * -1
                 Mov_Creditos.Cod_Operacion = W_OpCgoCred[5]
                 W_Oper                     = W_OpCgoCred[5]
                 Mov_Creditos.Descrip       = W_SiCanc + "Cgo.Int.DifCobro X Trasl.".
       IF NOT W_MismoCliente THEN DO:
          RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                      INPUT Creditos.Cod_Credito, INPUT W_Cta,  INPUT "",
                      INPUT "CR", INPUT Creditos.Nit,  INPUT STRING(Creditos.Num_Credito), 
                      INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                      INPUT Creditos.Agencia, INPUT W_Agencia, INPUT STRING(Creditos.Tip_Credito),            
                      INPUT W_Usuario, INPUT 0, INPUT ABS(W_Valor), INPUT 0).
       END.
    END.

    IF (VrAbonos[1] - VrCargos[1]) NE 0 THEN DO:
       ASSIGN W_Valor = (VrAbonos[1] - VrCargos[1])
              P_Capit = (VrAbonos[1] - VrCargos[1])
              W_Oper  = W_OpAboCred[1]
              W_Desc  = "Abono Capital.X Trasl.".

       RUN GrabaMov.
       IF W_Valor LT 0 THEN
          ASSIGN Mov_Creditos.Val_Efectivo  = W_Valor * -1
                 Mov_Creditos.Cod_Operacion = W_OpCgoCred[1]
                 W_Oper                     = W_OpCgoCred[1]
                 Mov_Creditos.Descrip       = W_SiCanc + "Cgo.Capital X Trasl.".
       IF NOT W_MismoCliente THEN DO:
          RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                      INPUT Creditos.Cod_Credito, INPUT W_Cta,  INPUT "",
                      INPUT "CR", INPUT Creditos.Nit,  INPUT STRING(Creditos.Num_Credito), 
                      INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                      INPUT Creditos.Agencia, INPUT W_Agencia, INPUT STRING(Creditos.Tip_Credito),            
                      INPUT W_Usuario, INPUT 0, INPUT ABS(W_Valor), INPUT 0).
       END.
    END.

    IF (VrAbonos[4] - VrCargos[4]) NE 0 THEN DO:
       ASSIGN W_Valor  = (VrAbonos[4] - VrCargos[4])
              P_IAntic = (VrAbonos[4] - VrCargos[4]) 
              W_Oper   = W_OpAboCred[5]
              W_Desc   = "Abono Int.Anticip.X Trasl.".

       RUN GrabaMov.
       IF W_Valor LT 0 THEN
          ASSIGN Mov_Creditos.Val_Efectivo  = W_Valor * -1
                 Mov_Creditos.Cod_Operacion = W_OpCgoCred[5]
                 W_Oper                     = W_OpCgoCred[5]
                 Mov_Creditos.Descrip       = W_SiCanc + "Cgo.Int.Antic.X Trasl.".
       IF NOT W_MismoCliente THEN DO:
          RUN Gra_Taquilla(INPUT W_Usuario, INPUT 0,   INPUT W_Oper, 
                      INPUT Creditos.Cod_Credito, INPUT W_Cta,  INPUT "",
                      INPUT "CR", INPUT Creditos.Nit,  INPUT STRING(Creditos.Num_Credito), 
                      INPUT STRING(Creditos.Num_Credito), INPUT 0, INPUT W_Agencia,           
                      INPUT Creditos.Agencia, INPUT W_Agencia, INPUT STRING(Creditos.Tip_Credito),            
                      INPUT W_Usuario, INPUT 0, INPUT ABS(W_Valor), INPUT 0).
       END.
    END.                                        
                                                      
  END PROCEDURE.                                  
                                                  
  /*--------------------------*/
  PROCEDURE GrabaMov:
    CREATE Mov_Creditos.
    ASSIGN Mov_Creditos.Agencia       = Creditos.Agencia
           Mov_Creditos.Cod_Credito   = Creditos.Cod_Credito
           Mov_Creditos.Nit           = Creditos.Nit
           Mov_Creditos.Num_Credito   = Creditos.Num_Credito
           Mov_Creditos.Cod_Operacion = W_Oper
           Mov_Creditos.Ofi_Destino   = Creditos.Agencia
           Mov_Creditos.Ofi_Fuente    = W_Agencia
           Mov_Creditos.Pagare        = Creditos.Pagare
           Mov_Creditos.Fecha         = W_Fecha
           Mov_Creditos.Hora          = vTime
           Mov_Creditos.Num_Documento = STRING (Comprobantes.Secuencia)
           Mov_Creditos.Usuario       = W_Usuario
           Mov_Creditos.Int_Corriente = Creditos.Int_Corriente
           Mov_Creditos.Int_MorCobrar = Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
           Mov_Creditos.Sdo_Capital   = Creditos.Sdo_Capital
           Mov_Creditos.Val_Efectivo  = W_Valor
           Mov_Creditos.Cpte          = Comprobantes.Comprobante
           Mov_Creditos.Descrip       = Tpc.Descrip.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpContable W-Tras_Cuentas 
PROCEDURE OpContable :
DEFINE VAR numDocAux AS INTEGER.

/*----- */
numDocAux = comprobantes.secuencia.

IF TPC.Age <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
    FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.Age
                                 AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
    IF AVAILABLE bfrComprobantes THEN DO:
        bfrComprobantes.secuencia = bfrComprobantes.secuencia + 1.
        numDocAux = bfrComprobantes.secuencia.
        flagCtaSucyAg = TRUE.
    END.
END.
ELSE DO:
    IF TPC.Age <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
        FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.Age
                                     AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
        IF AVAILABLE bfrComprobantes THEN
            numDocAux = bfrComprobantes.secuencia.
    END.
END.
/* ----- */


CREATE Mov_Contable.
ASSIGN Mov_Contable.Agencia = TPC.Age
       Mov_Contable.Cuenta = W_Cta
       Mov_Contable.Nit = TPC.NitP
       Mov_Contable.Fec_Contable = W_Fecha
       Mov_Contable.Comentario = TPC.Descrip
       Mov_Contable.Usuario = W_Usuario
       Mov_Contable.Cen_Costos = W_Cencosgral
       Mov_Contable.Destino = W_Agencia
       Mov_Contable.Comprobante = Comprobantes.Comprobante
       Mov_Contable.Num_Documento = /*Comprobantes.Secuencia*/ numDocAux
       Mov_Contable.Doc_Refer = TPC.DocRef
       Mov_Contable.Fec_Grabacion = TODAY
       Mov_Contable.Hora = TIME
       Mov_Contable.Estacion = W_Estacion
       Mov_Contable.Db = W_Valor.

IF TPC.age <> w_agencia THEN
    mov_Contable.enlace = STRING(comprobantes.secuencia).

IF Rs_Op = 2 THEN
    mov_contable.comentario = "TM/"+ mov_contable.comentario.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE periodos30 W-Tras_Cuentas 
PROCEDURE periodos30 :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Tras_Cuentas 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
 ------------------------------------------------------------------------------*/
DEFI VAR totD AS DECIMAL INIT 0.
DEFI VAR TotC AS DECIMAL INIT 0.
DEFI VAR TTotD AS DECIMAL INIT 0.
DEFI VAR TTotC AS DECIMAL INIT 0.
DEFI VAR I AS INTEG FORM "999".

IF NOT W_SiImpErr THEN
    RETURN.

{Incluido\RepEncabezado.I}

 ASSIGN W_Reporte = "Cpte Resumen : ERROR Contabilización de Traslados      Fecha del Informe: " +
                     STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
        W_EncColumna = "Comprobante: " + STRING(Comprobantes.Comprobante,"99") + "-" + 
                       STRING(Comprobantes.Secuencia,"99999999") + "-" + Comprobantes.Nombre +
                       "    " + W_Descrip.

 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.

 FIND LAST Agencias WHERE Agencias.Agencia GT 0 AND Agencias.Estado NE 3 NO-LOCK NO-ERROR.
 DO I = 1 TO Agencias.Agencia:
    FOR EACH Mov_Contable WHERE Mov_Contable.Agencia    EQ I
                         AND Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                         AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia NO-LOCK
                         BY Mov_Contable.Cuenta BY Mov_Contable.Nit:
        IF  Mov_Contable.Fec_Contable EQ W_Fecha 
        AND Mov_Contable.Usuario      EQ W_Usuario THEN DO:
            ASSIGN TotD  = TotD  + Mov_Contable.Db
                   TTotD = TTotD + Mov_Contable.Db
                   TotC  = TotC  + Mov_Contable.Cr
                   TTotC = TTotC + Mov_Contable.Cr.

            FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta                      
                                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.                         
            DISPLAY Mov_Contable.Agencia   LABEL "Ag."                                             
                    Mov_Contable.Cuenta       LABEL "Cta-Contable"                                    
                    Cuentas.Nombre            LABEL "Descripciòn de la Cuenta" WHEN AVAIL(Cuentas)
                    Mov_Contable.Nit          LABEL "Ced/Nit"  FORM "999999999999"                                    
                    Mov_Contable.Doc_Refer    LABEL "Doc-Refer"  FORM "99999999"                                  
                    Mov_Contable.Db           LABEL "TOTAL DEBITOS"  FORM "->>>>,>>>,>>9.99"    
                    Mov_Contable.Cr           LABEL "TOTAL CREDITOS" FORM "->>>>,>>>,>>9.99"  SKIP(0)  
                WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.                 
        END.
    END.
 END.

 DISPLAY SKIP(1)
         "                     TOTAL FINAL------------>                                       ------------------ ------------------"
         SKIP
         "                                                                                   "
         TTotD      FORM "->>>>>,>>>,>>9.99"
         TTotC      FORM "->>>>>,>>>,>>9.99"
            WITH DOWN WIDTH 180 FRAME FT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.
            
/*
 DEFINE VAR W_Rpta   AS LOGICAL.

 FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                         AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                         AND Mov_Contable.Fec_Contable  EQ W_Fecha
                         AND Mov_Contable.Usuario       EQ W_Usuario NO-LOCK
                             BREAK BY Mov_Contable.Agencia:
     IF FIRST-OF(Mov_Contable.Agencia) THEN DO:
        FIND Formatos WHERE Formatos.Agencia     EQ Mov_Contable.Agencia
                        AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato NO-LOCK NO-ERROR.

        IF AVAILABLE(Formatos) THEN
           RUN VALUE(Formatos.Nom_Proceso) (INPUT Comprobantes.comprobante,
                                            INPUT Comprobantes.Secuencia, INPUT Comprobantes.Secuencia,
                                            INPUT Mov_Contable.Agencia).
        ELSE
           RUN MostrarMensaje IN W_Manija (INPUT 345, OUTPUT W_Rpta).
     END.
 END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QUERY_TPC W-Tras_Cuentas 
PROCEDURE QUERY_TPC :
/*------------------------------------------------------------------------------
  Purpose:     
  ------------------------------------------------------------------------------*/
  CLOSE QUERY Br_PC.

  ASSIGN TOTC  = 0
         TotH  = 0
         W_TGmf = 0.

  FOR EACH TPC:
      IF TPC.Gmf GT 0 THEN DO:
         FIND FIRST Ahorros WHERE Ahorros.Nit     EQ TPC.NitP                     
                           AND Ahorros.Cod_Ahorro EQ TPC.CodP                     
                           AND Ahorros.Cue_Ahorro EQ TPC.CtaP                                             
                           AND Ahorros.Estado     EQ 1 NO-LOCK NO-ERROR.  
         ASSIGN RowidTpc = ROWID(Tpc).

         RUN MismoCteQuery.    /*Retiros validar si es traslado del mismo nit y mismo pdcto*/     
         IF NOT W_MismoCte OR W_VrConsig LT TPC.Cargo THEN 
            ASSIGN W_VrConsig = TPC.Cargo - W_VrConsig                         
                   TPC.Gmf    = ROUND(W_VrConsig * .004,0).       
         ELSE 
            ASSIGN TPC.Gmf = 0.
      END.

      ASSIGN TotC = TotC + TPC.Cargo
             TotH = TotH + TPC.Abono
             W_TGmf = W_TGmf + TPC.Gmf.

      IF TPC.Contra GT " " THEN 
         ASSIGN TotH = TotH + TPC.Cargo
                TotC = TotC + TPC.Abono.        
  END.

  OPEN QUERY Br_PC FOR EACH TPC.

  ASSIGN Rs_Op:SCREEN-VALUE IN FRAME F_Proc = "1"
         Rs_Op                              = 1
         Cmb_Cptos:SCREEN-VALUE             = "SdoCta"         
         Cmb_Cptos:SENSITIVE                = FALSE            
         W_Cpto:SCREEN-VALUE                = "SdoCta"         
         W_Cpto                             = "SdoCta"         
         W_CtaContra:SCREEN-VALUE           = " "              
         W_Cargo:SCREEN-VALUE               = "0"              
         W_Abono:SCREEN-VALUE               = "0"              
         W_AgP:SCREEN-VALUE                 = STRING(W_Agencia)
         W_Cta:SCREEN-VALUE                 = " "              
         W_AgDest:SCREEN-VALUE              = "0"  
         W_NitContra                        = ""
         W_NitContra:SCREEN-VALUE           = ""
         W_DocRef:SCREEN-VALUE              = ""
         W_NomCte2:SCREEN-VALUE             = ""
         W_NomCte2
         W_DocRef
         W_Cargo                   
         W_Abono                   
         W_CtaContra               
         W_AgP                     
         W_Cta                     
         W_AgDest.                 

  ASSIGN TotC:SCREEN-VALUE    = STRING(TotC)
         TotH:SCREEN-VALUE    = STRING(TotH)
         W_TGmf:SCREEN-VALUE  = STRING(W_TGmf)
         Difer:SCREEN-VALUE   = STRING(TotC - TotH - W_TGmf)
         Difer.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reclasificarContingentes W-Tras_Cuentas 
PROCEDURE reclasificarContingentes :
DEFINE VAR contingenteAntes AS DECIMAL.
DEFINE VAR pTasaMaxima AS DECIMAL.
DEFINE VAR pDiasAtraso AS INTEGER.
DEFINE VAR pFlagContingentes AS LOGICAL.
DEFINE VAR pInteresPeriodo AS DECIMAL.
DEFINE VAR pDiasParaContingente AS INTEGER.
DEFINE VAR pNumCuota AS INTEGER.
DEFINE VAR pFechaAux AS DATE.
DEFINE VAR aReclasificar AS DECIMAL.

RUN HallarPeriodo IN W_ManFin(INPUT Creditos.Per_Pago,
                              INPUT Creditos.Plazo,
                              OUTPUT W_NroDia,
                              OUTPUT P_NMeses,
                              OUTPUT W_NroPer,
                              OUTPUT P_NomPer).

pFlagContingentes = FALSE.
pDiasParaContingente = 0.
pNumCuota = 0.

/* Calculo los intereses causados por las cuotas pendientes */
FOR EACH CONTROL_pagos WHERE control_pagos.Nit = creditos.nit
                         AND control_pagos.Num_Credito = creditos.num_credito
                         AND control_pagos.Fec_Vcto <= TODAY
                         AND control_pagos.Id_PdoMes < 2 BREAK BY control_pagos.Num_Credito BY control_pagos.Fec_Vcto:
    IF FIRST-OF(CONTROL_pagos.num_credito) THEN DO:
        pDiasAtraso = w_fecha - CONTROL_pagos.fec_Vcto.

        IF pDiasAtraso > Pro_Creditos.per_garPer THEN
            pFlagContingentes = TRUE.
    END.

    IF pFlagContingentes = TRUE THEN DO:
        pDiasParaContingente = pDiasParaContingente + W_NroDia.

        IF pDiasParaContingente <= pro_creditos.per_GarPer + 30 THEN DO:
            IF CONTROL_pagos.contingente > 0 AND control_pagos.causacion = 0 THEN DO:
                CONTROL_pagos.causacion = CONTROL_pagos.contingente.
                CONTROL_pagos.contingente = 0.
            END.
        END.
    END.
    ELSE DO:
        IF CONTROL_pagos.contingente > 0 AND control_pagos.causacion = 0 THEN DO:
            CONTROL_pagos.causacion = CONTROL_pagos.contingente.
            CONTROL_pagos.contingente = 0.
        END.
    END.
    
    pFechaAux = CONTROL_pagos.fec_vcto.
    pNumCuota = CONTROL_pagos.Nro_Cuota.
END.

IF creditos.dias_atraso = 0 THEN DO:
    FIND LAST CONTROL_pagos WHERE CONTROL_pagos.nit = creditos.nit
                               AND CONTROL_pagos.num_credito = creditos.num_credito
                               AND CONTROL_pagos.fec_Vcto <= TODAY USE-INDEX ppal4 NO-LOCK NO-ERROR.
    IF AVAILABLE CONTROL_pagos THEN
        pNumCuota = CONTROL_pagos.nro_Cuota.
END.

/* Calculo los intereses pendientes de la última fecha de vencimiento a hoy */
FIND FIRST CONTROL_pagos WHERE control_pagos.Nit = creditos.nit
                           AND control_pagos.Num_Credito = creditos.num_credito
                           AND CONTROL_pagos.nro_cuota = pNumCuota + 1 NO-ERROR.
IF AVAILABLE CONTROL_pagos THEN DO:
    IF pFlagContingentes = FALSE THEN DO:
        IF CONTROL_pagos.contingente > 0 AND CONTROL_pagos.causacion = 0 THEN DO:
            CONTROL_pagos.causacion = CONTROL_pagos.contingente.
            CONTROL_pagos.contingente = 0.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RevisarFactura W-Tras_Cuentas 
PROCEDURE RevisarFactura :
DEFINE INPUT PARAMETER capitalAdevolver AS DECIMAL FORMAT "->>>,>>>,>>9.99".
DEFINE INPUT PARAMETER intAdevolver AS DECIMAL FORMAT "->>>,>>>,>>9.99".
DEFINE INPUT PARAMETER intDifCobroAdevolver AS DECIMAL FORMAT "->>>,>>>,>>9.99".
DEFINE INPUT PARAMETER moraAdevolver AS DECIMAL FORMAT "->>>,>>>,>>9.99".

DEFINE VAR vMora AS DECIMAL.
DEFINE VAR vInteres AS DECIMAL.
DEFINE VAR vIntDifCobro AS DECIMAL.
DEFINE VAR vCapital AS DECIMAL.

OUTPUT TO VALUE("\\192.168.1.101\Aplicacion\Obj\Reportes\logs\TrasladoInternoManual_Ag" + STRING(w_agencia,"99") + "_Usu" + w_usuario + STRING(TIME) + ".txt").

/* Corrección */
IF capitalAdevolver > 0 OR intAdevolver > 0 OR intDifCobroAdevolver > 0 OR moraAdevolver > 0 THEN DO:
    MESSAGE "Se está realizando una reversión/Avance. Desea que se reactiven las facturas" SKIP
            "ya pagas de acuerdo a la operación que se está realizando?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar reapertura de facturas" UPDATE flagAbrirFacturas AS LOGICAL.

    IF flagAbrirFacturas = TRUE THEN DO:
        DISPLAY creditos.nit capitalAdevolver intAdevolver moraAdevolver "Corrección" WITH WIDTH 200.

        vCapital = capitalAdevolver.
        vInteres = intAdevolver.
        vIntDifCobro = intDifCobroAdevolver.
        vMora = moraAdevolver.

        FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                               AND facturacion.num_credito = creditos.num_credito BY facturacion.fec_pago DESCENDING:
            IF vCapital <= facturacion.pago_capital THEN DO:
                facturacion.pago_capital = facturacion.pago_capital - vCapital.
                vCapital = 0.
            END.
            ELSE DO:
                vCapital = vCapital - facturacion.pago_capital.
                facturacion.pago_capital = 0.
            END.

            IF vInteres <= facturacion.pago_intCorriente THEN DO:
                facturacion.pago_intCorriente = facturacion.pago_intCorriente - vInteres.
                vInteres = 0.
            END.
            ELSE DO:
                vInteres = vInteres - facturacion.pago_intCorriente.
                facturacion.pago_intCorriente = 0.
            END.

            IF vIntDifCobro <= facturacion.pago_intDifCobro THEN DO:
                facturacion.pago_intDifCobro = facturacion.pago_intDifCobro - vIntDifCobro.
                vIntDifCobro = 0.
            END.
            ELSE DO:
                vIntDifCobro = vIntDifCobro - facturacion.pago_intDifCobro.
                facturacion.pago_intDifCobro = 0.
            END.

            IF vMora <= facturacion.pago_mora THEN DO:
                facturacion.pago_mora = facturacion.pago_mora - vMora.
                vMora = 0.
            END.
            ELSE DO:
                vMora = vMora - facturacion.pago_mora.
                facturacion.pago_mora = 0.
            END.

            facturacion.estado = 1.

            creditos.fec_pago = facturacion.fec_pago.

            IF vCapital <= 0 AND vInteres <= 0 AND vIntDifCobro <= 0 AND vMora <= 0 THEN
                LEAVE.
        END.
    END.

    /* Reportar a Visionamos */
    FIND FIRST tarjetas WHERE tarjetas.nit = creditos.nit
                          AND tarjetas.estado = '01'
                          AND tarjetas.num_credito = creditos.num_credito NO-LOCK NO-ERROR.
    IF AVAILABLE tarjetas THEN DO:
        FIND FIRST clientes WHERE clientes.nit = creditos.nit NO-LOCK NO-ERROR.

        CREATE reportarVisionamos.
        ASSIGN reportarVisionamos.fecha = w_fecha
               reportarVisionamos.agencia = w_agencia
               reportarVisionamos.nit = creditos.nit
               reportarVisionamos.numCuenta = STRING(creditos.num_credito)
               reportarVisionamos.tarjeta = tarjetas.tarjetaDB
               reportarVisionamos.tipoCuenta = 'CR'
               reportarVisionamos.tipoDoc = clientes.tipo_identificacion
               reportarVisionamos.tipoTrans = 'R'
               reportarVisionamos.valor = capitalAdevolver
               reportarVisionamos.usuario = w_usuario
               reportarVisionamos.estado = 1.

        RELEASE reportarVisionamos.
    END.
END.
ELSE DO: /*Pago de Crédito */
    DISPLAY creditos.nit capitalAdevolver intAdevolver moraAdevolver "Pago" WITH WIDTH 200.

    capitalAdevolver = capitalAdevolver * -1.
    intAdevolver = intAdevolver * -1.
    moraAdevolver = moraAdevolver * -1.

    vMora = moraAdevolver.
    vInteres = intAdevolver.
    vIntDifCobro = intDifCobroAdevolver.
    vCapital = capitalAdevolver.

    FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                           AND facturacion.num_credito = creditos.num_credito
                           AND facturacion.estado = 1 BY facturacion.fec_pago:
        IF vMora >= facturacion.int_mora - facturacion.pago_mora THEN DO:
            vMora = vMora - (facturacion.int_mora - facturacion.pago_mora).
            facturacion.pago_mora = facturacion.int_mora.
        END.
        ELSE DO:
            facturacion.pago_mora = facturacion.pago_mora + vMora.
            vMora = 0.
        END.

        IF vIntDifCobro >= facturacion.int_difCobro - facturacion.pago_intdifCobro THEN DO:
            vIntDifCobro = vIntDifCobro - (facturacion.int_difCobro - facturacion.pago_intdifCobro).
            facturacion.pago_intdifCobro = facturacion.int_difCobro.
        END.
        ELSE DO:
            facturacion.pago_intDifCobro = facturacion.pago_intDifCobro + vIntDifCobro.
            vIntDifCobro = 0.
        END.

        IF vInteres >= facturacion.int_corriente - facturacion.pago_intCorriente THEN DO:
            vInteres = vInteres - (facturacion.int_corriente - facturacion.pago_intCorriente).
            facturacion.pago_intCorriente = facturacion.int_corriente.
        END.
        ELSE DO:
            facturacion.pago_intCorriente = facturacion.pago_intCorriente + vINteres.
            vInteres = 0.
        END.

        IF vCapital >= facturacion.capital - facturacion.pago_capital THEN DO:
            vCapital = vCapital - (facturacion.capital - facturacion.pago_capital).
            facturacion.pago_capital = facturacion.capital.
        END.
        ELSE DO:
            facturacion.pago_capital = facturacion.pago_capital + vCapital.
            vCapital = 0.
        END.

        IF facturacion.int_mora - facturacion.pago_mora <= 0 AND facturacion.INT_corriente - facturacion.pago_intCorriente <= 0 AND facturacion.capital - facturacion.pago_capital <= 0 THEN DO:
            facturacion.estado = 2.

            creditos.fec_pago = ADD-INTERVAL(w_fecha,1,"months").
            creditos.fec_pago = DATE(MONTH(creditos.fec_pago),5,YEAR(creditos.fec_pago)).
        END.
    END.

    creditos.val_atraso = 0.
    creditos.cuo_atraso = 0.
    creditos.dias_atraso = 0.
    creditos.sdo_proyectado = creditos.sdo_capital + facturacion.pago_capital - facturacion.capital.

    FOR EACH facturacion WHERE facturacion.nit = creditos.nit
                           AND facturacion.num_credito = creditos.num_credito
                           AND facturacion.fec_pago <= w_fecha
                           AND facturacion.estado = 1 NO-LOCK BY facturacion.fec_pago DESCENDING:
        creditos.fec_pago = facturacion.fec_pago.
        creditos.val_atraso = creditos.val_atraso + facturacion.capital - facturacion.pago_capital.

        IF creditos.val_atraso < 0 THEN
            creditos.val_atraso = 0.

        creditos.cuo_atraso = creditos.cuo_atraso + 1.
        creditos.sdo_proyectado = creditos.sdo_Capital - creditos.val_atraso.
    END.

    IF creditos.fec_pago <= w_fecha THEN
        creditos.dias_atraso = w_fecha - creditos.fec_pago.
    ELSE
        creditos.sdo_proyectado = creditos.sdo_capital.
    
END.

OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TrasAg_Ahorro W-Tras_Cuentas 
PROCEDURE TrasAg_Ahorro :
DEFINE VAR numDocAux AS INTEGER.

ASSIGN W_Valor = Ahorros.Sdo_Disponible
       W_Cta = TempCtas.CtaPro.

RUN ContablesAhorro. /*Debito en Origen*/
IF W_Valor < 0 THEN DO:
    ASSIGN Mov_Contable.Db = 0
           Mov_Contable.Cr = W_Valor * -1.
END.

RUN ContablesAhorro. /*Crédito en Destino*/
ASSIGN Mov_Contable.Agencia = TPC.AgDest
       Mov_Contable.Db = 0
       Mov_Contable.Cr = W_Valor.

/*----- */
numDocAux = comprobantes.secuencia.

IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
    FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                 AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
    IF AVAILABLE bfrComprobantes THEN DO:
        bfrComprobantes.secuencia = bfrComprobantes.secuencia + 1.
        numDocAux = bfrComprobantes.secuencia.
        flagCtaSucyAg = TRUE.
    END.
END.
ELSE DO:
    IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
        FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                     AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
        IF AVAILABLE bfrComprobantes THEN
            numDocAux = bfrComprobantes.secuencia.
    END.
END.

mov_contable.num_documento = numDocAux.

/* ----- */


IF W_Valor LT 0 THEN DO:
     ASSIGN Mov_Contable.Cr = 0                       
            Mov_Contable.Db = W_Valor * -1.
END.

IF Ahorros.Int_Causado NE 0 THEN DO:
    ASSIGN W_Valor = Ahorros.Int_Causado             
           W_Cta   = TempCtas.CtaLiq.

    RUN ContablesAhorro. /*Debito en Origen*/
    IF W_Valor LT 0 THEN DO:
        ASSIGN Mov_Contable.Db = 0
               Mov_Contable.Cr = W_Valor * -1.
    END.

    RUN ContablesAhorro. /*Crédito en Destino*/ 
    ASSIGN Mov_Contable.Agencia = TPC.AgDest
           Mov_Contable.Db = 0
           Mov_Contable.Cr = W_Valor.

    /*----- */
    numDocAux = comprobantes.secuencia.

    IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
        FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                     AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
        IF AVAILABLE bfrComprobantes THEN DO:
            bfrComprobantes.secuencia = bfrComprobantes.secuencia + 1.
            numDocAux = bfrComprobantes.secuencia.
            flagCtaSucyAg = TRUE.
        END.
    END.
    ELSE DO:
        IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
            FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                         AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
            IF AVAILABLE bfrComprobantes THEN
                numDocAux = bfrComprobantes.secuencia.
        END.
    END.

    mov_contable.num_documento = numDocAux.

    /* ----- */


    IF W_Valor LT 0 THEN DO:
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = W_Valor * -1.
    END.
END.

IF Ahorros.Int_Pagar NE 0 THEN DO:
    ASSIGN W_Valor = Ahorros.Int_Pagar
           W_Cta = TempCtas.CtaIng.

    RUN ContablesAhorro. /*Debito en Origen*/
    IF W_Valor LT 0 THEN DO:
        ASSIGN Mov_Contable.Db = 0
               Mov_Contable.Cr = W_Valor * -1.
    END.

    RUN ContablesAhorro. /*Crédito en Destino*/
    ASSIGN Mov_Contable.Agencia = TPC.AgDest
           Mov_Contable.Db = 0
           Mov_Contable.Cr = W_Valor.

    /*----- */
    numDocAux = comprobantes.secuencia.

    IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
        FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                     AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
        IF AVAILABLE bfrComprobantes THEN DO:
            bfrComprobantes.secuencia = bfrComprobantes.secuencia + 1.
            numDocAux = bfrComprobantes.secuencia.
            flagCtaSucyAg = TRUE.
        END.
    END.
    ELSE DO:
        IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
            FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                         AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
            IF AVAILABLE bfrComprobantes THEN
                numDocAux = bfrComprobantes.secuencia.
        END.
    END.

    mov_contable.num_documento = numDocAux.
    
    /* ----- */

    IF W_Valor LT 0 THEN DO:
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = W_Valor * -1.
    END.
END.

IF Ahorros.Val_RetAcum NE 0 THEN DO:
    ASSIGN W_Valor = Ahorros.Val_RetAcum
           W_Cta = TempCtas.IntAnt.

    RUN ContablesAhorro. /*Debito en Origen*/
    IF W_Valor LT 0 THEN DO:
        ASSIGN Mov_Contable.Db = 0
               Mov_Contable.Cr = W_Valor * -1.
    END.

    RUN ContablesAhorro. /*Crédito en Destino*/
    ASSIGN Mov_Contable.Agencia = TPC.AgDest
           Mov_Contable.Db = 0
           Mov_Contable.Cr = W_Valor.

    /*----- */
    numDocAux = comprobantes.secuencia.

    IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = FALSE THEN DO:
        FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                     AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
        IF AVAILABLE bfrComprobantes THEN DO:
            bfrComprobantes.secuencia = bfrComprobantes.secuencia + 1.
            numDocAux = bfrComprobantes.secuencia.
            flagCtaSucyAg = TRUE.
        END.
    END.
    ELSE DO:
        IF TPC.AgDest <> w_agencia AND flagCtaSucyAg = TRUE THEN DO:
            FIND FIRST bfrComprobantes WHERE bfrComprobantes.agencia = TPC.AgDest
                                         AND bfrComprobantes.comprobante = comprobantes.comprobante NO-ERROR.
            IF AVAILABLE bfrComprobantes THEN
                numDocAux = bfrComprobantes.secuencia.
        END.
    END.

    mov_contable.num_documento = numDocAux.
        
    /* ----- */


    IF W_Valor < 0 THEN DO:
        ASSIGN Mov_Contable.Cr = 0
               Mov_Contable.Db = W_Valor * -1.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tras_EntreAgen W-Tras_Cuentas 
PROCEDURE Tras_EntreAgen :
DEFINE VAR newCuenta AS CHARACTER.

FIND FIRST bAhorros WHERE bAhorros.agencia = TPC.AgDest
                      AND bAhorros.tip_ahorro = ahorros.tip_ahorro
                      AND bAhorros.cod_ahorro = ahorros.cod_ahorro
                      AND bAhorros.cue_ahorros = ahorros.cue_ahorros NO-LOCK NO-ERROR.
IF AVAILABLE bAhorros THEN DO:
    FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro = ahorros.cod_ahorro
                             AND Pro_Ahorros.Tip_Ahorro = ahorros.tip_ahorro NO-ERROR.

    Pro_Ahorros.Num_Consecutivo = Pro_Ahorros.Num_Consecutivo + 1.
    newCuenta = STRING(Pro_Ahorros.Num_Consecutivo).
END.
ELSE
    newCuenta = ahorros.cue_ahorros.

ASSIGN VrDist = Ahorros.Sdo_Disponible + Ahorros.Int_Causado + Ahorros.Int_Pagar + Ahorros.Val_RetAcum
       VrDist = VrDist * -1.

RUN TrasAg_Ahorro.

RUN MovAhorro.

ASSIGN Mov_Ahorros.Cod_Operacion = W_OpTrasAg
       Mov_Ahorros.Age_Fuente = Ahorros.Agencia
       Mov_Ahorros.Age_Destino = Ahorros.Agencia
       Mov_Ahorros.Val_Efectivo = 0.

IF ahorros.tip_ahorro = 4 THEN DO:
    FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-ERROR.
    IF AVAILABLE clientes THEN
        clientes.agencia = TPC.AgDest.
END.

FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Nit = Ahorros.Nit
                       AND Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
                       AND Mov_Ahorros.Cue_Ahorro = Ahorros.Cue_Ahorro:
    Mov_Ahorros.Agencia = TPC.AgDest.
    mov_ahorros.cue_ahorros = newCuenta.
END.

FOR EACH Lib_Chequera WHERE Lib_Chequera.Cod_Producto = Ahorros.Cod_Ahorro
                        AND Lib_Chequera.Cue_Ahorros = Ahorros.Cue_Ahorro:
    Lib_Chequera.Agencia = TPC.AgDest.
    Lib_Chequera.Cue_Ahorros = newCuenta.
END.

FOR EACH Che_Transito WHERE Che_Transito.Tip_Producto = "1"
                        AND Che_Transito.Cod_Producto = Ahorros.Cod_Ahorro
                        AND Che_Transito.Num_Cuenta = Ahorros.Cue_Ahorro:
    Che_Transito.Agencia = TPC.AgDest.
    Che_Transito.num_cuenta = newCuenta.
END.

ahorros.cue_ahorros = newCuenta.
Ahorros.Agencia = TPC.AgDest.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida W-Tras_Cuentas 
PROCEDURE Valida :
DEFINE VAR xagencia LIKE cortolargo.agencia INITIAL 1.

FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK BY Pro_Ahorros.Cod_Ahorro:
    DO xagencia = 1 TO 100:
        FOR EACH CortoLargo WHERE cortolargo.agencia = xagencia
                              AND CortoLargo.Clase_Producto EQ 1
                              AND CortoLargo.Cod_Producto EQ Pro_Ahorros.Cod_Ahorro
                              AND CortoLargo.Plazo_Inicial GE 0 NO-LOCK BREAK BY CortoLargo.Agencia
                                                                              BY CortoLargo.Cod_Producto
                                                                              BY CortoLargo.Plazo_Inicial:
            IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
                CREATE TempCtas.
                ASSIGN TempCtas.Age = CortoLargo.Agencia
                       TempCtas.TipP = "A"
                       TempCtas.Pto = CortoLargo.Cod_Producto
                       TempCtas.CtaPro = CortoLargo.Cta_AsoAd
                       TempCtas.CtaSyA = CortoLargo.Cta_SyA.

                IF Pro_Ahorros.Tip_Ahorro GE 4 THEN
                    NEXT.

                FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 1
                                       AND Liqui_Int.Cod_Producto EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
                IF NOT AVAIL(Liqui_Int) THEN DO:
                    MESSAGE "Falta Liqui_Int Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro SKIP
                        VIEW-AS ALERT-BOX ERROR.

                    RETURN ERROR.
                END.

                ASSIGN TempCtas.CtaLiq = Liqui_Int.Cta_CauCr         /*Los Causados*/
                       TempCtas.CtaIng = Liqui_Int.CtaCr_LiqAso      /*Los Por Pagar*/
                       TempCtas.IntAnt = Liqui_Int.CtaCr_Ret.        /*Ret-Fuente*/
            END.
        END.
    END.
END.

/* oakley */

/*IF AVAILABLE creditos THEN DO:*/
    FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK BY Pro_Creditos.Cod_Credito:
        DO xagencia = 1 TO 100:
            FOR EACH CortoLargo WHERE cortolargo.agencia = xagencia
                                  AND CortoLargo.Clase_Producto EQ 2
                                  AND CortoLargo.Cod_Producto EQ Pro_Creditos.Cod_Credito
                                  AND CortoLargo.Plazo_Inicial GE 0 NO-LOCK BREAK BY CortoLargo.Agencia
                                                                                  BY CortoLargo.Cod_Producto
                                                                                  BY CortoLargo.Plazo_Inicial:
                IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
                    CREATE TempCtas.
                    ASSIGN TempCtas.Age = CortoLargo.Agencia
                           TempCtas.TipP = "C"
                           TempCtas.Pto = CortoLargo.Cod_Producto
                           TempCtas.CtaSyA = CortoLargo.Cta_SyA
                           TempCtas.CtaHon = CortoLargo.Cta_HonorariosDB
                           TempCtas.CtaPol = CortoLargo.Cta_PolizasDB
                           TempCtas.CtaCos = CortoLargo.Cta_CostasDB
                           TempCtas.CtaGar = CortoLargo.Cta_VigGarAd
                           TempCtas.CtaCGa = CortoLargo.Cta_ContrapartidaGar.

                    /*IF creditos.FOR_pago = 2 THEN*/
                        TempCtas.CtaPro = CortoLargo.Cta_AsoAd.
                        TempCtas.CtaProCaja = CortoLargo.Cta_NoaAd.
                    /*ELSE
                        TempCtas.CtaPro = CortoLargo.Cta_NoaAd.*/
                    
                    FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2
                                           AND Liqui_Int.Cod_Producto EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
                    IF NOT AVAIL(Liqui_Int) THEN DO:
                        MESSAGE "Falta Liqui_Int Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito SKIP
                            VIEW-AS ALERT-BOX ERROR.

                        /*RETURN ERROR.*/
                    END.

                    /*IF creditos.FOR_pago = 2 THEN*/
                        ASSIGN TempCtas.CtaLiq = Liqui_Int.CtaDb_LiqAso
                               TempCtas.CtaIng = Liqui_Int.CtaCr_LiqAso
                               TempCtas.IntAnt = Liqui_Int.CtaInt_AntAso
                               TempCtas.IntMor = Liqui_Int.CtaDb_MoraAso
                               TempCtas.DifCoD = Liqui_Int.CtaDb_DifCobAso
                               TempCtas.DifCoH = Liqui_Int.CtaCr_DifCobAso
                               TempCtas.Oper = Liqui_Int.Cod_Operacion
                               TempCtas.MorIng = Liqui_Int.CtaCr_MoraAso.
                    /*ELSE*/
                        ASSIGN TempCtas.CtaLiqCaja = Liqui_Int.CtaDb_Liq
                               TempCtas.CtaIngCaja = Liqui_Int.CtaCr_Liq
                               TempCtas.IntAntCaja = Liqui_Int.CtaInt_Ant
                               TempCtas.IntMorCaja = Liqui_Int.CtaDb_Mora
                               TempCtas.DifCoDCaja = Liqui_Int.CtaDb_DifCob
                               TempCtas.DifCoHCaja = Liqui_Int.CtaCr_DifCob
                               TempCtas.MorIngCaja = Liqui_Int.CtaCr_Mora.
                END.
            END.
        END.
    END.
/*END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidaAhorro W-Tras_Cuentas 
PROCEDURE ValidaAhorro :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  IF  W_Cpto EQ "SdoCta" 
  AND W_Cargo GT 0
  AND W_Cargo GT (Ahorros.Sdo_Dispon - Pro_Ahorros.Val_SdoMinimo) THEN DO:
     MESSAGE "Informativo: El Retiro + Sdo_Minimo es Superior al Saldo : $" Ahorros.Sdo_Dispon
             VIEW-AS ALERT-BOX TITLE "SOLO INFORMATIVO".
     /*RETURN ERROR.*/
  END. 

  IF  W_Cpto  EQ "SdoCta" 
  AND W_Cargo GT 0
  AND W_Cargo GT Ahorros.Sdo_Dispon THEN DO:
     MESSAGE "No es posible El Retiro, es Superior al Saldo : $" Ahorros.Sdo_Dispon
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END. 

  IF W_Cpto EQ "IntXPagar" AND W_Cargo GT Ahorros.Int_Pagar THEN DO:
     MESSAGE "El Retiro para IntXPagar- Ahorros es Superior al Sdo-IntXPagar: $" Ahorros.INT_Pagar
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END. 

  IF W_Cpto EQ "IntCausado" AND W_Cargo GT Ahorros.Int_Causado THEN DO:
     MESSAGE "El Retiro para IntCausado-Ahorros es Superior al Sdo-IntCausado: $" Ahorros.Int_Causado
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END. 

  IF W_Cpto EQ "RetFuente" AND W_Cargo GT Ahorros.Val_RetAcum THEN DO:
     MESSAGE "El Retiro para RetFuente-Ahorros es Superior al Sdo-RetFuente: $" Ahorros.Val_RetAcum
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END. 

  IF Rs_Op EQ 4 THEN DO:
     IF Ahorros.Sdo_Canje NE 0 THEN DO:
        MESSAGE "La cuenta tiene Sdo_Canje no es posible la cancelación de la Cuenta."
             VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
     END.

    /* IF W_Cargo NE Ahorros.Sdo_Disponible + Ahorros.Int_Pagar + Ahorros.Int_Causado THEN DO:
         MESSAGE "La cuenta no queda con Saldos = a 0(Cero), no es posible la cancelación de la Cuenta."
             VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
     END.*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidaCredito W-Tras_Cuentas 
PROCEDURE ValidaCredito :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  IF W_Cpto EQ "Honorarios" AND W_Abono GT Creditos.Honorarios THEN DO:
     MESSAGE "El Abono para Honorarios es Superior al Sdo-Honorarios : $" Creditos.Honorarios
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END. 

  IF W_Cpto EQ "Costas" AND W_Abono GT Creditos.Costas THEN DO:
     MESSAGE "El Abono para Costas es Superior al Sdo-Costas : $" Creditos.Costas
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END. 

  IF W_Cpto EQ "Polizas" AND W_Abono GT Creditos.Polizas THEN DO:
     MESSAGE "El Abono para Polizas es Superior al Sdo-Polizas : $" Creditos.Polizas
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END. 

  IF W_Cpto EQ "IntMora" AND W_Abono GT Creditos.Int_MorCobrar  THEN DO:
     MESSAGE "El Abono para Int.X Mora es Superior al Sdo-Int.Mora : $" Creditos.Int_MorCobrar 
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END. 

  IF W_Cpto EQ "Int-Ctes" AND W_Abono GT Creditos.Int_Corrientes THEN DO:
     MESSAGE "El Abono para Int.Corriente es Superior al Sdo-Int.Ctes : $" Creditos.Int_Corrientes
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END. 

  IF W_Cpto EQ "IntDifCob" AND W_Abono GT Creditos.Int_DifCobro THEN DO:
     MESSAGE "El Abono para Int.Dif.Cobro es Superior al Sdo-DifCobro : $" Creditos.Int_DifCobro
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.
  
  IF W_Cpto EQ "MoraDifCob" AND W_Abono GT Creditos.Int_MoraDifCob THEN DO:
     MESSAGE "El Abono para Mora-Dif.Cobro es Superior al SdoMora-DifCobro : $" Creditos.Int_MoraDifCob
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.
  
  IF W_Cpto EQ "SdoCta" AND W_Abono GT Creditos.Sdo_Capital AND creditos.sdo_capital >= 0 THEN DO:
     MESSAGE "El Abono para Sdo_Capital es Superior al Sdo_Capital : $" Creditos.Sdo_Capital
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.  

  IF W_Cpto EQ "IntAnticip" AND W_Cargo GT Creditos.Int_Anticipado  THEN DO:
     MESSAGE "El Cargo para Int_Anticipado es Superior al Sdo_Int_Anticipado : $" Creditos.Int_Anticipado
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END. 

 /* IF W_Cpto EQ "Int-Ctes" AND W_Cargo GT Creditos.Sdo_IntPag THEN DO:
     MESSAGE "El Cargo para Int.Corriente es Superior al Pagado por Int-Ctes : $" Creditos.Sdo_IntPag
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.*/ 

  IF W_Cpto EQ "IntDifCob" AND W_Cargo GT Creditos.Sdo_IntPag THEN DO:
     MESSAGE "El Cargo para Int-DifCob es Superior al Pagado por Intereses : $" Creditos.Sdo_IntPag
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END. 

  /*IF W_Cpto EQ "SdoCta" AND W_Cargo GT Creditos.Sdo_CapPag THEN DO:
     MESSAGE "El Cargo para Capital es Superior al Pagado por Sdo_Capital : $" Creditos.Sdo_CapPag
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.*/ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidaPdcto W-Tras_Cuentas 
PROCEDURE ValidaPdcto :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  IF W_Tipo EQ "C" OR W_Tipo EQ "2" THEN DO:   /*Créditos*/
     IF Rs_Op EQ 1 OR Rs_Op EQ 4 THEN DO:
        IF W_Cargo GT 0 THEN DO:
           MESSAGE "Acción Automática solo permite valor ABONO al Crédito..."
               VIEW-AS ALERT-BOX ERROR.
           RETURN ERROR.
        END.

        IF W_Abono GT (Creditos.Honorarios    + Creditos.Costas + Creditos.Polizas +
                       Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob +
                       Creditos.Int_DifCobro  + Creditos.Int_Corrientes +
                       Creditos.Sdo_Capital   - Creditos.Int_Anticipado) THEN DO:
           MESSAGE "El valor ABONO al Crédito es Superior al Saldo Total : " SKIP
                   "$ " Creditos.Honorarios    + Creditos.Costas + Creditos.Polizas +
                        Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob
                        Creditos.Int_DifCobro  + Creditos.Int_Corrientes +
                        Creditos.Sdo_Capital   - Creditos.Int_Anticipado
               VIEW-AS ALERT-BOX ERROR.
           RETURN ERROR.
        END.

        IF  Rs_Op EQ 4 
        AND W_Abono NE (Creditos.Honorarios    + Creditos.Costas + Creditos.Polizas +
                        Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob +
                        Creditos.Int_DifCobro  + Creditos.Int_Corrientes +
                        Creditos.Sdo_Capital   - Creditos.Int_Anticipado) THEN DO:
            MESSAGE "El valor ABONO al Crédito para Cancelación es Diferente : " SKIP
                     VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
     END.
     ELSE DO:
        RUN ValidaCredito NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
           RETURN ERROR.
     END.
  END.
  ELSE DO:
     RUN ValidaAhorro NO-ERROR.
     IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida_AgDestino W-Tras_Cuentas 
PROCEDURE Valida_AgDestino :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  IF W_AgDest EQ W_AgP THEN DO:
     MESSAGE "La Agencia Destino no puede ser igual a la Origen...Revise por favor."
              VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.
  ELSE DO:
     FIND FIRST Agencias WHERE Agencias.Agencia EQ W_AgDest
                           AND Agencias.Estado  EQ 1 NO-LOCK NO-ERROR.
     IF NOT AVAIL(Agencias) THEN DO:
        MESSAGE "La Agencia Destino no está disponible...No se permite la operación para está Agencia." 
                VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

