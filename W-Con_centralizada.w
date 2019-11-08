&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME W-Rec_XLibranza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Rec_XLibranza 

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

ON RETURN TAB.

{Incluido/Variable.I "SHARED"}
{Incluido/VARCON.I   "SHARED"}

DEFINE VAR P_Poliza AS DECIMAL INITIAL 0.
DEFINE VAR P_Honora AS DECIMAL INITIAL 0.
DEFINE VAR P_Costas AS DECIMAL INITIAL 0.
DEFINE VAR P_IMora AS DECIMAL INITIAL 0.
DEFINE VAR P_IMorDifC AS DECIMAL INITIAL 0.
DEFINE VAR P_IDifCob AS DECIMAL INITIAL 0.
DEFINE VAR P_ICte AS DECIMAL INITIAL 0.
DEFINE VAR P_IAntic AS DECIMAL INITIAL 0.
DEFINE VAR P_Capit AS DECIMAL INITIAL 0.
DEFINE VAR P_VlrNoDist AS DECIMAL INITIAL 0.
DEFINE VAR W_OpAboAho AS INTEGER INITIAL 010301001.
DEFINE VAR W_CodEmp AS INTEGER.
DEFINE VAR W_CodEmpI AS INTEGER.
DEFINE VAR W_NroPagI AS INTEGER.

   /* oakley */

   DEFI VAR W_Age     LIKE Empresas.Agencia.
   DEFI VAR W_VrADist LIKE Rec_Nomina.Val_Deduc INIT 0.
   DEFI VAR TDist     LIKE Rec_Nomina.Val_Deduc INIT 0.
   DEFI VAR TCuot     LIKE Rec_Nomina.Val_Deduc INIT 0.
   DEFI VAR TTDist    LIKE Rec_Nomina.Val_Deduc INIT 0.
   DEFI VAR TRec      LIKE Rec_Nomina.Val_Deduc INIT 0.
   DEFI VAR TDif      LIKE Rec_Nomina.Val_Deduc INIT 0.
   DEFI VAR w_arc     AS INTEGER INITIAL 0.
   DEFI VAR W_Cpte    LIKE Comprobantes.Comprobante.

   DEFI VAR W_ContabDistSi LIKE Rec_Nomina.Val_Deduc INIT 0.
   DEFI VAR W_ContabDistNo LIKE Rec_Nomina.Val_Deduc INIT 0.
   DEFI VAR W_Valor   LIKE Mov_Contable.Db INIT 0.
   DEFI VAR W_Cta     LIKE Mov_Contable.Cuenta.

   DEFI VAR TImp      LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR TNoImp    LIKE Ahorros.Sdo_Dispon INIT 0.
   DEFI VAR NNoImp    AS INTEG FORM "999999"  INIT 0.

   DEFI VAR W_RowId        AS ROWID.
   DEFI VAR W_Inf          AS LOG INIT FALSE.
   DEFI VAR W_SiContab     AS LOG INIT FALSE.
   DEFI VAR W_SiCpte       AS LOG INIT FALSE.
   DEFI VAR W_SiImp        AS LOG INIT FALSE.
   DEFI VAR W_SiImpF       AS LOG INIT FALSE.
    
   DEFI TEMP-TABLE TPdctos
        FIELD Agen    LIKE Ahorros.Agencia
        FIELD Prior   LIKE Pro_Ahorros.Prioridad
        FIELD FApert  LIKE Ahorros.Fec_Apert
        FIELD TP      AS   CHAR FORM "X(1)"
        FIELD CodP    LIKE Pro_Ahorros.Cod_Ahorro
        FIELD CtaP    LIKE Ahorros.Cue_Ahorro
        FIELD NomP    LIKE Pro_Ahorros.Nom_Produc
        FIELD Cuota   LIKE Ahorros.Cuota
        FIELD CuoOrig LIKE Ahorros.Cuota
        FIELD VrDist  LIKE Ahorros.Cuota
        FIELD VrSdo   LIKE Ahorros.Sdo_Dispon
        FIELD NC      LIKE Rec_Nomina.Num_Cuotas
        FIELD Abonos  LIKE Ahorros.Cuota EXTENT 9
              INDEX PF Prior FApert.

   DEFI TEMP-TABLE CopTPdctos LIKE TPdctos
        FIELD NitP  LIKE Ahorros.Nit
        FIELD NomC  LIKE Clientes.Nombre
        FIELD VRec  LIKE Ahorros.Sdo_Dispon
              INDEX NP NitP Prior
              INDEX Nit NitP.

   DEFI TEMP-TABLE TempCtas
        FIELD Agen   LIKE Ahorros.Agencia
        FIELD TipP   AS CHAR FORM "X(1)"
        FIELD Pto    LIKE Ahorros.Cod_Ahorro
        FIELD CtaPro LIKE Cuentas.Cuenta
        FIELD CtaIng LIKE Cuentas.Cuenta
        FIELD CtaLiq LIKE Cuentas.Cuenta
        FIELD IntAnt LIKE Cuentas.Cuenta
        FIELD IntMor LIKE Cuentas.Cuenta
        FIELD DifCoD LIKE Cuentas.Cuenta
        FIELD DifCoH LIKE Cuentas.Cuenta
        FIELD CtaPol LIKE Cuentas.Cuenta
        FIELD CtaHon LIKE Cuentas.Cuenta
        FIELD CtaCos LIKE Cuentas.Cuenta
        FIELD Oper   LIKE Liqui_Int.Cod_Operacion
        FIELD CtaSyA LIKE Cuentas.Cuenta.

   DEFI TEMP-TABLE CopMov_Contable LIKE Mov_Contable.

   DEFI TEMP-TABLE Tmp 
        FIELD Ced   AS CHAR FORM "X(12)"
        FIELD Nom   AS CHAR FORM "X(40)"
        FIELD Vr    LIKE ahorros.Sdo_Dispon INIT 0
        FIELD Err   AS CHAR FORM "X(25)".
   
   DEFINE TEMP-TABLE tmp-deducc
       FIELD cod_emp  AS CHARACTER FORMAT "x(80)" 
       FIELD cedula   AS CHARACTER FORMAT "x(12)"
       FIELD nombre   AS CHARACTER FORMAT "x(60)"
       FIELD nroobli  AS CHARACTER FORMAT "x(16)"
       FIELD nompro   AS CHARACTER FORMAT "X(70)"
       FIELD nrocuo   LIKE creditos.plazo
       FIELD fecdesc  LIKE creditos.fec_desembolso
       FIELD vdeduc   AS DECIMAL   FORMAT ">>>>>>>>>>>>"
       INDEX Idxded cedula nroobli.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Imp
&Scoped-define BROWSE-NAME Br_Pdctos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TPdctos Rec_Nomina Clientes

/* Definitions for BROWSE Br_Pdctos                                     */
&Scoped-define FIELDS-IN-QUERY-Br_Pdctos TPdctos.CodP "999" TPdctos.NomP TPdctos.CuoOrig TPdctos.VrDist TPdctos.NC TPdctos.VrSdo TPdctos.Abono[1] TPdctos.Abono[3] TPdctos.Abono[2] TPdctos.Abono[4] TPdctos.Abono[5] TPdctos.Abono[6] TPdctos.Abono[7] TPdctos.Abono[8] TPdctos.Prior "9999"   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Pdctos   
&Scoped-define SELF-NAME Br_Pdctos
&Scoped-define QUERY-STRING-Br_Pdctos FOR EACH TPdctos NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Pdctos OPEN QUERY Br_Pdctos FOR EACH TPdctos NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Pdctos TPdctos
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Pdctos TPdctos


/* Definitions for BROWSE Br_Rec                                        */
&Scoped-define FIELDS-IN-QUERY-Br_Rec Rec_Nomina.Nit Rec_Nomina.Val_Deducido Rec_Nomina.Num_Cuotas TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Rec   
&Scoped-define SELF-NAME Br_Rec
&Scoped-define QUERY-STRING-Br_Rec FOR EACH Rec_Nomina WHERE                              Rec_Nomina.Cod_Empresa EQ 0 NO-LOCK, ~
              EACH Clientes WHERE Clientes.Nit    EQ Rec_Nomina.Nit                        AND Clientes.Estado EQ 1 NO-LOCK
&Scoped-define OPEN-QUERY-Br_Rec OPEN QUERY Br_Rec FOR EACH Rec_Nomina WHERE                              Rec_Nomina.Cod_Empresa EQ 0 NO-LOCK, ~
              EACH Clientes WHERE Clientes.Nit    EQ Rec_Nomina.Nit                        AND Clientes.Estado EQ 1 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-Br_Rec Rec_Nomina Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Rec Rec_Nomina
&Scoped-define SECOND-TABLE-IN-QUERY-Br_Rec Clientes


/* Definitions for FRAME F_Proc                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Proc ~
    ~{&OPEN-QUERY-Br_Pdctos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-317 Rs_OpInf W_CmbEI Cmb_PagI W_FecIniI ~
W_FecFinI Btn_FinImp Btn_Imp-2 
&Scoped-Define DISPLAYED-OBJECTS Rs_OpInf W_CmbEI Cmb_PagI W_FecIniI ~
W_FecFinI 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Rec_XLibranza AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_SalirFec 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "&Regresar" 
     SIZE 9.57 BY 1.69 TOOLTIP "Regresar a la ventana principal".

DEFINE VARIABLE w-fecfin AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha de corte de la novedad" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE w-fecIni AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha Inicial  para el corte" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE BUTTON Btn_FinImp 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "&Regresar" 
     SIZE 9.57 BY 1.69 TOOLTIP "Regresar a la ventana principal".

DEFINE BUTTON Btn_Imp-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 9.57 BY 1.73 TOOLTIP "Informe por vencimientos y Extracto por Inversión.".

DEFINE VARIABLE Cmb_PagI AS CHARACTER FORMAT "X(36)":U 
     LABEL "Recaudos" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 33 BY 1 TOOLTIP "Pagos de esta Empresa"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CmbEI AS CHARACTER 
     LABEL "Empresa" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN MAX-CHARS 36
     SIZE 35.72 BY 1 TOOLTIP "Empresas Afiliadas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_FecFinI AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .85
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_FecIniI AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .85
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Rs_OpInf AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Detalle Grabación", 1,
"Disponible 1", 2,
"Disponible 2", 3
     SIZE 18.86 BY 2.42 TOOLTIP "Seleccione el informe"
     BGCOLOR 17 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-317
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.57 BY 2.92.

DEFINE BUTTON btn-Cuotas 
     LABEL "Novedad Pagaduría" 
     SIZE 20 BY 1.12.

DEFINE BUTTON Btn_Contabilizar 
     LABEL "&Contabilizar" 
     SIZE 11.72 BY 1.27 TOOLTIP "Abona la Distribución y Contabiliza".

DEFINE BUTTON Btn_CreCancelado 
     LABEL "Créditos Cancelados" 
     SIZE 20 BY 1.08.

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 11.72 BY 1.31
     BGCOLOR 8 .

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "&Imprimir" 
     SIZE 11.72 BY 1.58 TOOLTIP "Informe por vencimientos y Extracto por Inversión.".

DEFINE BUTTON Btn_Preliminar 
     LABEL "&Preliminar" 
     SIZE 11.72 BY 1.19 TOOLTIP "Informe Preliminar de la Distribución".

DEFINE BUTTON BUTTON-5 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 5" 
     SIZE 11.72 BY 1.42.

DEFINE VARIABLE Cmb_Cptes AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 24.86 BY 1 TOOLTIP "Seleccione el Comprobante para los Asientos Contables"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_pagos AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 25.14 BY 1 TOOLTIP "Pagos de esta Empresa pendientes por Contabilizar"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CmbEmp AS CHARACTER FORMAT "X(50)":U 
     LABEL "Empresa" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 36.14 BY 1 TOOLTIP "Empresas Afiliadas"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_CmbOfi AS CHARACTER FORMAT "X(50)":U 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 47.57 BY 1 TOOLTIP "Agencias Disponibles"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 50.72 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE txt_contrapartida AS CHARACTER FORMAT "9(8)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_apagar AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .77 TOOLTIP "Vr.Distribuido"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Cont AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Dist AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .77 TOOLTIP "Vr.Distribuido"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NCuotas AS INTEGER FORMAT ">9":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 3 BY .85 TOOLTIP "Nro de Cuotas"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NitCte AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 14.72 BY .85 TOOLTIP "Ced/Nit del Cliente"
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_NomCte AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 48.86 BY .85 TOOLTIP "Nombre del Cliente"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NroPago AS INTEGER FORMAT "999999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81 TOOLTIP "Nro.del Recaudo para esta Empresa"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_PorD AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY .73 TOOLTIP "Valor por Distribuir"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotNoD AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.29 BY .81 TOOLTIP "Total de este Recaudo no Distribuido"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotRec AS DECIMAL FORMAT "->>>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.29 BY .81 TOOLTIP "Total de este Recaudo"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_VrRec AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .85 TOOLTIP "Valor del Recaudo"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_Opcrea AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Manual", 8,
"Archivo", 9,
"Desde Pdtos.", 7
     SIZE 14.43 BY 1.88 TOOLTIP "Marque la opción para capturar los recaudos"
     BGCOLOR 17 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 8.23.

DEFINE RECTANGLE RECT-316
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.72 BY 2.92.

DEFINE VARIABLE Tgl-BorrarNomina AS LOGICAL INITIAL no 
     LABEL "Borrar Recaudo Nómina" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.29 BY .81
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE Tg_Nuevo AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 2.29 BY .54 TOOLTIP "Marque para generar un nuevo recaudo"
     BGCOLOR 17  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Pdctos FOR 
      TPdctos SCROLLING.

DEFINE QUERY Br_Rec FOR 
      Rec_Nomina, 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Pdctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Pdctos W-Rec_XLibranza _FREEFORM
  QUERY Br_Pdctos NO-LOCK DISPLAY
      TPdctos.CodP        COLUMN-LABEL "Pto"                         FORM   "999"
      TPdctos.NomP        COLUMN-LABEL "Descripc.del Producto"       FORMAT "X(20)"
      TPdctos.CuoOrig     COLUMN-LABEL "Valor Cuota"          FORMAT ">>>>>>,>>9.99"
      TPdctos.VrDist      COLUMN-LABEL "Vr.Distribuido"       FORMAT ">>>>>>>,>>9.99"
      TPdctos.NC          COLUMN-LABEL "NC"
      TPdctos.VrSdo       COLUMN-LABEL "Sdo.Ant.Pdcto"        FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[1]    COLUMN-LABEL "Abono Capital"        FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[3]    COLUMN-LABEL "Int.Corrientes"       FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[2]    COLUMN-LABEL "Int.Anticipado"       FORMAT "->>>>>>,>>9.99"
      TPdctos.Abono[4]    COLUMN-LABEL "Int.Dif.Cobro"        FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[5]    COLUMN-LABEL "Interés x Mora"       FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[6]    COLUMN-LABEL "Abono Costas"         FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[7]    COLUMN-LABEL "Honorarios"           FORMAT ">>>>>>>,>>9.99"
      TPdctos.Abono[8]    COLUMN-LABEL "Polizas"              FORMAT ">>>>>>>,>>9.99"
      TPdctos.Prior       COLUMN-LABEL "Prio"                 FORM   "9999"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 110 BY 5.65
         BGCOLOR 15 FGCOLOR 0 FONT 5
         TITLE BGCOLOR 15 FGCOLOR 0 "Distribución del Recaudo Seleccionado" ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN TOOLTIP "Distribución Preliminar".

DEFINE BROWSE Br_Rec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Rec W-Rec_XLibranza _FREEFORM
  QUERY Br_Rec NO-LOCK DISPLAY
      Rec_Nomina.Nit FORMAT "X(12)":U      COLUMN-LABEL "Cédula/Nit"
      Rec_Nomina.Val_Deducido              COLUMN-LABEL "Valor Recaudo" FORMAT ">,>>>,>>>,>>9.99":U
      Rec_Nomina.Num_Cuotas FORMAT "99":U  COLUMN-LABEL "N.C"
      TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)
                                           COLUMN-LABEL "Nombre del Cliente" FORMAT "X(35)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 70 BY 7.88
         BGCOLOR 15 FGCOLOR 0 FONT 5 ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN TOOLTIP "Seleccione el Recaudo del Cliente a modificar".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Imp
     Rs_OpInf AT ROW 1.42 COL 17.14 NO-LABEL
     W_CmbEI AT ROW 4.31 COL 11.43 COLON-ALIGNED
     Cmb_PagI AT ROW 6.08 COL 11.43 COLON-ALIGNED
     W_FecIniI AT ROW 7.12 COL 11.57 COLON-ALIGNED
     W_FecFinI AT ROW 7.12 COL 36 COLON-ALIGNED
     Btn_FinImp AT ROW 8.35 COL 39
     Btn_Imp-2 AT ROW 8.38 COL 3.86
     "No.Pago  Fec-Pago  Fec-Contabilizac." VIEW-AS TEXT
          SIZE 32.72 BY .62 AT ROW 5.54 COL 13.57
          BGCOLOR 18 FGCOLOR 15 
     RECT-317 AT ROW 1.23 COL 13.43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 17.57 ROW 8.46
         SIZE 51.57 BY 10.23
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Opciones de Informes".

DEFINE FRAME frm-FPagaduria
     w-fecIni AT ROW 1.35 COL 28 COLON-ALIGNED WIDGET-ID 2
     Btn_SalirFec AT ROW 1.54 COL 46 WIDGET-ID 10
     w-fecfin AT ROW 2.5 COL 28 COLON-ALIGNED WIDGET-ID 8
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 9.72 ROW 5.54
         SIZE 58 BY 3.65
         BGCOLOR 17 FGCOLOR 0 
         TITLE "Fechas  a validar" WIDGET-ID 100.

DEFINE FRAME F_Proc
     Tgl-BorrarNomina AT ROW 4.23 COL 87.72 WIDGET-ID 4
     Btn_CreCancelado AT ROW 13.81 COL 74 WIDGET-ID 2
     btn-Cuotas AT ROW 12.5 COL 74
     W_CmbEmp AT ROW 1.19 COL 74.29 COLON-ALIGNED
     W_CmbOfi AT ROW 1.27 COL 9.14 COLON-ALIGNED
     Cmb_pagos AT ROW 2.19 COL 85.43 COLON-ALIGNED NO-LABEL
     Tg_Nuevo AT ROW 2.62 COL 4
     Rs_Opcrea AT ROW 3.15 COL 5 NO-LABEL
     W_NitCte AT ROW 3.15 COL 22 COLON-ALIGNED NO-LABEL
     W_VrRec AT ROW 3.08 COL 41 COLON-ALIGNED NO-LABEL
     W_NroPago AT ROW 3.15 COL 73 COLON-ALIGNED NO-LABEL
     W_NCuotas AT ROW 3.08 COL 67.72 COLON-ALIGNED NO-LABEL
     Cmb_Cptes AT ROW 3.27 COL 85.43 COLON-ALIGNED NO-LABEL
     W_NomCte AT ROW 4 COL 22 COLON-ALIGNED NO-LABEL
     txt_contrapartida AT ROW 5.85 COL 73 COLON-ALIGNED
     BUTTON-5 AT ROW 6.35 COL 96.72
     Br_Rec AT ROW 7.23 COL 3.14
     W_TotRec AT ROW 7.65 COL 73 COLON-ALIGNED NO-LABEL
     Btn_Imp AT ROW 7.96 COL 96.72
     W_TotNoD AT ROW 9.35 COL 73 COLON-ALIGNED NO-LABEL
     Btn_Preliminar AT ROW 9.73 COL 96.72
     Btn_Contabilizar AT ROW 11.12 COL 96.72 HELP
          "Permite Realizar la contabilización de Depreciación"
     Btn_Done AT ROW 12.69 COL 96.72 HELP
          "Sale del proceso de Depreciación y Ajustes"
     Br_Pdctos AT ROW 15.04 COL 3
     Msaje AT ROW 21.12 COL 43.14 COLON-ALIGNED NO-LABEL
     W_Dist AT ROW 21.27 COL 21.57 COLON-ALIGNED NO-LABEL
     W_Cont AT ROW 21.27 COL 94.72 COLON-ALIGNED NO-LABEL
     W_PorD AT ROW 21.31 COL 1.43 COLON-ALIGNED NO-LABEL
     W_apagar AT ROW 11.23 COL 73 COLON-ALIGNED NO-LABEL
     "Ced./Nit Cliente                   Valor Recaudo             Nro.Cuotas" VIEW-AS TEXT
          SIZE 50 BY .62 AT ROW 2.35 COL 24
          BGCOLOR 18 FGCOLOR 15 
     "Valor a Pagar" VIEW-AS TEXT
          SIZE 16 BY .54 AT ROW 10.42 COL 75
          BGCOLOR 18 FGCOLOR 15 
     "Total del Recaudo" VIEW-AS TEXT
          SIZE 16.29 BY .54 AT ROW 6.96 COL 75
          BGCOLOR 17 FGCOLOR 7 
     "Contrapartida                            Comprobante" VIEW-AS TEXT
          SIZE 35.29 BY .65 AT ROW 5.12 COL 75
          BGCOLOR 17 FGCOLOR 7 
     "Vlr. este Recaudo" VIEW-AS TEXT
          SIZE 16 BY .58 AT ROW 20.73 COL 3.43
          BGCOLOR 18 FGCOLOR 15 
     "Valor Distribuido" VIEW-AS TEXT
          SIZE 16 BY .54 AT ROW 20.73 COL 23.57
          BGCOLOR 18 FGCOLOR 15 
     "Regist.Proceso" VIEW-AS TEXT
          SIZE 13.72 BY .5 AT ROW 20.73 COL 96.72
          BGCOLOR 1 
     "Tot. NO Distribuido" VIEW-AS TEXT
          SIZE 16.29 BY .54 AT ROW 8.73 COL 75
          BGCOLOR 17 FGCOLOR 7 
     "Nuevo Recaudo" VIEW-AS TEXT
          SIZE 14.72 BY .54 AT ROW 2.62 COL 6
          BGCOLOR 17 FGCOLOR 7 
     "Recaudo:" VIEW-AS TEXT
          SIZE 8.57 BY .81 AT ROW 2.23 COL 75
          BGCOLOR 17 FGCOLOR 7 
     RECT-314 AT ROW 6 COL 94.43
     RECT-316 AT ROW 2.38 COL 2.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.43 BY 21.31
         BGCOLOR 17 FGCOLOR 0 FONT 5.


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
  CREATE WINDOW W-Rec_XLibranza ASSIGN
         HIDDEN             = YES
         TITLE              = "Distribución Recaudos por Libranza, Prog.W-Rec_XLibranza.W"
         HEIGHT             = 21.12
         WIDTH              = 113
         MAX-HEIGHT         = 22.5
         MAX-WIDTH          = 114.72
         VIRTUAL-HEIGHT     = 22.5
         VIRTUAL-WIDTH      = 114.72
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
/* SETTINGS FOR WINDOW W-Rec_XLibranza
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frm-FPagaduria
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME frm-FPagaduria:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Imp
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_Imp:HIDDEN           = TRUE
       FRAME F_Imp:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Proc
   Custom                                                               */
/* BROWSE-TAB Br_Rec BUTTON-5 F_Proc */
/* BROWSE-TAB Br_Pdctos Btn_Done F_Proc */
/* SETTINGS FOR BUTTON Btn_Contabilizar IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Msaje IN FRAME F_Proc
   NO-ENABLE 2                                                          */
ASSIGN 
       Msaje:HIDDEN IN FRAME F_Proc           = TRUE.

/* SETTINGS FOR RADIO-SET Rs_Opcrea IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_apagar IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX W_CmbOfi IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Cont IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Dist IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomCte IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NroPago IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_PorD IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TotNoD IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TotRec IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Rec_XLibranza)
THEN W-Rec_XLibranza:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Pdctos
/* Query rebuild information for BROWSE Br_Pdctos
     _START_FREEFORM
OPEN QUERY Br_Pdctos FOR EACH TPdctos NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Pdctos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Rec
/* Query rebuild information for BROWSE Br_Rec
     _START_FREEFORM
  OPEN QUERY Br_Rec FOR EACH Rec_Nomina WHERE
                             Rec_Nomina.Cod_Empresa EQ 0 NO-LOCK,
       EACH Clientes WHERE Clientes.Nit    EQ Rec_Nomina.Nit
                       AND Clientes.Estado EQ 1 NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE Br_Rec */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Imp
/* Query rebuild information for FRAME F_Imp
     _Query            is NOT OPENED
*/  /* FRAME F_Imp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Rec_XLibranza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Rec_XLibranza W-Rec_XLibranza
ON END-ERROR OF W-Rec_XLibranza /* Distribución Recaudos por Libranza, Prog.W-Rec_XLibranza.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Rec_XLibranza W-Rec_XLibranza
ON WINDOW-CLOSE OF W-Rec_XLibranza /* Distribución Recaudos por Libranza, Prog.W-Rec_XLibranza.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Rec
&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Br_Rec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Rec W-Rec_XLibranza
ON MOUSE-SELECT-CLICK OF Br_Rec IN FRAME F_Proc
DO:
   IF AVAIL(Rec_Nomina) THEN DO:
      ASSIGN W_NitCte:SCREEN-VALUE  = Rec_Nomina.Nit
             W_NitCte
             W_VrRec:SCREEN-VALUE   = "0.00"
             W_VrRec                = 0
             W_NCuotas:SCREEN-VALUE = STRING(Rec_Nomina.Num_Cuotas)
             W_NCuotas.

      CLOSE QUERY Br_Pdctos.

      RUN DistRecaudo.    
            
      OPEN QUERY Br_Pdctos FOR EACH TPdctos NO-LOCK INDEXED-REPOSITION.
     
      APPLY "LEAVE" TO W_NitCte.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Cuotas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Cuotas W-Rec_XLibranza
ON CHOOSE OF btn-Cuotas IN FRAME F_Proc /* Novedad Pagaduría */
DO:
  ASSIGN w-fecini = w_fecha  w-fecfin = w_fecha   w_arc = 1.
  ASSIGN w-fecini:SCREEN-VALUE IN FRAME frm-fpagaduria = string(w_fecha)  w-fecfin:SCREEN-VALUE = string(w_fecha).
  ASSIGN FRAME F_Proc:SENSITIVE = FALSE
         FRAME Frm-Fpagaduria:VISIBLE    = TRUE.
  APPLY "Entry" TO w-fecini IN FRAME frm-fpagaduria.
 /* RUN cuotasnomina.r.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Contabilizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Contabilizar W-Rec_XLibranza
ON CHOOSE OF Btn_Contabilizar IN FRAME F_Proc /* Contabilizar */
DO:
    ASSIGN W_Inf                              = TRUE
           W_SiContab                         = TRUE
           Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Distrib.Contable desde Pdctos de C/Cliente..."
           Msaje:VISIBLE                      = TRUE
           W_TotNoD                           = 0
           W_TotNoD:SCREEN-VALUE              = STRING(W_TotNoD)
           W_ContabDistSi                     = 0
           W_ContabDistNo                     = 0.

    FOR EACH CopTPdctos:      DELETE CopTPdctos.      END.
    FOR EACH TPdctos:         DELETE TPdctos.         END.
    FOR EACH CopMov_Contable: DELETE CopMov_Contable. END.
    FOR EACH TempCtas:        DELETE TempCtas.        END.

    CLOSE QUERY Br_Rec.                                              
    CLOSE QUERY Br_Pdctos.  
       
    RUN Valida NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "La Configuración Contable Presento Errores...Proceso cancelado."
              VIEW-AS ALERT-BOX ERROR.

       ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor"
              W_Inf                              = FALSE
              W_SiContab                         = FALSE.

       RETURN.
    END.

    IF Txt_Contrapartida:SCREEN-VALUE EQ "" THEN DO:
        MESSAGE "Debe ingresar la Cuenta Contrapartida ..Proceso cancelado."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

       ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor"
       W_Inf                              = FALSE
       W_SiContab                         = FALSE.

        RETURN.
    END.

    FIND FIRST Cuentas WHERE Cuentas.Tipo      EQ 2
                         AND Cuentas.Estado    EQ 1
                         AND Cuentas.Cuenta    EQ Txt_Contrapartida:SCREEN-VALUE NO-LOCK NO-ERROR.

    FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ W_CodEmp NO-ERROR.

    IF NOT AVAIL(Cuentas) OR NOT AVAIL(Empresas) THEN DO:
       MESSAGE "La Cuenta de Caja y la Empresa deben existir Activas..." SKIP
             "               No se acepta la Operación." VIEW-AS ALERT-BOX ERROR.
       ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor"
              W_Inf                              = FALSE
              W_SiContab                         = FALSE.
       RETURN.
    END.

    FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
    
    FIND FIRST Comprobantes WHERE Comprobantes.Agencia    EQ W_Agencia
                                 AND Comprobantes.Comprob EQ W_Cpte
                                 AND Comprobantes.Estado  EQ 1 NO-ERROR.

    IF NOT AVAIL(Comprobantes) THEN DO:
       MESSAGE "El Comprobante-Fuente Contable para el proceso debe existir en Entidad y en Comprobantes" SKIP
             "                        No se acepta la Operación." VIEW-AS ALERT-BOX ERROR.
       ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor"
              W_Inf                              = FALSE
              W_SiContab                         = FALSE.

       RETURN.
    END.

    SESSION:SET-WAIT-STATE("GENERAL").

    RUN Contabilizar NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "La Contabilización Presento Errores...Proceso cancelado."
              VIEW-AS ALERT-BOX ERROR.

       ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Distrib.Contable Cancelada...Revise por favor"
              W_Inf                              = FALSE
              W_SiContab                         = FALSE.

       SESSION:SET-WAIT-STATE("").

       RETURN.
    END.
    ELSE DO:
       SESSION:SET-WAIT-STATE("").
       RETURN.

       DEFI VAR Listado AS CHAR FORM "X(40)".                                                                   
                                                                                                                
       Listado = W_PathSpl + "DRContab-" + STRING(W_CodEmp) + "-" + STRING(W_NroPago)  + ".Lst".                
                                                                                                                
       {Incluido\ImpArch.I "listado"}                                                                           
                                                                                                                
       ASSIGN W_Inf                              = FALSE                                                        
              W_SiContab                         = FALSE                                                        
              Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generada la Distrib.Contable desde Pdctos de C/Cliente..."  
              Msaje:VISIBLE                      = FALSE                                                        
              W_Cont                             = 0                                                            
              W_Cont:SCREEN-VALUE                = STRING(W_Cont).                                              
                                                                                                                
       FOR EACH CopTPdctos:      DELETE CopTPdctos.      END.
       FOR EACH TPdctos:         DELETE TPdctos.         END.
       FOR EACH CopMov_Contable: DELETE CopMov_Contable. END.
       FOR EACH TempCtas:        DELETE TempCtas.        END.  
        
       RUN ComboPagos.
                                                                                                                
       SESSION:SET-WAIT-STATE("").
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_CreCancelado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CreCancelado W-Rec_XLibranza
ON CHOOSE OF Btn_CreCancelado IN FRAME F_Proc /* Créditos Cancelados */
DO:
    ASSIGN w-fecini = w_fecha  w-fecfin = w_fecha w_arc = 2.
    ASSIGN w-fecini:SCREEN-VALUE IN FRAME frm-fpagaduria = string(w_fecha)  w-fecfin:SCREEN-VALUE = string(w_fecha).
    ASSIGN FRAME F_Proc:SENSITIVE = FALSE
           FRAME Frm-Fpagaduria:VISIBLE    = TRUE.
    APPLY "Entry" TO w-fecini IN FRAME frm-fpagaduria.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Rec_XLibranza
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME Btn_FinImp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FinImp W-Rec_XLibranza
ON CHOOSE OF Btn_FinImp IN FRAME F_Imp /* Regresar */
DO:
   ASSIGN FRAME F_Proc:SENSITIVE = TRUE
          FRAME F_Imp:VISIBLE    = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp W-Rec_XLibranza
ON CHOOSE OF Btn_Imp IN FRAME F_Proc /* Imprimir */
DO:
  ASSIGN FRAME F_Proc:SENSITIVE    = FALSE
         FRAME F_Imp:VISIBLE       = TRUE
         W_FecIniI:SCREEN-VALUE    = STRING(TODAY)
         W_FecIniI
         W_FecFinI:SCREEN-VALUE    = STRING(TODAY)
         W_FecFinI
         Rs_OpInf
         Cmb_PagI:LIST-ITEMS       = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME Btn_Imp-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp-2 W-Rec_XLibranza
ON CHOOSE OF Btn_Imp-2 IN FRAME F_Imp /* Imprimir */
DO:
  DEFI VAR Listado AS CHAR FORM "X(40)".
  
  ASSIGN Listado  = "C:\InfDRec.Lst"
         W_SiImpF = TRUE.
  
  {Incluido\Imprimir.I "listado"}

  W_SiImpF = FALSE.
END.

/*-----------------*/
PROCEDURE InfFrame:
  {Incluido\RepEncabezado.I}

  FIND FIRST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmpI
                          AND Rec_Nomina.Nro_Pago    EQ W_NroPagI
                          AND Rec_Nomina.Fec_Contab  EQ ? NO-LOCK NO-ERROR.

  ASSIGN W_Reporte    = "Reporte   : Detalle-Grabación Recaudo       Fecha del Informe: " +
                        STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
         W_EncColumna = "Empresa: " + STRING(W_CodEmpI) + " - Nro.Pago: " + STRING(W_NroPagI)
         TImp         = 0
         W_Cont       = 0. 

  IF NOT AVAIL(Rec_Nomina) THEN
     W_EncColumna = W_EncColumna + "     Recaudo Ya Contabilizado.".
  ELSE 
     W_EncColumna = W_EncColumna + "     Recaudo Pendiente X Contabilizar.".

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.

  FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmpI
                        AND Rec_Nomina.Nro_Pago    EQ W_NroPagI NO-LOCK BY Rec_Nomina.Nit:
      ASSIGN TImp   = TImp + Rec_Nomina.Val_Deduc
             W_Cont = W_Cont + 1.

      FIND FIRST Clientes WHERE Clientes.Nit EQ Rec_Nomina.Nit
                            AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.    

      DISPLAY Rec_Nomina.Nit             LABEL "Cédula/Nit"
              TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)   
                                         LABEL "Nombre del Cliente" FORM "X(40)" WHEN AVAIL(Clientes)
              Rec_Nomina.Val_Deduc       LABEL "Valor Recaudo"              
         WITH DOWN WIDTH 140 FRAME FImp USE-TEXT NO-LABELS STREAM-IO NO-BOX.
  END.
    
  DISPLAY SKIP(1)
          "TOTALES : Registros : " + STRING(W_Cont,"999999") + " Por valor de $ " +
           STRING(Timp,"->>>>,>>>,>>9.99")                 FORMAT "X(120)"          
      WITH DOWN WIDTH 140 FRAME FTtImp USE-TEXT NO-LABELS STREAM-IO NO-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Btn_Preliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Preliminar W-Rec_XLibranza
ON CHOOSE OF Btn_Preliminar IN FRAME F_Proc /* Preliminar */
DO:
   ASSIGN W_Inf                              = TRUE
          Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Distrib.Preliminar desde Pdctos de C/Cliente..."
          Msaje:VISIBLE                      = TRUE
          W_TotNoD                           = 0
          W_TotNoD:SCREEN-VALUE              = STRING(W_TotNoD).

   FOR EACH CopTPdctos: DELETE CopTPdctos. END.
    
   SESSION:SET-WAIT-STATE("GENERAL").

   FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp
                         AND Rec_Nomina.Nro_Pago    EQ W_NroPago
                         AND Rec_Nomina.Fec_Contab  EQ ? NO-LOCK:
       IF Rec_Nomina.Val_Deduc GT 0 THEN DO:
          ASSIGN W_NitCte            = Rec_Nomina.Nit
                 W_Cont              = W_Cont + 1
                 W_Cont:SCREEN-VALUE = STRING(W_Cont).

          RUN DistRecaudo.

          ASSIGN W_TotNoD              = W_TotNoD + W_VrADist
                 W_TotNoD:SCREEN-VALUE = STRING(W_TotNoD).
       END.
   END.

   DEFI VAR Listado AS CHAR FORM "X(40)".
  
   listado = W_PathSpl + "DistRPrel.Lst".
  
   {Incluido\Imprimir.I "listado"}

   ASSIGN W_Inf                              = FALSE
          Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generada la Distrib.Preliminar desde Pdctos de C/Cliente..."
          Msaje:VISIBLE                      = FALSE
          W_Cont                             = 0
          W_Cont:SCREEN-VALUE                = STRING(W_Cont).

   FOR EACH CopTPdctos: DELETE CopTPdctos. END.       
    
   APPLY "MOUSE-SELECT-CLICK" TO Br_Rec.

   SESSION:SET-WAIT-STATE("").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frm-FPagaduria
&Scoped-define SELF-NAME Btn_SalirFec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalirFec W-Rec_XLibranza
ON CHOOSE OF Btn_SalirFec IN FRAME frm-FPagaduria /* Regresar */
DO:
   ASSIGN FRAME F_Proc:SENSITIVE = TRUE
          FRAME Frm-Fpagaduria:VISIBLE    = FALSE.
 DEFINE VAR zpath   AS CHARACTER FORMAT "X(70)".
 DEFINE VARIABLE REGISTRO AS CHARACTER FORMAT "X(180)".
 IF w_arc = 1 THEN 
 DO:
   zpath = W_PathSpl + "nomina_CodEmp" + STRING(w_codEmp) + "_" + STRING(day(w_fecha),"99") + STRING(MONTH(w_fecha),'99') + STRING(YEAR(w_fecha),'9999').
   ASSIGN w-fecini w-fecfin.
    FOR EACH Tmp-deducc: DELETE Tmp-deducc.    END.
         FOR EACH clientes WHERE clientes.cod_empresa EQ W_CodEmp:
             FOR EACH creditos WHERE creditos.nit EQ clientes.nit 
                                 AND creditos.sdo_capital GT 0
                                 AND creditos.FOR_pago    EQ 2 
                                 AND ( creditos.fec_desembolso GE w-fecini AND
                                       creditos.Fec_desembolso LE w-fecfin)
                 NO-LOCK:
                 FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
                 FIND FIRST tmp-deducc   WHERE tmp-deducc.cedula  EQ clientes.nit AND
                                               tmp-deducc.nroobli EQ TRIM(STRING(Creditos.num_credito)) NO-ERROR.
                 IF NOT AVAILABLE (tmp-deducc) THEN CREATE tmp-deducc.
                 ASSIGN tmp-deducc.cod_emp = W_CmbEmp:SCREEN-VALUE  IN FRAME F_Proc
                        tmp-deducc.cedula  = clientes.nit
                        tmp-deducc.nombre  = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)  
                        tmp-deducc.nroobli = TRIM(STRING(Creditos.num_credito))
                        tmp-deducc.nompro  = TRIM(Pro_Creditos.Nom_Producto) 
                        tmp-deducc.nrocuo  = creditos.plazo
                        tmp-deducc.fecdesc = Creditos.Fec_Pago
                        tmp-deducc.vdeduc  = creditos.cuota.
             END.

             FOR EACH ahorros WHERE ahorros.nit      EQ clientes.nit
                                AND AHORROS.FOR_PAGO EQ 2 
                                AND ahorros.estado = 1  
                 AND ( ahorros.fec_apertura GE w-fecini AND
                       ahorros.Fec_apertura LE w-fecfin)
                 NO-LOCK:
                 FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
                 FIND FIRST tmp-deducc   WHERE tmp-deducc.cedula  EQ clientes.nit AND
                                               tmp-deducc.nroobli EQ Ahorros.cue_ahorros NO-ERROR.
                 IF NOT AVAILABLE (tmp-deducc) THEN CREATE tmp-deducc.
                 ASSIGN tmp-deducc.cod_emp = W_CmbEmp:SCREEN-VALUE  IN FRAME F_Proc
                        tmp-deducc.cedula  = clientes.nit
                        tmp-deducc.nombre  = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)  
                        tmp-deducc.nroobli = Ahorros.cue_ahorros
                        tmp-deducc.nompro  = Pro_Ahorros.Nom_Producto
                        tmp-deducc.nrocuo  = 0
                        tmp-deducc.fecdesc = w_fecha
                        tmp-deducc.vdeduc  = ahorros.cuota.
             END.
         END.

     OUTPUT TO VALUE(zpath).
     FOR EACH tmp-deducc:
         REGISTRO =  TRIM(STRING(tmp-deducc.cod_emp)) + ";" + TRIM(tmp-deducc.cedula)          + ";" +
                     TRIM(tmp-deducc.nombre)          + ";" + TRIM(STRING(tmp-deducc.nroobli)) + ";" +
                     TRIM(tmp-deducc.nompro)          + ";" +
                     TRIM(STRING(tmp-deducc.nrocuo))  + ";" + TRIM(STRING(tmp-deducc.vdeduc))  + ";" + 
                     TRIM(STRING(tmp-deducc.fecdesc)).
         PUT REGISTRO SKIP.
     END.
     FIND FIRST Empresas WHERE Empresas.Estado    EQ 1
                         AND Empresas.Cod_empresa EQ W_CodEmp
                         AND Empresas.Agencia     EQ w_agencia NO-ERROR.
     IF AVAILABLE(Empresas) THEN
        ASSIGN empresas.fec_constitucion = w-fecfin.
     FIND CURRENT empresas NO-LOCK NO-ERROR.
     MESSAGE "GENERACION DE CUOTAS TERMINO EXITOSAMENTE" SKIP
             "UBICACION DEL ARCHIVO: " zpath  VIEW-AS ALERT-BOX.
  END.
 ELSE
 DO:
   zpath = W_PathSpl + "CreCancelado_CodEmp" + STRING(w_codEmp) + "_" + STRING(day(w_fecha),"99") + STRING(MONTH(w_fecha),'99') + STRING(YEAR(w_fecha),'9999').
   ASSIGN w-fecini w-fecfin.
    FOR EACH Tmp-deducc: DELETE Tmp-deducc.    END.
         FOR EACH clientes WHERE clientes.cod_empresa EQ W_CodEmp:
             FOR EACH creditos WHERE creditos.nit EQ clientes.nit 
                                 AND creditos.sdo_capital EQ 0
                                 AND creditos.FOR_pago    EQ 2 
                                 AND ( creditos.fec_cancetotal GE w-fecini AND
                                       creditos.Fec_cancetotal LE w-fecfin)
                 NO-LOCK:
                 FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
                 FIND FIRST tmp-deducc   WHERE tmp-deducc.cedula  EQ clientes.nit AND
                                               tmp-deducc.nroobli EQ TRIM(STRING(Creditos.num_credito)) NO-ERROR.
                 IF NOT AVAILABLE (tmp-deducc) THEN CREATE tmp-deducc.
                 ASSIGN tmp-deducc.cod_emp = W_CmbEmp:SCREEN-VALUE  IN FRAME F_Proc
                        tmp-deducc.cedula  = clientes.nit
                        tmp-deducc.nombre  = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)  
                        tmp-deducc.nroobli = TRIM(STRING(Creditos.num_credito))
                        tmp-deducc.nompro  = TRIM(Pro_Creditos.Nom_Producto) 
                        tmp-deducc.nrocuo  = creditos.plazo
                        tmp-deducc.fecdesc = Creditos.Fec_Pago
                        tmp-deducc.vdeduc  = creditos.cuota.
             END.

             FOR EACH ahorros WHERE ahorros.nit      EQ clientes.nit
                                AND AHORROS.FOR_PAGO EQ 2 
                 AND ( ahorros.fec_cancela GE w-fecini  AND
                       ahorros.Fec_cancela LE w-fecfin) AND
                 ahorros.estado = 2 NO-LOCK:
                 FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
                 FIND FIRST tmp-deducc   WHERE tmp-deducc.cedula  EQ clientes.nit AND
                                               tmp-deducc.nroobli EQ Ahorros.cue_ahorros NO-ERROR.
                 IF NOT AVAILABLE (tmp-deducc) THEN CREATE tmp-deducc.
                 ASSIGN tmp-deducc.cod_emp = W_CmbEmp:SCREEN-VALUE  IN FRAME F_Proc
                        tmp-deducc.cedula  = clientes.nit
                        tmp-deducc.nombre  = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) + " " + TRIM(Clientes.Nombre)  
                        tmp-deducc.nroobli = Ahorros.cue_ahorros
                        tmp-deducc.nompro  = Pro_Ahorros.Nom_Producto
                        tmp-deducc.nrocuo  = 0
                        tmp-deducc.fecdesc = w_fecha
                        tmp-deducc.vdeduc  = ahorros.cuota.
             END.
         END.

     OUTPUT TO VALUE(zpath).
     FOR EACH tmp-deducc:
         REGISTRO =  TRIM(STRING(tmp-deducc.cod_emp)) + ";" + TRIM(tmp-deducc.cedula)          + ";" +
                     TRIM(tmp-deducc.nombre)          + ";" + TRIM(STRING(tmp-deducc.nroobli)) + ";" +
                     TRIM(tmp-deducc.nompro)          + ";" +
                     TRIM(STRING(tmp-deducc.nrocuo))  + ";" + TRIM(STRING(tmp-deducc.vdeduc))  + ";" + 
                     TRIM(STRING(tmp-deducc.fecdesc)).
         PUT REGISTRO SKIP.
     END.
     MESSAGE "GENERACION DE CANCELACION DE CREDITOS/CUOTAS AHORRO TERMINO EXITOSAMENTE" SKIP
             "UBICACION DEL ARCHIVO: " zpath  VIEW-AS ALERT-BOX.
  END.
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 W-Rec_XLibranza
ON CHOOSE OF BUTTON-5 IN FRAME F_Proc /* Button 5 */
DO:
  RUN W-InfDia.R.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Cptes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Cptes W-Rec_XLibranza
ON VALUE-CHANGED OF Cmb_Cptes IN FRAME F_Proc
DO:
  ASSIGN W_Cpte = INTEG(SUBSTRING(Cmb_Cptes:SCREEN-VALUE,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME Cmb_PagI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_PagI W-Rec_XLibranza
ON VALUE-CHANGED OF Cmb_PagI IN FRAME F_Imp /* Recaudos */
DO:
   ASSIGN W_NroPagI = INTEG(SUBSTRING(Cmb_PagI:SCREEN-VALUE,1,6)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME Cmb_pagos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_pagos W-Rec_XLibranza
ON VALUE-CHANGED OF Cmb_pagos IN FRAME F_Proc
DO:
   ASSIGN W_NroPago              = INTEG(SUBSTRING(Cmb_Pagos:SCREEN-VALUE,1,6))
          W_NroPago:SCREEN-VALUE = STRING(W_NroPago)
          W_TotNoD               = 0
          W_TotNoD:SCREEN-VALUE  = STRING(W_TotNoD).
   IF W_NroPago = 999999 THEN DO:
     FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp
                           AND Rec_Nomina.Nro_Pago    EQ W_NroPago
                           AND Rec_Nomina.Fec_Contab  EQ ? :
       IF Rec_Nomina.Val_Deduc GT 0 THEN DO:
          DELETE rec_nomina.
       END.
     END.
     FIND FIRST rec_nomina NO-LOCK NO-ERROR.
   END.
   ELSE
      RUN QueryRec.                                        
END                                                               .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_Opcrea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Opcrea W-Rec_XLibranza
ON MOUSE-SELECT-CLICK OF Rs_Opcrea IN FRAME F_Proc
DO:
  ASSIGN Rs_OpCrea
         W_NroPago              = 1
         W_NroPago:SCREEN-VALUE = "000001"
         Rs_OpCrea:SENSITIVE    = FALSE.
    
  FIND LAST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp
                         AND Rec_Nomina.Nro_Pago    GT 0 NO-LOCK NO-ERROR.
  IF AVAIL(Rec_Nomina) THEN
     ASSIGN W_NroPago              = Rec_Nomina.Nro_Pago + 1
            W_NroPago:SCREEN-VALUE = STRING(Rec_Nomina.Nro_Pago + 1).

  IF Rs_OpCrea EQ 1 THEN 
     APPLY "ENTRY" TO W_NitCte.
  ELSE DO:
     IF Rs_OpCrea EQ 2 THEN
        RUN HallaArchivo.
     ELSE 
        RUN HallaPdctos.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tgl-BorrarNomina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tgl-BorrarNomina W-Rec_XLibranza
ON VALUE-CHANGED OF Tgl-BorrarNomina IN FRAME F_Proc /* Borrar Recaudo Nómina */
DO:
    ASSIGN Tgl-BorrarNomina.
    ASSIGN W_NroPago              = INTEG(SUBSTRING(Cmb_Pagos:SCREEN-VALUE,1,6))
           W_NroPago:SCREEN-VALUE = STRING(W_NroPago)
           W_TotNoD               = 0
           W_TotNoD:SCREEN-VALUE  = STRING(W_TotNoD).
      FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp
                            AND Rec_Nomina.Nro_Pago    EQ W_NroPago
                            AND Rec_Nomina.Fec_Contab  EQ ? :
        IF Rec_Nomina.Val_Deduc GT 0 THEN DO:
           DELETE rec_nomina.
        END.
      END.
      FIND FIRST rec_nomina NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tg_Nuevo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_Nuevo W-Rec_XLibranza
ON MOUSE-SELECT-CLICK OF Tg_Nuevo IN FRAME F_Proc
DO:
  ASSIGN Tg_Nuevo
         Rs_OpCrea:SENSITIVE = FALSE.

  IF Tg_Nuevo /*AND W_CodEmp GT 0*/ THEN DO:
     ASSIGN Rs_OpCrea:SENSITIVE    = TRUE
            W_NitCte               = " "
            W_NitCte:SCREEN-VALUE  = " "
            W_VrRec                = 0
            W_VrRec:SCREEN-VALUE   = "0.00"
            W_NCuotas              = 1
            W_NCuotas:SCREEN-VALUE = "1"
            W_TotNoD               = 0
            W_TotNoD:SCREEN-VALUE  = STRING(W_TotNoD).
    
     FOR EACH TPdctos. DELETE TPdctos. END.

     CLOSE QUERY Br_Rec.
     CLOSE QUERY Br_Pdctos.
  END.
  ELSE IF Tg_Nuevo THEN 
    MESSAGE "Seleccione una Empresa para el Nuevo Recaudo..." VIEW-AS ALERT-BOX.

  ASSIGN Tg_Nuevo              = FALSE
         Tg_Nuevo:SCREEN-VALUE = "No".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txt_contrapartida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txt_contrapartida W-Rec_XLibranza
ON LEAVE OF txt_contrapartida IN FRAME F_Proc
DO:
  FIND FIRST Cuentas    WHERE Cuentas.Tipo      EQ 2
                          AND Cuentas.Estado    EQ 1
                          AND Cuentas.Cuenta    EQ txt_contrapartida:SCREEN-VALUE NO-LOCK NO-ERROR.

  IF NOT AVAIL(Cuentas)  THEN DO:
       MESSAGE "La Cuenta debe existir y estar activa..." SKIP
             "               No se acepta la Operación." VIEW-AS ALERT-BOX ERROR.
       
       RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frm-FPagaduria
&Scoped-define SELF-NAME w-fecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-fecIni W-Rec_XLibranza
ON LEAVE OF w-fecIni IN FRAME frm-FPagaduria /* Fecha Inicial  para el corte */
DO:
  ASSIGN w-fecini.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define SELF-NAME W_CmbEI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEI W-Rec_XLibranza
ON VALUE-CHANGED OF W_CmbEI IN FRAME F_Imp /* Empresa */
DO:
  ASSIGN W_CodEmpI           = INTEGER(SUBSTRING(W_CmbEI:SCREEN-VALUE,1,4)) 
         Cmb_PagI:LIST-ITEMS = ""
         W_NroPagI           = 0.
 
  FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empr     EQ W_CodEmpI
                            BREAK BY Rec_Nomina.Nro_pago:
      IF FIRST-OF(Rec_Nomina.Nro_pago) THEN 
         Cmb_PagI:ADD-LAST(STRING(Rec_Nomina.Nro_pago,"999999")     + " - " +
                           STRING(Rec_Nomina.Fec_Pago,"99/99/9999") + " - " +
                           STRING(Rec_Nomina.Fec_Contab,"99/99/9999")).
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Proc
&Scoped-define SELF-NAME W_CmbEmp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEmp W-Rec_XLibranza
ON VALUE-CHANGED OF W_CmbEmp IN FRAME F_Proc /* Empresa */
DO:
  ASSIGN W_CodEmp               = INTEGER(SUBSTRING(W_CmbEmp:SCREEN-VALUE,1,4)) 
         Cmb_Pagos:LIST-ITEMS   = ""
         W_NroPago              = 0
         W_NroPago:SCREEN-VALUE = "0".
    
  RUN ComboPagos.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NCuotas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NCuotas W-Rec_XLibranza
ON LEAVE OF W_NCuotas IN FRAME F_Proc
DO:
  ASSIGN W_NCuotas.
    
  IF W_NCuotas LE 0 THEN
     W_NCuotas = 1. 
        
  IF W_CodEmp GT 0 AND W_NroPago GT 0 THEN DO:
     FIND FIRST Clientes WHERE Clientes.Nit         EQ W_NitCte
                           /*AND Clientes.Cod_Empresa EQ W_CodEmp*/
                           AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.                            
     IF AVAIL(Clientes) THEN DO:                    
        FIND LAST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp
                               AND Rec_Nomina.Nro_Pago    EQ W_NroPago
                               AND Rec_Nomina.Nit         EQ W_NitCte
                               AND Rec_Nomina.Fec_Contab  EQ ? NO-ERROR.
        IF NOT AVAIL(Rec_Nomina) AND W_VrRec GT 0 THEN DO:
           CREATE Rec_Nomina.
           ASSIGN Rec_Nomina.Agencia      = W_Agencia
                  Rec_Nomina.Cod_Empresa  = W_CodEmp       
                  Rec_Nomina.Nro_Pago     = W_NroPago
                  Rec_Nomina.Nit          = W_NitCte 
                  Rec_Nomina.Num_Cuotas   = W_NCuotas
                  Rec_Nomina.Fec_Contab   = ? 
                  Rec_Nomina.Fec_Grabac   = W_Fecha
                  Rec_Nomina.Fec_Pago     = W_Fecha
                  Rec_Nomina.Val_Deducido = W_VrRec
                  W_VrRec                 = 0
                  W_VrRec:SCREEN-VALUE    = "0.00"
                  W_RowId                 = ROWID(Rec_Nomina).                
        END.
        ELSE IF AVAIL(Rec_Nomina) AND (W_VrRec + Rec_Nomina.Val_Deducido) GE 0 THEN
           ASSIGN Rec_Nomina.Num_Cuotas   = W_NCuotas
                  Rec_Nomina.Val_Deducido = Rec_Nomina.Val_Deducido + W_VrRec
                  W_VrRec                 = 0
                  W_VrRec:SCREEN-VALUE    = "0.00"
                  W_RowId                 = ROWID(Rec_Nomina).
        ELSE MESSAGE "El valor para Distribuir debe ser Mayor que 0(Cero)..." SKIP
                     "                         Revise por favor."  VIEW-AS ALERT-BOX.

        IF AVAIL(Rec_Nomina) THEN
           W_RowId = ROWID(Rec_Nomina).

        APPLY "ENTRY" TO W_NitCte.
            
        RUN QueryRec.
            
        REPOSITION Br_Rec TO ROWID W_RowId.
            
        RUN DistRecaudo. 
     END.
     ELSE MESSAGE "El Nit debe existir Activo en Clientes para esta Empresa" VIEW-AS ALERT-BOX.      
  END.
  ELSE DO:
      IF Rs_opcrea NE 1 THEN 
         MESSAGE rs_opcrea "Debe existir la Empresa y Nro.de Recaudo...Revise por favor." VIEW-AS ALERT-BOX.
  END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NitCte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCte W-Rec_XLibranza
ON LEAVE OF W_NitCte IN FRAME F_Proc
DO:
   ASSIGN W_NitCte
          W_NomCte:SCREEN-VALUE = "".

   FIND FIRST Clientes WHERE Clientes.Nit         EQ W_NitCte 
                         /*AND Clientes.Cod_Empresa EQ W_CodEmp*/ NO-LOCK NO-ERROR.
   IF NOT AVAIL(Clientes) THEN                                                                                                                                                                      
      MESSAGE "El Nit debe existir Activo en Clientes para esta Empresa" VIEW-AS ALERT-BOX.                                 
   ELSE ASSIGN W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +    
                                       " " + TRIM(Clientes.Nombre).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCte W-Rec_XLibranza
ON MOUSE-SELECT-DBLCLICK OF W_NitCte IN FRAME F_Proc
DO:
  ASSIGN FRAME F_Proc:SENSITIVE = FALSE.                                                           
                                                                                                    
  RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                    OUTPUT W_NitCte, OUTPUT W_NomCte, OUTPUT W_NomCte, OUTPUT W_Age).       
                                                                                                    
  ASSIGN FRAME F_Proc:SENSITIVE = TRUE.

  FIND FIRST Clientes WHERE Clientes.Nit         EQ W_NitCte
                        AND Clientes.Cod_Empresa EQ W_CodEmp NO-LOCK NO-ERROR.                            
  IF AVAIL(Clientes) THEN                                                                         
     ASSIGN W_NitCte:SCREEN-VALUE = W_NitCte                                                      
            W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +   
                                        " " + TRIM(Clientes.Nombre).
  APPLY "Leave" TO SELF.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_VrRec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_VrRec W-Rec_XLibranza
ON LEAVE OF W_VrRec IN FRAME F_Proc
DO:
   ASSIGN W_VrRec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imp
&Scoped-define BROWSE-NAME Br_Pdctos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Rec_XLibranza 


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

  FIND FIRST Agencias WHERE Agencias.Agencia EQ W_Agencia
                        AND Agencias.Estado  EQ 1  NO-LOCK NO-ERROR.
  IF NOT AVAIL(Agencias) THEN DO:
     MESSAGE "La Agencia no está disponible para realizar Transacciones..." VIEW-AS ALERT-BOX ERROR.

     APPLY "CLOSE":U TO THIS-PROCEDURE.
     RETURN NO-APPLY.
  END.

  W_CmbOfi:ADD-LAST("000 CONSOLIDADO").
  
  ASSIGN W_OfiIni       = W_Agencia
         W_OfiFin       = W_Agencia
         Msaje:VISIBLE  = FALSE.
                  
  FOR EACH Agencias WHERE Agencias.Estado  EQ 1
                      AND Agencias.Agencia GT 0 NO-LOCK:
      W_CmbOfi:ADD-LAST(STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(25)")).

      IF Agencias.Agencia EQ W_Agencia THEN
         W_CmbOfi:SCREEN-VALUE = (STRING(Agencias.Agencia,"999") + "-" + STRING(Agencias.Nombre,"X(40)")).
  END.              
    
  FOR EACH Empresas WHERE Empresas.Estado      EQ 1
                      AND Empresas.Cod_empresa GT 0 
                      AND Empresas.Agencia     EQ w_agencia NO-LOCK:
     FIND FIRST Clientes WHERE Clientes.Nit    EQ Empresas.Nit
                            AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
        
      IF AVAIL(Clientes) THEN DO: 
         W_CmbEmp:ADD-LAST(STRING(Empresas.Cod_empresa,"9999") + "-" +
                           STRING(Empresas.ALIAS_Empresa,"X(45)")).                   
         W_CmbEI:ADD-LAST(STRING(Empresas.Cod_empresa,"9999") + "-" +
                           STRING(Empresas.ALIAS_Empresa,"X(45)")).
     END.
  END. 
  W_CmbEmp:SCREEN-VALUE = (STRING(0005,"9999") + "-" + "EMPRESA PAGO PERSONAL").
  
  FOR EACH Comprobantes WHERE Comprobantes.Agencia         EQ W_Agencia
                          AND Comprobantes.Comprobante     EQ 8
                          AND Comprobantes.Estado          EQ 1 NO-LOCK:
      Cmb_Cptes:ADD-LAST(STRING(Comprobantes.Comprobante,"999") + "-" + 
                         STRING(Comprobantes.Nombre,"X(25)")).
  END.
     
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
     WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Abonar W-Rec_XLibranza 
PROCEDURE Abonar :
/*------------------------------------------------------------------------------
  Purpose:     
 ------------------------------------------------------------------------------*/
  IF TPdctos.TP EQ "A" THEN DO:   /*Abona Ahorros*/
     FIND FIRST Ahorros WHERE Ahorros.Nit        EQ W_NitCte
                          AND Ahorros.Cod_Ahorro EQ TPdctos.CodP
                          AND Ahorros.Cue_Ahorro EQ TPdctos.CtaP
                          AND Ahorros.Estado     EQ 1 NO-ERROR.
     IF AVAIL(Ahorros) THEN 
        FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                                 AND Pro_Ahorros.Estado     EQ 1 NO-LOCK NO-ERROR.
     IF NOT AVAIL(Ahorros) OR NOT AVAIL(Pro_Ahorros) THEN DO:
        MESSAGE "Falta Pro_Ahorros : " TPdctos.CodP SKIP
                "O Falta la Cuenta : " TPdctos.CtaP ",del Cliente con Cédula.: " W_NitCte SKIP
                "Para realizar el abono del Recaudo...?"
                VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
     END.

     FIND FIRST TempCtas WHERE TempCtas.Agen EQ Ahorros.Agencia
                           AND TempCtas.TipP EQ "A"
                           AND TempCtas.Pto  EQ Ahorros.Cod_Ahorro NO-LOCK NO-ERROR.
     IF NOT AVAIL(TempCtas) THEN DO:
        MESSAGE "Falta Configuración con Producto_Ahorro: " Ahorros.Cod_Ahorro SKIP
                "Para la Agencia : " Ahorros.Agencia
                VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
     END.

     /* Oct.24/05 GAER*/
     ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + TPdctos.VrDist
            Ahorros.Fec_UltTrans   = W_Fecha
            Ahorros.Num_DepMes     = Ahorros.Num_DepMes + 1                 
            Ahorros.Val_DepMes     = Ahorros.Val_DepMes + TPdctos.VrDist
            Ahorros.Val_DepDia     = Ahorros.Val_DepDia + TPdctos.VrDist
            Ahorros.Num_DepDia     = Ahorros.Num_DepDia + 1.

     RUN MovAhorro.

     RUN ContablesAhorro NO-ERROR.
     IF ERROR-STATUS:ERROR THEN
        RETURN ERROR.
  END.
  ELSE DO:   
     FIND Creditos WHERE Creditos.Nit         EQ W_NitCte
                     AND Creditos.Cod_Credito EQ TPdctos.CodP
                     AND Creditos.Num_Credito EQ INTEG(TPdctos.CtaP)
                     AND Creditos.Sdo_Capital GT 0 NO-LOCK NO-ERROR.
     IF AVAIL(Creditos) THEN DO:
        RUN AboCredito.R         /*Distribuye abonos en Créditos,graba Mov_creditos,Mov_Contab y PlanPagos*/
            (INPUT TRUE,
             INPUT Creditos.Agencia,Creditos.Cod_Credito,Creditos.Nit,
             INPUT Creditos.Num_Credito,TPdctos.VrDist,
             INPUT Comprobantes.Comprobante,Comprobantes.Secuencia,0,
             OUTPUT P_Poliza,  OUTPUT P_Honora, OUTPUT P_Costas, OUTPUT P_IMorDifC, OUTPUT P_IMora,
             OUTPUT P_IDifCob, OUTPUT P_ICte,   OUTPUT P_IAntic, OUTPUT P_Capit,
             OUTPUT P_VlrNoDist) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN
           RETURN ERROR.
     END.
     ELSE DO:
         MESSAGE "No se halló el Crédito vigente para Distribuir el Pago..." SKIP
                 "Del Nit. : " W_NitCte ", Cod_producto : " TPdctos.CodP ", Nro-Crédito : " TPdctos.CtaP SKIP
                 "                        Revise por favor...Distribución cancelada."
                 VIEW-AS ALERT-BOX ERROR.

         RETURN ERROR.
     END.

     IF P_VlrNoDist GT 0 OR P_VlrNoDist LT 0 THEN DO:
        MESSAGE "El Programa AboCreditos.P...Retornó valor no distribuido para" SKIP
                "el Nit. : " W_NitCte ", Cod_producto : " TPdctos.CodP ", Nro-Crédito : " TPdctos.CtaP SKIP
                "                        Revise por favor...Distribución cancelada."
                VIEW-AS ALERT-BOX ERROR.
        
        RETURN ERROR.
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
     
     IF TempCtas.Agen NE W_Agencia THEN 
        RUN CompletaSyA.                                                                                              
                                                              
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ComboPagos W-Rec_XLibranza 
PROCEDURE ComboPagos :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  Cmb_Pagos:LIST-ITEMS IN FRAME F_Proc = "".
  DEFINE VAR wcant AS LOGICAL INITIAL FALSE.
  FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empr     EQ W_CodEmp
                        AND Rec_Nomina.Fec_Contabil EQ ? NO-LOCK
                            BREAK BY Rec_Nomina.Nro_pago:
      IF FIRST-OF(Rec_Nomina.Nro_pago) THEN DO:
         Cmb_Pagos:ADD-LAST(STRING(Rec_Nomina.Nro_pago,"999999") + " - " +
                            STRING(Rec_Nomina.Fec_Pago,"99/99/9999")).
         
         IF Rec_Nomina.Nro_Pago EQ W_NroPago THEN
            Cmb_Pagos:SCREEN-VALUE = STRING(Rec_Nomina.Nro_pago,"999999") + " - " +
                                     STRING(Rec_Nomina.Fec_Pago,"99/99/9999").    
      END.
      wcant = TRUE.
  END.
 /*   IF wcant THEN
       Cmb_Pagos:ADD-LAST("999999" + " - " + "BORRRAR PLAN PAGOS"). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CompletaSyA W-Rec_XLibranza 
PROCEDURE CompletaSyA :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
 DEFI VAR W_OtraAg LIKE Agencias.Agencia.

 FIND FIRST CopMov_Contable WHERE CopMov_Contable.Agencia EQ TempCtas.Agen
                              AND CopMov_Contable.Cuenta  EQ TempCtas.CtaSyA
                              AND CopMov_Contable.Nit     EQ STRING(W_Agencia,"999")
                              AND CopMov_Contable.Db      GT 0   NO-ERROR.
 IF NOT AVAIL(CopMov_Contable) THEN                                                  
    CREATE CopMov_Contable.                                                          
                                                                                     
 ASSIGN CopMov_Contable.Agencia        = TempCtas.Agen                               
        CopMov_Contable.Cuenta         = TempCtas.CtaSyA                             
        CopMov_Contable.Nit            = STRING(W_Agencia,"999")                        
        CopMov_Contable.Fec_Contable   = W_Fecha                                     
        CopMov_Contable.Comentario     = "Distrib.Recaudo"                           
        CopMov_Contable.Usuario        = W_Usuario                                   
        CopMov_Contable.Cen_Costos     = W_Cencosgral                                
        CopMov_Contable.Destino        = W_Agencia                                   
        CopMov_Contable.Comprobante    = Comprobantes.Comprobante                    
        CopMov_Contable.Num_Documento  = Comprobantes.Secuencia                      
        CopMov_Contable.Fec_Grabacion  = TODAY                                       
        CopMov_Contable.Hora           = TIME                                        
        CopMov_Contable.Estacion       = W_Estacion                                  
        CopMov_Contable.Db             = CopMov_Contable.Db + TPdctos.VrDist         
        W_OtraAg                       = TempCtas.Agen.                              
                                                                                 
 /*Comentariado Abril 13/05 GAER, Debe ser con la misma cta de la partida Original
   FIND FIRST TempCtas WHERE TempCtas.Agen EQ W_Agencia                                
                       AND TempCtas.TipP EQ TPdctos.TP                                      
                       AND TempCtas.Pto  EQ TPdctos.CodP NO-LOCK NO-ERROR.     */
                                                                                 
 FIND FIRST CopMov_Contable WHERE CopMov_Contable.Agencia EQ W_Agencia               
                              AND CopMov_Contable.Cuenta  EQ TempCtas.CtaSyA 
                              AND CopMov_Contable.Nit     EQ STRING(W_OtraAg,"999") 
                              AND CopMov_Contable.Cr      GT 0 NO-ERROR.             
 IF NOT AVAIL(CopMov_Contable) THEN                                                  
    CREATE CopMov_Contable.                                                          
                                                                                     
 ASSIGN CopMov_Contable.Agencia        = W_Agencia                                   
        CopMov_Contable.Cuenta         = TempCtas.CtaSyA                             
        CopMov_Contable.Nit            = STRING(W_OtraAg,"999")                        
        CopMov_Contable.Fec_Contable   = W_Fecha                                     
        CopMov_Contable.Comentario     = "Distrib.Recaudo"                           
        CopMov_Contable.Usuario        = W_Usuario                                   
        CopMov_Contable.Cen_Costos     = W_Cencosgral                                
        CopMov_Contable.Destino        = W_Agencia                                    
        CopMov_Contable.Comprobante    = Comprobantes.Comprobante                    
        CopMov_Contable.Num_Documento  = Comprobantes.Secuencia                      
        CopMov_Contable.Fec_Grabacion  = TODAY                                       
        CopMov_Contable.Hora           = TIME                                        
        CopMov_Contable.Estacion       = W_Estacion                                  
        CopMov_Contable.Cr             = CopMov_Contable.Cr + TPdctos.VrDist.        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar W-Rec_XLibranza 
PROCEDURE Contabilizar :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
 DEFI VAR TotT       LIKE Mov_Contable.Db INIT 0.
 DEFI VAR Listado    AS CHAR FORM "X(40)".
 DEFI VAR W_SiErr    AS LOG INIT FALSE.
 DEFI VAR K          AS INTEG FORM "99".
 DEFI VAR P_ImpAplic LIKE Mov_Contable.Db INIT 0.
 
 DO TRANSACTION ON ERROR UNDO:
    ASSIGN Comprobantes.Secuencia    = Comprobantes.Secuencia    + 1
           Empresas.Consecutivo_Pago = Empresas.Consecutivo_Pago + 1
           Empresas.Fec_UltPago[1]   = W_Fecha.        
        
    FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
    FIND CURRENT Empresas     NO-LOCK NO-ERROR.

    RUN Contab_RecLibr.R (INPUT W_CodEmp,W_NroPago,Txt_Contrapartida:SCREEN-VALUE IN FRAME F_Proc,
                                Comprobantes.Comprobante,Comprobante.Secuencia,
                          OUTPUT W_SiErr).
    IF W_SiErr THEN DO:
       MESSAGE "POR FAVOR,    Revise la Contabilización y el Abono en C/Producto," SKIP
               "Hubo ERRORES, La Totalidad de la Transacción debe quedar DESTRUÏDA." 
           VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.

    RETURN.

    FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp
                          AND Rec_Nomina.Nro_Pago    EQ W_NroPago
                          AND Rec_Nomina.Fec_Contab  EQ ?:
        IF Rec_Nomina.Val_Deduc GT 0 THEN DO:
           ASSIGN W_NitCte            = Rec_Nomina.Nit
                  W_Cont              = W_Cont + 1
                  W_Cont:SCREEN-VALUE IN FRAME F_Proc = STRING(W_Cont).

           RUN DistRecaudo NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
              RETURN ERROR.

           ASSIGN W_TotNoD              = W_TotNoD + W_VrADist
                  W_TotNoD:SCREEN-VALUE = STRING(W_TotNoD)
                  Rec_Nomina.Fec_Contab = W_Fecha.
        END.
        ELSE Rec_Nomina.Fec_Contab = W_Fecha.
    END.

    FIND FIRST Cuentas WHERE Cuentas.Tipo      EQ 2
                         AND Cuentas.Estado    EQ 1
                         AND Cuentas.Cuenta    EQ Txt_Contrapartida:SCREEN-VALUE NO-LOCK NO-ERROR.    

    CREATE Mov_Contable.                                                 
    ASSIGN Mov_Contable.Agencia        = W_Agencia                          
           Mov_Contable.Cuenta         = Cuentas.Cuenta                     
           Mov_Contable.Nit            = Empresas.Nit                       
           Mov_Contable.Fec_Contable   = W_Fecha                            
           Mov_Contable.Comentario     = "Distrib.Recaudo"                  
           Mov_Contable.Usuario        = W_Usuario                          
           Mov_Contable.Cen_Costos     = W_Cencosgral                       
           Mov_Contable.Destino        = W_Agencia                          
           Mov_Contable.Comprobante    = Comprobantes.Comprobante           
           Mov_Contable.Num_Documento  = Comprobantes.Secuencia             
           Mov_Contable.Fec_Grabacion  = TODAY                              
           Mov_Contable.Hora           = TIME                               
           Mov_Contable.Estacion       = W_Estacion                         
           Mov_Contable.Db             = W_ContabDistSi + W_ContabDistNo.                    

    IF W_ContabDistNo GT 0 THEN DO:
       CREATE Mov_Contable.                                          
       ASSIGN Mov_Contable.Agencia        = W_Agencia                
              Mov_Contable.Cuenta         = Cuentas.Cuenta           
              Mov_Contable.Nit            = Empresas.Nit             
              Mov_Contable.Fec_Contable   = W_Fecha                  
              Mov_Contable.Comentario     = "NO-Distrib.en Recaudo"        
              Mov_Contable.Usuario        = W_Usuario                
              Mov_Contable.Cen_Costos     = W_Cencosgral             
              Mov_Contable.Destino        = W_Agencia                
              Mov_Contable.Comprobante    = Comprobantes.Comprobante 
              Mov_Contable.Num_Documento  = Comprobantes.Secuencia   
              Mov_Contable.Fec_Grabacion  = TODAY                    
              Mov_Contable.Hora           = TIME                     
              Mov_Contable.Estacion       = W_Estacion               
              Mov_Contable.Cr             = W_ContabDistNo.
    END.

    FOR EACH CopMov_Contable:
        CREATE Mov_Contable.
        BUFFER-COPY CopMov_Contable TO Mov_Contable.

        DELETE CopMov_Contable.
    END.

    FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                            AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                            AND Mov_Contable.Fec_Contable  EQ W_Fecha NO-LOCK
                                BREAK BY Mov_Contable.Agencia:
        ASSIGN TotT = TotT + (Mov_Contable.Db - Mov_Contable.Cr).

        IF LAST-OF(Mov_Contable.Agencia) AND TotT NE 0 THEN DO:
           MESSAGE "Los DEBE - HABER en la Agencia : " Mov_Contable.Agencia SKIP
                   "                 Están Diferentes...Revise por favor."
                      VIEW-AS ALERT-BOX ERROR.
           W_SiErr = TRUE.
        END.
    END.

   /* DO K = 1 TO 20:  /*Solo si se desea aplicar gmf a los salario*/
       IF TotAho_Emp11[K] GT 0 THEN DO:
          RUN RutGMF.R (INPUT  TRUE,W_Agencia,K,3,001,"890909246-7", 
                        INPUT  txt_contrapartida:SCREEN-VALUE,040101018,TotAho_Emp11[K], 
                        INPUT  Comprobantes.Comprobante,                                                   
                        INPUT  STRING(Comprobantes.Secuencia),"Abono Salarios",1,0,                         
                        OUTPUT P_ImpAplic) NO-ERROR.                                                       
          IF ERROR-STATUS:ERROR THEN DO:                                                               
             MESSAGE "El programa RutGMF.P...Retornó ERROR, no se permite la Operación." SKIP
                     TotAho_Emp11[K]
                VIEW-AS ALERT-BOX ERROR.                                                              
             W_SiErr = TRUE.                                                                             
          END.          
       END.
    END.*/

    ASSIGN listado  = W_PathSpl + "CpteDRContab-" + STRING(W_CodEmp) + "-" + STRING(W_NroPago)  + ".Lst"
           W_Sicpte = TRUE.
                                                                                                                
    {Incluido\Imprimir.I "listado"} 

    ASSIGN W_Sicpte = FALSE.

    IF W_SiErr THEN 
       RETURN ERROR.
 END.  /*Fin Tx*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ContablesAhorro W-Rec_XLibranza 
PROCEDURE ContablesAhorro :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia        = TempCtas.Agen             
         Mov_Contable.Cuenta         = TempCtas.CtaPro          
         Mov_Contable.Nit            = W_NitCte
         Mov_Contable.Fec_Contable   = W_Fecha                        
         Mov_Contable.Comentario     = "Distrib.Recaudo"          
         Mov_Contable.Usuario        = W_Usuario                      
         Mov_Contable.Cen_Costos     = W_Cencosgral                   
         Mov_Contable.Destino        = W_Agencia                      
         Mov_Contable.Comprobante    = Comprobantes.Comprobante       
         Mov_Contable.Num_Documento  = Comprobantes.Secuencia         
         Mov_Contable.Fec_Grabacion  = TODAY                          
         Mov_Contable.Hora           = TIME                           
         Mov_Contable.Estacion       = W_Estacion               
         Mov_Contable.Cr             = TPdctos.VrDist.

  IF TempCtas.Agen NE W_Agencia THEN
      RUN CompletaSyA.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Rec_XLibranza  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Rec_XLibranza)
  THEN DELETE WIDGET W-Rec_XLibranza.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DistRecaudo W-Rec_XLibranza 
PROCEDURE DistRecaudo :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR J  AS INTEG FORM "99".
  DEFINE VAR v_forma_pago like empresas.for_pago.
  FOR EACH TPdctos. DELETE TPdctos. END.
    
  ASSIGN W_PorD                                = 0
         W_PorD:SCREEN-VALUE IN FRAME F_Proc = "0.00"
         W_Dist                                = 0
         W_Dist:SCREEN-VALUE                   = "0.00"
         w_apagar                              = 0
         W_apagar:SCREEN-VALUE                 = "0.00".

  /*27/01/2005 Edisson Alexander Moncada  Hallamos periocidad de la empresa*/
  v_forma_pago = 3.
  FIND FIRST empresas WHERE empresas.cod_empresa = W_CodEmp  NO-LOCK NO-ERROR.
  IF AVAIL empresas THEN DO:
     v_forma_pago = Empresas.For_Pago.
  END.

  IF NOT W_Inf THEN 
     FIND LAST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp
                            AND Rec_Nomina.Nro_Pago    EQ W_NroPago
                            AND Rec_Nomina.Nit         EQ W_NitCte
                            AND Rec_Nomina.Val_Deduc   GT 0
                            AND Rec_Nomina.Fec_Contab  EQ ? NO-LOCK NO-ERROR. 

  IF NOT AVAIL(Rec_Nomina) THEN RETURN.
  
   
                
  FOR EACH Ahorros WHERE Ahorros.Nit      EQ W_NitCte
                     AND Ahorros.Cuota    GT 0
                     AND Ahorros.FOR_Pago EQ 2
                     AND Ahorros.Estado   EQ 1 NO-LOCK BY Ahorros.Cod_Ahorro:
      FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Ahorros.Cod_Ahorro
                               AND Pro_Ahorros.Estado     EQ 1 NO-LOCK NO-ERROR. 
      CREATE TPdctos.
      ASSIGN TPdctos.Prior    = Pro_Ahorros.Prioridad
             TPdctos.CodP     = Ahorros.Cod_Ahorro
             TPdctos.TP       = "A"
             TPdctos.Agen     = Ahorros.Agencia
             TPdctos.CtaP     = Ahorros.Cue_Ahorro
             TPdctos.FApert   = Ahorros.Fec_Apertura
             TPdctos.Cuota    = Ahorros.Cuota
             TPdctos.CuoOrig  = Ahorros.Cuota
             TPdctos.VrSdo    = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
             TPdctos.VrDist   = 0
             TPdctos.NC       = 0
             TPdctos.NomP     = Pro_Ahorros.Nom_Produc
             w_apagar         = w_apagar + TPdctos.cuoorig
             w_apagar:SCREEN-VALUE = STRING(w_apagar).                      
      /*buscamos la pereiocidad del pago de los creditos, y si los creditos tienen
        periocidad diferente a la empresa respetamos la periocidad de los creditos*/
      IF ahorros.Per_Deduccion EQ 4 AND v_forma_pago EQ 3 THEN
         TPdctos.Cuota = round(Ahorros.Cuota / 2,0).

  END.                     
    
  FOR EACH Creditos WHERE Creditos.Nit         EQ W_NitCte
                      AND Creditos.Cuota       GT 0
                      AND Creditos.FOR_Pago    EQ 2
                      AND Creditos.Sdo_capital GT 0 NO-LOCK BY Creditos.Cod_Credito:
      FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito
                                AND Pro_Creditos.Estado      EQ 1 NO-LOCK NO-ERROR. 
      CREATE TPdctos.
      ASSIGN TPdctos.Prior    = Pro_Creditos.Prioridad
             TPdctos.CodP     = Creditos.Cod_Credito
             TPdctos.TP       = "C"
             TPdctos.CtaP     = STRING(Creditos.Num_Credito)
             TPdctos.Agen     = Creditos.Agencia
             TPdctos.FApert   = Creditos.Fec_Desemb
             TPdctos.Cuota    = Creditos.Cuota
             TPdctos.CuoOrig  = Creditos.Cuota
             TPdctos.VrSdo    = Creditos.Honorarios    + Creditos.Costas         + Creditos.Polizas +
                                Creditos.Int_MorCobrar + Creditos.Int_MoraDifCob +
                                Creditos.Int_DifCobro  + Creditos.Int_Corrientes +
                                Creditos.Sdo_Capital   - Creditos.Int_Anticipado
             TPdctos.VrDist   = 0
             TPdctos.NC       = 0
             TPdctos.NomP     = Pro_Creditos.Nom_Produc
             w_apagar         = w_apagar + TPdctos.cuoorig
             w_apagar:SCREEN-VALUE = STRING(w_apagar). 

      /* Alexander Moncada adiccion 27/01/2005*/
      IF creditos.Per_pago EQ 4 AND v_forma_pago EQ 3 THEN
         ASSIGN  TPdctos.Cuota = round(Creditos.Cuota / 2,0)
                 TPdctos.CuoOrig = round(Creditos.Cuota / 2,0).
      
      /*IF Creditos.Cuota GT TPdctos.VrSdo THEN
         TPdctos.Cuota = TPdctos.VrSdo.*/
      
      /* Alexander Moncada adiccion 27/01/2005*/
       IF TPdctos.Cuota GT TPdctos.VrSdo THEN
          TPdctos.Cuota = TPdctos.VrSdo.



  END.  

  ASSIGN W_VrADist = Rec_Nomina.Val_Deduc.

  DO J = 1 TO Rec_Nomina.Num_Cuotas:
     FOR EACH TPdctos BY TPdctos.Prior BY TPdctos.FApert:
         IF W_VrADist GE TPdctos.Cuota THEN
            ASSIGN TPdctos.VrDist = TPdctos.VrDist + TPdctos.Cuota
                   TPdctos.NC     = TPdctos.NC + 1
                   W_VrADist      = W_VrADist - TPdctos.Cuota.
         ELSE 
            ASSIGN TPdctos.VrDist = TPdctos.VrDist + W_VrADist
                   TPdctos.NC     = TPdctos.NC + 1
                   W_VrADist      = 0.

         IF TPdctos.TP EQ "C" THEN DO:
            IF TPdctos.VrSdo - TPdctos.VrDist LE 0 THEN
               TPdctos.Cuota = 0. 
            ELSE IF  TPdctos.Cuota GT 0
                 AND TPdctos.Cuota GT (TPdctos.VrSdo - TPdctos.VrDist) THEN
                     TPdctos.Cuota = (TPdctos.VrSdo - TPdctos.VrDist).         
         END.
            
         IF W_VrADist LE 0 THEN
            LEAVE.         
     END.

     IF W_VrADist LE 0 THEN
        LEAVE.
  END.

  IF W_VrADist GT 0 THEN 
     FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK
                             BY Pro_Ahorros.Prioridad DESCEND:
         FIND FIRST Ahorros WHERE Ahorros.Nit        EQ W_NitCte
                              AND Ahorros.Cod_Ahorro EQ Pro_Ahorros.Cod_Ahorro
                              AND Ahorros.Estado     EQ 1 NO-LOCK NO-ERROR.
         IF AVAIL(Ahorros) THEN DO:                                                                              
            FIND FIRST TPdctos WHERE TPdctos.TP   = "A"                                                          
                                 AND TPdctos.CodP = Ahorros.Cod_Ahorro                                           
                                 AND TPdctos.CtaP = Ahorros.Cue_Ahorro NO-ERROR.                                 
            IF NOT AVAIL(TPdctos) THEN                                                                           
               CREATE TPdctos.                                                                                   
                                                                                                                 
            ASSIGN TPdctos.Prior    = Pro_Ahorros.Prioridad                                                      
                   TPdctos.CodP     = Ahorros.Cod_Ahorro                                                         
                   TPdctos.CtaP     = Ahorros.Cue_Ahorro                                                         
                   TPdctos.TP       = "A" 
                   TPdctos.Agen     = Ahorros.Agencia
                   TPdctos.FApert   = Ahorros.Fec_Apertura                                                       
                   TPdctos.Cuota    = Ahorros.Cuota                                                              
                   TPdctos.CuoOrig  = Ahorros.Cuota 
                   TPdctos.VrSdo    = Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
                   TPdctos.VrDist   = TPdctos.VrDist + W_VrADist                                                 
                   TPdctos.NomP     = Pro_Ahorros.Nom_Produc                                                     
                   W_VrADist        = 0.  
            LEAVE.
         END.                                                                                                    
  END.

  IF NOT W_Inf THEN DO:
     IF W_VrADist GT 0 THEN
        MESSAGE "El Cliente NO tiene Ctas-Ahorro para Abonarle el Remanente de la Distribución..." SKIP 
                         "                    Revise por Favor."                                                 
                         VIEW-AS ALERT-BOX.              

     FOR EACH TPdctos:                                                     
         ASSIGN w_apagar = w_apagar + TPdctos.cuoorig
                W_Dist   = W_Dist + TPdctos.VrDist.

         IF TPdctos.TP EQ "C" AND TPdctos.VrDist GT 0 THEN DO:
            RUN AboCredito.R         /*Distribuye abonos en Créditos,sin grabar*/
                (INPUT  FALSE,                                                       
                 INPUT  TPdctos.Agen,TPdctos.CodP,W_NitCte,          
                 INPUT  TPdctos.CtaP,TPdctos.VrDist,                         
                 INPUT  1,1,0,0,                                    
                 OUTPUT TPdctos.Abonos[8],OUTPUT TPdctos.Abonos[7],OUTPUT TPdctos.Abonos[6],
                 OUTPUT TPdctos.Abonos[9],
                 OUTPUT TPdctos.Abonos[5],OUTPUT TPdctos.Abonos[4],OUTPUT TPdctos.Abonos[3],
                 OUTPUT TPdctos.Abonos[2],OUTPUT TPdctos.Abonos[1],
                 OUTPUT P_VlrNoDist).                                                
         END.
     END.                                                                  
                                                                           
     ASSIGN W_PorD              = Rec_Nomina.Val_Deduc                     
            W_PorD:SCREEN-VALUE = STRING(W_PorD)                           
            W_Dist:SCREEN-VALUE = STRING(W_Dist).                          
                                                                           
     OPEN QUERY Br_Pdctos FOR EACH TPdctos NO-LOCK INDEXED-REPOSITION.     
  END.
  ELSE DO:
     IF W_SiContab AND W_VrADist GT 0 THEN 
        ASSIGN W_ContabDistNo = W_ContabDistNo + W_VrADist.
        
     FIND FIRST Clientes WHERE Clientes.Nit       EQ W_NitCte 
                         AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.  

     FOR EACH TPdctos:
         CREATE CopTPdctos.
         BUFFER-COPY TPdctos TO CopTPdctos.

         ASSIGN CopTPdctos.NitP = W_NitCte
                CopTPdctos.NomC = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +   
                                        " " + TRIM(Clientes.Nombre)                                
                CopTPdctos.VRec = Rec_Nomina.Val_Deduc.

         IF W_SiContab AND TPdctos.VrDist GT 0 THEN DO:
            ASSIGN W_ContabDistSi = W_ContabDistSi + TPdctos.VrDist.

            RUN Abonar NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
               RETURN ERROR.           
         END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Rec_XLibranza  _DEFAULT-ENABLE
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
  DISPLAY Tgl-BorrarNomina W_CmbEmp W_CmbOfi Cmb_pagos Tg_Nuevo Rs_Opcrea 
          W_NitCte W_VrRec W_NroPago W_NCuotas Cmb_Cptes W_NomCte 
          txt_contrapartida W_TotRec W_TotNoD Msaje W_Dist W_Cont W_PorD 
          W_apagar 
      WITH FRAME F_Proc IN WINDOW W-Rec_XLibranza.
  ENABLE Tgl-BorrarNomina Btn_CreCancelado btn-Cuotas W_CmbEmp Cmb_pagos 
         Tg_Nuevo W_NitCte W_VrRec W_NCuotas Cmb_Cptes txt_contrapartida 
         BUTTON-5 Br_Rec Btn_Imp Btn_Preliminar Btn_Done Br_Pdctos RECT-314 
         RECT-316 
      WITH FRAME F_Proc IN WINDOW W-Rec_XLibranza.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  DISPLAY w-fecIni w-fecfin 
      WITH FRAME frm-FPagaduria IN WINDOW W-Rec_XLibranza.
  ENABLE w-fecIni Btn_SalirFec w-fecfin 
      WITH FRAME frm-FPagaduria IN WINDOW W-Rec_XLibranza.
  {&OPEN-BROWSERS-IN-QUERY-frm-FPagaduria}
  DISPLAY Rs_OpInf W_CmbEI Cmb_PagI W_FecIniI W_FecFinI 
      WITH FRAME F_Imp IN WINDOW W-Rec_XLibranza.
  ENABLE RECT-317 Rs_OpInf W_CmbEI Cmb_PagI W_FecIniI W_FecFinI Btn_FinImp 
         Btn_Imp-2 
      WITH FRAME F_Imp IN WINDOW W-Rec_XLibranza.
  {&OPEN-BROWSERS-IN-QUERY-F_Imp}
  VIEW W-Rec_XLibranza.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaArchivo W-Rec_XLibranza 
PROCEDURE HallaArchivo :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFINE VAR Archivo AS CHARACTER FORMAT "X(40)".
  DEFINE VAR Datos   AS CHARACTER FORMAT "X(80)".
 
  SESSION:SET-WAIT-STATE("GENERAL").
    
  ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Nuevo-Recaudo desde Archivo..."
         Msaje:VISIBLE                      = TRUE
         W_Cont:SCREEN-VALUE                = "0"
         W_Cont                             = 0
         TImp                               = 0
         TNoImp                             = 0
         NNoImp                             = 0.

  SYSTEM-DIALOG GET-FILE Archivo
     TITLE      "Selecc.el Archivo...Diseño : Cédula-X(12),Valor-9999999999.99,Nombre-X(40)"
     FILTERS    "Archivos Txt (*.Txt)"   "*.*"
     MUST-EXIST
     USE-FILENAME.
     
  INPUT FROM VALUE(Archivo).
  REPEAT:
    IMPORT UNFORMATTED Datos NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "El Archivo tiene Problemas...Revise por favor" VIEW-AS ALERT-BOX.
       LEAVE.
    END.

    CREATE Tmp.
    ASSIGN Tmp.Ced = TRIM(SUBSTRING(Datos,1,12))
           Tmp.Ced = trim(STRING(DECIMAL(Tmp.Ced)))
           Tmp.Vr  = DEC(SUBSTRING(Datos,13,11)).
           /*Tmp.Nom = SUBSTRING(Datos,27,40)*/
    FIND FIRST Clientes WHERE Clientes.Nit         EQ Tmp.Ced
                          AND Clientes.Cod_Empresa EQ W_CodEmp
                          AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.                            
    IF AVAIL(Clientes) AND Tmp.Vr GT 0 THEN DO:                                                                             
       FIND FIRST Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp                             
                               AND Rec_Nomina.Nro_Pago    EQ W_NroPago                            
                               AND Rec_Nomina.Nit         EQ Tmp.Ced                              
                               AND Rec_Nomina.Fec_Contab  EQ ? NO-ERROR.                          
       IF NOT AVAIL(Rec_Nomina) THEN                                                              
          CREATE Rec_Nomina.                                                                      
                                                                                                  
        ASSIGN Rec_Nomina.Agencia      = W_Agencia                                                
               Rec_Nomina.Cod_Empresa  = W_CodEmp                                                 
               Rec_Nomina.Nro_Pago     = W_NroPago                                                
               Rec_Nomina.Nit          = Tmp.Ced                                                  
               Rec_Nomina.Num_Cuotas   = 1                                                        
               Rec_Nomina.Fec_Contab   = ?                                                        
               Rec_Nomina.Fec_Grabac   = W_Fecha                                                  
               Rec_Nomina.Fec_Pago     = W_Fecha                                                  
               Rec_Nomina.Val_Deducido = Rec_Nomina.Val_Deducido + Tmp.Vr
               W_Cont                  = W_Cont + 1
               W_Cont:SCREEN-VALUE     = STRING(W_Cont)
               TImp                    = TImp + Tmp.Vr.
    END.                                                                                          
    ELSE DO:
        MESSAGE "La Cédula : " Tmp.Ced " de : " Tmp.Nom SKIP                                     
                "por valor de $ " Tmp.Vr "...No aparece activa en Clientes...O" SKIP 
                "El valor no es mayor que CERO(0)...........Registro Ignorado." 
                VIEW-AS ALERT-BOX.                                                                    
        ASSIGN NNoImp  = NNoImp + 1
               TNoImp  = TNoImp + Tmp.Vr
               Tmp.Err = "Cliente no existe Activo".
    END.
  END.
  INPUT CLOSE.

  RUN QueryRec.

  MESSAGE "Se Importaron : " W_cont " Registros, por valor de $ " TImp SKIP
          "Se Omitieron  : " NNoImp " Registros, por valor de $ " TNoImp
          VIEW-AS ALERT-BOX.

  DEFI VAR Listado AS CHAR FORM "X(40)".
  
  ASSIGN W_SiImp = TRUE
         listado = W_PathSpl + "ImpRec.Lst".
  
  {Incluido\Imprimir.I "listado"}

  W_SiImp = FALSE.

  FOR EACH Tmp: DELETE Tmp. END.

  SESSION:SET-WAIT-STATE("").

  ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generado Nuevo-Recaudo desde Archivo..."
         Msaje:VISIBLE                      = FALSE.

END PROCEDURE.


/*-----------------*/
PROCEDURE InfImport:
  {Incluido\RepEncabezado.I}

  ASSIGN W_Reporte    = "Reporte   : Importación Recaudo desde Archivo      Fecha del Informe: " +
                        STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
         W_EncColumna = "Empresa: " + STRING(W_CodEmp) + " - Nro.Pago: " + STRING(W_NroPago). 

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.

  FOR EACH Tmp:
      DISPLAY Tmp.Ced    LABEL "Cédula/Nit"
              Tmp.Nom    LABEL "Nombre del Cliente"
              Tmp.Vr     LABEL "Valor Recaudo"
              Tmp.Err    LABEL "Inconsistencia"
         WITH DOWN WIDTH 140 FRAME FImp USE-TEXT NO-LABELS STREAM-IO NO-BOX.
  END.

  DISPLAY SKIP(1)
          "TOTALES : Registros Importados : " + STRING(W_Cont,"999999") + " Por valor de $ " +
           STRING(Timp,"->>>>,>>>,>>9.99")                 FORMAT "X(120)" SKIP
          "-------   Registros Omitidos   : " + STRING(NNoImp,"999999") + " Por valor de $ " + 
           STRING(TNoImp,"->>>>,>>>,>>9.99")               FORMAT "X(120)"
      WITH DOWN WIDTH 140 FRAME FTtImp USE-TEXT NO-LABELS STREAM-IO NO-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallaPdctos W-Rec_XLibranza 
PROCEDURE HallaPdctos :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR T_VrRec LIKE Rec_Nomina.Val_Deducido INIT 0.
  DEFINE VAR v_forma_pago like empresas.for_pago.
                                            
  SESSION:SET-WAIT-STATE("GENERAL").
    
  ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "Generando Nuevo-Recaudo desde Pdctos de C/Cliente..."
         Msaje:VISIBLE                      = TRUE.
    
   /*27/01/2005 Edisson Alexander Moncada  Hallamos periocidad de la empresa*/
  v_forma_pago = 3.
  FIND FIRST empresas WHERE empresas.cod_empresa = W_CodEmp  NO-LOCK NO-ERROR.
  IF AVAIL empresas THEN DO:
     v_forma_pago = Empresas.For_Pago.
  END.  
    
  FOR EACH Clientes WHERE Clientes.Cod_Empresa EQ W_CodEmp
                      AND Clientes.Estado      EQ 1 NO-LOCK BY Clientes.Nit:
      
      FOR EACH Ahorros WHERE Ahorros.Nit       EQ Clientes.Nit
                         AND Ahorros.Cuota     GT 0
                         AND Ahorros.FOR_Pago  EQ 2
                         AND Ahorros.Estado    EQ 1 NO-LOCK BY Ahorros.Cod_Ahorro:
           /*buscamos la pereiocidad del pago de los creditos, y si los creditos tienen
            periocidad diferente a la empresa respetamos la periocidad de los creditos*/
            IF ahorros.Per_Deduccion EQ 4 AND v_forma_pago EQ 3 THEN
                ASSIGN T_VrRec = T_VrRec + round(Ahorros.Cuota / 2,0).              
            ELSE                              
                ASSIGN T_VrRec = T_VrRec + round(Ahorros.Cuota / 2,0).              
      END.

      FOR EACH Creditos WHERE Creditos.Nit         EQ Clientes.Nit
                          AND Creditos.Cuota       GT 0
                          AND Creditos.FOR_Pago    EQ 2
                          AND Creditos.Sdo_capital GT 0 NO-LOCK BY Creditos.Cod_Credito:
              /* Alexander Moncada adiccion 27/01/2005*/
                IF creditos.Per_pago EQ 4 AND v_forma_pago EQ 3 THEN
                   ASSIGN T_VrRec = T_VrRec + round(Creditos.Cuota / 2,0).              
                else
                   ASSIGN T_VrRec = T_VrRec + Creditos.Cuota.                     
      END.

      IF T_VrRec GT 0 THEN DO:
         CREATE Rec_Nomina.                           
         ASSIGN Rec_Nomina.Agencia      = W_Agencia       
                Rec_Nomina.Cod_Empresa  = W_CodEmp           
                Rec_Nomina.Nro_Pago     = W_NroPago     
                Rec_Nomina.Nit          = Clientes.Nit      
                Rec_Nomina.Num_Cuotas   = 1            
                Rec_Nomina.Fec_Contab   = ?             
                Rec_Nomina.Fec_Grabac   = W_Fecha       
                Rec_Nomina.Fec_Pago     = W_Fecha       
                Rec_Nomina.Val_Deducido = T_VrRec      
                T_VrRec                 = 0
                W_Cont                  = W_Cont + 1
                W_Cont:SCREEN-VALUE     = STRING(W_Cont).  
      END.
  END.

  RUN QueryRec.

  SESSION:SET-WAIT-STATE("").     

  MESSAGE "El Nuevo Recaudo fue generado desde productos de C/Cliente...Puede continuar."
          VIEW-AS ALERT-BOX.
        
  ASSIGN Msaje:SCREEN-VALUE IN FRAME F_Proc = "El Nuevo-Recaudo Ya fue generado..."
         Msaje:VISIBLE                      = FALSE
         W_Cont                             = 0
         W_Cont:SCREEN-VALUE                = STRING(W_Cont).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpCpte W-Rec_XLibranza 
PROCEDURE ImpCpte :
/*------------------------------------------------------------------------------
      Invocado desde ProcesoImprimir.
  Purpose:     Imprime el resumen contable de la Distribuciòn.
  ------------------------------------------------------------------------------*/
  DEFI VAR TotD   LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TotC   LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TTotD  LIKE Mov_Contable.Db INIT 0.
  DEFI VAR TTotC  LIKE Mov_Contable.Db INIT 0.

  {Incluido\RepEncabezado.I}

  ASSIGN W_Reporte = "Cpte Resumen : Distribución Contabilizada de Recaudo      Fecha del Informe: " +
                      STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
         W_EncColumna = "Comprobante: " + STRING(Comprobantes.Comprobante,"99") + "-" + 
                        STRING(Comprobantes.Secuencia,"99999999") + "      Empresa: " + 
                        STRING(W_CodEmp) + " - Nro.Pago: " + STRING(W_NroPago).

  VIEW FRAME F-Encabezado.
  VIEW FRAME f-ftr.

  FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ Comprobantes.Comprobante
                          AND Mov_Contable.Num_Documento EQ Comprobantes.Secuencia
                          AND Mov_Contable.Fec_Contable  EQ W_Fecha NO-LOCK
                              BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta:
      ASSIGN TotD  = TotD  + Mov_Contable.Db
             TTotD = TTotD + Mov_Contable.Db
             TotC  = TotC  + Mov_Contable.Cr
             TTotC = TTotC + Mov_Contable.Cr.

      IF LAST-OF(Mov_Contable.Cuenta) THEN DO:
         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta
                              AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
         DISPLAY Mov_Contable.Agencia   LABEL "Ag."
                 Mov_Contable.Cuenta    LABEL "Cta-Contable"
                 Cuentas.Nombre         LABEL "Descripciòn de la Cuenta" WHEN AVAIL(Cuentas)
                 TotD                   LABEL "TOTAL DEBITOS"  FORM "->>>>>>,>>>,>>9.99"
                 TotC                   LABEL "TOTAL CREDITOS" FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME F21 USE-TEXT NO-LABELS STREAM-IO NO-BOX.

         ASSIGN TotD  = 0
                TotC  = 0.
      END.

  END.

  DISPLAY SKIP(1)
          "                     TOTAL FINAL------------>               ------------------ ------------------"
          SKIP
          "                                                           "
          TTotD      FORM "->>>>>>,>>>,>>9.99"
          TTotC      FORM "->>>>>>,>>>,>>9.99"
             WITH DOWN WIDTH 140 FRAME FT21T USE-TEXT NO-LABELS STREAM-IO NO-BOX.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MovAhorro W-Rec_XLibranza 
PROCEDURE MovAhorro :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
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
         Mov_Ahorros.Val_Efectivo   = TPdctos.VrDist
         Mov_Ahorros.Cpte           = Comprobantes.Comprobante
         Mov_Ahorros.Cod_Operacion  = W_OpAboAho
         Mov_Ahorros.Descrip        = "Consig.X Rec-Nómina".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Rec_XLibranza 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  IF W_SiCpte THEN DO:
     RUN ImpCpte.

     RETURN.
  END.

  IF W_SiImp THEN DO:
     RUN InfImport.

     RETURN.
  END.

  IF W_SiImpF THEN DO:
     RUN InfFrame.

     RETURN.
  END.

  {Incluido\RepEncabezado.I}

  DO WITH FRAME F_Proc. END.

  ASSIGN W_Reporte    = "Reporte   : Distribución Preliminar de Recaudo      Fecha del Informe: " +
                        STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS")
         W_EncColumna = "Cuentas        Pdcto Descripción                          Cuota        Vr.Distribuido NC       Sdo.Ant.Pdcto.     Diferencia"
         TDist        = 0
         TTDist       = 0
         TRec         = 0
         TDif         = 0.

 IF W_SiContab THEN
    W_Reporte    = "Reporte   : Distribución Contabilizada de Recaudo      Fecha del Informe: " +
                        STRING(W_Fecha,"99/99/9999") + "      Hora : " + STRING(TIME,"HH:MM:SS").

 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.

 DISPLAY SKIP(0)
         "Empresa : " + W_CmbEmp:SCREEN-VALUE + " Nro.de Pago : " + 
                         W_NroPago:SCREEN-VALUE FORMAT "X(120)" 
        SKIP(1)
     WITH DOWN WIDTH 150 FRAME F1 NO-LABELS.

 FOR EACH CopTPdctos BREAK BY CopTPdctos.Nit:
     ASSIGN TDist = TDist + CopTPdctos.VrDist
            TCuot = TCuot + CopTPdctos.CuoOrig.

     DISPLAY CopTPdctos.CtaP        
             CopTPdctos.CodP       
             CopTPdctos.NomP         FORM "X(22)"
             CopTPdctos.CuoOrig    
             CopTPdctos.VrDist     
             CopTPdctos.NC         
             CopTPdctos.VrSdo         SKIP(0)
         WITH DOWN WIDTH 150 FRAME Det NO-BOX NO-LABELS STREAM-IO USE-TEXT.

     IF LAST-OF(CopTPdctos.Nit) THEN
        RUN TotXNit.
 END.

 DISPLAY  SKIP(2)
          "Total General   :                              "
          TRec                                   FORMAT "->>>>>>>,>>9.99"
          "     "
          TTDist                                 FORMAT "->>>>>>>,>>9.99"
          "                       "
          TDif                                   FORMAT "->>>>>>>,>>9.99"
        WITH DOWN WIDTH 150 FRAME TotGral NO-BOX NO-LABELS STREAM-IO USE-TEXT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE QueryRec W-Rec_XLibranza 
PROCEDURE QueryRec :
/*------------------------------------------------------------------------------
  Purpose:      
------------------------------------------------------------------------------*/
  ASSIGN W_TotRec                              = 0
         W_TotRec:SCREEN-VALUE IN FRAME F_Proc = "0.00".
    
  CLOSE QUERY Br_Rec.                                              
  CLOSE QUERY Br_Pdctos.  
    
  FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp          
                        AND Rec_Nomina.Nro_Pago    EQ W_NroPago         
                        AND Rec_Nomina.Fec_Contab  EQ ? NO-LOCK:
      ASSIGN W_TotRec              = W_TotRec + Rec_Nomina.Val_Deduc
             W_TotRec:SCREEN-VALUE = STRING(W_TotRec).
  END.
    
                                                                         
  OPEN QUERY Br_Rec FOR EACH Rec_Nomina WHERE Rec_Nomina.Cod_Empresa EQ W_CodEmp          
                                          AND Rec_Nomina.Nro_Pago    EQ W_NroPago         
                                          AND Rec_Nomina.Fec_Contab  EQ ? NO-LOCK,        
                        EACH Clientes WHERE Clientes.Nit    EQ Rec_Nomina.Nit
                                        AND Clientes.Estado EQ 1 NO-LOCK.       
                                                                                                             
  RUN ComboPagos.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TotXNit W-Rec_XLibranza 
PROCEDURE TotXNit :
/*------------------------------------------------------------------------------
  Purpose:     
  ------------------------------------------------------------------------------*/
  DISPLAY SKIP(0)
          CopTPdctos.NitP
          CopTPdctos.NomC                       FORMAT "X(34)"
          /*CopTPdctos.VRec                       FORMAT "->>>>>>>,>>9.99"*/
          TCuot                                 FORMAT "->>>>>>>,>>9.99"
          "     "
          TDist                                 FORMAT "->>>>>>>,>>9.99"
          "                       "
          (TDist - TCuot)                       FORMAT "->>>>>>>,>>9.99" SKIP(1)
        WITH DOWN WIDTH 150 FRAME Tot1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  ASSIGN TTDist = TTDist + TDist
         TRec   = TRec   + TCuot
         TDif   = TDif   + (TDist - CopTPdctos.VRec)
         TDist  = 0
         TCuot  = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida W-Rec_XLibranza 
PROCEDURE Valida :
/*------------------------------------------------------------------------------
  Purpose: Valida y halla las Ctas-Contables (Las Graba en tabla-temp TempCtas).
------------------------------------------------------------------------------*/
  FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK 
                          BY Pro_Ahorros.Cod_Ahorro:
      FOR EACH CortoLargo WHERE CortoLargo.Clase_Producto EQ 1
                            AND CortoLargo.Cod_Producto   EQ Pro_Ahorros.Cod_Ahorro
                            AND CortoLargo.Plazo_Inicial  GE 0 NO-LOCK
               BREAK BY CortoLargo.Agencia BY CortoLargo.Cod_Producto BY CortoLargo.Plazo_Inicial:
          IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
             FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd
                                  AND Cuentas.Tipo   EQ 2
                                  AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
             IF AVAIL(Cuentas) THEN 
                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_SyA
                                     AND Cuentas.Tipo   EQ 2
                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.

             IF NOT AVAIL(Cuentas) THEN DO:
                MESSAGE "En CortoLargo.Cta_AsoAd y CortoLargo.Cta_SyA deben existir Activas en Cuentas..." SKIP
                        "Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro   SKIP
                        "De la Agencia : "                  CortoLargo.Agencia
                        VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
             END.

             CREATE TempCtas.
             ASSIGN TempCtas.Age    = CortoLargo.Agencia
                    TempCtas.TipP   = "A"
                    TempCtas.Pto    = CortoLargo.Cod_Producto
                    TempCtas.CtaPro = CortoLargo.Cta_AsoAd
                    TempCtas.CtaSyA = CortoLargo.Cta_SyA.
          END.
      END.
  END.
    
  FOR EACH Pro_Creditos WHERE Pro_Creditos.Estado EQ 1 NO-LOCK 
                           BY Pro_Creditos.Cod_Credito:
      FOR EACH CortoLargo WHERE CortoLargo.Clase_Producto EQ 2
                            AND CortoLargo.Cod_Producto   EQ Pro_Creditos.Cod_Credito
                            AND CortoLargo.Plazo_Inicial  GE 0 NO-LOCK
               BREAK BY CortoLargo.Agencia BY CortoLargo.Cod_Producto BY CortoLargo.Plazo_Inicial:
          IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:
             FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd
                                  AND Cuentas.Tipo   EQ 2
                                  AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
             IF AVAIL(Cuentas) THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_SyA
                                     AND Cuentas.Tipo   EQ 2
                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                IF AVAIL(Cuentas) THEN DO:
                   FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_CostasDB 
                                        AND Cuentas.Tipo   EQ 2
                                        AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                   IF AVAIL(Cuentas) THEN DO:
                      FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_HonorariosDB 
                                           AND Cuentas.Tipo   EQ 2
                                           AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                      IF AVAIL(Cuentas) THEN
                         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_PolizasDB 
                                              AND Cuentas.Tipo   EQ 2
                                              AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR. 
                   END.
                END.
             END.
                
             IF NOT AVAIL(Cuentas) THEN DO:
                MESSAGE "En CortoLargo.Cta_AsoAd,Cta_SyA,Cta_CostasDB,Cta_HonorariosDB,Cta_PolizasDB..." SKIP
                        "deben existir Activas en Cuentas...Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito   SKIP
                        "De la Agencia : "                  CortoLargo.Agencia
                        VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
             END.

             CREATE TempCtas.
             ASSIGN TempCtas.Age    = CortoLargo.Agencia
                    TempCtas.TipP   = "C"
                    TempCtas.Pto    = CortoLargo.Cod_Producto
                    TempCtas.CtaPro = CortoLargo.Cta_AsoAd
                    TempCtas.CtaSyA = CortoLargo.Cta_SyA
                    TempCtas.CtaHon = CortoLargo.Cta_HonorariosDB
                    TempCtas.CtaPol = CortoLargo.Cta_PolizasDB
                    TempCtas.CtaCos = CortoLargo.Cta_CostasDB.
                
             FIND FIRST Liqui_Int WHERE Liqui_Int.Clase_Producto EQ 2
                                    AND Liqui_Int.Cod_Producto   EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
             IF NOT AVAIL(Liqui_Int) THEN DO:
                MESSAGE "Falta Liqui_Int Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito   SKIP
                        VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
             END.
             
             FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_LiqAso
                                  AND Cuentas.Tipo   EQ 2
                                  AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
             IF AVAIL(Cuentas) THEN DO:
                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_LiqAso 
                                     AND Cuentas.Tipo   EQ 2
                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                IF AVAIL(Cuentas) THEN DO:
                   FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaInt_AntAso 
                                        AND Cuentas.Tipo   EQ 2
                                        AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                   IF AVAIL(Cuentas) THEN DO:
                      FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_MoraAso 
                                           AND Cuentas.Tipo   EQ 2
                                           AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                      IF AVAIL(Cuentas) THEN DO:
                         FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaDb_DifCobAso 
                                              AND Cuentas.Tipo   EQ 2
                                              AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                         IF AVAIL(Cuentas) THEN
                            FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Liqui_Int.CtaCr_DifCobAso 
                                                 AND Cuentas.Tipo   EQ 2
                                                 AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.
                      END.
                   END.
                END.
             END.

             IF NOT AVAIL(Cuentas) THEN DO:
                MESSAGE "En Liqui_Int las Cuentas : CtaCr_LiqAso,CtaDb_LiqAso,CtaCr_DifCobAso"
                        "                           CtaInt_AntAso,CtaDb_MoraAso,CtaDb_DifCobAso" SKIP
                        "Deben existir Activas en Plan de Cuentas..." SKIP
                        "Para el Pro_Creditos.Cod_Credito : " Pro_Creditos.Cod_Credito
                        VIEW-AS ALERT-BOX ERROR.

                RETURN ERROR.
             END.

             ASSIGN TempCtas.CtaLiq = Liqui_Int.CtaDb_LiqAso
                    TempCtas.CtaIng = Liqui_Int.CtaCr_LiqAso
                    TempCtas.IntAnt = Liqui_Int.CtaInt_AntAso
                    TempCtas.IntMor = Liqui_Int.CtaDb_MoraAso 
                    TempCtas.DifCoD = Liqui_Int.CtaDb_DifCobAso
                    TempCtas.DifCoH = Liqui_Int.CtaCr_DifCobAso
                    TempCtas.Oper   = Liqui_Int.Cod_Operacion.
          END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

