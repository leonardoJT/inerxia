&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 
/*------------------------------------------------------------------------
  File: W-ProRec_Refinancia.w
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{INCLUIDO\VARIABLE.I "SHARED"}
DEFINE INPUT PARAMETER P_NomIns AS CHARACTER FORMAT "X(50)".
DEFINE INPUT PARAMETER P_NitCli LIKE Clientes.Nit.
DEFINE INPUT PARAMETER P_CodCre LIKE Creditos.Cod_Credito.
DEFINE INPUT PARAMETER P_TipCre LIKE Creditos.Tip_Credito.
DEFINE INPUT PARAMETER P_NumCre LIKE Creditos.Num_Credito.
DEFINE INPUT PARAMETER P_CodOpe LIKE Operacion.Cod_Operacion.
DEFINE INPUT PARAMETER P_Progra AS INTEGER FORMAT "9".

  DEFI VAR Pagas   LIKE Creditos.Cuo_Pagadas.
  DEFI VAR FecPP   LIKE Creditos.Fec_Pago.   
  
  DEFI   VAR W_PdoPag     AS INTEG FORM "99".
  DEFI   VAR W_SiPdo      AS LOG   INIT FALSE.
  DEFI   VAR W_FecControl AS DATE.
  DEFI   VAR W_QnaDec     AS INTEG FORM "9" INIT 0.
  DEFI   VAR W_RowIdPP    AS ROWID.
  
/*para buscar un cliente*/
  DEFINE VARIABLE P_Nit       LIKE Clientes.Nit.
  DEFINE VARIABLE p_Nombre    LIKE Clientes.Nombre.
  DEFINE VARIABLE P_Apellido  LIKE Clientes.Apellido1.
  DEFINE VARIABLE P_AgeCli    LIKE Clientes.Agencia.
  DEFINE VARIABLE W_Nuevo     AS LOGICAL.
  DEFI   VAR      W_CuoExtra  LIKE Creditos.Sdo_Capital INIT 0.
  DEFINE VAR      W_Contenido AS CHARACTER FORMAT "X(400)".
  DEFI   VAR      W_NvoPdo    AS INTEG FORM "999" INIT 12.

  DEFI TEMP-TABLE TPlanP  LIKE PlanPagos.
  DEFI TEMP-TABLE CPlanP  LIKE PlanPagos.
  DEFI TEMP-TABLE TExtras LIKE Extras.
  
/*guarda los usuarios disponibles para la siguiente instancia*/
  DEFINE TEMP-TABLE TProIns
    FIELD TP_Agencia LIKE Agencias.Agencia
    FIELD TP_Orden LIKE Instancias.Orden_Instancia
    FIELD TP_Instancia LIKE Instancias.Instancia
    FIELD TP_NomInstan AS CHARACTER FORMAT "X(30)"
    FIELD TP_Usuario   LIKE Usuarios.Usuario
    FIELD TP_NomUsuar  AS CHARACTER FORMAT "X(30)"
    FIELD TP_Cantidad  AS INTEGER FORMAT "999".
    
/*Contiene los usuarios por instancia*/  
  DEFINE TEMP-TABLE TUXI
    FIELD Agencia  LIKE Usuarios.Agencia
    FIELD Usuario  LIKE Usuarios.Usuario
    FIELD Nombre   LIKE Usuarios.Nombre
    FIELD Cantidad AS INTEGER FORMAT "999"
    FIELD Proceso  AS LOGICAL.
    
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
&Scoped-define QUERY-STRING-Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit                                        AND  TExtras.Num_Solicitud EQ Creditos.Num_Credito                         NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Extras OPEN QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit                                        AND  TExtras.Num_Solicitud EQ Creditos.Num_Credito                         NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Extras TExtras
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Extras TExtras


/* Definitions for FRAME F_Extras                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Extras ~
    ~{&OPEN-QUERY-Br_Extras}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-296 RECT-320 RECT-324 RECT-325 RECT-326 ~
RECT-327 RECT-328 RECT-330 RECT-331 RECT-332 Rs_Op W_CedAbo W_FecIni ~
Btn_Acepta_Terminos Btn_Extras Rs_SistAmort Btn_Salir W_CapVdo W_CuoPaga ~
W_IntVdo W_FecPP 
&Scoped-Define DISPLAYED-FIELDS Creditos.Fec_Desembolso Creditos.Fec_Pago ~
Creditos.Sdo_Capital Creditos.Fec_UltPago Creditos.Plazo Creditos.Cuota ~
Creditos.Tasa Creditos.Int_DifCobro Creditos.Int_Corrientes ~
Creditos.Int_Anticipado Creditos.Abogado Creditos.Sdo_Proyectado ~
Creditos.For_Pago Creditos.Cuo_Pagadas Creditos.Val_Atraso ~
Creditos.Dias_Atraso Creditos.Per_Pago Creditos.Cuo_Atraso ~
Creditos.Provision Creditos.Fec_Reestructurado 
&Scoped-define DISPLAYED-TABLES Creditos
&Scoped-define FIRST-DISPLAYED-TABLE Creditos
&Scoped-Define DISPLAYED-OBJECTS W_AboCap Rs_Op W_PerPag W_TotExt ~
W_CuoFaltan W_NvoPlazo W_SdoOtros W_NvaCuota W_IntMora W_NvaTasa ~
W_CuoFalNva W_CedAbo W_FecIni Tg_ReInicio Rs_SistAmort W_CapVdo W_CuoPaga ~
W_IntVdo W_FecPP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Acepta_Terminos 
     LABEL "Aceptar Nuevos Términos" 
     SIZE 18.57 BY 1.23.

DEFINE BUTTON Btn_Extras 
     LABEL "E x t r a s" 
     SIZE 10.29 BY 1.08.

DEFINE BUTTON Btn_ModifVcto 
     LABEL "<----Modificar Valores Vencidos" 
     SIZE 22.29 BY 1.08 TOOLTIP "Click, Solo si desea Cargar/Rebajar Valores Vencidos".

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 176" 
     SIZE 10.57 BY 2.04.

DEFINE VARIABLE W_AboCap AS DECIMAL FORMAT "$>>>>,>>>,>>9":U INITIAL 0 
     LABEL "(Simula) Abono Extra a Capital :" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .69 TOOLTIP "Teclee el valor abono extra para Hallar nueva Cuota y/o nuevo plazo"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CapVdo AS DECIMAL FORMAT "->>>>,>>>,>>9":U INITIAL 0 
     LABEL "Cargo(+) O Reversa(-) Capital Vencido" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .77 TOOLTIP "Teclee el valor para afectar capital vencido, Si es reversa debe ser Negativo"
     BGCOLOR 15 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE W_CedAbo AS CHARACTER FORMAT "X(12)":U 
     LABEL "Céd" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE W_CuoFalNva AS DECIMAL FORMAT "9999":U INITIAL 0 
     LABEL "Nuevas Cuotas X Pagar" 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .81
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CuoFaltan AS DECIMAL FORMAT "9999":U INITIAL 0 
     LABEL "Cuotas X Pagar" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_CuoPaga AS INTEGER FORMAT ">,>>9":U INITIAL 0 
     LABEL "Cuotas Pagadas" 
     VIEW-AS FILL-IN 
     SIZE 6.57 BY .77 TOOLTIP "Nuevas Cuotas Pagadas"
     BGCOLOR 15 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE W_FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Nueva Fec-Inicio" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .85
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecPP AS DATE FORMAT "99/99/9999":U 
     LABEL "Prox.Pago" 
     VIEW-AS FILL-IN 
     SIZE 10.43 BY .77 TOOLTIP "Fecha Próximo Pago"
     BGCOLOR 15 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE W_IntMora AS DECIMAL FORMAT "->>>>>,>>>,>>>,>>9.99" INITIAL 0 
     LABEL "Interès Moratorio" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15 .

DEFINE VARIABLE W_IntVdo AS DECIMAL FORMAT "->>>>,>>>,>>9":U INITIAL 0 
     LABEL "Cargo(+) O Reversa(-) Interés Vencido" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .77 TOOLTIP "Teclee el valor para afectar Interés vencido, Si es reversa debe ser Negativo"
     BGCOLOR 15 FGCOLOR 7  NO-UNDO.

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
     SIZE 12.72 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_Op AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "ReCalcular Plazo", 1,
"ReCalcular Cuota", 2,
"ReFinanciar -ReInicio-", 3,
"Clàusula Aceleratoria", 4,
"Modificar Condiciones", 5
     SIZE 18.57 BY 4.73
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_SistAmort AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sistema Amortización Cuota Fija", 1,
"Cuota Unica", 2
     SIZE 41.14 BY .69 NO-UNDO.

DEFINE RECTANGLE RECT-296
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.72 BY 13.42.

DEFINE RECTANGLE RECT-320
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43.14 BY 5.88.

DEFINE RECTANGLE RECT-324
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21.57 BY .88.

DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39.72 BY .92.

DEFINE RECTANGLE RECT-326
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20.86 BY .96.

DEFINE RECTANGLE RECT-327
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39.72 BY 1.04.

DEFINE RECTANGLE RECT-328
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.57 BY .81.

DEFINE RECTANGLE RECT-330
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21.72 BY .62.

DEFINE RECTANGLE RECT-331
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85.14 BY 2.15.

DEFINE RECTANGLE RECT-332
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42.43 BY 1.31.

DEFINE VARIABLE Tg_ReInicio AS LOGICAL INITIAL no 
     LABEL "ReIniciar" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.43 BY .46
     BGCOLOR 15  NO-UNDO.

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
     W_AboCap AT ROW 1.12 COL 68.57 COLON-ALIGNED
     Rs_Op AT ROW 1.88 COL 28.29 NO-LABEL
     W_PerPag AT ROW 2.04 COL 81.72 COLON-ALIGNED
     Creditos.Fec_Desembolso AT ROW 2.08 COL 14 COLON-ALIGNED
          LABEL "Fecha Desembolso"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_TotExt AT ROW 2.27 COL 56.14 COLON-ALIGNED
     Creditos.Fec_Pago AT ROW 2.88 COL 14 COLON-ALIGNED
          LABEL "Fec.Próximo Pago"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_CuoFaltan AT ROW 2.92 COL 81.72 COLON-ALIGNED
     Creditos.Sdo_Capital AT ROW 3.27 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Fec_UltPago AT ROW 3.69 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NvoPlazo AT ROW 3.85 COL 81.72 COLON-ALIGNED
     Creditos.Plazo AT ROW 4.15 COL 58 COLON-ALIGNED
          LABEL "Plazo Actual"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_SdoOtros AT ROW 4.54 COL 14 COLON-ALIGNED
     W_NvaCuota AT ROW 4.88 COL 81.72 COLON-ALIGNED
     Creditos.Cuota AT ROW 5.08 COL 58 COLON-ALIGNED
          LABEL "Cuota Actual"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_IntMora AT ROW 5.31 COL 14 COLON-ALIGNED
     Creditos.Tasa AT ROW 5.96 COL 58 COLON-ALIGNED
          LABEL "T.Nominal Anual"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NvaTasa AT ROW 5.96 COL 81.86 COLON-ALIGNED
     Creditos.Int_DifCobro AT ROW 6.12 COL 14 COLON-ALIGNED
          LABEL "Interés Contingente"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Creditos.Int_Corrientes AT ROW 6.96 COL 14 COLON-ALIGNED
          LABEL "Interés Corriente"
          VIEW-AS FILL-IN 
          SIZE 11.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_CuoFalNva AT ROW 7.12 COL 87.86 COLON-ALIGNED
     Creditos.Int_Anticipado AT ROW 7.77 COL 14 COLON-ALIGNED
          LABEL "Interés-Anticipado"
          VIEW-AS FILL-IN 
          SIZE 11.14 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_CedAbo AT ROW 8.31 COL 52.29 COLON-ALIGNED
     Creditos.Abogado AT ROW 8.38 COL 29.29 HELP
          "" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Abogado", yes,
"No-Abogado", no
          SIZE 20.43 BY .54
          BGCOLOR 15 
     Creditos.Sdo_Proyectado AT ROW 8.65 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.For_Pago AT ROW 9.42 COL 29.57 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Caja", 1,
"Nómina", 2,
"Déb.Automático", 3
          SIZE 38.29 BY .5
          BGCOLOR 15 
     Creditos.Cuo_Pagadas AT ROW 9.46 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_FecIni AT ROW 10.15 COL 82.43 COLON-ALIGNED
     Creditos.Val_Atraso AT ROW 10.27 COL 14 COLON-ALIGNED
          LABEL "Capital_Vencido" FORMAT "->,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.43 BY 16.54
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Cre
     Tg_ReInicio AT ROW 10.38 COL 47
     Creditos.Dias_Atraso AT ROW 11.08 COL 14 COLON-ALIGNED
          LABEL "Dias_Vencidos" FORMAT "-99999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Per_Pago AT ROW 11.35 COL 29.29 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Semanal", 1,
"Decadal", 2,
"Qnal", 3,
"Mensual", 4
          SIZE 38.72 BY .58
          BGCOLOR 15 
     Btn_Acepta_Terminos AT ROW 11.46 COL 76.72
     Creditos.Cuo_Atraso AT ROW 11.88 COL 14 COLON-ALIGNED
          LABEL "Cuotas_Vencidas"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 12 FGCOLOR 15 
     Creditos.Provision AT ROW 12.69 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Extras AT ROW 13.12 COL 80.57
     Rs_SistAmort AT ROW 13.31 COL 29.43 NO-LABEL
     Creditos.Fec_Reestructurado AT ROW 13.46 COL 14 COLON-ALIGNED
          LABEL "Fec-Reestructurado"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Salir AT ROW 14.54 COL 87.72
     W_CapVdo AT ROW 14.73 COL 27 COLON-ALIGNED
     W_CuoPaga AT ROW 14.77 COL 53.72 COLON-ALIGNED
     Btn_ModifVcto AT ROW 15.12 COL 63.29
     W_IntVdo AT ROW 15.69 COL 27 COLON-ALIGNED
     W_FecPP AT ROW 15.69 COL 50 COLON-ALIGNED
     "Información del Crédito" VIEW-AS TEXT
          SIZE 19.57 BY .77 AT ROW 1.19 COL 4.43
          FGCOLOR 7 FONT 5
     "Periodicidad de Pago" VIEW-AS TEXT
          SIZE 15.14 BY .5 AT ROW 10.81 COL 32
          BGCOLOR 18 
     "Acción a Procesar" VIEW-AS TEXT
          SIZE 16.14 BY .58 AT ROW 1.23 COL 29.14
          FGCOLOR 7 FONT 5
     "Nuevas Condiciones" VIEW-AS TEXT
          SIZE 18.43 BY .5 AT ROW 7.08 COL 38.72
          BGCOLOR 12 FONT 5
     RECT-296 AT ROW 1 COL 1.29
     RECT-320 AT ROW 7 COL 28.14
     RECT-324 AT ROW 8.62 COL 28.86
     RECT-325 AT ROW 9.62 COL 28.86
     RECT-326 AT ROW 1.04 COL 3.72
     RECT-327 AT ROW 11.12 COL 28.72
     RECT-328 AT ROW 1.12 COL 28.86
     RECT-330 AT ROW 7.04 COL 37
     RECT-331 AT ROW 14.5 COL 1.29
     RECT-332 AT ROW 12.96 COL 28.72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.43 BY 16.54
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
         COLUMN             = 21.57
         ROW                = 5.92
         HEIGHT             = 16.54
         WIDTH              = 98.43
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
   NO-ENABLE EXP-HELP                                                   */
/* SETTINGS FOR BUTTON Btn_ModifVcto IN FRAME F_Cre
   NO-ENABLE                                                            */
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
/* SETTINGS FOR RADIO-SET Creditos.For_Pago IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Int_Anticipado IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_Corrientes IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Creditos.Int_DifCobro IN FRAME F_Cre
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET Creditos.Per_Pago IN FRAME F_Cre
   NO-ENABLE                                                            */
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
/* SETTINGS FOR TOGGLE-BOX Tg_ReInicio IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Creditos.Val_Atraso IN FRAME F_Cre
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN W_AboCap IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CuoFalNva IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_CuoFaltan IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_IntMora IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NvaCuota IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NvaTasa IN FRAME F_Cre
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NvoPlazo IN FRAME F_Cre
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
OPEN QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                                       AND  TExtras.Num_Solicitud EQ Creditos.Num_Credito
                        NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
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
  DEFI VAR NomOp    AS CHAR FORM "X(20)".
  DEFI VAR NomReIn  AS CHAR FORM "X(15)" INIT "No - ReIniciar".
  DEFI VAR W_CuoYaPag LIKE Creditos.Cuo_Pagadas.
  DEFI VAR W_RowIdCr  AS ROWID.
  DEFI VAR W_RowIdPP  AS ROWID.
  DEFI VAR W_TasaUs   LIKE Indicadores.Tasa.

  FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_Usura
                           AND Indicadores.Estado    EQ 1 
                           AND Indicadores.FecVcto   GE W_Fecha NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Indicadores) THEN DO:
     MESSAGE "Falta Indicador de Usura Vigente, No se permite la Modificaciòn."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  W_TasaUs = Indicadores.Tasa.
  RUN EFNV IN W_ManFin  (INPUT W_TasaUs / 100, 12, OUTPUT W_TasaUS).
  W_TasaUS = W_TasaUS * 1200.
  
  ASSIGN W_CuoFaltan
         W_CuoFalNva.

  IF DECIMAL(W_NvoPlazo:SCREEN-VALUE IN FRAME F_Cre) LE 0 OR
     DECIMAL(W_NvaCuota:SCREEN-VALUE IN FRAME F_Cre) LE 0 OR 
     W_NvaCuota:SCREEN-VALUE IN FRAME F_Cre EQ "?" THEN DO:
     MESSAGE "El nuevo plazo es cero o no se ha liquidado de nuevo" SKIP
             "no se aceptan los nuevos terminos" VIEW-AS ALERT-BOX.
     APPLY "Entry" TO W_NvoPlazo IN FRAME F_Cre.
     RETURN.
  END.

  IF Rs_Op EQ 5 AND W_NvaTasa NE Creditos.Tasa THEN
     MESSAGE "La Tasa es Diferente a la Actual del Credito..." SKIP
             "Luego de Salvar las Nuevas Condiciones," SKIP
             "Recuerde Revisar Cuota y Plazo, en el Plan de Pagos."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  IF Rs_Op EQ 1 THEN
     ASSIGN NomOp = "Rebajar Plazo".
  ELSE IF Rs_Op EQ 2 THEN
     ASSIGN NomOp = "Rebajar Cuota".
  ELSE IF Rs_Op EQ 3 THEN
     ASSIGN NomOp = "ReFinanciar -ReInicio-".
  ELSE IF Rs_Op EQ 4 THEN
     ASSIGN NomOp = "Clàusula Aceleratoria".
  ELSE 
     ASSIGN NomOp = "Modifición Condiciones".

  IF Tg_ReInicio OR Rs_Op EQ 3 THEN
     NomReIn = "Si - ReIniciar". 
 
  IF Tg_ReInicio THEN
     W_NvoPlazo = Creditos.Plazo - Creditos.Cuo_Pagadas.
/*  ELSE IF Rs_Op EQ 5 AND Creditos.Per_Pago EQ INTEG(Creditos.Per_Pago:SCREEN-VALUE) THEN
     ASSIGN W_NvaCuota  = Creditos.Cuota
            W_NvoPlazo  = Creditos.Plazo
            W_CuoFalNva = Creditos.Plazo - Creditos.Cuo_Pagadas.
  ELSE IF Rs_Op EQ 5 AND Creditos.Per_Pago NE INTEG(Creditos.Per_Pago:SCREEN-VALUE) THEN DO:
     IF      Creditos.Per_Pago EQ 4 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 3 THEN
        ASSIGN W_NvoPlazo = Creditos.Plazo * 2
               W_CuoYaPag = Creditos.Cuo_Pagadas * 2.
     ELSE IF Creditos.Per_Pago EQ 4 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 2 THEN
        ASSIGN W_NvoPlazo = Creditos.Plazo * 3
               W_CuoYaPag = Creditos.Cuo_Pagadas * 3.
     ELSE IF Creditos.Per_Pago EQ 4 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 1 THEN
        ASSIGN W_NvoPlazo = ROUND(Creditos.Plazo * 4.345,0)
               W_CuoYaPag = ROUND(Creditos.Cuo_Pagadas * 4.345,0).
     ELSE IF Creditos.Per_Pago EQ 3 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 4 THEN
        ASSIGN W_NvoPlazo = ROUND(Creditos.Plazo / 2,0)
               W_CuoYaPag = ROUND(Creditos.Cuo_Pagadas / 2,0).
     ELSE IF Creditos.Per_Pago EQ 3 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 2 THEN
        ASSIGN W_NvoPlazo = ROUND((Creditos.Plazo / 2) * 3,0)
               W_CuoYaPag = ROUND((Creditos.Cuo_Pagadas / 2) * 3,0).
     ELSE IF Creditos.Per_Pago EQ 3 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 1 THEN
        ASSIGN W_NvoPlazo = ROUND((Creditos.Plazo / 2) * 4.345,0)
               W_CuoYaPag = ROUND((Creditos.Cuo_Pagadas / 2) * 4.345,0).
     ELSE IF Creditos.Per_Pago EQ 2 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 4 THEN
        ASSIGN W_NvoPlazo = ROUND(Creditos.Plazo / 3,0)
               W_CuoYaPag = ROUND(Creditos.Cuo_Pagadas / 3,0).
     ELSE IF Creditos.Per_Pago EQ 2 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 3 THEN
        ASSIGN W_NvoPlazo = ROUND((Creditos.Plazo / 3) * 2,0)
               W_CuoYaPag = ROUND((Creditos.Cuo_Pagadas / 3) * 2,0).
     ELSE IF Creditos.Per_Pago EQ 2 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 1 THEN
        ASSIGN W_NvoPlazo = ROUND((Creditos.Plazo / 3) * 4.345,0)
               W_CuoYaPag = ROUND((Creditos.Cuo_Pagadas / 3) * 4.345,0).
     ELSE IF Creditos.Per_Pago EQ 1 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 4 THEN
        ASSIGN W_NvoPlazo = ROUND(Creditos.Plazo / 4.345,0)
               W_CuoYaPag = ROUND(Creditos.Cuo_Pagadas / 4.345,0).
     ELSE IF Creditos.Per_Pago EQ 1 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 3 THEN
        ASSIGN W_NvoPlazo = ROUND((Creditos.Plazo / 4.345) * 2,0)
               W_CuoYaPag = ROUND((Creditos.Cuo_Pagadas / 4.345) * 2,0).
     ELSE IF Creditos.Per_Pago EQ 1 AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 2 THEN
        ASSIGN W_NvoPlazo = ROUND((Creditos.Plazo / 4.345) * 3,0)
               W_CuoYaPag = ROUND((Creditos.Cuo_Pagadas / 4.345) * 3,0).
  END. */

  MESSAGE "El Crédito lo Liquidó con la siguiente OPCIÓN: " NomOp SKIP
          "                                                          " NomReIn SKIP
          "Plazo del Crédito : " Creditos.Plazo    "            Plazo Nuevo : " W_NvoPlazo   SKIP
          "Cuota del Crédito : " Creditos.Cuota    "            Cuota Nueva : " W_NvaCuota   SKIP
          "Cuotas X Pagar    : " W_CuoFaltan     "    Nuevas Cuotas X Pagar : "  W_CuoFalNva SKIP
          "Tasa del Crédito  : " Creditos.Tasa    "              Tasa Nueva : " W_NvaTasa    SKIP
          "En Abogado        : " Creditos.Abogado:SCREEN-VALUE  SKIP
          "                    Está Segura(o) de Salvar la Modificación...?"
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Refinanciación"
       UPDATE W_RptaConf AS LOGICAL.
  
  IF NOT W_RptaConf THEN
     RETURN.
       
  FIND CURRENT Creditos NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "El Crédito no puede ser actualizado, Està en Bloqueo..." SKIP
             "              Intente nuevamente."  VIEW-AS ALERT-BOX.
     APPLY "choose" TO Btn_Salir.
  END.
  
  FIND CURRENT PlanPagos NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "El PlanPagos del Crédito no puede ser actualizado, Està en Bloqueo..." SKIP
             "              Intente nuevamente."  VIEW-AS ALERT-BOX.
     APPLY "choose" TO Btn_Salir.
  END.

  ASSIGN W_RowIdPP = ROWID(PlanPagos).
  
  IF Rs_Op EQ 4 THEN
     RUN TotalMoroso.
  ELSE IF Rs_Op EQ 3 THEN
     RUN ReInicio.
  ELSE IF Rs_Op EQ 5 THEN DO:
     IF Tg_ReInicio THEN 
        RUN ReInicio.  
     ELSE IF Creditos.Per_Pago NE INTEG(Creditos.Per_Pago:SCREEN-VALUE) THEN DO:
        /*RUN Cambio_Plan. */
     END.

     ASSIGN Creditos.Per_Pago       = INTEG (Creditos.Per_Pago:SCREEN-VALUE)
            Creditos.Sistema        = Rs_SistAmort
            Creditos.For_Pago       = INTEG (Creditos.For_Pago:SCREEN-VALUE)
            Creditos.Abogado        = FALSE
            Creditos.Nit_Juzgado    = W_CedAbo                   
            Creditos.Tasa           = W_NvaTasa.
              
     IF Creditos.Abogado:SCREEN-VALUE EQ "Yes" THEN DO:
         Creditos.Abogado = TRUE.   
         IF Creditos.Intercobros THEN DO:
            FIND LAST Mov_intercobros WHERE Mov_Intercobros.Nit    EQ Creditos.Nit AND
                                            Mov_Intercobros.Pagare EQ Creditos.Pagare NO-ERROR.
            IF AVAILABLE(Mov_Intercobros) THEN
               ASSIGN Mov_Intercobros.Fec_TerPeriodo = W_Fecha
                      Mov_Intercobros.Fec_Desmarca   = W_Fecha.
            RELEASE Mov_Intercobros.
            Creditos.Intercobros = NO.
         END.
     END.

     /*IF Creditos.Cuota NE W_NvaCuota OR Creditos.Plazo NE W_NvoPlazo THEN */
     RUN Modif_PlanPag.

     RUN Mov_Credito.

     FIND LAST PlanPagos WHERE PlanPagos.Agencia   EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 1 NO-ERROR.
     IF AVAILABLE(PlanPagos) THEN DO:
        ASSIGN PlanPagos.Tasa  = W_NvaTasa 
               PlanPagos.Cuota = PlanPagos.Cuota - Creditos.Cuota + W_NvaCuota 
               PlanPagos.Plazo = W_NvoPlazo
               Creditos.Cuota  = W_NvaCuota 
               Creditos.Plazo  = W_NvoPlazo
               W_RowIdCr       = ROWID(Creditos)
               W_RowIdPP       = ROWID(PlanPagos).
        IF Creditos.Sistema NE 2 THEN
           RUN Halla_PagasFalt.R (INPUT W_RowIdCr,W_RowIdPP,W_TasaUs). 
     END.
     ELSE ASSIGN Creditos.Cuota = W_NvaCuota 
                 Creditos.Plazo = W_NvoPlazo. 
  END.
  ELSE 
     RUN Refinanc.

     
  FIND CURRENT Creditos  NO-LOCK NO-ERROR.
  FIND CURRENT PlanPagos NO-LOCK NO-ERROR.

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
  DEFI VAR JJ        AS INTEG FORM "99999".
  DEFI VAR W_FecTra  AS DATE.
  DEFI VAR W_FecIni  AS DATE.

  W_NroDias = 30.
  IF Creditos.Per_Pago EQ 1 THEN
     W_NroDias = 7.
  ELSE IF Creditos.Per_Pago EQ 2 THEN
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
  FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                     AND TExtras.Num_Solicitud EQ Creditos.Num_Credito:
      IF TExtras.Estado EQ 1 AND TExtras.Nro_Cuota NE W_PPExtra THEN
         W_TotExt = W_TotExt + TExtras.Vr_CuoExtra.
      ELSE IF TExtras.Estado NE 1 THEN
         DELETE TExtras.
  END.

  IF W_TotExt GE Creditos.Sdo_Capital THEN DO:
     MESSAGE "El Valor Total de las Cuotas Extras + El valor de Esta Extra" SKIP
             "Debe ser Menor al Sdo-Capital...No se Acepta la Operaciòn."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  FIND FIRST TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                      AND  TExtras.Num_Solicitud EQ Creditos.Num_Credito
                      AND  TExtras.Nro_Cuota     EQ W_PPExtra NO-ERROR.
  IF NOT AVAIL(TExtras) THEN
     CREATE TExtras.

  ASSIGN W_FecIni = Creditos.Fec_Desem.

  IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desem THEN
     W_FecIni = Creditos.Fec_PagAnti.

  ASSIGN TExtras.Agencia       = Creditos.Agencia
         TExtras.Cod_Credito   = Creditos.Cod_Credito
         TExtras.Nit           = Creditos.Nit
         TExtras.Num_Solicitud = Creditos.Num_Credito
         TExtras.Nro_Cuota     = W_PPExtra
         TExtras.Vr_CuoExtra   = W_VrExtra
         TExtras.Estado        = 1
         W_TotExt              = 0
         W_FecTra              = W_FecIni.

  DO JJ = 1 TO W_PPExtra:
     RUN Halla_FecVcto.R (INPUT  W_FecIni,W_NroDias,W_FecTra,   
                          OUTPUT W_FecTra).
     TExtras.Fec_Vcto = W_FecTra.
  END.       

  FOR EACH Extras WHERE TExtras.Nit           EQ Creditos.Nit
                    AND TExtras.Num_Solicitud EQ Creditos.Num_Credito NO-LOCK:
      IF TExtras.Estado EQ 1 THEN DO:
         ASSIGN W_TotExt = W_TotExt + TExtras.Vr_CuoExtra
                W_TotExt:SCREEN-VALUE IN FRAME F_Cre = STRING(W_TotExt).
      
         FIND LAST PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    LE 2
                        AND PlanPagos.Nro_Cuota    EQ TExtras.Nro_Cuota NO-ERROR.
         ASSIGN PlanPagos.Cuota = Creditos.Cuota + TExtras.Vr_CuoExtra WHEN AVAIL(PlanPagos).
      END.
      ELSE DO:
         FIND LAST PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    LE 2
                        AND PlanPagos.Nro_Cuota    EQ TExtras.Nro_Cuota NO-ERROR.
         ASSIGN PlanPagos.Cuota = Creditos.Cuota WHEN AVAIL(PlanPagos).
      END.
  END.  

  CLOSE QUERY Br_Extras.
  OPEN  QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                                         AND   TExtras.Num_Solicitud EQ Creditos.Num_Credito NO-LOCK 
                     BY TExtras.Nro_Cuota INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_EliExt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_EliExt Wwin
ON CHOOSE OF Btn_EliExt IN FRAME F_Extras /* Eliminar Extra */
DO:
   FIND FIRST TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                      AND   TExtras.Num_Solicitud EQ Creditos.Num_Credito
                      AND   TExtras.Nro_Cuota     EQ W_PPExtra NO-ERROR.
   IF AVAIL(TExtras) THEN DO:
      DELETE TExtras.
      FIND LAST PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    LE 2
                        AND PlanPagos.Nro_Cuota    EQ W_PPExtra NO-ERROR.
      ASSIGN PlanPagos.Cuota = Creditos.Cuota WHEN AVAIL(PlanPagos).
   END.

   ASSIGN W_TotExt = 0.

   FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                    AND   TExtras.Num_Solicitud EQ Creditos.Num_Credito NO-LOCK:
      IF TExtras.Estado EQ 1 THEN
         ASSIGN W_TotExt = W_TotExt + TExtras.Vr_CuoExtra
                W_TotExt:SCREEN-VALUE IN FRAME F_Cre = STRING(W_TotExt).
   END.  

   CLOSE QUERY Br_Extras.
   OPEN  QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                                          AND   TExtras.Num_Solicitud EQ Creditos.Num_Credito
               NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cre
&Scoped-define SELF-NAME Btn_Extras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Extras Wwin
ON CHOOSE OF Btn_Extras IN FRAME F_Cre /* E x t r a s */
DO:
   ASSIGN FRAME F_Cre:SENSITIVE  = FALSE
          FRAME F_Extras:VISIBLE = TRUE.


   CLOSE QUERY Br_Extras.
   OPEN  QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                                          AND   TExtras.Num_Solicitud EQ Creditos.Num_Credito
                        NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ModifVcto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ModifVcto Wwin
ON CHOOSE OF Btn_ModifVcto IN FRAME F_Cre /* <----Modificar Valores Vencidos */
DO:
  IF (W_CapVdo EQ 0 AND W_IntVdo EQ 0)
  OR W_FecPP         EQ ? 
  OR STRING(W_FecPP) EQ "  /  /    " THEN DO:
     MESSAGE "No existen valores para modificar los vencimientos del Credito." SKIP
             "O falta la Fecha de Proximo Pago."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  MESSAGE "Valor afectar Capital Vencido $" W_CapVdo SKIP
          "Valor afectar Interes Vencido $" W_IntVdo SKIP
          "Nuevas Cuotas Pagadas         #" W_CuoPaga SKIP
          "Nueva Fecha de Proximo-pago   F" W_FecPP
          "                    Està Segura(o) de Salvar la Modificaciòn...?"
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Vlrs.Vencidos"
       UPDATE W_RptaVdo AS LOGICAL.
  
  IF NOT W_RptaVdo THEN
     RETURN.

  FIND CURRENT Creditos NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "El Crèdito no puede ser actualizado, Està en Bloqueo..." SKIP
             "              Intente nuevamente."  VIEW-AS ALERT-BOX.
     APPLY "choose" TO Btn_Salir.
  END.
  
  FIND CURRENT PlanPagos NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
     MESSAGE "El PlanPagos del Crèdito no puede ser actualizado, Està en Bloqueo..." SKIP
             "              Intente nuevamente."  VIEW-AS ALERT-BOX.
     APPLY "choose" TO Btn_Salir.
  END.                                                                        

  ASSIGN Creditos.Capital_Acum  = Creditos.Capital_Acum  + W_CapVdo
         Creditos.Sdo_Proyec    = Creditos.Sdo_Proyec    - W_CapVdo
         Creditos.Int_LiqAcum   = Creditos.Int_LiqAcum   + W_IntVdo
         PlanPagos.Capital_Acum = PlanPagos.Capital_Acum + W_CapVdo
         PlanPagos.Int_LiqAcum  = PlanPagos.Int_LiqAcum  + W_IntVdo
         Creditos.Val_Atraso    = 0
         Creditos.Dias_Atraso   = 0
         Creditos.Cuo_Atraso    = 0
         Creditos.Cuo_Pagadas   = W_CuoPaga
         PlanPagos.Cuo_Pagas    = W_CuoPaga
         Creditos.Fec_Pago      = W_FecPP
         PlanPagos.Fec_ProxPago = W_FecPP.

  ASSIGN Creditos.Val_Atraso = Creditos.Capital_Acum - Creditos.Sdo_CapPag WHEN
                               Creditos.Capital_Acum GT Creditos.Sdo_CapPag. 

  IF Creditos.Val_Atraso GT Creditos.Sdo_Capital THEN
     Creditos.Val_Atraso = Creditos.Sdo_Capital.

  IF Creditos.Val_Atraso GT 0 AND Creditos.Val_Atraso LE Creditos.Cuota THEN
     Creditos.Cuo_Atraso = 1.

  ASSIGN Creditos.Dias_Atraso = W_Fecha - W_FecPP WHEN
                                W_Fecha GT W_FecPP.

  ASSIGN Creditos.Cuo_Atraso  = PlanPagos.Nro_Cuota - W_CuoPaga - 1 WHEN
                                PlanPagos.Nro_Cuota GT W_CuoPaga.


  MESSAGE "La Modificacion de valores Vencidos Finalizo OK." SKIP
          "Por favor revise las variables del Credito y PlanPagos luego de la Modificacion."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  FIND CURRENT Creditos  NO-LOCK NO-ERROR.
  FIND CURRENT PlanPagos NO-LOCK NO-ERROR.

  RUN Imprimir.  

  ASSIGN W_CapVdo              = 0
         W_CapVdo:SCREEN-VALUE = "0"
         W_IntVdo              = 0  
         W_IntVdo:SCREEN-VALUE = "0".
            
  APPLY "choose" TO Btn_Salir.
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

  FIND CURRENT planpagos NO-LOCK NO-ERROR.
  ASSIGN FRAME F_Extras:VISIBLE = FALSE
         FRAME F_Cre:SENSITIVE  = TRUE.

 /* APPLY "Choose" TO Btn_Salvar IN FRAME F_Creditos.*/
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
  /* APPLY "Value-Changed" TO Creditos.Per_Pago.   */
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
  W_SiPdo = FALSE.

  IF INTEG(Creditos.For_Pago:SCREEN-VALUE) EQ 2 THEN DO:
     FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR. 
                                                                                              
     IF AVAIL(Empresas) THEN                                                             
        Creditos.Per_Pago:SCREEN-VALUE = STRING(Empresas.FOR_Pago).                         
     ELSE 
        ASSIGN Creditos.Per_Pago:SCREEN-VALUE = "4".
  END.                                                                                       
  ELSE                                                                                       
     ASSIGN Creditos.Per_Pago:SCREEN-VALUE = "4". 

  IF Creditos.Per_Pago NE INTEG(Creditos.Per_Pago:SCREEN-VALUE) THEN DO:  
     W_SiPdo = TRUE.

     IF NOT Tg_ReInicio THEN DO:                                                           
        MESSAGE "Se Está Modificando la Periodicidad de Pago del Crédito...?" SKIP      
                "El Crédito sera Liquidado Como ReIniciado...."  SKIP
                "              Si esta Segura(o) de no REInicio, Debe Desmarcar ReIniciar."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.  

        ASSIGN Tg_ReInicio              = TRUE
               Tg_ReInicio:SCREEN-VALUE = "Yes".
     END.

     APPLY "Value-Changed" TO Tg_ReInicio.                                              
  END.
  ELSE DO:
     ASSIGN W_NvaTasa:SCREEN-VALUE   = STRING(Creditos.Tasa)
            Tg_ReInicio              = FALSE
            Tg_ReInicio:SCREEN-VALUE = "No".
     APPLY "Value-Changed" TO Tg_ReInicio.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rs_Op
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Op Wwin
ON MOUSE-SELECT-CLICK OF Rs_Op IN FRAME F_Cre
DO:
  ASSIGN Rs_Op
         W_NvoPlazo:SENSITIVE        = FALSE
         Creditos.Abogado:SENSITIVE  = FALSE
         Creditos.Per_Pago:SENSITIVE = FALSE
         Creditos.FOR_Pago:SENSITIVE = FALSE
         Tg_ReInicio:SENSITIVE       = FALSE
         W_NvaTasa:SENSITIVE         = FALSE.

  ASSIGN Creditos.Per_Pago:SCREEN-VALUE = STRING(Creditos.Per_Pago)
         Creditos.For_Pago:SCREEN-VALUE = STRING(Creditos.For_Pago)
         Creditos.Abogado:SCREEN-VALUE  = "No"
         W_NvaCuota:SCREEN-VALUE = STRING(Creditos.Cuota)
         W_NvoPlazo:SCREEN-VALUE = STRING(Creditos.Plazo)
         W_NvaTasa:SCREEN-VALUE  = STRING(Creditos.Tasa)
         W_NvaCuota
         W_NvoPlazo
         W_NvaTasa.

  IF P_Progra EQ 9 AND Rs_Op GT 3 THEN DO:
     ASSIGN Rs_Op = 1
            Rs_Op:SCREEN-VALUE = "1".
     APPLY "Mouse-Select-Click" TO SELF.
  END.

  IF Creditos.Abogado THEN
     Creditos.Abogado:SCREEN-VALUE = "Yes".
         
  IF Rs_Op EQ 3 THEN
     ASSIGN W_NvoPlazo:SENSITIVE = TRUE.
  ELSE IF Rs_Op EQ 5 AND P_Progra NE 9 THEN
     ASSIGN W_NvoPlazo:SENSITIVE        = TRUE
            W_NvaCuota:SENSITIVE        = TRUE
            Creditos.Abogado:SENSITIVE  = TRUE
            Creditos.Per_Pago:SENSITIVE = TRUE
            Creditos.FOR_Pago:SENSITIVE = TRUE
            Tg_ReInicio:SENSITIVE       = TRUE
            W_NvaTasa:SENSITIVE         = TRUE.
  ELSE IF Rs_Op LE 3 THEN
     RUN Liquidar.
  ELSE
     W_CuoFalNva:SCREEN-VALUE = "1".
  
  IF Rs_Op EQ 3 THEN
     APPLY "ENTRY" TO W_NvoPlazo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Op Wwin
ON VALUE-CHANGED OF Rs_Op IN FRAME F_Cre
DO:
  APPLY "Mouse-Select-Click" TO SELF.
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


&Scoped-define SELF-NAME Tg_ReInicio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_ReInicio Wwin
ON MOUSE-SELECT-CLICK OF Tg_ReInicio IN FRAME F_Cre /* ReIniciar */
DO:
  APPLY "Value-Changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_ReInicio Wwin
ON VALUE-CHANGED OF Tg_ReInicio IN FRAME F_Cre /* ReIniciar */
DO:
  DEFINE VARIABLE TasaW    LIKE Solicitud.Tasa.  

  ASSIGN Tg_ReInicio.

  IF /*Tg_ReInicio AND*/ Creditos.Per_Pago NE INTEG(Creditos.Per_Pago:SCREEN-VALUE) THEN DO:
     TasaW = Creditos.Tasa.
            
     RUN NVEF IN W_ManFin (INPUT TasaW / (W_PdoAno * 100),INPUT W_PdoAno,OUTPUT TasaW).       
            
     ASSIGN TasaW    = TasaW * 100
            W_NvoPdo = 12.

     IF      Creditos.Per_Pago:SCREEN-VALUE EQ "1" THEN 
        ASSIGN W_NvoPdo = 52.
     ELSE IF Creditos.Per_Pago:SCREEN-VALUE EQ "2" THEN 
        ASSIGN W_NvoPdo = 36.
     ELSE IF Creditos.Per_Pago:SCREEN-VALUE EQ "3" THEN 
        ASSIGN W_NvoPdo = 24.

     RUN EFNV IN W_ManFin  (INPUT (TasaW / 100), W_NvoPdo, OUTPUT TasaW).

     ASSIGN W_NvaTasa              = (TasaW * 100 * W_NvoPdo)
            W_NvaTasa:SCREEN-VALUE = STRING(W_NvaTasa).

     MESSAGE "La Tasa del Crédito es Modificada de acuerdo con el Nuevo Período de Pago." SKIP
             "Y la Cuota También  es Modificada...Revíselas por Favor."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

     IF       Creditos.Per_Pago EQ 3
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 4 THEN
         ASSIGN W_NvaCuota = Creditos.Cuota * 2.
     ELSE IF  Creditos.Per_Pago EQ 3
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 1 THEN
         ASSIGN W_NvaCuota = (Creditos.Cuota * 2) / 4.
     ELSE IF  Creditos.Per_Pago EQ 3
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 2 THEN
         ASSIGN W_NvaCuota = (Creditos.Cuota * 2) / 3.
     ELSE IF  Creditos.Per_Pago EQ 4
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 3 THEN
         ASSIGN W_NvaCuota = Creditos.Cuota / 2.
     ELSE IF  Creditos.Per_Pago EQ 4
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 2 THEN
         ASSIGN W_NvaCuota = Creditos.Cuota / 3.
     ELSE IF  Creditos.Per_Pago EQ 4
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 1 THEN
         ASSIGN W_NvaCuota = Creditos.Cuota / 4.     
     ELSE IF  Creditos.Per_Pago EQ 1
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 4 THEN
         ASSIGN W_NvaCuota = (Creditos.Cuota * 4).
     ELSE IF  Creditos.Per_Pago EQ 1
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 3 THEN
         ASSIGN W_NvaCuota = (Creditos.Cuota * 4) / 2.
     ELSE IF  Creditos.Per_Pago EQ 1
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 2 THEN
         ASSIGN W_NvaCuota = (Creditos.Cuota * 4) / 3.
     ELSE IF  Creditos.Per_Pago EQ 2
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 4 THEN
         ASSIGN W_NvaCuota = (Creditos.Cuota * 3).
     ELSE IF  Creditos.Per_Pago EQ 2
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 3 THEN
         ASSIGN W_NvaCuota = (Creditos.Cuota * 3) / 2.
     ELSE IF  Creditos.Per_Pago EQ 2
     AND INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 1 THEN
         ASSIGN W_NvaCuota = (Creditos.Cuota * 3) / 4.

     W_NvaCuota:SCREEN-VALUE = STRING (W_NvaCuota).

     RUN liquidar.     
  END.
  ELSE 
     ASSIGN W_NvaCuota:SCREEN-VALUE  = STRING(Creditos.Cuota)
            W_NvoPlazo:SCREEN-VALUE  = STRING(Creditos.Plazo - Creditos.Cuo_Pagadas)
            W_CuoFalNva:SCREEN-VALUE = STRING(Creditos.Plazo - Creditos.Cuo_Pagadas)
            W_NvaTasa:SCREEN-VALUE   = STRING(Creditos.Tasa)
            W_NvaCuota
            W_NvoPlazo
            W_NvaTasa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_AboCap
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_AboCap Wwin
ON LEAVE OF W_AboCap IN FRAME F_Cre /* (Simula) Abono Extra a Capital : */
DO:
   ASSIGN W_AboCap.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CapVdo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CapVdo Wwin
ON LEAVE OF W_CapVdo IN FRAME F_Cre /* Cargo(+) O Reversa(-) Capital Vencido */
DO:
   ASSIGN W_CapVdo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CedAbo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CedAbo Wwin
ON LEAVE OF W_CedAbo IN FRAME F_Cre /* Céd */
DO:
  ASSIGN W_CedAbo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CuoPaga
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CuoPaga Wwin
ON LEAVE OF W_CuoPaga IN FRAME F_Cre /* Cuotas Pagadas */
DO:
   ASSIGN W_CuoPaga.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecIni Wwin
ON LEAVE OF W_FecIni IN FRAME F_Cre /* Nueva Fec-Inicio */
DO:
  DEFI VAR W_NroDias AS INTEG FORM "9999".
  DEFI VAR FecCont   AS DATE.
  DEFI VAR FecIni    AS DATE.
  DEFI VAR W_RowIdCr AS ROWID.
  DEFI VAR W_RowIdPP AS ROWID.
  DEFI VAR W_TasaUs  LIKE Indicadores.Tasa.
  DEFI VAR W_SiId1   AS LOG INIT FALSE.

  ASSIGN W_FecIni.

  IF W_FecIni LT Creditos.Fec_Desemb THEN
     MESSAGE "Revise por Favor, La Nueva Fec-Inic es Anterior a La de Desembolso..." SKIP
             "                  Será Modificada la de Desembolso..." SKIP
             "En el Siguiente Mensaje puede Tomar la Decisión de Continuar o NO."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

  MESSAGE "Está Segura(o) de Modificar la Fecha de Inicio del PlanPagos del Crédito seleccionado...?" SKIP
          "                    Por Nueva Fecha : " W_FecIni
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Fec-Inicio"
       UPDATE W_RptaFI AS LOGICAL.
  
  IF NOT W_RptaFI THEN
     RETURN.

  FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_Usura
                           AND Indicadores.Estado    EQ 1 
                           AND Indicadores.FecVcto   GE W_Fecha NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Indicadores) THEN DO:
     MESSAGE "Falta Indicador de Usura Vigente, No se permite la Modificaciòn."
         VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  W_TasaUs = Indicadores.Tasa.
  RUN EFNV IN W_ManFin  (INPUT W_TasaUs / 100, 12, OUTPUT W_TasaUS).
  W_TasaUS = W_TasaUS * 1200.

  FIND CURRENT Creditos NO-ERROR.

  IF W_FecIni LT Creditos.Fec_Desemb THEN
     Creditos.Fec_Desemb = W_FecIni.

  ASSIGN Creditos.Fec_PagAnti = W_FecIni
         W_NroDias            = 30.

  IF      Creditos.Per_Pago EQ 1 THEN                                               
         W_NroDias = 7.                                                                
  ELSE IF Creditos.Per_Pago EQ 2 THEN                                                  
         W_NroDias = 10.                                                               
  ELSE IF Creditos.Per_Pago EQ 3 THEN                                                  
         W_NroDias = 15.                                                               
                                                                                    
  ASSIGN FecCont = Creditos.Fec_Desemb                                         
         FecIni  = Creditos.Fec_Desemb
         W_SiId1 = FALSE.                                                                                 
                                                                                    
  IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GE Creditos.Fec_Desemb THEN    
     ASSIGN FecCont = Creditos.Fec_PagAnti                                             
            FecIni  = Creditos.Fec_PagAnti.   
                                                                                       
  FOR EACH PlanPagos WHERE PlanPagos.Agencia       EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Nro_Cuota    GT 0 BY PlanPagos.Nro_Cuota:
      ASSIGN PlanPagos.Fec_Ini = FecCont.
      RUN Halla_FecVcto.R (INPUT  FecIni,W_NroDias,FecCont,                 
                           OUTPUT FecCont).
      ASSIGN PlanPagos.Fec_Vcto = FecCont.             

      IF FecCont LT W_Fecha THEN
         ASSIGN PlanPagos.Id_PdoMes = 2.
      ELSE IF FecCont GT W_Fecha THEN
         ASSIGN PlanPagos.Id_PdoMes = 0.

      IF FecCont GE W_Fecha AND NOT W_SiId1 THEN
         ASSIGN W_SiId1             = TRUE
                PlanPagos.Id_PdoMes = 1.
  END.

  FIND LAST PlanPagos WHERE PlanPagos.Agencia      EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 1 NO-ERROR.
  IF NOT AVAIL(PlanPagos) THEN DO:
     MESSAGE "No se Halló el Id_PdoMes = 1(Pdo.Transcurre) en el PlanPagos,Revise por favor." 
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN.
  END.

  ASSIGN W_RowIdCr = ROWID(Creditos)
         W_RowIdPP = ROWID(PlanPagos).
  
  RUN Mov_Credito.

  RUN Halla_PagasFalt.R (INPUT W_RowIdCr,W_RowIdPP,W_TasaUs).
  FIND CURRENT Creditos NO-LOCK NO-ERROR.
  FIND CURRENT PlanPagos NO-LOCK NO-ERROR.
  RUN Imprimir.  
            
  APPLY "choose" TO Btn_Salir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecPP Wwin
ON LEAVE OF W_FecPP IN FRAME F_Cre /* Prox.Pago */
DO:
   ASSIGN W_FecPP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_IntVdo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_IntVdo Wwin
ON LEAVE OF W_IntVdo IN FRAME F_Cre /* Cargo(+) O Reversa(-) Interés Vencido */
DO:
   ASSIGN W_IntVdo.
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
  
  IF Rs_Op EQ 3 THEN 
     ASSIGN W_NvaCuota:SCREEN-VALUE = STRING(Creditos.Cuota)
            W_NvaCuota.

  IF Rs_Op NE 5 THEN  
     RUN Liquidar.
  
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
  DEFI VAR W_ContCuota  LIKE PlanPagos.Nro_Cuota INIT 0.
  DEFI VAR I            AS   INTEG FORM "9999".
  
  FOR EACH TPlanP. DELETE TPlanP. END.

  ASSIGN W_FecControl    = PlanPagos.Fec_Vcto
         PlanPagos.Cuota = W_NvaCuota
         W_PdoPag        = 30
         W_QnaDec        = 0
         W_ContCuota     = PlanPagos.Nro_Cuota.

  IF INTEG(Creditos.Per_Pago:SCREEN-VALUE IN FRAME F_Cre) EQ 1 THEN  
     W_PdoPag = 7.                                                      
  ELSE IF INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 2 THEN               
     W_PdoPag = 10.                                                     
  ELSE IF INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 3 THEN               
     W_PdoPag = 15.                                                     

  FOR EACH PlanPagos WHERE PlanPagos.Agencia      EQ Creditos.Agencia    
                       AND PlanPagos.Nit          EQ Creditos.Nit        
                       AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                       AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                       AND PlanPagos.Id_PdoMes    EQ 0 BY PlanPagos.Nro_Cuota: /*Solo Futuros*/
      IF PlanPagos.Nro_Cuota GT W_NvoPlazo THEN
         DELETE PlanPagos.       /*(Futuros) mayor que el nuevo plazo se eliminan*/
      ELSE DO:
         ASSIGN PlanPagos.Cuota    = W_NvaCuota
                PlanPagos.Tasa     = W_NvaTasa
                PlanPagos.Fec_Inic = W_FecControl 
                PlanPagos.Fec_Vcto = W_FecControl + W_PdoPag 
                W_FecControl       = W_FecControl + W_PdoPag
                W_ContCuota        = PlanPagos.Nro_Cuota.
                
         IF W_PdoPag NE 7 THEN 
            RUN Cambio_Pdo.     /*Al final de Este mismo procedimiento*/

         CREATE TPlanP.
         BUFFER-COPY PlanPagos TO TPlanP.
      END.
  END.

  IF NOT AVAIL(TPlanP) THEN DO:
     FIND PlanPagos WHERE ROWID(PlanPagos) EQ W_RowIdPP NO-ERROR.   
     CREATE TPlanP.
     BUFFER-COPY PlanPagos TO TPlanP.
  END.

  IF W_ContCuota LT W_NvoPlazo THEN DO I = W_ContCuota + 1 TO W_NvoPlazo:
     CREATE PlanPagos.
     BUFFER-COPY TPlanP TO PlanPagos.
     ASSIGN PlanPagos.Plazo        = W_NvoPlazo
            PlanPagos.Cuota        = W_NvaCuota
            PlanPagos.Monto_Actual = Creditos.Monto
            PlanPagos.Tasa         = W_NvaTasa
            PlanPagos.Cuo_Pagas    = 0
            PlanPagos.Id_PdoMes    = 0
            PlanPagos.Nro_Cuota    = I
            PlanPagos.Fec_Inic     = W_FecControl            
            PlanPagos.Fec_Vcto     = W_FecControl + W_PdoPag 
            W_FecControl           = W_FecControl + W_PdoPag.

     ASSIGN PlanPagos.Cargos_Pdo        = 0
            PlanPagos.Cargos_Acum       = 0
            PlanPagos.Pagos_OtrosPdo    = 0
            PlanPagos.Pagos_OtrosAcum   = 0
            PlanPagos.Int_MoraPdo       = 0
            PlanPagos.Int_MoraAcum      = 0
            PlanPagos.Pagos_MoraPdo     = 0
            PlanPagos.Pagos_MoraAcum    = 0
            PlanPagos.Int_LiqPdo        = 0
            PlanPagos.Int_LiqAcum       = 0
            PlanPagos.Pagos_IntPdo      = 0
            PlanPagos.Pagos_IntAcum     = 0
            PlanPagos.Capital_Pdo       = 0
            PlanPagos.Capital_Acum      = 0
            PlanPagos.Pagos_CapitalPdo  = 0
            PlanPagos.Pagos_CapitalAcum = 0. 

     IF W_PdoPag NE 7 THEN 
        RUN Cambio_Pdo.     /*Al final de Este mismo procedimiento*/   
  END.
END PROCEDURE.

PROCEDURE Cambio_Pdo:
   IF W_PdoPag EQ 30 THEN 
      RUN Mes_Cont.   
   ELSE DO:                                            
      W_QnaDec = W_QnaDec + 1.                              
                                                       
      IF W_PdoPag EQ 15 AND W_QnaDec EQ 2 THEN DO:          
         RUN Mes_Cont.                                      
         W_QnaDec = 0.                                      
      END.                                                  
      ELSE IF W_PdoPag EQ 10 AND W_QnaDec EQ 3 THEN DO:     
         RUN Mes_Cont.                                      
         W_QnaDec = 0.                                      
      END.                                                  
   END.                                                     

END PROCE.

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
  DISPLAY W_AboCap Rs_Op W_PerPag W_TotExt W_CuoFaltan W_NvoPlazo W_SdoOtros 
          W_NvaCuota W_IntMora W_NvaTasa W_CuoFalNva W_CedAbo W_FecIni 
          Tg_ReInicio Rs_SistAmort W_CapVdo W_CuoPaga W_IntVdo W_FecPP 
      WITH FRAME F_Cre IN WINDOW Wwin.
  IF AVAILABLE Creditos THEN 
    DISPLAY Creditos.Fec_Desembolso Creditos.Fec_Pago Creditos.Sdo_Capital 
          Creditos.Fec_UltPago Creditos.Plazo Creditos.Cuota Creditos.Tasa 
          Creditos.Int_DifCobro Creditos.Int_Corrientes Creditos.Int_Anticipado 
          Creditos.Abogado Creditos.Sdo_Proyectado Creditos.For_Pago 
          Creditos.Cuo_Pagadas Creditos.Val_Atraso Creditos.Dias_Atraso 
          Creditos.Per_Pago Creditos.Cuo_Atraso Creditos.Provision 
          Creditos.Fec_Reestructurado 
      WITH FRAME F_Cre IN WINDOW Wwin.
  ENABLE RECT-296 RECT-320 RECT-324 RECT-325 RECT-326 RECT-327 RECT-328 
         RECT-330 RECT-331 RECT-332 Rs_Op W_CedAbo W_FecIni Btn_Acepta_Terminos 
         Btn_Extras Rs_SistAmort Btn_Salir W_CapVdo W_CuoPaga W_IntVdo W_FecPP 
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
            Btn_Acepta_Terminos:VISIBLE IN FRAME F_Cre = FALSE
            Btn_ModifVcto:VISIBLE                      = FALSE
            W_AboCap:SENSITIVE                         = TRUE
            W_CapVdo:SENSITIVE                         = FALSE
            W_IntVdo:SENSITIVE                         = FALSE
            W_FecPP:SENSITIVE                          = FALSE
            W_CuoPaga:SENSITIVE                        = FALSE.

  /*IF W_Agencia NE 10 THEN DO:
     Rs_Op:DISABLE("ReFinanciar -ReInicio-").
     Rs_Op:DISABLE("Clàusula Aceleratoria").
     Rs_Op:DISABLE("Modificar Condiciones").
     ASSIGN Btn_ModifVcto:VISIBLE = FALSE
            W_CapVdo:SENSITIVE    = FALSE
            W_IntVdo:SENSITIVE    = FALSE
            W_FecPP:SENSITIVE     = FALSE
            W_CuoPaga:SENSITIVE   = FALSE.
  END.*/

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

  
  IF Rs_Op EQ 4 OR Creditos.Sistema NE 1 THEN
     RETURN.
    
  DO WITH FRAME F_Cre:
     ASSIGN TotPtW   = Creditos.Sdo_Capital
            CuotaW   = Creditos.Cuota
            W_CuoPla = 3
            TasaW    = Creditos.Tasa
            NPdo     = Creditos.Per_Pago.

     IF Rs_Op EQ 5 THEN DO:
        ASSIGN TasaW    = W_NvaTasa
               CuotaW   = W_NvaCuota
               PlazoW   = 0
               W_CuoPla = 2
               NPdo     = INTEG(Creditos.Per_Pago:SCREEN-VALUE).
                           
        RUN NVEF IN W_ManFin (INPUT TasaW / (W_NvoPdo * 100),INPUT W_NvoPdo,OUTPUT TasaW).       
     END.
     ELSE 
        RUN NVEF IN W_ManFin (INPUT TasaW / (W_PdoAno * 100),INPUT W_PdoAno,OUTPUT TasaW).
            
     TasaW = TasaW * 100.        
     
     IF Rs_Op EQ 1 THEN 
        ASSIGN PlazoW   = 0
               W_CuoPla = 2.
     ELSE IF Rs_Op NE 5 THEN                                                   
        ASSIGN CuotaW = 0
               PlazoW = W_NvoPlazo.

     IF Rs_Op EQ 2 THEN 
        PlazoW = Creditos.Plazo - Creditos.Cuo_Pagadas.   

     IF P_Progra EQ 9 THEN
        TotPtW = TotPtW - W_AboCap.

     RUN Calculo_Cuota.R (INPUT-OUTPUT TotPtw, INPUT-OUTPUT PlazoW, INPUT-OUTPUT CuotaW,            
                     INPUT-OUTPUT TInteW, INPUT-OUTPUT TasaW, INPUT 0,                              
                     INPUT 0,INPUT NPdo, INPUT W_CuoPla,                                      
                     INPUT 1,                                                    
                     INPUT Creditos.Sistema).   

     ASSIGN W_NvaCuota:SCREEN-VALUE = STRING(CuotaW)
            W_NvoPlazo:SCREEN-VALUE = STRING(PlazoW)
            W_NvaCuota
            W_NvoPlazo.
            
     IF Rs_Op EQ 1 THEN
        ASSIGN W_NvoPlazo:SCREEN-VALUE  = STRING(PlazoW + Creditos.Cuo_Pagadas)
               W_CuoFalNva:SCREEN-VALUE = STRING(PlazoW)
               W_NvoPlazo.               
     ELSE IF Rs_Op EQ 2 THEN
        ASSIGN W_CuoFalNva:SCREEN-VALUE = STRING(Creditos.Plazo - Creditos.Cuo_Pagadas)
               W_NvoPlazo:SCREEN-VALUE  = STRING(Creditos.Plazo)
               W_NvoPlazo. 
     ELSE IF Rs_Op EQ 3 OR Rs_Op EQ 5 THEN
        ASSIGN W_CuoFalNva:SCREEN-VALUE = STRING(PlazoW).  
     
     IF W_NvaCuota LE 0 OR W_NvoPlazo LE 0 THEN DO:                                                                        
        MESSAGE "El Valor de la cuota y/o Plazo debe ser mayor a cero. Rectifique!" VIEW-AS ALERT-BOX ERROR.  
                                                         
        RETURN ERROR.                                                                               
     END.                                                                                                  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mes_Cont Wwin 
PROCEDURE Mes_Cont :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF MONTH(W_FecControl) EQ 4 OR MONTH(W_FecControl) EQ 6 OR MONTH(W_FecControl) EQ 9 
  OR MONTH(W_FecControl) EQ 11 THEN                                                            
     ASSIGN W_FecControl       = W_FecControl + 1                                              
            PlanPagos.Fec_Vcto = W_FecControl.                                                 
  ELSE IF MONTH(W_FecControl) EQ 3 THEN                                                        
     ASSIGN W_FecControl       = W_FecControl - 2                                              
            PlanPagos.Fec_Vcto = W_FecControl.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Modif_PlanPag Wwin 
PROCEDURE Modif_PlanPag :
/*------------------------------------------------------------------------------
  Purpose:       
  Notes:   Oct.11/06 GAER    
------------------------------------------------------------------------------*/
  DEFI VAR PdoSigue  AS INTEG FORM "9999".
  DEFI VAR NN        AS INTEG FORM "9999".
  DEFI VAR FecVcto   AS DATE.
  DEFI VAR FecCont   AS DATE.
  DEFI VAR FecIni    AS DATE.
  DEFI VAR W_NroDias AS INTEG FORM "9999".

  IF Creditos.Sistema EQ 2 THEN DO:
     ASSIGN W_NroDias = 30.
     IF      Creditos.Per_Pago EQ 1 THEN
            W_NroDias = 7.
     ELSE IF Creditos.Per_Pago EQ 2 THEN
            W_NroDias = 10.
     ELSE IF Creditos.Per_Pago EQ 3 THEN
            W_NroDias = 15.

     ASSIGN FecCont         = Creditos.Fec_Desemb
            FecIni          = Creditos.Fec_Desemb
            PlanPagos.Cuota = W_NvaCuota.

     IF Creditos.Fec_PagAnti NE ? AND Creditos.Fec_PagAnti GT Creditos.Fec_Desemb THEN
        ASSIGN FecCont = Creditos.Fec_PagAnti
               FecIni  = Creditos.Fec_PagAnti.
               
     RUN Halla_FecVcto.R (INPUT  FecIni,(W_NroDias * W_NvoPlazo),FecCont,   
                          OUTPUT FecCont).
     ASSIGN PlanPagos.Fec_Vcto     = FecCont
            PlanPagos.Fec_ProxPag  = FecCont
            Creditos.Fec_Pago      = FecCont
            Creditos.Sdo_Proyec    = Creditos.Monto            
            Creditos.Capital_Acum  = 0
            Creditos.Cuo_Pagadas   = 0
            PlanPagos.Cuo_Pagas    = 0
            PlanPagos.Capital_Acum = 0
            Creditos.Val_Atraso    = 0
            Creditos.Dias_Atraso   = 0
            Creditos.Cuo_Atraso    = 0.

     IF FecCont LE W_Fecha THEN
        ASSIGN Creditos.Sdo_Proyec    = 0
               Creditos.Capital_Acum  = Creditos.Monto
               PlanPagos.Capital_Acum = Creditos.Monto
               Creditos.Val_Atraso    = Creditos.Sdo_Capital
               Creditos.Dias_Atraso   = W_Fecha - FecCont + 1
               Creditos.Cuo_Atraso    = 1.

     FOR EACH PlanPagos WHERE PlanPagos.Agencia    EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0:   /*Solo Futuros sobran en Cuota Unica*/                        
         DELETE PlanPagos.
     END.

     RETURN.
  END.

  FIND LAST PlanPagos WHERE PlanPagos.Agencia      EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0   /*Solo Futuros*/
                        AND PlanPagos.Nro_Cuota    GT 0 NO-ERROR.
  IF AVAILABLE(PlanPagos) THEN DO:
     IF PlanPagos.Nro_Cuota EQ W_NvoPlazo THEN    /*Igual Pdos en el PlanPagos a creditos*/
        FOR EACH PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0   /*Solo Futuros*/
                        AND PlanPagos.Nro_Cuota    GT 0:
            ASSIGN PlanPagos.Cuota = PlanPagos.Cuota - Creditos.Cuota + W_NvaCuota
                   PlanPagos.Tasa  = W_NvaTasa.

            IF PlanPagos.Cuota LT W_NvaCuota THEN
               PlanPagos.Cuota = W_NvaCuota.
     END. 
     ELSE IF PlanPagos.Nro_Cuota GT W_NvoPlazo THEN   /*Sobran Pdos en el PlanPagos*/
        FOR EACH PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0   /*Solo Futuros*/
                        AND PlanPagos.Nro_Cuota    GT 0:
            IF PlanPagos.Nro_Cuota GT W_NvoPlazo THEN
               DELETE PlanPagos.
            ELSE DO:
               ASSIGN PlanPagos.Cuota = PlanPagos.Cuota - Creditos.Cuota + W_NvaCuota
                      PlanPagos.Plazo = W_NvoPlazo
                      PlanPagos.Tasa  = W_NvaTasa.

               IF PlanPagos.Cuota LT W_NvaCuota THEN
                  PlanPagos.Cuota = W_NvaCuota.
            END.
     END. 
     ELSE DO:  /*Faltan Pdos en el PlanPagos*/
        FOR EACH PlanPagos WHERE PlanPagos.Agencia EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0   /*Solo Futuros*/
                        AND PlanPagos.Nro_Cuota    GT 0:
            ASSIGN PlanPagos.Cuota = PlanPagos.Cuota - Creditos.Cuota + W_NvaCuota
                   PlanPagos.Plazo = W_NvoPlazo
                   PlanPagos.Tasa  = W_NvaTasa
                   PdoSigue        = PlanPagos.Nro_Cuota
                   FecVcto         = PlanPagos.Fec_Vcto
                   FecCont         = PlanPagos.Fec_Vcto.                   

            IF PlanPagos.Cuota LT W_NvaCuota THEN
               PlanPagos.Cuota = W_NvaCuota.

            IF NOT AVAIL(CPlanP) THEN
               CREATE CPlanP.

            BUFFER-COPY PlanPagos TO CPlanP.
        END.

        ASSIGN W_NroDias = 30.
        IF      Creditos.Per_Pago EQ 1 THEN
                W_NroDias = 7.
        ELSE IF Creditos.Per_Pago EQ 2 THEN
                W_NroDias = 10.
        ELSE IF Creditos.Per_Pago EQ 3 THEN
                W_NroDias = 15.

        DO NN = PdoSigue + 1 TO W_NvoPlazo:
           CREATE PlanPagos.
           BUFFER-COPY CPlanP TO PlanPagos.

           ASSIGN PlanPagos.Nro_Cuota = NN
                  PlanPagos.Fec_Ini   = FecCont
                  PlanPagos.Cuota     = W_NvaCuota.

           RUN Halla_FecVcto.R (INPUT  FecVcto,W_NroDias,FecCont,   
                                OUTPUT FecCont).
           PlanPagos.Fec_Vcto = FecCont.                  
        END.

        FOR EACH CPlanP: DELETE CPlanP. END.
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
          Creditos.INT_Corrientes:SCREEN-VALUE = STRING(Creditos.INT_Corrientes)
          Creditos.INT_Anticipado:SCREEN-VALUE = STRING(Creditos.INT_Anticipado).

   ASSIGN Creditos.Per_Pago:SCREEN-VALUE = STRING(Creditos.Per_Pago)
          Creditos.For_Pago:SCREEN-VALUE = STRING(Creditos.For_Pago)
          Creditos.Abogado:SCREEN-VALUE  = "No"
          W_NvaCuota:SCREEN-VALUE = STRING(Creditos.Cuota)
          W_NvoPlazo:SCREEN-VALUE = STRING(Creditos.Plazo)
          W_INTMora:SCREEN-VALUE   = STRING(Creditos.INT_MorCobrar + Creditos.INT_MoraDifCob)
          W_SdoOtros:SCREEN-VALUE  = STRING(Creditos.Costas + Creditos.Honorarios + Creditos.Polizas)
          W_CuoFaltan:SCREEN-VALUE = STRING(Creditos.Plazo - Creditos.Cuo_Pagadas)
          W_CuoFalNva:SCREEN-VALUE = STRING(Creditos.Plazo - Creditos.Cuo_Pagadas)
          W_NvaTasa:SCREEN-VALUE   = STRING(Creditos.Tasa)
          Rs_SistAmort:SCREEN-VALUE   = STRING(Creditos.Sistema)
          W_FecIni:SCREEN-VALUE       = STRING(Creditos.Fec_PagAnti)
          W_CedAbo                    = Creditos.Nit_Juzgado
          W_CedAbo:SCREEN-VALUE       = Creditos.Nit_Juzgado
          W_FecIni
          Rs_SistAmort
          W_NvaCuota
          W_NvoPlazo.

   IF Creditos.Sdo_Proyectado LE 0 THEN
      Creditos.Sdo_Proyectado:SCREEN-VALUE = "0".

   IF Creditos.Val_Atraso GT Creditos.Sdo_Capital THEN
      Creditos.Val_Atraso:SCREEN-VALUE = STRING(Creditos.Sdo_Capital).

   IF Creditos.Abogado THEN
      Creditos.Abogado:SCREEN-VALUE = "Yes".

   FOR EACH TExtras: DELETE TExtras. END.
   ASSIGN W_TotExt = 0
          W_TotExt:SCREEN-VALUE = "0".

   FOR EACH PlanPagos WHERE PlanPagos.Agencia      EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0 NO-LOCK:
       IF PlanPagos.Cuota GT Creditos.Cuota THEN DO:
          CREATE TExtras.
          ASSIGN TExtras.Agencia       = Creditos.Agencia     
                 TExtras.Cod_Credito   = Creditos.Cod_Credito 
                 TExtras.Nit           = Creditos.Nit         
                 TExtras.Num_Solicitud = Creditos.Num_Credito 
                 TExtras.Nro_Cuota     = PlanPagos.Nro_Cuota            
                 TExtras.Vr_CuoExtra   = PlanPagos.Cuota - Creditos.Cuota  
                 TExtras.Fec_Vcto      = PlanPagos.Fec_Vcto
                 TExtras.Estado        = 1                    
                 W_TotExt              = W_TotExt + TExtras.Vr_CuoExtra                    
                 W_TotExt:SCREEN-VALUE = STRING(W_TotExt).             
       END.
   END.

   OPEN  QUERY Br_Extras FOR EACH TExtras WHERE TExtras.Nit           EQ Creditos.Nit
                                          AND   TExtras.Num_Solicitud EQ Creditos.Num_Credito
                        NO-LOCK BY TExtras.Nro_Cuota INDEXED-REPOSITION.
          
   FIND LAST PlanPagos WHERE PlanPagos.Agencia     EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(PlanPagos) THEN DO:
      MESSAGE "La Tabla PlanPagos para el Pdo-Actual No Existe..." SKIP
              "                                     Refinanciaciòn cancelada..."
              VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.  
   
   CASE Creditos.Per_Pago:
     WHEN 1 THEN 
          ASSIGN W_PerPag:SCREEN-VALUE  = "Semanas"
                 W_PdoPag               = 7
                 W_PdoAno               = 52.
     WHEN 2 THEN 
          ASSIGN W_PerPag:SCREEN-VALUE  = "Dècadas"
                 W_PdoPag               = 10
                 W_PdoAno               = 36.
     WHEN 3 THEN 
          ASSIGN W_PerPag:SCREEN-VALUE  = "Quincenas"
                 W_PdoPag               = 15
                 W_PdoAno               = 24.
     WHEN 4 THEN 
          ASSIGN W_PerPag:SCREEN-VALUE  = "Meses"
                 W_PdoPag               = 30
                 W_PdoAno               = 12.
   END CASE.     
   
   ASSIGN W_CuoExtra = PlanPagos.Cuota - Creditos.Cuota
          Pagas      = Creditos.Cuo_Pagadas
          FecPP      = Creditos.Fec_Pago.
   
  /* RUN Liquidar.*/
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
    DEFINE VAR T_Plazo   AS CHARACTER FORMAT "X(30)".
    DEFINE VAR T_Dedu    AS CHARACTER FORMAT "X(30)".
    FIND Clientes WHERE Clientes.Nit EQ Creditos.Nit NO-LOCK NO-ERROR.
    ASSIGN W_Cliente = Creditos.Nit + " - " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
 
    W_EncColumna = "Cliente Solicitante         :   " + W_Cliente.
    W_Reporte   = "REESTRUCTURACION   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.

 DO WITH FRAME F_Cre:
   CASE Creditos.Per_Pago:
     WHEN 1 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Semanas".
     WHEN 2 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Dècadas".
     WHEN 3 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Quincenas".
     WHEN 4 THEN T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " Meses".
   END CASE.
   
   DISPLAY 
     "=============================================DATOS GENERALES DEL CREDITO=================================================" AT 1
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
     "Afectado Valor Vencido de Capital en $:" AT 1
     W_CapVdo                                  AT 40 FORMAT "->>>>,>>>,>>9"
     "Afectado Valor Vencido de Interes en $:" AT 1
     W_IntVdo                                  AT 40 FORMAT "->>>>,>>>,>>9"
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
/*------------------------------------------------------------------------------
  Purpose:  Actualiza nuevas condiciones en Crèditos y en PlanPagos.
            (Opciòn Rebaja Plazo y/o Cuota).
------------------------------------------------------------------------------*/
  ASSIGN PlanPagos.Cuota = PlanPagos.Cuota - Creditos.Cuota + W_NvaCuota /*La que transcurre*/
         PlanPagos.Plazo = W_NvoPlazo
         Creditos.Cuota  = W_NvaCuota
         Creditos.Plazo  = W_NvoPlazo.
         
  IF Rs_Op EQ 1 THEN DO:  /*Rebajò el Plazo*/
     FOR EACH PlanPagos WHERE PlanPagos.Agencia    EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0
                        AND PlanPagos.Nro_Cuota    GT W_NvoPlazo:   /*Futuros que Sobran*/
         DELETE PlanPagos.
     END.
  
     FOR EACH PlanPagos WHERE PlanPagos.Agencia    EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0:           /*Futuros que Faltan*/      
         ASSIGN PlanPagos.Plazo = W_NvoPlazo.                                                       
     END.  
  END.
  ELSE DO:    /*Rebajò Cuota, Igual Plazo*/
     FOR EACH PlanPagos WHERE PlanPagos.Agencia    EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                        AND PlanPagos.Id_PdoMes    EQ 0:           /*Futuros que Faltan*/      
         ASSIGN PlanPagos.Cuota = W_NvaCuota.                                                       
     END.  
  END.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReInicio Wwin 
PROCEDURE ReInicio :
/*------------------------------------------------------------------------------
  Purpose:  El crèdito se reinicia con las nuevas condiciones, y en PlanPagos.
 ------------------------------------------------------------------------------*/
  DEFI VAR W_ContCuota  LIKE PlanPagos.Nro_Cuota INIT 0.
  DEFI VAR I            AS   INTEG FORM "9999".
  DEFI VAR W_DiasTrans  AS   INTEG FORM "9999".
  
  FOR EACH TPlanP. DELETE TPlanP. END.
        
  IF RS_Op EQ 5 THEN DO:
     ASSIGN W_PdoPag = 30.
     IF INTEG(Creditos.Per_Pago:SCREEN-VALUE IN FRAME F_Cre) EQ 1 THEN
        W_PdoPag = 7.
     ELSE IF INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 2 THEN
        W_PdoPag = 10.
     ELSE IF INTEG(Creditos.Per_Pago:SCREEN-VALUE) EQ 3 THEN
        W_PdoPag = 15.

     W_NvoPlazo = Creditos.Plazo - Creditos.Cuo_Pagadas.
  END.
            
  ASSIGN Creditos.Monto        = Creditos.Sdo_Capital
         Creditos.Sdo_Proyecta = Creditos.Sdo_Capital
         Creditos.Cuota        = W_NvaCuota
         Creditos.Plazo        = W_NvoPlazo
         Creditos.Capital_Acum = 0            
         Creditos.Sdo_CapPag   = 0         
         Creditos.Sdo_IntMor   = 0
         Creditos.Cuo_Pagadas  = 0
         Creditos.Val_Atraso   = 0
         Creditos.Cuo_Atraso   = 0
         Creditos.Dias_Atraso  = 0
         Creditos.Fec_Pago     = PlanPagos.Fec_Vcto
         Creditos.Int_LiqAcum  = 0
         Creditos.Sdo_IntPag   = Creditos.Int_Anticipado.                  

  ASSIGN PlanPagos.Id_PdoMes   = 4   /*Cumplidos PP-Anterior*/
         W_FecControl          = PlanPagos.Fec_Vcto.

  CREATE TPlanP.
  BUFFER-COPY PlanPagos TO TPlanP.
  
  CREATE PlanPagos.
  BUFFER-COPY TPlanP TO PlanPagos.
  ASSIGN PlanPagos.Id_PdoMes = 1            /*El nuevo que transcurre */
         PlanPagos.Nro_Cuota = 1
         PlanPagos.Plazo     = W_NvoPlazo
         PlanPagos.Cuota     = W_NvaCuota
         PlanPagos.Tasa      = Creditos.Tasa
         PlanPagos.Cargos_Pdo        = 0          
         PlanPagos.Cargos_Acum       = Creditos.Costas + Creditos.Honorarios + Creditos.Polizas
         PlanPagos.Pagos_OtrosPdo    = 0
         PlanPagos.Pagos_OtrosAcum   = 0
         PlanPagos.Int_MoraPdo       = 0 
         PlanPagos.Int_MoraAcum      = Creditos.Int_MoraDifCob + Creditos.Int_MorCobrar
         PlanPagos.Pagos_MoraPdo     = 0
         PlanPagos.Pagos_MoraAcum    = 0.

  ASSIGN PlanPagos.Int_LiqPdo        = 0
         PlanPagos.Int_LiqAcum       = 0  
         PlanPagos.Pagos_IntPdo      = 0
         PlanPagos.Pagos_IntAcum     = Creditos.Int_Anticipado
         PlanPagos.Capital_Pdo       = 0
         PlanPagos.Capital_Acum      = 0                               
         PlanPagos.Pagos_CapitalPdo  = 0 
         PlanPagos.Pagos_CapitalAcum = 0                                       
         PlanPagos.Fec_ProxPago      = Creditos.Fec_Pago
         PlanPagos.Monto_Actual      = Creditos.Sdo_Capital 
         PlanPagos.Cuo_Pagas         = 0
         W_ContCuota                 = 1.  

  IF  RS_Op EQ 5 AND Tg_ReInicio AND W_SiPdo 
  AND PlanPagos.Fec_Vcto GT (W_Fecha + W_PdoPag) THEN 
        ASSIGN PlanPagos.Fec_Vcto = PlanPagos.Fec_Vcto - W_PdoPag
               Creditos.Fec_Pago  = PlanPagos.Fec_Vcto
               W_FecControl       = PlanPagos.Fec_Vcto.

  /*Agosto 18/05 GAER, Inicia el 1er.pdo con la liq.de los ya transcurridos*/
  ASSIGN PlanPagos.Int_LiqPdo = ROUND((Creditos.Sdo_Proyecta * (W_NvaTasa / 360) * 
                                      (W_PdoPag - (PlanPagos.Fec_Vcto - W_Fecha))) / 100,0).
  ASSIGN PlanPagos.Int_LiqPdo = 0 WHEN PlanPagos.Int_LiqPdo LT 0.

  CREATE TPlanP.
  BUFFER-COPY PlanPagos TO TPlanP.

  FOR EACH PlanPagos WHERE PlanPagos.Agencia      EQ Creditos.Agencia    
                       AND PlanPagos.Nit          EQ Creditos.Nit        
                       AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                       AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito
                       AND (PlanPagos.Id_PdoMes   EQ 2 OR PlanPagos.Id_PdoMes EQ 0): 
      IF PlanPagos.Id_PdoMes EQ 2 THEN
         PlanPagos.Id_PdoMes = 4. /*Los ya transcurridos, pasan a No_vigentes*/
      ELSE 
         DELETE PlanPagos.       /*El resto(Futuros) se eliminan*/
  END.

  IF W_ContCuota GE W_NvoPlazo THEN
     RETURN.

  ASSIGN W_QnaDec = 0.
  
  IF W_ContCuota LT W_NvoPlazo THEN DO I = W_ContCuota + 1 TO W_NvoPlazo:
     W_DiasTrans = W_DiasTrans + W_PdoPag.
     
     CREATE PlanPagos.
     BUFFER-COPY TPlanP TO PlanPagos.
     ASSIGN PlanPagos.Plazo        = W_NvoPlazo
            PlanPagos.Cuota        = W_NvaCuota
            PlanPagos.Monto_Actual = Creditos.Sdo_Capital
            PlanPagos.Tasa         = Creditos.Tasa
            PlanPagos.Cuo_Pagas    = 0
            PlanPagos.Id_PdoMes    = 0
            PlanPagos.Nro_Cuota    = I
            PlanPagos.Fec_Inic     = W_FecControl            
            PlanPagos.Fec_Vcto     = W_FecControl + W_PdoPag 
            W_FecControl           = W_FecControl + W_PdoPag.

     ASSIGN PlanPagos.Cargos_Pdo        = 0
            PlanPagos.Cargos_Acum       = 0
            PlanPagos.Pagos_OtrosPdo    = 0
            PlanPagos.Pagos_OtrosAcum   = 0
            PlanPagos.Int_MoraPdo       = 0
            PlanPagos.Int_MoraAcum      = 0
            PlanPagos.Pagos_MoraPdo     = 0
            PlanPagos.Pagos_MoraAcum    = 0
            PlanPagos.Int_LiqPdo        = 0
            PlanPagos.Int_LiqAcum       = 0
            PlanPagos.Pagos_IntPdo      = 0
            PlanPagos.Pagos_IntAcum     = 0
            PlanPagos.Capital_Pdo       = 0
            PlanPagos.Capital_Acum      = 0
            PlanPagos.Pagos_CapitalPdo  = 0
            PlanPagos.Pagos_CapitalAcum = 0. 

     IF W_PdoPag NE 7 THEN DO:
        IF W_PdoPag EQ 30 THEN 
           RUN Mes_Cont.   
        ELSE DO:
           W_QnaDec = W_QnaDec + 1.

           IF W_PdoPag EQ 15 AND W_QnaDec EQ 2 THEN DO:
              RUN Mes_Cont.
              W_QnaDec = 0.
           END.
           ELSE IF W_PdoPag EQ 10 AND W_QnaDec EQ 3 THEN DO:
              RUN Mes_Cont. 
              W_QnaDec = 0.
           END.
        END.
     END.
  END.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TotalMoroso Wwin 
PROCEDURE TotalMoroso :
/*------------------------------------------------------------------------------
  Purpose:  Aplica Clàusula aceleratoria de pago (Total Sdo_Capital) en mora,
            y en PlanPagos. 
------------------------------------------------------------------------------*/
  ASSIGN Creditos.Sdo_proyectado = 0
         Creditos.Capital_Acum   = Creditos.Monto
         Creditos.Cuo_Atraso     = Creditos.Plazo - Creditos.Cuo_Pagadas 
         Creditos.Dias_Atraso    = (W_Fecha + 1)  - Creditos.Fec_Pago
         Creditos.Val_Atraso     = Creditos.Sdo_capital.
         
  ASSIGN PlanPagos.Capital_Pdo  = PlanPagos.Capital_Pdo + Creditos.Sdo_capital
         PlanPagos.Capital_Acum = Creditos.Monto.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

